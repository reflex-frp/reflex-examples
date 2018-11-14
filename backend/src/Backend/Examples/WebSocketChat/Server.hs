{-# LANGUAGE OverloadedStrings #-}

module Backend.Examples.WebSocketChat.Server where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_,
                                     readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)
import           Data.Aeson         (encode, decode)
import qualified Data.ByteString    as B
import           Data.ByteString.Lazy (toStrict)
import           Data.Char          (isPunctuation, isSpace)
import           Data.Semigroup     ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Network.WebSockets as WS

--------------------------------------------------------------------------------
import           Common.Examples.WebSocketChat.Message
--------------------------------------------------------------------------------

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    -- forM_ clients $ \(_, conn) -> WS.sendTextData conn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn $
        (toStrict . encode . S2Cbroadcast) message

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msgbs <- WS.receiveData conn :: IO B.ByteString
    let msgC = decode $ WS.toLazyByteString msgbs :: Maybe C2S
        -- msg = case msgC of
        --     Just (C2Sjoin txt) -> txt
        --     Just C2Sclose      -> "close msg"
        --     Just (C2Smsg txt)  -> txt
        --     Nothing            -> "hmm nothing"
    -- T.putStrLn $ "msg = " <> msg
    clients <- readMVar state
    case msgC of
        Nothing           ->
            T.putStrLn "Decoded msgC is nothing..."
        Just (C2Smsg txt) ->
            T.putStrLn $ "C2Smsg should not happen here, txt =" <> txt
        Just C2Sclose     ->
            T.putStrLn "C2Sclose should never happen here..."
        Just (C2Sjoin nm) ->
          case nm of
              _ | any ($ nm) [T.null, T.any isPunctuation, T.any isSpace] -> do
                        T.putStrLn $ ":" <> nm <> ":"
                        WS.sendTextData conn $ (toStrict . encode) S2Cnameproblem
                | clientExists client clients ->
                    WS.sendTextData conn $ (toStrict . encode) S2Cuserexists
                | otherwise -> flip finally disconnect $ do
                   modifyMVar_ state $ \s -> do
                       let s' = addClient client s
                       WS.sendTextData conn $ (toStrict . encode . S2Cwelcome) $
                           T.intercalate ", " (map fst s)
                       broadcast (nm <> " joined") s'
                       return s'
                   talk conn state client
          where
            client     = (nm,conn)
            disconnect = do
                -- Remove client and return new state
                s <- modifyMVar state $ \s ->
                    let s' = removeClient client s in return (s', s')
                broadcast (fst client <> " disconnected") s

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
    msgbs <- WS.receiveData conn :: IO B.ByteString
    case decode $ WS.toLazyByteString msgbs of
        Nothing           ->
            T.putStrLn "Decoded msgC is nothing..."
        Just C2Sclose     ->
            undefined -- TBD
        Just (C2Sjoin nm) ->
            T.putStrLn $ "C2Sjoin should not happen here, nm =" <> nm
        Just (C2Smsg txt) ->
            readMVar state >>= broadcast (user <> ": " <> txt)


