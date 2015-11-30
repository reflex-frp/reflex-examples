{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Main where

import Common.Api

import Control.Lens
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Either
import Data.Monoid
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Network.WebSockets
import Network.WebSockets.Snap
import Snap
import Snap.Util.FileServe

data State
   = State { _state_nextConnId :: ConnId
           , _state_conns :: Map ConnId Connection
           , _state_nickToConn :: Map Nick (Set ConnId)
           , _state_channelToNick :: Map ChannelId (Set Nick)
           }

makeLenses ''State

emptyState :: State
emptyState = State
  { _state_nextConnId = ConnId 1
  , _state_conns = mempty
  , _state_nickToConn = mempty
  , _state_channelToNick = mempty
  }

withConnInState :: IORef State -> Connection -> (ConnId -> IO a) -> IO a
withConnInState sRef c = bracket open close
  where open = atomicModifyIORef' sRef $ \s ->
          let cid = _state_nextConnId s
              s' = s & state_nextConnId .~ succ cid
                     & state_conns %~ Map.insert cid c
          in (s', cid)
        close cid = atomicModifyIORef_' sRef $
          (state_conns %~ Map.delete cid) .
          (state_nickToConn %~ fmap (Set.filter (/= cid))) --TODO: Create a reverse mapping to make this faster

addNick :: Nick -> ConnId -> State -> State
addNick n cid = state_nickToConn %~ insertItem n cid

removeNick :: Nick -> ConnId -> State -> State
removeNick n cid = state_nickToConn %~ deleteItem n cid

getConnsForNick :: Nick -> State -> [Connection]
getConnsForNick n s = Map.elems $ Map.intersection (_state_conns s) (Map.fromSet (const ()) connIds)
  where connIds = Map.findWithDefault Set.empty n $ _state_nickToConn s

getConnsForChannel :: ChannelId -> State -> [Connection]
getConnsForChannel c s = Map.elems $ Map.intersection (_state_conns s) (Map.fromSet (const ()) connIds)
  where nicks = Map.findWithDefault Set.empty c $ _state_channelToNick s
        connIds = Set.unions $ Map.elems $ Map.intersection (_state_nickToConn s) $ Map.fromSet (const ()) nicks

getConnsForDestination :: Destination -> State -> [Connection]
getConnsForDestination = either getConnsForNick getConnsForChannel

joinChannel :: ChannelId -> Nick -> State -> State
joinChannel c n = state_channelToNick %~ insertItem c n

leaveChannel :: ChannelId -> Nick -> State -> State
leaveChannel c n = state_channelToNick %~ deleteItem c n


insertItem :: (Ord k, Ord v) => k -> v -> Map k (Set v) -> Map k (Set v)
insertItem k v = Map.alter f k
  where f mvs = Just $ case mvs of
          Nothing -> Set.singleton v
          Just vs -> Set.insert v vs

deleteItem :: (Ord k, Ord v) => k -> v -> Map k (Set v) -> Map k (Set v)
deleteItem k v = Map.alter f k
  where f mvs = case mvs of
          Nothing -> Nothing
          Just vs ->
            let vs' = Set.delete v vs
            in if Set.null vs'
               then Nothing
               else Just vs'

atomicModifyIORef_' :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_' r f = atomicModifyIORef' r $ \a -> (f a, ())

handleApi :: MonadSnap m => IORef State -> m ()
handleApi sRef = runWebSocketsSnap $ \pendingConn -> do
  conn <- acceptRequest pendingConn
  withConnInState sRef conn $ \cid -> forever $ do
    Text upRaw <- receiveDataMessage conn
    Right up <- return $ eitherDecode' upRaw
    case up of
      Up_Message m -> do
        s <- readIORef sRef
        t <- getCurrentTime
        let conns = getConnsForDestination (_message_to m) s ++ if (isLeft $ _message_to m)
              then getConnsForNick (_message_from m) s
              else []
        forM_ conns $ \receiverConn -> do
          sendTextData receiverConn $ encode $ Down_Message $ Envelope t m
      Up_AddNick n -> atomicModifyIORef_' sRef $ addNick n cid
      Up_RemoveNick n -> atomicModifyIORef_' sRef $ removeNick n cid
      Up_JoinChannel c n -> atomicModifyIORef_' sRef $ joinChannel c n
      Up_LeaveChannel c n -> atomicModifyIORef_' sRef $ leaveChannel c n
    return ()

handleState :: (MonadSnap m, MonadIO m) => IORef State -> m ()
handleState sRef = do
  s <- liftIO $ readIORef sRef
  writeText $ T.unlines
    [ "Next ConnId: " <> T.pack (show $ _state_nextConnId s)
    , "Connections: " <> T.pack (show $ Map.keys $ _state_conns s)
    , "Nick to Connection: " <> T.pack (show $ Map.toList $ _state_nickToConn s)
    , "ChannelId to Nick: " <> T.pack (show $ Map.toList $ _state_channelToNick s)
    ]

main :: IO ()
main = do
  sRef <- newIORef emptyState
  quickHttpServe $ route
    [ ("", serveDirectory "client.jsexe" <|> serveDirectory "static")
    , ("api", handleApi sRef)
    , ("state", handleState sRef)
    ]
