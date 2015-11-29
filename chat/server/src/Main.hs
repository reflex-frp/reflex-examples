{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Main where

import Common.Api

import Control.Lens
import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.Aeson
import Data.Aeson.TH
import Data.IORef
import Data.Text (Text)
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
           }

makeLenses ''State

emptyState :: State
emptyState = State
  { _state_nextConnId = ConnId 1
  , _state_conns = mempty
  , _state_nickToConn = mempty
  }

withConnInState :: IORef State -> Connection -> (ConnId -> IO a) -> IO a
withConnInState sRef c = bracket open close
  where open = do
          atomicModifyIORef' sRef $ \s ->
            let cid = _state_nextConnId s
            in (s { _state_nextConnId = succ cid }, cid)
        close cid = atomicModifyIORef_' sRef $
          (state_conns %~ Map.delete cid) .
          (state_nickToConn %~ fmap (Set.filter (/= cid))) --TODO: Create a reverse mapping to make this faster

addNick :: Nick -> ConnId -> State -> State
addNick n cid = state_nickToConn %~ Map.insertWith Set.union n (Set.singleton cid)

getConnsForNick :: Nick -> State -> [Connection]
getConnsForNick n s = Map.elems $ Map.intersection (_state_conns s) connIds
  where connIds = Map.fromSet (const ()) $ Map.findWithDefault Set.empty n $ _state_nickToConn s

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
        forM_ (getConnsForNick (_message_to m) s) $ \receiverConn -> do
          sendTextData receiverConn $ encode $ Down_Message $ Envelope t m
      Up_AddNick n -> atomicModifyIORef_' sRef $ addNick n cid
    return ()

main :: IO ()
main = do
  sRef <- newIORef emptyState
  quickHttpServe $ route
    [ ("", serveDirectory "static")
    , ("api", handleApi sRef)
    ]
