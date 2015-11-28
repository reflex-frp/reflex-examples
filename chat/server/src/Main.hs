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
        close cid = do
          atomicModifyIORef' sRef $ \s ->
            let s' = s & state_conns %~ Map.delete cid
            in (s', ())

handleApi :: MonadSnap m => IORef State -> m ()
handleApi sRef = runWebSocketsSnap $ \pendingConn -> do
  conn <- acceptRequest pendingConn
  withConnInState sRef conn $ \cid -> forever $ do
    Text upRaw <- receiveDataMessage conn
    Right up <- return $ eitherDecode' upRaw
    case up of
      Up_Message n m -> sendDataMessage conn $ Text $ encode $ Down_Message n m
    return ()

main :: IO ()
main = do
  sRef <- newIORef emptyState
  quickHttpServe $ route
    [ ("", serveDirectory "static")
    , ("api", handleApi sRef)
    ]
