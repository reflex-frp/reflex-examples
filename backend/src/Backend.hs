{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Control.Concurrent
import Network.WebSockets.Snap

import qualified Backend.Examples.WebSocketChat.Server as WebSocketChat

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      state <- newMVar WebSocketChat.newServerState
      serve $ \case
        BackendRoute_Missing :=> Identity () -> return ()
        BackendRoute_WebSocketChat :=> Identity () -> do
          runWebSocketsSnap (WebSocketChat.application state)

  , _backend_routeEncoder = backendRouteEncoder
  }
