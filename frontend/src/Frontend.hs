{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Frontend where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import qualified Obelisk.ExecutableConfig as Cfg

import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Fix (MonadFix)
import Common.Route

import Frontend.Head
import Frontend.Home
import Frontend.Nav

import qualified Frontend.Examples.BasicToDo.Main as BasicToDo
import qualified Frontend.Examples.DragAndDrop.Main as DragAndDrop
import qualified Frontend.Examples.FileReader.Main as FileReader
import qualified Frontend.Examples.ScreenKeyboard.Main as ScreenKeyboard
import qualified Frontend.Examples.NasaPod.Main as NasaPod
import qualified Frontend.Examples.PegSolitaire.Main as PegSolitaire
import qualified Frontend.Examples.TicTacToe.Main as TicTacToe
import qualified Frontend.Examples.DisplayGameUpdates.Main as DisplayGameUpdates
import qualified Frontend.Examples.WebSocketEcho.Main as WebSocketEcho
import qualified Frontend.Examples.WebSocketChat.Main as WebSocketChat

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = pageHead
  , _frontend_body = do
      r <- liftIO $ Cfg.get "config/common/route"
      el "header" $ nav
      el "main" $ article $ subRoute_ $ \case
        FrontendRoute_Home -> home
        FrontendRoute_Examples -> maybeRoute_ home $ examples r =<< askRoute
      return ()
  }

-- | Displays the example
examples
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     )
  => Maybe Text
  -> Dynamic t (R Example)
  -> RoutedT t (R Example) m ()
examples route _ = subRoute_ $ \case
  Example_BasicToDo -> BasicToDo.app
  Example_DragAndDrop -> DragAndDrop.app
  Example_FileReader -> FileReader.app
  Example_ScreenKeyboard -> ScreenKeyboard.app
  Example_NasaPod -> NasaPod.app
  Example_PegSolitaire -> PegSolitaire.app
  Example_TicTacToe -> TicTacToe.app
  Example_DisplayGameUpdates -> DisplayGameUpdates.app
  Example_WebSocketEcho -> WebSocketEcho.app
  Example_WebSocketChat -> WebSocketChat.app route

-- | An @<article>@ tag that will set its title and the class of its child
-- @<section>@ based on the current route
article
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     )
  => m () -- ^ Article content widget
  -> m ()
article c = el "article" $ do
  r <- askRoute
  el "h3" $ dynText $ routeDescription <$> r
  divClass "" c
