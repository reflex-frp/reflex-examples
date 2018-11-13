{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Frontend where

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Control.Monad.Fix (MonadFix)
import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Frontend.Head
import Frontend.Home
import Frontend.Nav

import qualified Frontend.Examples.BasicToDo.Main as BasicToDo
import qualified Frontend.Examples.DragAndDrop.Main as DragAndDrop
import qualified Frontend.Examples.FileReader.Main as FileReader
import qualified Frontend.Examples.ScreenKeyboard.Main as ScreenKeyboard
import qualified Frontend.Examples.NasaPod.Main as NasaPod
import qualified Frontend.Examples.PegSolitaire.Main as PegSolitaire
import qualified Frontend.Examples.WebSocketEcho.Main as WebSocketEcho
import qualified Frontend.Examples.WebSocketChat.Main as WebSocketChat

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = pageHead
  , _frontend_body = do
      -- The recursion here allows us to send a click event from the content area "up" into the header
      rec el "header" $ nav click
          click <- mainContainer $ do
            article $ subRoute_ $ \case
              FrontendRoute_Home -> home
              FrontendRoute_BasicToDo -> BasicToDo.app
              FrontendRoute_DragAndDrop -> DragAndDrop.app
              FrontendRoute_FileReader -> FileReader.app
              FrontendRoute_ScreenKeyboard -> ScreenKeyboard.app
              FrontendRoute_NasaPod -> NasaPod.app
              FrontendRoute_PegSolitaire -> PegSolitaire.app
              FrontendRoute_WebSocketEcho -> WebSocketEcho.app
              FrontendRoute_WebSocketChat -> WebSocketChat.app
      return ()
  }

-- | The @<main>@ tag that will contain most of the site's content
mainContainer :: DomBuilder t m => m () -> m (Event t ())
mainContainer w = domEvent Click . fst <$> el' "main" w

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
  let sectionClass = ("class" =: "")
  elAttr "section" sectionClass c
