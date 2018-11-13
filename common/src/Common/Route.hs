{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Dependent.Sum (DSum (..))

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_BasicToDo :: FrontendRoute ()
  FrontendRoute_DragAndDrop :: FrontendRoute ()
  FrontendRoute_FileReader :: FrontendRoute ()
  FrontendRoute_ScreenKeyboard :: FrontendRoute ()
  FrontendRoute_NasaPod :: FrontendRoute ()
  FrontendRoute_PegSolitaire :: FrontendRoute ()
  FrontendRoute_WebSocketEcho :: FrontendRoute ()
  FrontendRoute_WebSocketChat :: FrontendRoute ()
deriving instance Show (FrontendRoute a)

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Home -> PathEnd $ unitEncoder mempty
      FrontendRoute_BasicToDo -> PathSegment "basictodo" $ unitEncoder mempty
      FrontendRoute_DragAndDrop -> PathSegment "draganddrop" $ unitEncoder mempty
      FrontendRoute_FileReader -> PathSegment "filereader" $ unitEncoder mempty
      FrontendRoute_ScreenKeyboard -> PathSegment "screenkeyboard" $ unitEncoder mempty
      FrontendRoute_NasaPod -> PathSegment "nasapod" $ unitEncoder mempty
      FrontendRoute_PegSolitaire -> PathSegment "pegsolitaire" $ unitEncoder mempty
      FrontendRoute_WebSocketEcho -> PathSegment "websocketecho" $ unitEncoder mempty
      FrontendRoute_WebSocketChat -> PathSegment "websocketchat" $ unitEncoder mempty

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]


-- | Provide a human-readable name for a given section
sectionTitle :: Some FrontendRoute -> Text
sectionTitle (Some.This sec) = case sec of
  FrontendRoute_Home -> "Home"
  FrontendRoute_BasicToDo -> "ToDo"
  FrontendRoute_DragAndDrop -> "Drap n Drop"
  FrontendRoute_FileReader -> "File Reader"
  FrontendRoute_ScreenKeyboard -> "Screen Keyboard"
  FrontendRoute_NasaPod -> "Nasa Pod"
  FrontendRoute_PegSolitaire -> "Peg Solitaire"
  FrontendRoute_WebSocketEcho -> "WebSocket Echo"
  FrontendRoute_WebSocketChat -> "WebSocket Chat"


-- | Provide a human-readable name for a route
routeTitle :: R FrontendRoute -> Text
routeTitle (sec :=> _) = sectionTitle $ Some.This sec

-- | Given a section, provide its default route
sectionHomepage :: Some FrontendRoute -> R FrontendRoute
sectionHomepage (Some.This sec) = sec :/ case sec of
  FrontendRoute_Home -> ()
  FrontendRoute_BasicToDo -> ()
  FrontendRoute_DragAndDrop -> ()
  FrontendRoute_FileReader -> ()
  FrontendRoute_ScreenKeyboard -> ()
  FrontendRoute_NasaPod -> ()
  FrontendRoute_PegSolitaire -> ()
  FrontendRoute_WebSocketEcho -> ()
  FrontendRoute_WebSocketChat -> ()

-- | Provide a human-readable description for a given section
sectionDescription :: Some FrontendRoute -> Text
sectionDescription (Some.This sec) = case sec of
  FrontendRoute_Home -> "Examples of Reflex FRP usage"
  FrontendRoute_BasicToDo -> "A simple ToDo list app"
  FrontendRoute_DragAndDrop ->
    "An example to demonstrate Drag and Drop functionality using JavaScript code?"
  FrontendRoute_FileReader ->
    "Read a file on the client side using FileReader"
  FrontendRoute_ScreenKeyboard ->
    "Use an onscreen keyboard along with the normal user input"
  FrontendRoute_NasaPod ->
    "Demonstrates XHR requests, by fetching Nasa' Astronomy Picture of the Day"
  FrontendRoute_PegSolitaire ->
    "A simple client side game"
  FrontendRoute_WebSocketEcho ->
    "Demonstrates use of WebSocket by sending and receiving messages from websocket.org' echo API"
  FrontendRoute_WebSocketChat ->
    "Use WebSocket communication to implement a simple chat server, this uses the common and backend packages to implement a simple server"

-- | Provide a human-readable description for a given route
routeDescription :: R FrontendRoute -> Text
routeDescription (sec :=> _) = sectionDescription $ Some.This sec
