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
  BackendRoute_WebSocketChat :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Examples :: FrontendRoute (Maybe (R Example))
deriving instance Show (FrontendRoute a)

data Example :: * -> * where
  Example_BasicToDo :: Example ()
  Example_DragAndDrop :: Example ()
  Example_FileReader :: Example ()
  Example_ScreenKeyboard :: Example ()
  Example_NasaPod :: Example ()
  Example_PegSolitaire :: Example ()
  Example_TicTacToe :: Example ()
  Example_DisplayGameUpdates :: Example ()
  Example_ECharts :: Example ()
  Example_WebSocketEcho :: Example ()
  Example_WebSocketChat :: Example ()
deriving instance Show (Example a)

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_WebSocketChat -> PathSegment "websocketchat" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Home -> PathEnd $ unitEncoder mempty
      FrontendRoute_Examples -> PathSegment "examples" $ maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
        Example_BasicToDo -> PathSegment "basictodo" $ unitEncoder mempty
        Example_DragAndDrop -> PathSegment "draganddrop" $ unitEncoder mempty
        Example_FileReader -> PathSegment "filereader" $ unitEncoder mempty
        Example_ScreenKeyboard -> PathSegment "screenkeyboard" $ unitEncoder mempty
        Example_NasaPod -> PathSegment "nasapod" $ unitEncoder mempty
        Example_PegSolitaire -> PathSegment "pegsolitaire" $ unitEncoder mempty
        Example_TicTacToe -> PathSegment "tictactoe" $ unitEncoder mempty
        Example_DisplayGameUpdates -> PathSegment "displaygameupdates" $ unitEncoder mempty
        Example_ECharts -> PathSegment "echarts" $ unitEncoder mempty
        Example_WebSocketEcho -> PathSegment "websocketecho" $ unitEncoder mempty
        Example_WebSocketChat -> PathSegment "websocketchat" $ unitEncoder mempty

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''Example
  ]


-- | Provide a human-readable name for a given section
exampleTitle :: Some Example -> Text
exampleTitle (Some.This sec) = case sec of
  Example_BasicToDo -> "Basic To Do List"
  Example_DragAndDrop -> "Drag n Drop"
  Example_FileReader -> "File Reader"
  Example_ScreenKeyboard -> "Onscreen Keyboard"
  Example_NasaPod -> "Nasa: Picture of the Day"
  Example_PegSolitaire -> "Peg Solitaire"
  Example_TicTacToe -> "Tic Tac Toe"
  Example_DisplayGameUpdates -> "Display Game Updates"
  Example_ECharts -> "ECharts Examples"
  Example_WebSocketEcho -> "WebSocket Echo"
  Example_WebSocketChat -> "WebSocket Chat"


-- | Provide a human-readable name for a route
routeTitle :: R FrontendRoute -> Text
routeTitle = \case
  (FrontendRoute_Home :=> _) -> "Examples"
  (FrontendRoute_Examples :=> Identity ex) -> case ex of
    (Nothing) -> "Examples"
    (Just (sec :=> _)) -> exampleTitle $ Some.This sec

-- | Given a section, provide its default route
sectionHomepage :: Some Example -> R Example
sectionHomepage (Some.This sec) = sec :/ case sec of
  Example_BasicToDo -> ()
  Example_DragAndDrop -> ()
  Example_FileReader -> ()
  Example_ScreenKeyboard -> ()
  Example_NasaPod -> ()
  Example_PegSolitaire -> ()
  Example_TicTacToe -> ()
  Example_DisplayGameUpdates -> ()
  Example_WebSocketEcho -> ()
  Example_WebSocketChat -> ()

-- | Provide a human-readable description for a given section
exampleDescription :: Some Example -> Text
exampleDescription (Some.This sec) = case sec of
  Example_BasicToDo -> "A simple To-Do list app with user input handling and state management."
  Example_DragAndDrop ->
    "An example to demonstrate Drag and Drop functionality"
  Example_FileReader ->
    "Read a file on the client using FileReader"
  Example_ScreenKeyboard ->
    "Use an onscreen keyboard along with the normal user input"
  Example_NasaPod ->
    "Demonstrates XHR requests, by fetching Nasa' Astronomy Picture of the Day"
  Example_PegSolitaire ->
    "A simple client side game"
  Example_TicTacToe ->
    "A simple client side game"
  Example_DisplayGameUpdates ->
    "An example to demonstrate nested Dynamic values. A widget to show updates for a game."
  Example_ECharts ->
    "Usage of ECharts (external JS library) with GHCJS and Reflex"
  Example_WebSocketEcho ->
    "Demonstrates use of WebSocket by sending and receiving messages from websocket.org' echo API"
  Example_WebSocketChat ->
    "A simple chat server, this uses the common and backend packages to share data between client and server"

-- | Provide a human-readable description for a given route
routeDescription :: R FrontendRoute -> Text
routeDescription  = \case
  (FrontendRoute_Home :=> _) -> desc
  (FrontendRoute_Examples :=> Identity m) -> case m of
    (Nothing) -> desc
    (Just (ex :=> _)) -> exampleDescription $ Some.This ex
  where
    desc :: Text
    desc = "Welcome to Reflex Examples"

routeSourceCode :: R FrontendRoute -> Text
routeSourceCode = \case
  (FrontendRoute_Home :=> _) -> src
  (FrontendRoute_Examples :=> Identity m) -> case m of
    (Nothing) -> src
    (Just ex) -> exampleSourceCode ex
  where
    src :: Text
    src = "https://github.com/reflex-frp/reflex-examples"

exampleSourceCode :: R Example -> Text
exampleSourceCode (sec :=> _) = base <> path <> file
  where
    base = "https://github.com/reflex-frp/reflex-examples/blob/master/frontend/src/Frontend/Examples/"
    file = "/Main.hs"
    path = case sec of
      Example_BasicToDo -> "BasicToDo"
      Example_DragAndDrop -> "DragAndDrop"
      Example_FileReader -> "FileReader"
      Example_ScreenKeyboard -> "ScreenKeyboard"
      Example_NasaPod -> "NasaPod"
      Example_PegSolitaire -> "PegSolitaire"
      Example_TicTacToe -> "TicTacToe"
      Example_DisplayGameUpdates -> "DisplayGameUpdates"
      Example_ECharts -> "ECharts"
      Example_WebSocketEcho -> "WebSocketEcho"
      Example_WebSocketChat -> "WebSocketChat"
