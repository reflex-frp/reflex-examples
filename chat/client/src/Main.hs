{-# LANGUAGE OverloadedStrings, RecursiveDo #-}
module Main where

import Common.Api

import Reflex.Dom
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding
import Control.Monad.IO.Class

main :: IO ()
main = mainWidget $ do
  rec i <- textInput $ def & setValue .~ ("" <$ send)
      let send = textInputGetEnter i
  ws <- webSocket "ws://localhost:8000/api" $ def
    & webSocketConfig_send .~ tag ((:[]) . LBS.toStrict . encode . Up_Message . Message (Nick "ryan") (Nick "ryan") . T.pack <$> current (value i)) send
  performEvent_ $ liftIO . print <$> _webSocket_recv ws
  return ()
