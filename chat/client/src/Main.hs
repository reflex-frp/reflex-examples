{-# LANGUAGE OverloadedStrings, RecursiveDo, LambdaCase #-}
module Main where

import Common.Api

import Reflex.Dom
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Data.Monoid

main :: IO ()
main = mainWidget $ do
  nick <- el "div" nickInput
  rec i <- textInput $ def & setValue .~ ("" <$ send)
      let send = textInputGetEnter i
  let wsUp = leftmost [ tag (Up_Message . Message (Nick "ryan") (Left $ Nick "ryan") . T.pack <$> current (value i)) send
                      , fmapMaybe (fmap Up_AddNick) (updated nick)
                      ]
  ws <- webSocket "ws://localhost:8000/api" $ def
    & webSocketConfig_send .~ fmap ((:[]) . LBS.toStrict . encode) wsUp
  performEvent_ $ liftIO . print <$> _webSocket_recv ws
  return ()

nickInput :: MonadWidget t m => m (Dynamic t (Maybe Nick))
nickInput = do
  nickInput <- textInput def
  addNick <- button "Add Nick"
  nick <- holdDyn Nothing $ tag (validNick . T.pack <$> current (value nickInput)) $ leftmost [addNick, textInputGetEnter nickInput]
  (nickMsgAttr, nickMsg) <- splitDyn <=< forDyn nick $ \case
    Nothing -> ("style" =: "color: red;", "No nickname set!")
    Just n -> ("style" =: "color: green;", "Hi, " <> (T.unpack $ unNick n) <> "!")
  elDynAttr "small" nickMsgAttr $ dynText nickMsg
  return nick

validNick :: Text -> Maybe Nick
validNick t = if T.null (T.strip t) then Nothing else Just $ Nick t

