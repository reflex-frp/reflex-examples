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
import Control.Monad.Trans.Maybe

main :: IO ()
main = mainWidget $ do
  nick <- el "div" nickInput
  recipient <- el "div" recipientNickInput
  rec i <- textInput $ def & setValue .~ ("" <$ send)
      let send = textInputGetEnter i
  let wsUp = leftmost [ fmapMaybe (fmap Up_Message) $ tag (directMessage <$> current nick <*> current recipient <*> current (value i)) send
                      , fmapMaybe (fmap Up_AddNick) (updated nick)
                      ]
  ws <- webSocket "ws://localhost:8000/api" $ def
    & webSocketConfig_send .~ fmap ((:[]) . LBS.toStrict . encode) wsUp
  performEvent_ $ liftIO . print <$> _webSocket_recv ws
  return ()

directMessage :: Maybe Nick -> Maybe Nick -> String -> Maybe Message
directMessage sender recipient msg = Message <$> sender <*> (Left <$> recipient) <*> pure (T.pack msg)

nickInput :: MonadWidget t m => m (Dynamic t (Maybe Nick))
nickInput = do
  n <- textInput def
  addNick <- button "Add Nick"
  nick <- holdDyn Nothing $ tag (validNick . T.pack <$> current (value n)) $ leftmost [addNick, textInputGetEnter n]
  (nickMsgAttr, nickMsg) <- splitDyn <=< forDyn nick $ \case
    Nothing -> ("style" =: "color: red;", "No nickname set!")
    Just n -> ("style" =: "color: green;", "Hi, " <> (T.unpack $ unNick n) <> "!")
  elDynAttr "small" nickMsgAttr $ dynText nickMsg
  return nick

recipientNickInput :: MonadWidget t m => m (Dynamic t (Maybe Nick))
recipientNickInput = do
  rec recipient <- mapDyn (validNick . T.pack) <=< fmap value $ textInput $ def & attributes .~ validationAttrs
      validationAttrs <- forDyn recipient $ \r -> if isNothing r then "style" =: "border: 1px red solid;" <> "placeholder" =: "Enter recipient" else mempty
  return recipient

validNick :: Text -> Maybe Nick
validNick t = if T.null (T.strip t) then Nothing else Just $ Nick t

