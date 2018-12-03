{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Examples.WebSocketChat.Main where

import           Control.Monad.IO.Class
import           Data.Aeson         (encode,decode)
import           Data.ByteString    as B
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           Data.Text (Text)
import           GHCJS.DOM.HTMLElement       (focus)
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom         hiding (mainWidget)
import           Reflex.Dom.Core    (mainWidget)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad      (void)
import qualified Obelisk.ExecutableConfig as Cfg

--------------------------------------------------------------------------------
import           Common.Examples.WebSocketChat.Message
--------------------------------------------------------------------------------

main :: IO ()
main = run $ mainWidget app

--------------------------------------------------------------------------------

-- TODO
--  - factor out the performEvents (see keyboard -example)
--  - factor out the message forming
--  - factor out the textInput-button combos
--  - add close connection button and associated message handling
app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     )
  => m ()
app = do
  rec
    msgEvDyn <- widgetHold loginWidget (messagingWidget <$ loggedInEv)
    let
      msgSendEv = switch (current msgEvDyn)
      msgRecEv = fmapMaybe decodeOneMsg wsRespEv
      eRecRespTxt = fmap showMsg msgRecEv
      loggedInEv = fmapMaybe loginEv msgRecEv
    wsRespEv <- prerender (return never) $ do
      let sendEv = fmap ((:[]) . toStrict . encode) msgSendEv
      Just r <- liftIO $ Cfg.get "config/common/route"
      ws <- webSocket (T.strip r <> "/websocketchat") $ def & webSocketConfig_send .~ sendEv
      return (_webSocket_recv ws)
    receivedMessages <- foldDyn (\m ms -> ms ++ [m]) [] eRecRespTxt
    void $ el "div" $ do
      el "p" $ text "Responses from the backend chat -server:"
      el "ul" $ simpleList receivedMessages (\m -> el "li" $ dynText m)
  blank
  where
    loginEv = \case
      (S2Cwelcome _) -> Just ()
      _ -> Nothing

    decodeOneMsg :: B.ByteString -> Maybe S2C
    decodeOneMsg = decode . fromStrict

    showMsg :: S2C -> Text
    showMsg = \case
      (S2Cbroadcast txt) -> txt
      (S2Cwelcome txt)  -> "Welcome! Users: " <> txt
      S2Cuserexists     -> "User already exists"
      S2Cnameproblem    -> "Name cannot contain punctuation or "

loginWidget
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => m (Event t C2S)
loginWidget = el "div" $ do
  rec
    tn <- inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") eNewName
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Enter Username")
    doFocus tn
    bn <- button "Join"
    -- Clean the name a bit (todo, clean more):
    let eNewName = fmap T.strip
          $ tag (current $ value tn)
          $ leftmost [bn, keypress Enter tn]
  return $ C2Sjoin <$> eNewName

messagingWidget
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => m (Event t C2S)
messagingWidget = el "div" $ do
  rec
    t <- inputElement $ def & inputElementConfig_setValue .~ fmap (const "") newMessage
    doFocus t
    b <- button "Send"
    let newMessage = tag (current $ value t)
          $ leftmost [b, keypress Enter t]
  return $ C2Smsg <$> newMessage

doFocus
  :: ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js m
     )
  => InputElement EventResult (DomBuilderSpace m) t
  -> m ()
doFocus ie = prerender (return ()) $ do
  pb <- getPostBuild
  let h = _inputElement_raw ie
  performEvent_ (fmap (liftJSM . const (focus h)) pb)
  return ()
