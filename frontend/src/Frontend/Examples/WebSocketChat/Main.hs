{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}

module Frontend.Examples.WebSocketChat.Main where

import qualified Data.Aeson as Aeson
import           Data.ByteString    as B
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.Functor.Sum
import           Data.List.NonEmpty
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           Data.Text (Text)
import           GHCJS.DOM.HTMLElement       (focus)
import           Language.Javascript.JSaddle
import           Obelisk.Route
import           Reflex
import           Reflex.Dom
import           Control.Monad.Fix (MonadFix)
import           Control.Monad      (void)
import           Text.URI

--------------------------------------------------------------------------------
import           Common.Examples.WebSocketChat.Message
import           Common.Route
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
     , Prerender js t m
     )
  => Maybe Text
  -> m ()
app r = do
  rec
    msgEvDyn <- widgetHold loginWidget (messagingWidget <$ loggedInEv)
    let
      msgSendEv = switch (current msgEvDyn)
      msgRecEv = fmapMaybe decodeOneMsg wsRespEv
      eRecRespTxt = fmap showMsg msgRecEv
      loggedInEv = fmapMaybe loginEv msgRecEv
    wsRespEv <- fmap switchDyn $ prerender (return never) $ do
      case checkEncoder backendRouteEncoder of
        Left err -> do
          el "div" $ text err
          return never
        Right encoder -> do
          let wsPath = fst $ encode encoder $ FullRoute_Backend BackendRoute_WebSocketChat :/ ()
              sendEv = fmap ((:[]) . toStrict . Aeson.encode) msgSendEv
          let mUri = do
                uri' <- mkURI =<< r
                pathPiece <- nonEmpty =<< mapM mkPathPiece wsPath
                wsScheme <- case uriScheme uri' of
                  rtextScheme | rtextScheme == mkScheme "https" -> mkScheme "wss"
                  rtextScheme | rtextScheme == mkScheme "http" -> mkScheme "ws"
                  _ -> Nothing
                return $ uri'
                  { uriPath = Just (False, pathPiece)
                  , uriScheme = Just wsScheme
                  }
          case mUri of
            Nothing -> return never
            Just uri -> do
              ws <- webSocket (render uri) $ def & webSocketConfig_send .~ sendEv
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
    decodeOneMsg = Aeson.decode . fromStrict

    showMsg :: S2C -> Text
    showMsg = \case
      (S2Cbroadcast txt) -> txt
      (S2Cwelcome txt)  -> "Welcome! Users: " <> txt
      S2Cuserexists     -> "User already exists"
      S2Cnameproblem    -> "Name cannot contain punctuation or "

loginWidget
  :: ( DomBuilder t m
     , Prerender js t m
     )
  => m (Event t C2S)
loginWidget = fmap switchDyn . el "div" $ prerender (pure never) $ do
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
     , Prerender js t m
     )
  => m (Event t C2S)
messagingWidget = fmap switchDyn . el "div" $ prerender (pure never) $ do
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
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadJSM (Performable m)
     )
  => InputElement EventResult (DomBuilderSpace m) t
  -> m ()
doFocus ie = do
  pb <- getPostBuild
  let h = _inputElement_raw ie
  performEvent_ (fmap (liftJSM . const (focus h)) pb)
  return ()
