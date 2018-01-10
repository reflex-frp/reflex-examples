{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Aeson         (encode,decode)
import           Data.ByteString    as B
import           Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.Map           as Map
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           GHCJS.DOM.HTMLElement       (focus)
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom         hiding (mainWidget)
import           Reflex.Dom.Core    (mainWidget)

--------------------------------------------------------------------------------
import           CommonWsChat
--------------------------------------------------------------------------------

main :: IO ()
main = run $ mainWidget app

--------------------------------------------------------------------------------

-- TODO
--  - factor out the performEvents (see keyboard -example)
--  - factor out the message forming
--  - factor out the textInput-button combos
--  - add close connection button and associated message handling
app :: forall t m. MonadWidget t m => m ()
app = do
  pb <- delay 0 =<< getPostBuild
  header
  rec eJoinMessage <- elDynAttr "div" dJoinHidden $ mdo
          tn <- textInput $ def & setValue .~ fmap (const "") eJMsg
          bn <- button " Join username!"
          let htmlTn = _textInput_element tn
          _ <- performEvent (fmap (liftJSM . const (focus htmlTn)) pb)
          -- Clean the name a bit (todo, clean more):
          let eNewName :: Event t T.Text = fmap T.strip
                $ tag (current $ value tn)
                $ leftmost [bn, keypress Enter tn]
              eJMsg = fmap ((:[]) . toStrict . encode . C2Sjoin) eNewName
          pure eJMsg
      el "br" blank
      eTxtMessage <- elDynAttr "div" dSendMsgHidden $ mdo
          t <- textInput $ def & setValue .~ fmap (const "") newMessage
          let h = _textInput_element t
          eJm <- delay 0.1 eJoinMessage
          _ <- performEvent (fmap (liftJSM . const (focus h)) eJm)
          b <- button "Send"
          let newMessage = fmap ((:[]) . toStrict . encode . C2Smsg)
                $ tag (current $ value t)
                $ leftmost [b, keypress Enter t]
          pure newMessage
      let nM = leftmost [eJoinMessage, eTxtMessage]
      ws <- webSocket "ws://localhost:8000" $ def & webSocketConfig_send .~ nM
      let eReceiveResponse = _webSocket_recv ws
          eRecRespTxt = fmap bs2txt eReceiveResponse
      dSendMsgHidden <-foldDyn removeHidden hidden eRecRespTxt
      let dJoinHidden = fmap oppHidden dSendMsgHidden
      receivedMessages <- foldDyn (\m ms -> ms ++ [m]) [] eRecRespTxt
      el "p" $ text "Responses from the backend chat -server:"
      _ <- el "ul"
             $ simpleList receivedMessages
             $ \m -> el "li" $ dynText m
      blank
  footer
    where
      bs2txt :: B.ByteString -> T.Text
      bs2txt msgbs =
          case msgS of
              Just (S2Cbroadcast txt) -> txt
              Just (S2Cwelcome txt)  -> "Welcome! Users: " <> txt
              Just S2Cuserexists     -> "User already exists"
              Just S2Cnameproblem    -> "Name cannot contain punctuation or "
                                        <> " whitespace, and cannot be empty"
              Nothing                -> "decoded srv msg is nothing"
          where
            msgS = decode $ fromStrict msgbs :: Maybe S2C
      hidden = "hidden" =: "hidden"
      oppHidden :: Map.Map T.Text T.Text -> Map.Map T.Text T.Text
      oppHidden am =
          if am == Map.empty
             then hidden
             else Map.empty
      removeHidden :: T.Text
                   -> Map.Map T.Text T.Text
                   -> Map.Map T.Text T.Text
      removeHidden bs amap =
        if T.isPrefixOf "Welcome!" bs
           then Map.empty
           else amap

--------------------------------------------------------------------------------

linkNewTab :: MonadWidget t m => T.Text -> T.Text -> m ()
linkNewTab href s = elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

header :: MonadWidget t m => m ()
header = do
  el "strong" $ do
    linkNewTab
        "https://github.com/reflex-frp/reflex-platform/reflex-dom" "Reflex.Dom"
    text " WebSocket test page"
  el "p" $ do
    text "Send a message to the "
    -- linkNewTab "https://www.websocket.org/echo.html" "WebSocket.org"
    text "websocket chat server:"

footer :: MonadWidget t m => m ()
footer = do
  el "hr" blank
  el "p" $ do
    text "The code for this example can be found in the "
    linkNewTab "https://github.com/reflex-frp/reflex-examples" "Reflex Examples"
    text " repo."
