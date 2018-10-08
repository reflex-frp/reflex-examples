{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.ByteString    as B
import qualified Data.Map           as Map
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Reflex.Dom         hiding (mainWidget)
import           Reflex.Dom.Core    (mainWidget)

main :: IO ()
main = run $ mainWidget app

--------------------------------------------------------------------------------

app :: forall t m. MonadWidget t m => m ()
app = do
  header
  rec eJoinMessage <- elDynAttr "div" dJoinHidden $ mdo
          tn <- textInput $ def & setValue .~ fmap (const "") eJMsg
          bn <- button " Join username!"
          -- Clean the name a bit (todo, clean more):
          let eNewName :: Event t T.Text = fmap T.strip
                $ tag (current $ value tn)
                $ leftmost [bn, keypress Enter tn]
              eJMsg = fmap ((:[]) . encodeUtf8 . ("Hi! I am " <>)) eNewName
          pure eJMsg
      el "br" blank
      eTxtMessage <- elDynAttr "div" dSendMsgHidden $ mdo
          t <- textInput $ def & setValue .~ fmap (const "") newMessage
          b <- button "Send"
          let newMessage :: Event t [B.ByteString] = fmap ((:[]) . encodeUtf8)
                $ tag (current $ value t)
                $ leftmost [b, keypress Enter t]
          pure newMessage
      let nM = leftmost [eJoinMessage, eTxtMessage]
      ws <- webSocket "ws://localhost:8000" $ def & webSocketConfig_send .~ nM
      let eReceiveResponse = _webSocket_recv ws
      dSendMsgHidden <-foldDyn removeHidden hidden eReceiveResponse
      let dJoinHidden = fmap oppHidden dSendMsgHidden
      receivedMessages <- foldDyn (\m ms -> ms ++ [m]) [] eReceiveResponse
      el "p" $ text "Responses from the backend chat -server:"
      _ <- el "ul"
             $ simpleList receivedMessages
             $ \m -> el "li" $ dynText $ fmap decodeUtf8 m
      blank
  footer
    where
      hidden = "hidden" =: "hidden"
      oppHidden :: Map.Map T.Text T.Text -> Map.Map T.Text T.Text
      oppHidden am =
          if am == Map.empty
             then hidden
             else Map.empty
      removeHidden :: B.ByteString
                   -> Map.Map T.Text T.Text
                   -> Map.Map T.Text T.Text
      removeHidden bs amap =
        if B.isPrefixOf "Welcome!" bs
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
