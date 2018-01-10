{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}

import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Reflex.Dom         hiding (mainWidget)
import           Reflex.Dom.Core    (mainWidget)

main :: IO ()
main = run $ mainWidget app

app :: forall t m. MonadWidget t m => m ()
app = do
  header
  rec t <- textInput $ def & setValue .~ fmap (const "") newMessage
      b <- button "Send"
      let newMessage = fmap ((:[]) . encodeUtf8)
            $ tag (current $ value t)
            $ leftmost [b, keypress Enter t]
  ws <- webSocket "ws://echo.websocket.org"
         $ def & webSocketConfig_send .~ newMessage
  receivedMessages <- foldDyn (\m ms -> ms ++ [m]) []
         $ _webSocket_recv ws
  el "p" $ text "Responses from the WebSocket.org echo service:"
  _ <- el "ul"
         $ simpleList receivedMessages
         $ \m -> el "li" $ dynText $ fmap decodeUtf8 m
  footer

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
    linkNewTab "https://www.websocket.org/echo.html" "WebSocket.org"
    text "'s websocket echo service:"

footer :: MonadWidget t m => m ()
footer = do
  el "hr" blank
  el "p" $ do
    text "The code for this example can be found in the "
    linkNewTab "https://github.com/reflex-frp/reflex-examples" "Reflex Examples"
    text " repo."
