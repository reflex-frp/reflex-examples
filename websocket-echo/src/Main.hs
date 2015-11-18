{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Monoid

main :: IO ()
main = mainWidget $ do
  el "strong" $ do
    elAttr "a" ("href" =: "https://github.com/ryantrinkle/reflex-dom" <> "target" =: "_blank") $ text "Reflex.Dom"
    text " WebSocket test page"
  el "p" $ do
    text "Send a message to the "
    elAttr "a" ("href" =: "https://www.websocket.org/echo.html" <> "target" =: "_blank") $ text "WebSocket.org"
    text "'s websocket echo service:"
  rec t <- textInput $ def & setValue .~ fmap (const "") newMessage
      b <- button "Send"
      let newMessage = fmap ((:[]) . encodeUtf8 . T.pack) $ tag (current $ value t) $ leftmost [b, textInputGetEnter t]
  ws <- webSocket "ws://echo.websocket.org" $ def & webSocketConfig_send .~ newMessage
  receivedMessages <- foldDyn (\a b -> b ++ [a]) [] $ _webSocket_recv ws
  el "p" $ text "Responses from the WebSocket.org echo service:"
  el "ul" $ simpleList receivedMessages $ \m -> el "li" $ dynText =<< mapDyn (T.unpack . decodeUtf8) m
  return ()
