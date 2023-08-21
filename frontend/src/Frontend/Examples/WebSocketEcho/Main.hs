{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}

module Frontend.Examples.WebSocketEcho.Main where

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Reflex.Dom hiding (mainWidget)
import Reflex.Dom.Core (mainWidget)

main :: IO ()
main = run $ mainWidget app

app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , Prerender t m
     )
  => m ()
app = do
  header
  rec t <- inputElement $ def & inputElementConfig_setValue .~ fmap (const "") newMessage
      b <- button "Send"
      let newMessage = fmap ((:[]) . encodeUtf8)
            $ tag (current $ value t)
            $ leftmost [b, keypress Enter t]

  receivedMessages <- fmap join $ prerender (return (constDyn [])) $ do
    ws <- webSocket "wss://ws.ifelse.io" $ def
      & webSocketConfig_send .~ newMessage
    foldDyn (\m ms -> ms ++ [m]) [] $ _webSocket_recv ws

  el "p" $ text "Responses from the WebSocket echo service:"
  _ <- el "ul"
         $ simpleList receivedMessages
         $ \m -> el "li" $ dynText $ fmap decodeUtf8 m
  return ()

header :: DomBuilder t m => m ()
header = do
  el "strong" $ do
    text " WebSocket test page"
  el "p" $ do
    text "Send a message to the WebSocket echo service (https://ws.ifelse.io/):"
