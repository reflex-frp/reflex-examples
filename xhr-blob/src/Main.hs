{-# LANGUAGE CPP, JavaScriptFFI, ScopedTypeVariables, LambdaCase #-}

import Reflex.Dom
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T

#ifdef ghcjs_HOST_OS
import GHCJS.Types
import GHCJS.Foreign
import Data.JSString (unpack)
#else
import Foreign.Ptr
import System.Glib.FFI
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame
#endif

-- Below are two bindings to window.URL.createObjectURL, one using GHCJS for use in the browser and another using JavaScriptCore for use in webkitgtk
-- https://developer.mozilla.org/en-US/docs/Web/API/URL/createObjectURL
#ifdef ghcjs_HOST_OS
-- GHCJS javascript FFI binding of window.URL.createObjectURL
foreign import javascript unsafe "window['URL']['createObjectURL']($1)" createObjectURL_ :: JSRef XhrResponseBody -> IO JSString

-- The first argument is unnecessary here, but necessary in the webkitgtk binding
createObjectURL :: a -> XhrResponseBody -> IO (Maybe String)
createObjectURL _ r = Just . unpack <$> createObjectURL_ (unXhrResponseBody r)

#else
-- JavaScriptCore binding of createObjectURL for use with webkitgtk
createObjectURL :: WebView -> XhrResponseBody -> IO (Maybe String)
createObjectURL wv r = do
  c <- webFrameGetGlobalContext =<< webViewGetMainFrame wv
  o <- jsstringcreatewithutf8cstring "window['URL']['createObjectURL'](this)" >>= (\s -> jsevaluatescript c s (unXhrResponseBody r) nullPtr 1 nullPtr)
  isNull <- jsvalueisnull c o
  case isNull of
    True -> return Nothing
    False -> do
      j <- jsvaluetostringcopy c o nullPtr
      l <- jsstringgetmaximumutf8cstringsize j
      s <- allocaBytes (fromIntegral l) $ \ps -> do
             _ <- jsstringgetutf8cstring'_ j ps (fromIntegral l)
             peekCString ps
      return $ Just s

#endif

main :: IO ()
main = mainWidget $ title >> testXhrResponseBody >> testXhrResponseText >> testXhrExceptions

testXhrResponseText :: forall t m. MonadWidget t m => m ()
testXhrResponseText = do
  pb <- getPostBuild
  header "_xhrResponse_responseText test" "Retrieves ./out.stats and displays resulting responseText."

  rt :: Event t (Either XhrException XhrResponse) <- performRequestAsync $ XhrRequest "GET" "./out.stats" def <$ pb
  dynText <=< holdDyn "" $ fforMaybe rt $ \case
    Right r -> fmap T.unpack $ _xhrResponse_responseText r
    Left ex -> Just "Exception"

  el "hr" $ return ()

testXhrResponseBody :: forall t m. (HasWebView m, MonadWidget t m) => m ()
testXhrResponseBody = do
  pb <- getPostBuild
  header "_xhrResponse_response test" "Sets responseType to 'blob', retrieves test.jpg, and displays response blob"
  el "hr" $ return ()

  wv <- askWebView -- JavaScriptCore requires a JS context to run FFI calls. This context can be derived from the WebView.

  let imgReq = XhrRequest "GET" "./test.jpg" $ def { _xhrRequestConfig_responseType = Just XhrResponseType_Blob }
  ri :: Event t (Either XhrException XhrResponse) <- performRequestAsync $ imgReq <$ pb
  imageUrl <- fmap (fmapMaybe id) $ performEvent $ ffor ri $ \ri' -> case fmap _xhrResponse_response ri' of
      Right (Just i) -> liftIO $ createObjectURL wv i
      _ -> return $ Nothing
  _ <- widgetHold (return ()) $ ffor imageUrl $ \u -> elAttr "img" ("src" =: u) $ return ()

  el "hr" $ return ()

testXhrExceptions :: (HasWebView m, MonadWidget t m) => m ()
testXhrExceptions = do
  pb <- getPostBuild
  header "XhrException test" "Tries to retrieve resources at invalid URLs and displays exception image/message."
  el "hr" $ return ()

  wv <- askWebView

  let imgReq = XhrRequest "GET" "http://the-fakest-host.abcdef/not-there.jpg" $ def { _xhrRequestConfig_responseType = Just XhrResponseType_Blob }
  ri :: Event t (Either XhrException XhrResponse) <- performRequestAsync $ imgReq <$ pb
  imageUrl <- fmap (fmapMaybe id) $ performEvent $ ffor ri $ \ri' -> case fmap _xhrResponse_response ri' of
      Right (Just i) -> liftIO $ createObjectURL wv i
      _ -> return $ Just "./error.png"
  _ <- widgetHold (return ()) $ ffor imageUrl $ \u -> elAttr "img" ("src" =: u) $ return ()

  el "hr" $ return ()

  rt :: Event t (Either XhrException XhrResponse) <- performRequestAsync $ XhrRequest "GET" "http://the-fakest-host.abcdef/not-there.txt" def <$ pb
  dynText <=< holdDyn "" $ fforMaybe rt $ \case
    Right r -> fmap T.unpack $ _xhrResponse_responseText r
    Left ex -> Just $ "Exception handled: " ++ show ex

  el "hr" $ return ()

title :: forall t m. MonadWidget t m => m ()
title = do
  el "h1" $ text "Reflex.Dom XhrResponseBody Test"
  el "hr" $ return ()

header :: MonadWidget t m => String -> String -> m ()
header h2 p = do
  el "h2" $ text h2
  el "p" $ text p
  el "hr" $ return ()


