{-# LANGUAGE CPP, JavaScriptFFI, ScopedTypeVariables #-}

import Reflex.Dom
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T

#ifdef ghcjs_HOST_OS
import GHCJS.Types
import GHCJS.Foreign
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
createObjectURL _ r = Just . fromJSString <$> createObjectURL_ (unXhrResponseBody r)

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
main = mainWidget $ title >> testXhrResponseBody >> testXhrResponseText

testXhrResponseText :: forall t m. MonadWidget t m => m ()
testXhrResponseText = do
  pb <- getPostBuild
  header "_xhrResponse_responseText test" "Retrieves ./out.stats and displays resulting responseText."

  rt :: Event t (Maybe T.Text) <- fmap (fmap _xhrResponse_responseText) $ performRequestAsync $ XhrRequest "GET" "./out.stats" def <$ pb
  dynText <=< holdDyn "" $ fmapMaybe (T.unpack <$>) rt

  el "hr" $ return ()

testXhrResponseBody :: forall t m. (HasWebView m, MonadWidget t m) => m ()
testXhrResponseBody = do
  pb <- getPostBuild
  header "_xhrResponse_response test" "Sets responseType to 'blob', retrieves test.jpg, and displays response blob"
  el "hr" $ return ()

  wv <- askWebView -- JavaScriptCore requires a JS context to run FFI calls. This context can be derived from the WebView.

  let imgReq = XhrRequest "GET" "./test.jpg" $ def { _xhrRequestConfig_responseType = Just XhrResponseType_Blob }
  ri :: Event t (Maybe XhrResponseBody) <- fmap (fmap _xhrResponse_response) $ performRequestAsync $ imgReq <$ pb
  imageUrl <- fmap (fmapMaybe join) $ performEvent $ fmap (maybe (return Nothing) (fmap Just . liftIO . createObjectURL wv)) ri
  _ <- widgetHold (return ()) $ ffor imageUrl $ \u -> elAttr "img" ("src" =: u) $ return ()

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


