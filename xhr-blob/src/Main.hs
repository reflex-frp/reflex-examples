{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import           Control.Monad            ((<=<))
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.Text                as T
import           Data.Semigroup           ((<>))
import           Reflex.Dom               hiding (mainWidget)
import           Reflex.Dom.Core          (mainWidget)

import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.String (textFromJSString)

import qualified JSDOM.Types as G (liftJSM, fromJSVal, toJSVal
  , JSVal, JSString, Blob)
import           JSDOM.URL (newURL, createObjectURL)
-- import qualified GHCJS.DOM.Types as G (liftJSM, fromJSVal, toJSVal
--   , JSVal, JSString, Blob)
-- import           GHCJS.DOM.URL (newURL, createObjectURL)


-- From the old version:
-- #ifdef ghcjs_HOST_OS
-- import           GHCJS.Foreign
-- import           GHCJS.Types
-- #else
-- import           Foreign.Ptr
-- import           GI.WebKit2 (WebView)
-- import           Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
-- import           Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
-- import           Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
-- import           Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame
-- import           Graphics.UI.Gtk.WebKit.WebView
-- import           System.Glib.FFI
-- #endif

-- Below are two bindings to window.URL.createObjectURL, one using GHCJS for use
-- in the browser and another using JavaScriptCore for use in webkitgtk
-- https://developer.mozilla.org/en-US/docs/Web/API/URL/createObjectURL
-- #ifdef ghcjs_HOST_OS
-- -- GHCJS javascript FFI binding of window.URL.createObjectURL
-- foreign import javascript unsafe "window['URL']['createObjectURL']($1)" createObjectURL_ :: JSRef XhrResponseBody -> IO JSString

-- -- The first argument is unnecessary here, but necessary in the webkitgtk binding
-- createObjectURL :: a -> XhrResponseBody -> IO (Maybe String)
-- createObjectURL _ r = Just . fromJSString <$> createObjectURL_ (unXhrResponseBody r)

-- #else
-- -- JavaScriptCore binding of createObjectURL for use with webkitgtk
-- createObjectURL :: WebView -> XhrResponseBody -> IO (Maybe String)
-- createObjectURL wv r = do
--   c <- webFrameGetGlobalContext =<< webViewGetMainFrame wv
--   o <- jsstringcreatewithutf8cstring "window['URL']['createObjectURL'](this)" >>= (\s -> jsevaluatescript c s (unXhrResponseBody r) nullPtr 1 nullPtr)
--   isNull <- jsvalueisnull c o
--   case isNull of
--     True -> return Nothing
--     False -> do
--       j <- jsvaluetostringcopy c o nullPtr
--       l <- jsstringgetmaximumutf8cstringsize j
--       s <- allocaBytes (fromIntegral l) $ \ps -> do
--              _ <- jsstringgetutf8cstring'_ j ps (fromIntegral l)
--              peekCString ps
--       return $ Just s

-- #endif

-- mozilla.org:
-- Js_EvaluateScript is obsolote, params (my guess about the correspondence)
--  c       = cx = context
--  "body"  = obj = scope in which to run the script
--  s       = src = string to compile and execute
--  1       = length = of src
--  nullPtr = filename = this name is used in error msgs
--  nullPtr = lineno = also used with error msgs
--
--  rval = receives the result value


-- GHCJS javascript FFI binding of window.URL.createObjectURL
#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
    "window['URL']['createObjectURL']($1)" createObjectURL2_
        :: G.JSVal -> IO G.JSString
#else
createObjectURL2_ :: G.JSVal -> IO G.JSString
createObjectURL2_ _ = return undefined
#endif

createObjectURL2 :: G.Blob -> JSM T.Text
createObjectURL2 b = do
    jv <- G.liftJSM $ G.toJSVal b
    cou <- liftIO $ createObjectURL2_ jv
    pure $ textFromJSString cou


main :: IO ()
main = run $ mainWidget app

app :: forall t m. MonadWidget t m => m ()
-- app = title >> testXhrResponseText
app = title >> testXhrResponseBody >> testXhrResponseText

myHost :: T.Text
myHost = "."
-- myHost = "http://localhost:8000"
-- myHost = "http://192.168.0.x:8000"

testXhrResponseText :: forall t m. MonadWidget t m => m ()
testXhrResponseText = do
  pb <- getPostBuild
  header "_xhrResponse_responseText test"
    "Retrieves ./out.stats and displays resulting responseText."

  rt :: Event t (Maybe T.Text) <- fmap (fmap _xhrResponse_responseText)
            $ performRequestAsync
            $ XhrRequest "GET" (myHost <> "/out.stats") def <$ pb
  dynText <=< holdDyn "" $ fmapMaybe id rt
  el "hr" $ return ()


testXhrResponseBody :: forall t m. (HasWebView m, MonadWidget t m) => m ()
testXhrResponseBody = do
  pb <- getPostBuild
  header "_xhrResponse_response test"
    "Sets responseType to 'blob', retrieves test.jpg, and displays response blob"
  el "hr" $ return ()

  -- From the older version:
  -- wv <- askWebView -- JavaScriptCore requires a JS context to run FFI calls.
  -- This context can be derived from the WebView.

  let imgReq = XhrRequest "GET" (myHost <> "/test.jpg")
            $ def { _xhrRequestConfig_responseType = Just XhrResponseType_Blob }
  ri :: Event t (Maybe XhrResponseBody) <- fmap (fmap _xhrResponse_response)
            $ performRequestAsync $ imgReq <$ pb
  text "use XhrResponseBody and jsffi"
  -- url <- U.newURL ("./test.jpg" :: T.Text) -- this fails
  url <- newURL (myHost <> "/test.jpg")
  _ <- el "div" . widgetHold (text "body is empty / no ev?") . ffor ri
    $ \mb -> do
        mbb <- G.liftJSM $ fromXhrRB2 mb
        mImgUrl <-
            case mbb of
                Just b -> do
                    -- co <- createObjectURL url b
                    -- The above one compiles but doesn't work with ghcjs.
                    -- Console says something about undefined etc. wiht JSDOM.
                    -- With GHCJS.DOM console says the it is not a function.
                    --
                    co <- G.liftJSM $ createObjectURL2 b
                    pure $ Just co
                Nothing -> pure Nothing
        case mImgUrl of
            Just iu -> elAttr "img" ("src" =: iu) blank
            Nothing -> blank
        blank
  el "hr" blank
  where
      fromXhrRB2 :: Maybe XhrResponseBody -> JSM (Maybe G.Blob)
      fromXhrRB2 (Just (XhrResponseBody_Blob b)) =
          G.liftJSM $ (G.fromJSVal <=< G.toJSVal) b
      fromXhrRB2 _ = pure Nothing


title :: forall t m. MonadWidget t m => m ()
title = do
  el "h1" $ text "Reflex.Dom XhrResponseBody Test"
  el "hr" $ return ()

header :: MonadWidget t m => T.Text -> T.Text -> m ()
header h2 p = do
  el "h2" $ text h2
  el "p" $ text p
  el "hr" $ return ()


