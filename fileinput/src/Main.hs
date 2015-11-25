{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom
import Data.Monoid
import Data.Maybe
import Control.Monad.Trans
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.DOM.Types hiding (Event)
import GHCJS.DOM.EventM

main :: IO ()
main = mainWidget $ do
  header
  filesDyn <- value <$> fileInput def
  urlE <- dataURLFileReader (fmapMaybe listToMaybe (updated filesDyn))
  el "div" . widgetHold blank . ffor urlE $ \url ->
    elAttr "img" ("src" =: url) blank
  footer

linkNewTab :: MonadWidget t m => String -> String -> m ()
linkNewTab href s =
  elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

header :: MonadWidget t m => m ()
header = do
  el "strong" $ do
    linkNewTab "https://github.com/ryantrinkle/reflex-dom" "Reflex.Dom"
    text " FileInput test page"
  el "p" $ do
    text "Select an image file."

footer :: MonadWidget t m => m ()
footer = do
  el "hr" $ return ()
  el "p" $ do
    text "The code for this example can be found in the "
    linkNewTab "https://github.com/reflex-frp/reflex-examples" "Reflex Examples"
    text " repo."

---- Implementation of FileReader stuff, pending an update of reflex-dom ----
---------------- to using a newer version of ghcjs-dom ----------------------

newtype FileReader = FileReader { unFileReader :: JSRef FileReader }

instance ToJSRef FileReader where
  toJSRef (FileReader r) = return r
  {-# INLINE toJSRef #-}

instance FromJSRef FileReader where
  fromJSRef = return . fmap FileReader . maybeJSNull
  {-# INLINE fromJSRef #-}

instance GObjectClass FileReader where
  toGObject = GObject . castRef . unFileReader
  unsafeCastGObject = FileReader . castRef . unGObject

foreign import javascript unsafe "new FileReader()"
        js_newFileReader :: IO FileReader

newFileReader :: MonadIO m => m FileReader
newFileReader = liftIO js_newFileReader

foreign import javascript unsafe "$1[\"result\"]"
        js_getResult :: FileReader -> IO JSString

getResult :: MonadIO m => FileReader -> m String
getResult fr = liftIO (fmap fromJSString (js_getResult fr))

foreign import javascript unsafe "$1[\"readAsDataURL\"]($2)"
        js_readAsDataURL :: FileReader -> File -> IO ()

readAsDataURL :: (MonadIO m) => FileReader -> File -> m ()
readAsDataURL fr f = liftIO (js_readAsDataURL fr f)

buildEvent :: (MonadWidget t m)
           => ((a -> IO ()) -> WidgetHost m r)
           -> m (Event t a)
buildEvent install =
  do pb <- getPostBuild
     performEventAsync . (<$ pb) $ \k ->
       install k >> return ()

dataURLFileReader :: (MonadWidget t m) => Event t File -> m (Event t String)
dataURLFileReader request =
  do fileReader <- liftIO newFileReader
     let handler :: (String -> IO ()) -> EventM UIEvent FileReader ()
         handler k = liftIO $ k =<< getResult fileReader
     performEvent_ (fmap (\f -> readAsDataURL fileReader f) request)
     buildEvent (liftIO . connect "load" fileReader . handler)

-------------------------------------------------------------------------------
