{-# LANGUAGE RecursiveDo #-}

import Reflex.Dom
import Reflex.Host.Class
import Data.Dependent.Sum
import Data.List
import Data.Monoid
import Data.Maybe
import Control.Monad.Identity
import Control.Monad.Trans
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.DOM.FileReader
import GHCJS.DOM.Types hiding (Event)
import GHCJS.DOM.EventM

main :: IO ()
main = mainWidget $ do
  header
  filesDyn <- value <$> fileInput def
  urlE <- fmap (ffilter ("data:image" `isPrefixOf`)) . dataURLFileReader . fmapMaybe listToMaybe . updated $ filesDyn
  el "div" . widgetHold blank . ffor urlE $ \url ->
    elAttr "img" ("src" =: url <> "style" =: "max-width: 80%") blank
  footer

linkNewTab :: MonadWidget t m => String -> String -> m ()
linkNewTab href s =
  elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

header :: MonadWidget t m => m ()
header = do
  el "strong" $ do
    linkNewTab "https://github.com/reflex-frp/reflex-dom" "Reflex.Dom"
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

askPostEvent :: MonadWidget t m => m (EventTrigger t a -> a -> IO ())
askPostEvent = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  return (\t a -> postGui $ runWithActions [t :=> Identity a])


askPostEvent :: MonadWidget t m => m (EventTrigger t a -> a -> IO ())
askPostEvent = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  return (\t a -> postGui $ runWithActions [t :=> a])


buildEvent :: (MonadWidget t m)
           => ((a -> IO ()) -> IO (IO ()))
           -> m (Event t a)
buildEvent install = do
  postEvent <- askPostEvent
  newEventWithTrigger (install . postEvent)

dataURLFileReader :: (MonadWidget t m) => Event t File -> m (Event t String)
dataURLFileReader request =
  do fileReader <- liftIO newFileReader
     let getResultString :: FileReader -> IO (Maybe String)
         getResultString fr = do v <- getResult fr
                                 s <- fromJSVal v
                                 return (fmap fromJSString s)
         handler :: (Maybe String -> IO ()) -> EventM FileReader UIEvent ()
         handler k = liftIO $ k =<< getResultString fileReader
     performEvent_ (fmap (\f -> readAsDataURL fileReader (Just f)) request)
     e <- buildEvent (on fileReader load . handler)
     return (fmapMaybe id e)

