{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings #-}

import Reflex.Dom
import Reflex.Host.Class
import Data.Dependent.Sum
import Data.List
import Data.Monoid
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Identity
import Control.Monad.Trans
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.DOM.FileReader
import GHCJS.DOM.Types (File, UIEvent, liftJSM)
import GHCJS.DOM.EventM

main :: IO ()
main = mainWidget $ do
  header
  filesDyn <- value <$> fileInput def
  urlE <- fmap (ffilter ("data:image" `T.isPrefixOf`)) . dataURLFileReader . fmapMaybe listToMaybe . updated $ filesDyn
  el "div" . widgetHold blank . ffor urlE $ \url ->
    elAttr "img" ("src" =: url <> "style" =: "max-width: 80%") blank
  footer

linkNewTab :: MonadWidget t m => Text -> Text -> m ()
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

dataURLFileReader :: (MonadWidget t m) => Event t File -> m (Event t Text)
dataURLFileReader request =
  do fileReader <- liftJSM newFileReader
     performEvent_ (fmap (\f -> readAsDataURL fileReader (Just f)) request)
     e <- wrapDomEvent fileReader (`on` load) . liftJSM $ do
       v <- getResult fileReader
       s <- (fromJSVal <=< toJSVal) v
       -- return . fmap T.pack $ s
       return s
     return (fmapMaybe id e)
