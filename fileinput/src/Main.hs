{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Monad         ((<=<))
import           Data.Maybe            (listToMaybe)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHCJS.DOM.EventM      (on)
import           GHCJS.DOM.FileReader  (newFileReader, readAsDataURL, load
                                       , getResult)
import           GHCJS.DOM.Types       (File (..))
import           Language.Javascript.JSaddle
import           Reflex.Dom            hiding (mainWidget)
import           Reflex.Dom.Core       (mainWidget)

main :: IO ()
main = run $ mainWidget app

app :: forall t m. MonadWidget t m => m ()
app = do
  header
  filesDyn <- value <$> fileInput def
  urlE <- fmap (ffilter ("data:image" `T.isPrefixOf`))
      . dataURLFileReader
      . fmapMaybe listToMaybe
      . updated $ filesDyn
  _ <-el "div"
      . widgetHold blank
      . ffor urlE $ \url ->
          elAttr "img" ("src" =: url <> "style" =: "max-width: 80%") blank
  footer

dataURLFileReader :: (MonadWidget t m) => Event t File -> m (Event t Text)
dataURLFileReader request =
  do fileReader <- liftJSM newFileReader
     performEvent_ (fmap (readAsDataURL fileReader .Just) request)
     e <- wrapDomEvent fileReader (`on` load) . liftJSM $ do
       v <- getResult fileReader
       (fromJSVal <=< toJSVal) v
     return (fmapMaybe id e)

linkNewTab :: MonadWidget t m => Text -> Text -> m ()
linkNewTab href s =
  elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

header :: MonadWidget t m => m ()
header = do
  el "strong" $ do
    linkNewTab "https://github.com/reflex-frp/reflex-dom" "Reflex.Dom"
    text " FileInput test page"
  el "p" $
    text "Select an image file."

footer :: MonadWidget t m => m ()
footer = do
  el "hr" $ return ()
  el "p" $ do
    text "The code for this example can be found in the "
    linkNewTab "https://github.com/reflex-frp/reflex-examples" "Reflex Examples"
    text " repo."

