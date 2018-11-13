{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies        #-}

module Frontend.Examples.FileReader.Main where

import           Control.Monad         ((<=<))
import           Data.Maybe            (listToMaybe)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHCJS.DOM.EventM      (on)
import           GHCJS.DOM.FileReader  (newFileReader, readAsDataURL, load
                                       , getResult)
import           Language.Javascript.JSaddle
import           Reflex.Dom            hiding (mainWidget)
import           Reflex.Dom.Core       (mainWidget)

main :: IO ()
main = run $ mainWidget app

app
  :: ( DomBuilder t m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     )
  => m ()
app = do
  header
  filesDyn <- fileInputElement
  urlE <- fmap (ffilter ("data:image" `T.isPrefixOf`))
      . dataURLFileReader
      . fmapMaybe listToMaybe
      . updated $ filesDyn
  el "br" blank
  el "div"
      . widgetHold blank
      . ffor urlE $ \url ->
          elAttr "img" ("src" =: url <> "style" =: "max-width: 80%") blank
  return ()

fileInputElement :: DomBuilder t m => m (Dynamic t [RawFile (DomBuilderSpace m)])
fileInputElement = do
  ie <- inputElement $ def
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
      ("type" =: "file" <> "accept" =: "image/png, image/jpeg")
  return (_inputElement_files ie)

dataURLFileReader
  :: ( DomBuilder t m
     , TriggerEvent t m
     , PerformEvent t m
     , Prerender js m
     )
  => Event t (RawFile (DomBuilderSpace m)) -> m (Event t Text)
dataURLFileReader request = prerender (return never) $ do
  fileReader <- liftJSM newFileReader
  performEvent_ (fmap (readAsDataURL fileReader . Just) request)
  e <- wrapDomEvent fileReader (`on` load) . liftJSM $ do
    v <- getResult fileReader
    (fromJSVal <=< toJSVal) v
  return (fmapMaybe id e)

header :: DomBuilder t m => m ()
header = do
  el "strong" $ do
    text " FileReader test page"
  el "p" $
    text "Select an image file. It will be shown below"
