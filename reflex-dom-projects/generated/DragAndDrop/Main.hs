{-# LANGUAGE OverloadedStrings #-}
module Main where

import Frontend.Examples.DragAndDrop.Main
import Reflex.Dom
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = mainWidgetWithHead pageHead $ do
  article $ app

-- | An @<article>@ tag that will set its title and the class of its child
-- @<section>@ based on the current route
article
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => m () -- ^ Article content widget
  -> m ()
article c = el "main" $ el "article" c

pageHead :: DomBuilder t m => m ()
pageHead = do
  el "title" $ text "Reflex FRP Examples"
  elAttr "meta" metaDesc blank
  elAttr "meta" metaKeywords blank
  elAttr "meta" viewport blank
  styleSheet $ "static/css/normalize.css"
  styleSheet $ "static/css/fontawesome.min.css"
  styleSheet $ "static/css/font.css"
  styleSheet $ "static/css/style.css"
  -- elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"echarts.min.js") blank

metaDesc :: Map Text Text
metaDesc = "name" =: "description"
        <> "content" =: "Reflex Functional Reactive Programming Examples"

metaKeywords :: Map Text Text
metaKeywords = "name" =: "keywords"
            <> "content" =: "reflex, reflex frp, functional reactive programming, haskell, framework, reflex dom"

viewport :: Map Text Text
viewport = "name" =: "viewport"
        <> "content" =: "width=device-width, initial-scale=1"

--  styleSheet are functions to add links to html <head>
styleSheet :: DomBuilder t m => Text -> m ()
styleSheet myLink = elAttr "link" attrs blank
  where attrs = "rel" =: "stylesheet"
             <> "type" =: "text/css"
             <> "href" =: myLink

tshow :: Show a => a -> Text
tshow = T.pack . show
