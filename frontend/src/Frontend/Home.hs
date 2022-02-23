{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Home (home) where

import Reflex.Dom
import Control.Monad (forM_)
import Data.Universe (universe)
import Obelisk.Route
import Obelisk.Route.Frontend
import Common.Route

home
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender js t m
     )
  => m ()
home = do
  elClass "p" "class" $ do
    text "Reflex is a fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."
  elClass "p" "class" $ do
    text "The following are some examples of using Reflex along with "
    let obSrc = "https://github.com/obsidiansystems/obelisk"
    elAttr "a" ("href" =: obSrc <> "target" =: "_blank") $ text "Obelisk."
  elClass "p" "class" $ do
    text "To run these examples on your machine, install Obelisk, clone this repo and run 'ob run' command."
  examplesList

examplesList
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender js t m
     )
  => m ()
examplesList = el "ul" $ do
  -- Iterate over all the top-level routes
  forM_ universe $ \section -> el "li" $ do
    el "h4" $ routeLink (FrontendRoute_Examples :/ (Just $ sectionHomepage section)) $
      text $ exampleTitle section
    el "p" $ text (exampleDescription section)
