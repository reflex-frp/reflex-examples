{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Home (home) where

import Reflex.Dom
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Some as Some
import Data.Universe (universe)
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend
import Common.Route

home
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
home = do
  elClass "p" "class" $ do
    text "Reflex is a fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."
    text "The following are some examples to demonstrate how the usage of Reflex"
  examplesList

examplesList
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
examplesList = el "ul" $ do
  -- Iterate over all the top-level routes
  forM_ universe $ \section -> el "li" $ do
    el "h4" $ routeLink (FrontendRoute_Examples :/ (Just $ sectionHomepage section)) $
      text $ exampleTitle section
    el "p" $ text (exampleDescription section)
