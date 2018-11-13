{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Nav (nav) where

import Common.Route
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Some as Some
import Data.Universe (universe)
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom

import Frontend.FontAwesome

-- | Build the entire nav bar, with hamburger menu for expanding on mobile
nav
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , Routed t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
nav = do
  openMenu <- divClass "logo-menu" $ do
    logo
    activeTab <- askRoute
    divClass "" $ do
      -- Build the title items, which will only be displayed on small screens
      dynText $ routeTitle <$> activeTab
    divClass "" $ do
      dynText $ routeSourceCode <$> activeTab
  el "nav" menu

-- | Displays the logo and returns an event that fires when the logo is clicked
logo :: (DomBuilder t m, SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m)
  => m ()
logo = do
  let logoAttrs = mconcat
        [ "class" =: "logo"
        , "src" =: static @"img/logo.svg"
        , "alt" =: "Reflex"
        ]
  routeLink (FrontendRoute_Examples :/ Nothing) $ elAttr "img" logoAttrs blank

-- | Build the nav's tabs
menu
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , Routed t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
menu = do
  forkMeOnGithub

forkMeOnGithub
  :: DomBuilder t m
  => m ()
forkMeOnGithub = do
  -- The banner is only shown on the bigger screens on top right corner
  elClass "span" "fork-link" $ elAttr "a" (("href" =: href) <> ("target" =: "_blank")) $
    elAttr "img" (("src" =: src) <> ("alt" =: alt)) $ return ()
  -- The inline link is only shown on mobile screens along with other menu options
  elClass "span" "fork-link-inline" $ elAttr "a" (("href" =: href) <> ("target" =: "_blank")) $
    text "Fork me on Github"
  where
    href = "https://github.com/reflex-frp/reflex-examples"
    src = "https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png"
    alt = "Fork me on GitHub"
