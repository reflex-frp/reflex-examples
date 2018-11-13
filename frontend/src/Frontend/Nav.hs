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
  => Event t () -- ^ When this event fires, collapse the menu
  -> m ()
nav collapseMenu = do
  openMenu <- divClass "logo-menu" $ do
    logo
    divClass "menu-toggle" $ do
      activeTab <- askRoute
      -- Build the title items, which will only be displayed on small screens
      (currentSection, _) <- elAttr' "a" ("class" =: "current-section") $
        dynText $ routeTitle <$> activeTab
      hamburger <- icon "bars"
      foldDyn ($) False $ leftmost
        [ not <$ domEvent Click currentSection
        , not <$ domEvent Click hamburger
        , const False <$ updated activeTab
        , const False <$ collapseMenu
        ]
  let openAttrs = ffor openMenu $ \case
        True -> "class" =: "active"
        False -> mempty
  elDynAttr "nav" openAttrs menu

-- | Displays the logo and returns an event that fires when the logo is clicked
logo :: (DomBuilder t m, SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m)
  => m ()
logo = do
  let logoAttrs = mconcat
        [ "class" =: "logo"
        , "src" =: static @"img/logo.svg"
        , "alt" =: "Reflex"
        ]
  routeLink (FrontendRoute_Home :/ ()) $ elAttr "img" logoAttrs blank

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
  -- Get the current route, so that we can highlight the corresponding tab
  currentTab <- askRoute
  let currentTabDemux = demux $ fmap (\(sec :=> _) -> Some.This sec) currentTab
  -- Iterate over all the top-level routes
  forM_ universe $ \section -> do
    -- Create a link that is highlighted if it is the current section
    let thisTabIsSelected = demuxed currentTabDemux section
        highlight = ffor thisTabIsSelected $ \case
          True -> "class" =: "nav-link active"
          False -> "class" =: "nav-link"
    elDynAttr "span" highlight $ routeLink (sectionHomepage section) $ text $ sectionTitle section
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
