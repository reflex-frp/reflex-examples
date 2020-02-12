{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Nav (nav) where

import Common.Route
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom
import Frontend.FontAwesome

-- | Build the entire nav bar, with hamburger menu for expanding on mobile
nav
  :: ( DomBuilder t m
     , PostBuild t m
     , Routed t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , Prerender js t m
     )
  => m ()
nav = do
  divClass "logo-menu" $ logo
  el "nav" menu

-- | Displays the logo and returns an event that fires when the logo is clicked
logo
  :: (DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender js t m
     )
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
     , Routed t (R FrontendRoute) m
     )
  => m ()
menu = do
  activeTab <- askRoute
  elAttr "span" ("class" =: "nav-link active") $ do
    -- Build the title items, which will only be displayed on small screens
    el "a" $ dynText $ routeTitle <$> activeTab
  elAttr "span" ("class" =: "nav-link") $ do
    let src = routeSourceCode <$> activeTab
    elDynAttr "a" ((\s -> "href" =: s <> "target" =: "_blank") <$> src) $ do
      text "Source Code"
      icon_ "external-link-alt"

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
