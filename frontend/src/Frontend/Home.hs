{-# LANGUAGE OverloadedStrings #-}
module Frontend.Home (home) where

import Reflex.Dom

home :: DomBuilder t m => m ()
home = do
  elClass "p" "class" $ do
    text "Reflex is a fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."
    text "The following are some examples to demonstrate how the usage of Reflex"
