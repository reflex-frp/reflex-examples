{-# LANGUAGE OverloadedStrings #-}
module Frontend.Footer (footer) where

import Control.Monad (forM_)
import Frontend.FontAwesome
import Reflex.Dom

footer :: DomBuilder t m => m ()
footer = do
  let links =
        [ ("Hackage", "https://hackage.haskell.org/package/reflex")
        , ("#reflex-frp", "http://webchat.freenode.net/?channels=%23reflex-frp&uio=d4")
        ]
  forM_ links $ \(name, url) -> elAttr "a" ("href" =: url) $ text name
  let socialIcon i title url = elAttr "a" ("href" =: url <> "title" =: title) $ brandIcon_ i
  socialIcon "twitter" "Twitter" "https://twitter.com/search?q=%23reflexfrp"
  socialIcon "github" "GitHub" "http://github.com/reflex-frp"
  socialIcon "reddit" "Reddit" "http://reddit.com/r/reflexfrp"
