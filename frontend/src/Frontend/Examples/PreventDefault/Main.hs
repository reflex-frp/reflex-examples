{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Examples.PreventDefault.Main (app) where

import Control.Lens.Operators
import Control.Monad
import Data.Proxy
import Reflex.Dom.Core

app :: forall t m. (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
app = do
  let clickStopPropagation = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)
      clickPreventDefault = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const preventDefault)
  (outerEl, (innerPreventDefault, innerStopPropagation)) <- elAttr' "div" ("style" =: "border: 2px dashed black; height: 200px;") $ do
    el "span" $ text "Outer"
    (innerPreventDefault, _) <- element "button" clickPreventDefault $ text "Inner preventDefault"
    (innerStopPropagation, _) <- element "button" clickStopPropagation $ text "inner stopPropagation"
    return (innerPreventDefault, innerStopPropagation)
  dynText <=< holdDyn "Click an element" $ leftmost
    [ "OUTER CLICK" <$ domEvent Click outerEl
    , "INNER CLICK PREVENT DEFAULT" <$ domEvent Click innerPreventDefault
    , "INNER CLICK STOP PROPAGATION" <$ domEvent Click innerStopPropagation
    ]
  return ()
