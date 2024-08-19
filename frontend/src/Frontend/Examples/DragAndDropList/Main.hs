{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Examples.DragAndDropList.Main where

import Control.Monad.Except
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Reflex.Dom

import Frontend.Examples.DragAndDropList.DragAndDropList

main :: IO ()
main = mainWidget app

-- One thing to note here is this will not work on mobile because this example handles click events and not touch events.
-- To support touch events you can load in some js to handle this explicity, but this is beyond the scope of this example.
app :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, Prerender t m) => m ()
app = do
  el "h1" $ text "Drag and Drop Demo"
  prerender_ blank $ do
    _ <- createDraggableList dragAndDropConfig
    blank

-- Here we define the configuration that is passed to the draggable list
dragAndDropConfig :: DragAndDropConstraints t m => DragAndDropConfig t m Int T.Text
dragAndDropConfig =
  DragAndDropConfig
    { initialItems = Map.fromList [(1, "Item 1"), (2, "Item 2"), (3, "Item 3"), (4, "Item 4"), (5, "Item 5")]
    , itemWidget = \key itemTextDyn -> createDraggableItem key itemTextDyn itemStyle
    }

-- This is just a helper function to pass the item CSS style
itemStyle :: T.Text
itemStyle = T.unwords
  [ "padding: 1rem;"
  , "margin: 0.5rem;"
  , "background-color: #bfdbfe;"
  , "cursor: move;"
  , "border: 1px solid #60a5fa;"
  , "border-radius: 0.25rem;"
  ]
