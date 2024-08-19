{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.Examples.DragAndDropList.DragAndDropList (
  DragEvent (..),
  DragAndDropConfig (..),
  DragAndDropConstraints,
  createDraggableList,
  createDraggableItem,
) where

import Control.Monad.Fix (MonadFix)
import qualified Data.Map as Map
import qualified Data.Text as T
import Reflex.Dom

-- | Events that can occur during drag and drop
data DragEvent k = DragStart k | DragEnter k | DragLeave k | DragEnd k
  deriving (Show, Eq)

-- | Constraints required for drag and drop functionality
type DragAndDropConstraints t m =
  ( DomBuilder t m
  , MonadHold t m
  , MonadFix m
  , PostBuild t m
  , MonadSample t (Performable m)
  , DomBuilderSpace m ~ GhcjsDomSpace
  )

-- | Configuration for creating a draggable list
data DragAndDropConfig t m k v = DragAndDropConfig
  { initialItems :: Map.Map k v
  , itemWidget :: k -> Dynamic t v -> m (Event t (DragEvent k))
  }

-- | Creates a draggable list based on the provided configuration
createDraggableList ::
  (Ord k, Eq v, DragAndDropConstraints t m) =>
  DragAndDropConfig t m k v ->
  m (Dynamic t (Map.Map k v))
createDraggableList config = do
  rec items <- foldDyn ($) (initialItems config) updateEvent
      updateEvent <- draggableList items (itemWidget config)
  return items

-- | Creates the draggable list widget
draggableList ::
  (Ord k, Eq v, DragAndDropConstraints t m) =>
  Dynamic t (Map.Map k v) ->
  (k -> Dynamic t v -> m (Event t (DragEvent k))) ->
  m (Event t (Map.Map k v -> Map.Map k v))
draggableList items widget = do
  events <- listWithKey items widget
  let allEvents = switch . current $ fmap (leftmost . Map.elems) events
  rec dragState <- foldDyn updateDragState Nothing allEvents
      let updateEvent = fmapMaybe (uncurry processDropAlpha) $ attach (current dragState) allEvents
  return updateEvent

-- | Updates the drag state based on drag events
updateDragState :: Eq k => DragEvent k -> Maybe (k, k) -> Maybe (k, k)
updateDragState (DragStart src) _ = Just (src, src)
updateDragState (DragEnter dst) (Just (src, _)) = Just (src, dst)
updateDragState (DragLeave _) state = state
updateDragState (DragEnd _) Nothing = Nothing
updateDragState (DragEnd _) (Just (src, dst)) = if src == dst then Nothing else Just (src, dst)
updateDragState (DragEnter dst) Nothing = Just (dst, dst)

-- | Processes the drop event and updates the item order
processDropAlpha :: Ord k => Maybe (k, k) -> DragEvent k -> Maybe (Map.Map k v -> Map.Map k v)
processDropAlpha (Just (src, dst)) (DragEnd _) | src /= dst = Just $ \m ->
  let srcItem = Map.findWithDefault (error "Source item not found") src m
      dstItem = Map.findWithDefault (error "Destination item not found") dst m
   in Map.insert dst srcItem $ Map.insert src dstItem m
processDropAlpha _ _ = Nothing

-- | Creates a draggable item widget
createDraggableItem ::
  DragAndDropConstraints t m =>
  k ->
  Dynamic t T.Text ->
  T.Text -> -- CSS styles
  m (Event t (DragEvent k))
createDraggableItem key contentDyn cssStyles = do
  (cElement, _) <-
    elAttr'
      "div"
      ( "style" =: cssStyles
          <> "draggable" =: "true"
      )
      $ dynText contentDyn
  let dragStartEvent = DragStart key <$ domEvent Dragstart cElement
      dragEnterEvent = DragEnter key <$ domEvent Dragenter cElement
      dragLeaveEvent = DragLeave key <$ domEvent Dragleave cElement
      dragEndEvent = DragEnd key <$ domEvent Dragend cElement
  return $ leftmost [dragStartEvent, dragEnterEvent, dragLeaveEvent, dragEndEvent]
