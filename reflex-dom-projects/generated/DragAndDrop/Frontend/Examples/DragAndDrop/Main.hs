{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DataKinds             #-}

module Frontend.Examples.DragAndDrop.Main where

import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import qualified GHCJS.DOM.DataTransfer    as DOM
import qualified GHCJS.DOM.HTMLElement     as DOM
import qualified GHCJS.DOM.Element         as DOM
import qualified GHCJS.DOM.EventM          as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.MouseEvent      as DOM
import qualified GHCJS.DOM.Types           as DOM (uncheckedCastTo)

import Language.Javascript.JSaddle
import Reflex.Dom.Core

app
  :: ( DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     )
  => m ()
app = do
  el "h3" $ text "Drag and Drop"
  el "div" $ draggable item1 "Haskell logo"
  el "div" $ draggable item2 "some text"
  text "Drap either the above logo or the text to the below box"
  handleDragEvents
  return ()


item1 :: DomBuilder t m => m (Element EventResult (DomBuilderSpace m) t, ())
item1 = elAttr' "img" ("draggable" =: "true" <> "src" =: haskellLogoUrl) blank
  where
    haskellLogoUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/180px-Haskell-Logo.svg.png"

item2 :: DomBuilder t m => m (Element EventResult (DomBuilderSpace m) t, ())
item2 = elAttr' "pre" ("draggable" =: "true"
      <> "style" =: "-moz-user-select:none;-ms-user-select:none;-webkit-user-select:none;user-select:none;")
      $ text "Draggable text"

draggable
  :: ( DomBuilder t m
     , TriggerEvent t m
     , PerformEvent t m
     , Prerender js m
     )
  => m (Element EventResult (DomBuilderSpace m) t, ())
  -> String
  -> m ()
draggable elmnt attachment = do
  dragsite <- fst <$> elmnt
  prerender (return ()) $ do
    dragStartEvent <- wrapDomEvent -- (_el_element dragsite)
              -- (DOM.getToElement $ _element_raw dragsite)
              (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw dragsite)
              (`DOM.on` DOM.dragStart) $ do
      dt <- fromMaybe (error "no dt?")
        <$> (DOM.getDataTransfer =<< DOM.event)
      DOM.setEffectAllowed dt ("all" :: JSString)
      DOM.setDropEffect dt ("move" :: JSString)
      DOM.setData dt
          ("application/x-reflex-description" :: JSString) attachment
    -- Bit of a hack here; this actually hooks the drag-start
    -- event to the DOM, since otherwise nothing reflex-side
    -- cares about the event
    performEvent_ $ return () <$ dragStartEvent
    return ()

handleDragEvents
  :: ( DomBuilder t m
     , TriggerEvent t m
     , PostBuild t m
     , MonadHold t m
     , PerformEvent t m
     , Prerender js m
     )
  => m ()
handleDragEvents = prerender (return ()) $ do
  let
    ddEvent :: (DOM.DataTransfer -> DOM.EventM e DOM.MouseEvent a) ->
               DOM.EventM e DOM.MouseEvent a
    ddEvent op = do
        dt <- fromMaybe (error "no DT?")
              <$> (DOM.getDataTransfer =<< DOM.event)
        op dt
    ddEvent_ :: DOM.EventM e DOM.MouseEvent () ->
                DOM.EventM e DOM.MouseEvent ()
    ddEvent_ op = ddEvent (const op)

  rec
    dragEnterEvent <- wrapDomEvent
        dsHTMLel (`DOM.on` DOM.dragEnter) (ddEvent_ DOM.preventDefault)
    dragLeaveEvent <- wrapDomEvent
        dsHTMLel (`DOM.on` DOM.dragLeave) (ddEvent_ $ return ())
    dropsite <- fst <$> elDynAttr' "div" dropsiteAttrs (dynText dropText)
    -- The following is defined to shorten the wrapDomEvent calls.
    let dsHTMLel = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw dropsite
    inDrop <- holdDyn False $ leftmost
        [ True  <$ dragEnterEvent
        , False <$ dragLeaveEvent
        , False <$ dropEvent]
    dropsiteAttrs <- return . ffor inDrop $ \case
        True -> "style" =: "border:1em solid blue;padding:2em;margin:2em;background-color:green;"
        False -> "style" =: "border:1em solid blue;padding:2em;margin:2em;"
    dragOverEvent <- wrapDomEvent
        dsHTMLel (`DOM.on` DOM.dragOver) (ddEvent_ DOM.preventDefault)
    performEvent_ $ return () <$ dragOverEvent

    dropEvent <- wrapDomEvent dsHTMLel (`DOM.on` DOM.drop) $ ddEvent $ \dt -> do
        DOM.preventDefault
        DOM.getData dt ("application/x-reflex-description" :: String)
    dropText <- holdDyn "Drop here" $ fmap ("Dropped " <>) dropEvent
  return ()
