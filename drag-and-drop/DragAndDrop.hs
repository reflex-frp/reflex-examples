{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified GHCJS.DOM.DataTransfer as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.MouseEvent as DOM
import GHCJS.Types (JSString)
import Reflex.Dom

main :: IO ()
main = mainWidget theWidget

theWidget :: forall t m. (MonadWidget t m) => m ()
theWidget = do
  rec
    let draggable :: m (El t, ()) -> String -> m ()
        draggable element attachment = do
          dragsite <- fst <$> element
          dragStartEvent <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw dragsite) (`DOM.on` DOM.dragStart) $ do
            dt <- fromMaybe (error "no dt?") <$> (DOM.getDataTransfer =<< DOM.event)
            DOM.setEffectAllowed dt ("all" :: JSString)
            DOM.setDropEffect dt ("move" :: JSString)
            DOM.setData dt ("application/x-reflex-description" :: JSString) attachment
          -- Bit of a hack here; this actually hooks the drag-start
          -- event to the DOM, since otherwise nothing reflex-side
          -- cares about the event
          performEvent_ $ return () <$ dragStartEvent
          return ()

    draggable (elAttr' "img" ("draggable" =: "true" <> "src" =: "arduino.jpg") blank) "a picture"
    draggable (elAttr' "pre" ("draggable" =: "true" <> "style" =: "-moz-user-select:none;-ms-user-select:none;-webkit-user-select:none;user-select:none;") $ text "main = putStrLn \"Hello world!\"") "some code"

    let ddEvent :: (DOM.DataTransfer -> DOM.EventM e DOM.MouseEvent a) ->
                   DOM.EventM e DOM.MouseEvent a
        ddEvent op = do
          dt <- fromMaybe (error "no DT?") <$> (DOM.getDataTransfer =<< DOM.event)
          op dt
        ddEvent_ :: DOM.EventM e DOM.MouseEvent () ->
                    DOM.EventM e DOM.MouseEvent ()
        ddEvent_ op = ddEvent (const op)

    dragEnterEvent <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw dropsite) (`DOM.on` DOM.dragEnter) (ddEvent_ DOM.preventDefault)
    dragLeaveEvent <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw dropsite) (`DOM.on` DOM.dragLeave) (ddEvent_ $ return ())

    dropsite <- fst <$> (elDynAttr' "div" dropsiteAttrs $ dynText dropText)
    inDrop <- holdDyn False (leftmost [True <$ dragEnterEvent, False <$ dragLeaveEvent, False <$ dropEvent])

    dropsiteAttrs <- forDyn inDrop $ \case
      True -> "style" =: "border:1em solid blue;padding:2em;margin:2em;background-color:green;"
      False -> "style" =: "border:1em solid blue;padding:2em;margin:2em;"
    dragOverEvent <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw dropsite) (`DOM.on` DOM.dragOver) (ddEvent_ DOM.preventDefault)
    performEvent_ $ return () <$ dragOverEvent

    dropEvent <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw dropsite) (`DOM.on` DOM.drop) $ ddEvent $ \dt -> do
      DOM.preventDefault
      DOM.getData dt ("application/x-reflex-description" :: String)
    dropText <- holdDyn "Drop here" $ fmap ("Dropped " <>) dropEvent
  return ()
