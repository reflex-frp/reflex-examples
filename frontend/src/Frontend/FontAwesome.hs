{-# LANGUAGE OverloadedStrings #-}
module Frontend.FontAwesome
  ( icon
  , icon_
  , brandIcon
  , brandIcon_
  , solidIcon
  , solidIcon_
  , regularIcon
  , regularIcon_
  , lightIcon
  , lightIcon_
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core

-- | The common base underlying each of the FontAwesome icon variations.
-- See https://fontawesome.com/how-to-use/on-the-web/setup/upgrading-from-version-4
iconBase :: DomBuilder t m => Text -> Text -> m (Element EventResult (DomBuilderSpace m) t)
iconBase style i = fst <$> elClass' "i" (T.unwords [style, "fa-fw", "fa-" <> i]) blank

-- | Icon element with the default FontAwesome style. Equivalent to 'solidIcon'.
icon :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t)
icon = iconBase "fa"

icon_ :: DomBuilder t m => Text -> m ()
icon_ = void . icon

-- | Icon element with the "solid" FontAwesome style.
solidIcon :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t)
solidIcon = icon

solidIcon_ :: DomBuilder t m => Text -> m ()
solidIcon_ = icon_

-- | Icon element with the "brand" FontAwesome style.
brandIcon :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t)
brandIcon = iconBase "fab"

brandIcon_ :: DomBuilder t m => Text -> m ()
brandIcon_ = void . brandIcon

-- | Icon element with the "regular" FontAwesome style.
regularIcon :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t)
regularIcon = iconBase "far"

regularIcon_ :: DomBuilder t m => Text -> m ()
regularIcon_ = void . regularIcon

-- | Icon element with the "light" FontAwesome style.
lightIcon :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t)
lightIcon = iconBase "fal"

lightIcon_ :: DomBuilder t m => Text -> m ()
lightIcon_ = void . lightIcon
