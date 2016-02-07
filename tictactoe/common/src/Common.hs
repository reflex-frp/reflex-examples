{-# LANGUAGE DeriveGeneric #-}
module Common where

import Data.Aeson
import GHC.Generics

data Marker = Marker_X
            | Marker_O
            deriving (Show, Read, Eq, Ord, Generic)

type Board = [[Maybe Marker]]

instance FromJSON Marker
instance ToJSON Marker
