{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Common.Api where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

newtype ConnId = ConnId { unConnId :: Integer } deriving (Show, Read, Eq, Ord, Enum, FromJSON, ToJSON)

newtype Nick = Nick { unNick :: Text } deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

data Up
   = Up_AddNick Text
   | Up_Message Nick Text
   deriving (Show, Read, Eq, Ord)

data Down
   = Down_Message Nick Text
   | Down_PLACEHOLDER
   deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions ''Up
deriveJSON defaultOptions ''Down
