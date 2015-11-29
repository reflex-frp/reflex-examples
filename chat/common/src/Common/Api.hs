{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Common.Api where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Time.Clock

newtype ConnId = ConnId { unConnId :: Integer } deriving (Show, Read, Eq, Ord, Enum, FromJSON, ToJSON)

newtype Nick = Nick { unNick :: Text } deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

newtype ChannelId = ChannelId { unChannelId :: Text } deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

type Destination = Either Nick ChannelId

data Message
   = Message { _message_from :: Nick
             , _message_to :: Destination
             , _message_body :: Text
             }
   deriving (Show, Read, Eq, Ord)

data Envelope a
   = Envelope { _envelope_time :: UTCTime
              , _envelope_contents :: a
              }
   deriving (Show, Read, Eq, Ord)

data Up
   = Up_AddNick Nick
   | Up_RemoveNick Nick
   | Up_Message Message
   deriving (Show, Read, Eq, Ord)

data Down
   = Down_Message (Envelope Message)
   | Down_PLACEHOLDER
   deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions ''Up
deriveJSON defaultOptions ''Down
deriveJSON defaultOptions ''Message
deriveJSON defaultOptions ''Envelope
