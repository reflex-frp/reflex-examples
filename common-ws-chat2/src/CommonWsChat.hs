{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

module CommonWsChat where

import           Data.Aeson (ToJSON, FromJSON, toEncoding, parseJSON,
                            defaultOptions,
                            genericToEncoding, genericParseJSON)
import qualified Data.Text as T
import           GHC.Generics (Generic)

data C2S = C2Sjoin T.Text
         | C2Sclose
         | C2Smsg T.Text
         deriving (Eq,Show, Generic)

-- options :: Options
options = defaultOptions -- { tagSingleConstructors = True }

instance ToJSON C2S where toEncoding = genericToEncoding options
instance FromJSON C2S where parseJSON = genericParseJSON options

data S2C = S2Cwelcome T.Text
         | S2Cbroadcast T.Text
         | S2Cuserexists
         | S2Cnameproblem
         deriving (Eq,Show, Generic)

instance ToJSON S2C where toEncoding = genericToEncoding options
instance FromJSON S2C where parseJSON = genericParseJSON options
