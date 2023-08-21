{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Examples.NasaPod.Main where

import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)
import Reflex.Dom

data Apod =
  Apod { date           :: T.Text
       , explanation    :: T.Text
       , hdurl          :: T.Text
       , media_type      :: T.Text
       , service_version :: T.Text
       , title          :: T.Text
       , url            :: T.Text
       }
     deriving (Generic, Show)

instance FromJSON Apod
instance ToJSON Apod

app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , Prerender t m
     )
  => m ()
app = el "div" $ do
  apiKey <- inputElement $ def & initialAttributes .~ ("placeholder" =: "Enter NASA API Key")
  submitEvent <- button "Submit"
  let submitApiKey = tagPromptlyDyn (value apiKey) submitEvent
      loadingWidget = el "div" $ do
        el "p" $ do
          text "Get your API key from the "
          elAttr "a" ("href" =: "https://api.nasa.gov/" <> "target" =: "_blank") $ text "NASA Open API"
          text " site"
  _ <- widgetHold loadingWidget $ fmap apod submitApiKey
  return ()

apod
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , Prerender t m
     )
  => Text
  -> m ()
apod apiKey = prerender_ (blank) $ do
  pb :: Event t () <- getPostBuild
  let
    defReq = "https://api.nasa.gov/planetary/apod?api_key=" <> apiKey
    xhrTxt :: Maybe T.Text -> T.Text
    xhrTxt = maybe defReq (\d -> defReq <> "&date=" <> d)
    req md = XhrRequest "GET" (xhrTxt md) def
  rec
    eRsp :: Event t XhrResponse <- performRequestAsync $ req <$>
      leftmost [ Nothing <$ pb
               , fmap Just eValidDate
               ]
    let
      eRspApod :: Event t Apod = fmapMaybe decodeXhrResponse eRsp
      imgUrl :: Event t T.Text = fmap url eRspApod
      explEv :: Event t T.Text = fmap explanation eRspApod
      srcAttr :: Event t (Map T.Text T.Text) = ffor imgUrl $ \u -> "src" =: u
    srcAttrDyn :: Dynamic t (Map T.Text T.Text)
      <- holdDyn mempty srcAttr
    elDynAttr "img" srcAttrDyn $ return ()
    date <- inputElement $ def &
        inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("placeholder" =: "YYYY-MM-DD")
    let eValidDate = ffilter ((==10) . T.length) $ updated $ value date
    el "p" $
      dynText =<< holdDyn "Waiting for response" explEv
  return ()
