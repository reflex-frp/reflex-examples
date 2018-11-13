{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Examples.NasaPod.Main where

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Data.Text (Text)
import           GHC.Generics                (Generic)
import           Reflex.Dom                  hiding (mainWidget)
import           Reflex.Dom.Core             (mainWidget)
import Control.Monad.Fix (MonadFix)

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

main :: IO ()
main = run $ mainWidget app

app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     )
  => m ()
app = el "div" $ do
  apiKey <- inputElement def
  submitEvent <- button "Submit"
  let submitApiKey = tagPromptlyDyn (value apiKey) submitEvent
      loadingWidget = text "Waiting for an API Key..."
  _ <- widgetHold loadingWidget $ fmap apod submitApiKey
  return ()

apod
  :: ( DomBuilder t m
     , MonadFix m
     , PerformEvent t m
     , MonadHold t m
     , PostBuild t m
     , TriggerEvent t m
     , Prerender js m
     )
  => Text
  -> m ()
apod apiKey = prerender (blank) $ do
  pb :: Event t () <- getPostBuild
  let defReq = "https://api.nasa.gov/planetary/apod?api_key=" <> apiKey
      xhrTxt :: Maybe T.Text -> T.Text
      xhrTxt = maybe defReq (\d -> defReq <> "&date=" <> d)
      req md = XhrRequest "GET" (xhrTxt md) def
  rec eRsp :: Event t XhrResponse <- performRequestAsync $ req <$>
        leftmost [ Nothing <$ pb
                 , fmap Just eValidDate
                 ]
      let eRspApod :: Event t Apod = fmapMaybe decodeXhrResponse eRsp
      let imgUrl :: Event t T.Text = fmap url eRspApod
      let srcAttr :: Event t (Map T.Text T.Text) = ffor imgUrl $ \u -> "src" =: u
      srcAttrDyn :: Dynamic t (Map T.Text T.Text)
        <- holdDyn mempty $ leftmost [ srcAttr
                                     , mempty <$ clear''
                                     ]
      elDynAttr "img" srcAttrDyn $ return ()
      -- previous <- button "<<"
      date <- textInput $ def &
          attributes .~ constDyn ("placeholder" =: "YYYY-MM-DD")
      let eValidDate = ffilter ((==10) . T.length) $ updated $ value date
      hideableButton :: Dynamic t (m (Event t ()))
        <- return $ ffor srcAttrDyn $ \m -> if Map.null m
                                               then return never
                                               else button "Clear"
      clear  :: Event t (Event t ())    <- dyn hideableButton
      clear' :: Behavior t (Event t ()) <- hold never clear
      let clear'' :: Event t () = switch clear'
  return ()

