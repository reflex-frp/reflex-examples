{-# LANGUAGE RecursiveDo, ScopedTypeVariables, DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)

data Apod = Apod { copyright :: String
                 , date :: String
                 , explanation :: String
                 , hdurl :: String
                 , media_type :: String
                 , service_version :: String
                 , title :: String
                 , url :: String
                 }
     deriving (Generic, Show)

instance FromJSON Apod
instance ToJSON Apod

main :: IO ()
main = mainWidget workshop

workshop :: MonadWidget t m => m ()
workshop = el "div" $ do
  apiKey <- textInput def
  submitEvent <- button "Submit"
  let submitApiKey = tagDyn (value apiKey) submitEvent
      loadingWidget = text "Waiting for an API Key..."
  widgetHold loadingWidget $ fmap (\s -> apod s) submitApiKey
  return ()

apod :: MonadWidget t m => String -> m ()
apod apiKey = do
  pb :: Event t () <- getPostBuild
  let defReq = "https://api.nasa.gov/planetary/apod?api_key="++apiKey
  let req md = XhrRequest "GET" (maybe defReq (\d -> defReq ++ "&date=" ++ d) md) def
  rec rsp :: Event t XhrResponse <- performRequestAsync $ fmap req $
        leftmost [ Nothing <$ pb
                 , fmap Just validDate
                 ]
      let rspApod :: Event t Apod = fmapMaybe (\r -> decodeXhrResponse r) rsp
      let imgUrl :: Event t String = fmap url rspApod
      let srcAttr :: Event t (Map String String) = ffor imgUrl $ \u -> "src" =: u
      srcAttrDyn :: Dynamic t (Map String String) <- holdDyn mempty $ leftmost [ srcAttr
                                                                               , mempty <$ clear''
                                                                               ]
      text "Reflex-Apod"
      elDynAttr "img" srcAttrDyn $ return ()
      -- previous <- button "<<"
      date <- textInput $ def & attributes .~ (constDyn $ "placeholder" =: "YYYY-MM-DD")
      let validDate = ffilter ((==10) . length) $ updated $ value date
      hideableButton :: Dynamic t (m (Event t ())) <- forDyn srcAttrDyn $ \m -> if Map.null m
                                                       then return never
                                                       else button "Clear"
      clear :: Event t (Event t ()) <- dyn hideableButton
      clear' :: Behavior t (Event t ()) <- hold never clear
      let clear'' :: Event t () = switch clear'
  return ()

