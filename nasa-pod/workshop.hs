{-# LANGUAGE RecursiveDo, ScopedTypeVariables, DeriveGeneric, OverloadedStrings #-}

import Data.Aeson
import GHC.Generics
import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid((<>))

data Apod = Apod { copyright :: T.Text
                 , date :: T.Text
                 , explanation :: T.Text
                 , hdurl :: T.Text
                 , media_type :: T.Text
                 , service_version :: T.Text
                 , title :: T.Text
                 , url :: T.Text
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
  let submitApiKey = tagPromptlyDyn (value apiKey) submitEvent
      loadingWidget = text "Waiting for an API Key..."
  widgetHold loadingWidget $ fmap apod submitApiKey
  return ()

apod :: MonadWidget t m => T.Text -> m ()
apod apiKey = do
  pb :: Event t () <- getPostBuild
  let defReq = "https://api.nasa.gov/planetary/apod?api_key=" <> apiKey
  let req md = XhrRequest "GET" (maybe defReq (\d -> defReq <> "&date=" <> d) md) def
  rec rsp :: Event t XhrResponse <- performRequestAsync $ fmap req $
        leftmost [ Nothing <$ pb
                 , fmap Just validDate
                 ]
      let rspApod :: Event t Apod = fmapMaybe decodeXhrResponse rsp
      let imgUrl :: Event t T.Text = fmap url rspApod
      let srcAttr :: Event t (Map T.Text T.Text) = ffor imgUrl $ \u -> "src" =: u
      srcAttrDyn :: Dynamic t (Map T.Text T.Text) <- holdDyn mempty $ leftmost [ srcAttr
                                                                               , mempty <$ clear''
                                                                               ]
      text "Reflex-Apod"
      elDynAttr "img" srcAttrDyn $ return ()
      -- previous <- button "<<"
      date <- textInput $ def & attributes .~ (constDyn $ "placeholder" =: "YYYY-MM-DD")
      let validDate = ffilter ((==10) . T.length) $ updated $ value date
      hideableButton :: Dynamic t (m (Event t ())) <- return $ ffor srcAttrDyn $ \m -> if Map.null m
                                                       then return never
                                                       else button "Clear"
      clear :: Event t (Event t ()) <- dyn hideableButton
      clear' :: Behavior t (Event t ()) <- hold never clear
      let clear'' :: Event t () = switch clear'
  return ()

