{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.Examples.ECharts.Main where

import ECharts hiding (ffor)

import Frontend.Examples.ECharts.ExamplesData (rainfallData, waterFlowData)
import Common.Examples.ECharts.Types

import qualified Obelisk.ExecutableConfig
import Network.URI (parseURI, URI(..), URIAuth(..))

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Fix (MonadFix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Control.Lens
import qualified Data.Some as Some
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import Data.Time.Calendar
import System.Random
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, parseJSON, genericParseJSON, defaultOptions, fieldLabelModifier)
import qualified Data.ByteString.Lazy as LBS
import Obelisk.Generated.Static
import Language.Javascript.JSaddle (JSException(..), catch, JSVal, toJSVal, JSM, MonadJSM, liftJSM, valToText)

import Reflex.Dom.Core

app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     , MonadIO m
     )
  => m ()
app = do
    Just r <- liftIO $ Obelisk.ExecutableConfig.get "config/common/route"
    prerender blank $ do
      let Just (URI scheme (Just auth)  _ _ _) = parseURI $ T.unpack $ T.strip r
          wsScheme = case scheme of
            "https:" -> "wss:"
            _ -> "ws:"
          wsUrl = T.pack $ wsScheme <> (uriRegName auth) <> (uriPort auth) <> "/cpustats"
      dEv <- do
        pb <- getPostBuild
        d1 <- holdDyn Nothing
          =<< getAndDecode ((static @"data/confidence-band.json") <$ pb)
        d2 <- holdDyn Nothing
          =<< getAndDecode ((static @"data/aqi-beijing.json") <$ pb)
        let d = (,) <$> d1 <*> d2
        return $ fforMaybe (updated d) $ \case
          (Just v1, Just v2) -> Just (v1, v2)
          _ -> Nothing

      clEv <- button "Show Dynamic Chart"
      widgetHold blank $ echarts wsUrl <$ clEv
      widgetHold blank $ seriesExamples (mkStdGen 0) <$> dEv
      blank

echarts
  :: forall t m.
     ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , HasJSContext m
     , MonadJSM m
     )
  => Text
  -> m ()
echarts wsUrl = el "main" $ do
  ws <- webSocket wsUrl $ def & webSocketConfig_send .~ (never :: Event t [Text])
  receivedMessages :: Dynamic t [(UTCTime, CpuStat Double)] <- foldDyn (\m ms -> case Aeson.decode (LBS.fromStrict m) of
    Nothing -> ms
    Just m' -> take 50 $ m' : ms) [] $ _webSocket_recv ws
  let cpuStatMap (t, c) = mconcat
        [ "user" =: [(t, _cpuStat_user c)]
        , "nice" =: [(t, _cpuStat_nice c)]
        , "system" =: [(t, _cpuStat_system c)]
        , "idle" =: [(t, _cpuStat_idle c)]
        , "iowait" =: [(t, _cpuStat_iowait c)]
        , "irq" =: [(t, _cpuStat_irq c)]
        , "softirq" =: [(t, _cpuStat_softirq c)]
        , "steal" =: [(t, _cpuStat_steal c)]
        , "guest" =: [(t, _cpuStat_guest c)]
        , "guestNice" =: [(t, _cpuStat_guestNice c)]
        ]
  dynamicTimeSeries "CPU Stats" $ Map.unionsWith (++) . fmap cpuStatMap . reverse <$> receivedMessages

dynamicTimeSeries
  :: ( DomBuilder t m
     , PerformEvent t m
     , PostBuild t m
     , MonadHold t m
     , MonadJSM (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => Text
  -> Dynamic t (Map Text [(UTCTime, Double)])
  -> m ()
dynamicTimeSeries title ts = do
  e <- fst <$> elAttr' "div" ("style" =: "width:600px; height:400px;") blank
  p <- getPostBuild
  chart <- performEvent $ ffor p $ \_ -> liftJSM $ ECharts.initECharts $ _element_raw e
  let opts0 = def
        { _chartOptions_title = Just $ def { _title_text = Just title }
        , _chartOptions_xAxis = def { _axis_type = Just AxisType_Time } :[]
        , _chartOptions_yAxis = def { _axis_type = Just AxisType_Value
                                    , _axis_min = Just $ Left 0
                                    , _axis_max = Just $ Left 101
                                    } :[]
        , _chartOptions_series = []
        }
  performEvent_ $ ffor chart $ \c -> liftJSM $ setOptionWithCatch c opts0
  mchart <- holdDyn Nothing $ Just <$> chart
  let opts = leftmost
        [ attach (current mchart) $ updated ts
        , attachWith (\t c -> (c, t)) (current ts) (updated mchart)
        ]
  performEvent_ $ fforMaybe opts $ \case
    (Nothing, _) -> Nothing
    (Just c, ts') -> Just $ liftJSM $ setOptionWithCatch c $ opts0
      { _chartOptions_series = ffor (reverse $ Map.toList ts') $ \(k, vs) -> Some.This $
        SeriesT_Line $ def
          & series_name ?~ k
          & series_smooth ?~ Right (0.7)
          & series_data ?~ (ffor vs $ \(t, v) -> def
            & data_name ?~ utcTimeToEpoch t
            & data_value ?~ (utcTimeToEpoch t, v)
                           )
      }

setOptionWithCatch :: ECharts -> ChartOptions -> JSM ()
setOptionWithCatch c o = setOption c o `catch` \(JSException e) -> (valToText e) >>= (liftIO . putStrLn . show) >> return ()

seriesExamples
  :: ( DomBuilder t m
     , PerformEvent t m
     , PostBuild t m
     , MonadHold t m
     , TriggerEvent t m
     , MonadFix m
     , MonadJSM (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadIO m
     , RandomGen g
     )
  => g
  -> ([ConfidenceData], AqiData)
  -> m ()
seriesExamples rGen (confData, aqiData) = elAttr "div" ("style" =: "display: flex; flex-wrap: wrap") $
  mapM_ renderChartOptions
    [ basicLineChart
    , basicAreaChart
    , smoothedLineChart
    , stackedAreaChart
    , rainfall
    , largeScaleAreaChart rGen
    , confidenceBand confData
    , rainfallAndWaterFlow
    , aqiChart aqiData
    , multipleXAxes
    ]

renderChartOptions
  :: ( DomBuilder t m
     , PerformEvent t m
     , PostBuild t m
     , MonadHold t m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO m
     , MonadJSM (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => ChartOptions
  -> m ()
renderChartOptions opts = do
  chart <- do
    e <- fst <$> elAttr' "div" ("style" =: "width:600px; height:400px; padding: 50px;") blank
    p <- getPostBuild
    performEvent $ ffor p $ \_ -> liftJSM $ ECharts.initECharts $ _element_raw e
  performEvent_ $ ffor chart $ \c -> do
    liftJSM $ setOptionWithCatch c opts

  -- widgetHold blank $ ffor chart getRenderTime
  blank
  -- where
  --   getRenderTime c = do
  --     clEv <- button "Make Dynamic"
  --     widgetHold blank $ ffor clEv $ \_ -> do
  --       ev <- tickLossyFromPostBuildTime 0.2
  --       performEvent_ $ ffor ev $ \_ -> do
  --         t1 <- liftIO $ getCurrentTime
  --         liftJSM $ setOptionWithCatch c opts
  --         t2 <- liftIO $ getCurrentTime
  --         liftIO $ putStrLn $ show (diffUTCTime t2 t1)
  --     blank

basicLineChart :: ChartOptions
basicLineChart = def
  { _chartOptions_xAxis = [def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing}
                          ]
  , _chartOptions_yAxis = [def { _axis_type = Just AxisType_Value }]
  , _chartOptions_series = [Some.This $ SeriesT_Line $ def
    & series_data ?~ (map DataInt yAxisData)]
  }
  where
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    yAxisData = [820, 932, 901, 934, 1290, 1330, 1320]

basicAreaChart :: ChartOptions
basicAreaChart = def
  { _chartOptions_xAxis = [def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing
                              , _axis_boundaryGap = Just $ Left False
                              }
                          ]
  , _chartOptions_yAxis = [def { _axis_type = Just AxisType_Value }]
  , _chartOptions_series = [Some.This $ SeriesT_Line $ def
    & series_data ?~ (map DataInt yAxisData)
    & series_areaStyle ?~ def ]
  }
  where
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    yAxisData = [820, 932, 901, 934, 1290, 1330, 1320]

smoothedLineChart :: ChartOptions
smoothedLineChart = def
  { _chartOptions_xAxis = [def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing}
                          ]
  , _chartOptions_yAxis = [def { _axis_type = Just AxisType_Value }]
  , _chartOptions_series = [Some.This $ SeriesT_Line $ def
    & series_data ?~ (map DataInt yAxisData)
    & series_smooth ?~ Left True]
  }
  where
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    yAxisData = [820, 932, 901, 934, 1290, 1330, 1320]

stackedAreaChart :: ChartOptions
stackedAreaChart = def
  { _chartOptions_title = Just $ def { _title_text = Just title }
  , _chartOptions_tooltip = Just $ def
    { _toolTip_trigger = Just "axis"
    , _toolTip_axisPointer = Just $ def
      { _axisPointer_type = Just $ "cross"
      , _axisPointer_label = Just $ def
        { _label_backgroundColor = Just "#6a7985"
        }
      }
    }
  , _chartOptions_toolbox = Just $ def
    { _toolBox_features =
      [ emptySaveAsImage { _feature_title = Just "Save as PNG" }
      ]
    }
  , _chartOptions_legend = Just $ def
    { _legend_data = Just $ [ ("A", def)
                            , ("B", def)
                            , ("C", def)
                            , ("D", def)
                            , ("E", def)
                            ]}
  , _chartOptions_grid = def
    { _grid_pos = Just
      (def { _pos_left = Just $ PosAlign_Percent 3
                   , _pos_right = Just $ PosAlign_Percent 4
                   , _pos_bottom = Just $ PosAlign_Percent 3})
            , _grid_containLabel = Just True
    } : []
  , _chartOptions_xAxis = def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing
                              , _axis_boundaryGap = Just $ Left False} : []
  , _chartOptions_yAxis = def { _axis_type = Just AxisType_Value
                              } : []
  , _chartOptions_series = [l1, l2, l3, l4, l5]
  }
  where
    title = "Stacked Area Chart"
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    stackLabel = "stackLabel"
    l1 = Some.This $ SeriesT_Line $ def
      & series_stack ?~ stackLabel
      & series_name ?~ "A"
      & series_areaStyle ?~ def
      & series_data ?~
      (map DataInt [120, 132, 101, 134, 90, 230, 210])
    l2 = Some.This $ SeriesT_Line $ def
      & series_stack ?~ stackLabel
      & series_name ?~ "B"
      & series_areaStyle ?~ def
      & series_data ?~
      (map DataInt [220, 182, 191, 234, 290, 330, 310])
    l3 = Some.This $ SeriesT_Line $ def
      & series_stack ?~ stackLabel
      & series_name ?~ "C"
      & series_areaStyle ?~ def
      & series_data ?~
      (map DataInt [150, 232, 201, 154, 190, 330, 410])
    l4 = Some.This $ SeriesT_Line $ def
      & series_stack ?~ stackLabel
      & series_name ?~ "D"
      & series_areaStyle ?~ def
      & series_data ?~
      (map DataInt [320, 332, 301, 334, 390, 330, 320])
    l5 = Some.This $ SeriesT_Line $ def
      & series_stack ?~ stackLabel
      & series_name ?~ "E"
      & series_areaStyle ?~ def
      & series_label ?~ def { _label_show = Just True, _label_position = Just $ Position_String "top"}
      & series_data ?~
      (map DataInt [820, 932, 901, 934, 1290, 1330, 1320])

rainfall :: ChartOptions
rainfall = def
  { _chartOptions_title = Just $ def
    {
      _title_text = Just "Rainfall/Water volume"
    , _title_subtext = Just "Flow of water and rainfall"
    , _title_pos = Just $ def {
        _pos_left = Just $ PosAlign_Align Align_Center
        }
    }
  , _chartOptions_grid = def
    { _grid_pos = Just $ def { _pos_bottom = Just $ PosAlign_Pixel 80 }
    } : []
  , _chartOptions_toolbox = Just $ def
    { _toolBox_features =
      [ emptyDataZoom { _feature_yAxisIndex = Just $ Aeson.String "none" }
      , emptyRestore
      , emptySaveAsImage
      ]
    }
  , _chartOptions_tooltip = Just $ def
    { _toolTip_trigger = Just "axis"
    , _toolTip_axisPointer = Just $ def
      { _axisPointer_type = Just $ "cross"
      , _axisPointer_label = Just $ def
        { _label_backgroundColor = Just "#505765" }
      }
    }
  , _chartOptions_legend = Just $ def
    { _legend_data = Just $ [ (xSeriesName, def)
                            , (ySeriesName, def)
                            ]
    , _legend_pos = Just $ def {_pos_left = Just $ PosAlign_Align Align_Left }
    }
  , _chartOptions_dataZoom =
    [ def
      { _dataZoom_show = Just True
      , _dataZoom_realtime = Just True
      , _dataZoom_start = Just $ Aeson.Number 65
      , _dataZoom_end = Just $ Aeson.Number 85
      }
    , def
      { _dataZoom_type = Just "inside"
      , _dataZoom_realtime = Just True
      , _dataZoom_start = Just $ Aeson.Number 65
      , _dataZoom_end = Just $ Aeson.Number 85
      }
    ]
  , _chartOptions_xAxis = def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing
                              , _axis_axisLine = Just $ def
                                { _axisLine_onZero = Just False }
                              , _axis_boundaryGap = Just $ Left False} :[]
  , _chartOptions_yAxis =
    [ def { _axis_type = Just AxisType_Value
          , _axis_name = Just "Water Flow (m^3/s)"
          , _axis_max = Just $ Left 500
          }
    , def { _axis_type = Just AxisType_Value
          , _axis_name = Just "Rainfall (mm)"
          , _axis_max = Just $ Left 5
          , _axis_inverse = Just $ True
          , _axis_nameLocation = Just $ AxisNameLocation_Start
          }
    ]
  , _chartOptions_series =
    [ Some.This $ SeriesT_Line $ def
        & series_data ?~ (map DataDouble waterFlowData)
        & series_name ?~ xSeriesName
        & series_animation ?~ False
        & series_areaStyle ?~ def
        & series_lineStyle ?~ def { _lineStyle_width = Just 1 }
        & series_markArea ?~ def
        { _markArea_silent = Just True
        , _markArea_data = Just $ Aeson.Array $ V.singleton $ Aeson.Array $ V.fromList
            [ Aeson.Object $ HashMap.singleton "xAxis"
              (Aeson.String $ dateF 9 12 7)
            , Aeson.Object $ HashMap.singleton "xAxis"
              (Aeson.String $ dateF 9 22 7)
            ]
        }
    , Some.This $ SeriesT_Line $ def
        & series_data ?~ (map DataDouble rainfallData)
        & series_name ?~ ySeriesName
        & series_yAxisIndex ?~ 1
        & series_animation ?~ False
        & series_areaStyle ?~ def
        & series_lineStyle ?~ def { _lineStyle_width = Just 1 }
        & series_markArea ?~ def
        { _markArea_silent = Just True
        , _markArea_data = Just $ Aeson.Array $ V.singleton $ Aeson.Array $ V.fromList
            [ Aeson.Object $ HashMap.singleton "xAxis"
              (Aeson.String $ dateF 9 10 7)
            , Aeson.Object $ HashMap.singleton "xAxis"
              (Aeson.String $ dateF 9 20 7)
            ]
        }
    ]
  }
  where
    xSeriesName = "Water flow"
    ySeriesName = "Rainfall"
    xAxisData = [dateF 6 12 t | t <- [2..23]]
      <> [dateF 6 d t | d <- [13..30], t <- [0..23]]
      <> [dateF 7 d t | d <- [1..31], t <- [0..23]]
      <> [dateF 8 d t | d <- [1..31], t <- [0..23]]
      <> [dateF 9 d t | d <- [1..30], t <- [0..23]]
      <> [dateF 10 d t | d <- [1..17], t <- [0..23]]
      <> [dateF 10 18 t | t <- [0..8]]
    dateF m d t = "2009/" <> tshow m <> "/" <> tshow d <> "\n" <> tshow t <> ":00"

tshow :: (Show a) => a -> Text
tshow = T.pack . show

largeScaleAreaChart :: RandomGen g => g -> ChartOptions
largeScaleAreaChart rGen = def
  { _chartOptions_title = Just $ def
    {
      _title_text = Just "Large Scale Area Chart"
    , _title_pos = Just $ def {
        _pos_left = Just $ PosAlign_Align Align_Center
        }
    }
  , _chartOptions_tooltip = Just $ def
    { _toolTip_trigger = Just "axis"
    -- TODO
    -- , _toolTip_pos = 
    }
  , _chartOptions_toolbox = Just $ def
    { _toolBox_features =
      [ emptyDataZoom { _feature_yAxisIndex = Just $ Aeson.String "none" }
      , emptyRestore
      , emptySaveAsImage
      ]
    }
  , _chartOptions_dataZoom =
    [ def
      { _dataZoom_show = Just True
      , _dataZoom_handleSize = Just (SN_String "80%")
      , _dataZoom_handleIcon = Just "M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z"
      , _dataZoom_handleStyle = Just $ def
        { _itemStyle_color = Just "#fff"
        , _itemStyle_shadow = Just $ def
          { _shadow_blur = Just 3
          , _shadow_color = Just "rgba(0, 0, 0, 0.6)"
          , _shadow_offsetX = Just 2
          , _shadow_offsetY = Just 2
          }
        }
      , _dataZoom_start = Just $ Aeson.Number 0
      , _dataZoom_end = Just $ Aeson.Number 10
      }
    , def
      { _dataZoom_type = Just "inside"
      , _dataZoom_start = Just $ Aeson.Number 0
      , _dataZoom_end = Just $ Aeson.Number 10
      }
    ]
  , _chartOptions_xAxis = def { _axis_type = Just AxisType_Category
                              , _axis_data = Just $ zip xAxisData $ repeat Nothing
                              , _axis_boundaryGap = Just $ Left False} :[]
  , _chartOptions_yAxis =
    [ def { _axis_type = Just AxisType_Value
          , _axis_boundaryGap = Just $ Right (SizeValue_Numeric 0, SizeValue_Percent 100)
          }
    ]
  , _chartOptions_series =
    [ Some.This $ SeriesT_Line $ def
        & series_data ?~ (map DataDouble randomData)
        & series_name ?~ xSeriesName
        & series_smooth ?~ Left True
        & series_itemStyle ?~ def { _itemStyle_color = Just "rgb(255, 70, 131)" }
        -- TODO uses new
        & series_areaStyle ?~ def
    ]
  }
  where
    xSeriesName = "Random Data"
    xAxisData = take dataSize $ map (\d -> tshow $ addDays d startDate) [0..]
    startDate = fromGregorian 1968 9 3
    dataSize = 20000
    rs :: [Double]
    rs = randomRs (-10, 10) rGen
    randomData = take dataSize $ scanl (\d r -> r + d) 50 rs

confidenceBandJsonData :: Text
confidenceBandJsonData = do
  "https://raw.githubusercontent.com/ecomfe/echarts-examples/gh-pages/public/data/asset/data/confidence-band.json"

data ConfidenceData = ConfidenceData
  { _confidenceData_value :: Double
  , _confidenceData_date :: Text
  , _confidenceData_l :: Double
  , _confidenceData_u :: Double
  }
  deriving (Generic)

instance FromJSON ConfidenceData where
  parseJSON = genericParseJSON $ defaultOptions
    { fieldLabelModifier = drop $ T.length "_confidenceData_"
    }

confidenceBand :: [ConfidenceData] -> ChartOptions
confidenceBand confData = def
  { _chartOptions_title = Just $ def
    {
      _title_text = Just "Confidence Band"
    , _title_pos = Just $ def {
        _pos_left = Just $ PosAlign_Align Align_Center
        }
    }
  , _chartOptions_tooltip = Just $ def
    { _toolTip_trigger = Just "axis"
    , _toolTip_axisPointer = Just $ def
      { _axisPointer_type = Just $ "cross"
      -- , _axisPointer_animation = Just False
      , _axisPointer_label = Just $ def
        { _label_backgroundColor = Just "#ccc"
        , _label_border = Just $ def
          { _border_color = Just "#aaa"
          , _border_width = Just 1
          }
        , _label_shadow = Just $ def
          { _shadow_blur = Just 0
          , _shadow_offsetX = Just 0
          , _shadow_offsetY = Just 0
          }
        , _label_textStyle = Just $ def
          { _textStyle_color = Just "#222"
          }
        }
      }
    -- TODO function
    -- , _toolTip_formatter =
    }
  , _chartOptions_grid = def
    { _grid_pos = Just $ def
      { _pos_left = Just $ PosAlign_Percent 3
      , _pos_right = Just $ PosAlign_Percent 4
      , _pos_bottom = Just $ PosAlign_Percent 3
      }
    , _grid_containLabel = Just True
    } : []
  , _chartOptions_xAxis = def
    { _axis_type = Just AxisType_Category
    , _axis_data = Just $ zip xAxisData $ repeat Nothing
    -- TODO function
    -- , _axis_label = Just $ def
    --   { _axisLabel_formatter =
    --   }
    , _axis_splitLine = Just $ def { _splitLine_show = Just False }
    , _axis_boundaryGap = Just $ Left False
    } :[]
  , _chartOptions_yAxis =
    [ def { _axis_type = Just AxisType_Value
          , _axis_splitLine = Just $ def { _splitLine_show = Just False }
          , _axis_splitNumber = Just $ 3
          }
    ]
  , _chartOptions_series =
    [ Some.This $ SeriesT_Line $ def
        & series_data ?~ (map (DataDouble . ((+) base) . _confidenceData_l) confData)
        & series_name ?~ "L"
        & series_stack ?~ "confidence-band"
        & series_symbol ?~ "none"
        & series_lineStyle ?~ def { _lineStyle_opacity = Just 0 }
    , Some.This $ SeriesT_Line $ def
        & series_data ?~ (map (DataDouble . (\v -> _confidenceData_u v - _confidenceData_l v)) confData)
        & series_name ?~ "U"
        & series_stack ?~ "confidence-band"
        & series_symbol ?~ "none"
        & series_lineStyle ?~ def { _lineStyle_opacity = Just 0 }
        & series_areaStyle ?~ def { _areaStyle_color = Just "#ccc" }
    , Some.This $ SeriesT_Line $ def
        & series_data ?~ (map (DataDouble . ((+) base) . _confidenceData_value) confData)
        & series_symbolSize ?~ Aeson.Number 6
        & series_showSymbol ?~ False
        & series_hoverAnimation ?~ False
        & series_itemStyle ?~ def { _itemStyle_color = Just "#c23531" }
    ]
  }
  where
    base = negate $ minimum $ map _confidenceData_l confData
    xSeriesName = "Confidence Band"
    xAxisData = map _confidenceData_date confData

rainfallAndWaterFlow :: ChartOptions
rainfallAndWaterFlow = def
  { _chartOptions_title = Just $ def
    {
      _title_text = Just "Rainfall and Water volume"
    , _title_subtext = Just "Flow of water and rainfall"
    , _title_pos = Just $ def {
        _pos_left = Just $ PosAlign_Align Align_Center
        }
    }
  , _chartOptions_tooltip = Just $ def
    { _toolTip_trigger = Just "axis"
    }
  , _chartOptions_legend = Just $ def
    { _legend_data = Just $ [ (xSeriesName, def)
                            , (ySeriesName, def)
                            ]
    , _legend_pos = Just $ def {_pos_left = Just $ PosAlign_Align Align_Left }
    }
  , _chartOptions_toolbox = Just $ def
    { _toolBox_features =
      [ emptyDataZoom { _feature_yAxisIndex = Just $ Aeson.String "none" }
      , emptyRestore
      , emptySaveAsImage
      ]
    }
  , _chartOptions_axisPointer = Just $ def
    { _axisPointer_link = Just $ Aeson.Object $
      HashMap.singleton "xAxisIndex" (Aeson.String "all")
    }
  , _chartOptions_dataZoom =
    [ def
      { _dataZoom_show = Just True
      , _dataZoom_realtime = Just True
      , _dataZoom_start = Just $ Aeson.Number 30
      , _dataZoom_end = Just $ Aeson.Number 70
      }
    , def
      { _dataZoom_type = Just "inside"
      , _dataZoom_realtime = Just True
      , _dataZoom_start = Just $ Aeson.Number 30
      , _dataZoom_end = Just $ Aeson.Number 70
      , _dataZoom_xAxisIndex = Just $ [0, 1]
      }
    ]
  , _chartOptions_grid = def
    { _grid_pos = Just $ def
      { _pos_left = Just $ PosAlign_Pixel 50
      , _pos_right = Just $ PosAlign_Pixel 50
      }
    , _grid_size = Just $ def
      { _size_height = Just $ SizeValue_Percent 35
      }
    } : def
    { _grid_pos = Just $ def
      { _pos_left = Just $ PosAlign_Pixel 50
      , _pos_right = Just $ PosAlign_Pixel 50
      , _pos_top = Just $ PosAlign_Percent 55
      }
    , _grid_size = Just $ def
      { _size_height = Just $ SizeValue_Percent 35
      }
    } : []
  , _chartOptions_xAxis = def
    { _axis_type = Just AxisType_Category
    , _axis_data = Just $ zip xAxisData $ repeat Nothing
    , _axis_axisLine = Just $ def { _axisLine_onZero = Just True }
    , _axis_boundaryGap = Just $ Left False
    } : def
    { _axis_type = Just AxisType_Category
    , _axis_data = Just $ zip xAxisData $ repeat Nothing
    , _axis_axisLine = Just $ def { _axisLine_onZero = Just True }
    , _axis_boundaryGap = Just $ Left False
    , _axis_gridIndex = Just 1
    , _axis_position = Just AxisPosition_Top
    } :[]
  , _chartOptions_yAxis =
    [ def { _axis_type = Just AxisType_Value
          , _axis_name = Just "Water Flow (m^3/s)"
          , _axis_max = Just $ Left 500
          }
    , def { _axis_type = Just AxisType_Value
          , _axis_name = Just "Rainfall (mm)"
          , _axis_gridIndex = Just $ 1
          , _axis_inverse = Just $ True
          }
    ]
  , _chartOptions_series =
    [ Some.This $ SeriesT_Line $ def
        & series_data ?~ (map DataDouble waterFlowData)
        & series_name ?~ xSeriesName
        & series_hoverAnimation ?~ False
        & series_symbolSize ?~ Aeson.Number 8
    , Some.This $ SeriesT_Line $ def
        & series_data ?~ (map DataDouble rainfallData)
        & series_name ?~ ySeriesName
        & series_xAxisIndex ?~ 1
        & series_yAxisIndex ?~ 1
        & series_symbolSize ?~ Aeson.Number 8
    ]
  }
  where
    xSeriesName = "Water flow"
    ySeriesName = "Rainfall"
    xAxisData = [dateF 6 12 t | t <- [2..23]]
      <> [dateF 6 d t | d <- [13..30], t <- [0..23]]
      <> [dateF 7 d t | d <- [1..31], t <- [0..23]]
      <> [dateF 8 d t | d <- [1..31], t <- [0..23]]
      <> [dateF 9 d t | d <- [1..30], t <- [0..23]]
      <> [dateF 10 d t | d <- [1..17], t <- [0..23]]
      <> [dateF 10 18 t | t <- [0..8]]
    dateF m d t = "2009/" <> tshow m <> "/" <> tshow d <> "\n" <> tshow t <> ":00"

type AqiData = [(Text, Double)]

aqiChart :: AqiData -> ChartOptions
aqiChart aqiData = def
  { _chartOptions_title = Just $ def { _title_text = Just title }
  , _chartOptions_tooltip = Just $ def
    { _toolTip_trigger = Just "axis"
    }
  , _chartOptions_toolbox = Just $ def
    { _toolBox_features =
      [ emptyDataZoom { _feature_yAxisIndex = Just $ Aeson.String "none" }
      , emptyRestore
      , emptySaveAsImage
      ]
    , _toolBox_pos = Just $ def {_pos_left = Just $ PosAlign_Align Align_Center }
    }
  , _chartOptions_xAxis = def
    { _axis_data = Just $ zip (map fst aqiData) $ repeat Nothing
    } :[]
  , _chartOptions_yAxis =  def
    { _axis_splitLine = Just $ def { _splitLine_show = Just False }
    } : []
  , _chartOptions_dataZoom =
    [ def
      { _dataZoom_startValue = Just $ Aeson.String "2014-06-01"
      }
    , def
      { _dataZoom_type = Just "inside"
      }
    ]
  , _chartOptions_visualMap = def
    { _visualMap_pos = Just $ def
      { _pos_top = Just $ PosAlign_Pixel 10
      , _pos_right = Just $ PosAlign_Pixel 10
      }
    , _visualMap_pieces = Just $ Aeson.Array $ V.fromList
      (ffor (zip sections (tail sections)) $ \((l, c), (h, _)) ->
        Aeson.Object $ HashMap.fromList
          [ ("gt", Aeson.Number l)
          , ("lte", Aeson.Number h)
          , ("color", Aeson.String c)
          ])
        <> ((\(l,c) -> V.fromList [ Aeson.Object $ HashMap.fromList
             [ ("gt", Aeson.Number l)
             , ("color", Aeson.String c)
             ]
           ]) (last sections))

    , _visualMap_outOfRange = Just $ def
      { _inOutOfRange_color = Just $ Aeson.String "#999"
      }
    } : []
  , _chartOptions_series =
    [ Some.This $ SeriesT_Line $ def
        & series_data ?~ (map (DataDouble . snd) aqiData)
        & series_name ?~ title
        & series_markLine ?~ def
        { _markLine_silent = Just True
        , _markLine_data = Just $ Aeson.Array $ V.fromList $
          ffor (tail sections) $ \(l, _) ->
            Aeson.Object $ HashMap.singleton "yAxis" (Aeson.Number l)
        }
    ]
  }
  where
    title = "AQI Data"
    sections =
        [ (0, "#096")
        , (50, "#ffde33")
        , (100, "#ff9933")
        , (150, "#cc0033")
        , (200, "#660099")
        , (300, "#7e0023")
        ]

multipleXAxes :: ChartOptions
multipleXAxes = def
  & chartOptions_legend ?~ (def
    & legend_data ?~ [ (xSeriesName1, def)
                     , (xSeriesName2, def)
                     ])
  & chartOptions_tooltip ?~ (def
    & toolTip_trigger ?~ "none"
    & toolTip_axisPointer ?~ (def
      & axisPointer_type ?~ "Cross"))
  & chartOptions_grid .~ (def
    & grid_pos ?~ (def
      & pos_top ?~ PosAlign_Pixel 70
      & pos_bottom ?~ PosAlign_Pixel 50)) : []
  & chartOptions_yAxis .~ (def
    & axis_type ?~ AxisType_Value) : []
  & chartOptions_xAxis .~ (def
    & axis_type ?~ AxisType_Category
    & axis_axisTick ?~ (def & axisTick_alignWithLabel ?~ True)
    & axis_axisLine ?~ (def
      & axisLine_onZero ?~ False
      & axisLine_lineStyle ?~ (def & lineStyle_color ?~ colors !! 1))
    -- TODO formatter
    -- & axis_axisPointer ?~ (def
    --   & axisPointer_label ?~ def)
    & axis_data ?~ zip (months xSeriesName2) (repeat Nothing))
  : (def
    & axis_type ?~ AxisType_Category
    & axis_axisTick ?~ (def & axisTick_alignWithLabel ?~ True)
    & axis_axisLine ?~ (def
      & axisLine_onZero ?~ False
      & axisLine_lineStyle ?~ (def & lineStyle_color ?~ colors !! 0))
    -- TODO formatter
    -- & axis_axisPointer ?~ (def
    --   & axisPointer_label ?~ def)
    & axis_data ?~ zip (months xSeriesName1) (repeat Nothing)) : []
  & chartOptions_series .~ (Some.This $ SeriesT_Line $ def
    & series_smooth ?~ Left True
    & series_name ?~ xSeriesName1
    & series_xAxisIndex ?~ 1
    & series_data ?~ map DataDouble x1)
  : (Some.This $ SeriesT_Line $ def
    & series_smooth ?~ Left True
    & series_name ?~ xSeriesName2
    & series_data ?~ map DataDouble x2) : []
  where
    xSeriesName1 = "2015"
    xSeriesName2 = "2016"
    colors = ["#5793f3", "#d14a61", "#675bba"]
    months y = map (\m -> y <> "-" <> tshow m) [1..12]
    x1 = [2.6, 5.9, 9.0, 26.4, 28.7, 70.7, 175.6, 182.2, 48.7, 18.8, 6.0, 2.3]
    x2 = [3.9, 5.9, 11.1, 18.7, 48.3, 69.2, 231.6, 46.6, 55.4, 18.4, 10.3, 0.7]
