{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.Examples.ECharts.Main where

import Reflex.Dom.Widget.ECharts

import Frontend.Examples.ECharts.ExamplesData (rainfallData, waterFlowData)

import qualified Obelisk.ExecutableConfig

import Data.ByteString.Lazy (toStrict, fromStrict)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Fix (MonadFix)
import Control.Monad (void, replicateM)
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
import Data.ByteString (ByteString)

import Data.List.NonEmpty (nonEmpty)
import Text.URI
import Reflex.Dom.Core
import Obelisk.Route
import Common.Route
import Data.Functor.Sum

app
  :: forall t m js .
     ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     )
  => Maybe Text
  -> m ()
app r = prerender blank $ elAttr "div" ("style" =: "display: flex; flex-wrap: wrap") $ do
  mapM_ wrap
    [ basicLineChart
    , cpuStatTimeLineChart
    , stackedAreaChart
    , rainfall
    , multipleXAxes
    , largeScaleAreaChart
    ]

  dEv <- do
    pb <- getPostBuild
    d1 <- holdDyn Nothing
      =<< getAndDecode ((static @"data/confidence-band.json") <$ pb)
    -- d2 <- holdDyn Nothing
    --   =<< getAndDecode ((static @"data/aqi-beijing.json") <$ pb)
    -- let d = (,) <$> d1 <*> d2
    return $ fforMaybe (updated d1) id
  widgetHold blank $ ffor dEv $ \c -> do
    void $ wrap $ confidenceBand c
  return ()
  where
    wrap m = elAttr "div" ("style" =: "padding: 50px;") m

tickWithSpeedSelector
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadFix m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => m (Event t TickInfo)
tickWithSpeedSelector = do
  r <- rangeInput $ def
    & rangeInputConfig_initialValue .~ 1
    & rangeInputConfig_attributes .~ constDyn (("min" =: "0.1") <> ("max" =: "2") <> ("step" =: "0.1"))
  dyn ((\v -> tickLossyFromPostBuildTime (fromRational $ toRational v)) <$> (value r))
    >>= switchHold never

basicLineChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => m (Chart)
basicLineChart = do
  tick <- tickWithSpeedSelector
  let
    f _ m = Map.fromList $ zip xAxisData $ ls ++ [l]
      where (l:ls) = map (\x -> Map.findWithDefault (DataInt 0) x m) xAxisData
  dd <- do
    cb <- el "div" $ do
      el "label" $ text "Dynamic line 1"
      checkbox False def
    let ev = gate (current $ value cb) tick
    foldDyn f yAxisData ev
  dd2 <- do
    cb <- el "div" $ do
      el "label" $ text "Dynamic line 2"
      checkbox False def
    let ev = gate (current $ value cb) tick
    foldDyn f yAxisData2 ev

  xd <- do
    cb <- el "div" $ do
      el "label" $ text "X-axis"
      checkbox False def
    let ev = gate (current $ value cb) tick
    foldDyn (\_ (l:ls) -> ls ++ [l]) xAxisData ev

  let chartDataDyn = (0 =: (def, dd, xd)) <> (1 =: (dd2Series, dd2, xd))
      dd2Series = def
        & series_smooth ?~ Left True
        & series_areaStyle ?~ def

  lineChart (LineChartConfig (600, 400)
              (constDyn basicLineChartOpts)
              chartDataDyn
            )
  where
    yAxisData = Map.fromList $ zip xAxisData $ map DataInt $ reverse [820, 932, 901, 934, 1290, 1330, 1320]
    yAxisData2 = Map.fromList $ zip xAxisData $ map DataInt $ [820, 932, 901, 934, 1290, 1330, 1320]
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    basicLineChartOpts :: ChartOptions
    basicLineChartOpts = def
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Value
        ) : []
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Category
        & axis_data ?~ (zip xAxisData $ repeat Nothing)
        ) : []

multipleXAxes
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadFix m
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => m (Chart)
multipleXAxes =
  lineChart $ LineChartConfig (600, 400) (constDyn multipleXAxesOpts)
    (chartDataDyn)
  where
    chartDataDyn = (0 =: (s1, constDyn y1, constDyn x1)) <> (1 =: (s2, constDyn y2, constDyn x2))
    s1 = def
      & series_smooth ?~ Left True
      & series_name ?~ xSeriesName1
      & series_xAxisIndex ?~ 1
    s2 = def
      & series_smooth ?~ Left True
      & series_name ?~ xSeriesName2
    xSeriesName1 = "2015"
    xSeriesName2 = "2016"
    colors = ["#5793f3", "#d14a61", "#675bba"]
    y1 = Map.fromList $ zip (months xSeriesName1) $
      map DataDouble [2.6, 5.9, 9.0, 26.4, 28.7, 70.7, 175.6, 182.2, 48.7, 18.8, 6.0, 2.3]
    y2 = Map.fromList $ zip (months xSeriesName2) $
      map DataDouble [3.9, 5.9, 11.1, 18.7, 48.3, 69.2, 231.6, 46.6, 55.4, 18.4, 10.3, 0.7]
    months y = map (\m -> y <> "-" <> tshow m) [1..12]
    x1 = months xSeriesName1
    x2 = months xSeriesName2

    multipleXAxesOpts :: ChartOptions
    multipleXAxesOpts = def
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
        & axis_data ?~ zip x2 (repeat Nothing))
      : (def
        & axis_type ?~ AxisType_Category
        & axis_axisTick ?~ (def & axisTick_alignWithLabel ?~ True)
        & axis_axisLine ?~ (def
          & axisLine_onZero ?~ False
          & axisLine_lineStyle ?~ (def & lineStyle_color ?~ colors !! 0))
        -- TODO formatter
        -- & axis_axisPointer ?~ (def
        --   & axisPointer_label ?~ def)
        & axis_data ?~ zip x1 (repeat Nothing)) : []

data CpuStat a = CpuStat
  { _cpuStat_user :: a
  , _cpuStat_nice :: a
  , _cpuStat_system :: a
  , _cpuStat_idle :: a
  , _cpuStat_iowait :: a
  , _cpuStat_irq :: a
  , _cpuStat_softirq :: a
  , _cpuStat_steal :: a
  , _cpuStat_guest :: a
  , _cpuStat_guestNice :: a
  }
  deriving (Show, Read, Eq, Ord, Bounded, Generic)

cpuStatTimeLineChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadSample t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     , TriggerEvent t m
     )
  => m (Chart)
cpuStatTimeLineChart = do
  ev <- cpuStatGenData
  let
    chartData = Map.fromList $ map (\(t, f) -> (t, (s t, len, g f))) sNames
    g f = ffor ev $ \(t, c) -> [(t, f c)]
    s n = def
      & series_smooth ?~ Left True
      & series_name ?~ n
    len = 50
    opts :: ChartOptions
    opts = def
      & chartOptions_title ?~ (def
        & title_text ?~ "Time Line Chart")
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Value
        & axis_min ?~ Left 0
        & axis_max ?~ Left 101
                              ) : []
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Time) : []
    sNames =
      [ ("user", _cpuStat_user)
      , ("nice", _cpuStat_nice)
      , ("system", _cpuStat_system)
      , ("idle", _cpuStat_idle)
      , ("iowait", _cpuStat_iowait)
      , ("irq", _cpuStat_irq)
      , ("softirq", _cpuStat_softirq)
      , ("steal", _cpuStat_steal)
      , ("guest", _cpuStat_guest)
      , ("guestNice", _cpuStat_guestNice)
      ]
  timeLineChart $ TimeLineChartConfig (600, 400) (constDyn opts)
    chartData

cpuStatGenData
  :: forall t m js .
     ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadSample t m
     , MonadHold t m
     , MonadFix m
     , MonadIO (Performable m)
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     )
  => m (Event t (UTCTime, CpuStat Double))
cpuStatGenData = do
  tick <- tickWithSpeedSelector

  let
    initStats = CpuStat 0 0 0 0 0 0 0 0 0 0

  rec
    cpuStat <- holdDyn initStats (snd <$> ev)

    ev <- performEvent $ ffor (tag (current cpuStat) tick) $ \c -> do
      t <- liftIO $ getCurrentTime
      rVals <- liftIO $ replicateM 10 $
        getStdRandom (randomR (-10, 10))
      let
        f i v = min 100 (max 0 (v + (rVals !! i)))
        s = CpuStat
          { _cpuStat_user = f 0 $ _cpuStat_user c
          , _cpuStat_nice = f 1 $ _cpuStat_nice c
          , _cpuStat_system = f 2 $ _cpuStat_system c
          , _cpuStat_idle = f 3 $ _cpuStat_idle c
          , _cpuStat_iowait = f 4 $ _cpuStat_iowait c
          , _cpuStat_irq = f 5 $  _cpuStat_irq c
          , _cpuStat_softirq = f 6 $ _cpuStat_softirq c
          , _cpuStat_steal = f 7 $ _cpuStat_steal c
          , _cpuStat_guest = f 8 $ _cpuStat_guest c
          , _cpuStat_guestNice = f 9 $ _cpuStat_guestNice c
          }
      return (t, s)
  return ev

stackedAreaChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => m (Chart)
stackedAreaChart =
  lineChart $ LineChartConfig (600, 400) (constDyn opts) $ Map.fromList
    $ zip [0..] $ map (\(l, d) ->
                         (l, constDyn $ Map.fromList
                           (zip xAxisData $ map DataInt d)
                         , constDyn xAxisData))
      [ (l1, d1)
      , (l2, d2)
      , (l3, d3)
      , (l4, d4)
      , (l5, d5)
      ]
  where
    opts = def
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
                                  , _axis_boundaryGap = Just $ Left False} : []
      , _chartOptions_yAxis = def { _axis_type = Just AxisType_Value
                                  } : []
      }
    title = "Stacked Area Chart"
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    stackLabel = "stackLabel"
    l1 :: Series SeriesLine
    l1 = def
      & series_stack ?~ stackLabel
      & series_name ?~ "A"
      & series_areaStyle ?~ def
    d1 = [120, 132, 101, 134, 90, 230, 210]
    l2 = def
      & series_stack ?~ stackLabel
      & series_name ?~ "B"
      & series_areaStyle ?~ def
    d2 = [220, 182, 191, 234, 290, 330, 310]
    l3 = def
      & series_stack ?~ stackLabel
      & series_name ?~ "C"
      & series_areaStyle ?~ def
    d3 = [150, 232, 201, 154, 190, 330, 410]
    l4 = def
      & series_stack ?~ stackLabel
      & series_name ?~ "D"
      & series_areaStyle ?~ def
    d4 = [320, 332, 301, 334, 390, 330, 320]
    l5 = def
      & series_stack ?~ stackLabel
      & series_name ?~ "E"
      & series_areaStyle ?~ def
      & series_label ?~ def { _label_show = Just True, _label_position = Just $ Position_String "top"}
    d5 = [820, 932, 901, 934, 1290, 1330, 1320]

rainfall
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => m (Chart)
rainfall =
  lineChart $ LineChartConfig (600, 400) (constDyn opts) $
    (0 =: (s1, constDyn d1, constDyn xAxisData))
    <> (1 =: (s2, constDyn d2, constDyn xAxisData))
  where
    opts = def
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
      }

    d1 = Map.fromList $ zip xAxisData $ map DataDouble waterFlowData
    s1 = def
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
    d2 = Map.fromList $ zip xAxisData $ map DataDouble rainfallData
    s2 = def
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

largeScaleAreaChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => m (Chart)
largeScaleAreaChart =
  lineChart $ LineChartConfig (600, 400) (constDyn opts) $
    (0 =: (s1, constDyn d1, constDyn xAxisData))
  where
    opts = def
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
                                  , _axis_boundaryGap = Just $ Left False} :[]
      , _chartOptions_yAxis =
        [ def { _axis_type = Just AxisType_Value
              , _axis_boundaryGap = Just $ Right (SizeValue_Numeric 0, SizeValue_Percent 100)
              }
        ]
      }
    xSeriesName = "Random Data"
    xAxisData = take dataSize $ map (\d -> tshow $ addDays d startDate) [0..]
    startDate = fromGregorian 1968 9 3
    dataSize = 20000
    rs :: [Double]
    rs = randomRs (-10, 10) (mkStdGen 0)
    s1 = def
      & series_name ?~ xSeriesName
      & series_smooth ?~ Left True
      & series_itemStyle ?~ def { _itemStyle_color = Just "rgb(255, 70, 131)" }
      -- TODO uses new
      & series_areaStyle ?~ def
    d1 = Map.fromList $ zip xAxisData $ map DataDouble $
      take dataSize $ scanl (\d r -> r + d) 50 rs

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

confidenceBand
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => [ConfidenceData]
  -> m (Chart)
confidenceBand confData =
  lineChart $ LineChartConfig (600, 400) (constDyn opts) $
    (1 =: (s1, constDyn d1, constDyn xAxisData))
    <> (2 =: (s2, constDyn d2, constDyn xAxisData))
    <> (3 =: (s3, constDyn d3, constDyn xAxisData))
  where
    opts = def
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
      }
    d1 =  Map.fromList $ zip xAxisData $
      map (DataDouble . ((+) base) . _confidenceData_l) confData
    s1 = def
      & series_name ?~ "L"
      & series_stack ?~ "confidence-band"
      & series_symbol ?~ "none"
      & series_lineStyle ?~ def { _lineStyle_opacity = Just 0 }
    d2 = Map.fromList $ zip xAxisData $
      map (DataDouble . (\v -> _confidenceData_u v - _confidenceData_l v)) confData
    s2 = def
      & series_name ?~ "U"
      & series_stack ?~ "confidence-band"
      & series_symbol ?~ "none"
      & series_lineStyle ?~ def { _lineStyle_opacity = Just 0 }
      & series_areaStyle ?~ def { _areaStyle_color = Just "#ccc" }
    d3 = Map.fromList $ zip xAxisData $
      map (DataDouble . ((+) base) . _confidenceData_value) confData
    s3 = def
      & series_symbolSize ?~ Aeson.Number 6
      & series_showSymbol ?~ False
      & series_hoverAnimation ?~ False
      & series_itemStyle ?~ def { _itemStyle_color = Just "#c23531" }
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
