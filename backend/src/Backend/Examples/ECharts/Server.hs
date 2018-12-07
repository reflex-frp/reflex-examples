{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Backend.Examples.ECharts.Server where

import Common.Examples.ECharts.Types
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import Data.Word
import Network.WebSockets.Snap
import qualified Network.WebSockets as WS

type ServerState = (Int, Map Int WS.Connection)

application :: MVar ServerState -> WS.ServerApp
application connections pending = do
  conn <- WS.acceptRequest pending
  conId <- modifyMVar connections $ \(next, conns) ->
    return $ (,) (next + 1, Map.insert next conn conns) next
  let disconnect = modifyMVar_ connections $ \(n, conns) ->
        return (n, Map.delete conId conns)
  flip finally disconnect $ forever $ WS.receiveDataMessage conn

initServer :: IO (MVar ServerState)
initServer = do
  connections <- newMVar (0, Map.empty)
  let diffpct :: UTCTime -> CpuStat Word64 -> UTCTime -> CpuStat Word64 -> (CpuStat Word64 -> Word64) -> Int -> Double
      diffpct oldt old newt new f n =
        (realToFrac $ (f new) - (f old)) / (realToFrac $ diffUTCTime newt oldt) / realToFrac n
      cpuStatDiff :: UTCTime -> CpuStat Word64 -> UTCTime -> CpuStat Word64 -> Int -> CpuStat Double
      cpuStatDiff oldt old newt new n =
        CpuStat
          { _cpuStat_user = diffpct oldt old newt new _cpuStat_user n
          , _cpuStat_nice = diffpct oldt old newt new _cpuStat_nice n
          , _cpuStat_system = diffpct oldt old newt new _cpuStat_system n
          , _cpuStat_idle = diffpct oldt old newt new _cpuStat_idle n
          , _cpuStat_iowait = diffpct oldt old newt new _cpuStat_iowait n
          , _cpuStat_irq = diffpct oldt old newt new _cpuStat_irq n
          , _cpuStat_softirq = diffpct oldt old newt new _cpuStat_softirq n
          , _cpuStat_steal = diffpct oldt old newt new _cpuStat_steal n
          , _cpuStat_guest = diffpct oldt old newt new _cpuStat_guest n
          , _cpuStat_guestNice = diffpct oldt old newt new _cpuStat_guestNice n
          }
      loop :: Maybe (UTCTime, (Int, CpuStat Word64)) -> IO ()
      loop mold = do
        t <- getCurrentTime
        stat <- getCpuStat
        let x = do
              (oldt, (_, oldstat)) <- mold
              (n, s) <- stat
              return (cpuStatDiff oldt oldstat t s n, (n, s))
        case x of
          Nothing -> do
            threadDelay 100000
            loop $ fmap (\a -> (t, a)) stat
          Just (pct, stat') -> do
            let msg = WS.Text (encode (t, pct)) Nothing
            _ <- withMVar connections $ \(_, conns) -> forM conns $ \c -> WS.sendDataMessage c msg
            threadDelay 100000
            loop $ Just (t, stat')
  _ <- forkIO $ loop Nothing
  return connections

getCpuStat :: IO (Maybe (Int, CpuStat Word64))
getCpuStat = do
  s <- readFile "/proc/stat"
  _ <- evaluate $ length s -- Make readFile strict
  pure $ do
    let cpus = takeWhile (\x -> "cpu" `isPrefixOf` x) $ lines s
    cpuSummaryLine : _ <- pure cpus
    [user, nice, system, idle, iowait, irq, softirq, steal, guest, guestNice] <- pure $ map read $ drop 1 $ words cpuSummaryLine
    pure
      ( length (tail cpus)
      , CpuStat
          { _cpuStat_user = user
          , _cpuStat_nice = nice
          , _cpuStat_system = system
          , _cpuStat_idle = idle
          , _cpuStat_iowait = iowait
          , _cpuStat_irq = irq
          , _cpuStat_softirq = softirq
          , _cpuStat_steal = steal
          , _cpuStat_guest = guest
          , _cpuStat_guestNice = guestNice
          }
      )
