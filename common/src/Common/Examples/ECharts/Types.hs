{-# LANGUAGE DeriveGeneric #-}
module Common.Examples.ECharts.Types where

import Data.Aeson
import GHC.Generics (Generic)

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

instance FromJSON a => FromJSON (CpuStat a)
instance ToJSON a => ToJSON (CpuStat a)
