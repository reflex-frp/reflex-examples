{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

----------------------------------------------------------------------
-- Types for othello
----------------------------------------------------------------------

module Types where

import           Control.DeepSeq
import           GHC.Generics
import           Data.Array

type Position = (Int, Int)

data Square = Empty | Black | White
  deriving (Show, Eq, Generic, NFData)

type Board = Array Position Square

data Input = BlackMove Position | WhiteMove Game deriving (Generic, NFData)

data Game = Game { player :: Square, board :: Board } deriving (Generic, NFData)

squares :: [Position]
squares = [(x, y) | y <- [1..8], x <- [1..8]]
