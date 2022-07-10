{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DataKinds             #-}

module Common.Examples.Chess where

import Control.Monad
import qualified Data.Array.IArray as A
import Data.Foldable
import Data.Maybe
import Data.Ord
import GHC.Generics

type Point = (Int, Int)

data Color = White | Black deriving (Eq, Show)
data Piece = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)

data ColoredPiece = ColoredPiece Color Piece deriving (Eq)

instance Show ColoredPiece where
  show (ColoredPiece clr p) = show clr <> " " <> show p

type Board = A.Array Point (Maybe ColoredPiece)

initialBoard :: Board
initialBoard = A.array ((0, 0), (7,7))
               [ ((i, j), square i j) | i <- [0..7], j <- [0..7] ]
  where
    initialColor n | n >= 6 = Just Black
                   | n <= 1 = Just White
                   | otherwise = Nothing
    mainRow n = n == 7 || n == 0
    pawnRow n = n == 1 || n == 6
    piece i j | pawnRow j = Just Pawn
              | mainRow j = case i of
                0 -> Just Rook
                1 -> Just Knight
                2 -> Just Bishop
                3 -> Just Queen
                4 -> Just King
                5 -> Just Bishop
                6 -> Just Knight
                7 -> Just Rook
                _ -> Nothing
              | otherwise = Nothing
    square :: Int -> Int -> Maybe ColoredPiece
    square i j = ColoredPiece <$> initialColor j <*> piece i j

data CastleState = CastleState
  { canCastleQueenSide :: Bool
  , canCastleKingSide  :: Bool
  } deriving (Show, Eq)

data ChessConfig = ChessConfig
  { configSelfCapture :: Bool
  } deriving (Eq, Show)

data GameState = GameState
  { gameStateBoard       :: Board
  , gameStateTurn        :: Color
  , gameStateCastle      :: Color -> CastleState
  , gameStatePhantomPawn :: (Maybe Point)
  , gameStateMoveCount   :: Int
  , gameStateConfig      :: ChessConfig
  } deriving Eq

instance Eq (Color -> CastleState) where
  a == b = a White == b White && a Black == b Black

instance Ord GameState where
  compare = comparing gameStateMoveCount

initialGS :: ChessConfig -> GameState
initialGS config = GameState initialBoard White (const $ CastleState True True) Nothing 0 config

validSquares :: [Point]
validSquares = [(i, j) | i <- [0..7], j <- [0..7]]

promotionPieces :: [Piece]
promotionPieces = [Queen, Rook, Bishop, Knight]

isValidSquare :: Point -> Bool
isValidSquare (x, y) = x >= 0 && y >= 0 && x <= 7 && y <= 7

(<!>) :: GameState -> Point -> Maybe ColoredPiece
gs <!> point = do
  guard $ isValidSquare point
  gameStateBoard gs A.! point

-- Can check whether either player is in check, for checkmate purposes
inCheck :: GameState -> Color -> Bool
inCheck board turn = not $ null $ do
  new <- validSquares
  Just (ColoredPiece newColor King) <- pure $ board <!> new
  guard $ newColor == turn

  old <- validSquares
  let attacker = opponent turn
  toList $ basicMove Queen board attacker old new

inCheckmate :: GameState -> Bool
inCheckmate board = inCheck board turn && null escapeScenarios where
  turn = gameStateTurn board
  escapeScenarios = do
    old <- validSquares
    new <- validSquares
    piece <- promotionPieces
    newBoard <- toList $ basicMove piece board turn old new
    guard $ not $ inCheck newBoard turn

inStalemate :: GameState -> Bool
inStalemate board = or
  [ not (inCheckmate board) && null validMoves
  , playerPieces White == [King] && playerPieces Black == [King]
  ] where
  validMoves = do
    old <- validSquares
    new <- validSquares
    piece <- promotionPieces
    maybeToList $ move board $ Move old new (gameStateTurn board) piece
  playerPieces player = do
    square <- validSquares
    Just (ColoredPiece color piece) <- pure $ board <!> square
    guard $ color == player
    pure piece

opponent :: Color -> Color
opponent = \case
  Black -> White
  White -> Black

data Move = Move
  { moveStart :: Point
  , moveFinish :: Point
  , movePlayer :: Color
  , movePromotion :: Piece
  }
  deriving Show

move :: GameState -> Move -> Maybe GameState
move board (Move old new turn piece) = do
  guard $ turn == gameStateTurn board
  newBoard <- basicMove piece board turn old new
  guard $ not $ inCheck newBoard turn
  pure newBoard

-- basicMove is parameterized on who is moving to allow checking for check
basicMove :: Piece -> GameState -> Color -> Point -> Point -> Maybe GameState
basicMove promotionPiece brd turn old new = do
  oldSquare@(Just (ColoredPiece color piece)) <- pure $ brd <!> old
  guard $ turn == color
  guard $ old /= new
  guard $ promotionPiece `elem` promotionPieces
  let dest = brd <!> new

  isCapture <- case dest of
    Just (ColoredPiece destColor _) -> do
      when (not $ configSelfCapture $ gameStateConfig brd) $ guard $ destColor /= turn
      pure True
    Nothing -> pure False

  let ((oldX, oldY), (newX, newY)) = (old, new)
      diffX = newX - oldX
      diffY = newY - oldY

      rookLike = diffX == 0 || diffY == 0
      bishopLike = abs diffX == abs diffY
      pawnLike = diffX == 0 && (diffY == pawnDirection || diffY == 2 * pawnDirection && oldY == pawnRow)
      pawnCaptureLike = diffY == pawnDirection && abs diffX == 1
      knightLike = (abs diffX == 1 && abs diffY == 2) || (abs diffX == 2 && abs diffY == 1)
      kingLike = abs diffX <= 1 && abs diffY <= 1

      stepX = signum diffX
      stepY = signum diffY
      nextStep (x, y) = (x + stepX, y + stepY)
      -- theoretically, the `takeWhile isValidSquare` should be unnecessary
      -- but I'm including it anyway so this value is always finite even with
      -- knight's moves
      steps = takeWhile (/= new) $ takeWhile isValidSquare $ iterate nextStep $ nextStep old
      clearPath = all isNothing $ (brd <!>) <$> steps

      -- in the form of potential additional board modifications
      -- some special moves
      enPassant :: [(Point, Maybe ColoredPiece)]
      enPassant = fromMaybe [] $ do
        Pawn <- pure piece
        phantom@(phantomX, _) <- gameStatePhantomPawn brd
        guard pawnCaptureLike
        guard $ new == phantom
        let
          coordinates = case turn of
            White -> (phantomX, 4)
            Black -> (phantomX, 3)
        pure [(coordinates, Nothing)]

      promotion :: [(Point, Maybe ColoredPiece)]
      promotion = fromMaybe [] $ do
        Pawn <- pure piece
        guard $ case (turn, newY) of
          (White, 7) -> True
          (Black, 0) -> True
          _          -> False
        pure [(new, Just (ColoredPiece turn promotionPiece))]

      oldCastleState = gameStateCastle brd turn

      -- TODO: Handle e.g. in check restriction, other restrictions
      castle :: [(Point, Maybe ColoredPiece)]
      castle = fromMaybe [] $ do
        King <- pure piece
        (test, rookX) <- case diffX of
          -2 -> pure (canCastleQueenSide, 0)
          2  -> pure (canCastleKingSide, 7)
          _  -> Nothing
        guard $ test oldCastleState
        guard clearPath
        -- TODO: Check that all squares the rook traverses are not under attack.
        let rookOld = (rookX, oldY)
            rookNew = (oldX + stepX, newY)
        _ <- basicMove Queen brd turn rookOld rookNew
        let rook = brd <!> rookOld
        pure $ [(rookOld, Nothing), (rookNew, rook)]

  case piece of
    Pawn | isCapture      -> guard pawnCaptureLike
    Pawn | null enPassant -> guard $ pawnLike && clearPath
    Pawn | otherwise      -> pure ()

    Bishop                -> guard $ bishopLike && clearPath
    Rook                  -> guard $ rookLike && clearPath
    Queen                 -> guard $ (rookLike || bishopLike) && clearPath
    Knight                -> guard knightLike

    King | null castle    -> guard kingLike
    King | otherwise      -> pure ()

  let
    newGameStateCastle clr | clr /= turn = gameStateCastle brd clr
                           | otherwise   = case (piece, oldX) of
      (King, _) -> CastleState False False
      (Rook, 7) -> oldCastleState { canCastleKingSide = False }
      (Rook, 0) -> oldCastleState { canCastleQueenSide = False }
      _         -> oldCastleState
    changes = [(old, Nothing), (new, oldSquare)] <> enPassant <> castle <> promotion

  pure $ GameState
    { gameStateBoard = gameStateBoard brd A.// changes
    , gameStateTurn = opponent turn
    , gameStateCastle = newGameStateCastle
    , gameStatePhantomPawn = do
        Pawn <- pure piece
        guard $ abs diffY == 2
        pure $ (oldX, oldY + stepY)
    , gameStateMoveCount = gameStateMoveCount brd + 1
    , gameStateConfig = gameStateConfig brd
    }

  where
    pawnDirection = case turn of
      White -> 1
      Black -> -1
    pawnRow = case turn of
      White -> 1
      Black -> 6
