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
-- This implements the game rules of chess, without any Reflex
-- or frontend code. The purpose of this repo is to demonstrate
-- Reflex, not to demonstrate chess, so this is not intended
-- as an exemplary implementation of chess, just an example of
-- an implementation of chess that might be integrated with Reflex.
--
-- It is kept separate from the frontend code because in Reflex,
-- and in Haskell in general, it's important to separate something
-- that can be written purely (like the rules of chess) from something
-- from interface code (like the Reflex frontend to it).

import Control.Monad
import qualified Data.Array.IArray as A
import Data.Foldable
import Data.Maybe
import Data.Ord
import GHC.Generics

data Point = Point Word Word
    deriving (Eq, Show, Ord)

pointToTuple (Point x y) = (x, y)
tupleToPoint (x, y) = Point x y

instance A.Ix Point where
    range (p1, p2) = map tupleToPoint $ A.range (pointToTuple p1, pointToTuple p2)
    index (p1, p2) p3 = A.index (pointToTuple p1, pointToTuple p2) (pointToTuple p3)
    inRange (p1, p2) p3 = A.inRange (pointToTuple p1, pointToTuple p2) (pointToTuple p3)

data Color = White | Black deriving (Eq, Show)
data Piece = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)

data ColoredPiece = ColoredPiece Color Piece deriving (Eq)

instance Show ColoredPiece where
  show (ColoredPiece clr p) = show clr <> " " <> show p

type Board = A.Array Point (Maybe ColoredPiece)

initialBoard :: Board
initialBoard = A.array (Point 0 0, Point 7 7)
               [ (Point i j, square i j) | i <- [0..7], j <- [0..7] ]
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
    square i j = ColoredPiece <$> initialColor j <*> piece i j

data CastleState = CastleState
  { canCastleQueenSide :: Bool
  , canCastleKingSide  :: Bool
  } deriving (Show, Eq)

data ChessConfig = ChessConfig
  { config_selfCapture :: Bool
  } deriving (Eq, Show)

data GameState = GameState
  { gameState_board       :: Board
  , gameState_turn        :: Color
  , gameState_castle      :: Color -> CastleState
  , gameState_phantomPawn :: (Maybe Point)
  , gameState_moveCount   :: Int
  , gameState_config      :: ChessConfig
  } deriving Eq

instance Eq (Color -> CastleState) where
  a == b = a White == b White && a Black == b Black

instance Ord GameState where
  compare = comparing gameState_moveCount

initialGS :: ChessConfig -> GameState
initialGS config = GameState initialBoard White (const $ CastleState True True) Nothing 0 config

validSquares :: [Point]
validSquares = [Point i j | i <- [0..7], j <- [0..7]]

promotionPieces :: [Piece]
promotionPieces = [Queen, Rook, Bishop, Knight]

isValidSquare :: Point -> Bool
isValidSquare (Point x y) = x <= 7 && y <= 7

indexGS :: GameState -> Point -> Maybe ColoredPiece
gs `indexGS` point = do
  guard $ isValidSquare point
  gameState_board gs A.! point

-- | Can check whether either player is in check, for checkmate purposes
inCheck :: GameState -> Color -> Bool
inCheck board turn = not $ null $ do
  new <- validSquares
  Just (ColoredPiece newColor King) <- pure $ board `indexGS` new
  guard $ newColor == turn

  old <- validSquares
  let attacker = opponent turn
  toList $ basicMove Queen board attacker old new

inCheckmate :: GameState -> Bool
inCheckmate board = inCheck board turn && null escapeScenarios where
  turn = gameState_turn board
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
    maybeToList $ move board $ Move old new (gameState_turn board) piece
  playerPieces player = do
    square <- validSquares
    Just (ColoredPiece color piece) <- pure $ board `indexGS` square
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
  guard $ turn == gameState_turn board
  newBoard <- basicMove piece board turn old new
  guard $ not $ inCheck newBoard turn
  pure newBoard

-- basicMove is parameterized on who is moving to allow checking for check
-- TODO: This is a gnarly piece of code that could be much better factored
basicMove :: Piece -> GameState -> Color -> Point -> Point -> Maybe GameState
basicMove promotionPiece brd turn old new = do
  oldSquare@(Just (ColoredPiece color piece)) <- pure $ brd `indexGS` old
  guard $ turn == color
  guard $ old /= new
  guard $ promotionPiece `elem` promotionPieces
  let dest = brd `indexGS` new

  isCapture <- case dest of
    Just (ColoredPiece destColor _) -> do
      when (not $ config_selfCapture $ gameState_config brd) $ guard $ destColor /= turn
      pure True
    Nothing -> pure False

  let (Point oldX oldY, Point newX newY) = (old, new)
      diffX = toInteger newX - toInteger oldX
      diffY = toInteger newY - toInteger oldY
      absDiffX = abs diffX
      absDiffY = abs diffY

      rookLike = diffX == 0 || diffY == 0
      bishopLike = absDiffX == absDiffY
      pawnLike = diffX == 0 && (diffY == pawnDirection || diffY == 2 * pawnDirection && oldY == pawnRow)
      pawnCaptureLike = diffY == pawnDirection && absDiffX == 1
      knightLike = (absDiffX == 1 && absDiffY == 2) || (absDiffX == 2 && absDiffY == 1)
      kingLike = absDiffX <= 1 && absDiffY <= 1

      clearPath = all isNothing $ (brd `indexGS`) <$> steps where
          nextStep (Point x y) = Point (step diffX x) (step diffY y)
          step diff' x' = if diff' > 0
            then x' + 1
            else x' - 1
          steps = takeWhile (/= new) $ iterate nextStep $ nextStep old

      -- in the form of potential additional board modifications
      -- some special moves
      enPassant :: [(Point, Maybe ColoredPiece)]
      enPassant = fromMaybe [] $ do
        Pawn <- pure piece
        phantom@(Point phantomX _) <- gameState_phantomPawn brd
        guard pawnCaptureLike
        guard $ new == phantom
        let
          coordinates = case turn of
            White -> Point phantomX 4
            Black -> Point phantomX 3
        pure [(coordinates, Nothing)]

      promotion :: [(Point, Maybe ColoredPiece)]
      promotion = fromMaybe [] $ do
        Pawn <- pure piece
        guard $ case (turn, newY) of
          (White, 7) -> True
          (Black, 0) -> True
          _          -> False
        pure [(new, Just (ColoredPiece turn promotionPiece))]

      oldCastleState = gameState_castle brd turn

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
        let rookOld = Point rookX oldY
            rookNew = Point (if diffX > 0 then oldX + 1 else oldX - 1) newY
        _ <- basicMove Queen brd turn rookOld rookNew
        let rook = brd `indexGS` rookOld
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
    newGameStateCastle clr | clr /= turn = gameState_castle brd clr
                           | otherwise   = case (piece, oldX) of
      (King, _) -> CastleState False False
      (Rook, 7) -> oldCastleState { canCastleKingSide = False }
      (Rook, 0) -> oldCastleState { canCastleQueenSide = False }
      _         -> oldCastleState
    changes = [(old, Nothing), (new, oldSquare)] <> enPassant <> castle <> promotion
    steppedY = if diffY > 0 then oldY + 1 else oldY - 1

  pure $ GameState
    { gameState_board = gameState_board brd A.// changes
    , gameState_turn = opponent turn
    , gameState_castle = newGameStateCastle
    , gameState_phantomPawn = do
        Pawn <- pure piece
        guard $ absDiffY == 2
        pure $ Point oldX steppedY
    , gameState_moveCount = gameState_moveCount brd + 1
    , gameState_config = gameState_config brd
    }

  where
    pawnDirection = case turn of
      White -> 1
      Black -> -1
    pawnRow = case turn of
      White -> 1
      Black -> 6
