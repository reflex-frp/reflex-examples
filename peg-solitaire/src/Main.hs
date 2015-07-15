{-# LANGUAGE RecursiveDo      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Main where

import Control.Applicative ((<$))
import Data.Array.IArray
import Data.FileEmbed
import Data.Monoid         ((<>))

import Reflex
import Reflex.Dom

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

type Point = (Int, Int)

-- | Idk : I don't know.
data Trool = Yes | Idk | No deriving (Eq)

type Board = Array Point Trool

-- | If the user chooses an amibuous starting point we need to remember it
--   so that when the destination is chosen we know what the move was.
data GameState = GameState {board :: Board, start :: Maybe Point}

-- | The "English" board.
initialBoard :: Board
initialBoard = array ((-3, -3), (3, 3))
                     [((i, j),valid i j) | i <- [(-3)..3], j <-[(-3)..3]]
  where
    valid 0 0 = No
    valid i j = if abs i < 2 || abs j < 2 then Yes else No

initialState :: GameState
initialState = GameState initialBoard Nothing

data Compass = North | East | South | West deriving (Enum)

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

main :: IO ()
main = mainWidgetWithCss ( $(embedFile "static/css/normalize.css")
                        <> $(embedFile "static/css/skeleton.css") ) $ do
  elClass "div" "container" $ do
    el "h1" $ text "Peg Solitaire"
    el "div" $ do
      el "br" (return ())
      el "div" $ do
        rec gs  <- foldDyn move initialState pos
            pos <- mkBoard gs
            el "h3" $ dynText =<< mapDyn (\g -> "score: " ++ (show $ score g)) gs
        return ()

-- | This is slightly complicated by the fact that some moves are not uniquely
--   determined by there starting position. In this case we need to remember
--   the statting point so that when the player disambiguates by clicking the
--   destination square we know what move was made.
move :: Point -> GameState -> GameState
move p gs = case (start gs, length lm) of
  -- If start is a Just value the player needs to chosse the destination.
  (Just s, _ ) -> if p `elem` cs
                  -- A legal destination was chosen so make the move and clean up
                  -- the board.
                  then game $ b // (map (\q -> (q, No)) cs)
                                // [(p, Yes), (s, No), (middle p s , No)]
                  else GameState b (Just s)

  -- No legal moves, try again.
  (Nothing, 0) -> game b

  -- One legal move so execute it.
  (Nothing, 1) -> game $ b // [(p, No), (fst $ head lm, No), (snd $ head lm, Yes)]

  -- Several legal moves so highlight the choices.
  _          -> GameState (b // map (\m -> (snd m, Idk)) lm) (Just p)
  where
    game brd = GameState brd Nothing
    b = board gs
    lm = legalMoves b p
    cs = idks b

-- | Create a game cell that returns it's coordinates when clicked.
cell :: MonadWidget t m => (Dynamic t GameState) -> Point -> m (Event t Point)
cell gs p = el "td" $ do
    rec (e, _) <- elDynAttr' "img" attrs (return ())
        attrs  <- mapDyn (square p . board) gs
    return $ p <$ _el_clicked e
    where
      square pos bd
        | not $ onBoard pos = off
        | bd ! pos == Yes = yes
        | bd ! pos == Idk = idk
        | otherwise = no
      yes = "src"=: "ball.svg"   <> "style" =: "display: block"
      no  = "src"=: "square.svg" <> "style" =: "display: block"
      idk = "src"=: "ball.svg"   <> "style" =: "display: block; opacity: 0.35"
      off = "src"=: "square.svg" <> "style" =: "display: block; opacity: 0"

-- | Row j of cells.
row :: MonadWidget t m => (Dynamic t GameState) -> Int ->  m (Event t Point)
row gs j = do
  el "tr" $ do
    cells <- mapM (cell gs) [(i, j) | i <- [-3..3]]
    return $ leftmost cells

-- The game board, returns the coordinates of the clicked cell.
mkBoard :: MonadWidget t m => (Dynamic t GameState) -> m (Event t Point)
mkBoard gs = do
  el "table" $ do
    rows <- mapM (row gs) [-3..3]
    return $ leftmost rows

--------------------------------------------------------------------------------
-- Game Logic
--------------------------------------------------------------------------------

legalMoves :: Board -> Point -> [(Point, Point)]
legalMoves b p = map snd . filter fst $ moves
  where
    legal = map (legalMove b p) [North .. West]
    pos   = map (\d -> (position 1 p d, position 2 p d)) [North .. West]
    moves = zip legal pos

legalMove :: Board -> Point -> Compass -> Bool
legalMove b p d = onBoard (position 1 p d)
               && onBoard (position 2 p d)
               && b ! p == Yes
               && b ! (position 1 p d) == Yes
               && b ! (position 2 p d) == No

onBoard :: Point -> Bool
onBoard (x, y) = (abs x < 2 || abs y < 2) && (abs x < 4 && abs y < 4)

position :: Int -> Point -> Compass -> Point
position n (x, y) dir = case dir of
  North -> (x, y-n)
  East  -> (x+n, y)
  South -> (x, y+n)
  West  -> (x-n, y)

score :: GameState -> Int
score gs = sum . map (\x -> if x == Yes then 1 else 0) . elems . board $ gs

idks :: Board -> [Point]
idks b = map fst . filter (\e -> snd e == Idk) . assocs  $ b

middle :: Point -> Point -> Point
middle (x1, y1) (x2, y2) = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)
