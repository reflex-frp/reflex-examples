{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DataKinds             #-}

module Frontend.Examples.PegSolitaire.Main where

import           Control.Arrow     ((&&&))
import           Data.Array.IArray as A
import           Data.Monoid       ((<>))
import qualified Data.Text         as T

import           Reflex
import           Reflex.Dom        hiding (mainWidgetWithCss)
import           Reflex.Dom.Core   (mainWidgetWithCss)
import           Obelisk.Generated.Static
import Control.Monad.Fix (MonadFix)

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
initialBoard = A.array ((-3, -3), (3, 3))
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

-- main :: IO ()
-- main = run $ mainWidgetWithCss
--     ( $(embedFile "static/css/normalize.css")
--       <> $(embedFile "static/css/skeleton.css")
--       <> $(embedFile "static/css/font-awesome.min.css")
--     ) app

-- TODO Add CSS head

app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     )
  => m ()
app = elClass "div" "container" $ do
  el "br" blank
  elAttr "h1" ("style" =: "text-align: center") $ text "PEG SOLITAIRE"
  el "div" $ do
    el "br" blank
    el "div" $ do
      rec
        gs  <- foldDyn move initialState pos
        pos <- mkBoard gs
        elAttr "h3" ("style" =: "text-align: center") $
          dynText $ fmap (\g -> "Score: " <> (T.pack . show . score) g) gs
      return ()

-- | This is slightly complicated by the fact that some moves are not uniquely
--   determined by there starting position. In this case we need to remember
--   the starting point so that when the player disambiguates by clicking the
--   destination square we know what move was made.
move :: Point -> GameState -> GameState
move p gs = case (start gs, length lm) of
  -- If start is a Just value the player needs to chosse the destination.
  (Just s, _ ) -> if p `elem` cs
                  -- A legal destination was chosen so make the move and clean up
                  -- the board.
                  then game $ b // map (\q -> (q, No)) cs
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
cell
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     )
  => Dynamic t GameState -> Point -> m (Event t Point)
cell gs p = el "td" $ do
    rec (e, _) <- elDynAttr' "img" attrs (return ())
        attrs  <- (return . fmap (square p . board)) gs
    return $ p <$ domEvent Click e
    where
      square pos bd
        | not $ onBoard pos = off
        | bd A.! pos == Yes = yes
        | bd A.! pos == Idk = idk
        | otherwise = no
      yes = "src"=: static @"peg-solitaire/images/ball.svg"
        <> "style" =: "display: block"
      no  = "src"=: static @"peg-solitaire/images/square.svg"
        <> "style" =: "display: block"
      idk = "src"=: static @"peg-solitaire/images/ball.svg"
        <> "style" =: "display: block; opacity: 0.35"
      off = "src"=: "static/images/square.svg"
        <> "style" =: "display: block; opacity: 0"

-- | Row j of cells.
row
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     )
  => Dynamic t GameState -> Int ->  m (Event t Point)
row gs j =
  el "tr" $ do
    cells <- mapM (cell gs) [(i, j) | i <- [-3..3]]
    return $ leftmost cells

-- The game board, returns the coordinates of the clicked cell.
mkBoard
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     )
  => Dynamic t GameState -> m (Event t Point)
mkBoard gs =
  elAttr "table" ("style" =: "margin-left: auto; margin-right: auto") $ do
    rows <- mapM (row gs) [-3..3]
    return $ leftmost rows

--------------------------------------------------------------------------------
-- Game Logic
--------------------------------------------------------------------------------

legalMoves :: Board -> Point -> [(Point, Point)]
legalMoves b p = map snd . filter fst $ moves
  where
    legal = map (legalMove b p) [North .. West]
    pos   = map (position 1 p &&& position 2 p) [North .. West]
    moves = zip legal pos

legalMove :: Board -> Point -> Compass -> Bool
legalMove b p d = onBoard (position 1 p d)
               && onBoard (position 2 p d)
               && b A.! p == Yes
               && b A.! position 1 p d == Yes
               && b A.! position 2 p d == No

onBoard :: Point -> Bool
onBoard (x, y) = (abs x < 2 || abs y < 2) && (abs x < 4 && abs y < 4)

position :: Int -> Point -> Compass -> Point
position n (x, y) dir = case dir of
  North -> (x, y-n)
  East  -> (x+n, y)
  South -> (x, y+n)
  West  -> (x-n, y)

score :: GameState -> Int
score = sum . map (\x -> if x == Yes then 1 else 0) . elems . board

idks :: Board -> [Point]
idks = map fst . filter (\e -> snd e == Idk) . assocs

middle :: Point -> Point -> Point
middle (x1, y1) (x2, y2) = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)
