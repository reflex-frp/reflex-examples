----------------------------------------------------------------------
-- Game Logic
----------------------------------------------------------------------

module Game where

import           Types
import           Data.Array

type Line = [Position]

-- Memoize line calculations
line :: Array (Int, Int, Int) Line
line = array ((1, 1, 1), (8, 8, 8))
             [((i, j, d), line' i j d) | i <- [1..8], j <- [1..8], d <- [1..8]]
  where
    line' :: Int -> Int -> Int -> Line
    line' x y 1 = [(x, y + h)     | h <- [1..8], y+h <= 8]
    line' x y 2 = [(x, y - h)     | h <- [1..8], y-h >= 1]
    line' x y 3 = [(x + h, y)     | h <- [1..8], x+h <= 8]
    line' x y 4 = [(x - h, y)     | h <- [1..8], x-h >= 1]
    line' x y 5 = [(x + h, y + h) | h <- [1..8], y+h <= 8, x+h <= 8]
    line' x y 6 = [(x + h, y - h) | h <- [1..8], y-h >= 1, x+h <= 8]
    line' x y 7 = [(x - h, y + h) | h <- [1..8], y+h <= 8, x-h >= 1]
    line' x y 8 = [(x - h, y - h) | h <- [1..8], y-h >= 1, x-h >= 1]
    line' _ _ _ = []

-- pieces brd = map (brd !)
pieces :: Board -> Line -> [Square]
pieces brd = map (\x -> brd ! x)
{-# INLINE pieces #-}

opposite :: Square -> Square
opposite Black = White
opposite White = Black
opposite Empty = Empty

newBoard :: Board
newBoard = emptyArray // [((4,4), White),((4,5), Black),((5,4), Black),((5,5), White)]
  where
    emptyArray = listArray ((1,1),(8,8)) (repeat Empty)

newGame :: Game
newGame = Game Black newBoard

setPosition :: Board -> Position -> Square -> Board
setPosition brd square p =
    if (brd ! square) /= Empty
    then error $ "square " ++ show square ++ " is not empty"
    else brd // [(square, p)]

toFlip :: Board -> Square -> Line -> Line
toFlip _ _ []   = []
toFlip _ _ ([_]) = []
toFlip b p l
  | b ! head l == p || b ! head l == Empty = [] -- short circuit for obvious cases.
  | zs /= [] && fst (head zs) == p = map snd ys
  | otherwise = []
  where
    (ys, zs) = span ((== opposite p) . fst) $ zip (pieces b l) l

toFlipAll :: Board -> Square -> Position -> [Position]
toFlipAll b p s = concat [toFlip b p l | l <- map ln [1..8]]
  where
    ln z = line ! (fst s, snd s, z)

flipBoard :: Board -> Square -> Position -> Board
flipBoard b p s = b // ((s, p) : zip flips (repeat p))
  where
    flips = toFlipAll b p s

isLegal :: Board -> Square -> Position -> Bool
isLegal b p s = b ! s == Empty && (not . null $ toFlipAll b p s)

legalPositions :: Game -> [Position]
legalPositions (Game p b) = filter (isLegal b p) squares

legalMoves :: Game -> [(Game, Position)]
legalMoves g@(Game p b) = zip gs ls
  where
    ls = filter (isLegal b p) squares
    gs = map (\s ->  move s g) ls

move :: Position -> Game -> Game
move square g@(Game p b)
  | null (legalMoves g) = Game (opposite p) b
  | not (isLegal b p square) = g
  | otherwise = Game (opposite p) (flipBoard b p square)

isOver :: Board -> Bool
isOver b = not (any (isLegal b Black) squares || any (isLegal b White) squares)

findWinner :: Board -> Square
findWinner b
  | isOver b = case compare black white of
      GT -> Black
      LT -> White
      EQ -> Empty -- We use Empty to indicate a draw.
  | otherwise = error "The game is not over"
  where
    black = length $ filter (\s -> b ! s == Black) squares
    white = length $ filter (\s -> b ! s == White) squares
