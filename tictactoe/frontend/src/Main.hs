{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecursiveDo #-}

import Control.Monad (replicateM, replicateM_, forM, msum)
import Reflex.Dom
import Data.Maybe (catMaybes, isJust)
import Data.ByteString (ByteString)
import Data.List

main :: IO ()
main = mainWidgetWithCss css $ do
  el "h1" $ text "Tic Tac Toe"
  tictactoe

css :: ByteString
css = "table { border-collapse: collapse; }\
      \td + td, th + th { border-left: 1px solid; }\
      \tr + tr { border-top: 1px solid; }"

type Board = [[Maybe Marker]]

winner :: Board -> Maybe Marker
winner b = msum $ map rowWinner (transpose b) ++ map rowWinner b
  where
    rowWinner r = case nub r of
                       [Just x] -> Just x
                       _ -> Nothing

tictactoe :: MonadWidget t m => m ()
tictactoe = do
  rec board <- tictactoeBoard who'
      gameOver <- mapDyn (isJust . winner) board
      who <- mapDyn ((\x -> if x then Marker_X else Marker_O) . even . length . catMaybes . concat) board
      who' <- combineDyn (\go w -> if go then Nothing else Just w) gameOver who
  el "div" $ dynText =<< mapDyn (\w -> "Turn: " ++ markerToString w) who
  el "div" $ dynText =<< mapDyn (\b -> show $ fmap markerToString $ winner b) board
  dyn =<< mapDyn (\b -> displayBoard b) board
  return ()

displayBoard :: MonadWidget t m => Board -> m ()
displayBoard b = do
  el "pre" $ forM b $ \row -> el "div" $ forM row $ \cell -> text $ case cell of
                                                                         Nothing -> "-"
                                                                         Just m -> markerToString m
  return ()

tictactoeBoard :: MonadWidget t m => Dynamic t (Maybe Marker) -> m (Dynamic t Board)
tictactoeBoard who = el "table" $ do
  markers :: [Dynamic t [[Maybe Marker]]] <- replicateM 3 $ do
    row :: Dynamic t [Maybe Marker] <- el "tr" $ do
      markerRow :: [Dynamic t [Maybe Marker]] <- replicateM 3 $ do
        m <- el "td" $ inputWidget who
        singleM :: Dynamic t [(Maybe Marker)] <- mapDyn (:[]) m
        return singleM
      mconcatDyn markerRow
    return row
    mapDyn (:[]) row
  markersGrid :: Dynamic t [[Maybe Marker]] <- mconcatDyn markers
  return markersGrid

data Marker = Marker_X
            | Marker_O
            deriving (Show, Read, Eq, Ord)

inputWidget :: forall t m. MonadWidget t m => Dynamic t (Maybe Marker) -> m (Dynamic t (Maybe Marker))
inputWidget who = do
  rec edit <- buttonDyn dynamicLabel
      let makeMark :: Event t Marker = fmapMaybe id $ tag (current who) edit
      marker <- holdDyn Nothing $ fmap Just makeMark
      dynamicLabel <- mapDyn (maybe " " markerToString) marker
  return marker

markerToString :: Marker -> String
markerToString m = case m of
                        Marker_X -> "X"
                        Marker_O -> "O"
buttonDyn :: MonadWidget t m => Dynamic t String -> m (Event t ())
buttonDyn ds = do
  (b, _) <- el' "button" $ dynText ds
  return $ domEvent Click b


