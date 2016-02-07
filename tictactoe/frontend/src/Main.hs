{-# LANGUAGE ScopedTypeVariables, RecursiveDo #-}

import Control.Monad (replicateM, replicateM_, forM)
import Reflex.Dom

main :: IO ()
main = mainWidget $ do
  el "h1" $ text "Tic Tac Toe"
  tictactoe

type Board = [[Maybe Marker]]

tictactoe :: MonadWidget t m => m ()
tictactoe = do
  board <- tictactoeBoard
  dyn =<< mapDyn (\b -> displayBoard b) board
  return ()

displayBoard :: MonadWidget t m => Board -> m ()
displayBoard b = do
  el "pre" $ forM b $ \row -> el "div" $ forM row $ \cell -> text $ case cell of
                                                                       Nothing -> "-"
                                                                       Just m -> markerToString m
  return ()

tictactoeBoard :: MonadWidget t m => m (Dynamic t Board)
tictactoeBoard = el "table" $ do
  markers :: [Dynamic t [[Maybe Marker]]] <- replicateM 3 $ do
    row :: Dynamic t [Maybe Marker] <- el "tr" $ do
      markerRow :: [Dynamic t [Maybe Marker]] <- replicateM 3 $ do
        m <- elAttr "td" ("style" =: "border: 1px solid black;") inputWidget
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

inputWidget :: MonadWidget t m => m (Dynamic t (Maybe Marker))
inputWidget = do
  rec edit <- buttonDyn dynamicLabel
      marker <- holdDyn Nothing $ fmap (const $ Just Marker_X) edit
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


