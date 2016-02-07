{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecursiveDo #-}

import Common
import Control.Monad (replicateM, replicateM_, forM, msum)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Maybe (catMaybes, isJust)
import Reflex.Dom

main :: IO ()
main = mainWidgetWithCss css $ do
  el "h1" $ text "Tic Tac Toe"
  rec ws <- webSocket "ws://localhost:8000/api" $
              def & webSocketConfig_send .~ fmap ((:[]) . LBS.toStrict . encode) board
      board <- tictactoe (fmapMaybe decodeStrict $ _webSocket_recv ws)
  return ()

css :: ByteString
css = "table { border-collapse: collapse; }\
      \td + td, th + th { border-left: 1px solid; }\
      \tr + tr { border-top: 1px solid; }"

winner :: Board -> Maybe Marker
winner b = msum $ map rowWinner (transpose b) ++ map rowWinner b
  where
    rowWinner r = case nub r of
                       [Just x] -> Just x
                       _ -> Nothing

tictactoe :: MonadWidget t m => Event t Board -> m (Event t Board)
tictactoe receivedBoard = do
  rec board <- tictactoeBoard who'
      gameOver <- mapDyn (isJust . winner) board
      who <- mapDyn ((\x -> if x then Marker_X else Marker_O) . even . length . catMaybes . concat) board
      who' <- combineDyn (\go w -> if go then Nothing else Just w) gameOver who
  el "div" $ dynText =<< mapDyn (\w -> "Turn: " ++ markerToString w) who
  el "div" $ dynText =<< mapDyn (\b -> show $ fmap markerToString $ winner b) board
  dyn =<< mapDyn (\b -> displayBoard b) =<< holdDyn emptyBoard receivedBoard
  return $ updated board

emptyBoard :: Board
emptyBoard = replicate 3 $ replicate 3 Nothing

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


