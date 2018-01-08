{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           AI
import           Game
import           Types

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.Array as A
import           Data.Map      (Map, fromList)
import           Data.Monoid ((<>))
import qualified Data.Text as T

import           Reflex
-- import           Language.Javascript.JSaddle
import           Reflex.Dom                  hiding (mainWidget)
import           Reflex.Dom.Core             (mainWidget)

-------------------------------------------------------------------------------
-- View
-------------------------------------------------------------------------------
main :: IO ()
main = run $ mainWidget app

app :: forall t m. MonadWidget t m => m ()
app = do
  elAttr "div" ("style" =: s) $ text "Othello"
  setup
  where
  s = "font-size: 50px; margin-left: 155px; font-family: Helvetica; color: steelblue"

setup :: (MonadWidget t m) => m ()
setup = el "div" $ do
  rec rows <- mapM (row g) [1..8]
      let bm = leftmost (concat rows)
      wm <- performEventAsync $ fmap computeAiMove
                              . ffilter ((==White) . player)
                              $ attachWith (flip mkMove) (current g) bm
      dynText $ fmap ((<>"'s move") . T.pack . show . player) g
      g <- foldDyn mkMove newGame (leftmost [wm, bm])
  return ()

-- | A button whose color is based on the game state.
buttonDynAttr :: MonadWidget t m => Dynamic t (Map T.Text T.Text) -> m (Event t ())
buttonDynAttr attrs = do
  (e, _) <- elDynAttr' "button" attrs (text "")
  return $ domEvent Click e

-- | A button widget representing a square on the othello board.
--   The color of the square depends on the game state. Reports back
--   the Position of the square when the black player clicks on it.
squareWidget :: MonadWidget t m => Dynamic t Game -> Position ->  m (Event t Input)
squareWidget gameDyn coords = do
  let attrs = fmap (\r -> case board r A.! coords of
        Empty -> mkStyle "green"
        Black -> mkStyle "black"
        White -> mkStyle "white") gameDyn
  b <- buttonDynAttr attrs
  return $ fmap (const (BlackMove coords)) b
  where
    mkStyle c = fromList
      [ ("style", "outline: none; background-color: " <>
        c <> "; font-size: 40px; height: 60px; width: 60px") ]

-- | Get the nth row of squares from the list of squares.
row :: MonadWidget t m => Dynamic t Game -> Int -> m [Event t Input]
row g n = el "div" $
  mapM (squareWidget g) (take 8 . drop (8 * (n-1)) $ squares)

-- | Updates the Game based on the move made by the player (black) or the ai
--   white.
mkMove :: Input -> Game -> Game
mkMove (BlackMove x) g@(Game Black _) = move x g
mkMove (WhiteMove g) _ = g -- We trust that the AI's moves will always be legal
mkMove _ g = g

computeAiMove :: MonadIO m => Game -> (Input -> IO ()) -> m ()
computeAiMove g cb = liftIO $ do
  _ <- forkIO $
    -- threadDelay 1000000 -- Add a delay here if necessary
    cb $!! WhiteMove $ aiMove 2 g -- Fully evaluate before triggering the callback
  return ()
