{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (replicateM, replicateM_)
import Reflex.Dom
import Data.ByteString (ByteString)

main :: IO ()
main = mainWidgetWithCss css $ do
  el "h1" $ text "Tic Tac Toe"
  tictactoe

css :: ByteString
css = "table { border-collapse: collapse; }\
      \td + td, th + th { border-left: 1px solid; }\
      \tr + tr { border-top: 1px solid; }"

tictactoe :: MonadWidget t m => m ()
tictactoe = el "table" $ do
  replicateM_ 3 $
    el "tr" $ replicateM 3 $
      el "td" $ return ()

data Marker = Marker_X
            | Marker_O
            deriving (Show, Read, Eq, Ord)

-- inputWidget :: MonadWidget t m => m (Dynamic t (Maybe Marker))
-- inputWidget =
