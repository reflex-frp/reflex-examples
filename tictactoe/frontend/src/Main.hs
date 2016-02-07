import Control.Monad (replicateM, replicateM_)
import Reflex.Dom

main :: IO ()
main = mainWidget $ do
  el "h1" $ text "Tic Tac Toe"
  tictactoe

tictactoe :: MonadWidget t m => m ()
tictactoe = el "table" $ do
  replicateM_ 3 $
    el "tr" $ replicateM 3 $
      elAttr "td" ("style" =: "border: 1px solid black;") $ return ()

data Marker = Marker_X
            | Marker_O
            deriving (Show, Read, Eq, Ord)

-- inputWidget :: MonadWidget t m => m (Dynamic t (Maybe Marker))
-- inputWidget =
