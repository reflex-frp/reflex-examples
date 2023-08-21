{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.Examples.DisplayGameUpdates.Main
  (app)
  where

import Reflex.Dom
import Data.Text as T
import Control.Monad.Fix (MonadFix)
import Control.Monad (void)

-- Can either display full info/updates or just the current score
data Display t =
    Display_FullInfo (Dynamic t [Text])
  | Display_OnlyScore (Dynamic t Int)

data GameEvent =
    GameEvent_NewGame
  | GameEvent_DoTurn Text Int -- Some info and points earned

details :: Text
details = "This example demonstrates use of nested Dynamic values. "
  <> "The Display data has Dynamic values in it, "
  <> "and therefore `Dynamic t (Display t)` will have nested Dynamic values. "
  <> "To create this value we use foldDynM. This essentially creates a nested state machine."

app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => m ()
app = do
  el "div" $ text details
  el "br" blank

  toggleDisplayEv <-el "div" $
    button "Toggle Display"

  gameEv <- el "div" $ do
    newGameEv <- button "Start New Game"
    m1 <- button "Do move 1"
    m2 <- button "Do move 2"
    m3 <- button "Do move 3"
    m4 <- button "Do move 4"
    let
      move1 = (GameEvent_DoTurn "1" 1) <$ m1
      move2 = (GameEvent_DoTurn "2" 20) <$ m2
      move3 = (GameEvent_DoTurn "3" 10) <$ m3
      move4 = (GameEvent_DoTurn "4" 5) <$ m4
      newGame = GameEvent_NewGame <$ newGameEv
    return $ leftmost [move1, move2, move3, move4, newGame]

  -- Capture the score, in a Dynamic independent of Display
  -- This will maintain its value irrespective of the state of Display
  scoreDyn <- do
    let changeScore (GameEvent_NewGame) _ = 0
        changeScore (GameEvent_DoTurn _ s) p = p + s

    foldDyn changeScore 0 gameEv

  -- external state machine using foldDynM
  -- Here the (Display t) itself contains a Dynamic value
  -- displayDyn :: Dynamic t (Display t)
  displayDyn <- do
    let
      initState = Display_OnlyScore scoreDyn
      doToggle _ (Display_FullInfo _) = return (Display_OnlyScore scoreDyn)
      doToggle _ (Display_OnlyScore _) = do
        -- Internal state machine using foldDyn
        -- Capture the info text in a Dynamic which is in the scope
        -- of Display.
        -- So this will be reset whenever you toggle the display of score card
        textDyn <- foldDyn showGameEvent [] gameEv
        return (Display_FullInfo textDyn)
    foldDynM doToggle initState toggleDisplayEv

  el "div" $
    dyn_ (render <$> displayDyn)

  return ()

showGameEvent :: GameEvent -> [Text] -> [Text]
showGameEvent ev tOld = case ev of
  (GameEvent_NewGame) -> ["Starting New Game!"]
  (GameEvent_DoTurn t p) -> tOld <>
    ["Player did move: " <> t <> ", and got " <> (T.pack $ show p) <> " points."]

render
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Display t -> m ()
render = \case
  (Display_FullInfo ts) -> do
    text "Update: "
    el "br" blank
    void $ simpleList ts $ \t -> do
      dynText t
      el "br" blank
  (Display_OnlyScore s) -> text "Score: " >> dynText (T.pack <$> show <$> s)
