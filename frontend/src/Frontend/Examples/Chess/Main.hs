{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Frontend.Examples.Chess.Main (app) where

import Control.Monad
import Control.Monad.Fix
import Data.Bifunctor
import Data.Maybe
import qualified Data.Text as T

import Reflex.Dom
import Obelisk.Generated.Static

import Common.Examples.Chess

data WidgetState = WidgetState
  { widgetStateMoveStart :: Maybe Point
  , widgetStatePromotion :: Piece
  }

initialWidgetState :: WidgetState
initialWidgetState = WidgetState Nothing Queen

mkBoard
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t WidgetState -> Dynamic t (Maybe GameState) -> m (Event t Point)
mkBoard ws dmGs =
  elAttr "table" ("style" =: "margin-left: auto; margin-right: auto") $ el "tbody" $ do
    rows <- mapM (row ws dmGs) [7, 6..0]
    pure $ leftmost rows

row
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t WidgetState -> Dynamic t (Maybe GameState) -> Int ->  m (Event t Point)
row ws dmGs j =
  el "tr" $ do
    cells <- mapM (cell ws dmGs) [(i, j) | i <- [0..7]]
    pure $ leftmost cells

cell
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t WidgetState -> Dynamic t (Maybe GameState) -> Point -> m (Event t Point)
cell ws dmGs p = el "td" $ do
    (e, _) <- elDynAttr' "img" (imgTag <$> ws <*> dmGs) $ pure ()
    pure $ p <$ domEvent Click e
    where
      coloredPiece mgs = do
        gs <- mgs
        gs <!> p
      imgTag (WidgetState active _) mgs = "src" =: translate (coloredPiece mgs)
        <> "style" =: ("display: block; width: 45px; height: 45px; background-color: " <> backgroundColor active)
        <> "draggable" =: "false"
      backgroundColor (Just p') | p == p' = "yellow"
      backgroundColor _                   = defaultColor p
      defaultColor (i,j) | (i + j) `mod` 2 == 0 = "grey"
                         | otherwise            = "white"
      translate (Just (ColoredPiece clr pp)) = translatePiece clr pp
      translate _                 = $(static "chess/blank.svg")
      translatePiece White King   = $(static "chess/kl.svg")
      translatePiece White Queen  = $(static "chess/ql.svg")
      translatePiece White Rook   = $(static "chess/rl.svg")
      translatePiece White Bishop = $(static "chess/bl.svg")
      translatePiece White Knight = $(static "chess/nl.svg")
      translatePiece White Pawn   = $(static "chess/pl.svg")
      translatePiece Black King   = $(static "chess/kd.svg")
      translatePiece Black Queen  = $(static "chess/qd.svg")
      translatePiece Black Rook   = $(static "chess/rd.svg")
      translatePiece Black Bishop = $(static "chess/bd.svg")
      translatePiece Black Knight = $(static "chess/nd.svg")
      translatePiece Black Pawn   = $(static "chess/pd.svg")

handleClick :: (WidgetState, Maybe GameState) -> Point -> (WidgetState, Maybe Move)
handleClick (WidgetState highlight promotion, mBoard) new = (WidgetState newHighlight promotion, mMove) where
  newHighlight = do
    board <- mBoard
    guard $ isNothing highlight
    guard $ isNothing mMove
    case board <!> new of
      Just (ColoredPiece color _) | color == gameStateTurn board -> pure new
      _                                                          -> Nothing
  mMove = do
    case highlight of
      Just old -> do
        moveFrom old
      Nothing -> do
        board <- mBoard
        guard $ not $ configSelfCapture $ gameStateConfig board
        [mv] <- pure $ do
          old <- validSquares
          Just mv <- pure $ moveFrom old
          pure mv
        pure mv
  moveFrom old = do
    board <- mBoard
    let mv = Move old new (gameStateTurn board) promotion
    void $ move board mv
    pure mv


app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => m ()
app = localChessBoard

localChessBoard
  :: forall t m.
     ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => m ()
localChessBoard = do
    let initial = initialGS $ ChessConfig False -- no self-capture
        mkMove mBoard mv = do
          board <- mBoard
          move board mv
    rec
        localBoard <- holdDyn (Just initial) $ attachWith mkMove (current localBoard) chessMove
        chessMove <- chessBoard localBoard
    pure ()

chessBoard
  :: forall t m.
     ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t (Maybe GameState)
  -> m (Event t Move)
chessBoard mGameState = el "div" $ do
  elAttr "div" ("style" =: "text-align: center") $ do
    rec
      widgetState <- foldDyn ($) initialWidgetState $ leftmost $ [const <$> eNewWidgetState] <> promotionPiece
      click <- mkBoard widgetState mGameState
      let
        (eNewWidgetState, eMove) = second (fmapMaybe id) $ splitE $
          attachWith handleClick (current appState) click
        appState = (,) <$> widgetState <*> mGameState
      el "br" blank
      el "br" blank
      el "h1" $ do
        dynText $ T.pack . scenarioText <$> mGameState
      promotionPiece <- do
        elAttr "div" ("style" =: "text-align: left") $ do
            el "h2" $ text "Which piece to promote to, when a pawn promotes:"
            elAttr "ul" ("style" =: "list-style: none;") $ forM promotionPieces $ \piece -> do
                (e, _) <- el' "li" $ dynText $ T.pack . pieceText piece <$> widgetState
                pure $ (\state -> state { widgetStatePromotion = piece }) <$ domEvent Click e
    pure eMove

pieceText :: Piece -> WidgetState -> String
pieceText piece (WidgetState _ piece') | piece == piece' = "*" <> show piece
                                       | otherwise       = show piece

scenarioText :: Maybe GameState -> String
scenarioText Nothing = "Game is loading..."
scenarioText (Just board) = show turn <> checkText where
  turn = gameStateTurn board
  checkText | inCheckmate board = " has been checkmated"
            | inStalemate board = " would be next, but it's a stalemate"
            | inCheck board turn     = "'s turn -- to get out of check"
            | otherwise              = "'s turn"
