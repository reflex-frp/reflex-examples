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
  => Dynamic t WidgetState -> Dynamic t GameState -> m (Event t Point)
mkBoard ws dGs =
  elAttr "table" ("style" =: "margin-left: auto; margin-right: auto") $ el "tbody" $ do
    rows <- mapM (row ws dGs) [7, 6..0]
    pure $ leftmost rows

row
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t WidgetState -> Dynamic t GameState -> Word ->  m (Event t Point)
row ws dGs j =
  el "tr" $ do
    cells <- mapM (cell ws dGs) [Point i j | i <- [0..7]]
    pure $ leftmost cells

cell
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t WidgetState -> Dynamic t GameState -> Point -> m (Event t Point)
cell ws dGs p = el "td" $ do
    (e, _) <- elDynAttr' "img" (imgTag <$> ws <*> dGs) $ pure ()
    pure $ p <$ domEvent Click e
    where
      coloredPiece gs = gs `indexGS` p
      imgTag (WidgetState active _) gs = "src" =: translate (coloredPiece gs)
        <> "style" =: ("display: block; width: 45px; height: 45px; background-color: " <> backgroundColor active)
        <> "draggable" =: "false"

      backgroundColor (Just p') | p == p' = "yellow"
      backgroundColor _                   = defaultColor p

      defaultColor (Point i j) | (i + j) `mod` 2 == 0 = "grey"
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

handleClick :: (WidgetState, GameState) -> Point -> (WidgetState, Maybe Move)
handleClick (WidgetState highlight promotion, board) new = (WidgetState newHighlight promotion, mMove) where
  newHighlight = do
    guard $ isNothing highlight
    guard $ isNothing mMove
    case board `indexGS` new of
      Just (ColoredPiece color _) | color == gameState_turn board -> pure new
      _                                                           -> Nothing
  mMove = do
    case highlight of
      Just old -> do
        moveFrom old
      Nothing -> do
        guard $ not $ config_selfCapture $ gameState_config board
        [mv] <- pure $ do
          old <- validSquares
          Just mv <- pure $ moveFrom old
          pure mv
        pure mv
  moveFrom old = do
    let mv = Move old new (gameState_turn board) promotion
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
    let initial = initialGS $ ChessConfig
            -- TODO: Expose the feature or get rid of it
            { config_selfCapture = False
            }
    rec
        localBoard <- holdDyn initial $ fmapMaybe id $ attachWith move (current localBoard) chessMove
        chessMove <- chessBoard localBoard
    pure ()

chessBoard
  :: forall t m.
     ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t GameState
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

scenarioText :: GameState -> String
scenarioText board = show turn <> checkText where
  turn = gameState_turn board
  checkText | inCheckmate board = " has been checkmated"
            | inStalemate board = " would be next, but it's a stalemate"
            | inCheck board turn     = "'s turn -- to get out of check"
            | otherwise              = "'s turn"
