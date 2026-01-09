{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TemplateHaskell  #-}

module Board
    ( Board
    , boardPieces
    , boardSq
    , emptyBoard
    , initBoard
    , ppBoard
    ) where

import Control.Lens
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as M

import Color
import Sq
import Piece

newtype Board = Board { _unBoard :: Map Sq Piece }
makeLenses ''Board

boardPieces Board{..} = M.assocs _unBoard

emptyBoard = Board M.empty

boardSq :: Sq -> Lens' Board (Maybe Piece)
boardSq sq = unBoard . at sq

colorize White s = "\ESC[44m\ESC[30m" <> s <> "\ESC[0m"
colorize Black s = "\ESC[34m\ESC[40m" <> s <> "\ESC[0m"

ppBoard ep bd =
    [ concat
      [ bd ^. boardSq sq & maybe (empty sq) show & colorize c
      | sq <- sqs
      | c  <- colors
      ]
    | sqs <- allSquares
    | colors <- tails $ cycle [White,Black]
    ]
  where
    empty sq | Just sq == ep = ".."
             | otherwise     = "  "

pawns  c = zip (rowSquares $ pawnRow c)  $
    replicate 8 $ Piece c Pawn

pieces c = zip (rowSquares $ pieceRow c) $
    Piece c <$> [Rook,Knight,Bishop,Queen,King,Bishop,Knight,Rook]

initBoard = Board . M.fromList . concat $
    ($) <$> [pawns,pieces] <*> [Black,White]
