{-# LANGUAGE TemplateHaskell #-}

module Piece where

import Control.Lens

import Color

data PType = King | Queen | Rook | Bishop | Knight | Pawn
    deriving (Eq,Ord,Enum,Bounded,Ix,Show)

showPType :: PType -> String
showPType King   = "k"
showPType Queen  = "q"
showPType Rook   = "r"
showPType Bishop = "b"
showPType Knight = "n"
showPType Pawn   = "p"

readPType :: String -> PType
readPType "k" = King
readPType "q" = Queen
readPType "r" = Rook
readPType "b" = Bishop
readPType "n" = Knight
readPType "p" = Pawn

data Piece = Piece
    { _pieceColor :: Color
    , _pieceType  :: PType
    } deriving (Eq,Ord,Bounded,Ix)
makeLenses ''Piece

instance Show Piece where show = showPiece

showPiece Piece{..}
    | _pieceColor == White = map toUpper s
    | otherwise            = s
  where
    s = showColor _pieceColor <> showPType _pieceType

readPiece [c,t] = Piece (readColor [c]) (readPType [t])
