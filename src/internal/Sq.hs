module Sq
    ( Sq, Row
    , sqRow, pawnRow, pieceRow
    , epTarget
    , oob
    , toColor, toKing, toQueen
    , bdeltas, rdeltas, kdeltas, ndeltas, pdeltas
    , kingSq, kRookSq, qRookSq
    , rowSquares
    , allSquares
    ) where

import Color
import Linear.V2

newtype Row = Row Int deriving (Eq,Ord,Num)
newtype Sq = Sq (V2 Int) deriving (Eq,Ord,Num,Ix)

instance Show Sq where show = showSq

instance Read Sq where
  readsPrec _ (c:d:more) | isFile c && isRank d = [(readSq [c,d], more)]
  readsPrec _ _ = []

instance IsString Sq where fromString = readSq

isFile c = elem c ("abcdefgh" :: String)
isRank d = elem d ("12345678" :: String)

showSq :: Sq -> String
showSq (Sq (V2 r c)) = [toFile c, toRank r] where
    toFile c = chr $ c + ord 'a'
    toRank r = intToDigit $ r + 1

readSq :: String -> Sq
readSq [file,rank] = Sq (V2 r c) where
    r = digitToInt rank - 1
    c = ord file - ord 'a'

sqRow (Sq (V2 r c)) = Row r

pawnRow Black = Row 6
pawnRow White = Row 1
pieceRow Black = Row 7
pieceRow White = Row 0

epTarget (Sq (V2 r c)) (Sq (V2 r' c')) = Sq (V2 r c')

oob :: Sq -> Bool
oob (Sq (V2 r c)) = r < 0 || r > 7 || c < 0 || c > 7

toBlack = Sq $ V2 1 0
toWhite = Sq $ V2 n 0 where n = -1
toKing  = Sq $ V2 0 1
toQueen = Sq $ V2 0 n where n = -1

toColor Black = toBlack
toColor White = toWhite

bdeltas = (+) <$> [toKing,toQueen] <*> [toBlack,toWhite]
rdeltas = [toKing,toQueen,toBlack,toWhite]
kdeltas = map Sq $ delete 0 $ V2 <$> [-1..1] <*> [-1..1]
pdeltas Black = (+toWhite) <$> [toKing,toQueen]
pdeltas White = (+toBlack) <$> [toKing,toQueen]
ndeltas =
    let dd = [-2,-1,1,2] in
    [ Sq $ V2 r c | r <- dd, c <- dd, abs r /= abs c ]

kingSq  Black = readSq "e8"
kingSq  White = readSq "e1"
kRookSq Black = readSq "h8"
kRookSq White = readSq "h1"
qRookSq Black = readSq "a8"
qRookSq White = readSq "a1"

rowSquares (Row r) = Sq . V2 r <$> [0..7]

allSquares = [ [ Sq $ V2 r c | c <- [0..7] ] | r <- [7,6..0] ]
