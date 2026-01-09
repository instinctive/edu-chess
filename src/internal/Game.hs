{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Data.Array

import Color
import Sq
import Piece
import Board
import Zobrist

type UCI = String

data Side = Side
    { _sideKingSq  :: Sq
    , _sideKCastle :: Bool
    , _sideQCastle :: Bool
    , _sideMoves   :: [UCI]
    }
makeLenses ''Side

data Draw = Dead | Stalemate | Repetition | Move50
    deriving (Eq,Ord,Show)

data GState = Play | Win Color | Draw Draw
    deriving (Eq,Ord,Show)

data Game = Game
    { _gameState     :: GState
    , _gameTurn      :: Color
    , _gameBoard     :: Board
    , _gameSides     :: Array Color Side
    , _gameEnPassant :: Maybe Sq
    , _gameMoves     :: [Game]
    , _gameZobrist   :: Int
    , _gameHistory   :: [Int]
    }
makeLenses ''Game

instance Show Game where
    show g = show $ _gameState g

gamePieces = boardPieces . _gameBoard

gameSq :: Sq -> Lens' Game (Maybe Piece)
gameSq sq = gameBoard . boardSq sq

gameSide :: Color -> Lens' Game Side
gameSide c = \f g -> f (_gameSides g ! c) <&> \s -> over gameSides (//[(c,s)]) g

sideKingSq' :: Color -> Lens' Game Sq
sideKingSq' c = gameSide c . sideKingSq

sideKCastle' :: Color -> Lens' Game Bool
sideKCastle' c = gameSide c . sideKCastle

sideQCastle' :: Color -> Lens' Game Bool
sideQCastle' c = gameSide c . sideQCastle

sideMoves' :: Color -> Lens' Game [UCI]
sideMoves' c = gameSide c . sideMoves

lastMove g = head $ g ^. sideMoves' (oppColor . _gameTurn $ g)

initSide c = Side (kingSq c) True True []

initGame = Game Play White board sides Nothing [] zhash [zhash] where
    board = initBoard
    sides = listArray (Black,White) [initSide Black, initSide White]
    zhash = foldl' xor 0 $ zobrist <$> boardPieces board

-- ----------------------------------------------------------------------

ppGTurn c = show c <> " to move."

ppGState = \case
    Play -> "The game is in play."
    Win c  -> show c <> " has won."
    Draw Dead       -> "Draw by dead position."
    Draw Repetition -> "Draw by repetition."
    Draw Move50     -> "Draw by the 50-move rule."
    Draw Stalemate  -> "Draw by stalemate."

ppSide c Side{..} =
    show c <> " King is at " <> show _sideKingSq <>
    (_sideKCastle & bool "" ", can castle kingside") <>
    (_sideQCastle & bool "" ", can castle queenside") <>
    "."

ppGame g@Game{..} =
    zipWith (<>) (ppBoard _gameEnPassant _gameBoard) mm
    -- <> [ "History length: " <> show (length _gameHistory) ]
    -- <> map (uncurry ppSide) (assocs _gameSides)
    <> [ case _gameState of
        Play -> ppGTurn _gameTurn
        s -> ppGState s ]
  where
    wmoves = g^.sideMoves' White
    bmoves = g^.sideMoves' Black
    nwmoves = length wmoves
    nbmoves = length bmoves
    (nn,ww,bb) | nwmoves == nbmoves = ([nwmoves+1,nwmoves..],"....":wmoves, "    ":bmoves)
               | otherwise          = ([nwmoves,nwmoves-1..],       wmoves, "....":bmoves)
    mm = (reverse $ take 8 $ zipWith3 out nn ww bb) <> repeat ""
    out :: Int -> String -> String -> String
    out n w b = printf " %3d  %-5s %-5s" n w b

putGame = traverse_ putStrLn . ppGame
