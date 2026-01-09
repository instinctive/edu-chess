{-# LANGUAGE ImplicitParams #-}

module Moves where

import Control.Lens
import Data.Map.Strict qualified as M

import Color
import Sq
import Piece
import Game
import Zobrist

type GAME = (?game :: Game)

me,opp :: GAME => Color
me = _gameTurn ?game
opp = oppColor me

atSq sq = ?game ^. gameSq sq

zob ap = over gameZobrist (xor (zobrist ap))

clearHistory = set gameHistory []

opMove a b
    = set (gameSq a) Nothing
    . set (gameSq b) attacker
    . over (sideMoves' me) (show a <> show b :)
    . zob (a,p) . zob (b,p)
    . maybe id (\target -> zob (b,target)) defender
    . bool id clearHistory (p ^. pieceType == Pawn || isJust defender)
  where
    attacker@(Just p) = atSq a
    defender = atSq b

opRemove a
    = set (gameSq a) Nothing
    . zob (a,p)
    . clearHistory
  where
    Just p = atSq a

opPromote a t
    = set (gameSq a . _Just . pieceType) t
    . over (sideMoves' me . _head) (<> showPType t)
    . zob (a, Piece me Pawn) . zob (a, Piece me t)

targetType = second (fmap _pieceType)

slide sq delta =
    targetType <$> targets
  where
    targets
        | null more || fmap _pieceColor tgt == Just me = clear
        | otherwise = clear <> [q]
    sqs = takeWhile (not.oob) . tail $ iterate (+delta) sq
    cands = ap (,) atSq <$> sqs
    (clear,more@(~(q@(_,tgt):_))) = span (isNothing.snd) cands

slides sq deltas =
    concatMap (slide sq) deltas

jumps sq deltas =
    targetType <$> filter check cands
  where
    sqs = filter (not.oob) $ (+sq) <$> deltas
    cands = ap (,) atSq <$> sqs
    check (_, fmap _pieceColor -> pcolor) = pcolor /= Just me

bmoves sq = slides sq bdeltas
rmoves sq = slides sq rdeltas
nmoves sq = jumps sq ndeltas
kmoves sq = jumps sq kdeltas
qmoves sq = bmoves sq <> rmoves sq

ptakes sq =
    filter (isJust.snd) $
    jumps sq (pdeltas me)

padvance sq =
    filter (isNothing.snd) $ take n $ slide sq (toColor opp)
  where
    n = if sqRow sq == pawnRow me then 2 else 1

pnormal sq = ptakes sq <> padvance sq

pieceMoves :: GAME => PType -> Sq -> [(Sq, Maybe PType)]
pieceMoves = \case
    Pawn   -> pnormal
    Knight -> nmoves
    Bishop -> bmoves
    Rook   -> rmoves
    Queen  -> qmoves
    King   -> kmoves

isAttacked sq =
    (bmoves sq & check [Queen,Bishop]) ||
    (rmoves sq & check [Queen,Rook])   ||
    (nmoves sq & check [Knight])       ||
    (kmoves sq & check [King])         ||
    (ptakes sq & check [Pawn])
  where
    check pp = any (`elem` pp) . catMaybes . map snd

kingInCheck game =
    let ?game = game in
    isAttacked (game ^. sideKingSq' me)

setStatus game@Game{..} = game
    & set gameMoves moves
    . set gameState if
        | dead             -> Draw Dead
        | move50           -> Draw Move50
        | repetition       -> Draw Repetition
        | not (null moves) -> Play
        | kingInCheck game -> Win $ oppColor _gameTurn
        | otherwise        -> Draw Stalemate
  where
    dead = null tt || tt == [Bishop] || tt == [Knight] where
        tt = gamePieces game & map (_pieceType.snd) & filter (/=King)
    move50 = length _gameHistory >= 50
    repetition = length (filter (==_gameZobrist) _gameHistory) >= 3
    moves = let ?game = game in
        (concatMap getPieceMoves $ gamePieces game)
        <> kCastleMove <> qCastleMove

-- allMoves game@Game{..}
--     | dead             = Left Dead
--     | move50           = Left Move50
--     | repetition       = Left Repetition
--     | not (null moves) = Right moves
--     | kingInCheck game = Left (Win $ oppColor $ _gameTurn game)
--     | otherwise        = Left Stalemate
--   where
--     dead = null tt || tt == [Bishop] || tt == [Knight] where
--         tt = gamePieces game & map (_pieceType.snd) & filter (/=King)
--     move50 = length _gameHistory >= 50
--     repetition = length (filter (==_gameZobrist) _gameHistory) >= 3
--     moves = let ?game = game in
--         (concatMap getPieceMoves $ gamePieces game)
--         <> kCastleMove <> qCastleMove

setNextTurn
    = setStatus
    . over gameTurn oppColor
    . \g -> g & over gameHistory (_gameZobrist g :)

kCastleCheck c = bool id (set (sideKCastle' c) False)
qCastleCheck c = bool id (set (sideQCastle' c) False)

getPieceMoves :: GAME => (Sq,Piece) -> [Game]
getPieceMoves (sqFrom, p) | _pieceColor p /= me = []
getPieceMoves (sqFrom, p) =
    [ game & setNextTurn
    | (sqTo, _) <- pieceMoves (_pieceType p) sqFrom
    , let base = ?game & doMove sqTo
    , game <- checkProm base sqTo p
    , not $ kingInCheck game
    ]
    <> epMoves sqFrom p
  where
    doMove sqTo
        = opMove sqFrom sqTo
        . kCastleCheck  me (sqFrom == kRookSq me || sqFrom == kingSq me)
        . qCastleCheck  me (sqFrom == qRookSq me || sqFrom == kingSq me)
        . kCastleCheck opp (sqTo == kRookSq opp)
        . qCastleCheck opp (sqTo == qRookSq opp)
        . bool id (set (sideKingSq' me) sqTo) ( _pieceType p == King )
        . set gameEnPassant (checkEnPassant sqTo)
    checkProm base sqTo p
        | _pieceType p /= Pawn = [base]
        | sqRow sqTo == pieceRow opp = promote <$> [Queen .. Knight]
        | otherwise = [base]
      where
        promote t = base & opPromote sqTo t
    checkEnPassant sqTo
        | _pieceType p /= Pawn                       = Nothing
        | sqTo /= sqFrom + toColor opp + toColor opp = Nothing
        | otherwise = Just $ sqFrom + toColor opp

epMoves sqFrom p | _pieceType p /= Pawn = []
epMoves sqFrom p = case ?game ^. gameEnPassant of
    Nothing -> []
    Just sqEp
        | all (/=sqEp) takes -> []
        | kingInCheck game   -> []
        | otherwise -> [ game & setNextTurn ]
      where
        takes = (+sqFrom) <$> pdeltas me
        game = ?game
            & opMove sqFrom sqEp
            . opRemove (epTarget sqFrom sqEp)
            . set gameEnPassant Nothing

kCastleMove :: GAME => [Game]
kCastleMove
    | not (?game ^. sideKCastle' me) = []
    | any (isJust.atSq) ee           = []
    | any isAttacked aa              = []
    | otherwise = [ ?game & castleMove k k' r r' ]
  where
    [k,r',k',r] = take 4 $ iterate (+toKing) $ ?game ^. sideKingSq' me
    aa = [k,r',k']
    ee = [r',k']

qCastleMove :: GAME => [Game]
qCastleMove
    | not (?game ^. sideQCastle' me) = []
    | any (isJust.atSq) ee           = []
    | any isAttacked aa              = []
    | otherwise = [ ?game & castleMove k k' r r' ]
  where
    [k,r',k',e,r] = take 5 $ iterate (+toQueen) $ ?game ^. sideKingSq' me
    aa = [k,r',k']
    ee = [r',k',e]

castleMove k k' r r'
    = setNextTurn
    . opMove k k'
    . opMove r r'
    . over (sideMoves' me) tail
    . set (sideKCastle' me) False
    . set (sideQCastle' me) False
