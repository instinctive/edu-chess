module Chess
    ( Game, GState(..)
    , newGame, gameState, gameMoves, lastMove
    , putGame
    ) where

import Internal ( Game, GState, Draw, putGame, lastMove )
import Internal qualified as X

newGame = X.setStatus X.initGame
gameState = X._gameState
gameMoves = X._gameMoves
