module Players where

import Control.Monad.Random.Class ( getRandomR )

import Chess

main = do
    players <- getArgs <&> map player
    case players of
        [ Left "playtowin" ] -> playToWin
        [ Right w, Right b ] -> play w b >>= putGame
        _ -> putStrLn "Usage: chess {[human|random] [human|random] | playtowin}"

player "human" = Right human
player "random" = Right randomMan
player s = Left s

playToWin = go 1 where
    go n = do
        g <- play randomMan randomMan
        case gameState g of
          Win c -> do
            putStrLn $ "Game #" <> show n <> ":"
            putGame g
          _ -> go (n+1)

play w b = go newGame w b where
    go g curr next = curr g >>= \g' -> case gameState g' of
        Play -> go g' next curr
        s -> pure g'

pickOne l = getRandomR (0, length l - 1) <&> (l!!)

randomMan game = case gameState game of
    Play -> pickOne (gameMoves game)
    s -> pure game

human game = case gameState game of
    Play -> putGame game >> loop
    s -> pure game
  where
    loop = do
        move <- getLine
        find ((==move).lastMove) (gameMoves game)
            & maybe (invalid move) pure
    invalid move = do
        putStrLn $ "Invalid move: " <> show move
        loop
