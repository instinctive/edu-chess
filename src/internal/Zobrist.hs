{-# LANGUAGE OverloadedStrings #-}

module Zobrist where

import Control.Monad.Random.Class ( getRandoms )
import Data.Array.Unboxed

import Sq
import Piece

{-# NOINLINE zobristHashes #-}
zobristHashes :: UArray (Sq,Piece) Int
zobristHashes = unsafePerformIO $ do
    hashes <- getRandoms
    let ary = listArray (lo,hi) hashes :: UArray (Sq,Piece) Int
    pure ary
  where
    lo = ("a1",minBound)
    hi = ("h8",maxBound)

zobrist = (zobristHashes!)
