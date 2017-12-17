module Day17b where

import qualified Data.Vector.Unboxed as V
import Data.List (intersperse, elemIndex)
import Data.Maybe (fromJust)

data Spinlock = Spinlock {
  size :: !Int,
  pos :: !Int,
  zeroPos :: !Int,
  afterZero :: !Int
} deriving (Show)

initial :: Spinlock
initial = Spinlock { 
  size = 1,
  pos = 0,
  zeroPos = 0,
  afterZero = 0
}

insert :: Int -> Spinlock -> Spinlock
insert n s@(Spinlock size pos zeroPos afterZero) = s { 
  size = size+1,
  pos = pos+1,
  zeroPos = if pos < zeroPos then zeroPos+1 else zeroPos,
  afterZero = if pos == zeroPos then n else afterZero
}
    
stepN :: Int -> Spinlock -> Spinlock
stepN n s = s { pos = (pos s + n) `mod` size s }

insertions :: Int -> Int -> Int -> Spinlock -> Spinlock
insertions n limit steps s 
  | n > limit = s
  | otherwise = insertions (n+1) limit steps $! insert n (stepN steps s)
  
