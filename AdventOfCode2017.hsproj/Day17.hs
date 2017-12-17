module Day17 where

import qualified Data.Vector.Unboxed as V
import Data.List (intersperse, elemIndex)
import Data.Maybe (fromJust)

-- Part 1
  
type Spinlock = (V.Vector Int, Int)

initial :: Spinlock
initial = (V.fromList [0], 0)

insert :: Int -> Spinlock -> Spinlock
insert n (xs, p) = (xs', p')
  where
    (before, after) = V.splitAt p xs
    xs' = V.concat [before, V.singleton n, after]
    p' = p+1

stepN :: Int -> Spinlock -> Spinlock
stepN n (xs, p) = (xs, (p + n) `mod` V.length xs)

insertions :: Int -> Int -> Int -> Spinlock -> Spinlock
insertions n limit steps s 
  | n > limit = s
  | otherwise = insertions (n+1) limit steps $! insert n (stepN steps s)
  
toList :: Spinlock -> String
toList (xs, p) = show before ++ "**" ++ show after
  where
    (before, after) = V.splitAt p xs

-- Part 2

findAfter :: Spinlock -> Int -> Maybe Int
findAfter (xs, _) n = fmap getNext index
  where
    index = V.elemIndex n xs
    nextIndex n = (n + 1) `mod` V.length xs
    getNext n = xs V.! (nextIndex n)
