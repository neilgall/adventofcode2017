module Da where

import Data.List (intersperse, elemIndex)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

-- Part 1
  
type Spinlock = ([Int], [Int])

initial :: Spinlock
initial = ([0], [])

insert :: Int -> Spinlock -> Spinlock
insert n (before, after) = (n:before, after) 

step :: Spinlock -> Spinlock
step (bs, []) = ([head (reverse bs)], tail (reverse bs))
step (bs, a:as) = (a:bs, as)

stepN :: Int -> Spinlock -> Spinlock
stepN 0 s = s
stepN n s = stepN (n-1) (step s)

insertions :: Int -> Int -> Int -> Spinlock -> Spinlock
insertions n limit steps s 
  | n > limit = s
  | otherwise = insertions (n+1) limit steps $ insert n (stepN steps s)
  
toList :: Spinlock -> String
toList (before, after) = 
  let
    bs = map show $ reverse $ tail before
    pos = "**" ++ show (head before) ++ "**"
    as = map show after
    all = bs ++ pos:as
  in
    "[" ++ concat (intersperse "," all) ++ "]" 

-- Part 2

findAfter :: Spinlock -> Int -> Int
findAfter (before, after) n =
  case elemIndex n before of
    Just i -> if i == 0 then head after else before !! (i - 1)    
    Nothing -> after !! (fromJust (elemIndex n after) + 1)
