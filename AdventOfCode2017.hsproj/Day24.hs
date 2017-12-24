module Day24 where

import Data.Foldable (maximumBy)
import Data.List.Split (splitOn)

type Component = (Int, Int)
type Bridge = [Component]

parse :: String -> [Component]
parse = map (pair . map read . splitOn "/") . lines
  where
    pair (a:b:_) = (a,b)
    
connect :: Int -> [Component] -> [Component]
connect n = filter (\(x, y) -> x == n || y == n)

remove :: Component -> [Component] -> [Component]
remove r (c:cs) = if c == r then cs else c:(remove r cs)

unused :: Int -> Component -> Int
unused n (x,y) = if n == x then y else x

bridge :: Int -> [Component] -> [Bridge]
bridge start cs = do
  next <- connect start cs
  rest <- [] : bridge (unused start next) (remove next cs)
  return $ next:rest

feature :: (Ord a) => (Bridge -> a) -> Bridge -> Bridge -> Ordering
feature f b1 b2 = compare (f b1) (f b2)

strength :: Bridge -> Int
strength = sum . map (uncurry (+))
