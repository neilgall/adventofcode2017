{-# LANGUAGE BangPatterns #-}
module Day6 where

import Prelude hiding (foldl1, length)
import Data.Vector.Unboxed hiding (map)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

readInts :: String -> [Int]
readInts = map read . words

type Banks = Vector Int

makeBanks :: [Int] -> Banks
makeBanks = fromList

fullestBank :: Banks -> Int
fullestBank banks = fst $ foldl1 maxv (indexed banks) 
  where
    maxv (j,b) (i,a) = if a > b then (i,a) else (j,b)
    
distribute :: Banks -> Int -> Int -> Banks
distribute banks 0 _ = banks
distribute banks n i = distribute (addone banks i) (n - 1) (nextBankIndex banks i)
  where
    addone banks i = banks // [(i, 1 + banks ! i)]
   
nextBankIndex :: Banks -> Int -> Int
nextBankIndex banks i = (i + 1) `mod` (length banks)

reallocate :: Banks -> Banks
reallocate banks = 
  let
    fullest = fullestBank banks
    count = banks ! fullest
    start = nextBankIndex banks fullest
  in
    distribute (banks // [(fullest, 0)]) count start

cycleReallocate :: Banks -> Int
cycleReallocate banks = cycleReallocate' banks Set.empty 0

cycleReallocate' :: Banks -> Set.Set Banks -> Int -> Int
cycleReallocate' !banks !history !steps
    | banks `Set.member` history = steps
    | otherwise = cycleReallocate' (reallocate banks) (Set.insert banks history) (steps + 1)

-- Part 2

countReallocate :: Banks -> Int
countReallocate banks = countReallocate' banks Map.empty 0

countReallocate' :: Banks -> Map.Map Banks Int -> Int -> Int
countReallocate' !banks !history !steps =
  case Map.lookup banks history of
    Just n -> steps - n
    Nothing -> countReallocate' (reallocate banks) (Map.insert banks steps history) (steps + 1)
    