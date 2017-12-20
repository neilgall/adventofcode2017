{-# LANGUAGE BangPatterns #-}
module Day5 where
  
import Data.Array
  
-- Part 1

type Instructions = Array Int Int

makeInstructions :: [Int] -> Instructions
makeInstructions instr = listArray (0, length instr - 1) instr

delta1 :: Int -> Int
delta1 = (+1)

jumps :: [Int] -> (Int -> Int) -> Int
jumps instructions delta = jumps' (makeInstructions instructions) delta 0 1

jumps' :: Instructions -> (Int -> Int) -> Int -> Int -> Int
jumps' !instructions !delta !steps !n =
  let
    instruction = instructions ! (n-1)
    nextInstructions = instructions // [((n-1), delta instruction)]
    nextN = n + instruction
  in
    if n > length instructions then steps
    else jumps' nextInstructions delta (steps + 1) nextN
    
-- Part 2

delta2 :: Int -> Int
delta2 !n = if n >= 3 then n-1 else n+1
