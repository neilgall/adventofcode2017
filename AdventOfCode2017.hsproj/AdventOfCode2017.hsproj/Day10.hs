module Day10 where

import Data.Bits (xor)
import Data.List.Split (chunksOf)
import Text.Printf

-- Part 1
  
twist :: [a] -> Int -> Int -> [a]
twist xs start len = uncycle (begin ++ flipped ++ end)
  where
    n = length xs
    over = (start + len) - n
    ring = cycle xs
    begin = take start ring
    flipped = reverse . take len . drop start $ ring
    end = drop (start + len) ring
    uncycle c = take n (take over (drop n c) ++ drop over c)
    
twists :: [a] -> [Int] -> [a]
twists xs lens = twists' xs lens 0 0
  
twists' :: [a] -> [Int] -> Int -> Int -> [a]
twists' xs [] _ _ = xs
twists' xs (len:lens) pos skip = 
  twists' (twist xs pos len) lens (wrap $ pos + len + skip) (wrap $ skip + 1)
  where
    wrap n = n `mod` (length xs)

headProduct :: [Int] -> Int
headProduct (x:y:_ ) = x * y

-- Part 2

salt :: Enum a => [a] -> [Int]
salt xs = (map fromEnum xs) ++ [17, 31, 73, 47, 23]

repeat64 :: [a] -> [a]
repeat64 xs = concat $ replicate 64 xs

sparseHash :: Enum a => [a] -> [Int]
sparseHash = twists [0..255] . repeat64 . salt

denseHash :: [Int] -> [Int]
denseHash = map (foldl1 xor) . chunksOf 16

hexString :: [Int] -> String
hexString xs = concat $ map (printf "%02x") xs

hash :: Enum a => [a] -> String
hash = hexString . denseHash . sparseHash
