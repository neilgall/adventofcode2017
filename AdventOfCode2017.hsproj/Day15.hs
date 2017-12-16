module Day15 where

import Data.Bits  
import Data.Int

-- Part 1

type GInt = Int64
data Generator = Generator GInt GInt deriving (Show)

seed :: Generator -> GInt
seed (Generator _ s) = s

next :: Generator -> Generator
next (Generator factor seed) = 
  Generator factor ((seed * factor) `mod` 2147483647)  

generate :: Generator -> [GInt]
generate g = (seed $! next g):(generate $! next g)

cmp16 :: (GInt, GInt) -> Bool
cmp16 (a, b) = (a .&. 0xFFFF) == (b .&. 0xFFFF)

judge :: [(GInt, GInt)] -> Int
judge = length . filter cmp16

pairs :: Int -> Generator -> Generator -> [(GInt, GInt)]
pairs count a b = take count $ zip (generate a) (generate b)


-- Part 2

pickyGenerate :: Generator -> GInt -> [GInt]
pickyGenerate g n = filter (\v -> v `mod` n == 0) (generate g)

pickyPairs :: Int -> Generator -> Generator -> [(GInt, GInt)]
pickyPairs count a b = take count $ zip (pickyGenerate a 4) (pickyGenerate b 8)
