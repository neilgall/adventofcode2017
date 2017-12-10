module Day1 where

import Data.Char (digitToInt)
import Data.List (group, splitAt, unzip)

-- Part 1

digits :: String -> [Int]
digits = map digitToInt

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

endsSame :: Eq a => [a] -> Bool
endsSame xs 
  | length xs < 2 = False
  | otherwise = (xs !! 0) == (xs !! (length xs - 1))

rotateUntilEndsDiffer :: Eq a => [a] -> [a]
rotateUntilEndsDiffer xs
  | endsSame xs = rotateUntilEndsDiffer $ rotate xs
  | otherwise = xs

groupValue :: [Int] -> Int
groupValue xs = (length xs - 1) * (xs !! 0)

captcha :: String -> Int
captcha = sum . map groupValue . group . rotateUntilEndsDiffer . digits

-- Part 2

splitList :: [a] -> ([a], [a])
splitList xs = splitAt (length xs `div` 2) xs

pairUpList :: [a] -> [(a, a)]
pairUpList = uncurry zip . splitList

pairValue :: (Int, Int) -> Int
pairValue (x, y) = if x == y then x+y else 0

captcha2 = sum . map pairValue . pairUpList . digits