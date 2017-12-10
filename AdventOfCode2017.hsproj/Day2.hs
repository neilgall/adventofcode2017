module Day2 where
 
-- Part 1

parseData :: String -> [[Int]]
parseData = map (map read . words) . lines

checksum :: ([Int] -> Int) -> [[Int]] -> Int
checksum rowChecksum = sum . map rowChecksum

rowChecksum1 :: [Int] -> Int
rowChecksum1 cells = maximum cells - minimum cells

-- Part 2

rowChecksum2 :: [Int] -> Int
rowChecksum2 cells = (fst divisibles) `div` (snd divisibles)
  where
    divisibles = findDivisibles cells
    
findDivisibles :: [Int] -> (Int, Int)
findDivisibles = head . filter divisible . pairs 
  where
    divisible (a, b) = (a `div` b) * b == a

pairs :: Ord a => [a] -> [(a, a)]
pairs [] = []
pairs (x:[]) = []
pairs (x:y:zs) = (ordered [x, y]):pairs (x:zs) ++ pairs (y:zs)
  where
    ordered pair = (maximum pair, minimum pair)