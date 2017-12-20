module Day11 where
  
import Data.Char
import Data.List (inits, foldl')
import Data.List.Split (splitOn)

data Dir = N | NE | SE | S | SW | NW deriving (Read, Show)

readDir = read . map toUpper

parseData :: String -> [Dir]
parseData = map readDir . splitOn ","

-- Part 1

type Pos = (Int,Int)

step :: Pos -> Dir -> Pos
step (n, ne) N  = (n+1, ne)
step (n, ne) S  = (n-1, ne)
step (n, ne) NE = (n,   ne+1)
step (n, ne) SE = (n-1, ne+1)
step (n, ne) SW = (n,   ne-1)
step (n, ne) NW = (n+1, ne-1)

follow :: Pos -> [Dir] -> Pos
follow = foldl' step

distance :: Pos -> Int
distance (n, ne)
  | n >= 0 && ne >= 0 = n + ne
  | n <= 0 && ne <= 0 = (-n - ne)
  | n >= 0 && ne <= 0 = n
  | n <= 0 && ne >= 0 = ne

away :: [Dir] -> Int
away = distance . follow (0,0)

-- Part 2

furthest :: [Dir] -> Int
furthest = maximum . map away . inits