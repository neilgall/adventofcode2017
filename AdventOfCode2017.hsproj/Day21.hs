{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Day21 where
  
import Data.Either (rights)
import Data.List (nub)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Matrix as Mx
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- Parser and Input

type Square a = Mx.Matrix a
type Mapping a = (Square a, Square a)
type Patterns a = M.Map (Square a) (Square a)

instance (Ord a) => Ord (Mx.Matrix a) where
  compare x y = compare (concat $ Mx.toLists x) (concat $ Mx.toLists y)

load :: String -> IO (Patterns Char)
load f = do
    content <- readFile f
    return . head . rights . pure $ parse parseInput f content

parseInput :: GenParser Char st (Patterns Char)
parseInput = M.fromList <$> parsePattern `sepEndBy` endOfLine

parsePattern :: GenParser Char st (Mapping Char)
parsePattern = (,) <$> parseSquare <*> (string " => " >> parseSquare)

parseSquare :: GenParser Char st (Square Char)
parseSquare = Mx.fromLists <$> many pixel `sepBy` (char '/')
  where
    pixel = (char '.') <|> (char '#')

asLists :: ([[a]] -> [[a]]) -> Square a -> Square a
asLists f = Mx.fromLists . f . Mx.toLists

flipH :: Square a -> Square a
flipH = asLists (map reverse)

flipV :: Square a -> Square a
flipV = asLists reverse

rotateL :: Square a -> Square a
rotateL = Mx.transpose . flipH

rotateR :: Square a -> Square a
rotateR = Mx.transpose . flipV

rotate180 :: Square a -> Square a
rotate180 = rotateR . rotateR

extendPatternKeys :: (Ord a, Eq a) => Patterns a -> Patterns a
extendPatternKeys = M.fromList . concat . map addKeys . M.toList
  where
    flips = [id, flipH, flipV]
    rotates = [id, rotateR, rotateL, rotate180]
    transforms = [f . r | f <- flips, r <- rotates]
    addKeys (x,y) = map (,y) . nub . sequence transforms $ x
    
-- Part 1 

type Image = Mx.Matrix Char

initial :: Image
initial = Mx.fromLists [".#.","..#","###"]

expand :: Patterns Char -> Image -> Image
expand patterns image =
  let
    blocks = toBlocks (blockSize image) image
    expBlock b = fromJust $ M.lookup b patterns
    newBlocks = map expBlock blocks
  in
    assemble (nBlockRows image) newBlocks

expandN :: Int -> Patterns Char -> Image -> Image
expandN 0 _ image = image
expandN n patterns image = expandN (n-1) patterns (expand patterns image)

blockSize :: Image -> Int
blockSize i = if (Mx.nrows i) `mod` 2 == 0 then 2 else 3

nBlockRows :: Image -> Int
nBlockRows i = Mx.nrows i `div` (blockSize i)

toBlocks :: Int -> Image -> [Image]
toBlocks n image =
  let
    size = Mx.nrows image
    starts = [1,1+n..size]
    block (r, c) = Mx.submatrix r (r+n-1) c (c+n-1) image 
  in
    map block [(r,c) | r <- starts, c <- starts]
    
assemble :: Int -> [Image] -> Image
assemble n blocks =
  let
    rows = chunksOf n blocks    
  in
    Mx.fromLists . concat $ map assembleRow rows
    
assembleRow :: [Image] -> [[Char]]
assembleRow = foldl1 join . map Mx.toLists
  where
    join a b = map (uncurry (++)) $ zip a b

countOn :: Image -> Int
countOn = length . filter (== '#') . concat . Mx.toLists
