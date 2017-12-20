{-# LANGUAGE BangPatterns #-}
module Day14 where
  
import qualified Data.Array as A
import qualified Data.Set as S
import Day10 (hash)

-- Part 1

hashRow :: String -> Int -> String
hashRow key row = hash $ key ++ "-" ++ (show row) 
    
hashGrid :: String -> [String]
hashGrid key = map (hashRow key) [0..127]

countUsed :: String -> Int
countUsed [] = 0
countUsed !(c:cs) = countBits c + countUsed cs
  where
    countBits c
      | c == '0'          = 0
      | c `elem` "1248"   = 1
      | c `elem` "3569ac" = 2
      | c `elem` "7bde"   = 3
      | c == 'f'          = 4
    
countGrid :: String -> Int
countGrid key = sum $ map (countUsed . hashRow key) [0..127]

-- Part 2

data Cell = Empty | Used | Region Int deriving (Show, Eq, Ord)
type Pos = (Int, Int)
type Disk a = A.Array Int (A.Array Int a)

at :: Disk a -> Pos -> a
disk `at` (x, y) = (disk A.! y) A.! x

(//) :: Disk a -> (Pos, a) -> Disk a
disk // ((x, y), a) = disk A.// [(y, (disk A.! y) A.// [(x, a)])]

yIndices :: Disk a -> [Int]
yIndices disk = A.indices disk

xIndices :: Disk a -> [Int]
xIndices disk = A.indices (disk A.! 0)

isValidPos :: Disk a -> Pos -> Bool
isValidPos disk (x, y) = x `elem` (xIndices disk) && y `elem` (yIndices disk)

cells :: Disk a -> [a]
cells !disk = concat $ map A.elems (A.elems disk)

mkDisk :: [[Char]] -> Disk Cell
mkDisk rows = 
  let
    fillCell c = [bit3 c, bit2 c, bit1 c, bit0 c]
    fillRow cells = A.listArray (0, 127) $ concat $ map fillCell cells
    bit0 c = if c `elem` "13579bdf" then Used else Empty
    bit1 c = if c `elem` "2367abef" then Used else Empty
    bit2 c = if c `elem` "4567cdef" then Used else Empty
    bit3 c = if c `elem` "89abcdef" then Used else Empty
  in
    A.listArray (0, 127) $ map fillRow rows

allocateGroups :: Disk Cell -> Disk Cell
allocateGroups disk = allocateGroups' 1 disk
  where
    allocateGroups' index disk = 
      if hasUnallocatedGroups disk
      then allocateGroups' (index+1) (allocateGroup index disk)
      else disk

countGroups :: Disk Cell -> Int
countGroups disk = S.size (S.filter isRegion groups)
  where
    groups = foldr S.insert S.empty (cells disk)
    isRegion (Region _ ) = True
    isRegion _ = False

isUnallocated :: Disk Cell -> Pos -> Bool
isUnallocated disk pos = (disk `at` pos) == Used

hasUnallocatedGroups :: Disk Cell -> Bool
hasUnallocatedGroups disk =
  let
    hasUnallocatedRow row = any (== Used) (A.elems row)
  in
    any hasUnallocatedRow (A.elems disk)
    
findUnallocatedGroup :: Disk Cell -> Pos
findUnallocatedGroup disk = 
    head $ filter (isUnallocated disk) [(x, y) | x <- xIndices disk, y <- yIndices disk]

allocateGroup :: Int -> Disk Cell -> Disk Cell
allocateGroup index disk =
  if hasUnallocatedGroups disk
  then allocateGroup' index disk (findUnallocatedGroup disk)
  else disk
  
allocateGroup' :: Int -> Disk Cell -> Pos -> Disk Cell
allocateGroup' index disk pos =
  foldl (allocateGroup' index) (allocateCell index disk pos) (neighbours disk pos)

neighbours :: Disk Cell -> Pos -> [Pos]
neighbours disk (x, y) =
    filter (isUnallocated disk) . filter (isValidPos disk) $ [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]  

allocateCell :: Int -> Disk Cell -> Pos -> Disk Cell
allocateCell index disk pos = disk // (pos, Region index)

showDisk :: Disk Cell -> [String]
showDisk = map showRow . A.elems
  where
    showRow = concat . map showCell . A.elems
    showCell Empty = "empty"
    showCell Used  = "#used"
    showCell (Region n) = take 5 $ (show n ++ repeat '.')
    
showCorner :: Disk Cell -> String
showCorner disk = unlines $ take 8 $ map (take 40) $ showDisk disk
