{-# LANGUAGE BangPatterns #-}
module Day13 where

import qualified Data.Map as Map
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data Layer = Layer Int Int deriving (Show, Eq)

parseLayers :: GenParser Char st [Layer]
parseLayers = parseLayer `sepEndBy` endOfLine

parseLayer :: GenParser Char st Layer
parseLayer = do
  depth <- integer
  spaces >> char ':' >> spaces
  range <- integer
  pure $ Layer depth range
  
integer :: GenParser Char st Int
integer = fmap read (many digit)  

-- Part 1

data Direction = Down | Up deriving (Show)
data Scanner = Scanner Layer Int Direction deriving (Show)

depth :: Layer -> Int
depth (Layer d _) = d

atEnd :: Layer -> Int -> Bool
atEnd (Layer _ range) pos = pos == pred range

mkScanner :: Layer -> Scanner
mkScanner !layer = Scanner layer 0 Down

scannerDepth :: Scanner -> Int
scannerDepth (Scanner layer _ _) = depth layer

moveScanner :: Scanner -> Scanner

-- can't move a scanner with depth of only one
moveScanner s@(Scanner (Layer _ 1) _ _) = s

moveScanner (Scanner layer pos Down)
  | atEnd layer pos = Scanner layer (pred pos) Up
  | otherwise = Scanner layer (succ pos) Down
  
moveScanner (Scanner layer pos Up)
  | pos == 0 = Scanner layer (succ pos) Down
  | otherwise = Scanner layer (pred pos) Up 
  
trip :: [Layer] -> [Int]
trip layers = trip' (map mkScanner layers) 0 []

trip' :: [Scanner] -> Int -> [Int] -> [Int]
trip' !scanners !pos !results =
  let
    last = maximum $ map scannerDepth scanners
    nextPos = succ pos
    nextScanners = map moveScanner scanners
    caught = map scannerDepth . filter (isCaught pos) $ scanners
    nextResults = results ++ caught
  in
    if pos > last then results
    else trip' nextScanners nextPos nextResults
    
isCaught :: Int -> Scanner -> Bool
isCaught pos (Scanner (Layer d _) scanner _) = pos == d && scanner == 0 

severity :: [Layer] -> [Int] -> Int
severity _ [] = 0
severity layers (c:cs) =
  let
    findLayer = filter (\l -> depth l == c) layers
    layerSeverity (Layer d r) = d * r
    local = sum $ map layerSeverity findLayer
  in
    local + severity layers cs
    
-- Part 2

layerCatches :: Int -> Layer -> Bool
layerCatches delay (Layer depth range) =
  (delay + depth) `mod` (2 * (range - 1)) == 0

tripCatches :: [Layer] -> Int -> Bool
tripCatches layers delay = any (layerCatches delay) layers

delayTrip :: [Layer] -> Int
delayTrip layers = delayTrip' layers 0

delayTrip' :: [Layer] -> Int -> Int
delayTrip' layers delay =
  if tripCatches layers delay then delayTrip' layers (succ delay)
  else delay
  
    
