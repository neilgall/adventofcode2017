module Day7 where
 
import Data.Maybe (maybe, listToMaybe, fromJust)
import Data.Foldable (maximumBy)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map

type Name = String
type Weight = Int

-- Raw Input Parser

data RawInput = RawInput Name Weight [Name] deriving (Show)

rawInput :: GenParser Char st RawInput
rawInput = do
  n <- pname
  w <- pweight
  cs <- option [] pchildren
  return (RawInput n w cs)
  
pname :: GenParser Char st String
pname = many letter

pweight :: GenParser Char st Int
pweight = do
  spaces >> char '('
  w <- many digit
  char ')' >> spaces
  return (read w)

parrow :: GenParser Char st Char
parrow = (char '-') >> (char '>')
  
pchildren :: GenParser Char st [String]
pchildren = parrow >> spaces >> (pname `sepBy` (char ',' >> spaces))

-- Part 1

type Index = Map.Map Name RawInput

makeIndex :: [RawInput] -> Index
makeIndex [] = Map.empty
makeIndex (r@(RawInput name _ _):rs) = Map.insert name r (makeIndex rs) 

data Tower = Empty | Tower Name Weight [Tower] deriving (Show, Eq)

tower :: [RawInput] -> Tower
tower rs = tower' (makeIndex rs) rs

tower' :: Index -> [RawInput] -> Tower
tower' _ [] = Empty
tower' index ((RawInput name _ _):rs) = deeperOf (makeTower index name) (tower' index rs)

makeTower :: Index -> Name -> Tower
makeTower index name = case Map.lookup name index of
  Nothing -> Empty
  Just (RawInput name weight children) -> Tower name weight (map (makeTower index) children)
   
deeperOf :: Tower -> Tower -> Tower
deeperOf t u = if (depth t) > (depth u) then t else u

depth :: Tower -> Int
depth Empty = 0
depth (Tower _ _ []) = 1
depth (Tower _ _ cs) = 1 + maximum (map depth cs)

-- Part 2 

weight :: Tower -> Int
weight Empty = 0
weight (Tower _ w cs) = w + sum (map weight cs)

noError = (Empty, 0)

balanceError :: Tower -> (Tower, Int)
balanceError Empty = noError
balanceError (Tower _ _ []) = noError
balanceError t@(Tower _ _ cs) =
  let
    childWeights = map weight cs
    childErrors = map balanceError cs    
    childrenBalanced = null $ filter (/= noError) childErrors
    badChildWeight = fromJust $ oddOneOut (==) childWeights
    badChild = head $ filter (\t -> weight t == badChildWeight) cs 
    goodChild = head $ filter (\t -> weight t /= badChildWeight) cs
    selfError = (badChild, weight badChild - weight goodChild)
  in
    if allEqual childWeights then noError
    else if childrenBalanced then selfError
    else balanceError badChild
    
allEqual :: [Int] -> Bool
allEqual [] = True
allEqual (x:[]) = True
allEqual (x:y:zs) = (x == y) && allEqual (y:zs)

flatten :: Tower -> Tower
flatten Empty = Empty
flatten t@(Tower n w cs) = Tower n (weight t) (prune' cs)
  where
    prune' [] = []
    prune' (t@(Tower n w _):cs) = (Tower n (weight t) []):(prune' cs)
    
oddOneOut :: (a -> a -> Bool) -> [a] -> Maybe a
oddOneOut p xs = oddOneOut' p xs []

oddOneOut' :: (a -> a -> Bool) -> [a] -> [a] -> Maybe a
oddOneOut' p [] _ = Nothing
oddOneOut' p (x:[]) _ = Nothing
oddOneOut' p (x:x':xs) ys
  | p x x' = oddOneOut' p (x':xs) (x:ys)
  | otherwise = Just $ if p x (head (xs ++ ys)) then x' else x
 