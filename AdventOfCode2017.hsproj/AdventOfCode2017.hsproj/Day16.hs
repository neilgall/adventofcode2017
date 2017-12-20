module Day16 where
  
import Prelude hiding (enumFromTo, splitAt, (++))
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Vector as V (Vector(..), (//), (!), (++), length, enumFromTo, slice, elemIndex)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- Parser

type Program = Char

data Move 
  = Spin Int
  | Exchange Int Int
  | Partner Program Program
  deriving (Show)
  
parseMoves :: GenParser Char st [Move]
parseMoves = parseMove `sepEndBy` (char ',')

parseMove :: GenParser Char st Move
parseMove = parseSpin <|> parseExchange <|> parsePartner

parseSpin :: GenParser Char st Move
parseSpin = Spin <$> intAfter 's'

parseExchange :: GenParser Char st Move
parseExchange = Exchange <$> intAfter 'x' <*> intAfter '/'
  
parsePartner :: GenParser Char st Move
parsePartner = Partner <$> letterAfter 'p' <*> letterAfter '/'

intAfter :: Char -> GenParser Char st Int
intAfter c = char c >> fmap read (many digit)  

letterAfter :: Char -> GenParser Char st Char
letterAfter c = char c >> letter

-- Part 1

type Programs = Vector Program

start :: Program -> Program -> Programs
start first last = enumFromTo first last

move :: Programs -> Move -> Programs
move ps (Spin n) =
  let
    l = V.length ps
  in
    (slice (l - n) n ps) ++ (slice 0 (l - n) ps)

move ps (Exchange a b) =
  ps // [(b, ps ! a), (a, ps ! b)]

move ps (Partner a b) = 
  let
    ia = fromJust (elemIndex a ps)
    ib = fromJust (elemIndex b ps)
  in
    ps // [(ib, ps ! ia), (ia, ps ! ib)]

dance :: Programs -> [Move] -> Programs
dance = foldl' move

-- Part 2

findCycleLength :: Programs -> [Move] -> Int
findCycleLength start moves = find start moves 1
  where
    find ps moves n
      | dance ps moves == start = n
      | otherwise = find (dance ps moves) moves (n+1)

optimalDances :: Int -> Programs -> [Move] -> Programs
optimalDances count start moves =
  let
    cycleLength = findCycleLength start moves
    remainder = count `mod` cycleLength
    repeatMoves = concat $ replicate remainder moves
  in
    dance start repeatMoves
    