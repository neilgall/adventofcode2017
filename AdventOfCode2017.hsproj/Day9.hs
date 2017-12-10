module Day9 where

import Data.Either (rights)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data Item = Garbage Int | Group [Item] deriving (Show)

stream :: GenParser Char st Item
stream = group

group :: GenParser Char st Item
group = fmap Group $ between (char '{') (char '}') groupContent
groupContent = (group <|> garbage) `sepBy` (char ',')
  
garbage :: GenParser Char st Item
garbage = between (char '<') (char '>') garbageChars
  
garbageChars :: GenParser Char st Item
garbageChars = do
  count <- many (bang <|> notChar '>')
  pure $ Garbage (sum count)

bang :: GenParser Char st Int
bang = (char '!') >> anyChar >> pure 0

notChar :: Char -> GenParser Char st Int
notChar c = satisfy (/= c) >> pure 1

-- Part 1

parseStream :: String -> Either ParseError Item
parseStream = parse stream ""

run :: (Item -> a) -> String -> Either ParseError a
run f s = f <$> parseStream s

score :: Int -> Item -> Int
score _ (Garbage _) = 0
score n (Group gs) = n + (sum . map (score (n+1))) gs

-- Part 2

countGarbage :: Item -> Int
countGarbage (Garbage n) = n
countGarbage (Group gs) = (sum . map countGarbage) gs
