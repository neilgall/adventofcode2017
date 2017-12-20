module Day3 where
  
import Prelude hiding (Left, Right)
import qualified Data.Map.Strict as Map

-- part 1

boxSizeFromIndex :: Int -> Int
boxSizeFromIndex n = 2 * ceiling ((sqrt $ fromIntegral n) / 2)

data Box = Box Int Int Int Int deriving (Show)
data Side = Top | Right | Bottom | Left deriving (Show)
data Position = Position Int Int deriving (Eq, Show)

boxFromSize :: Int -> Box
boxFromSize size =
  let
    side = size - 1
    topLeft = size * size
    topRight = topLeft - side
    bottomRight = topRight - side
    bottomLeft = bottomRight - side
  in
    Box topLeft topRight bottomRight bottomLeft

side :: Int -> Side
side n =
  let
    (Box topLeft topRight bottomRight bottomLeft) = boxFromSize . boxSizeFromIndex $ n
  in
    if n > topRight then Top
    else if n > bottomRight then Right
    else if n > bottomLeft then Bottom
    else Left

positionFromIndex :: Int -> Position
positionFromIndex n =
  let
    half = (boxSizeFromIndex n) `div` 2
    minusHalf = 1 - half
    (Box topLeft topRight bottomRight bottomLeft) = boxFromSize . boxSizeFromIndex $ n
  in
    case side n of
      Top -> Position (half - (n - topRight)) half
      Right -> Position half (n - bottomRight + minusHalf)
      Bottom -> Position (n - bottomLeft + minusHalf) minusHalf
      Left -> Position minusHalf (bottomLeft - n + minusHalf)

manhattanDistance :: Position -> Int
manhattanDistance (Position x y) = abs x + abs y

-- part 2

boxSizeFromPosition :: Position -> Int
boxSizeFromPosition (Position x y) = 2 * maximum [x, y, 1 - x, 1 - y]

sideFromPosition :: Position -> Side
sideFromPosition p@(Position x y) =
  let
    size = boxSizeFromPosition p
    half = size `div` 2
  in
    if 1 - x == half && y < half then Left
    else if 1 - y == half then Bottom
    else if x == half then Right
    else Top

indexFromPosition :: Position -> Int
indexFromPosition p@(Position x y) =
  let
    size = boxSizeFromPosition p
    half = size `div` 2
    (Box topLeft topRight bottomRight bottomLeft) = boxFromSize size
    side = sideFromPosition p
  in
    case side of
      Top -> topLeft - half - x + 1
      Right -> topRight - half + y
      Bottom -> bottomRight - half + x
      Left -> bottomLeft - half - y + 1

neighbourPositions :: Position -> [Position]
neighbourPositions p@(Position x y) = filter (/= p)
  [Position (x+xd) (y+yd) | xd <- [-1..1], yd <- [-1..1]]
  
valueAtIndex :: Int -> Int
valueAtIndex n = valueAtIndexSequential 1 n Map.empty

valueAtIndexSequential :: Int -> Int -> Map.Map Int Int -> Int
valueAtIndexSequential n m cache =
  let
    calculate = valueAtIndexWithCache cache n
  in
    if n == m then calculate
    else valueAtIndexSequential (n+1) m (Map.insert n calculate cache)

valueAtIndexWithCache :: Map.Map Int Int -> Int -> Int
valueAtIndexWithCache _ 1 = 1
valueAtIndexWithCache cache n =
  let
    position = positionFromIndex n
    neighbours = neighbourPositions position
    cached i = Map.findWithDefault 0 i cache
  in
    sum . map (cached . indexFromPosition) $ neighbours
