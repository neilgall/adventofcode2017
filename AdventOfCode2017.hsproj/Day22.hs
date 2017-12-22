module Day22 where
  
import Prelude hiding (Left, Right)
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Matrix as Mx
import Data.Tuple (swap)

data Node = Clean | Weakened | Infected | Flagged deriving (Eq)
data Direction = Up | Right | Down | Left deriving (Eq, Show)

instance Show Node where
  show Clean = "."
  show Infected = "#"
  show Weakened = "W"
  show Flagged = "F"

turnRight :: Direction -> Direction
turnRight Up = Right
turnRight Right = Down
turnRight Down = Left
turnRight Left = Up

turnLeft :: Direction -> Direction
turnLeft Up = Left
turnLeft Left = Down
turnLeft Down = Right
turnLeft Right = Up

turnAround :: Direction -> Direction
turnAround Up = Down
turnAround Down = Up
turnAround Left = Right
turnAround Right = Left

type Pos = (Int, Int)
type Grid = M.Map Pos Node

move :: Pos -> Direction -> Pos
move (x,y) Up    = (x, y-1)
move (x,y) Down  = (x, y+1)
move (x,y) Left  = (x-1, y)
move (x,y) Right = (x+1, y)

toNode :: Char -> Node
toNode '.' = Clean
toNode '#' = Infected

load :: String -> Grid
load s = toMap
  where
    mx = Mx.fromLists . (map . map) toNode . lines $ s
    rows = Mx.nrows mx
    cols = Mx.ncols mx
    toMap = M.fromList [((x,y), mx Mx.! (y,x)) | x <- [1..cols], y <- [1..rows]]

centre :: Grid -> Pos
centre g = (c width, c height)
  where
    (width, height) = maximum (M.keys g)
    c n = n `div` 2  + 1

data VirusCarrier = VirusCarrier {
  _grid :: !Grid,
  _pos :: !Pos,
  _dir :: !Direction,
  _infections :: !Int
} deriving (Show)

currentNode :: VirusCarrier -> Node
currentNode vc = M.findWithDefault Clean (_pos vc) (_grid vc)

countInfection :: VirusCarrier -> VirusCarrier
countInfection vc = vc { _infections = 1 + (_infections vc) }

initial :: Grid -> VirusCarrier
initial g = VirusCarrier { _grid = g, _pos = centre g, _dir = Up, _infections = 0 }

burst1 :: State VirusCarrier ()
burst1 = do
  node <- gets currentNode 
  case node of
     Clean -> burst' Infected turnLeft >> modify countInfection
     Infected -> burst' Clean turnRight

burst2 :: State VirusCarrier ()
burst2 = do
  node <- gets currentNode 
  case node of
    Clean -> burst' Weakened turnLeft
    Weakened -> burst' Infected id >> modify countInfection
    Infected -> burst' Flagged turnRight
    Flagged -> burst' Clean turnAround

burst' :: Node -> (Direction -> Direction) -> State VirusCarrier ()
burst' node turn = do
  newDir  <- gets $ turn . _dir
  pos     <- gets _pos
  newGrid <- gets $ (M.insert pos node) . _grid
  modify $ \vc -> vc { _grid = newGrid, _pos = (move pos newDir), _dir = newDir }
  
doN :: State s () -> Int -> State s ()
doN _ 0 = return ()
doN f n = f >> doN f (n-1)
