module Day22 where
  
import Prelude hiding (Left, Right)
import Control.Monad.State
import qualified Data.Matrix as M
import Data.Tuple (swap)

data Node = Clean | Infected deriving (Eq)
data Direction = Up | Right | Down | Left deriving (Eq, Show)

instance Show Node where
  show Clean = "."
  show Infected = "#"

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

type Pos = (Int, Int)
type Grid = M.Matrix Node

move :: Pos -> Direction -> Pos
move (x,y) Up    = (x, y-1)
move (x,y) Down  = (x, y+1)
move (x,y) Left  = (x-1, y)
move (x,y) Right = (x+1, y)

toNode :: Char -> Node
toNode '.' = Clean
toNode '#' = Infected

load :: String -> Grid
load = M.fromLists . (map . map) toNode . lines

centre :: Grid -> Pos
centre g = (c (M.ncols g), c (M.nrows g))
  where
    c i = i `div` 2 + 1

emptyRow :: Grid -> Grid
emptyRow g = M.fromLists [replicate (M.ncols g) Clean]

emptyCol :: Grid -> Grid
emptyCol g = M.fromLists $ replicate (M.nrows g) [Clean]

growOnTop :: Grid -> Grid
growOnTop g = emptyRow g M.<-> g

growOnBottom :: Grid -> Grid
growOnBottom g = g M.<-> emptyRow g
    
growOnLeft :: Grid -> Grid
growOnLeft g = emptyCol g M.<|> g

growOnRight :: Grid -> Grid
growOnRight g = g M.<|> emptyCol g

growGrid :: Grid -> Pos -> (Grid, Pos)
growGrid g p@(x,y) 
  | x <= 0 = (growOnLeft g, (x+1, y))
  | y <= 0 = (growOnTop g, (x, y+1))
  | x > (M.ncols g) = (growOnRight g, p)
  | y > (M.nrows g) = (growOnBottom g, p)
  | otherwise = (g, p)

data VirusCarrier = VirusCarrier {
  _grid :: Grid,
  _pos :: Pos,
  _dir :: Direction,
  _infections :: Int
} deriving (Show)

currentNode :: VirusCarrier -> Node
currentNode vc = (_grid vc) M.! (swap $ _pos vc)

countInfection :: VirusCarrier -> VirusCarrier
countInfection vc = vc { _infections = 1 + (_infections vc) }

initial :: Grid -> VirusCarrier
initial g = VirusCarrier { _grid = g, _pos = centre g, _dir = Up, _infections = 0 }

burst :: State VirusCarrier ()
burst = do
  node <- gets currentNode 
  case node of
    Clean -> burst' Infected turnLeft >> modify countInfection
    Infected -> burst' Clean turnRight

burst' :: Node -> (Direction -> Direction) -> State VirusCarrier ()
burst' node turn = do
  newDir  <- gets $ turn . _dir
  pos     <- gets _pos
  setNode <- gets $ (M.setElem node (swap pos)) . _grid
  let (newGrid, newPos) = growGrid setNode (move pos newDir)
  modify $ \vc -> vc { _grid = newGrid, _pos = newPos, _dir = newDir }
  
burstN :: Int -> State VirusCarrier ()
burstN 0 = return ()
burstN n = burst >> burstN (n-1)
