{-# LANGUAGE TemplateHaskell #-}
module Day19 where
    
import Control.Lens hiding (at)
import Control.Monad.State
import Data.Char
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.Vector as V
import Prelude hiding (Left, Right)

type Grid = V.Vector (V.Vector Char)

data Direction = Up | Down | Left | Right deriving (Show)

nextDirs :: Direction -> [Direction]
nextDirs Down  = [Left, Right]
nextDirs Up    = [Left, Right]
nextDirs Left  = [Up, Down]
nextDirs Right = [Up, Down]

parseInput :: String -> Grid
parseInput = V.fromList . map V.fromList . lines

type Pos = (Int, Int)

move :: Direction -> Pos -> Pos
move Up    (x, y) = (x, y - 1)
move Down  (x, y) = (x, y + 1)
move Left  (x, y) = (x - 1, y)
move Right (x, y) = (x + 1, y)

at :: Grid -> Pos -> Char
at g (x, y) = g V.! y V.! x

data Path = Path {
    _grid :: Grid,
    _pos :: Pos,
    _dir :: Direction,
    _trace :: [Char],
    _stepCount :: Int
} deriving (Show)
makeLenses ''Path

findStart :: Grid -> Path
findStart g = Path g (x, y) Down [] 1
  where
      x = fromJust $ V.findIndex (== '|') (g V.! y)
      y = 0
 
canMove :: Grid -> Pos -> Direction -> Bool
canMove grid pos dir = inBounds && onPath
    where
        (x', y') = move dir pos
        inBounds = 0 <= y' && y' < V.length grid &&
                   0 <= x' && x' < V.length (grid V.! y')
        onPath = not . isSpace $ grid `at` (move dir pos)

nextDir :: Grid -> Pos -> Direction -> Maybe Direction
nextDir grid pos dir = listToMaybe $ filter (canMove grid pos) (nextDirs dir)  

step :: State Path Bool
step = do
    (Path grid pos' dir' _ _) <- get

    let current = grid `at` pos'
    when (isUpper current) $ modify $ over trace (++[current])
        
    if (canMove grid pos' dir') then do
        modify $ over pos (move dir') . over stepCount (+1)
        return True
                
    else do
        case (nextDir grid pos' dir') of
            Nothing -> return False
            Just newDir -> do
                modify $ set dir newDir
                return True
       
steps :: Int -> State Path ()
steps 0 = return ()
steps n = step >> steps (n-1)

follow :: State Path ()
follow = do
    continue <- step
    when continue follow
    
        