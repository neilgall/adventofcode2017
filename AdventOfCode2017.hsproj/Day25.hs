module Day25 where
  
import Prelude hiding (Left, Right)
import Data.Char (digitToInt)
import Data.Either (rights)
import qualified Data.IntMap.Strict as T
import qualified Data.Map.Strict as M
import Text.Parsec.Char
import qualified Text.ParserCombinators.Parsec as P

type State = Char
type ActionKey = (State, Int)

data Direction = Left | Right deriving (Eq, Show)

data Action = Action {
  _write :: Int,
  _move :: Direction,
  _nextState :: State
} deriving (Show)

data Blueprint = Blueprint {
  _start :: State,
  _steps :: Int,
  _actions :: M.Map ActionKey Action
} deriving (Show)

parseBlueprint :: P.GenParser Char st Blueprint
parseBlueprint = do
  string "Begin in state "
  begin <- letter
  string "." >> endOfLine
  string "Perform a diagnostic checksum after "
  steps <- fmap read (P.many digit)
  string " steps."
  actions <- P.many parseAction
  pure $ Blueprint begin steps (M.fromList $ concat actions)

parseAction :: P.GenParser Char st [(ActionKey, Action)]
parseAction = do
  spaces >> string "In state "
  state <- letter
  string ":" >> endOfLine
  P.count 2 $ do
    spaces >> string "If the current value is "
    value <- fmap digitToInt digit
    string ":" >> endOfLine
    spaces >> string "- Write the value "
    write <- fmap digitToInt digit
    string "." >> endOfLine
    spaces >> string "- Move one slot to the "
    dir <- (string "left" >> pure Left) P.<|> (string "right" >> pure Right)
    string "." >> endOfLine
    spaces >> string "- Continue with state "
    next <- letter
    string "."
    pure $ ((state, value), Action write dir next)
  
load :: String -> IO Blueprint
load f = do
  c <- readFile f
  return $ head . rights . pure $ P.parse parseBlueprint f c
  
data Machine = Machine {
  _state :: !State,
  _pos :: !Int,
  _tape :: T.IntMap Int
} deriving (Show)

newMachine :: Blueprint -> Machine
newMachine bp = Machine {
  _state = _start bp,
  _pos = 0,
  _tape = T.empty
}

runBlueprint :: Blueprint -> Machine
runBlueprint bp = runSteps (_steps bp) (_actions bp) (newMachine bp)

runSteps :: Int -> M.Map ActionKey Action -> Machine -> Machine
runSteps 0 _ machine = machine
runSteps n actions machine = runSteps (n-1) actions (runStep actions machine)

runStep :: M.Map ActionKey Action -> Machine -> Machine
runStep actions (Machine state pos tape) = Machine newState newPos newTape
  where
    tapeValue = T.findWithDefault 0 pos tape
    action = actions M.! (state, tapeValue)
    newTape = T.insert pos (_write action) tape
    newPos = move pos (_move action)
    newState = _nextState action
    
move n Left = n-1
move n Right = n+1

checksum :: Machine -> Int
checksum (Machine _ _ tape) = length . filter (==1) . map snd . T.toList $ tape

