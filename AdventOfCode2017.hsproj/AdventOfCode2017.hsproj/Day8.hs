module Day8 where
  
import qualified Data.Map as Map
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- Input parser

data Comparator = Eq | Neq | Lt | Gt | Lteq | Gteq
  deriving (Show, Eq)
 
mkComparator :: String -> Comparator
mkComparator s
  | s == "==" = Eq
  | s == "!=" = Neq
  | s == ">"  = Gt
  | s == "<"  = Lt
  | s == ">=" = Gteq
  | s == "<=" = Lteq

data Instruction a = Instruction {
  modifyRegister :: String,
  modifyOffset :: a,
  conditionRegister:: String,
  conditionComparator :: Comparator,
  conditionComparand :: a
} deriving (Show, Eq)

parseInstruction :: GenParser Char st (Instruction Int)
parseInstruction = do
  mreg <- pname
  sign <- pincdec
  offs <- pvalue
  pif
  creg <- pname
  cop <- poperator
  cval <- pvalue
  pure $ Instruction mreg (sign * offs) creg cop cval
 
pname :: GenParser Char st String
pname = spaces >> many letter

pincdec :: GenParser Char st Int
pincdec = spaces >> fmap (\w -> if w == "inc" then 1 else -1) (many letter)

pvalue :: GenParser Char st Int
pvalue = do
  spaces
  sign <- option 1 (char '-' >> pure (-1))
  digits <- many digit
  pure (sign * read digits)
  
pif :: GenParser Char st String
pif = spaces >> string "if"

poperator :: GenParser Char st Comparator
poperator = spaces >> fmap mkComparator (many poperatorChars)

poperatorChars :: GenParser Char st Char
poperatorChars = choice [char '=', char '!', char '<', char '>']

-- Part 1

data ProgramState a = ProgramState {
  history :: [[(String, a)]],
  registers :: Map.Map String a
} deriving (Show)

newState = ProgramState { history = [], registers = Map.empty }

listState :: ProgramState a -> [(String, a)]
listState = Map.assocs . registers

readRegister :: Num a => ProgramState a -> String -> a
readRegister state name =
  Map.findWithDefault 0 name (registers state)

writeRegister :: ProgramState a -> String -> a -> ProgramState a
writeRegister state name value =
  state { 
    history = (listState state):(history state),
    registers = Map.insert name value (registers state)
  }

runProgram :: (Num a, Ord a) => ProgramState a -> [Instruction a] -> ProgramState a
runProgram = foldl runInstruction

runInstruction :: (Num a, Ord a) => ProgramState a -> Instruction a -> ProgramState a
runInstruction state instruction =
  if conditionMatched state instruction
    then nextState state instruction
    else state

conditionMatched :: (Num a, Ord a) => ProgramState a -> Instruction a -> Bool
conditionMatched state instruction =
  let
    r = readRegister state (conditionRegister instruction)
    c = conditionComparand instruction
    op = comparison (conditionComparator instruction)
  in
    op r c
   
comparison :: (Num a, Ord a) => Comparator -> (a -> a -> Bool)
comparison c = case c of
  Eq   -> (==)
  Neq  -> (/=)
  Gt   -> (>)
  Lt   -> (<)
  Gteq -> (>=)
  Lteq -> (<=)
  
nextState :: Num a => ProgramState a -> Instruction a -> ProgramState a
nextState state instruction =
  let
    reg = modifyRegister instruction
    offs = modifyOffset instruction
    val = readRegister state reg
  in
    writeRegister state reg (offs + val)
    