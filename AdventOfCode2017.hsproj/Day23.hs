{-# LANGUAGE LambdaCase, TemplateHaskell, RankNTypes #-}
module Day23 where
  
import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import Data.Either (rights)
import Data.Ix (inRange)
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Text.Parsec.Char
import qualified Text.ParserCombinators.Parsec as P

type Register = Char

data Arg
    = Value Int
    | Reg Register
    | Label String
    deriving (Show, Eq)

data Instr
    = Set Register Arg
    | Sub Register Arg
    | Mul Register Arg
    | Jnz Arg Arg
    deriving (Show, Eq)
    
data LabelledInstr = LabelledInstr (Maybe String) Instr deriving (Show, Eq)

parseInstrs :: P.GenParser Char st [LabelledInstr]
parseInstrs = parseInstr `P.sepEndBy` endOfLine

parseInstr :: P.GenParser Char st LabelledInstr
parseInstr = do
  label <- P.optionMaybe parseLabel
  spaces
  instr <- P.choice [P.try parseSet,
                     P.try parseSub,
                     P.try parseMul,
                     P.try parseJnz]
  return (LabelledInstr label instr)

parseLabel :: P.GenParser Char st String
parseLabel = char ':' >> P.many (P.choice [letter, digit, char '_'])

parseArg :: P.GenParser Char st Arg
parseArg = spaces >> P.choice [Reg <$> parseReg, Label <$> parseLabel, parseConst]

parseConst :: P.GenParser Char st Arg
parseConst = Value <$> fmap read (P.many (char '-' P.<|> digit))

parseReg :: P.GenParser Char st Register
parseReg = spaces >> letter

parseSet = Set <$> (string "set" >> parseReg) <*> parseArg 
parseSub = Sub <$> (string "sub" >> parseReg) <*> parseArg 
parseMul = Mul <$> (string "mul" >> parseReg) <*> parseArg 
parseJnz = Jnz <$> (string "jnz" >> parseArg) <*> parseArg

parseProgram :: String -> [LabelledInstr]
parseProgram input = concat $ rights [P.parse parseInstrs "" input]

type Program = A.Array Int Instr

data CPU = CPU {
    _program :: Program,
    _symbols :: M.Map String Int,
    _registers :: M.Map Register Int,
    _pc :: Int,
    _muls :: Int
} deriving (Show)
makeLenses ''CPU

load :: String -> Int -> IO CPU
load f a = do
  input <- readFile f
  let p = head . rights . pure $ P.parse parseInstrs f input
  let program = A.listArray (0, length p - 1) p
  return CPU {
    _program = toArray (map dropLabel p),
    _symbols = findSymbols (toArray p),
    _registers = M.singleton 'a' a,
    _pc = 0,
    _muls = 0
  }
  
dropLabel :: LabelledInstr -> Instr
dropLabel (LabelledInstr _ i) = i

toArray :: [a] -> A.Array Int a
toArray as = A.listArray (0, length as - 1) as

findSymbols :: A.Array Int LabelledInstr -> M.Map String Int
findSymbols = M.fromList . map removeMaybes . filter findLabelled . map indexedLabels . A.assocs
  where
    indexedLabels (i, (LabelledInstr lab _)) = (lab, i)
    findLabelled (lab, _) = isJust lab
    removeMaybes (Just label, i) = (label, i)

register :: Register -> Lens' CPU Int
register r f core@CPU{ _registers = regs } = fmap
    (\v -> core { _registers = M.insert r v regs })
    (f (M.findWithDefault 0 r regs))

jump :: Int -> CPU -> CPU
jump n = over pc (+(n-1))

modifyRegister :: Register -> (Int -> Int -> Int) -> Int -> State CPU ()
modifyRegister r f a = modify $ over (register r) (f a)

running :: State CPU Bool
running = do
  CPU{ _program = p, _pc = pc } <- get
  return $ inRange (A.bounds p) pc

runCPU :: State CPU ()
runCPU = running >>= \r -> when r $ step >> runCPU

fetchInstr :: State CPU Instr
fetchInstr = do
    pc' <- gets $ view pc
    prog <- gets $ view program
    modify $ over pc (+1)
    return $ prog A.! pc'

runInstr :: Instr -> State CPU ()
runInstr instr = case instr of
    Set r x -> eval x >>= modifyRegister r const
    Sub r x -> eval x >>= modifyRegister r (flip (-))
    Mul r x -> do
      eval x >>= modifyRegister r (*)
      modify $ over muls (+1)
    Jnz r x -> jnz r x

step :: State CPU ()
step = fetchInstr >>= runInstr

jnz :: Arg -> Arg -> State CPU ()
jnz r x = do
    r' <- eval r
    pc' <- gets $ view pc
    when (r' /= 0) $
      case x of
        (Value i)   -> modify (jump i)
        (Reg r)     -> gets (view $ register r) >>= modify . jump
        (Label lab) -> gets (label lab . view symbols) >>= modify . set pc  

label :: String -> M.Map String Int -> Int
label lab syms = fromJust $ M.lookup lab syms

eval :: Arg -> State CPU Int
eval (Value i) = return i
eval (Reg r) = gets $ view (register r)


-- Part 2

isPrime :: Int -> Bool
isPrime k = null [x | x <- [2..k-1], k `mod` x == 0]

b = 93*100 + 100000
c = b + 17000
range = [b,b+17..c]
