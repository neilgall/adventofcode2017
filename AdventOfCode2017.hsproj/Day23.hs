{-# LANGUAGE LambdaCase, TemplateHaskell, RankNTypes #-}
module Day23 where
  
import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import Data.Either (rights)
import Data.Ix (inRange)
import qualified Data.Array as A
import qualified Data.Map as M
import Text.Parsec.Char
import qualified Text.ParserCombinators.Parsec as P

type Register = Char

data Arg
    = Value Int
    | Reg Register
    deriving (Show, Eq)

data Instr
    = Set Register Arg
    | Sub Register Arg
    | Mul Register Arg
    | Jnz Arg Arg
    deriving (Show, Eq)
    
parseInstrs :: P.GenParser Char st [Instr]
parseInstrs = parseInstr `P.sepEndBy` endOfLine

parseInstr :: P.GenParser Char st Instr
parseInstr = P.choice [P.try parseSet,
                       P.try parseSub,
                       P.try parseMul,
                       P.try parseJnz]

parseArg :: P.GenParser Char st Arg
parseArg = spaces >> P.choice [Reg <$> parseReg, parseConst]

parseConst :: P.GenParser Char st Arg
parseConst = Value <$> fmap read (P.many (char '-' P.<|> digit))

parseReg :: P.GenParser Char st Register
parseReg = spaces >> letter

parseSet = Set <$> (string "set" >> parseReg) <*> parseArg 
parseSub = Sub <$> (string "sub" >> parseReg) <*> parseArg 
parseMul = Mul <$> (string "mul" >> parseReg) <*> parseArg 
parseJnz = Jnz <$> (string "jnz" >> parseArg) <*> parseArg

parseProgram :: String -> [Instr]
parseProgram input = concat $ rights [P.parse parseInstrs "" input]

type Program = A.Array Int Instr

data CPU = CPU {
    _program :: Program,
    _registers :: M.Map Register Int,
    _pc :: Int,
    _muls :: Int
} deriving (Show)
makeLenses ''CPU

load :: String -> IO CPU
load f = do
  input <- readFile f
  let p = head . rights . pure $ P.parse parseInstrs f input
  return CPU {
    _program = A.listArray (0, length p - 1) p,
    _registers = M.empty,
    _pc = 0,
    _muls = 0
  }

--register :: Register -> CPU -> Int
--register r = M.findWithDefault 0 r . view registers

register :: Register -> Lens' CPU Int
register r f core@CPU{ _registers = regs } = fmap
    (\v -> core { _registers = M.insert r v regs })
    (f (M.findWithDefault 0 r regs))

jump :: Int -> CPU -> CPU
jump n = over pc (+n)

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
    x' <- eval x
    when (r' /= 0) $ modify (jump $ x' - 1)

eval :: Arg -> State CPU Int
eval (Value i) = return i
eval (Reg r) = gets $ view (register r)
