{-# LANGUAGE LambdaCase #-}
module Day18 where
  
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
    = Const Int
    | Reg Register
    deriving (Show, Eq)

data Instr
    = Snd Arg
    | Set Register Arg
    | Add Register Arg
    | Mul Register Arg
    | Mod Register Arg
    | Rcv Register
    | Jgz Arg Arg
    deriving (Show, Eq)
    
parseInstrs :: P.GenParser Char st [Instr]
parseInstrs = parseInstr `P.sepEndBy` endOfLine

parseInstr :: P.GenParser Char st Instr
parseInstr = P.choice [P.try parseSnd,
                       P.try parseSet,
                       P.try parseAdd,
                       P.try parseMul,
                       P.try parseMod,
                       P.try parseRcv,
                       P.try parseJgz]

parseArg :: P.GenParser Char st Arg
parseArg = spaces >> P.choice [Reg <$> parseReg, parseConst]

parseConst :: P.GenParser Char st Arg
parseConst = Const <$> fmap read (P.many (char '-' P.<|> digit))

parseReg :: P.GenParser Char st Register
parseReg = spaces >> letter

parseSnd = Snd <$> (string "snd" >> spaces >> parseArg)
parseSet = Set <$> (string "set" >> parseReg) <*> parseArg 
parseAdd = Add <$> (string "add" >> parseReg) <*> parseArg 
parseMul = Mul <$> (string "mul" >> parseReg) <*> parseArg 
parseMod = Mod <$> (string "mod" >> parseReg) <*> parseArg 
parseRcv = Rcv <$> (string "rcv" >> parseReg)
parseJgz = Jgz <$> (string "jgz" >> parseArg) <*> parseArg

parseProgram :: String -> [Instr]
parseProgram input = concat $ rights [P.parse parseInstrs "" input]

type Program = A.Array Int Instr

data CPU = CPU {
    program :: Program,
    registers :: M.Map Register Int,
    pc :: Int,
    sound :: Int,
    lastRcv :: Maybe Int
} deriving (Show)

load :: [Instr] -> CPU
load p = CPU {
    program = A.listArray (0, length p - 1) p,
    registers = M.empty,
    pc = 0,
    sound = 0,
    lastRcv = Nothing
}

register :: Register -> CPU -> Int
register r cpu = M.findWithDefault 0 r (registers cpu)

jump :: Int -> CPU -> CPU
jump n cpu = cpu { pc = (pc cpu) + n }

modifyRegister :: Register -> (Int -> Int -> Int) -> Int -> State CPU ()
modifyRegister r f a = modify $ \cpu -> cpu { registers = M.insert r (f a $ register r cpu) (registers cpu) }

running :: State CPU Bool
running = do
  (CPU p _ pc _ r) <- get
  return $ r == Nothing && inRange (A.bounds p) pc

runCPU :: State CPU ()
runCPU = running >>= \r -> when r $ step >> runCPU

fetchInstr :: State CPU Instr
fetchInstr = do
    instr <- gets $ \cpu -> (program cpu) A.! (pc cpu)
    modify (jump 1)
    return instr

runInstr :: Instr -> State CPU ()
runInstr instr = case instr of
    Snd x   -> eval x >>= (\y -> modify $ \cpu -> cpu { sound = y })
    Set r x -> eval x >>= modifyRegister r const
    Add r x -> eval x >>= modifyRegister r (+)
    Mul r x -> eval x >>= modifyRegister r (*)
    Mod r x -> eval x >>= modifyRegister r (flip mod)
    Rcv r   -> rcv r
    Jgz r x -> jgz r x

step :: State CPU ()
step = fetchInstr >>= runInstr

rcv :: Register -> State CPU ()
rcv r = do
    v <- gets $ register r
    when (v > 0) $ do 
        f <- gets sound
        modify $ \cpu -> cpu { lastRcv = Just f }
 
jgz :: Arg -> Arg -> State CPU ()
jgz r x = do
    r' <- eval r
    x' <- eval x
    when (r' > 0) $ modify (jump $ x' - 1)

eval :: Arg -> State CPU Int
eval (Const i) = return i
eval (Reg r) = gets (register r)
