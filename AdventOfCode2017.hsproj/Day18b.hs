{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Day18b where
  
import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import qualified Data.Array as A
import Data.Array.Lens
import Data.Either (rights)
import Data.Ix (inRange)
import qualified Data.Map as M
import Data.Map.Lens
import Text.Parsec.Char
import qualified Text.ParserCombinators.Parsec as P

type Register = Char

data Arg
    = Val Int
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
parseConst = Val <$> fmap read (P.many (char '-' P.<|> digit))

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

data Core = Core {
    _registers :: M.Map Register Int,
    _pc :: Int,
    _blocked :: Bool,
    _sentCount :: Int,
    _queue :: [Int]
} deriving (Show)

data CPU = CPU {
    _program :: Program,
    _cores :: M.Map Int Core
} deriving (Show)

mkCore :: Int -> Core
mkCore c = Core {
    _registers = M.fromList [('p', c)],
    _pc = 0,
    _blocked = False,
    _sentCount = 0,
    _queue = []
}

load :: Int -> [Instr] -> CPU
load cores prog = CPU {
    _program = A.listArray (0, length prog - 1) prog,
    _cores = M.fromList $ map (\c -> (c, mkCore c)) [0..cores-1]
}

makeLenses ''Core
makeLenses ''CPU

-- special lens to access a core by index
core :: Int -> Lens' CPU Core
core c f cpu@CPU{ _cores = cs} = fmap
    (\core -> cpu { _cores = M.insert c core cs })
    (f (M.findWithDefault (mkCore c) c cs))
    
-- special lens to access a register by name
register :: Register -> Lens' Core Int
register r f core@Core{ _registers = regs } = fmap
    (\v -> core { _registers = M.insert r v regs })
    (f (M.findWithDefault 0 r regs))

jump :: Int -> Int -> CPU -> CPU
jump c j = over (core c . pc) (+j)

running :: State CPU Bool
running = do
    bounds <- gets $ A.bounds . view program
    pcs <- gets $ toListOf (cores . traverse . pc)
    blocked <- gets $ toListOf (cores . traverse . blocked)
    return $ all (inRange bounds) pcs && not (all id blocked)

runCPU :: State CPU ()
runCPU = do
    r <- running 
    cs <- gets $ M.keys . view cores
    when r $ do 
        traverse step cs
        runCPU

fetchInstr :: Int -> State CPU Instr
fetchInstr c = do
    pc' <- gets $ view (core c . pc)
    prog <- gets $ view program
    modify $ over (core c . pc) (+1)
    return $ prog A.! pc'

runInstr :: Int -> Instr -> State CPU ()
runInstr c instr = case instr of
    Snd x   -> eval c x >>= send c
    Set r x -> eval c x >>= modifyRegister (core c . register r) const
    Add r x -> eval c x >>= modifyRegister (core c . register r) (+)
    Mul r x -> eval c x >>= modifyRegister (core c . register r) (*)
    Mod r x -> eval c x >>= modifyRegister (core c . register r) (flip mod)
    Rcv r   -> receive c r
    Jgz r x -> jgz c r x
    where
        modifyRegister r f a = modify $ over r (f a)

step :: Int -> State CPU ()
step c = fetchInstr c >>= runInstr c

steps :: Int -> Int -> State CPU ()
steps c 0 = return ()
steps c n = step c >> steps c (n-1)

send :: Int -> Int -> State CPU ()
send c msg = do
    cores <- gets $ length . view cores
    let to = (c + 1) `mod` cores
    modify $ over (core c . sentCount) (+1)
    modify $ over (core to . queue) (++[msg])
    modify $ set (core to . blocked) False

receive :: Int -> Register -> State CPU ()
receive c r = do
    q <- gets $ view (core c . queue)
    if null q then do
        modify $ set (core c . blocked) True
        modify $ jump c (-1)
    else do
        modify $ set (core c . register r) (head q)
        modify $ over (core c . queue) tail

jgz :: Int -> Arg -> Arg -> State CPU ()
jgz c r x = do
    r' <- eval c r
    x' <- eval c x
    when (r' > 0) $ modify $ jump c (x'-1)

eval :: Int -> Arg -> State CPU Int
eval _ (Val i) = return i
eval c (Reg r) = gets $ view (core c . register r)
