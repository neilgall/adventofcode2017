{-# LANGUAGE BangPatterns, DeriveFunctor #-}
module Day20 where

import Data.Either (rights)
import Data.List (minimum, elemIndex, (\\))
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Ord
import qualified Data.Set as Set
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- Parser

data Vector a = Vector a a a deriving (Eq, Ord, Functor)
data Component a = Component { _position, _velocity, _acceleration :: a } deriving (Eq, Ord, Functor)
type Particle a = Vector (Component a)
type Coord = Double
type Time = Int

instance (Show a) => Show (Vector a) where
    show (Vector x y z) = show x ++ "/" ++ show y ++ "/" ++ show z

instance (Show a) => Show (Component a) where
    show (Component p v a) = "{ p" ++ show p ++ " v" ++ show v ++ " a" ++ show a ++ " }"

particleFromVectors :: Vector a -> Vector a -> Vector a -> Particle a
particleFromVectors (Vector px py pz) (Vector vx vy vz) (Vector ax ay az) =
    Vector (Component px vx ax) (Component py vy ay) (Component pz vz az) 

parseInput :: GenParser Char st [Particle Coord]
parseInput = many parseParticle

parseParticle :: GenParser Char st (Particle Coord)
parseParticle = particleFromVectors <$> parsePosition <*> parseVelocity <*> parseAcceleration
parsePosition = string "p=" >> parseVector
parseVelocity = string ", v=" >> parseVector
parseAcceleration = string ", a=" >> parseVector

parseVector :: GenParser Char st (Vector Coord)
parseVector = do
    char '<' >> spaces
    x <- parseNumber
    char ',' >> spaces
    y <- parseNumber
    char ',' >> spaces
    z <- parseNumber
    char '>' >> spaces
    pure $ Vector x y z

parseNumber :: GenParser Char st Coord
parseNumber = do
    sign <- optionMaybe (char '-')
    digits <- many digit
    pure $ read (fromMaybe ' ' sign : digits)

-- Part 1

load :: String -> IO [Particle Coord]
load f = do
    content <- readFile f
    pure . head . rights . pure $ parse parseInput f content
    
manhatten :: Num a => Vector a -> a
manhatten (Vector x y z) = abs x + abs y + abs z

indexOfMinimum :: (Ord a) => [a] -> Maybe Int
indexOfMinimum as = elemIndex (minimum as) as

integrate :: Fractional a => a -> Component a -> a
integrate t (Component p v a) = 0.5*t*t*a + t*v + p

integrateParticle :: Fractional a => a -> Particle a -> Vector a
integrateParticle t = fmap (integrate t)

-- Part 2

step :: (Fractional a) => [Particle a] -> [Particle a]
step [] = []
step (p:ps) = fmap step' p:step ps
    where
        step' (Component p v a) = Component (p+v+a) (v+a) a

position :: Particle a -> Vector a
position = fmap pos
    where
        pos (Component p _ _) = p

collisions :: (Eq a) => [Particle a] -> [Particle a]
collisions [] = []
collisions (p:ps) = include' ++ collisions exclude
    where
        (include, exclude) = span (collide p) ps
        include' = if null include then [] else p:include
        collide p1 p2 = position p1 == position p2

maxDistance :: (Ord a, Fractional a) => [Particle a] -> a
maxDistance ps = if length ps < 2 then 0 else maxDistance' ps
    where
        maxDistance' (p:ps) = maximum [distance p p' | p' <- ps]
        distance p1 p2 = manhatten $ diff (position p1) (position p2)
        diff (Vector x y z) (Vector x' y' z') = Vector (x-x') (y-y') (z-z')

simulate :: (Fractional a, Ord a) => [Particle a] -> [Particle a]
simulate ps = simulate' (maxDistance ps) ps

simulate' :: (Fractional a, Ord a) => a -> [Particle a] -> [Particle a]
simulate' maxD ps = if terminate then nextps else simulate' (max d maxD) nextps
    where
        ps' = step ps
        colls = collisions ps'
        nextps = ps' \\ colls
        d = maxDistance nextps
        terminate =(d > maxD && null colls) || length nextps < 2
