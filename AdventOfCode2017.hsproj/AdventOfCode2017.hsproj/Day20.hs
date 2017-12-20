{-# LANGUAGE BangPatterns, DeriveFunctor #-}
module Day20 where

import Data.Either (rights)
import Data.List (minimum, elemIndex, sort)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Ord
import qualified Data.Set as Set
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- Parser

data Vector a = Vector a a a deriving (Show, Eq, Ord, Functor)
data Component a = Component { _position, _velocity, _acceleration :: a } deriving (Show, Eq, Ord, Functor)
type Particle a = Vector (Component a)
type Coord = Double
type Time = Int

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

data Occurs a = Never | Always | AtTimes (Set.Set a) deriving (Show, Eq)

instance (Ord a) => Monoid (Occurs a) where
    mempty = Never
    Never `mappend` _ = Never
    _ `mappend` Never = Never
    Always `mappend` c = c
    c `mappend` Always = c
    (AtTimes t1) `mappend` (AtTimes t2) =
        let i = Set.intersection t1 t2 in if Set.null i then Never else (AtTimes i)

instance (Ord a) => Ord (Occurs a) where
    compare Never _ = GT
    compare _ Never = LT
    compare Always _ = LT
    compare _ Always = GT
    compare (AtTimes t1) (AtTimes t2) = compare (Set.findMin t1) (Set.findMin t2)

occursFromTimes :: [Double] -> Occurs Time
occursFromTimes ts = if null ts then Never else AtTimes (Set.fromList $ map round ts)

linear :: (RealFrac a) => a -> a -> a
linear a b = -b / a

quadratic :: (RealFrac a, Floating a) => a -> a -> a -> [a]
quadratic a b c = 
    let
        s = sqrt (b*b - 4*a*c)
        a2 = 2*a
    in
        sort [(-b + s) / a2, (-b - s) / a2]

componentCollisions :: Component Coord -> Component Coord -> Occurs Time
componentCollisions c1 c2 =
    let
        (Component p v a) = componentDiff c1 c2
        quadratic' = occursFromTimes $ filter (>= 0) $ quadratic (a/2) v p
        linear' = occursFromTimes $ filter (>= 0) $ pure $ linear v p
        point' = if p == 0 then Always else Never
    in
        if a == 0 then
            if v == 0 then point' else linear'
        else
            quadratic'

componentDiff :: Component Coord -> Component Coord -> Component Coord
componentDiff (Component p1 v1 a1) (Component p2 v2 a2) = Component (p1 - p2) (v1 - v2) (a1 - a2)

particleCollision :: Particle Coord -> Particle Coord -> Occurs Time
particleCollision (Vector x1 y1 z1) (Vector x2 y2 z2) =
    let
        xs = componentCollisions x1 x2
        ys = componentCollisions y1 y2
        zs = componentCollisions z1 z2
    in
        xs `mappend` ys `mappend` zs

data Collision a t = Collision (Set.Set (Particle a)) (Occurs t) deriving (Show, Eq)

collision :: Particle Coord -> Particle Coord -> Collision Coord Time
collision p q = Collision (Set.fromList [p, q]) (particleCollision p q)

mergeCollisions :: [Collision Coord Time] -> [Collision Coord Time]
mergeCollisions [] = []
mergeCollisions (c:[]) = c:[]
mergeCollisions (c:c':cs) =
    let
        (Collision p1 t1) = c
        (Collision p2 t2) = c'
        i = Set.intersection p1 p2
        merged = Collision (Set.union p1 p2) t1
    in
        if t1 /= t2 || null i
            then c:mergeCollisions (c':cs)
            else mergeCollisions (merged:cs)

involved :: (Eq a, Ord a) => Collision a t -> Particle a -> Bool
involved (Collision ps _) p = p `Set.member` ps

instance (Eq a, Ord t) => Ord (Collision a t) where
    compare (Collision _ t1) (Collision _ t2) = compare t1 t2

orderedCollisions :: [Particle Coord] -> [Collision Coord Time]
orderedCollisions ps = mergeCollisions $ sort $ filter happens $ collisions ps
    where
        collisions [] = []
        collisions (p:ps) = [collision p q | q <- ps] ++ collisions ps
        happens (Collision _ Never) = False
        happens _ = True
    
annihilate :: [Particle Coord] -> [Particle Coord]
annihilate ps = annihilateUntilNoCollisions ps (orderedCollisions ps)

annihilateUntilNoCollisions :: [Particle Coord] -> [Collision Coord Time] -> [Particle Coord]
annihilateUntilNoCollisions !ps [] = ps
annihilateUntilNoCollisions !ps (c:cs) = annihilateUntilNoCollisions nextps nextcs
    where
        nextps = filter (not . involved c) ps
        nextcs = filter (\(Collision ps _) -> not $ any (involved c) ps) cs
