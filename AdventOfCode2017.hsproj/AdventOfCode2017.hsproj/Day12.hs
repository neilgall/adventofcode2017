module Day12 where
  
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- Parser

type Graph = Map.Map Int (Set.Set Int)

parseInput :: GenParser Char st Graph
parseInput = fmap Map.fromList $ many parseEntry

parseEntry :: GenParser Char st (Int, Set.Set Int)
parseEntry = do
  key <- integer
  spaces
  string "<->"
  spaces
  values <- integer `sepBy` (char ',' >> spaces)
  optional endOfLine
  pure (key, Set.fromList values)

integer :: GenParser Char st Int
integer = fmap read (many digit)  

-- Part 1

connections :: Graph -> Int -> Set.Set Int
connections graph from =
  let
    immediate = Map.findWithDefault Set.empty from graph
    remaining = Map.delete from graph
    followed = Set.toList $ Set.map (connections remaining) immediate
  in
    Set.union immediate (Set.unions followed)
    
groups :: Graph -> Set.Set (Set.Set Int)
groups graph =
  let
    noMoreGroups = null (Map.keys graph)
    firstGroup = connections graph (head $ Map.keys graph)
    remainingGraph = Set.foldr' Map.delete graph firstGroup
  in
    if noMoreGroups then Set.empty
    else Set.insert firstGroup (groups remainingGraph)
