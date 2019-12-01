import Control.Monad
import Data.Function
import Data.List
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS (scc)

type Point = (Int, Int, Int, Int)
type Constellation = Gr Point ()

manhattanD (x1, y1, z1, t1) (x2, y2, z2, t2) = (abs $ x1 - x2) + (abs $ y1 - y2) + (abs $ z1 - z2) + (abs $ t1 - t2)

pointsToNodes :: [Point] -> [LNode Point]
pointsToNodes = zip [0..]

makeConstellation :: [Point] -> Constellation
makeConstellation points = mkGraph nodes edges where
    nodes = pointsToNodes points
    edges = [ (i, j, ()) | (i, ip) <- nodes, (j, jp) <- nodes, i /= j, ip `connects` jp ]
    connects p1 p2 = manhattanD p1 p2 <= 3

solve1 :: [Point] -> Int
solve1 points = length $ scc $ makeConstellation points

getPoint :: String -> Point
getPoint line = (read $ ints !! 0, read $ ints !! 1, read $ ints !! 2, read $ ints !! 3) where
    ints = words $ map (\c -> if c == ',' then ' ' else c) line

main' = do
    points <- map getPoint <$> replicateM 1257 getLine
    putStrLn $ show $ solve1 points
