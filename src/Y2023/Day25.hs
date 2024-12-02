module Y2023.Day25 (main') where

import Algebra.Graph.Label (Capacity)
import Algebra.Graph.Labelled.AdjacencyMap
import Algebra.Graph.ToGraph (reachable)
import Data.Map ((!))
import Data.Text qualified as T
import Lib.Graph (Flow (getCap, getFlow), edmondsKarp)
import Lib.IO
import Relude
import Relude.Unsafe qualified as Unsafe

-----------------------
-- Type declarations --
-----------------------

data Wiring = Wiring Text [Text]

type WiringGraph = AdjacencyMap (Capacity Int) Text

------------
-- Part 1 --
------------

solve1 :: [Wiring] -> Int
solve1 wiringDiagram = length components1 * length components2
  where
    wiringGraph = buildWiringGraph wiringDiagram
    components = vertexList wiringGraph
    cutCandidates = [(v, w) | v <- components, w <- components, v < w]
    minCutResult :: [(Int, Set Text, Set Text)]
    minCutResult = uncurry (findMinCut wiringGraph) <$> cutCandidates
    (_, components1, components2) = Unsafe.fromJust . find (\(minCut, _, _) -> minCut == 3) $ minCutResult

buildWiringGraph :: [Wiring] -> WiringGraph
buildWiringGraph wiringDiagram = fold $ fold edges
  where
    edges :: [[WiringGraph]]
    edges = [[v -< 1 >- u, u -< 1 >- v] | (Wiring v us) <- wiringDiagram, u <- us]

findMinCut :: WiringGraph -> Text -> Text -> (Int, Set Text, Set Text)
findMinCut graph source sink = (maxFlow, srcSide, sinkSide)
  where
    flowGraph = edmondsKarp graph source sink
    maxFlow = sum $ (\v -> getFlow (adjacencyMap flowGraph ! v ! sink)) <$> toList (preSet sink flowGraph)
    nonSaturatedGraph = emap (\e -> if 0 <= getFlow e && getFlow e < getCap e then e else mempty) flowGraph
    srcSide = fromList $ reachable source nonSaturatedGraph
    sinkSide = fromList $ reachable sink nonSaturatedGraph

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  wiringDiagram <- parseWiring <<$>> readFileLines "inputs/Y2023/Day25.txt" :: IO [Wiring]
  print $ solve1 wiringDiagram

parseWiring :: Text -> Wiring
parseWiring line = Wiring component components
  where
    dropped = T.filter (/= ':') line
    (component : components) = T.splitOn " " dropped
