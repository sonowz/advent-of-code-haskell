module Y2023.Day17 (main') where

import Lib.IO
import Lib.Types
import Lib.Vector2D (Pos2D)
import Lib.Graph (dijkstra, bellmanFord, shortestPath)
import qualified Lib.Parser as P
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Data.Char (digitToInt)
import qualified Relude.Unsafe as Unsafe
import Data.Map ((!))
import Algebra.Graph.ToGraph (ToGraph(toAdjacencyMap))
import Algebra.Graph.Labelled.AdjacencyMap
import Algebra.Graph.Label

-----------------------
-- Type declarations --
-----------------------

type LavaMap = Map Pos HeatLoss

newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord, Pos2D) via (Int, Int)

newtype HeatLoss = HeatLoss Int
  deriving (Show, Eq, Num, Ord) via Int
  deriving (Monoid, Semigroup) via Sum Int

type LavaMapGraph = AdjacencyMap HeatLoss LavaNode

data LavaNode = LavaNode {
  pos :: Pos,
  lavaFlow :: LavaFlow,
  index :: PosIndex
} deriving (Show, Eq, Ord)

data PosIndex = PosMin | PosMid | PosMax deriving (Show, Eq, Ord)
data LavaFlow = Horizontal | Vertical | Both deriving (Show, Eq, Ord)

------------
-- Part 1 --
------------

solve1 :: LavaMap -> HeatLoss
solve1 lavaMap = minHeatLoss where
  startNode = LavaNode (Pos (0, 0)) Both PosMin
  endNodes = filter (\node -> pos node == Pos (maxPos lavaMap)) (vertexList graph)
  graph = constructGraph lavaMap
      <> (startNode -<lavaMap ! Pos (1, 0)>- LavaNode (Pos (1, 0)) Horizontal PosMid)
      <> (startNode -<lavaMap ! Pos (0, 1)>- LavaNode (Pos (0, 1)) Vertical PosMid)
  graph' :: AdjacencyMap (Distance HeatLoss) LavaNode
  graph' = emap (distance . unsafeFinite) graph
  dijkstraResult = minimum1 . Unsafe.fromJust . nonEmpty $ (dijkstra graph' startNode !) <$> endNodes
  minHeatLoss = Unsafe.fromJust . getFinite . getDistance $ dijkstraResult

constructGraph :: LavaMap -> LavaMapGraph
constructGraph lavaMap = graph where
  graph = horizontalGraph <> verticalGraph <> horizontalTurnGraph <> verticalTurnGraph

  horizontalGraph :: LavaMapGraph
  horizontalGraph = foldMap go ([0..maxY] :: [Int]) where
    (maxX, maxY) = maxPos lavaMap
    go :: Int -> LavaMapGraph
    go y = fold $ do
      (x1, x2, x3) <- slide3 [-2..maxX+2]
      let n1 = LavaNode (Pos (x1, y)) Horizontal PosMin
          n2 = LavaNode (Pos (x2, y)) Horizontal PosMid
          n3 = LavaNode (Pos (x3, y)) Horizontal PosMax
          w1 = lavaMap !? Pos (x1, y) ?: zero
          w2 = lavaMap !? Pos (x2, y) ?: zero
          w3 = lavaMap !? Pos (x3, y) ?: zero
      [n1 -<w2>- n2, n2 -<w3>- n3, n3 -<w2>- n2, n2 -<w1>- n1]

  verticalGraph :: LavaMapGraph
  verticalGraph = foldMap go ([0..maxX] :: [Int]) where
    (maxX, maxY) = maxPos lavaMap
    go :: Int -> LavaMapGraph
    go x = fold $ do
      (y1, y2, y3) <- slide3 [-2..maxY+2]
      let n1 = LavaNode (Pos (x, y1)) Vertical PosMin
          n2 = LavaNode (Pos (x, y2)) Vertical PosMid
          n3 = LavaNode (Pos (x, y3)) Vertical PosMax
          w1 = lavaMap !? Pos (x, y1) ?: zero
          w2 = lavaMap !? Pos (x, y2) ?: zero
          w3 = lavaMap !? Pos (x, y3) ?: zero
      [n1 -<w2>- n2, n2 -<w3>- n3, n3 -<w2>- n2, n2 -<w1>- n1]

  horizontalTurnGraph :: LavaMapGraph
  horizontalTurnGraph = foldMap go (keys lavaMap) where
    go :: Pos -> LavaMapGraph
    go (Pos (x, y)) = upGraph <> downGraph where
      posNodes = LavaNode (Pos (x, y)) Horizontal <$> [PosMin, PosMid, PosMax] :: [LavaNode]
      upGraph = fold $ case lavaMap !? Pos (x, y - 1) of
        Just w -> (\n -> n -<w>- LavaNode (Pos (x, y-1)) Vertical PosMax) <$> posNodes
        Nothing -> mempty
      downGraph = fold $ case lavaMap !? Pos (x, y + 1) of
        Just w -> (\n -> n -<w>- LavaNode (Pos (x, y+1)) Vertical PosMin) <$> posNodes
        Nothing -> mempty

  verticalTurnGraph :: LavaMapGraph
  verticalTurnGraph = foldMap go (keys lavaMap) where
    go :: Pos -> LavaMapGraph
    go (Pos (x, y)) = leftGraph <> rightGraph where
      posNodes = LavaNode (Pos (x, y)) Vertical <$> [PosMin, PosMid, PosMax] :: [LavaNode]
      leftGraph = fold $ case lavaMap !? Pos (x - 1, y) of
        Just w -> (\n -> n -<w>- LavaNode (Pos (x-1, y)) Horizontal PosMax) <$> posNodes
        Nothing -> mempty
      rightGraph = fold $ case lavaMap !? Pos (x + 1, y) of
        Just w -> (\n -> n -<w>- LavaNode (Pos (x+1, y)) Horizontal PosMin) <$> posNodes
        Nothing -> mempty


maxPos :: LavaMap -> (Int, Int)
maxPos = un . maximum1 . Unsafe.fromJust . nonEmpty . keys

slide3 :: [a] -> [(a, a, a)]
slide3 (x:y:z:xs) = (x, y, z) : slide3 (y:z:xs)
slide3 _ = []

------------
-- Part 2 --
------------

solve2 :: LavaMap -> HeatLoss
solve2 lavaMap = minHeatLoss where
  startNode = UltraLavaNode (Pos (-1, -1)) UStart
  endNode = UltraLavaNode (Pos (-1, -1)) UEnd
  (maxX, maxY) = maxPos lavaMap 

  connectEnd :: UltraLavaFlow -> (Int -> Pos) -> UltraLavaMapGraph
  connectEnd flow straight = fold graphs where
    graphs = drop 4 . snd $ mapAccumL (\acc i -> (acc <> (lavaMap ! straight i), UltraLavaNode (straight i) flow -<acc>- endNode)) (HeatLoss 0) [0..10]

  graph = constructUltraGraph lavaMap
    <> (startNode -<HeatLoss 1>- UltraLavaNode (Pos (0, 0)) URight)
    <> (startNode -<HeatLoss 1>- UltraLavaNode (Pos (0, 0)) UDown)
    <> connectEnd URight (\i -> Pos (maxX-i, maxY))
    <> connectEnd UDown (\i -> Pos (maxX, maxY-i))
  graph' :: AdjacencyMap (Distance Int) UltraLavaNode
  graph' =  emap (distance . unsafeFinite . un) graph
  dijkstraResult = dijkstra graph' startNode ! endNode
  minHeatLoss = HeatLoss $ (Unsafe.fromJust . getFinite . getDistance $ dijkstraResult) - 1

type UltraLavaMapGraph = AdjacencyMap HeatLoss UltraLavaNode

data UltraLavaNode = UltraLavaNode {
  uPos :: Pos,
  uLavaFlow :: UltraLavaFlow
} deriving (Show, Eq, Ord)

data UltraLavaFlow = UUp | UDown | ULeft | URight | UStart | UEnd deriving (Show, Eq, Ord)

constructUltraGraph :: LavaMap -> UltraLavaMapGraph
constructUltraGraph lavaMap = graph where
  graph = rightGraph <> leftGraph <> upGraph <> downGraph

  makeEdge :: UltraLavaNode -> UltraLavaFlow -> (Pos, HeatLoss) -> UltraLavaMapGraph
  makeEdge node flow (pos, baseWeight) = case lavaMap !? pos of
    Just w -> node -<(baseWeight <> w)>- UltraLavaNode pos flow
    Nothing -> mempty

  getEdges4to10 :: UltraLavaNode -> UltraLavaFlow -> (Int -> Pos) -> [UltraLavaMapGraph]
  getEdges4to10 node flow straight = edges where
    baseWeights :: [(Pos, HeatLoss)]
    baseWeights = filter ((> HeatLoss 0) . snd) . drop 4 . scanl (\(_, acc) pos -> (pos, acc <> (lavaMap !? pos ?: -9999))) (Pos (-1, -1), mempty) $ fmap straight [1..10]
    edges = (\(pos, w) -> node -<w>- UltraLavaNode pos flow) <$> baseWeights

  rightGraph :: UltraLavaMapGraph
  rightGraph = foldMap go (keys lavaMap) where
    go (Pos (x,y)) = upEdges <> downEdges where
      node = UltraLavaNode (Pos (x,y)) URight
      upEdges = fold $ getEdges4to10 node UUp (\i -> Pos (x+i, y))
      downEdges = fold $ getEdges4to10 node UDown (\i -> Pos (x+i, y))

  leftGraph :: UltraLavaMapGraph
  leftGraph = foldMap go (keys lavaMap) where
    go (Pos (x,y)) = upEdges <> downEdges where
      node = UltraLavaNode (Pos (x,y)) ULeft
      upEdges = fold $ getEdges4to10 node UUp (\i -> Pos (x-i, y))
      downEdges = fold $ getEdges4to10 node UDown (\i -> Pos (x-i, y))

  upGraph :: UltraLavaMapGraph
  upGraph = foldMap go (keys lavaMap) where
    go (Pos (x,y)) = rightEdges <> leftEdges where
      node = UltraLavaNode (Pos (x,y)) UUp
      rightEdges = fold $ getEdges4to10 node URight (\i -> Pos (x, y-i))
      leftEdges = fold $ getEdges4to10 node ULeft (\i -> Pos (x, y-i))

  downGraph :: UltraLavaMapGraph
  downGraph = foldMap go (keys lavaMap) where
    go (Pos (x,y)) = rightEdges <> leftEdges where
      node = UltraLavaNode (Pos (x,y)) UDown
      rightEdges = fold $ getEdges4to10 node URight (\i -> Pos (x, y+i))
      leftEdges = fold $ getEdges4to10 node ULeft (\i -> Pos (x, y+i))

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  lavaMap <- parseLavaMap <$> readFileLines "inputs/Y2023/Day17.txt" :: IO LavaMap
  print $ solve1 lavaMap
  print $ solve2 lavaMap

parseLavaMap :: [Text] -> LavaMap
parseLavaMap = readMap [0..] [0..] (HeatLoss . digitToInt)
