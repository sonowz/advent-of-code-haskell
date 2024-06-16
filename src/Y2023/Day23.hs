module Y2023.Day23 (main') where

import Algebra.Graph.Label (Distance, distance, getDistance, getFinite, unsafeFinite)
import Algebra.Graph.Labelled.AdjacencyMap
import Data.Map (assocs, (!))
import Data.Set qualified as Set
import Lib.IO
import Relude hiding (transpose)
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Unsafe qualified as Unsafe

-----------------------
-- Type declarations --
-----------------------

type Trails = Map Pos Tile

data Tile = TPath | TForest | TSlope Slope deriving (Show, Eq, Ord)

data Slope = SlopeUp | SlopeDown | SlopeLeft | SlopeRight deriving (Show, Eq, Ord)

isPath :: Trails -> Pos -> Bool
isPath trails pos = trails !? pos == Just TPath

isSlope :: Trails -> Pos -> Bool
isSlope trails pos = case trails !? pos of
  Just (TSlope _) -> True
  _ -> False

data PathSegment = PathSegment
  { ends :: Ends,
    segmentLength :: Int
  }

data Ends = Ends Pos Pos deriving (Show, Eq, Ord)

hasEnd :: PathSegment -> Pos -> Bool
hasEnd segment pos = let Ends e1 e2 = ends segment in e1 == pos || e2 == pos

data CompressedTrailGraph = CompressedTrailGraph
  { start :: Ends,
    end :: Ends,
    graph :: AdjacencyMap (Distance Int) Ends
  }

data Pos = Pos Int Int deriving (Show, Eq, Ord)

------------
-- Part 1 --
------------

solve1 :: Trails -> Int
solve1 trails = fromDistance longestPath
  where
    pathSegments = buildPathSegments trails
    compressedTrailGraph = buildCompressedTrailGraph trails pathSegments
    longestPath = findLongestPath compressedTrailGraph :: Distance Int

buildPathSegments :: Trails -> [PathSegment]
buildPathSegments trails = go mempty (keys trails)
  where
    go :: Set Pos -> [Pos] -> [PathSegment]
    go visited [] = []
    go visited (pos : rest) | pos `Set.member` visited = go visited rest
    go visited (pos : rest) | not (isPath trails pos) = go (Set.insert pos visited) rest
    go visited (pos : rest) = segments
      where
        paths = floodFill trails pos
        segment = PathSegment (findPathEnds trails paths) (length paths)
        segments = segment : go (visited <> paths) rest

buildCompressedTrailGraph :: Trails -> [PathSegment] -> CompressedTrailGraph
buildCompressedTrailGraph trails segments = CompressedTrailGraph start end graph
  where
    findSegmentByPos :: Pos -> Maybe PathSegment
    findSegmentByPos pos = find (`hasEnd` pos) segments

    connectBySlope :: Pos -> AdjacencyMap (Distance Int) Ends
    connectBySlope slopePos = maybeToMonoid $ do
      (startPos, endPos) <- getStartAndEndPos
      startSegment <- findSegmentByPos startPos
      endSegment <- findSegmentByPos endPos
      Just $ ends startSegment -< toDistance (segmentLength startSegment + 1) >- ends endSegment
      where
        getStartAndEndPos :: Maybe (Pos, Pos)
        getStartAndEndPos = do
          let (Pos slopeX slopeY) = slopePos
          slope <- trails !? slopePos
          case slope of
            TSlope SlopeUp -> Just (Pos slopeX (slopeY + 1), Pos slopeX (slopeY - 1))
            TSlope SlopeDown -> Just (Pos slopeX (slopeY - 1), Pos slopeX (slopeY + 1))
            TSlope SlopeRight -> Just (Pos (slopeX - 1) slopeY, Pos (slopeX + 1) slopeY)
            TSlope SlopeLeft -> Just (Pos (slopeX + 1) slopeY, Pos (slopeX - 1) slopeY)
            _ -> Nothing

    slopeTiles :: [Pos]
    slopeTiles = filter (isSlope trails) (keys trails)

    graphWithoutEnd :: AdjacencyMap (Distance Int) Ends
    graphWithoutEnd = foldMap connectBySlope slopeTiles

    startSegment :: PathSegment
    startSegment = Unsafe.fromJust $ do
      p <- find (\p@(Pos _ y) -> y == 0 && isPath trails p) (keys trails)
      find (`hasEnd` p) segments

    endSegment :: PathSegment
    endSegment =
      let maxY = maximum1 . Unsafe.fromJust . nonEmpty $ ((\(Pos _ y) -> y) <$> keys trails)
       in Unsafe.fromJust $ do
            p <- find (\p@(Pos _ y) -> y == maxY && isPath trails p) (keys trails)
            find (`hasEnd` p) segments

    start = ends startSegment
    end = Ends endPos endPos
      where
        endPos = let Ends (Pos x1 y1) (Pos x2 y2) = ends endSegment in if y1 > y2 then Pos x1 y1 else Pos x2 y2
    graph = graphWithoutEnd <> endEdge
      where
        endEdge = ends endSegment -< toDistance (segmentLength endSegment - 1) >- end

findLongestPath :: CompressedTrailGraph -> Distance Int
findLongestPath (CompressedTrailGraph start end graph) = Unsafe.fromJust $ go mempty start
  where
    adjMap = adjacencyMap graph
    neighbors v = assocs (adjMap ! v)
    go :: Set Ends -> Ends -> Maybe (Distance Int)
    go _ ends | ends == end = Just (toDistance 0)
    go visited ends | ends `Set.member` visited = Nothing
    go visited ends = do
      let visited' = Set.insert ends visited
      pathLengths <- nonEmpty $ mapMaybe (\(v, d) -> go visited' v <&> (+ d)) (neighbors ends)
      Just $ maximum1 pathLengths

findPathEnds :: Trails -> Set Pos -> Ends
findPathEnds trails paths =
  let connectedCount p = length $ filter (isPath trails) (adjacent p)
      ends = filter (\p -> connectedCount p == 1) (toList paths)
   in case (ends, paths) of
        ([a, b], _) -> Ends a b
        ([], [a]) -> Ends a a
        _ -> error "Path is not linear"

floodFill :: Trails -> Pos -> Set Pos
floodFill trails = go mempty
  where
    go :: Set Pos -> Pos -> Set Pos
    go visited pos | isPath trails pos = foldl' go (Set.insert pos visited) (filter (`notMember` visited) (adjacent pos))
    go visited _ = visited

adjacent :: Pos -> [Pos]
adjacent (Pos x y) = [Pos (x - 1) y, Pos (x + 1) y, Pos x (y - 1), Pos x (y + 1)]

toDistance :: Int -> Distance Int
toDistance = distance . unsafeFinite

fromDistance :: Distance Int -> Int
fromDistance = Unsafe.fromJust . getFinite . getDistance

------------
-- Part 2 --
------------

solve2 :: Trails -> Int
solve2 trails = fromDistance longestPath
  where
    pathSegments = buildPathSegments trails
    compressedTrailGraph = convertToBidirectional $ buildCompressedTrailGraph trails pathSegments
    longestPath = findLongestPath compressedTrailGraph :: Distance Int

convertToBidirectional :: CompressedTrailGraph -> CompressedTrailGraph
convertToBidirectional trailGraph = trailGraph {graph = bidirectionalGraph}
  where
    unidirectionalGraph = graph trailGraph
    bidirectionalGraph = unidirectionalGraph <> transpose unidirectionalGraph

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  trails <- parseTrails <$> readFileLines "inputs/Y2023/Day23.txt" :: IO Trails
  print $ solve1 trails
  print $ solve2 trails

parseTrails :: [Text] -> Trails
parseTrails lines = fromList $ do
  (y, line) <- zip [0 ..] lines
  (x, tile) <- zip [0 ..] (toString line)
  pure (Pos x y, parseTile tile)

parseTile :: Char -> Tile
parseTile '.' = TPath
parseTile '#' = TForest
parseTile '^' = TSlope SlopeUp
parseTile 'v' = TSlope SlopeDown
parseTile '<' = TSlope SlopeLeft
parseTile '>' = TSlope SlopeRight
parseTile _ = error "Invalid tile"
