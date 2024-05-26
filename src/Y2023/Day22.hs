module Y2023.Day22 (main') where

import Algebra.Graph
import Algebra.Graph.ToGraph (postSet, preSet)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Lib.IO
import Relude
import Relude.Extra.Map

-----------------------
-- Type declarations --
-----------------------

data Brick = Brick
  { x1 :: Int,
    y1 :: Int,
    z1 :: Int,
    x2 :: Int,
    y2 :: Int,
    z2 :: Int
  }
  deriving (Show, Eq, Ord)

data Pos2D = Pos2D Int Int deriving (Show, Eq, Ord)

newtype Footprint = Footprint (Set Pos2D) deriving (Monoid, Semigroup) via (Set Pos2D)

type BrickSupportGraph = Graph BrickNode

-- Compare with ID for performance
data BrickNode = BrickNode
  { brickId :: Int,
    brick :: Brick
  }

instance Eq BrickNode where
  (==) = (==) `on` brickId

instance Ord BrickNode where
  compare = compare `on` brickId

generateBrickNodes :: [Brick] -> [BrickNode]
generateBrickNodes = zipWith BrickNode [0 ..]

------------
-- Part 1 --
------------

solve1 :: [Brick] -> Int
solve1 bricks = length disintegratedBricks
  where
    stableBricks = generateBrickNodes $ fallBricks bricks
    supportGraph = buildSupportGraph stableBricks
    disintegratedBricks = filter (canSafelyDisintegrated supportGraph) stableBricks

fallBricks :: [Brick] -> [Brick]
fallBricks bricks = join . elems . foldl' fallBrickOnMap mempty $ orderedBricks
  where
    orderedBricks = sortOn z1 (orderBrickZ <$> bricks)

    orderBrickZ :: Brick -> Brick
    orderBrickZ b = if z1 b <= z2 b then b else Brick (x2 b) (y2 b) (z2 b) (x1 b) (y1 b) (z1 b)

    fallBrickOnMap :: Map Int [Brick] -> Brick -> Map Int [Brick]
    fallBrickOnMap zMap brick = Map.insertWith (<>) (z2 brick') [brick'] zMap
      where
        brick' = fallBrick zMap brick

    fallBrick :: Map Int [Brick] -> Brick -> Brick
    fallBrick zMap brick = brick {z1 = z1 brick - dz, z2 = z2 brick - dz}
      where
        z1' = findBottom (z1 brick)
        dz = z1 brick - z1'

        findBottom 1 = 1
        findBottom z = case Map.lookup (z - 1) zMap of
          Nothing -> findBottom (z - 1)
          Just floorBricks ->
            let floorFootprint = foldMap getFootprint floorBricks
             in if footprintIntersects (getFootprint brick) floorFootprint
                  then z
                  else findBottom (z - 1)

getFootprint :: Brick -> Footprint
getFootprint (Brick x1 y1 _ x2 y2 _) = Footprint . fromList $ Pos2D <$> [x1 .. x2] <*> [y1 .. y2]

footprintIntersects :: Footprint -> Footprint -> Bool
footprintIntersects (Footprint s1) (Footprint s2) = not . null $ s1 `Set.intersection` s2

-- Build edges from above to below
buildSupportGraph :: [BrickNode] -> BrickSupportGraph
buildSupportGraph brickNodes = foldl' addEdges mempty brickNodes
  where
    addEdges :: BrickSupportGraph -> BrickNode -> BrickSupportGraph
    addEdges graph brickNode = graph <> (vertex brickNode `connect` foldMap vertex (findSupportBricks brickNode))

    findSupportBricks :: BrickNode -> [BrickNode]
    findSupportBricks brickNode = filter ((supports `on` brick) brickNode) brickNodes

    supports :: Brick -> Brick -> Bool
    supports brick1 brick2 = z1 brick1 == (z2 brick2 + 1) && footprintIntersects (getFootprint brick1) (getFootprint brick2)

canSafelyDisintegrated :: BrickSupportGraph -> BrickNode -> Bool
canSafelyDisintegrated graph brick = all isSupportedByMoreThanOneBrick aboveBlocks
  where
    aboveBlocks = preSet brick graph
    isSupportedByMoreThanOneBrick b = length (postSet b graph) > 1

------------
-- Part 2 --
------------

solve2 :: [Brick] -> Int
solve2 bricks = sum fallingBrickCounts
  where
    stableBricks = generateBrickNodes $ fallBricks bricks
    supportGraph = buildSupportGraph stableBricks
    fallingBrickCounts = countFallingBricksWhenDisintegrated supportGraph <$> stableBricks

type BrickPriorityQueue = Set (Int, BrickNode)

countFallingBricksWhenDisintegrated :: BrickSupportGraph -> BrickNode -> Int
countFallingBricksWhenDisintegrated graph bn = length (go (one (0, bn)) (one bn)) - 1
  where
    go :: BrickPriorityQueue -> Set BrickNode -> Set BrickNode
    go pq fallingBricks = case Set.minView pq of
      Nothing -> fallingBricks
      Just ((_, brickNode), pq') -> go pq'' fallingBricks'
        where
          fallingAboveBricks = Set.filter (\b -> postSet b graph `Set.isSubsetOf` fallingBricks) aboveBricks
          aboveBricks = preSet brickNode graph

          pq'' = foldl' (\pq b -> Set.insert (z2 (brick b), b) pq) pq' fallingAboveBricks
          fallingBricks' = fallingBricks <> fallingAboveBricks

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  bricks <- parseBrick <<$>> readFileLines "inputs/Y2023/Day22.txt" :: IO [Brick]
  print $ solve1 bricks
  print $ solve2 bricks

parseBrick :: Text -> Brick
parseBrick line = Brick {..}
  where
    [x1, y1, z1, x2, y2, z2] = readInt <$> T.split (liftA2 (||) (== ',') (== '~')) line
