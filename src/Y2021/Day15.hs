module Y2021.Day15 where

import Algebra.Graph.Label (Distance, distance, finite, getDistance, getFinite)
import qualified Algebra.Graph.Labelled.AdjacencyMap as A
import Data.Vector (Vector)
import qualified Data.Vector as V
import Lib.Graph (dijkstra)
import Lib.IO
import Lib.Types
import Lib.Vector2D
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple

-----------------------
-- Type declarations --
-----------------------

newtype RiskLevel = RiskLevel Int deriving (Show, Eq, Ord, Enum, Num, Real, Integral) via Int
type CaveMap = Vector (Vector RiskLevel)
newtype Pos = Pos (Int, Int) deriving (Pos2D, Ord, Eq, Show) via (Int, Int)

instance Num Pos where
    Pos (x1, y1) + Pos (x2, y2) = Pos (x1 + x2, y1 + y2)
    Pos (x1, y1) - Pos (x2, y2) = Pos (x1 - x2, y1 - y2)
    Pos (x1, y1) * Pos (x2, y2) = Pos (x1 * x2, y1 * y2)
    abs (Pos (x, y)) = Pos (abs x, abs y)
    signum (Pos (x, y)) = Pos (signum x, signum y)
    fromInteger x = Pos (fromInteger x, fromInteger x)

------------
-- Part 1 --
------------

solve1 :: CaveMap -> RiskLevel
solve1 caveMap = maybe 0 fromDistance answer  where
    !graph       = caveMapToGraph caveMap
    !distanceMap = dijkstra graph (Pos (0, 0)) :: Map Pos (Distance RiskLevel)
    destination  = Pos (size2D caveMap) - Pos (1, 1)
    answer       = lookup destination distanceMap :: Maybe (Distance RiskLevel)

caveMapToGraph :: CaveMap -> A.AdjacencyMap (Distance RiskLevel) Pos
caveMapToGraph caveMap = A.edges (join edgeList)  where
    edgeList = imapMaybeL2D (\pos _ -> Just $ makeEdges pos) caveMap
    makeEdges :: Pos -> [(Distance RiskLevel, Pos, Pos)]
    makeEdges pos = catMaybes $ zipWith zipFn riskLevels (adjacent pos)      where
        riskLevels = (toDistance <=< at caveMap) <$> adjacent pos :: [Maybe (Distance RiskLevel)]
        zipFn (Just riskLevel) adjPos = Just (riskLevel, pos, adjPos)
        zipFn Nothing          _      = Nothing

adjacent :: Pos -> [Pos]
adjacent (Pos (x, y)) = [Pos (x - 1, y), Pos (x + 1, y), Pos (x, y - 1), Pos (x, y + 1)]

toDistance :: RiskLevel -> Maybe (Distance RiskLevel)
toDistance = fmap distance . finite

fromDistance :: Distance RiskLevel -> RiskLevel
fromDistance = fromMaybe (error "Negative number!") . getFinite . getDistance

------------
-- Part 2 --
------------

-- This solution takes about 5 seconds
-- where 'dijkstra' function takes majority of time
solve2 :: CaveMap -> RiskLevel
solve2 = solve1 . enlargeCaveMap

enlargeCaveMap :: CaveMap -> CaveMap
enlargeCaveMap caveMap = foldl1' concatVertical $ foldl1' concatHorizontal <$> enlargedRaw'  where
    -- [[0,1,2,3,4],[1,2,3,4,5],[2,3,4,5,6],[3,4,5,6,7],[4,5,6,7,8]]
    offsetMatrix = take 5 $ iterate (fmap (+ 1)) [0 .. 4] :: [[RiskLevel]]
    enlargedRaw  = (\ofs -> imap2D (plusFn ofs) caveMap) <<$>> offsetMatrix :: [[CaveMap]]
    plusFn offset = \(_ :: Pos) x -> let x' = x + offset in if x' > 9 then x' - 9 else x'
    enlargedRaw' =
        fromJust $ traverse nonEmpty enlargedRaw >>= nonEmpty :: NonEmpty (NonEmpty CaveMap)
    fromJust = fromMaybe (error "Impossible!")

concatHorizontal :: CaveMap -> CaveMap -> CaveMap
concatHorizontal = V.zipWith (<>)

concatVertical :: CaveMap -> CaveMap -> CaveMap
concatVertical = (<>)

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    caveMap <- parseCaveMap <$> readFileLines "inputs/Y2021/Day15.txt" :: IO CaveMap
    print $ solve1 caveMap
    print $ solve2 caveMap

parseCaveMap :: [Text] -> CaveMap
parseCaveMap lines = fromList $ parseLine <$> lines  where
    parseLine :: Text -> Vector RiskLevel
    parseLine line = fromList $ fmap (RiskLevel . readInt . toText . toList) (toString line)
    toList = one :: a -> [a]
