module Y2019.Day03 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Lib.IO
import Lib.Types

import Data.Maybe (fromJust)
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

-----------------------
-- Type declarations --
-----------------------

type Pos = (Int, Int)

manhattanD :: Pos -> Pos -> Int
manhattanD (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

newtype Wire = Wire [Pos] deriving (Show)
data Segment = Segment Pos Pos deriving (Show)

wireToSegments :: Wire -> [Segment]
wireToSegments = wireToSegments' . un  where
    wireToSegments' (x1 : x2 : xs) = Segment x1 x2 : wireToSegments' (x2 : xs)
    wireToSegments' _              = []

------------
-- Part 1 --
------------

solve1 :: (Wire, Wire) -> Int
solve1 (wire1, wire2) = minimum1 distances  where
    intersections = wireIntersections wire1 wire2
    distances     = fmap (manhattanD (0, 0)) intersections

-- Wires are guaranteed to have 1+ intersections
wireIntersections :: Wire -> Wire -> NonEmpty Pos
wireIntersections wire1 wire2 = fromMaybe err . nonEmpty . filter (/= (0, 0)) $ intersections  where -- Exclude (0, 0)
    segs1         = wireToSegments wire1
    segs2         = wireToSegments wire2
    intersections = concat [ segIntersections s1 s2 | s1 <- segs1, s2 <- segs2 ]
    err           = error "Two wires must have intersection!"

segIntersections :: Segment -> Segment -> [Pos]
segIntersections (Segment (a1, b1) (a2, b2)) (Segment (c1, d1) (c2, d2))
    | a1 == a2 && c1 == c2 && a1 == c1 = map (a1, ) (line b1 b2 d1 d2)
    | b1 == b2 && d1 == d2 && b1 == d1 = map (, b1) (line a1 a2 c1 c2)
    | a1 == a2 && d1 == d2 && between' b1 d1 b2 && between' c1 a1 c2 = [(a1, d1)]
    | b1 == b2 && c1 == c2 && between' a1 c1 a2 && between' d1 b1 d2 = [(c1, b1)]
    | otherwise                        = []  where
    line x1 x2 y1 y2
        | maxX < minY = []
        | maxY < minX = []
        | otherwise   = [max minX minY .. min maxX maxY]      where
        (minX, maxX) = (min x1 x2, max x1 x2)
        (minY, maxY) = (min y1 y2, max y1 y2)

between' x y z = (x <= y && y <= z) || (z <= y && y <= x)

------------
-- Part 2 --
------------

solve2 :: (Wire, Wire) -> Int
solve2 (wire1, wire2) = minimum1 sums  where
    intersections    = wireIntersections wire1 wire2
    followDistances1 = fmap (followDistance wire1) intersections
    followDistances2 = fmap (followDistance wire2) intersections
    sums             = NE.zipWith (+) followDistances1 followDistances2

-- [(Segment, Total length before this segment)]
wireToSegmentLengths :: Wire -> [(Segment, Int)]
wireToSegmentLengths wire = wireToSegments' (un wire) 0  where
    wireToSegments' (x1 : x2 : xs) l = (Segment x1 x2, l) : wireToSegments' (x2 : xs) l'
        where l' = l + manhattanD x1 x2
    wireToSegments' _ _ = []

followDistance :: Wire -> Pos -> Int
followDistance wire pos = dist  where
    segLengths = wireToSegmentLengths wire
    dists      = mapMaybe (\(seg, l) -> (+) l <$> getDist seg pos) segLengths
    dist       = fromJust $ viaNonEmpty head dists
    getDist (Segment (x1, y1) (x2, y2)) (a, b)
        | x1 == x2 && x1 == a && between' y1 b y2 = Just $ abs (b - y1)
        | y1 == y2 && y1 == b && between' x1 a x2 = Just $ abs (a - x1)
        | otherwise                               = Nothing

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    wires <- toTuple <$> (parseWire <<$>> readFileLines "inputs/Y2019/Day03.txt") :: IO (Wire, Wire)
    print $ solve1 wires
    print $ solve2 wires

unsafeRead = fromRight (error "parse error") . readEither
toTuple (x : y : _) = (x, y)

parseWire :: Text -> Wire
parseWire line = Wire $ scanl scanFn (0, 0) textList  where
    textList = words . toText . map (\c -> if c == ',' then ' ' else c) . toString $ line
    scanFn (x, y) text =
        let (dx, dy) = parseRelativeCoord (fromJust $ T.uncons text) in (x + dx, y + dy)

parseRelativeCoord ('R', x) = (unsafeRead x, 0)
parseRelativeCoord ('L', x) = (-(unsafeRead x), 0)
parseRelativeCoord ('U', x) = (0, unsafeRead x)
parseRelativeCoord ('D', x) = (0, -(unsafeRead x))
