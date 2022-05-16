module Y2021.Day25 where

import Data.Vector (Vector)
import Lib.IO
import Lib.Types
import Lib.Vector2D
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import qualified Text.Show as S

-----------------------
-- Type declarations --
-----------------------

data SeaCucumber = SeaCuRight | SeaCuDown deriving (Eq)
data Tile = TEmpty | TSC SeaCucumber deriving (Eq)
type RegionMap = Vector (Vector Tile)
type Pos = (Int, Int)

add :: (Int, Int) -> Pos -> Pos -> Pos
add (maxX, maxY) (x1, y1) (x2, y2) = ((x1 + x2) `mod` maxX, (y1 + y2) `mod` maxY)

instance S.Show Tile where
    show TEmpty           = "."
    show (TSC SeaCuRight) = ">"
    show (TSC SeaCuDown ) = "v"

------------
-- Part 1 --
------------

solve1 :: RegionMap -> Int
solve1 regionMap = noMoves + 1  where
    noMoves   = length $ takeWhile (not . uncurry (==)) $ zip mapStates (drop 1 mapStates)
    mapStates = iterate step regionMap

step :: RegionMap -> RegionMap
step m = moveDown . moveRight $ m  where
    moveRight map =
        foldl' (\m p -> set (set m p TEmpty) (right p) (TSC SeaCuRight)) map (moveRightPos map)
    moveRightPos map = filter (\p -> isSCRight map p && isEmpty map (right p)) allPos
    right = add mapSize (0, 1)
    moveDown map =
        foldl' (\m p -> set (set m p TEmpty) (down p) (TSC SeaCuDown)) map (moveDownPos map)
    moveDownPos map = filter (\p -> isSCDown map p && isEmpty map (down p)) allPos
    down = add mapSize (1, 0)

    isSCRight m p = m `unsafeAt` p == TSC SeaCuRight
    isSCDown m p = m `unsafeAt` p == TSC SeaCuDown
    isEmpty m p = m `unsafeAt` p == TEmpty
    mapSize = size2D m :: (Int, Int)
    allPos  = [ (x, y) | x <- [0 .. (fst mapSize - 1)], y <- [0 .. (snd mapSize - 1)] ] :: [Pos]

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    regionMap <- parseRegionMap <$> readFileLines "inputs/Y2021/Day25.txt" :: IO RegionMap
    print $ solve1 regionMap

parseRegionMap :: [Text] -> RegionMap
parseRegionMap lines = fromList $ parseLine <$> lines  where
    parseLine :: Text -> Vector Tile
    parseLine line = fromList $ readTile <$> toString line
    readTile :: Char -> Tile
    readTile '.' = TEmpty
    readTile '>' = TSC SeaCuRight
    readTile 'v' = TSC SeaCuDown
    readTile _   = error "parse error!"
