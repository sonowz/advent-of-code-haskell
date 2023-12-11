module Y2023.Day11 (main') where

import Data.Map.Strict (mapKeys)
import Data.Map.Strict qualified as M
import Lib.IO
import Lib.Parser qualified as P
import Lib.Types
import Lib.Vector2D (Pos2D)
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe qualified as Unsafe

-----------------------
-- Type declarations --
-----------------------

type Image = Map Pos Object

data Object = Empty | Galaxy deriving (Show, Eq)

newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord, Pos2D) via (Int, Int)

data Rect = Rect
  { minX :: Int,
    maxX :: Int,
    minY :: Int,
    maxY :: Int
  }
  deriving (Show, Eq)

(!.) :: Image -> Pos -> Object
image !. pos = image !? pos ?: Empty

------------
-- Part 1 --
------------

solve1 :: Image -> Int
solve1 image = sum dists
  where
    expandedImage = expandSpace 1 image
    galaxies = keys . M.filter (== Galaxy) $ expandedImage :: [Pos]
    dists = [manhattanDistance p1 p2 | p1 <- galaxies, p2 <- galaxies, p1 < p2]

makeSparse :: Image -> Image
makeSparse = M.filter (/= Empty)

expandSpace :: Int -> Image -> Image
expandSpace spaceMultiplier (makeSparse -> image) = mapKeys expandPoint image
  where
    maxX = maximum1 . Unsafe.fromJust . nonEmpty . fmap (\(Pos (x, y)) -> x) . keys $ image
    maxY = maximum1 . Unsafe.fromJust . nonEmpty . fmap (\(Pos (x, y)) -> y) . keys $ image
    boundingBox = Rect 0 maxX 0 maxY
    emptyRowIndices = getEmptyRowIndices boundingBox image
    emptyColumnIndices = getEmptyColumnIndices boundingBox image
    expandPoint :: Pos -> Pos
    expandPoint (Pos (x, y)) = Pos (x + expandX, y + expandY)
      where
        expandX = spaceMultiplier * count (< x) emptyColumnIndices
        expandY = spaceMultiplier * count (< y) emptyRowIndices

getEmptyRowIndices :: Rect -> Image -> [Int]
getEmptyRowIndices bound image = emptyRows
  where
    rows = [minY bound .. maxY bound] :: [Int]
    columns = [minX bound .. maxX bound] :: [Int]
    emptyRows = filter (\row -> all (\col -> image !. Pos (col, row) == Empty) columns) rows

getEmptyColumnIndices :: Rect -> Image -> [Int]
getEmptyColumnIndices Rect {..} image = getEmptyRowIndices bound' image'
  where
    bound' = Rect minY maxY minX maxX
    image' = mapKeys (\(Pos (x, y)) -> Pos (y, x)) image

manhattanDistance :: Pos -> Pos -> Int
manhattanDistance (Pos (x1, y1)) (Pos (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

------------
-- Part 2 --
------------

solve2 :: Image -> Int
solve2 image = sum dists
  where
    expandedImage = expandSpace 999_999 image
    galaxies = keys . M.filter (== Galaxy) $ expandedImage :: [Pos]
    dists = [manhattanDistance p1 p2 | p1 <- galaxies, p2 <- galaxies, p1 < p2]

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  image <- parseImage <$> readFileLines "inputs/Y2023/Day11.txt" :: IO Image
  print $ solve1 image
  print $ solve2 image

parseImage :: [Text] -> Image
parseImage = readMap [0 ..] [0 ..] parseObject
  where
    parseObject '.' = Empty
    parseObject '#' = Galaxy
    parseObject _ = error "Invalid object"
