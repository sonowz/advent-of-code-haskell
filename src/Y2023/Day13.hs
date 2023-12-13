module Y2023.Day13 (main') where

import Data.List (groupBy)
import Data.Vector (Vector, (!))
import Lib.IO
import Lib.Parser qualified as P
import Lib.Types
import Lib.Vector2D (Pos2D, set, size2D, unsafeAt)
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple

-----------------------
-- Type declarations --
-----------------------

type Pattern = Vector (Vector Tile)

data Tile = Ash | Rocks deriving (Show, Eq)

data Mirror = Horizontal Int | Vertical Int deriving (Show, Eq)

------------
-- Part 1 --
------------

solve1 :: [Pattern] -> Int
solve1 patterns = sum $ getMirrorValue . head' . findMirrors <$> patterns

findMirrors :: Pattern -> [Mirror]
findMirrors pattern = (Horizontal <$> findHorizontalMirrors pattern) <> (Vertical <$> findVerticalMirrors pattern)

findHorizontalMirrors :: Pattern -> [Int]
findHorizontalMirrors pattern = filter isReflectionLine [1 .. n - 1]
  where
    n = length pattern

    isReflectionLine :: Int -> Bool
    isReflectionLine i = and $ zipWith (==) ((pattern !) <$> [i - 1, i - 2 .. 0]) ((pattern !) <$> [i .. n - 1])

findVerticalMirrors :: Pattern -> [Int]
findVerticalMirrors = findHorizontalMirrors . fromList . fmap fromList . transpose . toList . fmap toList

getMirrorValue :: Mirror -> Int
getMirrorValue (Horizontal i) = 100 * i
getMirrorValue (Vertical i) = i

head' :: [Mirror] -> Mirror
head' = fromMaybe (error "No mirrors in pattern") . viaNonEmpty head

------------
-- Part 2 --
------------

solve2 :: [Pattern] -> Int
solve2 patterns = sum $ getMirrorValue . findMirrorWithSmack <$> patterns

findMirrorWithSmack :: Pattern -> Mirror
findMirrorWithSmack pattern = smackMirror
  where
    origMirror = head' . findMirrors $ pattern
    smackMirror = head' $ (filter (/= origMirror) . findMirrors) =<< getSmackedPatterns pattern

getSmackedPatterns :: Pattern -> [Pattern]
getSmackedPatterns pattern = [smackPos (y, x) | x <- [0 .. maxX - 1], y <- [0 .. maxY - 1]]
  where
    (maxY, maxX) = size2D pattern
    smackPos :: (Int, Int) -> Pattern
    smackPos pos = set pattern pos (oppositeTile $ pattern `unsafeAt` pos)
    oppositeTile Ash = Rocks
    oppositeTile Rocks = Ash

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  patterns <- parsePatterns <$> readFileLines "inputs/Y2023/Day13.txt" :: IO [Pattern]
  print $ solve1 patterns
  print $ solve2 patterns

parsePatterns :: [Text] -> [Pattern]
parsePatterns lines = readGrid parseTile <$> patternStrs
  where
    patternStrs = fromList $ sections lines

parseTile :: Char -> Tile
parseTile '.' = Ash
parseTile '#' = Rocks
parseTile c = error $ "Unknown tile: " <> show c

sections :: [Text] -> [[Text]]
sections = filter (/= [""]) . groupBy (\a b -> a /= "" && b /= "")