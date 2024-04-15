module Y2023.Day21 (main') where

import Data.Map.Strict (findMax)
import Lib.IO
import Lib.Types
import Lib.Vector2D (Pos2D)
import Relude
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Unsafe qualified as Unsafe
import Text.Show qualified as T

-----------------------
-- Type declarations --
-----------------------

type GardenMap = Map Pos Tile

newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord, Pos2D) via (Int, Int)

data Tile = TPlot | TRock deriving (Eq, Ord)

type ReachablePosSet = Set Pos

------------
-- Part 1 --
------------

solve1 :: GardenMap -> Pos -> Int
solve1 gardenMap startPos = length reachables
  where
    targetStep = 64
    reachables = reachableScans startPos (isPlot gardenMap) Unsafe.!! targetStep

isPlot :: GardenMap -> Pos -> Bool
isPlot gardenMap pos = gardenMap !? pos == Just TPlot

reachableScans :: Pos -> (Pos -> Bool) -> [ReachablePosSet]
reachableScans startPos isPlot = iterate (stepReachableState isPlot) initSet
  where
    initSet = one startPos :: ReachablePosSet

stepReachableState :: (Pos -> Bool) -> ReachablePosSet -> ReachablePosSet
stepReachableState isPlot = fromList . filter isPlot . concatMap adjacent . toList

adjacent :: Pos -> [Pos]
adjacent (Pos (x, y)) = [Pos (x + 1, y), Pos (x - 1, y), Pos (x, y + 1), Pos (x, y - 1)]

------------
-- Part 2 --
------------

{-
  If you print solution of part 1, you will see that the reachable area forms a perfect diamond.
  In other words, all of the perimeter points in the reachable area is reachable.
  Then, if you print all step counts, you will see a perfect diamond pattern in (131 * n + 65) steps.
  Note that the number 131 is the length of input garden map.
  Accidentally, the part 2 target step 26501365 is 131 * 202300 + 65.
  Then, drawing the plot over the grid every 131 steps will give the patterns.
  For explanation, refer to below article.
  https://github.com/villuna/aoc23/wiki/A-Geometric-solution-to-advent-of-code-2023,-day-21
-}

solve2 :: GardenMap -> Pos -> Integer
solve2 gardenMap startPos = stepCount
  where
    allReachables = take 201 $ reachableScans startPos (isPlot gardenMap)
    stepCount = getReachableCount allReachables 65 202300

getReachableCount :: [ReachablePosSet] -> Int -> Integer -> Integer
getReachableCount reachables edgeReachStep stepCount = totalReachableCount
  where
    reachablesLength = length reachables - 1
    getReachableCountAt i = fromIntegral . length $ reachables Unsafe.!! i

    fullOddStepReachableCount = getReachableCountAt reachablesLength
    fullEvenStepReachableCount = getReachableCountAt (reachablesLength - 1)
    innerHalfOddStepReachableCount = getReachableCountAt edgeReachStep
    innerHalfEvenStepReachableCount = getReachableCountAt (edgeReachStep - 1)
    outerHalfOddStepReachableCount = fullOddStepReachableCount - innerHalfOddStepReachableCount
    outerHalfEvenStepReachableCount = fullEvenStepReachableCount - innerHalfEvenStepReachableCount

    n = stepCount
    totalReachableCount =
      (n + 1) ^ 2 * fullOddStepReachableCount
        + n ^ 2 * fullEvenStepReachableCount
        - (n + 1) * outerHalfOddStepReachableCount
        + n * outerHalfEvenStepReachableCount

--- Functions used for experimenting ---

isPlotInInfiniteMap :: GardenMap -> Pos -> Bool
isPlotInInfiniteMap gardenMap (Pos (x, y)) = gardenMap !? Pos (x', y') == Just TPlot
  where
    Pos (maxX, maxY) = fst $ findMax gardenMap
    x' = x `mod` maxX
    y' = y `mod` maxY

printReachablesIn :: GardenMap -> Pos -> Int -> IO ()
printReachablesIn gardenMap startPos n = printReachablePosSet $ reachableScans startPos (isPlotInInfiniteMap gardenMap) Unsafe.!! n

printReachablePosSet :: ReachablePosSet -> IO ()
printReachablePosSet = putStrLn . flip showMap " " . toMap
  where
    toMap = fromList . fmap (,PrintO) . toList

data PrintO = PrintO

instance T.Show PrintO where
  show _ = "O"

printPerfectDiamondStepsIn :: GardenMap -> Pos -> Int -> IO ()
printPerfectDiamondStepsIn gardenMap startPos n = doPrint
  where
    reachables = take n . zip [0 ..] $ reachableScans startPos (isPlotInInfiniteMap gardenMap)
    doPrint = mapM_ (\(i, posSet) -> if isPerfectDiamond posSet then print i else pass) reachables

isPerfectDiamond :: ReachablePosSet -> Bool
isPerfectDiamond posSet = all (`member` posSet) requiredPos
  where
    toNonEmpty' = Unsafe.fromJust . nonEmpty . toList
    Pos (minX, minY) = minimum1 (toNonEmpty' posSet)
    Pos (maxX, maxY) = maximum1 (toNonEmpty' posSet)
    centerX = (minX + maxX) `div` 2
    centerY = (minY + maxY) `div` 2
    offsetRange = [0 .. ((maxX - minX) `div` 2)] :: [Int]
    requiredPos :: [Pos]
    requiredPos = do
      i <- offsetRange
      [Pos (minX + i, centerY + i), Pos (minX + i, centerY - i), Pos (maxX - i, centerY + i), Pos (maxX - i, centerY - i)]

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  (gardenMap, startPos) <- parseGardenMapAndStartPos <$> readFileLines "inputs/Y2023/Day21.txt" :: IO (GardenMap, Pos)
  print $ solve1 gardenMap startPos
  print $ solve2 gardenMap startPos

parseGardenMapAndStartPos :: [Text] -> (GardenMap, Pos)
parseGardenMapAndStartPos lines = (gardenMap, startPos)
  where
    gardenMap = readMap [0 ..] [0 ..] parseTile lines
    parseTile '.' = TPlot
    parseTile 'S' = TPlot
    parseTile '#' = TRock
    parseTile _ = error "Invalid tile"

    charMap = readMap [0 ..] [0 ..] id lines
    startPos = fst . Unsafe.fromJust $ find ((==) 'S' . snd) (toPairs charMap)
