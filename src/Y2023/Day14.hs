module Y2023.Day14 (main') where

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
import Text.Show qualified as S

-----------------------
-- Type declarations --
-----------------------

type Platform = Map Pos Tile

data Tile = TEmpty | TCube | TRound deriving (Eq, Ord, Generic, Hashable)

newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord, Pos2D, Hashable) via (Int, Int)

newtype Load = Load Int
  deriving (Show, Eq) via Int
  deriving (Semigroup, Monoid) via Sum Int

instance S.Show Tile where
  show TEmpty = "."
  show TCube = "#"
  show TRound = "O"

------------
-- Part 1 --
------------

solve1 :: Platform -> Load
solve1 = calcLoad . tilt (\(Pos (x, y)) -> Pos (x, y - 1))

tilt :: (Pos -> Pos) -> Platform -> Platform
tilt moveFn platform = platform'
  where
    posOrdering :: Pos -> Pos -> Ordering
    posOrdering = compare `on` (negate . dotProduct (moveFn (Pos (0, 0))))
    roundRocks = sortBy posOrdering . map fst . filter ((== TRound) . snd) $ toPairs platform
    platform' = foldl' (tiltRock moveFn) platform roundRocks

tiltRock :: (Pos -> Pos) -> Platform -> Pos -> Platform
tiltRock moveFn platform pos = platform'
  where
    findEndPos :: Pos -> Pos
    findEndPos p = case platform !? moveFn p of
      Just TEmpty -> findEndPos (moveFn p)
      _ -> p
    endPos = findEndPos pos
    platform' = insert endPos TRound (insert pos TEmpty platform)

calcLoad :: Platform -> Load
calcLoad platform = foldMap (Load . (maxY + 1 -) . getY . fst) . filter ((== TRound) . snd) . toPairs $ platform
  where
    maxY = maximum1 . nonEmpty' $ getY <$> keys platform
    nonEmpty' = Unsafe.fromJust . nonEmpty

dotProduct :: Pos -> Pos -> Int
dotProduct (Pos (x1, y1)) (Pos (x2, y2)) = x1 * x2 + y1 * y2

getX :: Pos -> Int
getX (Pos (x, _)) = x

getY :: Pos -> Int
getY (Pos (_, y)) = y

------------
-- Part 2 --
------------

solve2 :: Platform -> Load
solve2 platform = calcLoad $ iterations Unsafe.!! reducedIdx
  where
    iterations :: [Platform]
    iterations = iterate tiltCycle platform
    (cycleStartIdx, cycleLength) = findCycle iterations
    reducedIdx = ((1_000_000_000 - cycleStartIdx) `mod` cycleLength) + cycleStartIdx

tiltCycle :: Platform -> Platform
tiltCycle platform = flipfoldl' tilt platform moveFns
  where
    moveFns :: [Pos -> Pos]
    moveFns =
      [ \(Pos (x, y)) -> Pos (x, y - 1),
        \(Pos (x, y)) -> Pos (x - 1, y),
        \(Pos (x, y)) -> Pos (x, y + 1),
        \(Pos (x, y)) -> Pos (x + 1, y)
      ]

findCycle :: [Platform] -> (Int, Int)
findCycle = go mempty 0
  where
    go :: HashMap Platform Int -> Int -> [Platform] -> (Int, Int)
    go past i (platform : platforms) = case past !? platform of
      Just cycleStartIdx -> (cycleStartIdx, i - cycleStartIdx)
      Nothing -> go (insert platform i past) (i + 1) platforms

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  platform <- parsePlatform <$> readFileLines "inputs/Y2023/Day14.txt" :: IO Platform
  print $ solve1 platform
  print $ solve2 platform

parsePlatform :: [Text] -> Platform
parsePlatform = readMap [0 ..] [0 ..] parseTile
  where
    parseTile '.' = TEmpty
    parseTile '#' = TCube
    parseTile 'O' = TRound
    parseTile _ = error "Invalid tile"
