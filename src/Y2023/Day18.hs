module Y2023.Day18 (main') where

import Data.Char (digitToInt, isDigit)
import Data.Set qualified as Set
import Data.Text qualified as T
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

data DigPlan = DigPlan
  { dir :: Direction,
    meter :: Int,
    edgeColor :: Color
  }
  deriving (Show)

data Direction = DUp | DDown | DLeft | DRight deriving (Show, Eq, Ord)

newtype Color = Color Text deriving (Show, Eq)

type DigMap = Map Pos Tile

newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord, Pos2D) via (Int, Int)

data Tile = TEmpty | TWall | TTrench deriving (Eq)

instance S.Show Tile where
  show TEmpty = "."
  show TWall = "█"
  show TTrench = "#"

------------
-- Part 1 --
------------

solve1 :: [DigPlan] -> Int
solve1 digPlans = digArea digMap'
  where
    digMap :: DigMap
    digMap = snd $ foldl' dig (Pos (0, 0), mempty) digPlans
    digMap' = digInterior digMap

digArea :: DigMap -> Int
digArea = length . filter (not . isEmptyTile) . elems

dig :: (Pos, DigMap) -> DigPlan -> (Pos, DigMap)
dig (pos, digMap) digPlan = (pos', digMap')
  where
    pos' = pos `move` digPlan
    digMap' = foldl' (\m p -> insert p TWall m) digMap (pos `movePath` digPlan)

move :: Pos -> DigPlan -> Pos
move (Pos (x, y)) (DigPlan DUp m _) = Pos (x, y - m)
move (Pos (x, y)) (DigPlan DDown m _) = Pos (x, y + m)
move (Pos (x, y)) (DigPlan DLeft m _) = Pos (x - m, y)
move (Pos (x, y)) (DigPlan DRight m _) = Pos (x + m, y)

movePath :: Pos -> DigPlan -> [Pos]
movePath (Pos (x, y)) (DigPlan DUp m _) = Pos . (x,) <$> [y - m .. y]
movePath (Pos (x, y)) (DigPlan DDown m _) = Pos . (x,) <$> [y .. y + m]
movePath (Pos (x, y)) (DigPlan DLeft m _) = Pos . (,y) <$> [x - m .. x]
movePath (Pos (x, y)) (DigPlan DRight m _) = Pos . (,y) <$> [x .. x + m]

-- Assume (1, 1) belongs to trench
digInterior :: DigMap -> DigMap
digInterior digMap = foldl' (\m p -> insert p TTrench m) digMap $ floodFill digMap (Pos (1, 1))

floodFill :: DigMap -> Pos -> Set Pos
floodFill digMap = go mempty
  where
    go :: Set Pos -> Pos -> Set Pos
    go s p | member p s = s
    go s p | not (isEmptyTile $ digMap !? p ?: TEmpty) = s
    go s p = foldl' go (Set.insert p s) (adjacent p)

isEmptyTile :: Tile -> Bool
isEmptyTile TEmpty = True
isEmptyTile _ = False

adjacent :: Pos -> [Pos]
adjacent (Pos (x, y)) = Pos <$> [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

------------
-- Part 2 --
------------

solve2 :: [DigPlan] -> Int
solve2 digPlans = digAreaComp compressedDigMap''
  where
    correctDigPlans = correctDigPlan <$> digPlans
    compressedDigMap = initCompressedDigMap correctDigPlans
    compressedDigMap' = snd $ foldl' digComp (CompressedPos 0 0 1 1, compressedDigMap) correctDigPlans
    compressedDigMap'' = digInteriorComp compressedDigMap'

digAreaComp :: CompressedDigMap -> Int
digAreaComp = sum . fmap (getArea . fst) . filter (not . isEmptyTile . snd) . toPairs . compMap
  where
    getArea (CompressedPos sx sy ex ey) = (ex - sx) * (ey - sy)

correctDigPlan :: DigPlan -> DigPlan
correctDigPlan (DigPlan _ _ (Color color)) = DigPlan dir meter (Color color)
  where
    colorStr = toString color
    meter = readHex $ take 5 colorStr
    dir = case drop 5 colorStr of
      "0" -> DRight
      "1" -> DDown
      "2" -> DLeft
      "3" -> DUp
      _ -> error "Invalid color code"

-- The map is compressed by points made by digging
-- 'CompressedPos sx sy ex ey' represents a rectangle area from (sx, sy) to (ex-1, ey-1)
data CompressedDigMap = CompressedDigMap
  { compMap :: Map CompressedPos Tile,
    xCoordinates :: Set Int,
    yCoordinates :: Set Int
  }

data CompressedPos = CompressedPos
  { startX :: Int,
    startY :: Int,
    endX :: Int,
    endY :: Int
  }
  deriving (Show, Eq, Ord)

initCompressedDigMap :: [DigPlan] -> CompressedDigMap
initCompressedDigMap digPlans = CompressedDigMap {..}
  where
    diggedMap = snd $ foldl' digPointOnly (Pos (0, 0), mempty) digPlans
    xCoordinates = fromList . foldMap (\(Pos (x, y)) -> [x, x + 1]) . keys $ diggedMap
    yCoordinates = fromList . foldMap (\(Pos (x, y)) -> [y, y + 1]) . keys $ diggedMap
    compMap = mempty

    digPointOnly :: (Pos, DigMap) -> DigPlan -> (Pos, DigMap)
    digPointOnly (pos, digMap) digPlan = (pos', digMap')
      where
        pos' = pos `move` digPlan
        digMap' = insert pos' TWall digMap

digComp :: (CompressedPos, CompressedDigMap) -> DigPlan -> (CompressedPos, CompressedDigMap)
digComp (pos, digMap) digPlan = (pos', digMap')
  where
    movePath :: [CompressedPos]
    movePath = movePathComp digMap pos digPlan
    pos' = Unsafe.last movePath
    compMap' = foldl' (\m p -> insert p TWall m) (compMap digMap) movePath
    digMap' = digMap {compMap = compMap'}

-- Area size of last position should be 1
movePathComp :: CompressedDigMap -> CompressedPos -> DigPlan -> [CompressedPos]
movePathComp digMap (CompressedPos sx sy ex ey) (DigPlan dir m _) = case dir of
  DUp -> do
    (y2, y1) <- zipPair $ filterRange (sy - m) sy (flip compare) (yCoordinates digMap)
    pure $ CompressedPos sx y1 ex y2
  DDown -> do
    (y1, y2) <- zipPair $ filterRange sy (sy + m) compare (yCoordinates digMap)
    pure $ CompressedPos sx y1 ex y2
  DLeft -> do
    (x2, x1) <- zipPair $ filterRange (sx - m) sx (flip compare) (xCoordinates digMap)
    pure $ CompressedPos x1 sy x2 ey
  DRight -> do
    (x1, x2) <- zipPair $ filterRange sx (sx + m) compare (xCoordinates digMap)
    pure $ CompressedPos x1 sy x2 ey
  where
    filterRange :: Int -> Int -> (Int -> Int -> Ordering) -> Set Int -> [Int]
    filterRange start end ord = sortBy ord . toList . Set.takeWhileAntitone (<= end + 1) . Set.dropWhileAntitone (< start)
    zipPair :: [a] -> [(a, a)]
    zipPair [] = []
    zipPair [_] = []
    zipPair (x : y : xs) = (x, y) : zipPair (y : xs)

-- Assume square which starts with (1, 1) belongs to trench
digInteriorComp :: CompressedDigMap -> CompressedDigMap
digInteriorComp digMap@CompressedDigMap {..} = digMap {compMap = compMap'}
  where
    ex = Unsafe.fromJust $ Set.lookupGT 1 xCoordinates
    ey = Unsafe.fromJust $ Set.lookupGT 1 yCoordinates
    initPos = CompressedPos 1 1 ex ey
    compMap' = foldl' (\m p -> insert p TTrench m) compMap $ floodFillComp digMap initPos

floodFillComp :: CompressedDigMap -> CompressedPos -> Set CompressedPos
floodFillComp digMap = go mempty
  where
    go :: Set CompressedPos -> CompressedPos -> Set CompressedPos
    go s p | member p s = s
    go s p | not (isEmptyTile $ compMap digMap !? p ?: TEmpty) = s
    go s p = foldl' go (Set.insert p s) (adjacentComp digMap p)

adjacentComp :: CompressedDigMap -> CompressedPos -> [CompressedPos]
adjacentComp CompressedDigMap {..} (CompressedPos x1 y1 x2 y2) = adjs
  where
    x0 = Set.lookupLT x1 xCoordinates
    y0 = Set.lookupLT y1 yCoordinates
    x3 = Set.lookupGT x2 xCoordinates
    y3 = Set.lookupGT y2 yCoordinates
    adjs =
      catMaybes
        [ (\x0' -> CompressedPos x0' y1 x1 y2) <$> x0,
          (\x3' -> CompressedPos x2 y1 x3' y2) <$> x3,
          (\y0' -> CompressedPos x1 y0' x2 y1) <$> y0,
          (\y3' -> CompressedPos x1 y2 x2 y3') <$> y3
        ]

readHex :: String -> Int
readHex = foldl' (\acc c -> acc * 16 + hexToInt c) 0
  where
    hexToInt c | isDigit c = digitToInt c
    hexToInt c | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  digPlans <- parseDigPlan <<$>> readFileLines "inputs/Y2023/Day18.txt" :: IO [DigPlan]
  print $ solve1 digPlans
  print $ solve2 digPlans

parseDigPlan :: Text -> DigPlan
parseDigPlan line =
  DigPlan
    { dir = parseDir dirStr,
      meter = parseMeter meterStr,
      edgeColor = parseColor colorStr
    }
  where
    [dirStr, meterStr, colorStr] = words line

parseDir :: Text -> Direction
parseDir "R" = DRight
parseDir "L" = DLeft
parseDir "U" = DUp
parseDir "D" = DDown
parseDir _ = error "Invalid direction"

parseMeter :: Text -> Int
parseMeter = readInt

parseColor :: Text -> Color
parseColor = Color . T.take 6 . T.drop 2
