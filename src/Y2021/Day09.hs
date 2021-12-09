module Y2021.Day09 where

import qualified Data.Set as S
import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple

-----------------------
-- Type declarations --
-----------------------

type HeightMap = Map Pos Height
type Basin = Set Pos
newtype Pos = Pos (Int, Int) deriving (Ord, Eq, Show) via (Int, Int)
newtype Height = Height Int deriving (Ord, Eq, Show) via Int
newtype RiskLevel = RiskLevel Int deriving (Num, Show) via Int

------------
-- Part 1 --
------------

solve1 :: HeightMap -> RiskLevel
solve1 hm = sum riskLevels  where
    lowPoints = filter (isLowPoint hm) (keys hm) :: [Pos]
    riskLevels =
        map (\p -> riskLevel $ lookup p hm ?: error "Must have pos!") lowPoints :: [RiskLevel]

isLowPoint :: HeightMap -> Pos -> Bool
isLowPoint hm pos = fromMaybe False $ do
    posValue <- lookup pos hm
    return $ all (> posValue) adjacentValues
    where adjacentValues = mapMaybe (`lookup` hm) (adjacent pos) :: [Height]

-- 4 way
adjacent :: Pos -> [Pos]
adjacent (Pos (x, y)) = [Pos (x + 1, y), Pos (x - 1, y), Pos (x, y + 1), Pos (x, y - 1)]

riskLevel :: Height -> RiskLevel
riskLevel (Height h) = RiskLevel (h + 1)

------------
-- Part 2 --
------------

solve2 :: HeightMap -> Int
solve2 hm = product . take 3 . sortDesc $ basinSizes  where
    lowPoints  = filter (isLowPoint hm) (keys hm) :: [Pos]
    basins     = floodFill hm <$> lowPoints :: [Basin]
    basinSizes = length <$> basins :: [Int]
    sortDesc   = sortBy (flip compare) -- descending sort

-- Flood fill until height is 9 or out of boundary
floodFill :: HeightMap -> Pos -> Basin
floodFill hm = go hm mempty  where
    go hm s pos
        | -- Base case
          (posValue ?: 9) == 9 = s
        | -- Cut
          member pos s         = s
        | -- Branch case
          otherwise            = foldl' (go hm) s' (adjacent pos)
      where
        s'       = S.insert pos s :: Basin
        posValue = un <$> lookup pos hm :: Maybe Int

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    heightMap <- parseHeightMap <$> readFileLines "inputs/Y2021/Day09.txt" :: IO HeightMap
    print $ solve1 heightMap
    print $ solve2 heightMap

parseHeightMap :: [Text] -> HeightMap
parseHeightMap lines = fromList . join $ posPairs  where
    posPairs :: [[(Pos, Height)]]
    posPairs = zipWith (\y line -> zipWith (innerZipFn y) [0 ..] (toString line)) [0 ..] lines
    innerZipFn y = \x char -> (Pos (x, y), toHeight char)
    toHeight = Height . readInt . toText . (one :: Char -> String) :: Char -> Height
