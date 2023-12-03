module Y2023.Day03 (main') where

import Data.Char (digitToInt, isDigit)
import Data.Map.Strict ((!))
import Lib.IO
import Lib.Parser qualified as P
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

type EngineSchematic = Map Pos Tile

newtype Pos = Pos (Int, Int) deriving (Ord, Eq, Show, Pos2D) via (Int, Int)

data Tile = TEmpty | TSymbol Char | TNumber Int

data Part = Part Char [Int] deriving (Show)

------------
-- Part 1 --
------------

solve1 :: EngineSchematic -> Int
solve1 = sum . map getPartNumberSum . getAllParts
  where
    getPartNumberSum (Part _ numbers) = sum numbers

getAllParts :: EngineSchematic -> [Part]
getAllParts schematic = makeParts <$> symbolLocs
  where
    symbolLocs :: [(Pos, Char)]
    symbolLocs = mapMaybe mapFn (toPairs schematic)
      where
        mapFn (pos, TSymbol c) = Just (pos, c)
        mapFn _ = Nothing
    makeParts :: (Pos, Char) -> Part
    makeParts (pos, c) = Part c (getAdjacentNumbers schematic pos)

getAdjacentNumbers :: EngineSchematic -> Pos -> [Int]
getAdjacentNumbers schematic pos = ordNub $ expandToNumber schematic <$> adjacentNumberPoints
  where
    adjacentNumberPoints :: [Pos]
    adjacentNumberPoints = filter filterFn (adjacent pos)
      where
        filterFn p = case schematic !? p of
          Just (TNumber _) -> True
          _ -> False

expandToNumber :: EngineSchematic -> Pos -> Int
expandToNumber schematic pos = digitsToInt digits
  where
    digits = toInt . (schematic !) <$> getXRanges startPos endPos
    expandPos :: (Pos -> Pos) -> Pos -> Pos
    expandPos f p = case schematic !? f p of
      Just (TNumber _) -> expandPos f (f p)
      _ -> p
    startPos = expandPos (\(Pos (x, y)) -> Pos (x - 1, y)) pos
    endPos = expandPos (\(Pos (x, y)) -> Pos (x + 1, y)) pos

    getXRanges (Pos (x1, y1)) (Pos (x2, y2)) =
      if y1 == y2
        then [Pos (x, y1) | x <- [x1 .. x2]]
        else error "Pos not on the same line"
    toInt (TNumber n) = n
    toInt _ = error "Not a number"

adjacent :: Pos -> [Pos]
adjacent (Pos (x, y)) =
  [ Pos (x - 1, y - 1),
    Pos (x, y - 1),
    Pos (x + 1, y - 1),
    Pos (x - 1, y),
    Pos (x + 1, y),
    Pos (x - 1, y + 1),
    Pos (x, y + 1),
    Pos (x + 1, y + 1)
  ]

digitsToInt :: [Int] -> Int
digitsToInt = go . reverse
  where
    go [] = 0
    go (x : xs) = x + 10 * go xs

------------
-- Part 2 --
------------

solve2 :: EngineSchematic -> Int
solve2 = sum . fmap getGearRatio . filter isGear . getAllParts

isGear :: Part -> Bool
isGear (Part '*' [n1, n2]) = True
isGear _ = False

getGearRatio :: Part -> Int
getGearRatio (Part '*' [n1, n2]) = n1 * n2
getGearRatio _ = error "Not a gear"

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  engineSchematic <- parseEngineSchematic <$> readFileLines "inputs/Y2023/Day03.txt" :: IO EngineSchematic
  print $ solve1 engineSchematic
  print $ solve2 engineSchematic

parseEngineSchematic :: [Text] -> EngineSchematic
parseEngineSchematic = readMap [0 ..] [0 ..] parseTile
  where
    parseTile '.' = TEmpty
    parseTile c
      | isDigit c = TNumber (digitToInt c)
      | otherwise = TSymbol c
