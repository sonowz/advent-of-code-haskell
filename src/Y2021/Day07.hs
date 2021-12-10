module Y2021.Day07 where

import Data.Text (split)
import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))

-----------------------
-- Type declarations --
-----------------------

newtype Pos = Pos Int deriving (Read, Eq, Ord, Enum) via Int
newtype Fuel = Fuel Int deriving (Show, Ord, Eq, Num) via Int

------------
-- Part 1 --
------------

solve1 :: [Pos] -> Fuel
solve1 crabs = alignedDistanceSum alignPos crabs calcFuel  where
    alignPos = median crabs

alignedDistanceSum :: Pos -> [Pos] -> (Pos -> Pos -> Fuel) -> Fuel
alignedDistanceSum align crabs toFuel = sum $ map (toFuel align) crabs

median :: Ord a => [a] -> a
median l = (getNth midIndex . sort) l  where
    midIndex   = length l `div` 2
    getNth :: Int -> [a] -> a
    getNth n = unsafeHead . drop n
    unsafeHead = fromMaybe (error "No crabs!") . viaNonEmpty head

calcFuel :: Pos -> Pos -> Fuel
calcFuel a b = Fuel $ abs (un a - un b)

------------
-- Part 2 --
------------

solve2 :: [Pos] -> Fuel
solve2 crabs = minimum1 alignSums'  where
    alignCandidates = [minimum1 crabs' .. maximum1 crabs'] :: [Pos]
    alignSums       = map (\align -> alignedDistanceSum align crabs calcFuel2) alignCandidates

    fromJust        = fromMaybe (error "No crabs!")
    crabs'          = fromJust $ nonEmpty crabs :: NonEmpty Pos
    alignSums'      = fromJust $ nonEmpty alignSums :: NonEmpty Fuel

-- sigma [1..n] == n * (n+1) / 2
calcFuel2 :: Pos -> Pos -> Fuel
calcFuel2 a b = Fuel $ (d * (d + 1)) `div` 2 where d = abs (un a - un b) :: Int

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    crabs <- parseCrabPos . (!! 0) <$> readFileLines "inputs/Y2021/Day07.txt" :: IO [Pos]
    print $ solve1 crabs
    print $ solve2 crabs

parseCrabPos :: Text -> [Pos]
parseCrabPos = fmap (Pos . readInt) . split (== ',')
