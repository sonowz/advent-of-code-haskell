module Y2017.Day02 where

import Relude
import Relude.Extra.Foldable1
import Data.Maybe (fromJust)
import Data.Semigroup
import Lib.IO

-----------------------
-- Type declarations --
-----------------------

type Spreadsheet = [NonEmpty Int]
type Checksum = Int

------------
-- Part 1 --
------------

solve1 :: Spreadsheet -> Checksum
solve1 = sum . map minMaxDiff

minMaxDiff :: NonEmpty Int -> Int
minMaxDiff l = maximum1 l - minimum1 l

------------
-- Part 2 --
------------

solve2 :: Spreadsheet -> Checksum
solve2 = sum . mapMaybe evenDivide

-- This assumes that there are only one number which divides another number
evenDivide :: NonEmpty Int -> Maybe Int
evenDivide (toList -> l) = viaNonEmpty head [ x `div` y | x <- l, y <- l, x > y, x `mod` y == 0 ]

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    sheet <- map readInts <$> readFileLines "inputs/Y2017/Day02.txt" :: IO Spreadsheet
    print $ solve1 sheet
    print $ solve2 sheet
