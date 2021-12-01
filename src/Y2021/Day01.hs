module Y2021.Day01 where

import Data.List (zipWith3)
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

newtype Depth = Depth Int deriving (Read, Eq, Ord, Num) via Int

------------
-- Part 1 --
------------

solve1 :: [Depth] -> Int
solve1 = countIncreased

countIncreased :: [Depth] -> Int
countIncreased (d : ds) = count (== LT) ordList  where
    ordList = zipWith compare (d : ds) ds :: [Ordering]
    count f = length . filter f
countIncreased _ = 0


------------
-- Part 2 --
------------

solve2 :: [Depth] -> Int
solve2 (d1 : d2 : ds) = countIncreased windowedList
    where windowedList = zipWith3 (\a b c -> a + b + c) (d1 : d2 : ds) (d2 : ds) ds :: [Depth]
solve2 _ = 0

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    depths <- Depth . readInt <<$>> readFileLines "inputs/Y2021/Day01.txt" :: IO [Depth]
    print $ solve1 depths
    print $ solve2 depths
