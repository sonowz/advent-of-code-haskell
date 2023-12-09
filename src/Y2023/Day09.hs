module Y2023.Day09 (main') where

import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Lib.IO
import Lib.Parser qualified as P
import Lib.Types
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

type TimeSeries = NonEmpty Value

type Value = Int

------------
-- Part 1 --
------------

solve1 :: [TimeSeries] -> Int
solve1 valueHistories = sum extrapolatedValues
  where
    extrapolatedValues = last . extrapolateForward <$> valueHistories

extrapolateForward :: TimeSeries -> TimeSeries
extrapolateForward ts | all (== 0) ts = 0 <| ts
extrapolateForward ts = ts'
  where
    diffs = NE.zipWith (-) (tail' ts) ts
    diffs' = extrapolateForward diffs
    ts' = ts <> [last ts + last diffs']

tail' :: NonEmpty a -> NonEmpty a
tail' = Unsafe.fromJust . nonEmpty . tail

------------
-- Part 2 --
------------

solve2 :: [TimeSeries] -> Int
solve2 valueHistories = sum extrapolatedValues
  where
    extrapolatedValues = head . extrapolateBackward <$> valueHistories

extrapolateBackward :: TimeSeries -> TimeSeries
extrapolateBackward = NE.reverse . extrapolateForward . NE.reverse

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  valueHistories <- parseValueHistory <<$>> readFileLines "inputs/Y2023/Day09.txt" :: IO [TimeSeries]
  print $ solve1 valueHistories
  print $ solve2 valueHistories

parseValueHistory :: Text -> TimeSeries
parseValueHistory = fmap readInt . Unsafe.fromJust . nonEmpty . words
