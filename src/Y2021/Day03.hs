module Y2021.Day03 where

import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Text.Read as R

-----------------------
-- Type declarations --
-----------------------

data Binary = Zero | One deriving (Eq, Show)
newtype BinaryNum = BinaryNum [Binary]
type DiagReport = NonEmpty BinaryNum

unpackReport :: DiagReport -> [[Binary]]
unpackReport = toList . fmap un

packReport :: [[Binary]] -> DiagReport
packReport unpacked =
    nonEmpty (BinaryNum <$> unpacked) ?: error "Expected more than one number exist!"

binToInt :: BinaryNum -> Int
binToInt num = go (un num) 0  where
    go []          acc = acc
    go [Zero     ] acc = acc
    go [One      ] acc = acc + 1
    go (Zero : bs) acc = go bs (2 * acc)
    go (One  : bs) acc = go bs (2 * (acc + 1))

------------
-- Part 1 --
------------

solve1 :: DiagReport -> Int
solve1 report = binToInt gammaRate * binToInt epsilonRate  where
    gammaRate   = getGammaRate report
    epsilonRate = getEpsilonRate report

getGammaRate :: DiagReport -> BinaryNum
getGammaRate (unpackReport -> report) = BinaryNum $ mostCommon <$> transpose report

getEpsilonRate :: DiagReport -> BinaryNum
getEpsilonRate (unpackReport -> report) = BinaryNum $ leastCommon <$> transpose report

-- Part 2 specifies that if tie, then 1
mostCommon :: [Binary] -> Binary
mostCommon number = if length number < 2 * zeroCount then Zero else One
    where zeroCount = length $ filter (== Zero) number

leastCommon :: [Binary] -> Binary
leastCommon number = case mostCommon number of
    Zero -> One
    One  -> Zero


------------
-- Part 2 --
------------

solve2 :: DiagReport -> Int
solve2 report = binToInt co2Rating * binToInt oxygenRating  where
    co2Rating    = getCO2Rating report
    oxygenRating = getOxygenRating report

getCO2Rating :: DiagReport -> BinaryNum
getCO2Rating = go 0  where
    go _     (x :| []) = x
    go index report    = go (index + 1) (filterReport mostCommon report index)

getOxygenRating :: DiagReport -> BinaryNum
getOxygenRating = go 0  where
    go _     (x :| []) = x
    go index report    = go (index + 1) (filterReport leastCommon report index)

filterReport :: ([Binary] -> Binary) -> DiagReport -> Int -> DiagReport
filterReport criteria (unpackReport -> report) bitIndex = packReport filtered  where
    criteriaBits = transpose report !!? bitIndex ?: [] :: [Binary]
    criteriaBit  = criteria criteriaBits
    filtered     = filter (\number -> (number !!? bitIndex ?: Zero) == criteriaBit) report


--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    report <-
        fromList <$> (parseBinaryNum <<$>> readFileLines "inputs/Y2021/Day03.txt") :: IO DiagReport
    print $ solve1 report
    print $ solve2 report

parseBinaryNum :: Text -> BinaryNum
parseBinaryNum = BinaryNum . fmap parseBinary . toString  where
    parseBinary '0' = Zero
    parseBinary '1' = One
    parseBinary _   = error "Unexpected input!"
