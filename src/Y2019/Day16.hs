{-# LANGUAGE Strict #-}
module Y2019.Day16 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))
import Data.List (iterate')
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Lib.IO
import Lib.Types

-----------------------
-- Type declarations --
-----------------------

type SignalVector = Vector Int

------------
-- Part 1 --
------------

solve1 :: SignalVector -> Integer
solve1 signal = vectorToInteger $ V.take 8 signal100th
    where signal100th = iterate' nextSignal signal !! 100

-- Plus : 4nk + i + (n-2), Minus : 4nk + i + (3n-2)
plusIndex, minusIndex :: Int -> [Int]
plusIndex n = [ 4 * n * k + i + n - 2 | k <- [0 ..], i <- [1 .. n] ]
minusIndex n = [ 4 * n * k + i + (3 * n) - 2 | k <- [0 ..], i <- [1 .. n] ]

nextSignal :: SignalVector -> SignalVector
nextSignal signal = V.force $ fmap calcElement [1 .. n]  where
    n = length signal
    calcElement elemIndex = getDigit (plusValue elemIndex - minusValue elemIndex)
    trimIndices = takeWhile (< n)
    plusValue elemIndex = foldl' (\s i -> (signal V.! i) + s) 0 (trimIndices $ plusIndex elemIndex)
    minusValue elemIndex =
        foldl' (\s i -> (signal V.! i) + s) 0 (trimIndices $ minusIndex elemIndex)
    getDigit x = abs x `mod` 10

vectorToInteger :: SignalVector -> Integer
vectorToInteger = V.foldl' (\s v -> s * 10 + fromIntegral v) 0

------------
-- Part 2 --
------------

-- Note : this solution assumes that the 7-digit offset is bigger than half of the input length

solve2 :: SignalVector -> Integer
solve2 signal = message  where
    repeatedSignal     = stimes 10000 signal
    halfOffset         = V.length repeatedSignal `div` 2
    repeatedHalfSignal = V.force $ V.drop halfOffset repeatedSignal
    halfSignal100th    = iterate' nextSignalSecondHalf repeatedHalfSignal !! 100
    messageOffset      = fromIntegral . vectorToInteger $ V.take 7 signal :: Int
    message = vectorToInteger . V.take 8 $ V.drop (messageOffset - halfOffset) halfSignal100th

-- Second half of signal can be calculated by making sum series in reverse order
nextSignalSecondHalf :: SignalVector -> SignalVector
nextSignalSecondHalf = V.force . V.scanr1' (\a b -> (a + b) `mod` 10)

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    --print $ patternMatrix 10 20 `power` 3
    signal <- toSignalVector . readIntList <$> readFile "inputs/Y2019/Day16.txt" :: IO SignalVector
    print $ solve1 signal
    print $ solve2 signal

toSignalVector :: NonEmpty Int -> SignalVector
toSignalVector (toList -> l) = V.fromList l

readIntList :: String -> NonEmpty Int
readIntList = fromJust . nonEmpty . map (readInt . toText . one @String)
