module Y2023.Day01 where

import Data.Char (digitToInt, isDigit)
import Data.Text qualified as T
import Lib.IO
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

newtype Document = Document [Text]

newtype CalibrationValue = CalibrationValue Int deriving (Num, Show) via Int

------------
-- Part 1 --
------------

solve1 :: Document -> CalibrationValue
solve1 (Document lines) = sum $ precessLine <$> lines

precessLine :: Text -> CalibrationValue
precessLine = getCalibrationValue . extractDigits . toString
  where
    extractDigits :: String -> NonEmpty Int
    extractDigits = toNonEmptyDigits . map digitToInt . filter isDigit
    getCalibrationValue :: NonEmpty Int -> CalibrationValue
    getCalibrationValue digits = CalibrationValue (10 * head digits + last digits)

toNonEmptyDigits :: [Int] -> NonEmpty Int
toNonEmptyDigits = fromMaybe (error "No digits found!") . nonEmpty

------------
-- Part 2 --
------------

solve2 :: Document -> CalibrationValue
solve2 (Document lines) = sum $ precessLine2 <$> lines

precessLine2 :: Text -> CalibrationValue
precessLine2 = getCalibrationValue . extractFirstAndLastDigit
  where
    extractFirstAndLastDigit :: Text -> (Int, Int)
    extractFirstAndLastDigit line = (firstDigit, lastDigit)
      where
        firstDigit = Unsafe.head . mapMaybe lenientParseDigit . T.inits $ line
        lastDigit = Unsafe.head . mapMaybe lenientParseDigit . reversedInits $ line
    getCalibrationValue :: (Int, Int) -> CalibrationValue
    getCalibrationValue (first, last) = CalibrationValue (10 * first + last)

lenientParseDigit :: Text -> Maybe Int
lenientParseDigit text = find (`T.isInfixOf` text) (keys digitMap) >>= (`lookup` digitMap)

digitMap :: Map Text Int
digitMap =
  fromList
    [ ("0", 0),
      ("1", 1),
      ("2", 2),
      ("3", 3),
      ("4", 4),
      ("5", 5),
      ("6", 6),
      ("7", 7),
      ("8", 8),
      ("9", 9),
      ("one", 1),
      ("two", 2),
      ("three", 3),
      ("four", 4),
      ("five", 5),
      ("six", 6),
      ("seven", 7),
      ("eight", 8),
      ("nine", 9)
    ]

-- "abc" -> ["c", "bc", "abc"]
reversedInits :: Text -> [Text]
reversedInits = fmap toText . join . maybeToList . fmap toList . viaNonEmpty go . toString
  where
    go :: NonEmpty Char -> NonEmpty [Char]
    go (x :| []) = pure x :| []
    go (x :| xs@(_ : _)) = let y = go (fromList xs) in y <> [pure x <> last y]

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  doc <- Document <$> readFileLines "inputs/Y2023/Day01.txt" :: IO Document
  print $ solve1 doc
  print $ solve2 doc
