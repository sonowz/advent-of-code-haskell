module Y2023.Day04 (main') where

import Data.IntSet (intersection)
import Lib.IO
import Lib.Parser qualified as P
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Text

-----------------------
-- Type declarations --
-----------------------

data ScratchCard = ScratchCard
  { cardId :: Int,
    winningNumbers :: IntSet,
    numbers :: IntSet
  }

newtype Point = Point Int
  deriving (Show, Num) via Int
  deriving (Semigroup, Monoid) via Sum Int

------------
-- Part 1 --
------------

solve1 :: [ScratchCard] -> Point
solve1 = foldMap calcPoint

calcPoint :: ScratchCard -> Point
calcPoint ScratchCard {..} =
  let winCount = getWinCount winningNumbers numbers
   in if winCount >= 1
        then Point $ 2 ^ (winCount - 1)
        else 0

getWinCount :: IntSet -> IntSet -> Int
getWinCount winningNumbers = size . intersection winningNumbers

------------
-- Part 2 --
------------

solve2 :: [ScratchCard] -> Int
solve2 = sum . calcScratchCardCounts

calcScratchCardCounts :: Traversable t => t ScratchCard -> t Int
calcScratchCardCounts = snd . mapAccumL produceScratchCardCount (repeat 0)
  where
    produceScratchCardCount :: [Int] -> ScratchCard -> ([Int], Int)
    produceScratchCardCount (copyCount : futureCopyCounts) ScratchCard {..} = (futureCopyCounts', cardCount)
      where
        cardCount = copyCount + 1 -- Including the original card
        winCount = getWinCount winningNumbers numbers
        futureCopyCountUpdates = replicate winCount cardCount
        futureCopyCounts' = longZipWith (+) futureCopyCounts futureCopyCountUpdates

longZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
longZipWith f (x : xs) (y : ys) = f x y : longZipWith f xs ys
longZipWith f xs [] = xs
longZipWith f [] ys = ys

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  cards <- parseScratchCard <<$>> readFileLines "inputs/Y2023/Day04.txt" :: IO [ScratchCard]
  print $ solve1 cards
  print $ solve2 cards

parseScratchCard :: Text -> ScratchCard
parseScratchCard = either (error . show) id . parse parserScratchCard ""

parserScratchCard :: Parser ScratchCard
parserScratchCard = do
  string "Card" >> spaces
  cardId <- P.number
  string ":" >> spaces
  winningNumbers <- fromList <$> P.number `endBy1` spaces
  string "|" >> spaces
  numbers <- fromList <$> P.number `sepBy1` spaces
  return ScratchCard {..}
