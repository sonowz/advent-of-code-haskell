module Y2021.Day06 where

import Data.Text (split)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
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

newtype FishState = FishState [Integer] deriving (Show)

------------
-- Part 1 --
------------

solve1 :: FishState -> Integer
solve1 fishState = sum (un @[Integer] fishState80)
    where fishState80 = iterate step fishState !! 80

step :: FishState -> FishState
step (FishState days) = FishState newState  where
    -- Exhaustive pattern matching, using Maybe monad
    (day0, day1to6, day7, day8) = fromJust $ do
        (day0, day1to8) <- uncons days
        let (day1to6, day7to8) = splitAt 6 day1to8
        (day7, day8') <- uncons day7to8
        (day8, _    ) <- uncons day8'
        return (day0, day1to6, day7, day8)

    fromJust = fromMaybe (error "Invalid State!")
    newState = day1to6 <> [day7 + day0, day8, day0]

------------
-- Part 2 --
------------

solve2 :: FishState -> Integer
solve2 fishState = sum (un @[Integer] fishState256)
    where fishState256 = iterate step fishState !! 256

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    initFishState <-
        parseFishState . (!! 0) <$> readFileLines "inputs/Y2021/Day06.txt" :: IO FishState
    print $ solve1 initFishState
    print $ solve2 initFishState

-- Stateful creation
parseFishState :: Text -> FishState
parseFishState line = (FishState . toList) $ V.create $ do
    vecState <- MV.replicate 9 0
    forM_ fishes (addFish vecState)
    return vecState
  where
    fishes = readInt <$> split (== ',') line :: [Int]
    addFish state fish = MV.modify state (+ 1) fish
