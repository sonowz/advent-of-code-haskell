module Y2023.Day06 (main') where

import Lib.IO
import Lib.Parser qualified as P
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

data RaceInfo = RaceInfo
  { time :: Integer,
    distance :: Integer
  }
  deriving (Show)

data WinRange = WinRange Integer Integer deriving (Show)

------------
-- Part 1 --
------------

solve1 :: [RaceInfo] -> Integer
solve1 = product . fmap (getWinTimeCount . determineWinRange)

getWinTimeCount :: WinRange -> Integer
getWinTimeCount (WinRange leastWinTime winTimeRange) = winTimeRange - leastWinTime + 1

determineWinRange :: RaceInfo -> WinRange
determineWinRange (RaceInfo time distance) = WinRange leastWinTime (time - leastWinTime)
  where
    leastWinTime = fromMaybe (error "Invalid race!") $ find (\t -> calcTravelDistance time t > distance) ([1 ..] :: [Integer])

calcTravelDistance :: Integer -> Integer -> Integer
calcTravelDistance raceTime buttonTime = (raceTime - buttonTime) * buttonTime

------------
-- Part 2 --
------------

solve2 :: [RaceInfo] -> Integer
solve2 = getWinTimeCount . determineWinRange . getActualRaceInfo

getActualRaceInfo :: [RaceInfo] -> RaceInfo
getActualRaceInfo raceInfos = RaceInfo actualTime actualDistance
  where
    (times, distances) = unzip . fmap (\(RaceInfo t d) -> (t, d)) $ raceInfos
    actualTime = readInteger $ foldMap show times
    actualDistance = readInteger $ foldMap show distances

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  raceInfos <- parseRaceInfo <$> readFileLines "inputs/Y2023/Day06.txt" :: IO [RaceInfo]
  print $ solve1 raceInfos
  print $ solve2 raceInfos

parseRaceInfo :: [Text] -> [RaceInfo]
parseRaceInfo [line1, line2] = zipWith RaceInfo times distances
  where
    times = fmap readInteger . drop 1 . words $ line1
    distances = fmap readInteger . drop 1 . words $ line2
