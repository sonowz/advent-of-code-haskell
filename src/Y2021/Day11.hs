module Y2021.Day11 where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Lib.IO
import Lib.Types
import Lib.Vector2D
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple

-----------------------
-- Type declarations --
-----------------------

type OctopusMap = Vector (Vector Energy)
newtype Energy = Energy Int deriving (Show, Num, Ord, Eq) via Int
newtype Pos = Pos (Int, Int) deriving (Pos2D, Ord, Eq, Show) via (Int, Int)

------------
-- Part 1 --
------------

solve1 :: OctopusMap -> Int
solve1 m = sum (countFlashed <$> take 101 states) where states = iterate step m

step :: OctopusMap -> OctopusMap
step = setMinusToZero . doFlash . increaseEnergyLevel

increaseEnergyLevel :: OctopusMap -> OctopusMap
increaseEnergyLevel = fmap (fmap (+ 1))

doFlash :: OctopusMap -> OctopusMap
doFlash m
    | -- Nothing to do!
      null flashingPos = m
    | -- Recursive call
      otherwise        = doFlash mFlashed
  where
    flashingPos = imapMaybeL2D (\pos e -> if e > 9 then Just pos else Nothing) m :: [Pos]
    mFlashed    = foldl' (\map pos -> setMinusEnergy (increaseLight map pos) pos) m flashingPos
    increaseLight map pos =
        foldl' (\accMap adjPos -> update accMap adjPos (+ 1)) map (adjacent pos)
    setMinusEnergy map pos = set map pos (-999)

    update :: Pos2D pos => Vector (Vector a) -> pos -> (a -> a) -> Vector (Vector a)
    update map pos f = case map `at` pos of
        Just value -> set map pos (f value)
        Nothing    -> map

setMinusToZero :: OctopusMap -> OctopusMap
setMinusToZero = imap2D (\(_ :: Pos) energy -> if energy < 0 then 0 else energy)

countFlashed :: OctopusMap -> Int
countFlashed m = sum (length . V.filter (== 0) <$> m)

adjacent :: Pos -> [Pos]
adjacent (Pos (x, y)) = filter (/= Pos (x, y)) ninePoints
    where ninePoints = [ Pos (x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1] ]

------------
-- Part 2 --
------------

solve2 :: OctopusMap -> Int
solve2 m = firstAllFlashed  where
    states           = iterate step m
    enumeratedStates = zip [0 ..] states :: [(Int, OctopusMap)]
    firstAllFlashed  = fst . unsafeHead $ dropWhile (not . allFlashed . snd) enumeratedStates :: Int
    unsafeHead       = fromMaybe (error "Impossible!") . viaNonEmpty head

allFlashed :: OctopusMap -> Bool
allFlashed m = countFlashed m == mapSize where mapSize = let (x, y) = size2D m in x * y

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    octopusMap <- parseOctopusMap <$> readFileLines "inputs/Y2021/Day11.txt" :: IO OctopusMap
    print $ solve1 octopusMap
    print $ solve2 octopusMap

parseOctopusMap :: [Text] -> OctopusMap
parseOctopusMap lines = fromList $ parseLine <$> lines  where
    parseLine :: Text -> Vector Energy
    parseLine line = fromList $ Energy . readInt . one <$> toString line
