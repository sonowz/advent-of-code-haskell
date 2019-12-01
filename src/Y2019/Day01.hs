module Y2019.Day01 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Lib.IO

-----------------------
-- Type declarations --
-----------------------

newtype Mass = Mass Int deriving (Read, Num) via Int
newtype Fuel = Fuel Int deriving (Show, Eq, Ord, Num) via Int

------------
-- Part 1 --
------------

solve1 :: [Mass] -> Fuel
solve1 = sum . map requiredFuel

requiredFuel :: Mass -> Fuel
requiredFuel = Fuel . subtract 2 . floor . (/ 3) . fromIntegral . un @Int

------------
-- Part 2 --
------------

solve2 :: [Mass] -> Fuel
solve2 = sum . map totalRequiredFuel

totalRequiredFuel :: Mass -> Fuel
totalRequiredFuel mass
    | newFuel <= 0 = Fuel 0
    | otherwise    = newFuel + totalRequiredFuel newFuelAsMass  where
    newFuel       = requiredFuel mass
    newFuelAsMass = Mass (un newFuel)

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    masses <- Mass . readInt <<$>> readFileLines "inputs/Y2019/Day01.txt" :: IO [Mass]
    print $ solve1 masses
    print $ solve2 masses
