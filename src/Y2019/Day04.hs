module Y2019.Day04 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Lib.IO
import Lib.Types

-----------------------
-- Type declarations --
-----------------------

newtype PasswordRange = PwRange (Int, Int) deriving (Show)
newtype Password = Pw Int deriving (Show)

------------
-- Part 1 --
------------

solve1 :: PasswordRange -> Int
solve1 range = length . filter (candPassword . Pw) $ [100000 .. 999999]  where
    candPassword x = all ($ x) conditions
    conditions = [condRange range, condAdjacentSame, condIncreasing] :: [Password -> Bool]

condRange (PwRange (start, end)) (Pw x) = start <= x && x <= end
condAdjacentSame x = isJust $ find ((>= 2) . length) adjs where adjs = group (pwString x)
condIncreasing = condIncreasing' . pwString  where
    condIncreasing' (x1 : x2 : xs)
        | x1 <= x2  = condIncreasing' (x2 : xs)
        | otherwise = False
    condIncreasing' _ = True

pwString :: Password -> String
pwString = show . un @Int

------------
-- Part 2 --
------------

solve2 :: PasswordRange -> Int
solve2 range = length . filter (candPassword . Pw) $ [100000 .. 999999]  where
    candPassword x = all ($ x) conditions
    conditions = [condRange range, condAdjacentSame2, condIncreasing] :: [Password -> Bool]

condAdjacentSame2 x = isJust $ find ((== 2) . length) adjs where adjs = group (pwString x)

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    range <- parsePasswordRange <$> readFile "inputs/Y2019/Day04.txt" :: IO PasswordRange
    print $ solve1 range
    print $ solve2 range

replace :: Char -> Char -> String -> String
replace a b = map (\c -> if c == a then b else c)

parsePasswordRange :: String -> PasswordRange
parsePasswordRange line = PwRange (start, end)
    where start :| [end] = readInts . toText . replace '-' ' ' $ line
