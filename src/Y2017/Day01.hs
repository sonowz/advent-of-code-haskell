module Y2017.Day01 where

import           Relude
import           Data.List                      ( cycle )
import           Lib.IO

-----------------------
-- Type declarations --
-----------------------

type Captcha = [Int]

------------
-- Part 1 --
------------

sameOrZero :: Int -> Int -> Int
sameOrZero v1 v2 = if v1 == v2 then v1 else 0

solve1 :: Captcha -> Int
solve1 digits = sum (zipWith sameOrZero digits compareDigits)
    where compareDigits = drop 1 . cycle $ digits

------------
-- Part 2 --
------------

solve2 :: Captcha -> Int
solve2 digits = sum (zipWith sameOrZero digits compareDigits)
    where compareDigits = drop (length digits `div` 2) . cycle $ digits

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    digits <-
        parseDigits <$> readFileText "inputs/Y2017/Day01.txt" :: IO [Int]
    print $ solve1 digits
    print $ solve2 digits

parseDigits :: Text -> Captcha
parseDigits = map (fromMaybe 0 . readMaybe . pure) . toString
