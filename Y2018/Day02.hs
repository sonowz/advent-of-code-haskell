import Control.Monad
import Data.List

alphabets = ['a'..'z']


checkN n box = any check alphabets where
    check c =
        let filtered = filter (== c) box in
        length filtered == n

solve1 :: [String] -> Int
solve1 boxes =
    (length has2) * (length has3) where
        has2 = filter (checkN 2) boxes
        has3 = filter (checkN 3) boxes


same s1 s2 = filter (/= '0') sames where
    sames = zipWith (\x y -> if x == y then x else '0') s1 s2

solve2 :: [String] -> String
solve2 boxes = same p1 p2 where
    boxPairs = [ (x, y) | x <- boxes, y <- boxes ]
    differByOne (s1, s2) = (length s1) == (length (same s1 s2)) + 1
    (p1, p2) = head $ filter differByOne boxPairs


main' = do
    arr <- replicateM 250 getLine
    putStrLn $ show $ solve1 arr
    putStrLn $ solve2 arr
