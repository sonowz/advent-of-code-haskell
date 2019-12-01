import Control.Monad
import qualified Data.IntMap.Lazy as IntMap

readInt :: String -> Int
readInt ('+':s) = read s
readInt s       = read s

solve1 :: [Int] -> Int
solve1 = sum

solve2 :: [Int] -> Int
solve2 arr =
    freq !! twiceIndex where
        arr' = cycle arr
        freq = scanl (+) 0 arr'
        counter = scanl (\c f -> IntMap.insert f () c) IntMap.empty freq
        twiceIndex = length $ takeWhile (\(f, c) -> IntMap.notMember f c) (zip freq counter)

main' = do
    arr <- map readInt <$> replicateM 1029 getLine
    putStrLn $ show $ solve1 arr
    putStrLn $ show $ solve2 arr
