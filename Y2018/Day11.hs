import Control.Monad
import Data.Function
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as Vec

power serial y x = p4 where
    rackId = x + 10
    p1 = rackId * y
    p2 = p1 + serial
    p3 = p2 * rackId
    hundreds x = (x `mod` 1000) `div` 100
    p4 = (hundreds p3) - 5

makeGrid serial = Vec.generate 301 xCol where
    xCol y = Vec.generate 301 (power serial y)

maxSubRect :: Int -> Vector (Vector Int) -> (Int, Int, Int)
maxSubRect n grid = maxSubRect' where
    subRect x y = Vec.map (Vec.slice x n) (Vec.slice y n grid)
    rectSum a = Vec.sum (Vec.map Vec.sum a)
    sums = [(x, y, rectSum $ subRect x y) | x <- [1..(301 - n)], y <- [1..(301 - n)]]
    getSum (x, y, s) = s
    maxSubRect' = maximumBy (compare `on` getSum) sums


solve1 :: Int -> (Int, Int)
solve1 serial = (mx, my) where
    grid = makeGrid serial
    (mx, my, ms) = maxSubRect 3 grid

solve2 :: Int -> (Int, Int, Int)
solve2 serial = (mx, my, size) where
    grid = makeGrid serial
    nMaxSum = map (\n -> (n, maxSubRect n grid)) [1..50]
    getSum (x, y, s) = s
    (size, (mx, my, ms)) = maximumBy (compare `on` (getSum . snd)) nMaxSum

main' = do
    serial <- read <$> getLine
    putStrLn $ show $ solve1 serial
    putStrLn $ show $ solve2 serial
