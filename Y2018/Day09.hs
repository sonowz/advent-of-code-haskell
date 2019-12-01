import Control.Monad
import Data.Function
import Data.List
import Data.Array hiding ((!))
import qualified Data.Array as Array ((!))
import qualified Data.Foldable as Foldable


addN :: (Num a) => Int -> a -> Array Int a -> Array Int a
addN i n l = l // [(i, (l Array.! i) + n)]

data Cycle a = Cycle ([a], Int)

instance Foldable Cycle where
    foldr f z x = foldr f z (header' x)

header (l, h) = let (l2, l1) = splitAt h l in l1 ++ l2
header' (Cycle x) = header x

nIndex (Cycle (l, h)) i = i `mod` (length l)

(!) :: Cycle a -> Int -> a
x ! i = (header' x) !! (nIndex x i)

move :: Int -> Cycle a -> Cycle a
move n (Cycle (l, h)) = Cycle (l, (h + n) `mod` (length l))

-- Insert between head and tail, and make it head
insertH :: a -> Cycle a -> Cycle a
insertH v x = Cycle (v : (header' x), 0)

deleteH :: Cycle a -> Cycle a
deleteH = snd . deleteFindH

deleteFindH :: Cycle a -> (a, Cycle a)
deleteFindH x = let (hd:tl) = header' x in (hd, Cycle (tl, 0))


initCircle :: Cycle Int
initCircle = Cycle ([0], 0)

initScore :: Int -> Array Int Int
initScore n = listArray (0, n) (repeat 0)

putMarble t c = ((insertH t) . (move 2)) c

putMarble23 nPlayer t c s = (c', s') where
    player = t `mod` nPlayer
    p1 = t
    (p2, c') = (deleteFindH . (move (-7))) c
    s' = addN player (p1 + p2) s

game nPlayer = (initScore nPlayer) : turn nPlayer 1 initCircle (initScore nPlayer) where
    turn nP t c s
        | (t `mod` 23) /= 0 = let c' = putMarble t c in s : turn nP (t+1) c' s
        | otherwise         = let (c', s') = putMarble23 nP t c s in s' : turn nP (t+1) c' s'

getInput :: String -> (Int, Int)
getInput line = (read $ w !! 0, read $ w !! 6) where w = words line

solve1 :: Int -> Int -> Int
solve1 nPlayer turn = maximum scores where
    scores = map snd (assocs $ game nPlayer !! turn)

-- This can't be computed, since it is not implemented as doubly linked lists
solve2 :: Int -> Int -> Int
solve2 nPlayer turn = solve1 nPlayer (turn * 100)

main' = do
    (nPlayer, turn) <- getInput <$> getLine
    putStrLn $ show $ solve1 nPlayer turn
    putStrLn "Solution 2 can't be computed, since it is not implemented as doubly linked lists."
    putStrLn $ show $ solve2 nPlayer turn
