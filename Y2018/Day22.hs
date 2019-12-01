import Data.Function
import Data.List
import Data.Maybe
import Data.Hashable
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as Vec
import Debug.Trace

inf :: Int
inf = 10000000

lim :: Int -> Int
lim = min inf

($$) :: a -> (a -> b) -> b
($$) = flip ($)

updateB :: Vector (Vector a) -> (Int, Int) -> a -> Vector (Vector a)
updateB b (x, y) v = Vec.accum ($$) b [(y, \l -> Vec.accum ($$) l [(x, const v)])]

imapB :: ((Int, Int) -> a -> b) -> Vector (Vector a) -> Vector (Vector b)
imapB f b = Vec.imap (\y xs -> Vec.imap (\x o -> f (x, y) o) xs) b

mapMaybeB :: (a -> Maybe b) -> Vector (Vector a) -> [b]
mapMaybeB f b = (catMaybes . Vec.toList . Vec.concatMap (Vec.map f)) b

type Pos = (Int, Int)
type RiskBoard = Vector (Vector Int)
type DPBoard = Vector (Vector DP)
data DP = DP Int Int Int -- Climbing gear, Torch, Neither case
data Tool = ClimbGear | Torch | Neither
data Region = Rocky | Wet | Narrow

instance Eq Tool where
    ClimbGear == ClimbGear = True
    Torch == Torch = True
    Neither == Neither = True
    _ == _ = False

instance Eq DP where
    (DP x1 x2 x3) == (DP y1 y2 y3) = (x1, x2, x3) == (y1, y2, y3)

instance Show DP where
    show (DP x1 x2 x3) = pad x1 ++ "_" ++ pad x2 ++ "_" ++ pad x3 where
        pad x = take 2 $ s x ++ repeat '_'
        s x = if x == inf then "x" else show x

atInt :: RiskBoard -> Pos -> Int
atInt board (x, y) = board ! y ! x

atReg :: RiskBoard -> Pos -> Region
atReg board pos = case board `atInt` pos of
    0 -> Rocky
    1 -> Wet
    2 -> Narrow
    _ -> error "Invalid risk level"

at :: DPBoard -> Pos -> DP
at board (x, y) = board ! y ! x

safeAt :: DPBoard -> Pos -> Maybe DP
safeAt board (x, y) = board !? y >>= \ys -> ys !? x

atClimb :: DPBoard -> Pos -> Int
atClimb board pos = getValue (board `at` pos) ClimbGear

atTorch :: DPBoard -> Pos -> Int
atTorch board pos = getValue (board `at` pos) Torch

atNeither :: DPBoard -> Pos -> Int
atNeither board pos = getValue (board `at` pos) Neither

getValue :: DP -> Tool -> Int
getValue (DP c t n) ClimbGear = c
getValue (DP c t n) Torch = t
getValue (DP c t n) Neither = n

setValue :: DP -> Tool -> Int -> DP
setValue (DP c t n) ClimbGear x = DP x t n
setValue (DP c t n) Torch x = DP c x n
setValue (DP c t n) Neither x = DP c t x

genErosionLevels :: Pos -> Int -> [[Int]]
genErosionLevels (tx, ty) depth = erosionLevels where
    m x = x `mod` 20183
    erosionLevels = iterate (m . (+) 16807) depth : map rowLevels (zip erosionLevels [1..])
    rowLevels ((x0:xs), iy)
        | iy == ty  = scanl (\x (y, ix) -> m ((if ix == tx then 0 else x * y) + depth)) (m (x0 + 48271)) (zip xs [1..])
        | otherwise = scanl (\x y -> m (x * y + depth)) (m (x0 + 48271)) xs

genRiskBoard :: Pos -> Pos -> Int -> RiskBoard
genRiskBoard target (rx, ry) depth = board' where
    erosionLevels = genErosionLevels target depth
    sliced = take (ry+1) $ map (take (rx+1)) erosionLevels
    board = Vec.fromList $ map Vec.fromList sliced
    board' = imapB (\_ x -> x `mod` 3) board

solve1 :: Pos -> Int -> Int
solve1 target depth = Vec.sum $ Vec.map Vec.sum riskBoard where
    riskBoard = genRiskBoard target target depth

initDP :: Pos -> DPBoard
initDP (rx, ry) = board' where
    board = Vec.replicate (ry+1) $ Vec.replicate (rx+1) (DP inf inf inf)
    board' = updateB board (0, 0) (DP inf 0 inf) -- Start with torch

adjacent :: Pos -> [Pos]
adjacent (x, y) = [(x, y-1), (x-1, y), (x+1, y), (x, y+1)]

moveCost :: (Region, Tool) -> (Region, Tool) -> Int
moveCost _ (Rocky, Neither) = inf
moveCost _ (Wet, Torch) = inf
moveCost _ (Narrow, ClimbGear) = inf
moveCost (_, st) (_, dt)
    | st == dt = 1
    | st /= dt = 8

updateCost :: RiskBoard -> DPBoard -> Pos -> DPBoard
updateCost rBoard dpBoard pos = foldl updateCost' dpBoard (adjacent pos) where
    updateCost' board src
        | isNothing $ board `safeAt` src = board
        | otherwise = updateB board dst updatedDS where
            dst = pos
            sr = rBoard `atReg` src
            dr = rBoard `atReg` dst
            ss = board `at` src
            ds = board `at` dst
            tools = [ClimbGear, Torch, Neither]
            toolCfg = [(st, dt) | st <- tools, dt <- tools]
            withTool ds' (st, dt) = if newVal < oldVal then setValue ds' dt newVal else ds' where
                cost = moveCost (sr, st) (dr, dt)
                oldVal = getValue ds' dt
                newVal = lim $ getValue ss st + cost
            updatedDS = foldl withTool ds toolCfg
        
runDP :: RiskBoard -> DPBoard -> Pos -> DPBoard
runDP rBoard dpBoard (rx, ry) = foldl (\b p -> updateCost rBoard b p) dpBoard posSet where
    posSet = [(x, y) | x <- [0..rx], y <- [0..ry]]
    
-- TODO implement Normal Form evaluation of the board
-- Currently implemented by 'trace' function
runDPEquil :: RiskBoard -> DPBoard -> Pos -> Int -> DPBoard
runDPEquil rBoard dpBoard range n = iterate' (\b -> trace (show $ hash $ show b) $ runDP rBoard b range) dpBoard !! n

solve2 :: Pos -> Int -> Int
solve2 target depth = distances `atTorch` target where -- End with torch
    (tx, ty) = target
    range = (tx + 10, ty + 10)
    riskBoard = genRiskBoard target range depth
    dpBoard = initDP range
    distances = runDPEquil riskBoard dpBoard range 100

readPos :: String -> Pos
readPos s = (l !! 0, l !! 1) where
    l = map read $ words $ map replace s
    replace ',' = ' '
    replace x = x

main' = do
    depth <- read <$> drop 7 <$> getLine
    target <- readPos <$> drop 8 <$> getLine
    putStrLn $ show $ solve1 target depth
    putStrLn "Below are useless hash values.."
    putStrLn $ show $ solve2 target depth
