import Control.Monad
import Data.Function
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map


type Point = (Int, Int)
type Border = (Int, Int, Int, Int)

_x = fst
_y = snd

manhattanD (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

-- More than 2 nearest point == Nothing
nearestPoint :: [Point] -> Point -> Maybe Point
nearestPoint points x = if minCount == 1 then find ((/=) nullPoint) points' else Nothing where
    distances = map (manhattanD x) points
    minD = minimum distances
    minCount = length $ filter ((==) minD) distances
    nullPoint = (maxBound :: Int, maxBound :: Int)
    points' = zipWith (\p d -> if d == minD then p else nullPoint) points distances

getBorder :: [Point] -> Border
getBorder points = (l, b, r, t) where
    xs = _x `map` points
    ys = _y `map` points
    l = minimum xs
    r = maximum xs
    b = minimum ys
    t = maximum ys

-- 3x size of original border
getOuterBorder (l, b, r, t) = (l - w, b - h, r + w, t + h) where
    w = r - l
    h = t - b

getGrid :: Border -> [Point]
getGrid (l, b, r, t) = [(x, y) | x <- [l..r], y <- [b..t]]

getInfPoints :: Border -> [Point] -> Set Point
getInfPoints (l, b, r, t) points = infPoints where
    borderLine = [(x, y) | x <- [l, r], y <- [b..t]] ++ [(x, y) | x <- [l..r], y <- [b, t]]
    insertNearest s bp = maybe s (\np -> Set.insert np s) (nearestPoint points bp)
    infPoints = foldl insertNearest Set.empty borderLine


solve1 :: [Point] -> Int
solve1 points = maximum $ map snd (Map.toList nonInfMap) where
    border = getBorder points
    grid = getGrid border
    pointMap = Map.fromList (zip points (repeat 0)) -- Point -> NearestCount
    addNearest m p = maybe m (\np -> Map.update (\c -> Just (c+1)) np m) (nearestPoint points p)
    pointMap' = foldl addNearest pointMap grid
    infPoints = getInfPoints (getOuterBorder border) points
    nonInfMap = Set.foldl (flip Map.delete) pointMap' infPoints

solve2 :: [Point] -> Int
solve2 points = length $ filter (\x -> x < 10000) sumGrid where
    grid = getGrid $ getBorder points
    manhattanSum points p = sum $ map (manhattanD p) points
    sumGrid = map (manhattanSum points) grid


getPoint :: String -> Point
getPoint line = (read $ ints !! 0, read $ ints !! 1) where
    ints = words $ delete ',' line
    
main' = do
    points <- map getPoint <$> replicateM 50 getLine
    putStrLn $ show $ solve1 points
    putStrLn $ show $ solve2 points
    

