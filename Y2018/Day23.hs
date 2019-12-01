import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Text.Scanf
import qualified Data.PQueue.Max as PQ

type Bound = (Int, Int)
type Pos = (Int, Int, Int)
type Sphere = (Pos, Int) -- This is actually a octahedron in Euclidean space
type Cube = (Pos, Int) -- Minimum point, length
data BoundCube = BC Cube Int -- Bounding cube, # of inRange spheres

instance Eq BoundCube where
    (BC c1 n1) == (BC c2 n2) = c1 == c2 && n1 == n2

-- Priority comparison
instance Ord BoundCube where
    compare (BC c1 n1) (BC c2 n2)
        | n1 /= n2 = compare n1 n2
        | otherwise = compare d2 d1 where -- Closer one has more priority
            d1 = minimumBy (compare `on` (manhattanD (0, 0, 0))) (cubeVertices c1)
            d2 = minimumBy (compare `on` (manhattanD (0, 0, 0))) (cubeVertices c2)

manhattanD (x1, y1, z1) (x2, y2, z2) = (abs $ x1 - x2) + (abs $ y1 - y2) + (abs $ z1 - z2)

inRangeSphere (tPos, tRad) pos = manhattanD tPos pos <= tRad
inRangeCube ((tx, ty, tz), tl) (x, y, z) = btw tx x (tx+tl) && btw ty y (ty+tl) && btw tz z (tz+tl) where
    btw t1 x t2 = t1 <= x && x <= t2

add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

solve1 :: [Sphere] -> Int
solve1 spheres = length $ filter inStRange spheres where
    strongest = maximumBy (compare `on` snd) spheres
    inStRange (pos, _) = inRangeSphere strongest pos


sumTrue :: [Bool] -> Int
sumTrue l = sum $ map (\x -> if x == True then 1 else 0) l

sphereVertices :: Sphere -> [Pos]
sphereVertices (pos, r) = map (add pos) offsets where
    offsets = [(-r, 0, 0), (r, 0, 0), (0, -r, 0), (0, r, 0), (0, 0, -r), (0, 0, r)]

cubeVertices :: Cube -> [Pos]
cubeVertices (pos, l) = map (add pos) offsets where
    sgn = [0, l]
    offsets = [(x, y, z) | x <- sgn, y <- sgn, z <- sgn]

hasIntersect :: Cube -> Sphere -> Bool
hasIntersect cube sphere =
    any (inRangeSphere sphere) (cubeVertices cube) || any (inRangeCube cube) (sphereVertices sphere)
    
octDivide :: Cube -> [Cube]
octDivide (pos, l) = map makeCube newPos where
    nl = l `div` 2
    newPos = cubeVertices (pos, nl)
    makeCube p = (p, nl)

genBound :: [Sphere] -> Cube -> BoundCube
genBound spheres cube = BC cube n where
    n = sumTrue $ map (hasIntersect cube) spheres

-- DISCLAIMER: I failed to solve Part 2, and searched for a solution.
-- Binary search cubes in 3d space (like octree), maintaining them in a priority queue.
-- https://raw.githack.com/ypsu/experiments/master/aoc2018day23/vis.html
solve2 :: [Sphere] -> Int
solve2 spheres = manhattanD (0, 0, 0) maxPoint where
    inf = 2^31
    initBoundCube = BC ((-inf, -inf, -inf), 2 * inf) (length spheres)
    maxPoint = searchLoop (PQ.singleton initBoundCube)
    searchLoop pq = if snd cube == 0 then fst cube else searchLoop pq'' where
        (top, pq') = PQ.deleteFindMax pq
        (BC cube n) = top
        newCubes = map (genBound spheres) (octDivide cube)
        pq'' = foldl (flip PQ.insert) pq' newCubes
    
    
getSphere :: IO Sphere
getSphere = do
    line <- getLine
    (x :+ y :+ z :+ r :+ _) <- return $ fromJust $ readPosRad line
    return ((x, y, z), r) where
        readPosRad = scanf $ fmt_ $ "pos=<"%int.","%int.","%int.">, r="%int

main' = do
    spheres <- replicateM 1000 getSphere
    putStrLn $ show $ solve1 spheres
    putStrLn $ show $ solve2 spheres
