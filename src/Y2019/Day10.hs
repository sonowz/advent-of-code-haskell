module Y2019.Day10 where

import Prelude (atan2)
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import qualified Text.Show as S
import Lib.IO
import Lib.Types
import Lib.Vector2D

-- Warning: 'x' and 'y' were swapped in this solution

-----------------------
-- Type declarations --
-----------------------

newtype Pos = Pos (Int, Int) deriving (Pos2D, Ord, Eq, Show) via (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Pos2D, Ord, Eq, Num) via Pos

instance Num Pos where
    Pos (x1, y1) + Pos (x2, y2) = Pos (x1 + x2, y1 + y2)
    Pos (x1, y1) - Pos (x2, y2) = Pos (x1 - x2, y1 - y2)
    Pos (x1, y1) * Pos (x2, y2) = Pos (x1 * x2, y1 * y2)
    abs (Pos (x, y)) = Pos (abs x, abs y)
    signum (Pos (x, y)) = Pos (signum x, signum y)
    fromInteger x = Pos (fromInteger x, fromInteger x)

move (Pos (x, y)) (Dir (dx, dy)) = Pos (x + dx, y + dy)

type SpaceMap = Vector (Vector MapObj)
data MapObj = Empty | Asteroid deriving (Eq)

instance S.Show MapObj where
    show Empty    = "."
    show Asteroid = "#"

type VisitGrid = Vector (Vector Visit)
data Visit = Visited | NotVisited

------------
-- Part 1 --
------------

solve1 :: SpaceMap -> Int
solve1 grid = fromJust $ viaNonEmpty maximum1 detectCounts  where
    asteroidPos = asteroidPositions grid
    countAsteroid g = sum $ V.length . V.filter (== Asteroid) <$> g
    detectCounts = map (\pos -> countAsteroid $ filterLineOfSight pos grid) asteroidPos

newtype ShootingLine = SL [Pos]

filterLineOfSight :: Pos -> SpaceMap -> SpaceMap
filterLineOfSight pos grid = losGrid  where
    initVisitGrid = V.map (V.map (const NotVisited)) grid
    initLOSGrid   = V.map (V.map (const Empty)) grid
    shootLine :: Pos -> Dir -> Int -> VisitGrid -> (VisitGrid, ShootingLine)
    shootLine basePos baseDir n visitGrid = line      where
        dir        = Dir (n, n) * baseDir
        pos        = basePos `move` dir
        visitGrid' = set visitGrid pos Visited
        line       = case visitGrid `at` pos of
            Just NotVisited -> under ((:) pos) <$> shootLine basePos baseDir (n + 1) visitGrid'
            Just Visited    -> shootLine basePos baseDir (n + 1) visitGrid
            Nothing         -> (visitGrid, SL [])
    d                  = let (sx, sy) = size2D grid in sx + sy
    dirs               = concatMap dirAtDistance ([1 .. d] :: [Int])
    (_, shootingLines) = foldl'
        (\(visit, sls) dir -> flip (:) sls <$> shootLine pos dir 1 visit)
        (initVisitGrid, [])
        dirs
    writeLOSGrid los shootingLine =
        maybe los (\p -> set los p Asteroid) $ collision grid shootingLine
    losGrid = foldl' writeLOSGrid initLOSGrid shootingLines

dirAtDistance :: Int -> [Dir]
dirAtDistance d = concat ([ toAllQuadrant (Dir (x, d - x)) | x <- [1 .. d] ] :: [[Dir]])
    where toAllQuadrant (Dir (x, y)) = map Dir [(x, y), (-x, -y), (y, -x), (-y, x)]

collision :: SpaceMap -> ShootingLine -> Maybe Pos
collision grid (SL sl) = collision' sl  where
    collision' (p : ps) = case grid `at` p of
        Just Asteroid -> Just p
        Just Empty    -> collision' ps
        Nothing       -> Nothing
    collision' _ = Nothing

asteroidPositions :: SpaceMap -> [Pos]
asteroidPositions = imapMaybeL2D (\pos x -> if x == Asteroid then Just pos else Nothing)

------------
-- Part 2 --
------------

solve2 :: SpaceMap -> Int
solve2 grid = let Pos (x, y) = vaporized200 in 100 * y + x  where
    asteroidPos = asteroidPositions grid
    countAsteroid g = sum $ V.length . V.filter (== Asteroid) <$> g
    posLOSes     = map (\pos -> (pos, filterLineOfSight pos grid)) asteroidPos
    stationPos   = fst $ maximumBy (compare `on` countAsteroid . snd) posLOSes
    vaporized200 = vaporize stationPos grid !! (200 - 1)

vaporize :: Pos -> SpaceMap -> [Pos]
vaporize station grid
    | asteroidPositions grid == [] = []
    | otherwise                    = targets <> vaporize station grid'  where
    targets = findTargets360 station grid
    grid'   = foldl' (\g t -> set g t Empty) grid targets

findTargets360 :: Pos -> SpaceMap -> [Pos]
findTargets360 station grid = sortOn (angleFromNorth station) targets  where
    targets = asteroidPositions $ filterLineOfSight station grid
    angleFromNorth (Pos (sx, sy)) (Pos (dx, dy)) =
        -(atan2 (fromIntegral $ dy - sy) (fromIntegral $ dx - sx))

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    spaceMap <- parseSpaceMap <$> readFileLines "inputs/Y2019/Day10.txt" :: IO SpaceMap
    print $ solve1 spaceMap
    print $ solve2 spaceMap

parseSpaceMap :: [Text] -> SpaceMap
parseSpaceMap lines = V.fromList $ map (V.unfoldr parseLine) stringLines  where
    stringLines = map toString lines
    parseLine ('.' : xs) = Just (Empty, xs)
    parseLine ('#' : xs) = Just (Asteroid, xs)
    parseLine _          = Nothing
