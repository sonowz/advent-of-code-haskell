module Y2021.Day17 where

import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import qualified Relude.Unsafe as Unsafe
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text

-----------------------
-- Type declarations --
-----------------------

data TargetArea = TargetArea Int Int Int Int

newtype Pos = Pos (Int, Int) deriving (Ord, Eq, Show) via (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Num, Show) via Pos

instance Num Pos where
    Pos (x1, y1) + Pos (x2, y2) = Pos (x1 + x2, y1 + y2)
    Pos (x1, y1) - Pos (x2, y2) = Pos (x1 - x2, y1 - y2)
    Pos (x1, y1) * Pos (x2, y2) = Pos (x1 * x2, y1 * y2)
    abs (Pos (x, y)) = Pos (abs x, abs y)
    signum (Pos (x, y)) = Pos (signum x, signum y)
    fromInteger x = Pos (fromInteger x, fromInteger x)

move :: Pos -> Dir -> Pos
move (Pos (x, y)) (Dir (dx, dy)) = Pos (x + dx, y + dy)

------------
-- Part 1 --
------------

solve1 :: TargetArea -> Int
solve1 targetArea = maxY  where
    Dir (_, dy) = Unsafe.fromJust $ simulateMaxY targetArea
    maxY        = sum ([1 .. dy] :: [Int])

-- Returns velocity whose y is highest
simulateMaxY :: TargetArea -> Maybe Dir
simulateMaxY targetArea = find (canReachTargetArea targetArea) parameters  where
    (maxdx, maxdy) = calcMaxBound targetArea
    parameters = [ Dir (dx, dy) | dy <- sortDesc [(-maxdy) .. maxdy], dx <- [1 .. maxdx] ] :: [Dir]
    sortDesc = sortBy (flip compare)

canReachTargetArea :: TargetArea -> Dir -> Bool
canReachTargetArea targetArea velocity = any inArea boundedTrajectory  where
    TargetArea minX maxX minY maxY = targetArea
    inArea (Pos (x, y)) = minX <= x && x <= maxX && minY <= y && y <= maxY
    -- TODO: this assumes that minY < 0
    boundedTrajectory = takeWhile (\(Pos (x, y)) -> y >= minY) (trajectory velocity)

-- Lazy infinite list
trajectory :: Dir -> [Pos]
trajectory = go (Pos (0, 0))  where
    go pos velocity@(Dir (dx, dy)) = pos' : go pos' newVelocity      where
        pos'        = move pos velocity
        newVelocity = Dir (max (dx - 1) 0, dy - 1)

-- Calculate maximum bound of answer (x, y)
-- if (x > maxX), trajectory passes TargetArea at first step
-- if (minY > 0 && y > minY), trajectory passes TargetArea at first step
-- if (minY < 0 && y > minY), trajectory passes TargetArea right after it falls to y=0
-- Note that y-trajectory is symmetrical to higest y position,
-- thus it always touches y=0 when shoot upwards
calcMaxBound :: TargetArea -> (Int, Int)
calcMaxBound (TargetArea _ maxX minY _) = (maxX, abs minY)

------------
-- Part 2 --
------------

solve2 :: TargetArea -> Int
solve2 = length . simulate

-- Returns all velocities that goes into TargetArea
simulate :: TargetArea -> [Dir]
simulate targetArea = filter (canReachTargetArea targetArea) parameters  where
    (maxdx, maxdy) = calcMaxBound targetArea
    parameters     = [ Dir (dx, dy) | dy <- [(-maxdy) .. maxdy], dx <- [1 .. maxdx] ] :: [Dir]

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    targetArea <- parseTargetArea <$> readFileLines "inputs/Y2021/Day17.txt" :: IO TargetArea
    print $ solve1 targetArea
    print $ solve2 targetArea

parseTargetArea :: [Text] -> TargetArea
parseTargetArea text = fromRight (error "parse error") $ parse parserTargetArea "" (unlines text)

number :: Parser Int
number = do
    sign   <- optionMaybe (char '-')
    digits <- many1 digit
    return . readInt . toText $ maybe digits (: digits) sign

parserTargetArea :: Parser TargetArea
parserTargetArea =
    TargetArea
        <$> (string "target area: x=" >> number)
        <*> (string ".." >> number)
        <*> (string ", y=" >> number)
        <*> (string ".." >> number)
