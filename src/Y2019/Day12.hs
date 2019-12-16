module Y2019.Day12 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.Text
import Lib.IO
import Lib.Types

-----------------------
-- Type declarations --
-----------------------

data Moon = Moon Pos Dir deriving (Show)
newtype Pos = Pos (Int, Int, Int) deriving (Eq, Show) via (Int, Int, Int)
newtype Dir = Dir (Int, Int, Int) deriving (Num, Show) via Pos
newtype Acc = Acc (Int, Int, Int) deriving (Num, Show) via Pos

instance Num Pos where
    Pos (x1, y1, z1) + Pos (x2, y2, z2) = Pos (x1 + x2, y1 + y2, z1 + z2)
    Pos (x1, y1, z1) - Pos (x2, y2, z2) = Pos (x1 - x2, y1 - y2, z1 - z2)
    Pos (x1, y1, z1) * Pos (x2, y2, z2) = Pos (x1 * x2, y1 * y2, z1 * z2)
    abs (Pos (x, y, z)) = Pos (abs x, abs y, abs z)
    signum (Pos (x, y, z)) = Pos (signum x, signum y, signum z)
    fromInteger x = Pos (fromInteger x, fromInteger x, fromInteger x)

move (Pos (x, y, z)) (Dir (dx, dy, dz)) = Pos (x + dx, y + dy, z + dz)
accelerate (Dir (dx, dy, dz)) (Acc (ddx, ddy, ddz)) = Dir (dx + ddx, dy + ddy, dz + ddz)

------------
-- Part 1 --
------------

solve1 :: [Moon] -> Int
solve1 moons = sum $ map totalEnergy moons' where moons' = iterate nBodySim moons !! 1000

nBodySim :: [Moon] -> [Moon]
nBodySim moons = map (\m -> changeMoon m (getGravitySum m)) moons  where
    getPos (Moon p v) = p
    getGravitySum m = sum $ map (getGravity (getPos m) . getPos) moons
    changeMoon (Moon p v) a = Moon p' v'      where
        v' = v `accelerate` a
        p' = p `move` v'

getGravity :: Pos -> Pos -> Acc
getGravity (Pos (sx, sy, sz)) (Pos (dx, dy, dz)) = Acc (comp sx dx, comp sy dy, comp sz dz)  where
    comp a b
        | a == b = 0
        | a < b  = 1
        | a > b  = -1

totalEnergy :: Moon -> Int
totalEnergy (Moon (Pos (px, py, pz)) (Dir (dx, dy, dz))) = potentialEnergy * kineticEnergy  where
    potentialEnergy = sum . map abs $ [px, py, pz]
    kineticEnergy   = sum . map abs $ [dx, dy, dz]

------------
-- Part 2 --
------------

solve2 :: [Moon] -> Integer
solve2 moons = period + offset  where
    moonTimeSeries          = iterate nBodySim moons :: [[Moon]]
    moonStateTimeSeries     = makeStates moonTimeSeries :: [[MoonState]]
    xMoonStates             = map (!! 0) moonStateTimeSeries
    yMoonStates             = map (!! 1) moonStateTimeSeries
    zMoonStates             = map (!! 2) moonStateTimeSeries
    xCycle                  = findCycle xMoonStates :: MoonCycle
    yCycle                  = findCycle yMoonStates :: MoonCycle
    zCycle                  = findCycle zMoonStates :: MoonCycle
    MoonCycle period offset = mergeCycle xCycle $ mergeCycle yCycle zCycle

type MoonStateMap = HashMap MoonState Integer
type MoonState = [Int] -- [(x|y|z) position of moons] ++ [(x|y|z) velocity of moons]
data MoonCycle = MoonCycle Period Offset deriving (Show)
type Period = Integer
type Offset = Integer

t1 (x, y, z) = x
t2 (x, y, z) = y
t3 (x, y, z) = z

makeStates :: [[Moon]] -> [[MoonState]]
makeStates (moons : futures) = xyzStates : makeStates futures  where
    xyzStates = [makeState t1, makeState t2, makeState t3] :: [MoonState]
    getPos (Moon pos dir) = pos
    getDir (Moon pos dir) = dir
    poss = map (un . getPos) moons :: [(Int, Int, Int)]
    dirs = map (un . getDir) moons :: [(Int, Int, Int)]
    makeState coordFn = map coordFn poss <> map coordFn dirs :: MoonState

findCycle :: [MoonState] -> MoonCycle
findCycle moonStates = findCycle' mempty moonStates 0  where
    findCycle' :: MoonStateMap -> [MoonState] -> Integer -> MoonCycle
    findCycle' stateMap (state : futures) t = case stateMap !? state of
        Just prevT -> MoonCycle (t - prevT) prevT
        Nothing    -> findCycle' (insert state t stateMap) futures (t + 1)

mergeCycle :: MoonCycle -> MoonCycle -> MoonCycle
mergeCycle (MoonCycle p1 o1) (MoonCycle p2 o2) = MoonCycle period offset  where
    period = lcm p1 p2 -- Least Common Multiple
    offset = max o1 o2

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    moons <- parseMoon <<$>> readFileLines "inputs/Y2019/Day12.txt" :: IO [Moon]
    print $ solve1 moons
    print $ solve2 moons

parserMoon :: Parser Moon
parserMoon = do
    string "<x="
    x <- parserSignedInt
    string ", y="
    y <- parserSignedInt
    string ", z="
    z <- parserSignedInt
    string ">"
    return $ Moon (Pos (x, y, z)) (Dir (0, 0, 0))  where
    parserSignedInt :: Parser Int
    parserSignedInt = do
        sign  <- fromMaybe 1 <$> optionMaybe (char '-' >> return (-1))
        value <- toText <$> many1 digit
        return (sign * readInt value)

parseMoon text = fromRight (error "parse error") $ parse parserMoon "" text
