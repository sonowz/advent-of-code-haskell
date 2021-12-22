module Y2021.Day22 where

import qualified Data.Map.Strict as M
import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Text
import qualified Text.Show as S

-----------------------
-- Type declarations --
-----------------------

-- [inclusive, exclusive)
data Cuboid = Cuboid
    { minX :: Int
    , maxX :: Int
    , minY :: Int
    , maxY :: Int
    , minZ :: Int
    , maxZ :: Int
    }
    deriving Eq
data Power = PowerOn | PowerOff deriving (Show, Eq)
data RebootStep = RebootStep
    { getCuboid :: Cuboid
    , getPower  :: Power
    }
    deriving Show

-- Voxel whose coordinates are compressed using 'Map'
type CompVoxel = Map Int (Map Int (Map Int Power))


instance S.Show Cuboid where
    show = show . cuboidToList

cuboidToList :: Cuboid -> [Int]
cuboidToList Cuboid {..} = [minX, maxX, minY, maxY, minZ, maxZ]


------------
-- Part 1 --
------------

solve1 :: [RebootStep] -> Integer
solve1 rebootSteps = calcPowerOnVolume voxel  where
    rebootSteps'                = filter inside50 rebootSteps
    (xSlices, ySlices, zSlices) = extractBoundaries rebootSteps'
    !initVoxel                  = constructCompVoxel xSlices ySlices zSlices
    voxel = foldl' (\(!voxel) step -> runStep step voxel) initVoxel rebootSteps'

inside50 :: RebootStep -> Bool
inside50 (getCuboid -> cuboid) = all (\x -> -50 <= x && x <= 50) boundaries
    where boundaries = cuboidToList cuboid

-- Construct voxel whose coordinates are compressed by designated slices
constructCompVoxel :: Set Int -> Set Int -> Set Int -> CompVoxel
constructCompVoxel xSlices ySlices zSlices = xyzMap  where
    zMap   = M.fromSet (const PowerOff) zSlices :: Map Int Power
    yzMap  = M.fromSet (const zMap) ySlices :: Map Int (Map Int Power)
    xyzMap = M.fromSet (const yzMap) xSlices :: Map Int (Map Int (Map Int Power))

runStep :: RebootStep -> CompVoxel -> CompVoxel
runStep rebootStep voxel = voxel'  where
    Cuboid {..} = getCuboid rebootStep
    power       = getPower rebootStep
    voxel'      = updateBetween updateYZ minX maxX voxel :: CompVoxel
    updateYZ =
        updateBetween updateZ minY maxY :: Map Int (Map Int Power) -> Map Int (Map Int Power)
    updateZ = updateBetween (const power) minZ maxZ :: Map Int Power -> Map Int Power

-- Update values of 'map' whose key is between 'min' and 'max'
updateBetween :: forall k a . Ord k => (a -> a) -> k -> k -> Map k a -> Map k a
updateBetween f min max map = foldl' foldFn map targetKeys  where
    targetKeys = takeWhile (< max) . dropWhile (< min) $ keys map :: [k]
    foldFn map k = M.update (Just . f) k map

-- Returns set of all ('x', 'y', 'z') coordinates that appear in 'RebootStep's
extractBoundaries :: [RebootStep] -> (Set Int, Set Int, Set Int)
extractBoundaries rebootSteps = foldl' mergeSet (mempty, mempty, mempty) extracted  where
    mergeSet (x1, y1, z1) (x2, y2, z2) = (x1 <> x2, y1 <> y2, z1 <> z2)
    extracted = extract <$> rebootSteps
    extract :: RebootStep -> (Set Int, Set Int, Set Int)
    extract (getCuboid -> Cuboid{..}) = ([minX, maxX], [minY, maxY], [minZ, maxZ])

calcPowerOnVolume :: CompVoxel -> Integer
calcPowerOnVolume = sumX  where
    ranges :: [a] -> [(a, a)]
    ranges []        = []
    ranges (hd : tl) = zip (hd : tl) tl
    powerToInt :: Power -> Integer
    powerToInt PowerOn  = 1
    powerToInt PowerOff = 0
    foldFn :: Integer -> ((Int, Int), Integer) -> Integer
    foldFn acc ((start, end), x) = acc + x * (fromIntegral end - fromIntegral start)
    sumZ :: Map Int Power -> Integer
    sumZ zMap = foldl' foldFn 0 $ zip (ranges (keys zMap)) (powerToInt <$> elems zMap)
    sumY :: Map Int (Map Int Power) -> Integer
    sumY yzMap = foldl' foldFn 0 $ zip (ranges (keys yzMap)) (sumZ <$> elems yzMap)
    sumX :: CompVoxel -> Integer
    sumX xyzMap = foldl' foldFn 0 $ zip (ranges (keys xyzMap)) (sumY <$> elems xyzMap)


------------
-- Part 2 --
------------

-- WARNING! This solution takes about ~2 minutes and requires ~20GB RAM.
-- TODO: do not mutate the 'CompVoxel' in 'runStep' and use 'STRef'

solve2 :: [RebootStep] -> Integer
solve2 rebootSteps = calcPowerOnVolume voxel  where
    (xSlices, ySlices, zSlices) = extractBoundaries rebootSteps
    !initVoxel = constructCompVoxel xSlices ySlices zSlices
    voxel = foldl' (\(!voxel) step -> runStep step voxel) initVoxel rebootSteps


--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    rebootSteps <- parseRebootStep <<$>> readFileLines "inputs/Y2021/Day22.txt" :: IO [RebootStep]
    print $ solve1 rebootSteps
    print $ solve2 rebootSteps

parseRebootStep :: Text -> RebootStep
parseRebootStep text = fromRight (error "parse error") $ parse parserRebootStep "" text

number :: Parser Int
number = do
    sign   <- optionMaybe (char '-')
    digits <- many1 digit
    return . readInt . toText $ maybe digits (: digits) sign

parserRebootStep :: Parser RebootStep
parserRebootStep = do
    let parsePower "on"  = PowerOn
        parsePower "off" = PowerOff
        parsePower _     = error "Impossible!"
    power <- parsePower <$> (try (string "on") <|> try (string "off"))
    string " x="
    minX <- number
    string ".."
    maxX <- (+) 1 <$> number -- inclusive -> exclusive
    string ",y="
    minY <- number
    string ".."
    maxY <- (+) 1 <$> number
    string ",z="
    minZ <- number
    string ".."
    maxZ <- (+) 1 <$> number
    return $ RebootStep (Cuboid { .. }) power
