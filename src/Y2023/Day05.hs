module Y2023.Day05 (main') where

import Data.List (groupBy)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lib.IO
import Lib.Parser qualified as P
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe qualified as Unsafe

-----------------------
-- Type declarations --
-----------------------

data Almanac = Almanac
  { seeds :: [CatId CSeed],
    seedToSoil :: CatMapping CSeed CSoil,
    soilToFertilizer :: CatMapping CSoil CFertilizer,
    fertilizerToWater :: CatMapping CFertilizer CWater,
    waterToLight :: CatMapping CWater CLight,
    lightToTemperature :: CatMapping CLight CTemperature,
    temperatureToHumidity :: CatMapping CTemperature CHumidity,
    humidityToLocation :: CatMapping CHumidity CLocation
  }

newtype CatMapping (srcCat :: Category) (destCat :: Category) = CatMapping [CatMappingOverride] deriving (Show)

data CatMappingOverride = CatMappingOverride
  { destStartId :: Int64,
    srcStartId :: Int64,
    range :: Int64
  }
  deriving (Show)

data Category = CSeed | CSoil | CFertilizer | CWater | CLight | CTemperature | CHumidity | CLocation

newtype CatId (cat :: Category) = CatId Int64 deriving (Show, Eq, Ord) via Int64

data CatIdRange (cat :: Category) = CatIdRange Int64 Int64 deriving (Show)

------------
-- Part 1 --
------------

solve1 :: Almanac -> CatId CLocation
solve1 almanac = viaNonEmpty minimum1 locationIds ?: error "locationIds are empty"
  where
    seedToLocationId = getSeedToLocationFunction almanac
    locationIds = seedToLocationId <$> seeds almanac :: [CatId CLocation]

makeMapFunction :: CatMapping src dest -> CatId src -> CatId dest
makeMapFunction (CatMapping overrides) (CatId srcId) = CatId destId
  where
    mbOverride = find (\(CatMappingOverride _ srcStartId range) -> srcStartId <= srcId && srcId < srcStartId + range) overrides
    destId = case mbOverride of
      Nothing -> srcId
      Just (CatMappingOverride destStartId srcStartId _) -> destStartId + srcId - srcStartId

getSeedToLocationFunction :: Almanac -> CatId CSeed -> CatId CLocation
getSeedToLocationFunction Almanac {..} = resultFunction
  where
    mf = makeMapFunction
    resultFunction = mf humidityToLocation . mf temperatureToHumidity . mf lightToTemperature . mf waterToLight . mf fertilizerToWater . mf soilToFertilizer . mf seedToSoil

------------
-- Part 2 --
------------

solve2 :: Almanac -> CatId CLocation
solve2 almanac = viaNonEmpty minimum1 locationStartIds ?: error "locationIds are empty"
  where
    seedToLocationId = getSeedToLocationFunction2 almanac
    seedRanges = changeToRange (seeds almanac) :: [CatIdRange CSeed]
    locationIdRanges = seedToLocationId =<< seedRanges :: [CatIdRange CLocation]
    locationStartIds = (\(CatIdRange startId _) -> CatId startId) <$> locationIdRanges :: [CatId CLocation]

changeToRange :: [CatId CSeed] -> [CatIdRange CSeed]
changeToRange [] = []
changeToRange (CatId startId : CatId range : catIds) = CatIdRange startId (startId + range - 1) : changeToRange catIds
changeToRange _ = error "Invalid input"

getSeedToLocationFunction2 :: Almanac -> CatIdRange CSeed -> [CatIdRange CLocation]
getSeedToLocationFunction2 Almanac {..} = resultFunction
  where
    mf = makeMapFunction2
    resultFunction seed = mf humidityToLocation =<< mf temperatureToHumidity =<< mf lightToTemperature =<< mf waterToLight =<< mf fertilizerToWater =<< mf soilToFertilizer =<< mf seedToSoil seed

-- There are many things to optimize such as sorting, range merge,
-- but this solution already looks like too complicated...
makeMapFunction2 :: forall src dest. CatMapping src dest -> CatIdRange src -> [CatIdRange dest]
makeMapFunction2 (CatMapping overrides) srcIdRange = destIdRanges
  where
    accumResult = second join $ mapAccumL accumFn [srcIdRange] overrides :: ([CatIdRange src], [CatIdRange dest])
    accumFn :: [CatIdRange src] -> CatMappingOverride -> ([CatIdRange src], [CatIdRange dest])
    accumFn srcIdRanges override = bimap join catMaybes $ unzip overrideApplied
      where
        overrideApplied :: [([CatIdRange src], Maybe (CatIdRange dest))]
        overrideApplied = applyOverride override <$> srcIdRanges

    (remainingRanges, overridenRanges) = accumResult
    identityMapping = coerce :: CatIdRange src -> CatIdRange dest
    nonOverridenRanges = identityMapping <$> remainingRanges
    destIdRanges = nonOverridenRanges <> overridenRanges

applyOverride :: CatMappingOverride -> CatIdRange src -> ([CatIdRange src], Maybe (CatIdRange dest))
applyOverride override srcIdRange = (remainingRanges, overridenRange)
  where
    (CatMappingOverride destStartId srcStartId range) = override
    (CatIdRange startId endId) = srcIdRange
    srcEndId = srcStartId + range - 1
    destEndId = destStartId + range - 1

    intersectStartId = max startId srcStartId
    intersectEndId = min endId srcEndId
    overridenRange =
      if intersectStartId <= intersectEndId
        then Just $ CatIdRange (destStartId + intersectStartId - srcStartId) (destStartId + intersectEndId - srcStartId)
        else Nothing

    remainingRanges = case getOverlapType override srcIdRange of
      NoOverlap -> [srcIdRange]
      CompleteOverlap -> [CatIdRange startId (destStartId - 1), CatIdRange (destEndId + 1) endId]
      CompleteInclusion -> []
      PartialOverlap ->
        if startId < srcStartId
          then [CatIdRange startId (destStartId - 1)]
          else [CatIdRange (destEndId + 1) endId]

data OverlapType = NoOverlap | CompleteOverlap | CompleteInclusion | PartialOverlap

getOverlapType :: CatMappingOverride -> CatIdRange src -> OverlapType
getOverlapType (CatMappingOverride _ srcStartId range) (CatIdRange startId endId)
  | srcEndId < startId || endId < srcStartId = NoOverlap
  | startId < srcStartId && srcEndId < endId = CompleteOverlap
  | srcStartId <= startId && endId <= srcEndId = CompleteInclusion
  | otherwise = PartialOverlap
  where
    srcEndId = srcStartId + range - 1

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  almanac <- parseAlmanac <$> readFileLines "inputs/Y2023/Day05.txt" :: IO Almanac
  print $ solve1 almanac
  print $ solve2 almanac

parseAlmanac :: [Text] -> Almanac
parseAlmanac lines = almanac
  where
    almanacSections = fromList $ sections lines :: Vector [Text]
    seeds :: [CatId CSeed]
    seeds = fmap (CatId . fromInteger . readInteger) . drop 1 . words . Unsafe.head $ almanacSections V.! 0
    parseCatMapping :: Int -> CatMapping src dest
    parseCatMapping sectionIdx = CatMapping . fmap parseCatMappingOverride . drop 1 $ almanacSections V.! sectionIdx
    almanac =
      Almanac
        { seeds = seeds,
          seedToSoil = parseCatMapping 1,
          soilToFertilizer = parseCatMapping 2,
          fertilizerToWater = parseCatMapping 3,
          waterToLight = parseCatMapping 4,
          lightToTemperature = parseCatMapping 5,
          temperatureToHumidity = parseCatMapping 6,
          humidityToLocation = parseCatMapping 7
        }

parseCatMappingOverride :: Text -> CatMappingOverride
parseCatMappingOverride line = CatMappingOverride {..}
  where
    [destStartId, srcStartId, range] = fmap (fromInteger . readInteger) . words $ line

sections :: [Text] -> [[Text]]
sections = filter (/= [""]) . groupBy (\a b -> a /= "" && b /= "")
