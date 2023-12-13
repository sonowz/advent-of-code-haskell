module Y2023.Day12 (main') where

import Data.Text (splitOn)
import Lib.IO
import Lib.Parser qualified as P
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple

-----------------------
-- Type declarations --
-----------------------

data ConditionRecord = ConditionRecord
  { record :: Conditions,
    damagedCounts :: DamagedCounts
  }

type Conditions = [Condition]

type DamagedCounts = [Int]

data Condition = Operational | Damaged | Unknown deriving (Show, Eq, Ord)

------------
-- Part 1 --
------------

solve1 :: [ConditionRecord] -> Int
solve1 conditionRecords = sum $ possibleRecordCount <$> conditionRecords

type MemoState = Map (Int, Int) Int

possibleRecordCount :: ConditionRecord -> Int
possibleRecordCount (ConditionRecord record damagedCounts) = evalState action mempty
  where
    action :: State MemoState Int
    action = sum <$> mapM (go memoize damagedCounts) (dropOperationals record)

    go :: (DamagedCounts -> Conditions -> State MemoState Int) -> DamagedCounts -> Conditions -> State MemoState Int
    go f [] cs =
      if all couldBeOperational cs
        then pure 1
        else pure 0
    go f [count] cs =
      if length cs >= count && all couldBeDamaged (take count cs)
        then f [] (drop count cs)
        else pure 0
    go f (count : dc) cs =
      if length cs > count && all couldBeDamaged (take count cs) && (couldBeOperational <$> viaNonEmpty head (drop count cs)) == Just True
        then sum <$> mapM (f dc) (dropOperationals (drop (count + 1) cs))
        else pure 0

    dropOperationals :: Conditions -> [Conditions]
    dropOperationals [] = []
    dropOperationals (Damaged : cs) = [Damaged : cs]
    dropOperationals (Operational : cs) = dropOperationals cs
    dropOperationals (Unknown : cs) = (Unknown : cs) : dropOperationals cs

    memoize :: DamagedCounts -> Conditions -> State MemoState Int
    memoize dc cs = do
      memoMap <- get
      let dcLength = length dc
          csLength = length cs
      case memoMap !? (dcLength, csLength) of
        Just x -> pure x
        Nothing -> do
          x <- go memoize dc cs
          modify' (insert (dcLength, csLength) x)
          pure x

couldBeDamaged :: Condition -> Bool
couldBeDamaged Damaged = True
couldBeDamaged Unknown = True
couldBeDamaged _ = False

couldBeOperational :: Condition -> Bool
couldBeOperational Operational = True
couldBeOperational Unknown = True
couldBeOperational _ = False

------------
-- Part 2 --
------------

solve2 :: [ConditionRecord] -> Int
solve2 conditionRecords = sum $ possibleRecordCount . unfoldRecord <$> conditionRecords

unfoldRecord :: ConditionRecord -> ConditionRecord
unfoldRecord (ConditionRecord record damagedCounts) = ConditionRecord record' damagedCounts'
  where
    record' = join . intersperse [Unknown] . replicate 5 $ record
    damagedCounts' = join . replicate 5 $ damagedCounts

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  conditionRecord <- parseConditionRecord <<$>> readFileLines "inputs/Y2023/Day12.txt" :: IO [ConditionRecord]
  print $ solve1 conditionRecord
  print $ solve2 conditionRecord

parseConditionRecord :: Text -> ConditionRecord
parseConditionRecord line = ConditionRecord record damagedCounts
  where
    [recordStr, damagedCountsStr] = words line
    record = parseRecord <$> toString recordStr
    damagedCounts = readInt <$> splitOn "," damagedCountsStr

    parseRecord :: Char -> Condition
    parseRecord '.' = Operational
    parseRecord '#' = Damaged
    parseRecord '?' = Unknown
