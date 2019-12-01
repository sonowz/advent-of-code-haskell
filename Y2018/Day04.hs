import Control.Monad
import Data.Function
import Data.Either
import Data.List
import Data.Vector (Vector, (//))
import qualified Data.Vector as V
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Clock


type Record = (LocalTime, Msg)
data Msg = Guard Int | Sleep | Wakeup

type Id = Int
type SleepTime = Vector Int

_id = fst
_st = snd

num :: Parser Int
num = read <$> many1 digit

dateParser :: Parser LocalTime
dateParser = do
    yyyy <- num
    void $ char '-'
    mm <- num
    void $ char '-'
    dd <- num
    void $ char ' '
    h <- num
    void $ char ':'
    m <- num
    return LocalTime {
        localDay = fromGregorian (toInteger yyyy) mm dd,
        localTimeOfDay = TimeOfDay {
            todHour = h,
            todMin = m,
            todSec = 0
        }
  }

recordParser :: Parser Record
recordParser = do
    void $ char '['
    date <- dateParser
    void $ char ']'
    void $ char ' '
    msg <- pGuard <|> pSleep <|> pWakeup
    void $ many1 anyChar
    return (date, msg) where
        pGuard = do
            void $ string "Guard #"
            id <- num
            return $ Guard id
        pSleep = string "falls" >> return Sleep
        pWakeup = string "wakes" >> return Wakeup

getRecord l = fromRight (error "ERROR") $ parse recordParser "" l


groupBy' :: (a -> Maybe b) -> [a] -> [(b, [a])]
groupBy' f (x:xs) = (case f x of
    Just id -> groupBy'' id xs []
    Nothing -> groupBy' f xs) where
        groupBy'' id [] acc = [(id, acc)]
        groupBy'' id (x:xs) acc = case f x of
            Just id' -> (id, acc) : (groupBy'' id' xs []) 
            Nothing -> groupBy'' id xs (acc ++ [x])

emptySleepTime :: SleepTime
emptySleepTime = V.replicate 60 0
  
toSleepTimes :: [Record] -> SleepTime
toSleepTimes ((t1, Sleep):(t2, Wakeup):xs) =
    let m1 = todMin $ localTimeOfDay t1
        m2 = todMin $ localTimeOfDay t2 in
    (toSleepTimes xs) // [(t, 1) | t <- [m1..(m2-1)]]
toSleepTimes [] = emptySleepTime

getTotalSleep x = sum $ map V.sum $ _st x

-- (minute, count)
getMaxSleepMin :: [SleepTime] -> (Int, Int)
getMaxSleepMin x = (V.maxIndex sleepCounts, V.maximum sleepCounts) where
    sleepCounts = V.foldl sumTime emptySleepTime (V.fromList x)
    sumTime s x = V.zipWith (+) s x

-- [[(id, 0~60 min sleep)]]
getGuardSleeps :: [Record] -> [(Id, [SleepTime])]
getGuardSleeps records = guardSleeps' where
    guard (_, Guard id) = Just id
    guard _ = Nothing
    guardRecords = groupBy' guard records
    guardSleepChunks = map (\(id, r) -> (id, toSleepTimes r)) guardRecords
    guardSleeps = groupBy ((==) `on` _id) $ sortOn _id guardSleepChunks :: [[(Id, SleepTime)]]
    guardSleeps' = map (\x -> (_id $ head x, map _st x)) guardSleeps


solve1 :: [Record] -> Int
solve1 records = (_id maxSleepGuard) * (fst $ getMaxSleepMin $ _st maxSleepGuard) where
    guardSleeps = getGuardSleeps records
    maxSleepGuard = maximumBy (compare `on` getTotalSleep) guardSleeps

solve2 :: [Record] -> Int
solve2 records = (_id maxSleepGuard) * (fst $ _st maxSleepGuard) where
    guardSleeps = getGuardSleeps records
    maxSleeps = map (\x -> (_id x, getMaxSleepMin $ _st x)) guardSleeps
    maxSleepGuard = maximumBy (compare `on` (snd . _st)) maxSleeps

main' = do
    records <- map getRecord <$> replicateM 1143 getLine
    records' <- return $ sortOn fst records
    putStrLn $ show $ solve1 records'
    putStrLn $ show $ solve2 records'
