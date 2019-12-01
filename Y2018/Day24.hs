import Control.Monad
import Control.Applicative hiding ((<|>))
import Data.Function
import Data.List
import Data.Either
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec hiding (try)
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Error
import Debug.Trace

-- sortDesc = sortBy (flip compare)
sortDescBy f = sortBy (flip f)

data Group = Group
    { units :: Int
    , hp :: Int
    , immunes :: [String]
    , weaks :: [String]
    , atk :: Int
    , atkType :: String
    , initiative :: Int
    , isInfection :: Bool
    } deriving (Show, Eq)

instance Ord Group where
    compare g1 g2
        | effPower g1 /= effPower g2 = compare (effPower g1) (effPower g2)
        | otherwise = compare (initiative g1) (initiative g2)

effPower group = units group * atk group
effDamage attacker defender
    | atkType attacker `elem` immunes defender = 0
    | atkType attacker `elem` weaks defender   = effPower attacker * 2
    | otherwise                                = effPower attacker

opposites g1 g2 = isInfection g1 /= isInfection g2

dealDamage :: Group -> Group -> Maybe Group
dealDamage attacker defender = if units defender <= damUnits then Nothing else Just defender' where
    dam = effDamage attacker defender
    damUnits = dam `div` hp defender
    defender' = defender { units = units defender - damUnits }

selectTarget :: Group -> [(Int, Group)] -> (Maybe Int, [(Int, Group)])
selectTarget attacker [] = (Nothing, [])
selectTarget attacker groups = if maxDamage <= 0 then (Nothing, groups) else (Just (fst target), groups') where
    effDamages = map (\(i, def) -> if opposites attacker def then effDamage attacker def else 0) groups
    maxDamage = maximum effDamages
    targets = map fst $ filter (\(_, dam) -> dam == maxDamage) (zip groups effDamages)
    target = maximumBy (compare `on` snd) targets
    groups' = delete target groups

battleLoop :: Vector Group -> Vector Group
battleLoop groups = id $! groups' where
    assocs = Vec.toList $ Vec.indexed groups
    selectOrders = map fst $ sortDescBy (compare `on` snd) assocs :: [Int]
    foldFn (ts, cands) atki = (ts ++ [t], cands') where
        attacker = groups ! atki
        (t, cands') = selectTarget attacker cands
    targets = fst $ foldl foldFn ([], assocs) selectOrders
    plans = sortDescBy (compare `on` (\(i, j) -> initiative (groups ! i))) $ zip selectOrders targets :: [(Int, Maybe Int)]
    groupState = Vec.map Just groups :: Vector (Maybe Group)
    battle (Just atk) (Just def) = dealDamage atk def
    battle Nothing    d          = d
    foldBattle state (atki, Nothing)   = state
    foldBattle state (atki, Just defi) = state // [(defi, def')] where
        def' = battle (state ! atki) (state ! defi)
    groupState' = foldl foldBattle groupState plans
    groups' = Vec.mapMaybe id groupState'

infectionVictory groups = Vec.all isInfection groups
immuneVictory groups = Vec.all (not . isInfection) groups
hasVictory groups = infectionVictory groups || immuneVictory groups || null groups

doBattle :: [Group] -> [Group]
doBattle groups = Vec.toList $ untilN 100000 hasVictory battleLoop (Vec.fromList groups) where
    untilN n f g ix = snd $ until (\(i, x) -> i >= n || f x) (\(i, x) -> (i+1, g x)) (0, ix) -- Sometimes battles are too long

solve1 :: [Group] -> Int
solve1 groups = sum $ map units (doBattle groups)

giveBoost :: Int -> [Group] -> [Group]
giveBoost n groups = map (\g -> if isInfection g then g else boost g) groups where
    boost g = g { atk = atk g + n }

solve2 :: [Group] -> Int
solve2 groups = sum $ map units (resultByBoost !! minBoost) where
    resultByBoost = map (\n -> doBattle (giveBoost n groups)) [0..]
    minBoost = length $ takeWhile (not . immuneVictory . Vec.fromList) resultByBoost


-- Parsers --


numParser :: Parser Int
numParser = read <$> many1 digit

typeParser :: Parser ([String], [String])
typeParser = do
    void $ char '('
    gWeaks1 <- option [] (string "weak to " >> manyString <* drain (string "; "))
    gImmunes <- option [] (string "immune to " >> manyString <* drain (string "; "))
    gWeaks2 <- option [] (string "weak to " >> manyString)
    void $ string ") "
    return (gImmunes, gWeaks1 ++ gWeaks2) where
        manyString = many1 letter `sepBy1` string ", "
        drain x = x <|> return ""

atkTypeParser :: Parser String
atkTypeParser = do
    void $ char ' '
    gAtkType <- many1 letter
    if gAtkType == "damage" then parserZero else return gAtkType

groupParser :: Bool -> Parser Group
groupParser gIsInfection = do
    gUnits <- numParser
    void $ string " units each with "
    gHp <- numParser
    void $ string " hit points "
    (gImmunes, gWeaks) <- option ([], []) typeParser
    void $ string "with an attack that does "
    gAtk <- numParser
    gAtkType <- option "" $ try atkTypeParser
    void $ string " damage at initiative "
    gInitiative <- numParser
    return Group
        { units = gUnits
        , hp = gHp
        , immunes = gImmunes
        , weaks = gWeaks
        , atk = gAtk
        , atkType = gAtkType
        , initiative = gInitiative
        , isInfection = gIsInfection
        }

getGroup :: Bool -> IO Group
getGroup isInfection = do
    line <- getLine
    return $ fromRight (error "ERROR") $ parse (groupParser isInfection) "" line

main' = do
    void $ getLine
    immunes <- replicateM 10 (getGroup False)
    void $ getLine
    void $ getLine
    infections <- replicateM 10 (getGroup True)
    groups <- return $ immunes ++ infections
    putStrLn $ show $ solve1 groups
    putStrLn $ show $ solve2 groups
