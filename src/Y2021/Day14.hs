module Y2021.Day14 where

import Data.Map.Strict (update)
import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype ()
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))
import qualified Relude.Unsafe as Unsafe
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text

-----------------------
-- Type declarations --
-----------------------

newtype Element = Element Char deriving (Show, Eq, Ord)
data ElementPair = ElementPair Char Char
    deriving (Show, Eq, Ord)

-- Example) [CH -> B] is stored as : CH->(CB, BH)
type InsertionRules = Map ElementPair (ElementPair, ElementPair)

-- Order of 'ElementPair' is not important in this problem
data Polymer = Polymer
    -- Store counts of 'ElementPair's
    -- Example) AABABA is stored as : AA->1, AB->2, BA->2
    { getPairs :: Map ElementPair Integer
    -- 'getStart' and 'getEnd' is needed to count elements of pairs
    -- since elements at other than start and end of polymers
    -- appears twice in 'getPairs'.
    , getStart :: Element
    , getEnd   :: Element
    }

------------
-- Part 1 --
------------

solve1 :: Polymer -> InsertionRules -> Integer
solve1 template rules = maxCount - minCount  where
    polymerized10 = iterate (polymerize rules) template !! 10 :: Polymer
    elemCounts    = polymerToElemCount polymerized10
    minCount      = minimum (elems elemCounts) :: Integer
    maxCount      = maximum (elems elemCounts) :: Integer

    minimum       = fromJust . viaNonEmpty minimum1
    maximum       = fromJust . viaNonEmpty maximum1
    fromJust      = fromMaybe (error "Polymer is empty!")

-- By polymerizing, each 'ElementPair' becomes 2 'ElementPair's
polymerize :: InsertionRules -> Polymer -> Polymer
polymerize rules polymer = newPolymer'  where
    newPolymer  = polymer { getPairs = mempty } -- Initialize new polymer pairs
    newPolymer' = foldl' foldFn newPolymer (toPairs $ getPairs polymer)
    foldFn polymer (pair, count) = polymerizePair pair count polymer

    -- Insert 2 new 'ElementPair's into 'polymer'
    polymerizePair :: ElementPair -> Integer -> Polymer -> Polymer
    polymerizePair pair n polymer = case lookup pair rules of
        Just (newPair1, newPair2) -> updatePair
            $ (updateOrInsert (+ n) newPair1 . updateOrInsert (+ n) newPair2) (getPairs polymer)
        Nothing -> polymer
        where updatePair p = polymer { getPairs = p }

polymerToElemCount :: Polymer -> Map Element Integer
polymerToElemCount polymer = (`div` 2) <$> startEndAdded  where
    countDoubled = foldl' foldFn mempty (toPairs $ getPairs polymer)
    foldFn elemCounts (ElementPair e1 e2, count) =
        updateOrInsert (+ count) (Element e1) . updateOrInsert (+ count) (Element e2) $ elemCounts
    startEndAdded = updateOrInsert (+ 1) start . updateOrInsert (+ 1) end $ countDoubled
    (start, end)  = (getStart polymer, getEnd polymer)

updateOrInsert :: Ord a => (Integer -> Integer) -> a -> Map a Integer -> Map a Integer
updateOrInsert f key map = insert key (f value) map where value = fromMaybe 0 (lookup key map)

------------
-- Part 2 --
------------

solve2 :: Polymer -> InsertionRules -> Integer
solve2 template rules = maxCount - minCount  where
    polymerized40 = iterate (polymerize rules) template !! 40 :: Polymer
    elemCounts    = polymerToElemCount polymerized40
    minCount      = minimum (elems elemCounts) :: Integer
    maxCount      = maximum (elems elemCounts) :: Integer

    minimum       = fromJust . viaNonEmpty minimum1
    maximum       = fromJust . viaNonEmpty maximum1
    fromJust      = fromMaybe (error "Polymer is empty!")

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    (template, rules) <-
        parseManual <$> readFileLines "inputs/Y2021/Day14.txt" :: IO (Polymer, InsertionRules)
    print $ solve1 template rules
    print $ solve2 template rules

parseManual :: [Text] -> (Polymer, InsertionRules)
parseManual text = fromRight (error "parse error") $ parse parserManual "" (unlines text)

parserManual :: Parser (Polymer, InsertionRules)
parserManual = do
    template <- parserPolymer
    void newline
    void newline
    rules <- parserRule `sepEndBy1` newline
    return (template, fromList rules)

parserPolymer :: Parser Polymer
parserPolymer = do
    chars <- many1 upper
    let pairs = fromList . aggregate . listToPairs $ chars
        start = Element (Unsafe.head chars)
        end   = Element (Unsafe.last chars)
    return $ Polymer pairs start end
  where
    listToPairs :: [Char] -> [ElementPair]
    listToPairs (c1 : c2 : cs) = ElementPair c1 c2 : listToPairs (c2 : cs)
    listToPairs _              = []
    aggregate :: [ElementPair] -> [(ElementPair, Integer)]
    aggregate pairs = map (\g -> (Unsafe.head g, fromIntegral $ length g)) grouped
        where grouped = group $ sort pairs

parserRule :: Parser (ElementPair, (ElementPair, ElementPair))
parserRule = do
    first  <- upper
    second <- upper
    string " -> "
    result <- upper
    return (ElementPair first second, (ElementPair first result, ElementPair result second))
