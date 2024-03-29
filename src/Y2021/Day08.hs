module Y2021.Day08 where

import qualified Data.Set as S
import Lib.IO
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

data Seg7 = A|B|C|D|E|F|G deriving (Eq, Ord, Enum, Show)
type Digit = Set Seg7

data Entry = Entry
    { getUniqueSignals :: [Digit]
    , getFourSignals   :: [Digit]
    }


-- Mapping from correct -> wrong
-- Use phantom type to hold key elements
-- TODO: more generous coding and typecheck, using type constraint
newtype SegMapping (segs :: [Seg7]) = SegMapping (Map Seg7 Seg7)
type AllSeg7 = '[A , B , C , D , E , F , G]

newSegMapping :: SegMapping '[]
newSegMapping = SegMapping mempty

-- WARNING: segs' must be explicitly typed
insertSegMapping :: Seg7 -> Seg7 -> SegMapping segs -> SegMapping segs'
insertSegMapping k v (SegMapping m) = SegMapping $ insert k v m

-- WARNING: segs'' must be explicitly typed
unionSegMapping :: SegMapping segs -> SegMapping segs' -> SegMapping segs''
unionSegMapping (SegMapping m1) (SegMapping m2) = SegMapping (m1 <> m2)

getSegMapping :: Seg7 -> SegMapping segs -> Seg7
getSegMapping k (SegMapping m) = fromMaybe (error "No mapping!") (lookup k m)

inverseMapping :: SegMapping AllSeg7 -> SegMapping AllSeg7
inverseMapping (SegMapping m) = (SegMapping . fromList . fmap swap . toPairs) m


-- Use 'MonadFail' class in 'findX' functions
-- This enables non-exhaustive pattern matching & prevents runtime errors
type Failable = Either String
-- instance MonadFail (Either String)

-- Using custom operators with Sets for readability
infixl 6 .- -- Same precedence as (+), (-)
(.-) :: Ord a => Set a -> Set a -> Set a
(.-) = S.difference

infixl 7 ∩ -- Same precedence as (*), (/)
(∩) :: Ord a => Set a -> Set a -> Set a
(∩) = S.intersection

------------
-- Part 1 --
------------

solve1 :: [Entry] -> Int
solve1 entries = sum $ countTrivialSignals . getFourSignals <$> entries

countTrivialSignals :: [Digit] -> Int
countTrivialSignals = count trivialLength  where
    trivialLength = flip member ([2, 3, 4, 7] :: Set Int) . length
    count f = length . filter f

------------
-- Part 2 --
------------

solve2 :: [Entry] -> Int
solve2 = sum . fmap solveEntry

solveEntry :: Entry -> Int
solveEntry entry = intListToInt outputDigits  where
    segMapping   = findSegMapping (getUniqueSignals entry)
    outputDigits = decodeDigit segMapping <$> getFourSignals entry

intListToInt :: [Int] -> Int
intListToInt l = go l 0  where
    go []       acc = 0
    go [x     ] acc = acc + x
    go (x : xs) acc = go xs (10 * (acc + x))

nonEmptyFailable :: [a] -> Failable (NonEmpty a)
nonEmptyFailable = maybeToRight "Empty list!" . nonEmpty


unsafeFind :: (Digit -> Bool) -> [Digit] -> Digit
unsafeFind f l = fromMaybe (error "No find result!") $ find f l
digit1 :: [Digit] -> Digit
digit1 = unsafeFind ((== 2) . length)
digit4 :: [Digit] -> Digit
digit4 = unsafeFind ((== 4) . length)
digit7 :: [Digit] -> Digit
digit7 = unsafeFind ((== 3) . length)
digit235 :: [Digit] -> [Digit]
digit235 = filter ((== 5) . length)
digit069 :: [Digit] -> [Digit]
digit069 = filter ((== 6) . length)

-- 'a' is found using "1" and "7"
findA :: [Digit] -> Failable (SegMapping '[A])
findA s = do
    let [a] = digit7 s .- digit1 s
    return $ insertSegMapping A a newSegMapping

-- 'd' is found using "235" and "4"
findD :: [Digit] -> Failable (SegMapping '[D])
findD s = do
    adg <- foldl1' (∩) <$> nonEmptyFailable (digit235 s)
    let [d] = adg ∩ digit4 s
    return $ insertSegMapping D d newSegMapping

-- 'b' is found using "1", "4", and 'd'
findB :: [Digit] -> SegMapping '[D] -> Failable (SegMapping '[B , D])
findB s m = do
    let d   = getSegMapping D m
        bd  = digit4 s .- digit1 s
        [b] = bd .- one d
    return $ insertSegMapping B b m

-- 'c' and 'f' is found using "1" and "069"
findCF :: [Digit] -> Failable (SegMapping '[C , F])
findCF s = do
    abfg <- foldl1' (∩) <$> nonEmptyFailable (digit069 s)
    let cf  = digit1 s
        [f] = abfg ∩ cf
        [c] = cf .- one f
    return $ insertSegMapping F f . insertSegMapping C c $ newSegMapping

-- 'g' is found using "235", 'a', and 'd'
findG :: [Digit] -> SegMapping '[A , D] -> Failable (SegMapping '[A , D , G])
findG s m = do
    adg <- foldl1' (∩) <$> nonEmptyFailable (digit235 s)
    let a   = getSegMapping A m
        d   = getSegMapping D m

        [g] = adg .- fromList [a, d]
    return $ insertSegMapping G g m

-- 'e' is found by pigeonhole principle
findE :: SegMapping '[A , B , C , D , F , G] -> Failable (SegMapping AllSeg7)
findE m = do
    let allSegs   = fromList [A .. G] :: Set Seg7
        otherSegs = fromList $ map (`getSegMapping` m) [A, B, C, D, F, G] :: Set Seg7
        [e]       = allSegs .- otherSegs
    return $ insertSegMapping E e m

findSegMapping :: [Digit] -> SegMapping AllSeg7
findSegMapping digits = either (error . toText) id $ do
    a   <- findA digits
    d   <- findD digits
    bd  <- findB digits d
    cf  <- findCF digits
    adg <- findG digits (unionSegMapping a d)
    let abdg   = unionSegMapping adg bd :: SegMapping '[A , B , D , G]
        abcdfg = unionSegMapping abdg cf
    findE abcdfg

decodeDigit :: SegMapping AllSeg7 -> Digit -> Int
decodeDigit mapping digit = readDigit mappedDigit  where
    mappedDigit     = fromList $ (`getSegMapping` inversedMapping) <$> toList digit :: Digit

    -- Mapping from wrong -> correct
    inversedMapping = inverseMapping mapping

    -- Read 7 segment as int, as described in description
    readDigit :: Digit -> Int
    readDigit d
        | d == fromList [A, B, C, E, F, G]    = 0
        | d == fromList [C, F]                = 1
        | d == fromList [A, C, D, E, G]       = 2
        | d == fromList [A, C, D, F, G]       = 3
        | d == fromList [B, C, D, F]          = 4
        | d == fromList [A, B, D, F, G]       = 5
        | d == fromList [A, B, D, E, F, G]    = 6
        | d == fromList [A, C, F]             = 7
        | d == fromList [A, B, C, D, E, F, G] = 8
        | d == fromList [A, B, C, D, F, G]    = 9
        | otherwise                           = error "Invalid digit!"

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    entries <- parseEntry <<$>> readFileLines "inputs/Y2021/Day08.txt" :: IO [Entry]
    print $ solve1 entries
    print $ solve2 entries

parseEntry :: Text -> Entry
parseEntry line = Entry uniqueSignals fourSignals  where
    (uniqueSignalsText, fourSignalsText) = splitAt 10 (words line)
    uniqueSignals                        = parseDigit <$> uniqueSignalsText
    fourSignals                          = parseDigit <$> unsafeTail fourSignalsText
    unsafeTail                           = fromMaybe (error "Invalid input!") . viaNonEmpty tail


parseDigit :: Text -> Digit
parseDigit = fromList . fmap parseSeg7 . toString  where
    parseSeg7 'a' = A
    parseSeg7 'b' = B
    parseSeg7 'c' = C
    parseSeg7 'd' = D
    parseSeg7 'e' = E
    parseSeg7 'f' = F
    parseSeg7 'g' = G
    parseSeg7 _   = error "Invalid signal!"
