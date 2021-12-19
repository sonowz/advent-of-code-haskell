module Y2021.Day18 where

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
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Text
import qualified Text.Show as S

-----------------------
-- Type declarations --
-----------------------

data SnailNumber = Num Int | Pair SnailNumber SnailNumber deriving (Eq)

instance S.Show SnailNumber where
    show (Num x       ) = show x
    show (Pair sn1 sn2) = show ([sn1, sn2] :: [SnailNumber])

-- Partial 'Num' typeclass definition
instance Num SnailNumber where
    (+)         = addSnailNumber
    (*)         = error "Undefined operation!"
    abs         = id
    signum      = error "Undefined operation!"
    fromInteger = error "Undefined operation!"
    (-)         = error "Undefined operation!"

------------
-- Part 1 --
------------

solve1 :: NonEmpty SnailNumber -> Int
solve1 = getMagnitude . foldl1' (+)

addSnailNumber :: SnailNumber -> SnailNumber -> SnailNumber
addSnailNumber sn1 sn2 = reduce (Pair sn1 sn2)

reduce :: SnailNumber -> SnailNumber
reduce sn = maybe sn reduce (tryExplode sn <|> trySplit sn)

newtype Depth = Depth Int deriving (Num, Eq, Ord) via Int

-- If try succeeds, return 'Just'
-- if fails, return 'Nothing'
tryExplode :: SnailNumber -> Maybe SnailNumber
tryExplode sn = fst <$> go (Depth 0) sn  where
    -- '(Int, Int)' indicates a number to be added respectively (leftmost, rightmost)
    go :: Depth -> SnailNumber -> Maybe (SnailNumber, (Int, Int))
    -- Base cases
    go depth (Num _)                = Nothing
    go depth (Pair (Num left) (Num right)) | depth >= 4 = Just (Num 0, (left, right))
    -- Branch case
    go depth (Pair nested1 nested2) = (whenExploded1 <$> nested1') <|> (whenExploded2 <$> nested2')      where

        nested1' = go (depth + 1) nested1
        nested2' = go (depth + 1) nested2

        -- Add 'right' to rightmost number, and carry 'left' up
        whenExploded1 :: (SnailNumber, (Int, Int)) -> (SnailNumber, (Int, Int))
        whenExploded1 (new, (left, right)) = (Pair new (addLeft right nested2), (left, 0))
        -- Add 'left' to leftmost number, and carry 'right' up
        whenExploded2 :: (SnailNumber, (Int, Int)) -> (SnailNumber, (Int, Int))
        whenExploded2 (new, (left, right)) = (Pair (addRight left nested1) new, (0, right))

        addLeft :: Int -> SnailNumber -> SnailNumber
        addLeft 0 sn                = sn
        addLeft n (Num x          ) = Num (x + n)
        addLeft n (Pair left right) = Pair (addLeft n left) right
        addRight :: Int -> SnailNumber -> SnailNumber
        addRight 0 sn                = sn
        addRight n (Num x          ) = Num (x + n)
        addRight n (Pair left right) = Pair left (addRight n right)

-- If try succeeds, return 'Just'
-- if fails, return 'Nothing'
trySplit :: SnailNumber -> Maybe SnailNumber
-- Base cases
trySplit (Num x)
    | x < 10    = Nothing
    | otherwise = let (q, r) = x `divMod` 2 in Just $ Pair (Num q) (Num $ q + r)
-- Branch case
trySplit (Pair nested1 nested2) = nested1' <|> nested2'  where
    nested1' = flip Pair nested2 <$> trySplit nested1
    nested2' = Pair nested1 <$> trySplit nested2

getMagnitude :: SnailNumber -> Int
getMagnitude (Num x       ) = x
getMagnitude (Pair sn1 sn2) = 3 * getMagnitude sn1 + 2 * getMagnitude sn2

------------
-- Part 2 --
------------

solve2 :: [SnailNumber] -> Int
solve2 sns = maximum pairSums  where
    pairSums = [ getMagnitude (sn1 + sn2) | sn1 <- sns, sn2 <- sns, sn1 /= sn2 ] :: [Int]
    maximum :: Ord a => [a] -> a
    maximum = Unsafe.fromJust . viaNonEmpty maximum1

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    snailNumbers <-
        parseSnailNumber <<$>> readFileLines "inputs/Y2021/Day18.txt" :: IO [SnailNumber]
    let snailNumbers' = Unsafe.fromJust $ nonEmpty snailNumbers
    print $ solve1 snailNumbers'
    print $ solve2 snailNumbers

parseSnailNumber :: Text -> SnailNumber
parseSnailNumber text = fromRight (error "parse error") $ parse parserSnailNumber "" text

number :: Parser Int
number = readInt . toText <$> many1 digit

parserSnailNumber :: Parser SnailNumber
parserSnailNumber = try parserPair <|> try parserNum  where
    parserPair = do
        char '['
        sn1 <- parserSnailNumber
        char ','
        sn2 <- parserSnailNumber
        char ']'
        return $ Pair sn1 sn2
    parserNum = Num <$> number
