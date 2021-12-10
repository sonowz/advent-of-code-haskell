module Y2021.Day10 where

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

-- (), [], {}, <>
data BracketType = Round | Square | Curly | Angle deriving (Show, Eq)
data Bracket = Open BracketType | Close BracketType deriving (Show, Eq)
newtype Score = Score Int deriving (Show, Num, Eq, Ord) via Int

type Line = [Bracket]
newtype BracketStack = BracketStack [BracketType] deriving (Semigroup, Monoid) via [BracketType]

------------
-- Part 1 --
------------

solve1 :: [Line] -> Score
solve1 lines = sum $ getLineScore <$> lines

getLineScore :: Line -> Score
getLineScore line = case uncons remainingLine of
    Just (Open  _      , _) -> error "undefined behavior!"
    Just (Close bracket, _) -> getErrorScore bracket
    Nothing                 -> Score 0 -- Line is not corrupted
    where (remainingLine, bracketStack) = untilNothing (uncurry readValidBracket) (line, mempty)

-- Apply 'f' until nothing is returned
untilNothing :: (a -> Maybe a) -> a -> a
untilNothing f x = case f x of
    Just x' -> untilNothing f x'
    Nothing -> x

-- The bracket problem can be solved using 'stack' data structure
-- Stack pushes open brackets, and pops when close brackets
readValidBracket :: Line -> BracketStack -> Maybe (Line, BracketStack)
readValidBracket []       _     = Nothing -- all lines consumed
readValidBracket (b : bs) stack = case b of
    Open  bt -> Just (bs, push stack bt)
    Close bt -> do
        (top, stack') <- pop stack
        -- 'else' case: bracket doesn't match
        if bt == top then Just (bs, stack') else Nothing
  where
    push (BracketStack stack) x = BracketStack (x : stack)
    pop (BracketStack (top : stack')) = Just (top, BracketStack stack')
    pop (BracketStack _             ) = Nothing

getErrorScore :: BracketType -> Score
getErrorScore Round  = 3
getErrorScore Square = 57
getErrorScore Curly  = 1197
getErrorScore Angle  = 25137

------------
-- Part 2 --
------------

solve2 :: [Line] -> Score
solve2 lines = median scores  where
    -- 'mapMaybe' drops corrupted lines
    autocompletions = mapMaybe getAutocompleteBrackets lines :: [[Bracket]]
    scores          = getAutocompleteScore . fmap getBracketType <$> autocompletions :: [Score]
    getBracketType (Open  t) = t
    getBracketType (Close t) = t

median :: Ord a => [a] -> a
median l = (getNth midIndex . sort) l  where
    midIndex = length l `div` 2
    getNth :: Int -> [a] -> a
    getNth n = unsafeHead . drop n
    unsafeHead = fromMaybe (error "No scores!") . viaNonEmpty head

getAutocompleteBrackets :: Line -> Maybe [Bracket]
getAutocompleteBrackets line = do
    let (remainingLine, bracketStack) = untilNothing (uncurry readValidBracket) (line, mempty)
    guard (null remainingLine) -- If remainingLine is not empty, then line is corrupted
    Just . fmap Close . un $ bracketStack

getAutocompleteScore :: [BracketType] -> Score
getAutocompleteScore brackets = go brackets 0  where
    go []            acc = acc
    go (Round  : bs) acc = go bs (5 * acc + 1)
    go (Square : bs) acc = go bs (5 * acc + 2)
    go (Curly  : bs) acc = go bs (5 * acc + 3)
    go (Angle  : bs) acc = go bs (5 * acc + 4)

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    lines <- parseLine <<$>> readFileLines "inputs/Y2021/Day10.txt" :: IO [Line]
    print $ solve1 lines
    print $ solve2 lines

parseLine :: Text -> Line
parseLine = fmap parseBracket . toString  where
    parseBracket '(' = Open Round
    parseBracket '[' = Open Square
    parseBracket '{' = Open Curly
    parseBracket '<' = Open Angle
    parseBracket ')' = Close Round
    parseBracket ']' = Close Square
    parseBracket '}' = Close Curly
    parseBracket '>' = Close Angle
    parseBracket _   = error "Invalid bracket!"
