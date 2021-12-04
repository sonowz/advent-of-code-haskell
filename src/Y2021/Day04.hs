module Y2021.Day04 where

import Data.Text (split)
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Lib.IO
import Lib.Types
import Lib.Vector2D
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple

-----------------------
-- Type declarations --
-----------------------

type BingoBoard = Vector (Vector Int)
type DrawSequence = Vector Int

------------
-- Part 1 --
------------

solve1 :: DrawSequence -> [BingoBoard] -> Int
solve1 drawSeq boards = calcScore winDrawSeq winBoard  where
    winIndex = parametricSearchIndex searchFunc 0 (length drawSeq)
    searchFunc i = any (checkBingo (V.take i drawSeq)) boards
    winDrawSeq = V.take winIndex drawSeq
    winBoard   = find (checkBingo winDrawSeq) boards ?: error "Should have win board!"

checkBingo :: DrawSequence -> BingoBoard -> Bool
checkBingo drawSeq board = horizontalBingo || verticalBingo  where
    drawSet         = fromList $ toList drawSeq :: Set Int
    horizontalBingo = V.any (V.all (\x -> member x drawSet)) board
    verticalBingo   = any (\i -> V.all (\row -> member (row ! i) drawSet) board) rowIndices
    rowIndices      = [0 .. snd (size2D board) - 1] :: [Int]

calcScore :: DrawSequence -> BingoBoard -> Int
calcScore drawSeq board = sumUnmarked * calledNumber  where
    drawSet      = fromList $ toList drawSeq :: Set Int
    filtered     = fmap (V.filter (\x -> not $ member x drawSet)) board
    sumUnmarked  = sum (sum <$> filtered)
    calledNumber = V.last drawSeq

-- Search by index
-- Find least number which satisfies the predicate
parametricSearchIndex :: (Show a, Integral a, Eq a) => (a -> Bool) -> a -> a -> a
parametricSearchIndex f start end
    | start == end = start
    | f mid        = parametricSearchIndex f start mid
    | otherwise    = parametricSearchIndex f (mid + 1) end -- f mid == False
    where mid = (start + end) `div` 2


------------
-- Part 2 --
------------

solve2 :: DrawSequence -> [BingoBoard] -> Int
solve2 drawSeq boards = calcScore winDrawSeq lastWinBoard  where
    winIndex = parametricSearchIndex searchFunc 0 (length drawSeq)
    searchFunc i = all (checkBingo (V.take i drawSeq)) boards -- Part 2: 'any' => 'all'
    winDrawSeq     = V.take winIndex drawSeq
    prevWinDrawSeq = V.take (winIndex - 1) drawSeq
    lastWinBoard =
        find (not . checkBingo prevWinDrawSeq) boards ?: error "Should have non-win board!"

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    (drawSeq, boards) <-
        parseBingoSubsystem <$> readFileLines "inputs/Y2021/Day04.txt" :: IO
            (DrawSequence, [BingoBoard])
    print $ solve1 drawSeq boards
    print $ solve2 drawSeq boards

boardSize = 5 :: Int

parseBingoSubsystem :: [Text] -> (DrawSequence, [BingoBoard])
parseBingoSubsystem lines = (drawSeq, boards)  where
    textDrawSeq = (split (== ',') <$> lines !!? 0) ?: error "Parse error!"
    drawSeq     = V.fromList $ readInt <$> textDrawSeq
    textBoards  = splitBoard $ drop 2 lines
    splitBoard l
        | length l > boardSize = let (b, bs) = splitAt boardSize l in b : splitBoard (drop 1 bs)
        | otherwise            = [l]
    boards = parseBoard <$> textBoards
    parseBoard :: [Text] -> BingoBoard
    parseBoard l = V.fromList $ V.fromList . toList . readInts <$> l

