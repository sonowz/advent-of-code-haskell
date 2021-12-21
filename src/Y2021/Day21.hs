module Y2021.Day21 where

import qualified Data.Set as S
import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import qualified Relude.Unsafe as Unsafe

-----------------------
-- Type declarations --
-----------------------

data Player = Player1 | Player2 deriving (Show, Eq, Ord)
data GameBoard = GameBoard Pos Pos Score Score
    deriving (Show, Eq)
newtype Pos = Pos Int deriving (Show, Eq, Ord) via Int
newtype Score = Score Int deriving (Show, Num, Eq, Ord) via Int

-- Deterministic dice
newtype DetDice = DetDice Int

instance Num Pos where
    (Pos a) + (Pos b) = Pos ((a + b - 1) `mod` 10 + 1)
    (Pos a) * (Pos b) = Pos ((a * b - 1) `mod` 10 + 1)
    (Pos a) - (Pos b) = Pos ((a - b - 1) `mod` 10 + 1)
    abs (Pos a) = Pos (abs a)
    signum (Pos a) = Pos (signum a)
    fromInteger n = Pos (fromInteger n)

getPos :: Player -> GameBoard -> Pos
getPos Player1 (GameBoard p1 _  _ _) = p1
getPos Player2 (GameBoard _  p2 _ _) = p2

setPos :: Player -> Pos -> GameBoard -> GameBoard
setPos Player1 p1 (GameBoard _  p2 s1 s2) = GameBoard p1 p2 s1 s2
setPos Player2 p2 (GameBoard p1 _  s1 s2) = GameBoard p1 p2 s1 s2

getScore :: Player -> GameBoard -> Score
getScore Player1 (GameBoard _ _ s1 _ ) = s1
getScore Player2 (GameBoard _ _ _  s2) = s2

addScore :: Player -> Score -> GameBoard -> GameBoard
addScore Player1 s (GameBoard p1 p2 s1 s2) = GameBoard p1 p2 (s1 + s) s2
addScore Player2 s (GameBoard p1 p2 s1 s2) = GameBoard p1 p2 s1 (s2 + s)


------------
-- Part 1 --
------------

solve1 :: GameBoard -> Int
solve1 board = un (getScore lostPlayer endBoard) * rollCount  where
    gameStates = iterate play (board, Player1, initDetDice)
    (endBoard, lostPlayer, endDice) =
        Unsafe.head $ dropWhile (not . gameEnded1) gameStates :: GameState
    DetDice rollCount = endDice

gameEnded1 :: GameState -> Bool
gameEnded1 (board, _, _) = getScore Player1 board >= 1000 || getScore Player2 board >= 1000

initDetDice :: DetDice
initDetDice = DetDice 0

rollDetDice :: DetDice -> (Int, DetDice)
rollDetDice (DetDice x) = (x `mod` 100 + 1, DetDice (x + 1))

type GameState = (GameBoard, Player, DetDice)

play :: GameState -> GameState
play (board, player, detDice) = (board', nextPlayer player, detDice')  where
    diceRolls = iterate
        (\(rollSum, dice) -> let (roll, dice') = rollDetDice dice in (rollSum + roll, dice'))
        (0, detDice)
    (totalRoll, detDice') = diceRolls Unsafe.!! 3
    board'                = updateBoard totalRoll player board

updateBoard :: Int -> Player -> GameBoard -> GameBoard
updateBoard roll player board = board'  where
    nextPos   = getPos player board + Pos roll
    turnScore = Score $ un nextPos
    board'    = addScore player turnScore . setPos player nextPos $ board

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1


------------
-- Part 2 --
------------

-- Note that this solution is over-optimized:
--   using "dynamic programming" with state transition calculated only once,
--   the program works even when the game finishes at score 100
-- For more simple solution, one can implement just recursive calls to count universes
--   which will take less than 7^7 = 823543 recursive calls

solve2 :: GameBoard -> Integer
solve2 board = max player1WinCount player2WinCount  where
    initState    = State board Player1
    initStatePQ  = one initState :: StatePQ
    initUniverse = one (initState, 1) :: UniverseCount
    universes    = countUniverses initStatePQ initUniverse

    extractUniverseCount :: (GameState', Integer) -> Maybe (Player, Integer)
    extractUniverseCount (State board player, count) =
        if gameEnded2 board then Just (nextPlayer player, count) else Nothing
    endedUniverses  = catMaybes $ extractUniverseCount <$> toPairs universes :: [(Player, Integer)]
    player1WinCount = sum . fmap snd . filter ((==) Player1 . fst) $ endedUniverses :: Integer
    player2WinCount = sum . fmap snd . filter ((==) Player2 . fst) $ endedUniverses :: Integer

gameEnded2 :: GameBoard -> Bool
gameEnded2 board = getScore Player1 board >= 21 || getScore Player2 board >= 21

data GameState' = State GameBoard Player
    deriving Eq

-- This ordering ensures that transition of 'GameState' is always in increasing order
instance Ord GameState' where
    compare (State (GameBoard p11 p21 s11 s21) pl1) (State (GameBoard p12 p22 s12 s22) pl2) =
        compare (s11, s21, p11, p21, pl1) (s12, s22, p12, p22, pl2)

-- Priority queue of states
type StatePQ = Set GameState'

-- Stores number of universes in that 'Gamestate'
type UniverseCount = Map GameState' Integer

-- Use dynamic programming with recurrence relation
countUniverses :: StatePQ -> UniverseCount -> UniverseCount
countUniverses priorityQueue universes
    | S.null priorityQueue = universes
    | gameEnded2 board     = countUniverses priorityQueue' universes
    | otherwise            = countUniverses priorityQueue'' universes'
  where
    (state, priorityQueue') = S.deleteFindMin priorityQueue
    State board player      = state
    accCount                = universes !? state ?: error "No state in universes!"
    updateCount :: (Int, Integer) -> (StatePQ, UniverseCount) -> (StatePQ, UniverseCount)
    updateCount (roll, count) (pq, univ) = (pq', univ')      where
        board' = updateBoard roll player board
        count' = accCount * count
        state' = State board' (nextPlayer player)
        !pq'   = S.insert state' pq
        !univ' = updateOrInsert (+ count') state' univ
    (priorityQueue'', universes') =
        flipfoldl' updateCount (priorityQueue', universes) diracDiceDist

-- Distribution of 3 dirac dice rolls
-- [(sum of 3 rolls, roll count among 27 cases)]
diracDiceDist :: [(Int, Integer)]
diracDiceDist = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

updateOrInsert :: Ord a => (Integer -> Integer) -> a -> Map a Integer -> Map a Integer
updateOrInsert f key map = let value = map !? key ?: 0 in insert key (f value) map


--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    gameBoard <- parseGameBoard <$> readFileLines "inputs/Y2021/Day21.txt" :: IO GameBoard
    print $ solve1 gameBoard
    print $ solve2 gameBoard

parseGameBoard :: [Text] -> GameBoard
parseGameBoard lines = GameBoard (Pos p1Start) (Pos p2Start) (Score 0) (Score 0)  where
    p1Start = readInt $ words (lines Unsafe.!! 0) Unsafe.!! 4
    p2Start = readInt $ words (lines Unsafe.!! 1) Unsafe.!! 4
