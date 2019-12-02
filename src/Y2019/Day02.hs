module Y2019.Day02 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map hiding ((!?))
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Control.Monad.ST
import Lib.Types
import Lib.IO

-----------------------
-- Type declarations --
-----------------------

type Program = Vector Int
data ProgramLine = P1 Opcode | P4 Opcode Addr Addr Addr deriving (Show)
data Opcode = OpAdd | OpMul | OpHalt deriving (Show)
newtype Addr = Addr Int deriving (Show) deriving (Num) via Int
type PC = Addr

parseProgramLine :: Program -> PC -> ProgramLine
parseProgramLine program (un -> i :: Int) = programLine  where
    programLine = case op of
        1  -> P4 OpAdd (Addr p1) (Addr p2) (Addr p3)
        2  -> P4 OpMul (Addr p1) (Addr p2) (Addr p3)
        99 -> P1 OpHalt
        _  -> error $ "program parse error: " <> show op
    op = program `V.unsafeIndex` i
    p1 = program `V.unsafeIndex` (i + 1)
    p2 = program `V.unsafeIndex` (i + 2)
    p3 = program `V.unsafeIndex` (i + 3)

------------
-- Part 1 --
------------

solve1 :: Program -> Int
solve1 program = halted `V.unsafeIndex` 0  where
    halted      = runProgram program1202 (Addr 0)
    program1202 = V.update program $ fromList [(1, 12), (2, 02)]

runProgram :: Program -> PC -> Program
runProgram program i = program'  where
    program' = case parseProgramLine program i of
        P4 OpAdd r1 r2 r3 -> runBinaryOp program (+) r1 r2 r3 & continueProgram
        P4 OpMul r1 r2 r3 -> runBinaryOp program (*) r1 r2 r3 & continueProgram
        P1 OpHalt         -> program
    continueProgram p = runProgram p (i + 4)

runBinaryOp :: Program -> (Int -> Int -> Int) -> Addr -> Addr -> Addr -> Program
runBinaryOp program f r1 r2 r3 = program'  where
    computedValue :: Maybe Int
    computedValue = do
        v1 <- program !? un r1
        v2 <- program !? un r2
        return (v1 `f` v2)
    program' = case computedValue of
        Just v3 -> V.update program $ one (un r3, v3)
        Nothing -> error "program address error"

------------
-- Part 2 --
------------

solve2 :: Program -> Int
solve2 program = 100 * noun + verb  where
    tryNounVerb (n, v) = answerCheck haltedProgram      where
        tryingProgram = V.update program $ fromList [(1, n), (2, v)]
        haltedProgram = runProgram tryingProgram (Addr 0)
        answerCheck p = p `V.unsafeIndex` 0 == 19690720
    (noun, verb) = find tryNounVerb cases ?: (-1, -1)
    cases        = [ (n, v) | n <- [0 .. 99], v <- [0 .. 99] ] :: [(Int, Int)]

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    program <- parseProgram <$> readFile "inputs/Y2019/Day02.txt" :: IO Program
    print $ solve1 program
    print $ solve2 program

parseProgram :: String -> Program
parseProgram line = V.fromList $ map readInt textList <> [0, 0, 0]
    where textList = words . toText $ map (\c -> if c == ',' then ' ' else c) line
