module Y2019.Day09 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map hiding ((!?))
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Lib.Types
import Lib.IO

-----------------------
-- Type declarations --
-----------------------

type Program = Vector Integer
data ProgramLine
    = P0 Opcode
    | P1 Opcode Param
    | P2 Opcode Param Param
    | P3 Opcode Param Param Param
    deriving (Show)
data Opcode = OpAdd | OpMul | OpHalt | OpInput | OpOutput | OpJumpTrue | OpJumpFalse | OpLT | OpE | OpRel deriving (Show)
data Param = PImm Integer | PAddr Addr | PRel Addr deriving (Show)
newtype Addr = Addr Integer deriving (Show) deriving (Num, NFData) via Integer
type PC = Addr
type RelBase = Addr

toAddr :: RelBase -> Param -> Addr
toAddr _       (PAddr addr) = addr
toAddr relBase (PRel  rel ) = relBase + rel
toAddr _       _            = error "param is not an address type"

parseProgramLine :: Program -> PC -> ProgramLine
parseProgramLine program (Addr pc) = programLine  where
    programLine = case op of
        1  -> P3 OpAdd p1 p2 p3
        2  -> P3 OpMul p1 p2 p3
        3  -> P1 OpInput p1
        4  -> P1 OpOutput p1
        5  -> P2 OpJumpTrue p1 p2
        6  -> P2 OpJumpFalse p1 p2
        7  -> P3 OpLT p1 p2 p3
        8  -> P3 OpE p1 p2 p3
        9  -> P1 OpRel p1
        99 -> P0 OpHalt
        _  -> error $ "program parse error: " <> show op
    i           = fromIntegral pc :: Int
    (modes, op) = parseInstruction $ program `V.unsafeIndex` i
    p1          = modes !! 0 $ program `V.unsafeIndex` (i + 1)
    p2          = modes !! 1 $ program `V.unsafeIndex` (i + 2)
    p3          = modes !! 2 $ program `V.unsafeIndex` (i + 3)
    parseInstruction ins = (modes, op)      where
        op    = ins `mod` 100
        modes = parseMode $ ins `div` 100
    parseMode x
        | x == 0          = repeat (PAddr . Addr)
        | x `mod` 10 == 0 = PAddr . Addr : parseMode (x `div` 10)
        | x `mod` 10 == 1 = PImm : parseMode (x `div` 10)
        | x `mod` 10 == 2 = PRel . Addr : parseMode (x `div` 10)

-- Emulates program I/O
data InOut = InOut Integer [Integer] deriving (Show)

readIn (InOut input _) = input
writeOut (InOut input output) t = InOut input (output <> [t])

------------
-- Part 1 --
------------

solve1 :: Program -> Integer
solve1 program = last output'  where
    (_, InOut _ output) = runProgram initState program (Addr 0) (Addr 0)
    initState           = InOut 1 []
    output'             = fromMaybe (error "must have output") $ nonEmpty output

runProgram :: InOut -> Program -> PC -> RelBase -> (Program, InOut)
runProgram inout !program !i !relBase = program'  where -- Used bang pattern(!) for eager evaluation
    program' = case parseProgramLine program i of
        P3 OpAdd p1 p2 p3   -> runBinaryOp program relBase (+) p1 p2 p3 & continueProgram inout 4
        P3 OpMul p1 p2 p3   -> runBinaryOp program relBase (*) p1 p2 p3 & continueProgram inout 4
        P1 OpInput  p1      -> write p1 (readIn inout) & continueProgram inout 2
        P1 OpOutput p1 -> writeOut inout (value p1) & \inout' -> continueProgram inout' 2 program
        P2 OpJumpTrue p1 p2 -> if value p1 /= 0
            then jumpProgram inout (value p2) program
            else continueProgram inout 3 program
        P2 OpJumpFalse p1 p2 -> if value p1 == 0
            then jumpProgram inout (value p2) program
            else continueProgram inout 3 program
        P3 OpLT p1 p2 p3 ->
            write p3 (if value p1 < value p2 then 1 else 0) & continueProgram inout 4
        P3 OpE p1 p2 p3 ->
            write p3 (if value p1 == value p2 then 1 else 0) & continueProgram inout 4
        P1 OpRel p1 ->
            let relBase' = relBase + Addr (value p1) in runProgram inout program (i + 2) relBase'
        P0 OpHalt -> (program, inout)
    continueProgram io n p = runProgram io p (i + n) relBase
    jumpProgram io n p = runProgram io p (Addr n) relBase
    value = getParamValue program relBase
    write = writeValue program relBase

runBinaryOp
    :: Program -> RelBase -> (Integer -> Integer -> Integer) -> Param -> Param -> Param -> Program
runBinaryOp program relBase f p1 p2 p3 = program'  where
    v1       = getParamValue program relBase p1
    v2       = getParamValue program relBase p2
    v3       = v1 `f` v2
    program' = writeValue program relBase p3 v3

getParamValue :: Program -> RelBase -> Param -> Integer
getParamValue program _       (PImm imm) = imm
getParamValue program relBase param      = value  where
    intAddr = fromIntegral . un @Integer . toAddr relBase $ param :: Int
    value   = fromMaybe (error "program address error") $ program !? intAddr

writeValue :: Program -> RelBase -> Param -> Integer -> Program
writeValue program relBase param x = program'  where
    intAddr  = fromIntegral . un @Integer . toAddr relBase $ param :: Int
    program' = V.update program $ one (intAddr, x)

------------
-- Part 2 --
------------

solve2 :: Program -> Integer
solve2 program = last output'  where
    (_, InOut _ output) = runProgram initState program (Addr 0) (Addr 0)
    initState           = InOut 2 []
    output'             = fromMaybe (error "must have output") $ nonEmpty output

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    program <- parseProgram <$> readFile "inputs/Y2019/Day09.txt" :: IO Program
    print $ solve1 program
    print $ solve2 program

parseProgram :: String -> Program
parseProgram line = V.fromList $ map readInteger textList <> replicate 1000 0
    where textList = words . toText $ map (\c -> if c == ',' then ' ' else c) line
