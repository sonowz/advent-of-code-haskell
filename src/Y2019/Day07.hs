module Y2019.Day07 where

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
import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad
import Lib.Types
import Lib.IO
import Lib.NonEmpty

-----------------------
-- Type declarations --
-----------------------

type Program = Vector Int
data ProgramLine
    = P0 Opcode
    | P1 Opcode Param
    | P2 Opcode Param Param
    | P3 Opcode Param Param Param
    deriving (Show)
data Opcode = OpAdd | OpMul | OpHalt | OpInput | OpOutput | OpJumpTrue | OpJumpFalse | OpLT | OpE deriving (Show)
data Param = PImm Int | PAddr Addr deriving (Show)
newtype Addr = Addr Int deriving (Show) deriving (Num) via Int
type PC = Addr

parseProgramLine :: Program -> PC -> ProgramLine
parseProgramLine program (un -> i :: Int) = programLine  where
    programLine = case op of
        1  -> P3 OpAdd p1 p2 p3
        2  -> P3 OpMul p1 p2 p3
        3  -> P1 OpInput p1
        4  -> P1 OpOutput p1
        5  -> P2 OpJumpTrue p1 p2
        6  -> P2 OpJumpFalse p1 p2
        7  -> P3 OpLT p1 p2 p3
        8  -> P3 OpE p1 p2 p3
        99 -> P0 OpHalt
        _  -> error $ "program parse error: " <> show op
    (modes, op) = parseInstruction $ program `V.unsafeIndex` i
    p1          = modes !! 0 $ program `V.unsafeIndex` (i + 1)
    p2          = modes !! 1 $ program `V.unsafeIndex` (i + 2)
    p3          = modes !! 2 $ program `V.unsafeIndex` (i + 3)
    parseInstruction ins = (modes, op)      where
        op    = ins `mod` 100
        modes = parseMode $ ins `div` 100
    parseMode x
        | x == 0         = repeat (PAddr . Addr)
        | x `mod` 2 == 1 = PImm : parseMode (x `div` 10)
        | x `mod` 2 == 0 = PAddr . Addr : parseMode (x `div` 10)

-- Emulates program I/O
data StdEnv = StdEnv IChan IChan
type IChan = TChan Int

readIn (StdEnv input output) = readTChan input
writeOut (StdEnv input output) = writeTChan output

------------
-- Part 1 --
------------

solve1 :: Program -> IO Int
solve1 program = maximum1 <$> outputs  where
    ne            = fromJust . nonEmpty
    (<<>>)        = liftM2 (<>)
    phaseSettings = ne $ ne <$> permutations [0 .. 4] :: NonEmpty (NonEmpty Int)
    makeChannels phases =
        (initIChan `mapM` phases) <<>> (one <$> newTChanIO) :: IO (NonEmpty IChan)
    outputs = mapM (makeChannels >=> runAmplifiers program) phaseSettings :: IO (NonEmpty Int)

-- amplifier[i] takes ichan[i] as stdin, and ichan[i+1] as stdout
runAmplifiers :: Program -> NonEmpty IChan -> IO Int
runAmplifiers program ichans = runAmplifiers'  where
    runAmplifiers' = do
        amplifiers <- forkAmplifier (toList ichans)
        atomically $ writeTChan (head ichans) 0 -- input
        waitAll amplifiers
        atomically $ readTChan (last ichans) -- thruster output
    waitAll []     = pass
    waitAll asyncs = waitAny asyncs >>= \(x, _) -> waitAll (filter (/= x) asyncs)
    forkAmplifier :: [IChan] -> IO [Async Program]
    forkAmplifier (chan1 : chan2 : chans) = do
        let stdenv = StdEnv chan1 chan2
        newAmp    <- async $ runReaderT (runProgram program (Addr 0)) stdenv
        otherAmps <- forkAmplifier (chan2 : chans)
        return $ newAmp : otherAmps
    forkAmplifier _ = return []

initIChan :: Int -> IO IChan
initIChan phaseSetting = atomically $ do
    chan <- newTChan
    writeTChan chan phaseSetting
    return chan

runProgram :: Program -> PC -> ReaderT StdEnv IO Program
runProgram program i = ask >>= program'  where
    program' inout = case parseProgramLine program i of
        P3 OpAdd p1 p2 p3        -> runBinaryOp program (+) p1 p2 p3 & continueProgram 4
        P3 OpMul p1 p2 p3        -> runBinaryOp program (*) p1 p2 p3 & continueProgram 4
        P1 OpInput  (PAddr addr) -> atomically (readIn inout) >>= continueProgram 2 . write addr
        P1 OpOutput p1 -> atomically (writeOut inout $ value p1) >> continueProgram 2 program
        P2 OpJumpTrue p1 p2 ->
            if value p1 /= 0 then jumpProgram (value p2) program else continueProgram 3 program
        P2 OpJumpFalse p1 p2 ->
            if value p1 == 0 then jumpProgram (value p2) program else continueProgram 3 program
        P3 OpLT p1 p2 (PAddr a3) ->
            write a3 (if value p1 < value p2 then 1 else 0) & continueProgram 4
        P3 OpE p1 p2 (PAddr a3) ->
            write a3 (if value p1 == value p2 then 1 else 0) & continueProgram 4
        P0 OpHalt -> return program
    continueProgram n p = runProgram p (i + n)
    jumpProgram n p = runProgram p (Addr n)
    value = getParamValue program
    write = writeValue program

runBinaryOp :: Program -> (Int -> Int -> Int) -> Param -> Param -> Param -> Program
runBinaryOp program f p1 p2 (PAddr a3) = program'  where
    v1       = getParamValue program p1
    v2       = getParamValue program p2
    v3       = v1 `f` v2
    program' = writeValue program a3 v3
runBinaryOp _ _ _ _ _ = error "parameter 3 is not address"

getParamValue :: Program -> Param -> Int
getParamValue program (PAddr (Addr addr)) =
    fromMaybe (error "program address error") $ program !? addr
getParamValue program (PImm imm) = imm

writeValue :: Program -> Addr -> Int -> Program
writeValue program (Addr addr) x = V.update program $ one (addr, x)

------------
-- Part 2 --
------------

solve2 :: Program -> IO Int
solve2 program = maximum1 <$> outputs  where
    ne            = fromJust . nonEmpty
    phaseSettings = ne $ ne <$> permutations [5 .. 9] :: NonEmpty (NonEmpty Int)
    makeChannels phases = ntake 6 . NE.cycle <$> initIChan `mapM` phases :: IO (NonEmpty IChan)
    outputs = mapM (makeChannels >=> runAmplifiers program) phaseSettings :: IO (NonEmpty Int)

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    program <- parseProgram <$> readFile "inputs/Y2019/Day07.txt" :: IO Program
    print =<< solve1 program
    print =<< solve2 program

parseProgram :: String -> Program
parseProgram line = V.fromList $ map readInt textList
    where textList = words . toText $ map (\c -> if c == ',' then ' ' else c) line
