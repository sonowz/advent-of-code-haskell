module Y2019.Day19 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))
import Data.Maybe (fromJust)
import Data.Map (mapKeys)
import qualified Data.Map as M
import qualified Data.Map.Lazy as LM
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import qualified Text.Read as R
import qualified Text.Show as S
import System.IO (hSetBuffering, BufferMode(..), putChar)
import Lib.Types
import Lib.IO
import Lib.Vector2D

----------------------
-- Intcode Computer --
----------------------

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
data InOut = InOut (Maybe Integer) [Integer] deriving (Show)
type IChan = TChan Integer

readIn (InOut input output) = (input, InOut Nothing output)
writeOut (InOut input output) t = InOut input (output <> [t])
sigterm = -999999


data HaltedProgram = Halted Program InOut PC RelBase Cont
data Cont = Block | Halt

forkProgram :: Program -> IChan -> IChan -> IChan -> IO (Async ())
forkProgram program inChan outChan exitChan = async $ do
    -- Optimization: use IORef to store program vector
    programRef <- newIORef program
    let runner (Halted _ (InOut input output) pc relBase Halt) = do
            atomically $ writeOutputs outChan output
            atomically $ writeTChan outChan sigterm >> writeTChan exitChan 0 -- exit 0
        runner (Halted _ (InOut input output) pc relBase Block) = do
            atomically $ writeOutputs outChan output
            input' <- if isJust input
                then return input
                else do
                    threadDelay 100
                    atomically $ tryReadTChan inChan :: IO (Maybe Integer)
            program' <- readIORef programRef
            let
                Halted program'' inout' pc' relBase' cont' =
                    runProgram (InOut input' []) program' pc relBase
            writeIORef programRef program''
            runner $ Halted program inout' pc' relBase' cont'
    runner initState  where
    initState = Halted program (InOut Nothing []) (Addr 0) (Addr 0) Block
    writeOutputs chan outputs = forM_ outputs (writeTChan chan)

-- Debug program with interactive I/O
runInteractiveMode :: Program -> IO ()
runInteractiveMode program = do
    inChan   <- newTChanIO
    outChan  <- newTChanIO
    exitChan <- newTChanIO
    forkProgram program inChan outChan exitChan
    async $ forever $ atomically (readTChan outChan) >>= \x -> putTextLn ("> " <> show x)
    forever $ getLine >>= \x -> atomically (writeTChan inChan (readInteger x))

runProgram :: InOut -> Program -> PC -> RelBase -> HaltedProgram
runProgram inout !program !i !relBase = program'  where -- Used bang pattern(!) for eager evaluation
    program' = case parseProgramLine program i of
        P3 OpAdd p1 p2 p3 -> runBinaryOp program relBase (+) p1 p2 p3 & continueProgram inout 4
        P3 OpMul p1 p2 p3 -> runBinaryOp program relBase (*) p1 p2 p3 & continueProgram inout 4
        P1 OpInput p1 ->
            let (input, inout') = readIn inout
            in maybe (halt Block) (continueProgram inout' 2 . write p1) input
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
        P0 OpHalt -> halt Halt
    halt = Halted program inout i relBase
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
    value   = fromMaybe (error "program address error") $ program V.!? intAddr

writeValue :: Program -> RelBase -> Param -> Integer -> Program
writeValue program relBase param x = program'  where
    intAddr  = fromIntegral . un @Integer . toAddr relBase $ param :: Int
    program' = V.update program $ Relude.one (intAddr, x)

-----------------------
-- Type declarations --
-----------------------

newtype Pos = Pos (Int, Int) deriving (Pos2D, Ord, Eq, Show) via (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Num) via Pos

instance Num Pos where
    Pos (x1, y1) + Pos (x2, y2) = Pos (x1 + x2, y1 + y2)
    Pos (x1, y1) - Pos (x2, y2) = Pos (x1 - x2, y1 - y2)
    Pos (x1, y1) * Pos (x2, y2) = Pos (x1 * x2, y1 * y2)
    abs (Pos (x, y)) = Pos (abs x, abs y)
    signum (Pos (x, y)) = Pos (signum x, signum y)
    fromInteger x = Pos (fromInteger x, fromInteger x)

move (Pos (x, y)) (Dir (dx, dy)) = Pos (x + dx, y + dy)

type EmitterArea = LM.Map Pos Tile -- Using lazy version for Part 2
data Tile = TStationary | TPulled deriving (Eq)

instance S.Show Tile where
    show TStationary = "."
    show TPulled     = "#"

getTile :: Pos -> EmitterArea -> Tile
getTile key map = map !? key ?: TStationary
setTile :: Pos -> Tile -> EmitterArea -> EmitterArea
setTile = insert

------------
-- Part 1 --
------------

solve1 :: Program -> IO Int
solve1 program = countPulled <$> runAndScanEmitterArea program

runAndScanEmitterArea :: Program -> IO EmitterArea
runAndScanEmitterArea program = emitterArea  where
    area = [ Pos (x, y) | x <- [0 .. 49], y <- [0 .. 49] ] :: [Pos]
    foldfn m p = readTileAtPos program p >>= \t -> return (insert p t m)
    emitterArea = foldlM foldfn mempty area

readTileAtPos :: Program -> Pos -> IO Tile
readTileAtPos program (Pos (x, y)) = do
    inChan   <- newTChanIO
    outChan  <- newTChanIO
    exitChan <- newTChanIO
    running  <- forkProgram program inChan outChan exitChan
    atomically $ writeTChan inChan (fromIntegral x) >> writeTChan inChan (fromIntegral y)
    tile <- decodeTile <$> atomically (readTChan outChan)
    cancel running
    return tile

countPulled :: EmitterArea -> Int
countPulled emitterArea = length $ filter (== TPulled) (elems emitterArea)

decodeTile 0 = TStationary
decodeTile 1 = TPulled

------------
-- Part 2 --
------------

solve2 :: Program -> IO Int
solve2 program = do
    startPos <- findStartPos 1 program
    answer <$> findBeamPos 100 program (startPos, startPos)
    where answer (Pos (x, y)) = 10000 * x + y

findStartPos :: Int -> Program -> IO Pos
findStartPos n program = getStartPos >>= after  where
    after []         = findStartPos (n + 1) program
    after [startPos] = return startPos
    getStartPos =
        catMaybes <$> mapM
            findPulled
            ([ Pos (x, n) | x <- [0 .. n] ] <> [ Pos (n, y) | y <- [0 .. n] ]) :: IO [Pos]
    findPulled p = readTileAtPos program p >>= \case
        TPulled     -> return $ Just p
        TStationary -> return Nothing

findBeamPos :: Int -> Program -> (Pos, Pos) -> IO Pos
findBeamPos reqSize program (upBound, lowBound)
    | size == reqSize = return $ Pos (ux - size + 1, uy)
    | otherwise       = doLoop  where
    (Pos (ux, uy), Pos (lx, ly)) = (upBound, lowBound)
    size                         = ux - lx + 1
    doLoop                       = do
        upBound'  <- getNextBeamUpperBound program upBound
        lowBound' <- findMatchingLowBound program upBound' lowBound
        findBeamPos reqSize program (upBound', lowBound')

findMatchingLowBound :: Program -> Pos -> Pos -> IO Pos
findMatchingLowBound program up@(Pos (ux, uy)) low@(Pos (lx, ly))
    | ux + uy == lx + ly = return low
    | otherwise          = findMatchingLowBound program up =<< getNextBeamLowerBound program low

getNextBeamUpperBound :: Program -> Pos -> IO Pos
getNextBeamUpperBound program pos@(Pos (x, y)) = ifRightTileOr $ ifDiagTileOr downTile  where
    ifRightTileOr or = readTileAtPos program (Pos (x + 1, y)) >>= \case
        TPulled     -> return $ Pos (x + 1, y)
        TStationary -> or
    ifDiagTileOr or = readTileAtPos program (Pos (x + 1, y + 1)) >>= \case
        TPulled     -> return $ Pos (x + 1, y + 1)
        TStationary -> or
    downTile = return $ Pos (x, y + 1)

getNextBeamLowerBound :: Program -> Pos -> IO Pos
getNextBeamLowerBound program pos@(Pos (x, y)) = ifDownTileOr rightTile  where
    ifDownTileOr or = readTileAtPos program (Pos (x, y + 1)) >>= \case
        TPulled     -> return $ Pos (x, y + 1)
        TStationary -> or
    rightTile = return $ Pos (x + 1, y)

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    program <- parseProgram <$> readFile "inputs/Y2019/Day19.txt" :: IO Program
    print =<< solve1 program
    print =<< solve2 program

parseProgram :: String -> Program
parseProgram line = V.fromList $ map readInteger textList <> replicate 10000 0
    where textList = words . toText $ map (\c -> if c == ',' then ' ' else c) line
