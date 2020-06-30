module Y2019.Day15 where

import Prelude (getChar)
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))
import Algebra.Graph.Label
import Algebra.Graph.Labelled.AdjacencyMap
import qualified Algebra.Graph.Labelled.AdjacencyMap as A
import Data.Maybe (fromJust)
import Data.Map (mapKeys)
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import qualified Text.Show as S
import System.IO (hSetBuffering, BufferMode(..))
import Lib.Types
import Lib.IO
import Lib.Vector2D
import Lib.Graph

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

type ScanArea = Map Pos Tile
data Tile = TUnknown | TEmpty | TWall | TOxygen deriving (Eq)

instance S.Show Tile where
    show TUnknown = "??"
    show TEmpty   = "  "
    show TWall    = "##"
    show TOxygen  = "O "

getTile :: Pos -> ScanArea -> Tile
getTile key map = map !? key ?: TUnknown
setTile :: Pos -> Tile -> ScanArea -> ScanArea
setTile = insert

data TurnSide = TurnLeft | TurnRight deriving (Eq)
data Heading = North | South | West | East deriving (Show)
toDir North = Dir (0, 1)
toDir South = Dir (0, -1)
toDir West  = Dir (-1, 0)
toDir East  = Dir (1, 0)
fromDir (Dir (0 , 1 )) = North
fromDir (Dir (0 , -1)) = South
fromDir (Dir (-1, 0 )) = West
fromDir (Dir (1 , 0 )) = East
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

------------
-- Part 1 --
------------

-- Note : this solution assumes that there aren't any 2x2 empty space in the area

solve1 :: Program -> IO Int
solve1 program = do
    scannedArea <- scanArea program
    let oxygenPos = fst . fromJust $ find (\(_, t) -> t == TOxygen) (toPairs scannedArea)
    return $ findShortestPath scannedArea (Pos (0, 0)) oxygenPos

scanArea :: Program -> IO ScanArea
scanArea program = do
    leftScannedArea  <- runUntilFindOxygen TurnLeft program
    rightScannedArea <- runUntilFindOxygen TurnRight program
    let scannedArea = mergeScanArea leftScannedArea rightScannedArea
    return scannedArea

runUntilFindOxygen :: TurnSide -> Program -> IO ScanArea
runUntilFindOxygen turnSide program = do
    inChan        <- newTChanIO
    outChan       <- newTChanIO
    exitChan      <- newTChanIO
    programThread <- forkProgram program inChan outChan exitChan

    atomically $ writeTChan outChan 1 -- Write down TEmpty as initial output
    scannedArea <- runUntilFindOxygen' inChan outChan mempty (Pos (0, -1)) North
    cancel programThread
    return scannedArea
  where
    runUntilFindOxygen' :: IChan -> IChan -> ScanArea -> Pos -> Heading -> IO ScanArea
    runUntilFindOxygen' inChan outChan area prevPos heading = do
        scannedTile <- readTile
        let newPos     = move prevPos (toDir heading)
            area'      = setTile newPos scannedTile area
            currentPos = case scannedTile of
                TOxygen -> Nothing
                TEmpty  -> Just newPos
                TWall   -> Just prevPos
            continueLoop pos =
                let heading' = runDirLogic turnSide area' pos heading
                in
                    writeDir (toDir heading')
                        >> runUntilFindOxygen' inChan outChan area' pos heading'
        maybe (return area') continueLoop currentPos
      where
        readTile = decodeTile <$> atomically (readTChan outChan)
        writeDir dir = atomically (writeTChan inChan (encodeDir dir))

-- Maze traversing algorithm is used
runDirLogic :: TurnSide -> ScanArea -> Pos -> Heading -> Heading
runDirLogic turnSide area pos heading
    | handTile == TWall && frontTile /= TWall = heading
    | handTile == TWall && frontTile == TWall = turnR heading
    | handTile /= TWall                       = turn heading
  where
    headings = iterate turnLeft heading
    (frontTile : leftTile : backTile : rightTile : _) =
        map (\h -> getTile (move pos (toDir h)) area) headings
    handTile      = if turnSide == TurnLeft then leftTile else rightTile
    (turn, turnR) = if turnSide == TurnLeft then (turnLeft, turnRight) else (turnRight, turnLeft)


encodeDir (Dir (0 , 1 )) = 1
encodeDir (Dir (0 , -1)) = 2
encodeDir (Dir (-1, 0 )) = 3
encodeDir (Dir (1 , 0 )) = 4

decodeTile 0 = TWall
decodeTile 1 = TEmpty
decodeTile 2 = TOxygen
decodeTile _ = error "decodeTile failure"

mergeScanArea :: ScanArea -> ScanArea -> ScanArea
mergeScanArea src dst = foldl'
    (\m (pos, tile) -> insertWith mergeTile pos tile m)
    dst
    (toPairs src)  where
    mergeTile TUnknown t        = t
    mergeTile t        TUnknown = t
    mergeTile t        _        = t

findShortestPath :: ScanArea -> Pos -> Pos -> Int
findShortestPath areaMap src dst = (fromJust . getFinite . getDistance) dstDistance  where
    areaGraph         = makeGraph areaMap
    bellmanFordResult = bellmanFord areaGraph src
    dstDistance       = bellmanFordResult !? dst ?: 0

makeGraph :: ScanArea -> AdjacencyMap (Distance Int) Pos
makeGraph areaMap = overlays pointGraphs  where
    pointGraphs = map makePointGraph (toPairs areaMap)
    makePointGraph :: (Pos, Tile) -> AdjacencyMap (Distance Int) Pos
    makePointGraph (pos, tile)
        | tile & isEmptyOrOxygen = connect 1 (vertex pos) adjacents
        | otherwise              = A.empty      where
        adjacents    = overlays $ mapMaybe ifEmptyThenVertex adjacentDirs
        adjacentDirs = [Dir (1, 0), Dir (-1, 0), Dir (0, 1), Dir (0, -1)]
        ifEmptyThenVertex d =
            let pos' = move pos d
            in if getTile pos' areaMap & isEmptyOrOxygen then Just (vertex pos') else Nothing
        isEmptyOrOxygen t = t == TEmpty || t == TOxygen

------------
-- Part 2 --
------------

solve2 :: Program -> IO Int
solve2 program = do
    scannedArea <- scanArea program
    let oxygenPos = fst . fromJust $ find (\(_, t) -> t == TOxygen) (toPairs scannedArea)
    return $ findFarthestPath scannedArea oxygenPos

findFarthestPath :: ScanArea -> Pos -> Int
findFarthestPath areaMap src = (fromJust . getFinite . getDistance) farthestDistance  where
    areaGraph         = makeGraph areaMap
    bellmanFordResult = bellmanFord areaGraph src
    farthestDistance  = viaNonEmpty maximum1 (elems bellmanFordResult) ?: 0

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    program <- parseProgram <$> readFile "inputs/Y2019/Day15.txt" :: IO Program
    print =<< solve1 program
    print =<< solve2 program

parseProgram :: String -> Program
parseProgram line = V.fromList $ map readInteger textList <> replicate 1000 0
    where textList = words . toText $ map (\c -> if c == ',' then ' ' else c) line
