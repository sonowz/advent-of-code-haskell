module Y2019.Day17 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))
import qualified Data.String as String
import Data.Maybe (fromJust)
import Data.Map (mapKeys)
import qualified Data.Map as M
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

type StationArea = Map Pos Tile
data Tile = TEmpty | TScaffold | TRobot Robot deriving (Eq)

instance S.Show Tile where
    show TEmpty     = "."
    show TScaffold  = "#"
    show (TRobot r) = show r

instance R.Read Tile where
    readsPrec prec = readS      where
        readS ('.' : s) = pure (TEmpty, s)
        readS ('#' : s) = pure (TScaffold, s)
        readS (r   : s) = pure (TRobot (R.read [r]), s)

data Robot = RUp | RDown | RLeft | RRight | RTumbling deriving (Eq)

instance S.Show Robot where
    show RUp       = "^"
    show RDown     = "v"
    show RLeft     = "<"
    show RRight    = ">"
    show RTumbling = "X"

instance R.Read Robot where
    readsPrec prec = readS      where
        readS ('^' : s) = pure (RUp, s)
        readS ('v' : s) = pure (RDown, s)
        readS ('<' : s) = pure (RLeft, s)
        readS ('>' : s) = pure (RRight, s)
        readS ('X' : s) = pure (RTumbling, s)

getTile :: Pos -> StationArea -> Tile
getTile key map = map !? key ?: TEmpty
setTile :: Pos -> Tile -> StationArea -> StationArea
setTile = insert

robotDir RUp    = Dir (0, 1)
robotDir RDown  = Dir (0, -1)
robotDir RLeft  = Dir (-1, 0)
robotDir RRight = Dir (1, 0)
turnLeft RUp    = RLeft
turnLeft RLeft  = RDown
turnLeft RDown  = RRight
turnLeft RRight = RUp
turnRight RUp    = RRight
turnRight RRight = RDown
turnRight RDown  = RLeft
turnRight RLeft  = RUp

------------
-- Part 1 --
------------

-- Note : this solution assumes that there aren't any 2x2 empty space in the area

solve1 :: Program -> IO Int
solve1 program = do
    stationArea <- runAndReadStationArea program
    let intersections = findIntersections stationArea
    return $ sum $ map (\(Pos (x, y)) -> x * y) intersections

runAndReadStationArea :: Program -> IO StationArea
runAndReadStationArea program = do
    inChan   <- newTChanIO
    outChan  <- newTChanIO
    exitChan <- newTChanIO
    forkProgram program inChan outChan exitChan
    readStationArea outChan

readStationArea :: IChan -> IO StationArea
readStationArea outChan = parseArea <$> readAsString  where
    readAsString :: IO String
    readAsString = do
        ascii <- fromIntegral <$> (atomically $ readTChan outChan)
        if 0 <= ascii && ascii < 128 then (<>) [chr ascii] <$> readAsString else return []
    parseArea :: String -> StationArea
    parseArea str = M.fromList areaArrayWithPos      where
        areaArray = map (map (R.read . one)) (String.lines str) :: [[Tile]]
        alongY    = snd . mapAccumL (\p xs -> (p + Pos (0, 1), alongX p xs)) (Pos (0, 0))
        alongX yPos = scanl (\(p, _) t -> (p + Pos (1, 0), t)) (yPos - Pos (1, 0), TEmpty)
        areaArrayWithPos = concat (alongY areaArray) :: [(Pos, Tile)]

findIntersections :: StationArea -> [Pos]
findIntersections areaMap = mapMaybe checkPos (keys areaMap)  where
    checkPos pos = if getTile pos areaMap == TScaffold then checkAdjs else Nothing      where
        checkAdjs =
            if all (\p -> getTile (pos + p) areaMap == TScaffold) dirs then Just pos else Nothing
        dirs = [Pos (1, 0), Pos (-1, 0), Pos (0, 1), Pos (0, -1)] :: [Pos]

------------
-- Part 2 --
------------

-- This part can be quickly solved by hand.

solve2 :: Program -> IO Int
solve2 program = do
    let moveProgram = program V.// [(0, 2)] :: Program
    stationArea <- runAndReadStationArea program
    let path       = getScaffoldPath stationArea
        routineSet = doBacktrack path ?: error "backtracking failed"

    inChan   <- newTChanIO
    outChan  <- newTChanIO
    exitChan <- newTChanIO
    forkProgram moveProgram inChan outChan exitChan
    sendAsciiCommand inChan (routineStateToAsciiCommand routineSet)
    sendAsciiCommand inChan "n\n" -- Continuous video feed ('y' or 'n')

    -- Switching Bool argument lets the program show output or not
    nonAscii <- consumeWhileAscii outChan False
    return $ fromIntegral nonAscii

consumeWhileAscii :: IChan -> Bool -> IO Integer
consumeWhileAscii inChan printToScreen = do
    x <- atomically $ readTChan inChan
    if 0 <= x && x < 128
        then
            (if printToScreen then putChar (chr $ fromIntegral x) else pass)
                >> consumeWhileAscii inChan printToScreen
        else return x


data Segment = Segment Turn Length deriving (Show, Eq)
data Turn = TurnLeft | TurnRight deriving (Show, Eq)
type Length = Int
type Path = [Segment]

data Routine = RTA | RTB | RTC deriving (Show, Eq)
data RoutineState = RoutineState (Maybe Path) (Maybe Path) (Maybe Path) RoutineHistory deriving (Show)
type RoutineHistory = [Routine]
data TrackState = Registering Routine Path | DoingRoutine Routine Path
data Tries = Continue | Change Routine
tries = [Continue, Change RTA, Change RTB, Change RTC]

doBacktrack :: Path -> Maybe RoutineState
doBacktrack path =
    backtrack (RoutineState Nothing Nothing Nothing []) (Registering RTA []) path Continue

backtrack :: RoutineState -> TrackState -> Path -> Tries -> Maybe RoutineState
backtrack routineState (Registering reg regPath) (seg : path) Continue = do
    guard (length regPath <= 5)
    tryNext $ backtrack routineState (Registering reg (regPath <> [seg])) path
backtrack routineState (Registering reg regPath) (seg : path) (Change nextRoutine) = do
    guard (length regPath <= 4)
    routineState' <- register (Registering reg (regPath <> [seg])) routineState
    let trackState' = trackNextRoutine nextRoutine routineState'
    tryNext $ backtrack routineState' trackState' path
backtrack routineState (DoingRoutine routine (routineSeg : routinePath)) (seg : path) Continue = do
    guard (routineSeg == seg)
    guard (not $ null routinePath)
    tryNext $ backtrack routineState (DoingRoutine routine routinePath) path
backtrack routineState (DoingRoutine routine [routineSeg]) (seg : path) (Change nextRoutine) = do
    guard (routineSeg == seg)
    routineState' <- addHistory routine routineState
    let trackState' = trackNextRoutine nextRoutine routineState'
    tryNext $ backtrack routineState' trackState' path
backtrack routineState _ [] (Change _) = do
    guard (isRegistered RTA routineState)
    guard (isRegistered RTB routineState)
    guard (isRegistered RTC routineState)
    Just routineState
backtrack _ _ _ _ = Nothing

register :: TrackState -> RoutineState -> Maybe RoutineState
register (Registering RTA aPath) (RoutineState _ bPath cPath history) =
    checkHistoryLength $ RoutineState (Just aPath) bPath cPath (history <> [RTA])
register (Registering RTB bPath) (RoutineState aPath _ cPath history) =
    checkHistoryLength $ RoutineState aPath (Just bPath) cPath (history <> [RTB])
register (Registering RTC cPath) (RoutineState aPath bPath _ history) =
    checkHistoryLength $ RoutineState aPath bPath (Just cPath) (history <> [RTC])

addHistory routine (RoutineState a b c history) =
    checkHistoryLength $ RoutineState a b c (history <> [routine])

checkHistoryLength routineState@(RoutineState _ _ _ history) =
    if length history <= 10 then Just routineState else Nothing

isRegistered RTA (RoutineState (Just _) _        _        _) = True
isRegistered RTA (RoutineState Nothing  _        _        _) = False
isRegistered RTB (RoutineState _        (Just _) _        _) = True
isRegistered RTB (RoutineState _        Nothing  _        _) = False
isRegistered RTC (RoutineState _        _        (Just _) _) = True
isRegistered RTC (RoutineState _        _        Nothing  _) = False

getPath RTA (RoutineState path _    _    _) = path
getPath RTB (RoutineState _    path _    _) = path
getPath RTC (RoutineState _    _    path _) = path

trackNextRoutine :: Routine -> RoutineState -> TrackState
trackNextRoutine nextRoutine routineState =
    (DoingRoutine nextRoutine <$> getPath nextRoutine routineState) ?: (Registering nextRoutine [])

tryNext :: (Tries -> Maybe RoutineState) -> Maybe RoutineState
tryNext f = viaNonEmpty head $ mapMaybe f tries


routineStateToAsciiCommand :: RoutineState -> String
routineStateToAsciiCommand (RoutineState (Just a) (Just b) (Just c) routineList) =
    mainCommand <> "\n" <> aCommand <> "\n" <> bCommand <> "\n" <> cCommand <> "\n"
  where
    mainCommand = intercalate "," $ map showRoutine routineList
    aCommand    = intercalate "," $ map showSegment a
    bCommand    = intercalate "," $ map showSegment b
    cCommand    = intercalate "," $ map showSegment c
    showRoutine RTA = "A"
    showRoutine RTB = "B"
    showRoutine RTC = "C"
    showTurn TurnLeft  = "L"
    showTurn TurnRight = "R"
    showSegment (Segment turn length) = showTurn turn <> "," <> show length

sendAsciiCommand :: IChan -> String -> IO ()
sendAsciiCommand inChan = mapM_ (send . fromIntegral . ord)
    where send x = atomically $ writeTChan inChan x

getScaffoldPath :: StationArea -> Path
getScaffoldPath areaMap = getScaffoldPath' robotPos turnedRobot turn 0  where
    (robotPos, robot      ) = getRobot areaMap
    (turn    , turnedRobot) = firstTurn robotPos robot
    firstTurn :: Pos -> Robot -> (Turn, Robot)
    firstTurn pos robot = toResult $ find try tries ?: error "should have turn first"      where
        tries = [(TurnLeft, turnLeft), (TurnRight, turnRight)] :: [(Turn, Robot -> Robot)]
        try (turn, f) = getTile (move pos (robotDir (f robot))) areaMap == TScaffold
        toResult (turn, f) = (turn, f robot)
    getScaffoldPath' :: Pos -> Robot -> Turn -> Length -> Path
    getScaffoldPath' pos robot lastTurn lineLength
        | frontTile == TScaffold = getScaffoldPath' frontPos robot lastTurn (lineLength + 1)
        | leftTile == TScaffold = newSegment : getScaffoldPath' leftPos (turnLeft robot) TurnRight 1
        | rightTile == TScaffold = newSegment : getScaffoldPath'
            rightPos
            (turnRight robot)
            TurnLeft
            1
        | otherwise = [newSegment]      where
        frontPos   = move pos (robotDir robot)
        leftPos    = move pos (robotDir (turnLeft robot))
        rightPos   = move pos (robotDir (turnRight robot))
        frontTile  = getTile frontPos areaMap
        leftTile   = getTile leftPos areaMap
        rightTile  = getTile rightPos areaMap
        newSegment = Segment lastTurn lineLength

getRobot :: StationArea -> (Pos, Robot)
getRobot areaMap = fmap tileToRobot $ find isRobot (toPairs areaMap) ?: error
    "failed to find robot"  where
    isRobot (pos, TRobot _) = True
    isRobot _               = False
    tileToRobot (TRobot r) = r

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    program <- parseProgram <$> readFile "inputs/Y2019/Day17.txt" :: IO Program
    print =<< solve1 program
    print =<< solve2 program

parseProgram :: String -> Program
parseProgram line = V.fromList $ map readInteger textList <> replicate 5000 0
    where textList = words . toText $ map (\c -> if c == ',' then ' ' else c) line
