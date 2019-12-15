module Y2019.Day11 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))
import Data.Map (mapKeys)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import qualified Text.Show as S
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


data HaltedProgram = Halted Program InOut PC RelBase Cont
data Cont = Block | Halt

forkProgram :: Program -> IChan -> IChan -> IChan -> IO ()
forkProgram program inChan outChan exitChan = void . async $ do
    programRef <- newIORef program
    let runner (Halted _ inout pc relBase Halt) =
            atomically $ writeTChan outChan (-9999) >> writeTChan exitChan 0 -- exit 0
        runner (Halted _ (InOut (Just input) output) pc relBase Block) = do
            if output /= [] then atomically $ writeOutputs outChan output else pass
            program' <- readIORef programRef
            let
                Halted program'' inout' pc' relBase' cont' =
                    runProgram (InOut (Just input) []) program' pc relBase
            writeIORef programRef program''
            runner $ Halted program inout' pc' relBase' cont'
        runner (Halted _ (InOut Nothing output) pc relBase Block) = do
            atomically $ writeOutputs outChan output
            threadDelay 100
            input <- atomically $ tryReadTChan inChan :: IO (Maybe Integer)
            runner $ Halted program (InOut input []) pc relBase Block
    runner initState  where
    initState = Halted program (InOut Nothing []) (Addr 0) (Addr 0) Block
    writeOutputs chan outputs = forM_ outputs (writeTChan chan)


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
    program' = V.update program $ one (intAddr, x)

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

data Direction = DUp | DLeft | DDown | DRight deriving (Show)
toDir DUp    = Dir (0, 1)
toDir DDown  = Dir (0, -1)
toDir DLeft  = Dir (-1, 0)
toDir DRight = Dir (1, 0)
rotLeft DUp    = DLeft
rotLeft DLeft  = DDown
rotLeft DDown  = DRight
rotLeft DRight = DUp
rotRight DUp    = DRight
rotRight DRight = DDown
rotRight DDown  = DLeft
rotRight DLeft  = DUp

data Robot = Robot Pos Direction

type PanelMap = Map Pos Panel
data Panel = Black | White deriving (Eq)

instance S.Show Panel where
    show Black = "  "
    show White = "██"

getPanel :: Pos -> PanelMap -> Panel
getPanel key map = map !? key ?: Black
setPanel :: Pos -> Panel -> PanelMap -> PanelMap
setPanel = insert

------------
-- Part 1 --
------------

solve1 :: Program -> IO Integer
solve1 program = do
    inChan   <- newTChanIO
    outChan  <- newTChanIO
    exitChan <- newTChanIO
    forkProgram program inChan outChan exitChan
    let initMap = one (Pos (0, 0), Black) :: PanelMap
    panelMap <- runRobotWithProgram (inChan, outChan, exitChan) (Robot (Pos (0, 0)) DUp) initMap
    return . fromIntegral $ length panelMap

runRobotWithProgram :: (IChan, IChan, IChan) -> Robot -> PanelMap -> IO PanelMap
runRobotWithProgram chans@(inChan, outChan, exitChan) robot@(Robot pos _) panelMap = runLoop  where
    currentPanel = getPanel pos panelMap
    encodePanel Black = 0
    encodePanel White = 1
    decodePanel 0 = Black
    decodePanel 1 = White
    decodePanel _ = Black
    decodeDirection 0 = DLeft
    decodeDirection 1 = DRight
    checkProgramHalt = atomically (isEmptyTChan exitChan) :: IO Bool
    runLoop          = do
        (res :: Either () PanelMap) <- runExceptT $ do
            guardM (lift checkProgramHalt)
            lift $ atomically $ writeTChan inChan (encodePanel currentPanel)
            panel <- lift $ decodePanel <$> atomically (readTChan outChan)
            guardM (lift checkProgramHalt)
            rotation <- lift $ decodeDirection <$> atomically (readTChan outChan)
            guardM (lift checkProgramHalt)
            let (robot', panelMap') = paintPanel robot rotation panel panelMap
            lift $ runRobotWithProgram chans robot' panelMap'
        whenRight panelMap res return


paintPanel :: Robot -> Direction -> Panel -> PanelMap -> (Robot, PanelMap)
paintPanel (Robot rp rd) direction panel panelMap = (Robot rp' rd', panelMap')  where
    rotateFn DLeft  = rotLeft
    rotateFn DRight = rotRight
    rp'       = rp `move` toDir rd'
    rd'       = rotateFn direction rd
    panelMap' = setPanel rp panel panelMap

------------
-- Part 2 --
------------

solve2 :: Program -> IO String
solve2 program = do
    inChan   <- newTChanIO
    outChan  <- newTChanIO
    exitChan' <- newTChanIO
    forkProgram program inChan outChan exitChan'
    let initMap = one (Pos (0, 0), White) :: PanelMap
    panelMap <- runRobotWithProgram (inChan, outChan, exitChan') (Robot (Pos (0, 0)) DUp) initMap
    let yInversedPanelMap = mapKeys (\(Pos (x, y)) -> Pos (x, -y)) panelMap
    return $ showMap yInversedPanelMap "  "

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    program <- parseProgram <$> readFile "inputs/Y2019/Day11.txt" :: IO Program
    print =<< solve1 program
    putStr =<< solve2 program

parseProgram :: String -> Program
parseProgram line = V.fromList $ map readInteger textList <> replicate 1000 0
    where textList = words . toText $ map (\c -> if c == ',' then ' ' else c) line
