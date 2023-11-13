module Y2019.Day13 where

import Prelude (getChar)
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
sigterm = -999999


data HaltedProgram = Halted Program InOut PC RelBase Cont
data Cont = Block | Halt

forkProgram :: Program -> IChan -> IChan -> IChan -> IO ()
forkProgram program inChan outChan exitChan = void . async $ do
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

type Screen = Map Pos Tile
data Tile = TEmpty | TWall | TBlock | TPaddle | TBall deriving (Eq)

instance S.Show Tile where
    show TEmpty  = "  "
    show TWall   = "██"
    show TBlock  = "##"
    show TPaddle = "=="
    show TBall   = "()"


getTile :: Pos -> Screen -> Tile
getTile key map = map !? key ?: TEmpty
setTile :: Pos -> Tile -> Screen -> Screen
setTile = insert


------------
-- Part 1 --
------------

solve1 :: Program -> IO Integer
solve1 program = do
    inChan   <- newTChanIO
    outChan  <- newTChanIO
    exitChan <- newTChanIO
    forkProgram program inChan outChan exitChan
    let initMap = mempty :: Screen
    screen <- initArcadeWithProgram (inChan, outChan, exitChan) initMap
    return . fromIntegral . length $ M.filter (== TBlock) screen

initArcadeWithProgram :: (IChan, IChan, IChan) -> Screen -> IO Screen
initArcadeWithProgram chans@(inChan, outChan, exitChan) screen = runLoop  where
    checkProgramHalt = atomically (isEmptyTChan exitChan) :: IO Bool
    runLoop          = do
        (res :: Either () Screen) <- runExceptT $ do
            x <- lift $ fromIntegral <$> atomically (readTChan outChan)
            guard (x /= fromIntegral sigterm)
            y <- lift $ fromIntegral <$> atomically (readTChan outChan)
            guard (y /= fromIntegral sigterm)
            tile <- lift $ decodeTile <$> atomically (readTChan outChan)
            let pos     = Pos (x, y)
            let screen' = setTile pos tile screen
            lift $ initArcadeWithProgram chans screen'
        whenRight screen res return

decodeTile 0 = TEmpty
decodeTile 1 = TWall
decodeTile 2 = TBlock
decodeTile 3 = TPaddle
decodeTile 4 = TBall
decodeTile _ = TEmpty

------------
-- Part 2 --
------------

-- This part requires beating Arkanoid game to get answer --

solve2 :: Program -> IO Int
solve2 program = do
    hSetBuffering stdin NoBuffering -- For realtime arcade input
    putStrLn "Hit A to autoplay, or any other key to manual play"
    autoplay <- (==) 'a' <$> getChar
    putStrLn "Use G, H, Space key to play the arcade!"
    inChan   <- newTChanIO
    outChan  <- newTChanIO
    exitChan' <- newTChanIO
    let freeCoinProgram = writeValue program (Addr 0) (PAddr (Addr 0)) 2
    forkProgram freeCoinProgram inChan outChan exitChan'
    runArcadeWithProgram autoplay (inChan, outChan, exitChan') mempty 0

type Score = Int
data Joystick = JLeft | JNeutral | JRight deriving (Show)

runArcadeWithProgram :: Bool -> (IChan, IChan, IChan) -> Screen -> Score -> IO Score
runArcadeWithProgram autoplay chans@(inChan, outChan, exitChan) screen score = runLoop  where
    checkProgramHalt = atomically (isEmptyTChan exitChan) :: IO Bool
    runLoop          = do
        (res :: Either () Score) <- runExceptT $ do
            x' <- lift $ fromIntegral <$> atomically (readTChan outChan)
            guard (x' /= fromIntegral sigterm)
            y' <- lift $ fromIntegral <$> atomically (readTChan outChan)
            guard (y' /= fromIntegral sigterm)
            tileOrScore <- lift $ atomically (readTChan outChan)
            let pos     = Pos (x', y')
            let tile    = decodeTile tileOrScore
            let score'  = fromIntegral tileOrScore
            let screen' = setTile pos tile screen
            -- If ball is drawn, this is end of draw frame. Therefore, do real I/O
            lift $ ifM (return $ tile == TBall) (runRealworldIO autoplay screen') pass
            if pos == Pos (-1, 0)
                then lift $ runArcadeWithProgram autoplay chans screen score' -- Update score
                else lift $ runArcadeWithProgram autoplay chans screen' score -- Update screen
        whenRight score res return
    -- Print screen & get keyboard input (or autoplay)
    runRealworldIO False newScreen = do
        printGameScreen newScreen
        writeJoystick =<< getKeyboardInput
    runRealworldIO True newScreen = do
        printGameScreen newScreen
        threadDelay (25 * 1000) -- 0.025 second == 40 FPS
        writeJoystick joystick      where
        getPosOf tile = fst <$> viaNonEmpty head (filter ((==) tile . snd) (M.assocs newScreen))
        joystick = fromMaybe JNeutral $ do -- Maybe monad
            Pos (ballX  , _) <- getPosOf TBall
            Pos (paddleX, _) <- getPosOf TPaddle
            let relPos = signum (ballX - paddleX)
            return (decodeJoystick relPos)
    getKeyboardInput = getChar >>= \c -> maybe getKeyboardInput return (decodeKeyboardInput c)
    writeJoystick joystick = atomically $ writeTChan inChan (encodeJoystick joystick)
    printGameScreen newScreen = do
        putStrLn ""
        putStrLn $ "Score: " <> show score
        putStr $ showMap newScreen "  "
        putStrLn ""

decodeKeyboardInput 'g' = Just JLeft
decodeKeyboardInput 'h' = Just JRight
decodeKeyboardInput ' ' = Just JNeutral
decodeKeyboardInput _   = Nothing
encodeJoystick JLeft    = -1
encodeJoystick JNeutral = 0
encodeJoystick JRight   = 1
decodeJoystick (-1) = JLeft
decodeJoystick 0    = JNeutral
decodeJoystick 1    = JRight

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    program <- parseProgram <$> readFile "inputs/Y2019/Day13.txt" :: IO Program
    print =<< solve1 program
    print =<< solve2 program

parseProgram :: String -> Program
parseProgram line = V.fromList $ map readInteger textList <> replicate 1000 0
    where textList = words . toText $ map (\c -> if c == ',' then ' ' else c) line
