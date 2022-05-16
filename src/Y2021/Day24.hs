module Y2021.Day24 where

import qualified Data.Vector as V
import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import qualified Relude.Unsafe as Unsafe
import System.Random


-- Note: this part requires analysis of input instructions to figure out the solution
-- The program is divided into 14 sections which is either 'push' or 'pop' in Stack operation.
-- Bottom, or leaf level push-pops are independent of other inputs,
--   while higher, or non-leaf level push-pops are dependent of its lower level inputs.
-- This solution utilizes properties mentioned above and uses a brute-force approach.
-- In other words, 7x9x9 cases are examined, where 7 is the number of push-pop pairs,
--   and 9x9 is the number of cases in each pair, namely (1, 1) to (9, 9).

-----------------------
-- Type declarations --
-----------------------

type Value = Integer
data Instruction
    = Inp Reg
    | Add Reg Reg
    | Mul Reg Reg
    | Div Reg Reg
    | Mod Reg Reg
    | Eql Reg Reg
    | AddImm Reg Value
    | MulImm Reg Value
    | DivImm Reg Value
    | ModImm Reg Value
    | EqlImm Reg Value
data Reg = RW | RX | RY | RZ deriving (Show)

type Program = [Instruction]
type InputBuffer = [Value]
newtype RegState = RegState (Value, Value, Value, Value) deriving (Show)
newtype ModelNumber = ModelNumber Value deriving (Show)

getReg :: Reg -> RegState -> Value
getReg RW (RegState (w, x, y, z)) = w
getReg RX (RegState (w, x, y, z)) = x
getReg RY (RegState (w, x, y, z)) = y
getReg RZ (RegState (w, x, y, z)) = z

setReg :: Reg -> Value -> RegState -> RegState
setReg RW v (RegState (w, x, y, z)) = RegState (v, x, y, z)
setReg RX v (RegState (w, x, y, z)) = RegState (w, v, y, z)
setReg RY v (RegState (w, x, y, z)) = RegState (w, x, v, z)
setReg RZ v (RegState (w, x, y, z)) = RegState (w, x, y, v)

------------
-- Part 1 --
------------

solve1 :: Program -> Integer
solve1 program = constructValue 0 pairValues  where
    pairValues = foldl' findPairValues [] pairPos
    findPairValues acc pos = acc <> [getMax $ getPairValues program (constructValue 9 acc) pos]
    getMax (PairValues pos values) = PairValue pos (maximum1 values)

sliceInput :: Value -> InputBuffer
sliceInput 0 = []
sliceInput x = sliceInput (x `div` 10) <> [x `mod` 10]

concatNumber :: InputBuffer -> Value
concatNumber = go . reverse  where
    go (x : xs) = x + 10 * go xs
    go _        = 0

runProgram :: Program -> ModelNumber -> RegState
runProgram program modelNumber = regState  where
    inputBuffer   = sliceInput (un modelNumber)
    initState     = RegState (0, 0, 0, 0)
    (regState, _) = flipfoldl' (\x -> wrap . runInstruction x) (initState, inputBuffer) program
    wrap (Left  s) = error (show s)
    wrap (Right x) = x

runInstruction :: Instruction -> (RegState, InputBuffer) -> Either RegState (RegState, InputBuffer)
runInstruction (Inp a     ) (s, input : inputs) = Right (setReg a input s, inputs)
runInstruction (Inp a     ) (s, _             ) = Left s
runInstruction (Add    a b) (s, i             ) = Right (binOp (+) a b s, i)
runInstruction (Mul    a b) (s, i             ) = Right (binOp (*) a b s, i)
runInstruction (Div    a b) (s, i             ) = Right (binOp divTowardsZero a b s, i)
runInstruction (Mod    a b) (s, i             ) = Right (binOp mod a b s, i)
runInstruction (Eql a b) (s, i) = Right (binOp (\x y -> if x == y then 1 else 0) a b s, i)
runInstruction (AddImm a b) (s, i             ) = Right (binOpImmediate (+) a b s, i)
runInstruction (MulImm a b) (s, i             ) = Right (binOpImmediate (*) a b s, i)
runInstruction (DivImm a b) (s, i             ) = Right (binOpImmediate divTowardsZero a b s, i)
runInstruction (ModImm a b) (s, i             ) = Right (binOpImmediate mod a b s, i)
runInstruction (EqlImm a b) (s, i) =
    Right (binOpImmediate (\x y -> if x == y then 1 else 0) a b s, i)

divTowardsZero :: Value -> Value -> Value
divTowardsZero x y = let d = x `div` y in if d < 0 && x `mod` y > 0 then d + 1 else d

binOp :: (Value -> Value -> Value) -> Reg -> Reg -> RegState -> RegState
binOp f a b s = setReg a (getReg a s `f` getReg b s) s

binOpImmediate :: (Value -> Value -> Value) -> Reg -> Value -> RegState -> RegState
binOpImmediate f a imm s = setReg a (getReg a s `f` imm) s


data PairValue = PairValue PairPosition (Value, Value)
    deriving Show
data PairValues = PairValues PairPosition (NonEmpty (Value, Value))
    deriving Show
type PairPosition = (Int, Int)

isValidPair :: Program -> Value -> PairValue -> Bool
isValidPair program baseValue (PairValue (index1, index2) (value1, value2)) = isValid  where
    inputBuffer =
        toList
            .    V.slice 0 (index2 + 1)
            $    fromList (sliceInput baseValue)
            V.// [(index1, value1), (index2, value2)]
    run []           (s, b) = s
    run (ins : inss) (s, b) = case runInstruction ins (s, b) of
        Left  s'       -> s'
        Right (s', b') -> run inss (s', b')
    initState  = RegState (0, 0, 0, 0)
    finalState = run program (initState, inputBuffer)
    isValid    = getReg RX finalState == 0 && getReg RY finalState == 0

-- WARNING: This is an input dependent value!
-- How to construct:
--   The input can be divided into 14 sections, which begins with 'inp w' instruction.
--   If a section contains 'div z 26' instruction, then that is a 'pop' section, otherwise it is a 'push' section.
--   Identify each sections as either 'push' or 'pop' section, and make pairs with 'push' and 'pop'.
--   Then topologically sort each pairs with the deepest depth appearing first.
--   Example: UUUPUPUUPUPPPP ('U' is pUsh, 'P' is Pop)
--            [U[U[UP][UP][U[UP][UP]P]P]P]  # make pairs
--            [ (2, 3), (4, 5), (7, 8), (9, 10), (6, 11), (1, 12), (0, 13) ]  # topologically sorted, denoted as index
pairPos :: [PairPosition]
pairPos = [(2, 3), (4, 5), (7, 8), (9, 10), (6, 11), (1, 12), (0, 13)]

getPairValues :: Program -> Value -> PairPosition -> PairValues
getPairValues program baseValue pos = makePairValues
    $ filter (isValidPair program baseValue) candidates  where
    candidates = [ PairValue pos (v1, v2) | v1 <- [1 .. 9], v2 <- [1 .. 9] ]
    makePairValues =
        PairValues pos . Unsafe.fromJust . nonEmpty . fmap (\(PairValue _ value) -> value)

constructValue :: Value -> [PairValue] -> Value
constructValue defaultValue pairValues = concatNumber $ toList vec'  where
    vec  = V.replicate 14 defaultValue
    vec' = foldl' setPairValue vec pairValues
    setPairValue v (PairValue (i1, i2) (v1, v2)) = v V.// [(i1, v1), (i2, v2)]

------------
-- Part 2 --
------------

solve2 :: Program -> Value
solve2 program = constructValue 0 pairValues  where
    pairValues = foldl' findPairValues [] pairPos
    findPairValues acc pos = acc <> [getMin $ getPairValues program (constructValue 9 acc) pos]
    getMin (PairValues pos values) = PairValue pos (minimum1 values)

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    program <- parseInstruction <<$>> readFileLines "inputs/Y2021/Day24.txt" :: IO Program
    -- findNumberAuto program
    -- findNumberManual program
    print $ solve1 program
    print $ solve2 program

-- This is an utility function to find patterns automatically
-- 'z < 26' condition makes the printed output have high chances of
-- being a solution with a small tweak on numbers
findNumberAuto :: Program -> IO ()
findNumberAuto program = forever $ do
    input <- randomNumber
    let state@(RegState (w, x, y, z)) = runProgram program input
    if z < 26
        then do
            print (un input :: Integer)
            print state
        else pass
  where
    randomNumber :: IO ModelNumber
    randomNumber = ModelNumber . concatNumber <$> replicateM 14 (getStdRandom (randomR (1, 9)))      where

-- This is an utility function to find patterns manually
findNumberManual :: Program -> IO ()
findNumberManual program = forever $ do
    input <- ModelNumber . (\(Right x) -> x) . readEither . toString <$> getLine
    let state@(RegState (w, x, y, z)) = runProgram program input
    print state

parseInstruction :: Text -> Instruction
parseInstruction (toString -> line) = instruction  where
    command = take 3 line
    a       = take 1 . drop 4 $ line
    b       = drop 6 line
    readReg :: String -> Reg
    readReg "w" = RW
    readReg "x" = RX
    readReg "y" = RY
    readReg "z" = RZ
    readReg _   = error "parse error!"
    readRegOrImm :: String -> Either Reg Value
    readRegOrImm "w" = Left RW
    readRegOrImm "x" = Left RX
    readRegOrImm "y" = Left RY
    readRegOrImm "z" = Left RZ
    readRegOrImm ""  = Right 0
    readRegOrImm t   = first (error "parse error!") (readEither t)
    instruction = case (command, readRegOrImm b) of
        ("inp", _      ) -> Inp (readReg a)
        ("add", Left r ) -> Add (readReg a) r
        ("add", Right x) -> AddImm (readReg a) x
        ("mul", Left r ) -> Mul (readReg a) r
        ("mul", Right x) -> MulImm (readReg a) x
        ("div", Left r ) -> Div (readReg a) r
        ("div", Right x) -> DivImm (readReg a) x
        ("mod", Left r ) -> Mod (readReg a) r
        ("mod", Right x) -> ModImm (readReg a) x
        ("eql", Left r ) -> Eql (readReg a) r
        ("eql", Right x) -> EqlImm (readReg a) x
        _                -> error "parse error!"


