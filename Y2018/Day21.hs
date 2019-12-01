import Control.Monad
import Data.Function
import Data.List
import Data.Bits
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

-------------------
-- Copy of 19.hs --
-------------------

type Opcode       = String -- Changed from Int to String
type Operands     = (Int, Int, Int)
type Inst         = Operands -> Registers -> Registers
data Instructions = Instructions (String -> Inst)
data Registers    = Registers (Vector Int)

instance Eq Registers where
    (Registers r1) == (Registers r2) = r1 == r2

instance Show Registers where
    show (Registers r) = show r

emptyInst = Instructions (\_ -> error "No such instruction")

addInst :: Opcode -> Inst -> Instructions -> Instructions
addInst name f (Instructions fs) = Instructions (\s -> if s == name then f else fs s)

runInst :: Opcode -> Operands -> Registers -> Instructions -> Registers
runInst name ops regs (Instructions inst) = (inst name) ops regs

getReg :: Registers -> Int -> Int
getReg (Registers regs) n = regs ! n

setReg :: Registers -> Int -> Int -> Registers
setReg (Registers regs) n x = Registers (regs // [(n, x)])

initReg = Registers (Vec.replicate 6 0)

addArith prefix op =
    addInst (prefix ++ "r") (\(a, b, c) r -> setReg r c (getReg r a `op` getReg r b)) .
    addInst (prefix ++ "i") (\(a, b, c) r -> setReg r c (getReg r a `op` b))

instSet = 
    ( addArith "add" (+)
    . addArith "mul" (*)
    . addArith "ban" (.&.)
    . addArith "bor" (.|.)
    . addInst "setr" (\(a, b, c) r -> setReg r c (getReg r a))
    . addInst "seti" (\(a, b, c) r -> setReg r c a)
    . addInst "gtir" (\(a, b, c) r -> setReg r c (if a > getReg r b then 1 else 0))
    . addInst "gtri" (\(a, b, c) r -> setReg r c (if getReg r a > b then 1 else 0))
    . addInst "gtrr" (\(a, b, c) r -> setReg r c (if getReg r a > getReg r b then 1 else 0))
    . addInst "eqir" (\(a, b, c) r -> setReg r c (if a == getReg r b then 1 else 0))
    . addInst "eqri" (\(a, b, c) r -> setReg r c (if getReg r a == b then 1 else 0))
    . addInst "eqrr" (\(a, b, c) r -> setReg r c (if getReg r a == getReg r b then 1 else 0))
    ) emptyInst

runProgram :: Int -> Program -> Int -> Registers -> Registers
runProgram ip program pc regs
    | pc < 0 || pc >= length program = regs
    | otherwise = runProgram ip program pc' regs'' where
        (opcode, ops) = program ! pc
        regs' = setReg regs ip pc
        regs'' = runInst opcode ops regs' instSet
        pc' = (getReg regs'' ip) + 1

runProgramN ip program pc regs n
    | pc < 0 || pc >= length program || n == 0 = regs
    | otherwise = runProgramN ip program pc' regs'' (n-1) where
        (opcode, ops) = program ! pc
        regs' = setReg regs ip pc
        regs'' = runInst opcode ops regs' instSet
        pc' = (getReg regs'' ip) + 1

-----------------
-- End of copy --
-----------------

-- Run with breakpoint
runProgramBreak :: Int -> Int -> Program -> Int -> Registers -> Registers
runProgramBreak bp = runProgramUntil (\pc' _ -> pc' /= bp)

runProgramUntil :: (Int -> Registers -> Bool) -> Int -> Program -> Int -> Registers -> Registers
runProgramUntil f ip program pc regs
    | not $ f pc regs || pc < 0 || pc >= length program = regs
    | otherwise = runProgramUntil f ip program pc' regs'' where
        (opcode, ops) = program ! pc
        regs' = setReg regs ip pc
        regs'' = runInst opcode ops regs' instSet
        pc' = (getReg regs'' ip) + 1

-- Semantic analysis of the program: the program halts if r0 == r3 (line 28), where r3 is hashed itself witin loop.
-- So find first r3 value in line 28.
solve1 :: Int -> Program -> Int
solve1 ip program = getReg regs 3 where
    breakpoint = 28
    regs = runProgramBreak breakpoint ip program 0 initReg

-- Hashing sequence will form a cycle eventually, so find the end of the cycle
-- (Starting hash might not be in the cycle, like '6' shape)
-- It takes ~30 minutes to solve
solve2 :: Int -> Program -> Int
solve2 ip program = fst $ last pseudoCycle where
    breakpoint = 28
    regs = runProgramBreak breakpoint ip program 0 initReg
    regs' = runProgramN ip program breakpoint regs 1
    hashCycle lregs = trace (show $ getReg lregs'' 3) lregs'' where -- pc should be (breakpoint+1)
        lregs' = runProgramBreak breakpoint ip program (breakpoint+1) lregs
        lregs'' = runProgramN ip program breakpoint lregs' 1
    hashValues = iterate hashCycle regs' -- [Registers]
    hashValues' = map (\r -> getReg r 3) hashValues -- [hash values]
    hashSets = scanl (flip Set.insert) Set.empty hashValues'
    isUnique (x, s) = Set.notMember x s
    pseudoCycle = takeWhile isUnique (zip hashValues' (Set.empty : hashSets))
    

type Program = Vector (Opcode, Operands)

getProgram :: IO (Opcode, Operands)
getProgram = do
    (opcode, ops) <- splitAt 1 <$> words <$> getLine
    [a, b, c] <- return $ map read ops
    return (opcode !! 0, (a, b, c))

getInstructionPointer :: IO Int
getInstructionPointer = do
    line <- getLine
    return $ read $ [line !! 4]


main' = do
    ip <- getInstructionPointer
    program <- Vec.fromList <$> replicateM 31 getProgram
    putStrLn $ show $ solve1 ip program
    putStrLn $ show $ solve2 ip program
