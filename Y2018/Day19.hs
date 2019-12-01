import Control.Monad
import Data.Function
import Data.List
import Data.Bits
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import Debug.Trace

-------------------
-- Copy of 16.hs --
-------------------

type Opcode       = String -- Changed from Int to String
type Operands     = (Int, Int, Int)
type Inst         = Operands -> Registers -> Registers
data Instructions = Instructions (String -> Inst)
data Registers    = Registers (Vector Int)

instance Eq Registers where
    (Registers r1) == (Registers r2) = r1 == r2

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

-----------------
-- End of copy --
-----------------

runProgram :: Int -> Program -> Int -> Registers -> Registers
runProgram ip program pc regs
    | pc < 0 || pc >= length program = regs
    | otherwise = runProgram ip program pc' regs'' where
        (opcode, ops) = program ! pc
        regs' = setReg regs ip pc
        regs'' = runInst opcode ops regs' instSet
        pc' = (getReg regs'' ip) + 1

solve1 :: Int -> Program -> Int
solve1 ip program = getReg regs 0 where
    regs = runProgram ip program 0 initReg

-- Part 2 requires 'semantic analysis' of the program, since it takes about 10^15 operations to halt.
-- Input program is same as below code.
{--
r0 = 0
r2 = 10551319  # (was 919 in Part 1)
for r4 in range(r2):
    for r5 in range(r2):
        if r4 * r5 == r2:
            r0 += r4
print(r0)
--}
-- So the answer is (sum of divisors of r2).
-- Below codes are ouptuts I wrote to help analyze the program.

runProgram' ip program pc regs
    | pc < 0 || pc >= length program = ([], regs)
    | otherwise = (pc : records, tRegs) where
        (opcode, ops) = program ! pc
        regs' = setReg regs ip pc
        regs'' = runInst opcode ops regs' instSet
        pc' = (getReg regs'' ip) + 1
        (records, tRegs) = runProgram' ip program pc' regs''

runProgramN ip program pc regs n
    | pc < 0 || pc >= length program || n == 0 = regs
    | otherwise = runProgramN ip program pc' regs'' (n-1) where
        (opcode, ops) = program ! pc
        regs' = setReg regs ip pc
        regs'' = runInst opcode ops regs' instSet
        pc' = (getReg regs'' ip) + 1

runProgramTrace ip program pc regs
    | pc < 0 || pc >= length program = regs
    | otherwise = trace (show r) runProgramTrace ip program pc' regs'' where
        (Registers r) = regs
        (opcode, ops) = program ! pc
        regs' = setReg regs ip pc
        regs'' = runInst opcode ops regs' instSet
        pc' = (getReg regs'' ip) + 1

solve2 :: Int -> Program -> Int
solve2 ip program = getReg regs' 0 where
    (records, regs) = runProgram' ip program 0 initReg
    lastInstructions = drop (length records - 30) records
    r2 = getReg (runProgramN ip program 0 (setReg initReg 0 1) 500) 2 -- r0 <- 1
    regs' = trace ("Last 30 instruction sequence:\n" ++ show lastInstructions ++ "\nRegister 2 value: " ++ show r2 ++ "\nRegister trace from start:") $ 
        runProgramTrace ip program 0 initReg
    
    
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
    program <- Vec.fromList <$> replicateM 36 getProgram
    putStrLn $ show $ solve1 ip program
    putStrLn "Below are part 2 help outputs."
    putStrLn $ show $ solve2 ip program
