import Control.Monad
import Data.Function
import Data.List
import Data.Bits
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Opcode       = Int
type Operands     = (Int, Int, Int)
type Inst         = Operands -> Registers -> Registers
data Instructions = Instructions (String -> Inst)
data Registers    = Registers (Vector Int)

instance Eq Registers where
    (Registers r1) == (Registers r2) = r1 == r2

emptyInst = Instructions (\_ -> error "No such instruction")

addInst :: String -> Inst -> Instructions -> Instructions
addInst name f (Instructions fs) = Instructions (\s -> if s == name then f else fs s)

runInst :: String -> Operands -> Registers -> Instructions -> Registers
runInst name ops regs (Instructions inst) = (inst name) ops regs

getReg :: Registers -> Int -> Int
getReg (Registers regs) n = regs ! n

setReg :: Registers -> Int -> Int -> Registers
setReg (Registers regs) n x = Registers (regs // [(n, x)])

initReg = Registers (Vec.replicate 10 0)

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

instNames = ["addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti",
    "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr"]


batchInst :: Operands -> Registers -> [(String, Registers)]
batchInst ops regs = zip instNames $ map (\name -> runInst name ops regs instSet) instNames

candOps :: Sample -> [String]
candOps ((_, ops), reg1, reg2) = map fst corrects where
    batchResult = batchInst ops reg1
    corrects = filter (\(name, reg') -> reg' == reg2) batchResult

solve1 :: [Sample] -> Int
solve1 samples = length gte3 where
    candOpsCounts = map (length . candOps) samples
    gte3 = filter (\x -> x >= 3) candOpsCounts


foldlMutate :: ([a] -> a -> [a]) -> [a] -> [a]
foldlMutate f l = foldlMutate' 0 f l where
    foldlMutate' i f l
        | i >= length l = l
        | otherwise = foldlMutate' (i+1) f (f l $ l !! i)

getOpcodeMap :: [Sample] -> IntMap String
getOpcodeMap samples = IntMap.map (\l -> l !! 0) opcodeMap'' where
    -- Intersection
    opcodes = [0..(length instNames - 1)]
    opcodeMap :: IntMap [String]
    opcodeMap = IntMap.fromList (zip opcodes $ repeat instNames)
    getOpcode ((opcode, _), _, _) = opcode
    foldFn m s = IntMap.adjust (intersect $ candOps s) (getOpcode s) m
    opcodeMap' = foldl foldFn opcodeMap samples

    -- Elimination
    sortByCandCnt = sortBy (compare `on` (length . snd))
    assocs = IntMap.assocs opcodeMap'
    assocs' = sortByCandCnt assocs
    deleteCand l (opc, cands) = sortByCandCnt deleted where
        x = cands !! 0
        deleted = map (\(opc', cands') -> if opc /= opc' then (opc', delete x cands') else (opc', cands')) l 
    opcodeMap'' = IntMap.fromList $ foldlMutate deleteCand assocs'

solve2 :: [Sample] -> Program -> Int
solve2 samples program = getReg finalState 0 where
    opcodeMap = getOpcodeMap samples
    runner regs (opcode, ops) = runInst opname ops regs instSet where
        opname = opcodeMap IntMap.! opcode
    finalState = foldl runner initReg program


type Sample = ((Opcode, Operands), Registers, Registers)
type Program = [(Opcode, Operands)]

getRegister :: String -> Registers
getRegister l = Registers (Vec.fromList ints) where
    l' = filter ((/=) ',') $ take 10 $ drop 9 l
    ints = map read $ words l'

getSample :: IO Sample
getSample = do
    reg1 <- getRegister <$> getLine
    [op, a, b, c] <- map read <$> words <$> getLine
    reg2 <- getRegister <$> getLine
    getLine
    return ((op, (a, b, c)), reg1, reg2)

getProgram :: IO (Opcode, Operands)
getProgram = do
    [op, a, b, c] <- map read <$> words <$> getLine
    return (op, (a, b, c))

main' = do
    samples <- replicateM 811 getSample
    getLine
    getLine
    program <- replicateM 1020 getProgram
    putStrLn $ show $ solve1 samples
    putStrLn $ show $ solve2 samples program
