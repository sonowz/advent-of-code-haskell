import Control.Monad
import Data.Function
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

fiveChunks (x1:x2:x3:x4:x5:xs) = (x1:x2:x3:x4:[x5]) : fiveChunks (x2:x3:x4:x5:xs)
fiveChunks _ = []

nextState trans state = ".." ++ map gen chunks ++ ".." where
    chunks = fiveChunks state
    gen chunks = trans ! chunks

operate trans state n mPad pPad = finalState where
    initState = replicate mPad '.' ++ state ++ replicate pPad '.'
    finalState = iterate (nextState trans) initState !! n

solve1 :: Map String Char -> String -> Int
solve1 trans state = sum potValues where
    gens = 20
    pad = (2 * gens) + 10
    finalState = operate trans state gens pad pad
    potValues = zipWith (\x n -> if x == '#' then n else 0) finalState [(-pad)..]
    
-- The input state goes like "glider" in "Game of Life".
-- It glides with equilibrium state after some time,
-- and the answer forms a arithmetic sequence.
solve2 :: Map String Char -> String -> Integer
solve2 trans state = eq1 + (d * (finalGen - (toInteger equilGen))) where
    finalGen = 50000000000 :: Integer
    equilGen = 500 :: Int
    pPad = 2000
    mPad = 10
    potSum s = toInteger $ sum $ zipWith (\x n -> if x == '#' then n else 0) s [(-mPad)..]
    eq1 = potSum $ operate trans state equilGen mPad pPad
    eq2 = potSum $ operate trans state (equilGen + 1) mPad pPad
    d = eq2 - eq1
        
getState :: String -> String
getState line = (words line) !! 2

getTransition :: String -> (String, Char)
getTransition line = ((words line) !! 0, head $ (words line) !! 2)

main' = do
    state <- getState <$> getLine
    void getLine
    trans <- Map.fromList <$> map getTransition <$> replicateM 32 getLine
    putStrLn $ show $ solve1 trans state
    putStrLn $ show $ solve2 trans state
