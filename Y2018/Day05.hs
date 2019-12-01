import Control.Monad
import Data.Function
import Data.List
import Data.Char


xor b1 b2 = (b1 && (not b2)) || ((not b1) && b2)

react :: String -> String
react p = if p == (react' p) then p else react (react' p) where
    reactable a1 a2 = ((isLower a1) `xor` (isLower a2)) && ((toLower a1) == (toLower a2))
    react' (a1:a2:p)
        | reactable a1 a2 = react' p
        | otherwise       = a1 : (react' (a2:p))
    react' p = p
        
solve1 :: String -> Int
solve1 polymer = length (react polymer)

solve2 :: String -> Int
solve2 polymer = length (minimumBy (compare `on` length) reacted) where
    units = zip ['a'..'z'] ['A'..'Z']
    remove (x:xs) (c1, c2)
        | c1 == x || c2 == x = remove xs (c1, c2)
        | otherwise          = x : (remove xs (c1, c2))
    remove [] _ = ""
    polymers' = (remove polymer) `map` units
    reacted = react `map` polymers'

main' = do
    polymer <- getLine
    putStrLn $ show $ solve1 polymer
    putStrLn $ show $ solve2 polymer
