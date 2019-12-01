import Control.Monad
import Data.Function
import Data.List
import Data.Sequence (Seq, index, (><))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable


data Recipes = Recipes (Seq Int) Int Int

loop :: Recipes -> Recipes
loop (Recipes xs i1 i2) = Recipes xs' i1' i2' where
    x1 = xs `index` i1
    x2 = xs `index` i2
    xs' = xs >< (Seq.fromList $ sliceDecimal (x1 + x2))
    i1' = nIndex xs' (i1 + 1 + x1)
    i2' = nIndex xs' (i2 + 1 + x2)

initRecipes = Recipes (Seq.fromList [3, 7]) 0 1
recipes cond = seq $ until (cond . seq) loop initRecipes where
    seq (Recipes r _ _) = r


sliceDecimal n
    | n < 10    = [n]
    | otherwise = sliceDecimal (n `div` 10) ++ [n `mod` 10]

nIndex :: Seq a -> Int -> Int
nIndex xs n = n `mod` (Seq.length xs)


sumS :: Show a => [a] -> String
sumS l = foldl (++) "" $ map show l

solve1 :: Int -> String
solve1 n = (sumS . take 10 . drop n . Foldable.toList) (recipes (\r -> length r > n+10))

solve2 :: Int -> Int
solve2 n = (length l) - 6 - 1 where
    takeLast6 l = Seq.drop ((Seq.length l) - 6) l
    takeLast6' l = Seq.take 6 $ Seq.drop ((Seq.length l) - 7) l
    targetSeq = Seq.fromList $ sliceDecimal n
    l = recipes (\r -> (takeLast6 r) == targetSeq || (takeLast6' r) == targetSeq)
    
main' = do
    n <- read <$> getLine
    putStrLn $ solve1 n
    putStrLn $ show $ solve2 n
