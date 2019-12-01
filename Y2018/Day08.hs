import Control.Monad
import Data.Function
import Data.List
import Data.Tree

type XTree = Tree [Int]

buildTree :: [Int] -> XTree
buildTree nums = fst $ buildTree' nums where
    buildTree' (nc:nm:xs) = (tree, rems') where
        childTree :: ([XTree], [Int]) -> ([XTree], [Int])
        childTree (cs, xs) = let (c, rem) = buildTree' xs in (cs++[c], rem)
        (childs, rems) = iterate childTree ([], xs) !! nc
        (metadata, rems') = splitAt nm rems
        tree = Node {rootLabel = metadata, subForest = childs}
    buildTree' _ = error "Not enough numbers"

postorderUpdate :: (Tree a -> Forest b -> Tree b) -> Tree a -> Tree b
postorderUpdate f tree = f tree children' where
    children = subForest tree
    children' = map (postorderUpdate f) children

-- i: 1-index
retrieveOrZero :: [Int] -> Int -> Int
retrieveOrZero list i
    | 0 < i && i <= (length list) = list !! (i-1)
    | otherwise = 0

solve1 :: XTree -> Int
solve1 tree = sum $ concat $ flatten tree

solve2 :: XTree -> Int
solve2 tree = rootLabel (postorderUpdate updateFn tree) where
    updateFn tree []        = Node {rootLabel = sum (rootLabel tree), subForest = []}
    updateFn tree uChildren = Node {rootLabel = sum pickedValues, subForest = uChildren} where
        values = map rootLabel uChildren
        pickedValues = map (retrieveOrZero values) (rootLabel tree)

main' = do
    nums <- map read <$> words <$> getLine
    tree <- return $ buildTree nums
    putStrLn $ show $ solve1 tree
    putStrLn $ show $ solve2 tree
