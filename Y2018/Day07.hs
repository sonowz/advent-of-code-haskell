import Control.Monad
import Data.Function
import Data.List
import Data.Char
import Data.Graph
import Data.Array
import qualified Data.Foldable as Foldable


vertexRange = (ord 'A', ord 'Z')

getEdge :: String -> Edge
getEdge line = (ord $ w !! 1, ord $ w !! 7) where
    w = map head $ words line
    ord' = ord . head
    
getZeroDegVertices :: Array Vertex Int -> [Vertex]
getZeroDegVertices degs = map fst (filter (\(i, e) -> e == 0) $ assocs degs)

getTopologicalProps graph = (degs, roots) where
    degs = indegree graph
    roots = getZeroDegVertices degs

removeVertex graph degs roots v = (degs', roots'') where
    us = graph ! v
    m1Deg x = (x, (degs ! x) - 1)
    -- Subtract degree by 1, also v so that deg(v) < 0
    degs' = degs // (map m1Deg us) // [(m1Deg v)]
    roots' = delete v roots
    roots'' = roots' `union` getZeroDegVertices degs'

-- Topological sort followed by lexicological (actually natural) order
topLexSort :: Graph -> [Vertex]
topLexSort graph = sort' degs roots where
    (degs, roots) = getTopologicalProps graph
    removeMinVertex degs roots = (minV, degs', roots') where
        minV = minimum roots
        (degs', roots') = removeVertex graph degs roots minV
    sort' degs []    = []
    sort' degs roots = v : (sort' degs' roots') where
        (v, degs', roots') = removeMinVertex degs roots
            
solve1 :: Graph -> String
solve1 graph = map chr $ topLexSort graph

-- 'A' => 61
weight v = v - 4

updateTime graph works v = works' where
    us = graph ! v
    newWork u = max (works ! u) ((works ! v) + (weight v))
    works' = foldl (\w u -> w // [(u, newWork u)]) works us

-- Traversal by topology, lexical order and regarding time
topLexTraverse :: Graph -> Int
topLexTraverse graph = (snd lastWork) + (weight $ fst lastWork) where
    (degs, roots) = getTopologicalProps graph
    works = listArray vertexRange (repeat 0)
    removeVertices degs roots vs = (degs', roots') where
        (degs', roots') = foldl (\(d, r) v -> removeVertex graph d r v) (degs, roots) vs
    traverse degs []    works t = works
    traverse degs roots works t = traverse degs' roots' works' (t+1) where
        vs = roots `intersect` (map fst $ filter (\x -> (snd x) <= t) $ assocs works)
        (degs', roots') = removeVertices degs roots vs
        works' = foldl (updateTime graph) works vs
    totalWorks = traverse degs roots works 0
    lastWork = maximumBy (compare `on` snd) (assocs totalWorks)

-- This solution assumes that there are enough workers (same answer with infinite workers)
solve2 :: Graph -> Int
solve2 graph = topLexTraverse graph

main' = do
    edges <- map getEdge <$> replicateM 101 getLine
    dag <- return $ buildG vertexRange edges
    putStrLn $ solve1 dag
    putStrLn $ show $ solve2 dag
    