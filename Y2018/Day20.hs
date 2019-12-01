import Control.Monad
import Data.Function
import Data.Either
import Data.Maybe
import Data.List
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec hiding (try)
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Error
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.SP
import Data.Graph.Inductive.Internal.RootPath (getDistance)
import Debug.Trace

data Regex = Plain Char | Branch [Regex] | Concat [Regex]

instance Show Regex where
    show (Plain x) = [x]
    show (Branch rs) = "(" ++ (concatMap id $ intersperse "|" $ map show rs) ++ ")"
    show (Concat rs) = "(" ++ (concatMap id $ map show rs) ++ ")"

sepBy2 a b = do
    x <- a
    void b
    xs <- sepBy1 a b
    return (x : xs)

regexParser :: Parser Regex
regexParser = choice [try branches, plainAndParentheses] where
    plain = letter >>= return . Plain
    parentheses = between (char '(') (char ')') regexParser
    branches = Branch <$> sepBy2 plainAndParentheses (char '|')
    plainAndParentheses = Concat <$> (many $ plain <|> parentheses)
    
getRegex :: String -> Regex
getRegex l = fromRight (error "ERROR") $ parse regexParser "" l

type Pos = (Int, Int)
type Door = (Pos, Pos)
type RGraph = Gr Int Int

applyDir :: Char -> Pos -> Pos
applyDir 'N' (x, y) = (x, y-1)
applyDir 'E' (x, y) = (x+1, y)
applyDir 'S' (x, y) = (x, y+1)
applyDir 'W' (x, y) = (x-1, y)

walkRegex :: Pos -> Regex -> ([Pos], [Door])
walkRegex pos (Plain dir) = let pos' = applyDir dir pos in ([pos'], [(pos, pos')])
walkRegex pos (Branch []) = ([pos], [])
walkRegex pos (Branch (x:[])) = walkRegex pos x
walkRegex pos (Branch (x:xs)) = (ps ++ ps', paths ++ paths') where
    (ps, paths) = walkRegex pos x
    (ps', paths') = walkRegex pos (Branch xs)
walkRegex pos (Concat []) = ([pos], [])
walkRegex pos (Concat (x:[])) = walkRegex pos x
walkRegex pos (Concat (x:xs)) = (ps', paths ++ paths') where
    (ps, paths) = walkRegex pos x
    concatTupleMap f l = (concat pss, concat pathss) where
        (pss, pathss) = unzip $ map f l
    (ps', paths') = concatTupleMap (\p -> walkRegex p (Concat xs)) (nub ps)

posToNode :: Pos -> Node
posToNode (x, y) = (y * 10000) + x

doorToDEdge :: Door -> [LEdge Int]
doorToDEdge (p1, p2) = [(posToNode p1, posToNode p2, 1), (posToNode p2, posToNode p1, 1)]

edgeNodes :: LEdge Int -> [LNode Int]
edgeNodes (v1, v2, _) = [(v1, v1), (v2, v2)]

buildGraph :: Regex -> RGraph
buildGraph regex = mkGraph nodes edges where
    (_, doors) = walkRegex (0, 0) regex
    edges = concatMap doorToDEdge doors
    nodes = nub $ concatMap edgeNodes edges

solve1 :: RGraph -> Int
solve1 graph = maximum distances where
    distTree = spTree (posToNode (0, 0)) graph
    distances = map (\v -> fromMaybe 0 $ getDistance v distTree) (nodes graph)

solve2 :: RGraph -> Int
solve2 graph = length $ filter (\x -> x >= 1000) distances where
    distTree = spTree (posToNode (0, 0)) graph
    distances = map (\v -> fromMaybe 0 $ getDistance v distTree) (nodes graph)

trimLine l = take (length l - 2) $ drop 1 l

main' = do
    regex <- getRegex <$> trimLine <$> getLine
    graph <- return $ buildGraph regex
    putStrLn $ show $ solve1 graph
    putStrLn $ show $ solve2 graph