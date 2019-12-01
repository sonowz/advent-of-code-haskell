import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as Vec

($$) :: a -> (a -> b) -> b
($$) = flip ($)

imapB :: ((Int, Int) -> a -> b) -> Vector (Vector a) -> Vector (Vector b)
imapB f b = Vec.imap (\y xs -> Vec.imap (\x o -> f (x, y) o) xs) b

mapMaybeB :: (a -> Maybe b) -> Vector (Vector a) -> [b]
mapMaybeB f b = (catMaybes . Vec.toList . Vec.concatMap (Vec.map f)) b


type Pos = (Int, Int)
data Object = Open | Tree | Lumberyard
type Board = Vector (Vector Object)

instance Eq Object where
    Open == Open = True
    Tree == Tree = True
    Lumberyard == Lumberyard = True
    _ == _ = False

instance Show Object where
    show Open = "."
    show Tree = "|"
    show Lumberyard = "#"

showB board = concatMap id $ intersperse "\n" $ map (\l -> concatMap show l) (Vec.toList (Vec.map Vec.toList board))

getObj :: Char -> Object
getObj '.' = Open
getObj '|' = Tree
getObj '#' = Lumberyard
getObj _ = error "Invalid character"

at :: Board -> Pos -> Object
at board (x, y) = board ! y ! x

safeAt :: Board -> Pos -> Maybe Object
safeAt board (x, y) = board !? y >>= \ys -> ys !? x

adjacent :: Pos -> [Pos]
adjacent (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

adjacentObj :: Board -> Pos -> [Object]
adjacentObj board pos = catMaybes $ map (safeAt board) (adjacent pos)

gte :: Object -> Int -> ([Object] -> Bool)
gte x n = \objs -> length (filter ((==) x) objs) >= n

nextState :: Board -> Pos -> Object
nextState board pos = let adjs = adjacentObj board pos in
    case board `at` pos of
        Open -> if (Tree `gte` 3) adjs then Tree else Open
        Tree -> if (Lumberyard `gte` 3) adjs then Lumberyard else Tree
        Lumberyard -> if (Lumberyard `gte` 1) adjs && (Tree `gte` 1) adjs then Lumberyard else Open

afterMin :: Board -> Board
afterMin board = imapB (\pos _ -> nextState board pos) board

resourceValue board = nTree * nLumberyard where
    nTree = length $ mapMaybeB (\x -> if x == Tree then Just x else Nothing) board
    nLumberyard = length $ mapMaybeB (\x -> if x == Lumberyard then Just x else Nothing) board

resourceValue' board = (nTree, nLumberyard) where
    nTree = length $ mapMaybeB (\x -> if x == Tree then Just x else Nothing) board
    nLumberyard = length $ mapMaybeB (\x -> if x == Lumberyard then Just x else Nothing) board

solve1 :: Board -> Int
solve1 board = resourceValue board' where
    board' = iterate afterMin board !! 10

-- Equilibrium time
t = 3000

-- Board states will have a stable frequency after long time
findFreq board = freq where
    equilState = iterate afterMin board !! (t-1)
    cands = (take 100 . drop t) $ iterate afterMin board
    cands' = zip cands [t..]
    eqStates = filter (\(x,_) -> resourceValue' x == resourceValue' equilState) cands'
    freq = snd (head eqStates) - (t-1)

solve2 :: Board -> Int
solve2 board = resourceValue board' where
    freq = findFreq board
    offset = freq * (t `div` freq)
    targetState = offset + (1000000000 `mod` freq)
    board' = iterate afterMin board !! targetState


linesToBoard :: [String] -> Board
linesToBoard lines = Vec.fromList (map lineToBoard lines) where
    lineToBoard line = Vec.fromList (map getObj line)

main' = do
    board <- linesToBoard <$> replicateM 50 getLine
    putStrLn $ show $ solve1 board
    putStrLn $ show $ solve2 board
