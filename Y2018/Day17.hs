import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec

($$) :: a -> (a -> b) -> b
($$) = flip ($)

updateB :: Vector (Vector a) -> (Int, Int) -> a -> Vector (Vector a)
updateB b (x, y) v = Vec.accum ($$) b [(y, \l -> Vec.accum ($$) l [(x, const v)])]

mapMaybeB :: (a -> Maybe b) -> Vector (Vector a) -> [b]
mapMaybeB f b = (catMaybes . Vec.toList . Vec.concatMap (Vec.map f)) b

type Pos = (Int, Int)
data Direction = DLeft | DRight
data Object = Sand | Clay | Water | Reach
type Board = Vector (Vector Object)

instance Eq Object where
    Sand == Sand = True
    Clay == Clay = True
    Water == Water = True
    Reach == Reach = True
    _ == _ = False

instance Show Object where
    show Sand = "."
    show Clay = "#"
    show Water = "~"
    show Reach = "|"

showB board = concatMap id $ intersperse "\n" $ map (\l -> concatMap show l) (Vec.toList (Vec.map (Vec.toList . Vec.slice 450 100) board))

at :: Board -> Pos -> Object
at board (x, y) = board ! y ! x

isBlocked Water = True
isBlocked Clay = True
isBlocked _ = False

fall :: Board -> Pos -> (Board, Maybe Pos)
fall board (x, y)
    | y < 0 || y >= Vec.length board = (board, Nothing)
    | isBlocked $ board `at` (x, y)  = (board, Just (x, y-1))
    | otherwise                      = fall (updateB board (x, y) Reach) (x, y+1)

overflow :: Board -> Pos -> Direction -> (Pos, Bool)
overflow board pos DLeft = overflow' board pos (\x -> x-1)
overflow board pos DRight = overflow' board pos (\x -> x+1)
overflow' board (x, y) fmx
    | isBlocked $ board `at` (fmx x, y)   = ((x, y), True)
    | isBlocked $ board `at` (fmx x, y+1) = overflow' board (fmx x, y) fmx
    | otherwise                           = ((fmx x, y), False)

-- Either (filled status) (overflow status)
tryFill :: Board -> Pos -> (Board, Either Pos [Pos])
tryFill board pos = if lBlock && rBlock then (wBoard, Left (x, y-1)) else (ofBoard, Right overflows) where
    (lEnd, lBlock) = overflow board pos DLeft
    (rEnd, rBlock) = overflow board pos DRight
    overflows = (if lBlock then [] else [lEnd]) ++ (if rBlock then [] else [rEnd])
    (x, y) = pos
    (lx, _) = lEnd
    (rx, _) = rEnd
    fillY obj = (board ! y) // [(x', obj) | x' <- [lx..rx]]
    wBoard = board // [(y, fillY Water)]
    ofBoard = board // [(y, fillY Reach)]

loop :: Board -> [Pos] -> Board
loop board [] = board
loop board ps = loop board' (nub nps) where
    foldFn (b, ps') p = let (b', ps'') = loop' b p in (b', ps' ++ ps'')
    (board', nps) = foldl foldFn (board, []) ps

-- fall & tryFill -> New fall positions
loop' :: Board -> Pos -> (Board, [Pos])
loop' board pos = case fall board pos of
    (board', Nothing) -> (board', [])
    (board', Just pos') -> fillLoop board' pos' where
        fillLoop b p = case tryFill b p of
            (b', Left p') -> fillLoop b' p'
            (b', Right ps) -> (b', ps)

-- Returns sliced board which are counted lines
doSimulation :: Board -> Board
doSimulation board = sliced where
    clays = Vec.imapMaybe (\y ys -> if Vec.any ((==) Clay) ys then Just y else Nothing) board
    (minY, maxY) = (minimum clays, maximum clays)
    board' = loop board [(500, 0)]
    sliced = Vec.slice minY (maxY - minY + 1) board'

solve1 :: Board -> Int
solve1 board = waterVolume where
    board' = doSimulation board
    waterVolume = length $ mapMaybeB (\x -> if x == Water || x == Reach then Just 0 else Nothing) board'

solve2 :: Board -> Int
solve2 board = waterVolume where
    board' = doSimulation board
    waterVolume = length $ mapMaybeB (\x -> if x == Water then Just 0 else Nothing) board'

initBoard = Vec.replicate 2000 (Vec.replicate 1000 Sand)

-- "x=495, y=2..7" => "x 495 y 2 7"
spaced :: String -> String
spaced line = map (\x -> if x == '=' || x == '.' then ' ' else x) del where
    del = (delete ',' . delete '.') line

getClay :: Board -> IO Board
getClay board = do
    [axis, a, _, b1, b2] <- words <$> spaced <$> getLine
    return $ updateClay axis (read a) (read b1) (read b2) where
        updateClay "x" x y1 y2 = foldl (\b y -> updateB b (x, y) Clay) board [y1..y2]
        updateClay "y" y x1 x2 = foldl (\b x -> updateB b (x, y) Clay) board [x1..x2]

iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM 0 f x = return []
iterateM n f x = do
    y <- f x
    ys <- iterateM (n-1) f y
    return (y:ys)

main' = do
    board <- last <$> iterateM 1685 getClay initBoard
    putStrLn $ show $ solve1 board
    putStrLn $ show $ solve2 board
