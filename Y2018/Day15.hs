import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import qualified Data.Foldable as Foldable
import Debug.Trace

($$) :: a -> (a -> b) -> b
($$) = flip ($)

updateB :: Vector (Vector a) -> (Int, Int) -> a -> Vector (Vector a)
updateB b (x, y) v = Vec.accum ($$) b [(y, \l -> Vec.accum ($$) l [(x, const v)])]

mapMaybeB :: (a -> Maybe b) -> Vector (Vector a) -> [b]
mapMaybeB f b = (catMaybes . Vec.toList . Vec.concatMap (Vec.map f)) b

imapMaybeB :: ((Int, Int) -> a -> Maybe b) -> Vector (Vector a) -> [b]
imapMaybeB f b = (catMaybes . Vec.toList . iconcatMap (\y l -> Vec.imap (\x v -> f (x, y) v) l))  b where
    iconcatMap f = Vec.concat . Vec.toList . Vec.imap f


data Race = Elf | Goblin
data Unit = Unit{ race :: Race
                , hp :: Int
                , atk :: Int
                }

type Pos = (Int, Int)
data Object = Wall | Open | Obj Unit
type BattleField = Vector (Vector Object)

type OrderedList a = [a]

instance Eq Race where
    Elf    == Elf    = True
    Goblin == Goblin = True
    _      == _      = False

instance Show Unit where
    show u
        | race u == Elf = "E"
        | race u == Goblin = "G"

instance Show Object where
    show Wall = "#"
    show Open = "."
    show (Obj u) = show u

showB board = concatMap id $ intersperse "\n" $ map (\l -> concatMap show l) (Vec.toList (Vec.map Vec.toList board))

elf = Unit { race = Elf, hp = 200, atk = 3 }
goblin = Unit { race = Goblin, hp = 200, atk = 3 }

getObj :: Char -> Object
getObj '#' = Wall
getObj '.' = Open
getObj 'E' = Obj elf
getObj 'G' = Obj goblin
getObj _ = error "Invalid character"


at :: BattleField -> Pos -> Object
at board (x, y) = board ! y ! x

atUnit :: BattleField -> Pos -> Unit
atUnit board pos = case at board pos of
    Obj u -> u
    _ -> error "Not a unit"

isUnit (Obj u) = True
isUnit _ = False

isOpen Open = True
isOpen _ = False

readOrderSort :: [Pos] -> OrderedList Pos
readOrderSort = sortBy (\(x1, y1) (x2, y2)-> if y1 == y2 then compare x1 x2 else compare y1 y2)

adjacent :: Pos -> OrderedList Pos
adjacent (x, y) = [(x, y-1), (x-1, y), (x+1, y), (x, y+1)]

adjacentEnemies :: BattleField -> Pos -> OrderedList Pos
adjacentEnemies board pos = filter filterFn (adjacent pos) where
    ur = race $ board `atUnit` pos
    filterFn = \p -> (isUnit $ board `at` p) && ((race $ board `atUnit` p) /= ur)

enemyRanges :: BattleField -> Race -> OrderedList Pos
enemyRanges board tr = readOrderSort ranges' where
    units = turnOrders board
    enemies = filter (\p -> (race $ board `atUnit` p) /= tr) units
    ranges = nub $ concatMap adjacent enemies
    ranges' = filter (\p -> isOpen $ board `at` p) ranges

type DistanceBoard = Vector (Vector Int)

atDist :: DistanceBoard -> Pos -> Int
atDist dists (x, y) = dists ! y ! x

initDistances = Vec.replicate 500 (Vec.replicate 500 (-1))

expand :: BattleField -> DistanceBoard -> Pos -> (DistanceBoard, [Pos])
expand board dists pos = (dists'', adjacents') where
    adjacents = adjacent pos
    filterFn p = dists `atDist` p < 0
    partFn p = isOpen $ board `at` p
    (adjacents', blocked) = partition partFn $ filter filterFn adjacents
    d = (dists `atDist` pos) + 1
    dists' = foldl (\b p -> updateB b p d) dists adjacents'
    dists'' = foldl (\b p -> updateB b p 99999) dists' blocked

-- Maybe (Pos, distance from Pos)
chooseNearest :: BattleField -> Pos -> OrderedList Pos -> Maybe (Pos, Int)
chooseNearest board pos targets = loop (distances, [pos]) where
    distances = updateB initDistances pos 0
    loop (dists, []) = Nothing
    loop (dists, ps) = if nearests == [] then loop (dists', ps') else Just (nearest, dists' `atDist` nearest) where
        nearests = ps `intersect` targets
        nearest = readOrderSort nearests !! 0
        (dists', ps') = foldl (\(b, ps') p -> let (b', ps'') = expand board b p in (b', ps'' ++ ps')) (dists, []) ps
        
turnOrders :: BattleField -> [Pos]
turnOrders board = imapMaybeB getUnit board where
    getUnit pos (Obj u) = Just pos
    getUnit pos _ = Nothing

untilNoMove :: BattleField -> (Int, BattleField)
untilNoMove board = untilNoMove' 0 board where
    untilNoMove' t b = let (b', z) = doRound b in
        if z then (t, b') else untilNoMove' (t+1) b'

doRound :: BattleField -> (BattleField, Bool)
doRound board = (board', noMoves) where
    --trace (showB board' ++ "\n") $ (board', noMoves) where
    units = turnOrders board
    foldFn (b, acc) unit = case turn b unit of
        Just b' -> (b', False)
        Nothing -> (b, acc)
    (board', noMoves) = foldl foldFn (board, True) units

-- Nothing == no actions
turn :: BattleField -> Pos -> Maybe BattleField
turn board pos = if isUnit (board `at` pos) then board'' else Nothing where
    phase1 = tryMoveEnemies board pos
    phase2 = \(board', pos', enemies) -> ignoreNothing board' (tryAttack board' pos' enemies)
    ignoreNothing def m = maybe (Just def) Just m
    board'' = phase1 >>= phase2

tryMoveEnemies board pos = if exactNearEnemies /= [] then Just (board, pos, exactNearEnemies) else moveAndAdj where
    exactNearEnemies = adjacentEnemies board pos
    afterMove = \(board', pos') -> (board', pos', adjacentEnemies board' pos')
    moveAndAdj = tryMove board pos >>= return . afterMove

tryMove board pos = nearest >> Just (board'', movePos) where
    unit = board `atUnit` pos
    ranges = enemyRanges board (race unit)
    nearest = maybe Nothing (Just . fst) (chooseNearest board pos ranges)
    movables = filter (\p -> isOpen $ board `at` p) (adjacent pos)
    getDistance Nothing = maxBound :: Int
    getDistance (Just (p, d)) = d
    movePos = minimumBy (compare `on` (\p -> getDistance $ chooseNearest board p [fromJust nearest])) movables
    board' = updateB board pos Open
    board'' = updateB board' movePos (Obj unit)

tryAttack board pos [] = Nothing
tryAttack board pos enemies = Just $ attack board pos target where
    target = minimumBy (compare `on` (hp . atUnit board)) enemies
    
attack board pos target = updateB board target newObject where
    attackPower = atk $ board `atUnit` pos
    targetUnit = board `atUnit` target
    newHP = (hp targetUnit) - attackPower
    newObject = if newHP <= 0 then Open else Obj (targetUnit {hp = newHP})

solve1 :: BattleField -> Int
solve1 board = trace ((show (turns - 1)) ++ " * " ++ (show hpSum)) (turns - 1) * hpSum where
    (turns, board') = untilNoMove board
    hpSum = sum $ map (hp . atUnit board') (turnOrders board')


getElfs :: BattleField -> [Pos]
getElfs board = imapMaybeB getElf board where
    getElf pos (Obj u) | race u == Elf = Just pos
    getElf pos _ = Nothing

empower :: Int -> BattleField -> BattleField
empower power board = board' where
    elfs = getElfs board
    updateFn b (x, y) = Vec.accum ($$) b [(y, \l -> Vec.accum ($$) l [(x, \(Obj u) -> Obj (u {atk = power}))])]
    board' = foldl updateFn board elfs

solve2 :: BattleField -> Int
solve2 board = trace ((show (turns - 1)) ++ " * " ++ (show hpSum)) (turns - 1) * hpSum where
    initElfN = length $ getElfs board
    empowerStep n = if elfN == initElfN then (turns, board') else empowerStep (n+1) where
        (turns, board') = untilNoMove $ empower n board
        elfN = length $ getElfs board'
    (turns, board') = empowerStep 4
    hpSum = sum $ map (hp . atUnit board') (turnOrders board')


linesToBoard :: [String] -> BattleField
linesToBoard  lines = Vec.fromList (map lineToBoard  lines) where
    lineToBoard  line = Vec.fromList (map getObj line)


main' = do
    board <- linesToBoard <$> replicateM 32 getLine
    putStrLn "Answer 1 is turn * (hpSum - 6). I think it's a bug with author's code.."
    putStrLn $ show $ solve1 board
    putStrLn $ show $ solve2 board
