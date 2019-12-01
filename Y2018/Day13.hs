import Prelude hiding (Left, Right)
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import qualified Control.Exception as E
import Debug.Trace

data Track = XTrack | YTrack | STrack | BTrack | Cross | Empty

data Cart = Cart Orientation AtCross
data Orientation = Left | Right | Up | Down
data AtCross = CLeft | CStraight | CRight

type Mine = (Track, Maybe Cart)
type MineBoard = Vector (Vector Mine)

instance Show Track where
    show XTrack = "-"
    show YTrack = "|"
    show STrack = "/"
    show BTrack = "\\"
    show Cross = "+"
    show Empty = " "

instance Show Cart where
    show (Cart Left _) = "<"
    show (Cart Right _) = ">"
    show (Cart Up _) = "^"
    show (Cart Down _) = "v"

showM (_, Just c) = show c
showM (t, Nothing) = show t
ss b = unwords $ intersperse "\n" $ map (\l -> concatMap showM l) (Vec.toList (Vec.map Vec.toList b))

getMine :: Char -> Mine
getMine ' ' = (Empty, Nothing)
getMine '-' = (XTrack, Nothing)
getMine '|' = (YTrack, Nothing)
getMine '/' = (STrack, Nothing)
getMine '\\' = (BTrack, Nothing)
getMine '+' = (Cross, Nothing)
getMine '<' = (XTrack, Just (Cart Left CLeft))
getMine '>' = (XTrack, Just (Cart Right CLeft))
getMine '^' = (YTrack, Just (Cart Up CLeft))
getMine 'v' = (YTrack, Just (Cart Down CLeft))
getMine _ = error "Invalid character"

orientF :: Orientation -> ((Int -> Int), (Int -> Int))
orientF Left  = ((\x -> x-1), id)
orientF Right = ((\x -> x+1), id)
orientF Up    = (id, (\y -> y-1))
orientF Down  = (id, (\y -> y+1))

process :: Track -> Orientation -> AtCross -> Cart
process XTrack o a = Cart o a
process YTrack o a = Cart o a
process STrack Left  a = Cart Down  a
process STrack Right a = Cart Up    a
process STrack Up    a = Cart Right a
process STrack Down  a = Cart Left  a
process BTrack Left  a = Cart Up    a
process BTrack Right a = Cart Down  a
process BTrack Up    a = Cart Left  a
process BTrack Down  a = Cart Right a
process Cross o  CStraight = Cart o     CRight
process Cross Left  CLeft  = Cart Down  CStraight
process Cross Left  CRight = Cart Up    CLeft
process Cross Right CLeft  = Cart Up    CStraight
process Cross Right CRight = Cart Down  CLeft
process Cross Up    CLeft  = Cart Left  CStraight
process Cross Up    CRight = Cart Right CLeft
process Cross Down  CLeft  = Cart Right CStraight
process Cross Down  CRight = Cart Left  CLeft
process Empty o a = error ("b" ++ show (Cart o a))

($$) :: a -> (a -> b) -> b
($$) = flip ($)

move1 :: MineBoard -> Int -> Int -> Mine -> MineBoard
move1 board x y (_, Nothing) = board
move1 board x y (_, Just (Cart orient atCross)) = board' where
    (fnx, fny) = orientF orient
    nextMine = board ! (fny y) ! (fnx x)
    nextCart = case nextMine of
        (_, Just _) -> error ((show (x-1)) ++ "," ++ (show y)) -- Crash!!
        (t, Nothing) -> process t orient atCross
    nextMine' = (fst nextMine, nextCart)
    removeCart = [(y, \ys -> Vec.accum ($$) ys [(x, \(t, _) -> (t, Nothing))])]
    addCart = [(fny y, \ys -> Vec.accum ($$) ys [(fnx x, \(t, _) -> (t, Just nextCart))])]
    board' = Vec.accum ($$) board (removeCart ++ addCart)

tick1 :: MineBoard -> MineBoard
tick1 board = Vec.ifoldl' foldFn board board where
    foldFn = \b y ml -> Vec.ifoldl' (\b x m -> move1 b x y m) b ml

solve1 :: MineBoard -> String
solve1 board = dummyFn where
    loop = iterate' tick1 board
    dummyFn = case loop !! (1000000) ! 10000 ! 10000 of
        (_, Nothing) -> "dummy"
        (_, _) -> "string"


btoi True = 1
btoi False = 0

oneCart :: MineBoard -> Maybe (Int, Int)
oneCart board = if hasOneCart board then Just (findCart board) else Nothing where
    isCart (_, Nothing) = False
    isCart (_, Just _) = True
    hasOneCart board = (sum $ Vec.map (\ml -> sum $ Vec.map (btoi . isCart) ml) board) == 1
    findCart board = (Vec.imapMaybe (\y ml -> Vec.findIndex isCart ml >>= (\x -> Just (x, y))) board) ! 0

move2 :: MineBoard -> Int -> Int -> Mine -> MineBoard
move2 board x y (_, Nothing) = board
move2 board x y (_, Just (Cart orient atCross)) = if isNothing (snd $ board ! y ! x) then board else board' where -- Check crashed state
    (fnx, fny) = orientF orient
    nextMine = board ! (fny y) ! (fnx x)
    board' = case nextMine of
        (_, Just _) -> maybe boardN (\(x, y) -> error $ (show x) ++ "," ++ (show y)) (oneCart boardN) where -- Crash!!
            boardN = Vec.accum ($$) board (removeCart ++ (addCart Nothing))
        (t, Nothing) -> Vec.accum ($$) board (removeCart ++ (addCart $ Just $ process t orient atCross))
    removeCart = [(y, \ys -> Vec.accum ($$) ys [(x, \(t, _) -> (t, Nothing))])]
    addCart cart = [(fny y, \ys -> Vec.accum ($$) ys [(fnx x, \(t, _) -> (t, cart))])]

tick2 :: MineBoard -> MineBoard
tick2 board = Vec.ifoldl' foldFn board board where
    foldFn = \b y ml -> Vec.ifoldl' (\b x m -> move2 b x y m) b ml

solve2 :: MineBoard -> String
solve2 board = dummyFn where
    loop = iterate' tick2 board
    dummyFn = case loop !! (1000000) ! 10000 ! 10000 of
        (_, Nothing) -> "dummy"
        (_, _) -> "string"
 

linesToMine :: [String] -> MineBoard
linesToMine lines = Vec.fromList (map lineToMine lines) where
    lineToMine line = Vec.fromList (map getMine line)

main' = do
    mine <- linesToMine <$> replicateM 150 getLine
    E.catch (putStrLn $ solve1 mine) (\err -> putStrLn (show (err :: E.SomeException)))
    E.catch (putStrLn $ solve2 mine) (\err -> putStrLn (show (err :: E.SomeException)))
