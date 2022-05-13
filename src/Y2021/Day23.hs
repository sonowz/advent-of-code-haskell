module Y2021.Day23 where

import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import qualified Algebra.Graph.Labelled.AdjacencyMap as A
import qualified Algebra.Graph.ToGraph as G
import Lib.Graph (dijkstra, shortestPath)
import Algebra.Graph.Label (Distance, getDistance, getFinite)
import qualified Relude.Unsafe as Unsafe
import Data.List (intersect)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import GHC.IO (unsafePerformIO)
import qualified Text.Show as S
import qualified Data.Tree


-- TODO: refactor code
-- WARNING: this code is somewhat messy, as it went through lots of revisions

-----------------------
-- Type declarations --
-----------------------

-- Burrow structure
--  (Hallways)  1 2 --- 3 --- 4 --- 5 --- 6 7
--                   |     |     |     |
--  (Rooms)          A1    B1    C1    D1
--                   A2    B2    C2    D2
type Burrow = Map Node (Maybe Amphipod)
data Node = Hallway Int | Room Amphipod Int deriving (Show)
data Amphipod = Amber | Bronze | Copper | Desert deriving (Eq, Ord)
newtype Energy = Energy Int deriving (Show, Num, Ord, Eq) via Int

type DistanceMap = Map Node (Map Node Int)
type BurrowGraph = A.AdjacencyMap (Distance Int) Node

instance Eq Node where (==) = (==) `on` getNodeId
instance Ord Node where compare = compare `on` getNodeId
instance Show Amphipod where
  show Amber = "A"
  show Bronze = "B"
  show Copper = "C"
  show Desert = "D"

getNodeId :: Node -> Int
getNodeId (Hallway n) = n
getNodeId (Room a n) =  10000 + un (getEnergy a 1) + n

getEnergy :: Amphipod -> Int -> Energy
getEnergy Amber x = Energy x
getEnergy Bronze x = 10 * Energy x
getEnergy Copper x = 100 * Energy x
getEnergy Desert x = 1000 * Energy x

getOccupant :: Burrow -> Node -> Maybe Amphipod
getOccupant burrow node = join (burrow !? node)

type MovesFn = Burrow -> Node -> [Node]

------------
-- Part 1 --
------------

solve1 :: Burrow -> Energy
solve1 = Unsafe.fromJust . simulateBurrow moves

type BurrowStr = String
stringify :: Burrow -> BurrowStr
stringify = showMaybe <=< elems where
    showMaybe Nothing = " "
    showMaybe (Just x) = show x

-- Memoization using 'unsafePerformIO'
cache :: IORef (HashMap BurrowStr (Maybe Energy))
{-# NOINLINE cache #-}
cache = unsafePerformIO $ newIORef mempty

simulateBurrow :: MovesFn -> Burrow -> Maybe Energy
simulateBurrow movesFn b = unsafePerformIO $ do
    map <- readIORef cache
    case map !? burrowStr of
        Just x -> return x
        Nothing -> let !value = _simulateBurrow movesFn b in
            writeIORef cache (insert burrowStr value map) >> return value
  where
    burrowStr = stringify b

_simulateBurrow :: MovesFn -> Burrow -> Maybe Energy
_simulateBurrow movesFn !burrow
    | allDone = Just 0
    | otherwise = leastEnergy where
    allDone :: Bool
    allDone = all checkAmphipod ([Amber, Bronze, Copper, Desert] :: [Amphipod]) where
        checkAmphipod a = isJust $ do
            getOccupant burrow (Room a 1) >>= \a' -> guard (a == a')
            getOccupant burrow (Room a 2) >>= \a' -> guard (a == a')
            if isNothing (burrow !? Room Amber 3) then Just ()
            else do
                getOccupant burrow (Room a 3) >>= \a' -> guard (a == a')
                getOccupant burrow (Room a 4) >>= \a' -> guard (a == a')
                pass
    occupiedNodes = filter (isJust . getOccupant burrow) (keys burrow) :: [Node]
    !leastEnergy = viaNonEmpty minimum1 . catMaybes $ simulateAmphipod movesFn burrow <$> occupiedNodes

simulateAmphipod :: MovesFn -> Burrow -> Node -> Maybe Energy
simulateAmphipod movesFn !burrow !node = getOccupant burrow node >>= leastEnergy where
    leastEnergy a = viaNonEmpty minimum1 $! catMaybes ((\dst -> moveAndSimulate node dst a) <$> movesFn burrow node)
    getEnergyCost :: Node -> Node -> Amphipod -> Energy
    getEnergyCost src dst a = getEnergy a distance where
        distance = Unsafe.fromJust $ distanceMap' !? src >>= (!? dst)
    moveAndSimulate :: Node -> Node -> Amphipod -> Maybe Energy
    moveAndSimulate src dst a = (+) energy <$> simulateBurrow movesFn burrow' where
        energy = getEnergyCost src dst a
        burrow' = insert src Nothing . insert dst (Just a) $ burrow

moves :: Burrow -> Node -> [Node]
moves burrow node = join . maybeToList $ do
    a <- getOccupant burrow node
    return $ filterReachables $ targets node a where

    -- This does NOT consider if intermediate nodes are blocked
    targets :: Node -> Amphipod -> [Node]
    targets (Hallway _) a = let occ = occupants a [1, 2] in
        [Room a (2 - length occ) | all (== a) occ, length occ <= 1]
    targets (Room roomTarget 1) a
        | a == roomTarget = let occ = occupants a [2] in if all (== a) occ then [] else hallways
        | otherwise = hallways
    targets (Room roomTarget 2) a
        | a == roomTarget = []
        | otherwise = hallways
    targets (Room roomTarget _) a = error "Impossible room number!"
    occupants roomKind roomNumbers = catMaybes $ getOccupant burrow . Room roomKind <$> roomNumbers
    hallways = Hallway <$> [1..7]

    filterReachables :: [Node] -> [Node]
    filterReachables = filter filterFn where
        filterFn target =
            case blockerMap' !? (node, target) of
                Nothing -> False
                Just blockers -> all (isNothing . getOccupant burrow) blockers

distanceMap' :: DistanceMap
distanceMap' = distanceMap burrowGraph2

distanceMap :: BurrowGraph -> DistanceMap
distanceMap graph = distanceToInt <<$>> distances where
    !distances =    fromList $ (\v -> (v, dijkstra graph v)) <$> A.vertexList graph
    distanceToInt :: Distance Int -> Int
    distanceToInt = Unsafe.fromJust . getFinite . getDistance

blockerMap' :: Map (Node, Node) [Node]
blockerMap' = blockerMap burrowGraph2

blockerMap :: BurrowGraph -> Map (Node, Node) [Node]
blockerMap graph = fromList (calcBlocker =<< nodes) where
    nodes = A.vertexList graph
    calcBlocker :: Node -> [((Node, Node), [Node])]
    calcBlocker start = (\(end, (_, path)) -> ((start, end), Unsafe.tail path)) <$> toPairs pathMap where
        pathMap = shortestPath graph start


-- Burrow structure
--  (Hallways)  1 2 --- 3 --- 4 --- 5 --- 6 7
--                   |     |     |     |
--  (Rooms)          A1    B1    C1    D1
--                   A2    B2    C2    D2
burrowGraph :: BurrowGraph
burrowGraph = graph where
    h1 = A.vertex (Hallway 1)
    h2 = A.vertex (Hallway 2)
    h3 = A.vertex (Hallway 3)
    h4 = A.vertex (Hallway 4)
    h5 = A.vertex (Hallway 5)
    h6 = A.vertex (Hallway 6)
    h7 = A.vertex (Hallway 7)
    rA1 = A.vertex (Room Amber 1)
    rB1 = A.vertex (Room Bronze 1)
    rC1 = A.vertex (Room Copper 1)
    rD1 = A.vertex (Room Desert 1)
    rA2 = A.vertex (Room Amber 2)
    rB2 = A.vertex (Room Bronze 2)
    rC2 = A.vertex (Room Copper 2)
    rD2 = A.vertex (Room Desert 2)
    graph = foldl' A.overlay A.empty (
        [ bedge 2 rA1 (bedge 2 h2 h3)
        , bedge 2 rB1 (bedge 2 h3 h4)
        , bedge 2 rC1 (bedge 2 h4 h5)
        , bedge 2 rD1 (bedge 2 h5 h6)
        , bedge 1 h1 h2
        , bedge 1 h6 h7
        , bedge 1 rA1 rA2
        , bedge 1 rB1 rB2
        , bedge 1 rC1 rC2
        , bedge 1 rD1 rD2
        ] :: [BurrowGraph])
    -- Bidirected edge
    bedge e a b = A.overlay (A.connect e b a) (A.connect e a b)

------------
-- Part 2 --
------------

solve2 :: Burrow -> Energy
solve2 = Unsafe.fromJust . simulateBurrow moves2 . unfoldBurrow

unfoldBurrow :: Burrow -> Burrow
unfoldBurrow burrow = foldl' (\b (k, v) -> insert k v b) burrow' foldedNodes where
    burrow' = M.mapKeys mapKeyFn burrow
    mapKeyFn (Room a 2) = Room a 4
    mapKeyFn x = x
    foldedNodes =
        [ (Room Amber 2, Just Desert)
        , (Room Amber 3, Just Desert)
        , (Room Bronze 2, Just Copper)
        , (Room Bronze 3, Just Bronze)
        , (Room Copper 2,Just  Bronze)
        , (Room Copper 3,Just  Amber)
        , (Room Desert 2, Just Amber)
        , (Room Desert 3, Just Copper)
        ] :: [(Node, Maybe Amphipod)]

moves2 :: Burrow -> Node -> [Node]
moves2 burrow node = join . maybeToList $ do
    a <- getOccupant burrow node
    return $ filterReachables $ targets node a where

    -- This does NOT consider if intermediate nodes are blocked
    targets :: Node -> Amphipod -> [Node]
    targets (Hallway _) a = let occ = occupants a [1..4] in
        [Room a (4 - length occ) | all (== a) occ, length occ <= 3]
    targets (Room roomTarget 1) a
        | a == roomTarget = let occ = occupants a [2..4] in if all (== a) occ && length occ == 3 then [] else hallways
        | otherwise = hallways
    targets (Room roomTarget 2) a
        | a == roomTarget = let occ = occupants a [3, 4] in if all (== a) occ && length occ == 2 then [] else hallways
        | otherwise = hallways
    targets (Room roomTarget 3) a
        | a == roomTarget = let occ = occupants a [4] in if all (== a) occ then [] else hallways
        | otherwise = hallways
    targets (Room roomTarget 4) a
        | a == roomTarget = []
        | otherwise = hallways
    targets (Room roomTarget _) a = error "Impossible room number!"
    occupants roomKind roomNumbers = catMaybes $ getOccupant burrow . Room roomKind <$> roomNumbers
    hallways = Hallway <$> [1..7]

    filterReachables :: [Node] -> [Node]
    filterReachables = filter filterFn where
        filterFn target =
            case blockerMap' !? (node, target) of
                Nothing -> False
                Just blockers -> all (isNothing . getOccupant burrow) blockers

burrowGraph2 :: BurrowGraph
burrowGraph2 = graph where
    h1 = A.vertex (Hallway 1)
    h2 = A.vertex (Hallway 2)
    h3 = A.vertex (Hallway 3)
    h4 = A.vertex (Hallway 4)
    h5 = A.vertex (Hallway 5)
    h6 = A.vertex (Hallway 6)
    h7 = A.vertex (Hallway 7)
    rA1 = A.vertex (Room Amber 1)
    rB1 = A.vertex (Room Bronze 1)
    rC1 = A.vertex (Room Copper 1)
    rD1 = A.vertex (Room Desert 1)
    rA2 = A.vertex (Room Amber 2)
    rB2 = A.vertex (Room Bronze 2)
    rC2 = A.vertex (Room Copper 2)
    rD2 = A.vertex (Room Desert 2)
    rA3 = A.vertex (Room Amber 3)
    rB3 = A.vertex (Room Bronze 3)
    rC3 = A.vertex (Room Copper 3)
    rD3 = A.vertex (Room Desert 3)
    rA4 = A.vertex (Room Amber 4)
    rB4 = A.vertex (Room Bronze 4)
    rC4 = A.vertex (Room Copper 4)
    rD4 = A.vertex (Room Desert 4)
    graph = foldl' A.overlay A.empty (
        [ bedge 2 rA1 (bedge 2 h2 h3)
        , bedge 2 rB1 (bedge 2 h3 h4)
        , bedge 2 rC1 (bedge 2 h4 h5)
        , bedge 2 rD1 (bedge 2 h5 h6)
        , bedge 1 h1 h2
        , bedge 1 h6 h7
        , bedge 1 rA1 rA2
        , bedge 1 rB1 rB2
        , bedge 1 rC1 rC2
        , bedge 1 rD1 rD2
        , bedge 1 rA2 rA3
        , bedge 1 rB2 rB3
        , bedge 1 rC2 rC3
        , bedge 1 rD2 rD3
        , bedge 1 rA3 rA4
        , bedge 1 rB3 rB4
        , bedge 1 rC3 rC4
        , bedge 1 rD3 rD4
        ] :: [BurrowGraph])
    -- Bidirected edge
    bedge e a b = A.overlay (A.connect e b a) (A.connect e a b)

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    burrow <- parseBurrow <$> readFileLines "inputs/Y2021/Day23.txt" :: IO Burrow
    print $ solve1 burrow
    print $ solve2 burrow

-- This parsing method is not versatile, it assumes fixed layout
parseBurrow :: [Text] -> Burrow
parseBurrow lines = Unsafe.fromJust $ do
    let [a1, b1, c1, d1] = fmap parseAmphipod . filter isAmphipod . toString $ lines Unsafe.!! 2
        [a2, b2, c2, d2] = fmap parseAmphipod . filter isAmphipod . toString $ lines Unsafe.!! 3
    return . fromList $
        [ (Room Amber 1, Just a1)
        , (Room Amber 2, Just a2)
        , (Room Bronze 1, Just b1)
        , (Room Bronze 2, Just b2)
        , (Room Copper 1, Just c1)
        , (Room Copper 2, Just c2)
        , (Room Desert 1, Just d1)
        , (Room Desert 2, Just d2)
        ] <> hallways
    where
    hallways = (\n -> (Hallway n, Nothing)) <$> [1..7]
    isAmphipod :: Char -> Bool
    isAmphipod c = c `elem` ("ABCD" :: [Char])
    parseAmphipod 'A' = Amber
    parseAmphipod 'B' = Bronze
    parseAmphipod 'C' = Copper
    parseAmphipod 'D' = Desert
    parseAmphipod _ = error "parse error!"
