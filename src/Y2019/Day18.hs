module Y2019.Day18 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))
import Data.Maybe (fromJust)
import Algebra.Graph.Label
import Algebra.Graph.Labelled.AdjacencyMap
import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import qualified Algebra.Graph.ToGraph as G
import Algebra.Graph.Export.Dot
import Data.Bits (Bits(..))
import Data.Char
import qualified Data.HashTable.ST.Cuckoo as H
import Data.Tree
import qualified Data.Map as M
import qualified Data.String as String
import Control.Monad.ST
import qualified Text.Read as R
import qualified Text.Show as S
import Lib.IO
import Lib.Types
import Lib.Vector2D
import Lib.Graph

-----------------------
-- Type declarations --
-----------------------

newtype Pos = Pos (Int, Int) deriving (Pos2D, Ord, Eq, Show) via (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Num) via Pos

instance Num Pos where
    Pos (x1, y1) + Pos (x2, y2) = Pos (x1 + x2, y1 + y2)
    Pos (x1, y1) - Pos (x2, y2) = Pos (x1 - x2, y1 - y2)
    Pos (x1, y1) * Pos (x2, y2) = Pos (x1 * x2, y1 * y2)
    abs (Pos (x, y)) = Pos (abs x, abs y)
    signum (Pos (x, y)) = Pos (signum x, signum y)
    fromInteger x = Pos (fromInteger x, fromInteger x)

move (Pos (x, y)) (Dir (dx, dy)) = Pos (x + dx, y + dy)

type VaultMap = Map Pos Tile
data Tile = TEmpty | TWall | TKey Char | TDoor Char | TEntrance deriving (Eq)

instance S.Show Tile where
    show TEmpty    = "."
    show TWall     = "#"
    show (TKey  r) = [r]
    show (TDoor r) = [toUpper r]
    show TEntrance = "@"

instance R.Read Tile where
    readsPrec prec = readS      where
        readS ('.' : s) = pure (TEmpty, s)
        readS ('#' : s) = pure (TWall, s)
        readS ('@' : s) = pure (TEntrance, s)
        readS (c : s)
            | isAsciiUpper c = pure (TDoor (toLower c), s)
            | isAsciiLower c = pure (TKey c, s)
            | otherwise      = error ("invalid tile: " <> [c])

getTile :: Pos -> VaultMap -> Tile
getTile key map = map !? key ?: TEmpty
setTile :: Pos -> Tile -> VaultMap -> VaultMap
setTile = insert

type Vault = AdjacencyMap (Distance Int) VObject
data VObject = VEmpty Int | VKey Char | VDoor Char | VEntrance deriving (Show, Eq, Ord)

fromTile (Pos (x, y)) TEmpty    = Just $ VEmpty (100 * y + x)
fromTile _            TWall     = Nothing
fromTile _            (TKey  c) = Just $ VKey c
fromTile _            (TDoor c) = Just $ VDoor c
fromTile _            TEntrance = Just $ VEntrance

isEmptyObj (VEmpty _) = True
isEmptyObj _          = False


-- TSP-related stuffs --

indices = 27 :: Int -- Number of keys & entrances in the input, which is 26 + 1
inf = 999999999 :: Int

type VisitedState = Int -- This is a bitmask to track visited state
type IntIndex = Int -- [0..26] representing [Entrance, (Key 'a').. (Key 'z')]
addVisited :: VisitedState -> Int -> VisitedState
addVisited visited (bit -> mask) = visited .|. mask
hasVisited :: VisitedState -> Int -> Bool
hasVisited visited (bit -> mask) = (visited .&. mask) /= 0
isSuperOf :: VisitedState -> VisitedState -> Bool
isSuperOf a b = (complement a .|. b) == -1 -- (~a & b) == 0
allVisited :: VisitedState -> Bool
allVisited = (==) (bit indices - 1)
keyToIndex :: Char -> Int
keyToIndex c = ord c - ord 'a' + 1

type MemoizedResult s = H.HashTable s (IntIndex, VisitedState) Int
readVisited memVisited lastVisit visited = (?: -1) <$> H.lookup memVisited (lastVisit, visited)
writeVisited memVisited lastVisit visited = H.insert memVisited (lastVisit, visited)

type MemoizedResult2 s = H.HashTable s (LastVisits, VisitedState) Int
readVisited' memVisited lastVisits visited = (?: -1) <$> H.lookup memVisited (lastVisits, visited)
writeVisited' memVisited lastVisits visited = H.insert memVisited (lastVisits, visited)

------------
-- Part 1 --
------------

-- Input assumptions: there is no cycle path which has a key or door in it
-- This can allow vault to be treated as tree-like structure,
-- when only taking account of keys and doors
-- One can confirm this by using 'printGraphToDot' function below

solve1 :: VaultMap -> Int
solve1 vaultMap = getShortestPathLength simplifiedGraph  where
    graph           = mapToRawGraph vaultMap
    simplifiedGraph = simplifyGraph graph

mapToRawGraph :: VaultMap -> Vault
mapToRawGraph vaultMap = overlays localGraphs  where
    makeLocalGraph :: Pos -> Vault
    makeLocalGraph pos = if posTile == TWall then G.empty else localGraph      where
        posTile  = getTile pos vaultMap
        adjTiles = map (\p -> getTile p vaultMap) adjs
        adjs     = map (move pos) [Dir (1, 0), Dir (-1, 0), Dir (0, 1), Dir (0, -1)]
        tileToVertex pos tile = G.vertex <$> fromTile pos tile :: Maybe Vault
        posVertex   = fromJust $ tileToVertex pos posTile
        adjVertices = catMaybes $ zipWith tileToVertex adjs adjTiles
        localGraph  = G.connect 1 posVertex (overlays adjVertices)
    localGraphs = map makeLocalGraph (keys vaultMap)

-- Compress the graph by leaving only entrance, keys, doors, and junctions between them
simplifyGraph :: Vault -> Vault
simplifyGraph = simplifyStems . simplifyLeafs  where
    simplifyLeafs vault = if done then cleaned else simplifyLeafs cleaned      where
        cleaned = removeEmptyLeafs vault
        done    = null $ getEmptyLeafs cleaned
    simplifyStems vault = maybe vault removeAndLoop maybeStem      where
        maybeStem     = getOneEmptyStem vault
        removeAndLoop = simplifyStems . removeEmptyStem vault
    getEmptyLeafs :: Vault -> [VObject]
    getEmptyLeafs vault = map fst . filterByDegree . filterByObject $ vertexDegrees      where
        vertexNeighbors = toPairs $ adjacencyMap vault
        vertexDegrees   = map (fmap M.size) vertexNeighbors :: [(VObject, Int)]
        filterByObject  = filter (isEmptyObj . fst)
        filterByDegree  = filter ((<= 1) . snd)
    removeEmptyLeafs :: Vault -> Vault
    removeEmptyLeafs vault = G.induce (not . isEmptyLeaf) vault      where
        emptyLeafs  = getEmptyLeafs vault
        isEmptyLeaf = flip elem emptyLeafs
    getOneEmptyStem :: Vault -> Maybe VObject
    getOneEmptyStem vault =
        viaNonEmpty head . map fst . filterByDegree . filterByObject $ vertexDegrees      where
        vertexNeighbors = toPairs $ adjacencyMap vault
        vertexDegrees   = map (fmap M.size) vertexNeighbors :: [(VObject, Int)]
        filterByObject  = filter (isEmptyObj . fst)
        filterByDegree  = filter ((== 2) . snd)
    -- Remove vertex and connect 2 neighbors
    removeEmptyStem :: Vault -> VObject -> Vault
    removeEmptyStem vault v = removeVertex v . connectNeighbors $ vault      where
        neighbors            = fromJust (adjacencyMap vault !? v) :: Map VObject (Distance Int)
        [(v1, d1), (v2, d2)] = toPairs neighbors
        d                    = d1 + d2
        connectNeighbors vault = overlays [vault, v1 -< d >- v2, v2 -< d >- v1]

getShortestPathLength :: Vault -> Int
getShortestPathLength vault = shortestPathLength  where
    !distanceMap       = getDistanceMap vault
    !accessMap         = getAccessMap vault
    shortestPathLength = doTSP distanceMap accessMap

type DistanceMap = AdjacencyMap (Distance Int) IntIndex
type AccessMap = Map IntIndex VisitedState -- Required VisitedState(== obtained keys) to get this key

-- One should better use Floyd-Warshall algorithm, but here Bellman-Ford was used
getDistanceMap :: Vault -> DistanceMap
getDistanceMap vault = G.fromAdjacencyMaps (toPairs vKeyOnlyMap)  where
    vertices = vertexList vault
    vObjectMap =
        M.fromList $ map (\v -> (v, bellmanFord vault v)) vertices :: Map
                VObject
                (Map VObject (Distance Int))
    -- Delete all objects other than 'VKey', 'VEntrance'
    toIntIndexedKeyMap :: Map VObject a -> Map IntIndex a
    toIntIndexedKeyMap = M.delete (-1) . M.mapKeys f      where
        f (VKey c)  = keyToIndex c
        f VEntrance = 0
        f _         = -1
    vKeyOnlyMap = fmap toIntIndexedKeyMap (toIntIndexedKeyMap vObjectMap)

getAccessMap :: Vault -> AccessMap
getAccessMap vault = dfsBuildAccessMap 0 mempty dfsTree  where
    dfsTree = G.dfsForestFrom [VEntrance] vault !! 0
    dfsBuildAccessMap :: VisitedState -> AccessMap -> Tree VObject -> AccessMap
    dfsBuildAccessMap visited accessMap (Node (VDoor c) children) = foldl'
        (dfsBuildAccessMap visited')
        accessMap
        children
        where visited' = addVisited visited (keyToIndex c)
    dfsBuildAccessMap visited accessMap (Node (VKey c) children) = foldl'
        (dfsBuildAccessMap visited)
        accessMap'
        children      where
        accessMap' = insert intIndex visited accessMap
        intIndex   = keyToIndex c
    dfsBuildAccessMap visited accessMap (Node _ children) =
        foldl' (dfsBuildAccessMap visited) accessMap children

-- Differences with normal TSP algorithm:
-- 1) The robot don't need to come back to the entrance (see 'checkCompleted' below)
-- 2) Some keys are accessible only after certain keys are obtained (see 'isSuperOf' below)
doTSP :: DistanceMap -> AccessMap -> Int
doTSP distanceMap accessMap = runST $ do
    memVisited <- H.newSized (2 ^ 20)
    calcLength memVisited 1 0
  where
    calcLength :: MemoizedResult s -> VisitedState -> IntIndex -> ST s Int
    calcLength memoized visited lastVisit =
        checkCompleted $ maybeLength >>= maybe (calculatedLength >>= writeLength) return      where
        calcLengthWhen toVisit
            | hasVisited visited toVisit = return inf
            | not (curBlockingDoors `isSuperOf` visited) = return inf
            | otherwise                  = (+) curLength <$> getRemainingLength          where
            newVisited = addVisited visited toVisit
            curLength =
                (?: inf) . getFinite . getDistance $ G.edgeLabel lastVisit toVisit distanceMap
            curBlockingDoors   = lookup lastVisit accessMap ?: 0
            getRemainingLength = calcLength memoized newVisited toVisit
        calculatedLength = minimum1 <$> mapM calcLengthWhen ([0 .. indices - 1] :: NonEmpty Int)
        writeLength l = writeVisited memoized lastVisit visited l >> return l
        checkCompleted or = if allVisited visited then pure 0 else or
        maybeLength =
            (\x -> if x >= 0 then Just x else Nothing) <$> readVisited memoized lastVisit visited

------------
-- Part 2 --
------------

-- This solution takes about 2 minutes to solve..
-- Hashmap key could be optimized to single Int, taking less time.

solve2 :: VaultMap -> Int
solve2 vaultMap = getShortestPathLength' simplifiedGraph keyMap  where
    graph           = mapToRawGraph vaultMap
    simplifiedGraph = simplifyGraph graph
    keyMap          = getKeyMap vaultMap

-- Instead of updating map to split into 4 sections,
-- just calculate as original map and subtract 8, to compensate length difference.
getShortestPathLength' :: Vault -> KeyMap -> Int
getShortestPathLength' vault keyMap = shortestPathLength - 8  where
    !distanceMap       = getDistanceMap vault
    !accessMap         = getAccessMap vault
    shortestPathLength = doTSP' distanceMap accessMap keyMap

type KeyMap = Map IntIndex VaultSection
data VaultSection = VTopLeft | VTopRight | VBottomLeft | VBottomRight deriving (Eq, Show)
data LastVisits = LastVisits IntIndex IntIndex IntIndex IntIndex deriving (Eq, Show, Generic, Hashable)

-- Differences with normal TSP algorithm:
-- 1) The robot don't need to come back to the entrance (see 'checkCompleted' below)
-- 2) Some keys are accessible only after certain keys are obtained (see 'isSuperOf' below)
-- 3) 4 sections are taken into account when calculating distnaces (see 'lastVisit' and 'curLength' below)
doTSP' :: DistanceMap -> AccessMap -> KeyMap -> Int
doTSP' distanceMap accessMap keyMap = runST $ do
    memVisited <- H.newSized (2 ^ 20)
    calcLength memVisited 1 (LastVisits 0 0 0 0)
  where
    calcLength :: MemoizedResult2 s -> VisitedState -> LastVisits -> ST s Int
    calcLength memoized visited lastVisits =
        checkCompleted $ maybeLength >>= maybe (calculatedLength >>= writeLength) return      where
        calcLengthWhen toVisit
            | hasVisited visited toVisit = return inf
            | not (curBlockingDoors `isSuperOf` visited) = return inf
            | otherwise                  = (+) curLength <$> getRemainingLength          where
            lastVisit  = getLastVisit keyMap toVisit lastVisits
            newVisited = addVisited visited toVisit
            curLength =
                (?: inf) . getFinite . getDistance $ G.edgeLabel lastVisit toVisit distanceMap
            curBlockingDoors   = lookup lastVisit accessMap ?: 0
            getRemainingLength = calcLength memoized newVisited newLastVisits
            newLastVisits      = setLastVisit keyMap toVisit lastVisits
        calculatedLength = minimum1 <$> mapM calcLengthWhen ([0 .. indices - 1] :: NonEmpty Int)
        writeLength l = writeVisited memoized lastVisits visited l >> return l
        checkCompleted or = if allVisited visited then pure 0 else or
        maybeLength =
            (\x -> if x >= 0 then Just x else Nothing) <$> readVisited memoized lastVisits visited

getLastVisit :: KeyMap -> IntIndex -> LastVisits -> IntIndex
getLastVisit keyMap toVisit (LastVisits tl tr bl br) = case section of
    VTopLeft     -> tl
    VTopRight    -> tr
    VBottomLeft  -> bl
    VBottomRight -> br
    where section = lookup toVisit keyMap ?: VTopLeft

setLastVisit :: KeyMap -> IntIndex -> LastVisits -> LastVisits
setLastVisit keyMap toVisit (LastVisits tl tr bl br) = case section of
    VTopLeft     -> LastVisits toVisit tr bl br
    VTopRight    -> LastVisits tl toVisit bl br
    VBottomLeft  -> LastVisits tl tr toVisit br
    VBottomRight -> LastVisits tl tr bl toVisit
    where section = lookup toVisit keyMap ?: VTopLeft

getKeyMap :: VaultMap -> KeyMap
getKeyMap vaultMap = keyMap'  where
    entrancePos = fst $ find (\(_, o) -> o == TEntrance) (toPairs vaultMap) ?: error "no entrance"
    keyMap'     = foldl' insertKeySection mempty (toPairs vaultMap)
    insertKeySection keyMap (pos, TKey c) =
        insert (keyToIndex c) (getSection entrancePos pos) keyMap
    insertKeySection keyMap _ = keyMap
    getSection (Pos (x0, y0)) (Pos (x1, y1))
        | x0 > x1 && y0 > y1 = VTopLeft
        | x0 < x1 && y0 > y1 = VTopRight
        | x0 > x1 && y0 < y1 = VBottomLeft
        | otherwise          = VBottomRight

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    vaultMap <- readVaultMap <$> readFile "inputs/Y2019/Day18.txt" :: IO VaultMap
    -- Uncommnent this to see graph as Dot
    -- printGraphToDot vaultMap
    print $ solve1 vaultMap
    print $ solve2 vaultMap

readVaultMap :: String -> VaultMap
readVaultMap str = M.fromList areaArrayWithPos  where
    areaArray = map (map (\c -> R.read [c])) (String.lines str) :: [[Tile]]
    alongY    = snd . mapAccumL (\p xs -> (p + Pos (0, 1), alongX p xs)) (Pos (0, 0))
    alongX yPos = scanl (\(p, _) t -> (p + Pos (1, 0), t)) (yPos - Pos (1, 0), TEmpty)
    areaArrayWithPos = concat (alongY areaArray) :: [(Pos, Tile)]

-- This function prints graph as Dot language, so that one can see it visualized
printGraphToDot :: VaultMap -> IO ()
printGraphToDot vaultMap = putTextLn dotText  where
    graph   = mapToRawGraph vaultMap
    simple  = simplifyGraph graph
    dotText = export defaultStyleViaShow simple

