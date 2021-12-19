module Y2021.Day19 where

import qualified Algebra.Graph.AdjacencyMap as A
import qualified Data.Set as S
import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import qualified Relude.Unsafe as Unsafe
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Text

-----------------------
-- Type declarations --
-----------------------

newtype BeaconPos = BeaconPos (Int, Int, Int) deriving (Eq, Ord, Show) via (Int, Int, Int)
newtype ScannerPos = ScannerPos (Int, Int, Int) deriving (Num, Show, Eq, Ord) via BeaconPos

newtype ScannerId = ScannerId Int deriving (Show, Eq, Ord)
data ScanResult = ScanResult
    { getId      :: ScannerId
    , getScanner :: ScannerPos
    , getBeacons :: [BeaconPos]
    }
    deriving Show

-- Has edges when two scanners can be established
type ScannerGraph = A.AdjacencyMap ScanResult


instance Num BeaconPos where
    BeaconPos (x1, y1, z1) + BeaconPos (x2, y2, z2) = BeaconPos (x1 + x2, y1 + y2, z1 + z2)
    BeaconPos (x1, y1, z1) - BeaconPos (x2, y2, z2) = BeaconPos (x1 - x2, y1 - y2, z1 - z2)
    BeaconPos (x1, y1, z1) * BeaconPos (x2, y2, z2) = BeaconPos (x1 * x2, y1 * y2, z1 * z2)
    abs (BeaconPos (x, y, z)) = BeaconPos (abs x, abs y, abs z)
    signum (BeaconPos (x, y, z)) = BeaconPos (signum x, signum y, signum z)
    fromInteger x = BeaconPos (fromInteger x, fromInteger x, fromInteger x)
instance Semigroup ScannerPos where
    (<>) = (+)
instance Monoid ScannerPos where
    mempty = ScannerPos (0, 0, 0)

instance Eq ScanResult where
    (==) = (==) `on` getId
instance Ord ScanResult where
    compare = compare `on` getId

------------
-- Part 1 --
------------

-- For performance, 'ScannerGraph' is pre-calculated
solve1 :: ScannerGraph -> [ScanResult] -> Int
solve1 graph scanResults = length beacons  where
    start        = Unsafe.head scanResults
    (_, beacons) = alignAll start graph


-- Find first 'Just x'
findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f l = do
    let hd : tl = f <$> l
    foldl' (<|>) hd tl


-- If establish succeeds, return with second ScanResult
-- whose coordinates are aligned with first ScanResult
tryEstablish :: ScanResult -> ScanResult -> Maybe ScanResult
tryEstablish (ScanResult _ _ bc1) (ScanResult sid2 _ bc2) = findJust findFn matchCases  where
    findFn (sr1', sr2') = uncurry (ScanResult sid2) <$> tryEstablishWithOrientation sr1' sr2'
    matchCases :: [([BeaconPos], [BeaconPos])]
    matchCases = zip (repeat bc1) (transpose (orientations <$> bc2))

    tryEstablishWithOrientation :: [BeaconPos] -> [BeaconPos] -> Maybe (ScannerPos, [BeaconPos])
    tryEstablishWithOrientation bc1 bc2 = findJust has12Matchings firstMatchCases      where
        firstMatchCases = [ (p1, p2) | p1 <- bc1, p2 <- bc2 ]
        -- 'mp1' means matching point in first ScanResult
        has12Matchings :: (BeaconPos, BeaconPos) -> Maybe (ScannerPos, [BeaconPos])
        has12Matchings (mp1, mp2) = if length matchings >= 12
            then Just (toScannerPos sc2Aligned, toList bc2Aligned) -- aligned coordinates
            else Nothing
          where
            bc1'       = fromList bc1 :: Set BeaconPos
            -- since mp1 and mp2 are same position, align bc2 by adding aligned sc2
            sc2Aligned = mp1 - mp2 :: BeaconPos
            bc2Aligned = fromList $ (+ sc2Aligned) <$> bc2 :: Set BeaconPos
            matchings  = S.intersection bc1' bc2Aligned :: Set BeaconPos
            toScannerPos (BeaconPos x) = ScannerPos x


-- Returns 24 positions
orientations :: BeaconPos -> [BeaconPos]
orientations (BeaconPos (x, y, z)) =
    [ BeaconPos (xy2xyz xy) | xy <- orientations2D, xy2xyz <- zRollAxes ]  where
    -- Rotation in xy plane, 4 ways
    orientations2D :: [(Int, Int)]
    orientations2D = [(x, y), (y, -x), (-x, -y), (-y, x)]
    -- "Roll axis" of z, while considering xy conform to "right-hand rule", 6 ways
    -- Search for "pitch-yaw-roll" for more information
    zRollAxes :: [(Int, Int) -> (Int, Int, Int)]
    zRollAxes =
        [ \(x, y) -> (x, y, z)
        , \(x, y) -> (x, -y, -z)
        , \(x, y) -> (x, z, -y)
        , \(x, y) -> (x, -z, y)
        , \(x, y) -> (z, y, -x)
        , \(x, y) -> (-z, y, x)
        ]


buildScannerGraph :: [ScanResult] -> ScannerGraph
buildScannerGraph scanResults = A.edges edgeList  where
    !edgeList =
        [ (sr1, sr2)
        | sr1 <- scanResults
        , sr2 <- scanResults
        , sr1 /= sr2
        , isJust (tryEstablish sr1 sr2)
        ]


type Visited = Set ScanResult
type ScannerGraph' = Map ScanResult (Set ScanResult)

-- Collect positions of all scanners and beacons relative to 'start' Scanner
-- Uses DFS
alignAll :: ScanResult -> ScannerGraph -> (Set ScannerPos, Set BeaconPos)
alignAll start graph = fstSnd $ go (A.adjacencyMap graph) start start mempty  where
    fstSnd (f, s, _) = (f, s)
    (!) m k = Unsafe.fromJust $ (!?) m k
    toBeaconPos (ScannerPos x) = BeaconPos x
    go
        :: ScannerGraph'
        -> ScanResult
        -> ScanResult
        -> Visited
        -> (Set ScannerPos, Set BeaconPos, Visited)
    go graph prev v visited
        | member v visited = (mempty, mempty, visited)
        | otherwise        = recursed      where
        visited' = S.insert v visited
        beacons  = fromList $ getBeacons v :: Set BeaconPos
        scanner  = getScanner v :: ScannerPos

        children = toList (graph ! v) :: [ScanResult]
        recursed = foldl' foldFn (one scanner, beacons, visited') children
        foldFn (childScanners, childBeacons, visit) v' =
            let -- Graph data is not aligned to Scanner 0, so align it
                vAligned'                               = Unsafe.fromJust $ tryEstablish v v'
                (childScanners', childBeacons', visit') = go graph v vAligned' visit
            in (childScanners <> childScanners', childBeacons <> childBeacons', visit')


------------
-- Part 2 --
------------

solve2 :: ScannerGraph -> [ScanResult] -> Int
solve2 graph scanResults = viaNonEmpty maximum1 distances ?: 0  where
    start     = Unsafe.head scanResults
    scanners  = toList . fst $ alignAll start graph :: [ScannerPos]
    distances = [ manhattanD s1 s2 | s1 <- scanners, s2 <- scanners ]

manhattanD :: ScannerPos -> ScannerPos -> Int
manhattanD (ScannerPos (x1, y1, z1)) (ScannerPos (x2, y2, z2)) =
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)


--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    scanResults <- parseScanResult <$> readFileLines "inputs/Y2021/Day19.txt" :: IO [ScanResult]
    let graph = buildScannerGraph scanResults
    print $ solve1 graph scanResults
    print $ solve2 graph scanResults

number :: Parser Int
number = do
    sign   <- optionMaybe (char '-')
    digits <- many1 digit
    return . readInt . toText $ maybe digits (: digits) sign

parseScanResult :: [Text] -> [ScanResult]
parseScanResult text = fromRight (error "parse error") $ parse parserScanResults "" (unlines text)

parserScanResults :: Parser [ScanResult]
parserScanResults = parserScanResult `sepBy1` newline

parserScanResult :: Parser ScanResult
parserScanResult = do
    string "--- scanner "
    scannerId <- ScannerId <$> number
    string " ---\n"
    ScanResult scannerId (ScannerPos (0, 0, 0)) <$> parserPos `sepEndBy1` newline

parserPos :: Parser BeaconPos
parserPos = do
    [x, y, z] <- number `sepBy1` char ','
    return $ BeaconPos (x, y, z)
