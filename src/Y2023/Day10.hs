module Y2023.Day10 (main') where

import Data.Map.Strict (mapWithKey, (!))
import Data.Set qualified as Set
import Lib.IO
import Lib.Parser qualified as P
import Lib.Types
import Lib.Vector2D (Pos2D)
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe qualified as Unsafe
import Text.Show qualified as S

-----------------------
-- Type declarations --
-----------------------

data PipeMapInfo = PipeMapInfo
  { startPos :: Pos,
    pipeMap :: PipeMap
  }

type PipeMap = Map Pos Pipe

-- Contains (position, direction to next position)
type LoopPath = [(Pos, Dir)]

newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord, Pos2D) via (Int, Int)

data Pipe = PEmpty | Pipe (Set Dir) deriving (Eq)

instance S.Show Pipe where
  show PEmpty = " "
  show (Pipe dirs) = case toList dirs of
    [North, South] -> "┃"
    [East, West] -> "━"
    [North, East] -> "┗"
    [North, West] -> "┛"
    [South, West] -> "┓"
    [South, East] -> "┏"
    _ -> error "Invalid pipe"

data Dir = North | South | East | West deriving (Show, Eq, Ord, Enum, Bounded)

allDirs :: [Dir]
allDirs = universe

------------
-- Part 1 --
------------

solve1 :: PipeMapInfo -> Int
solve1 (updatePipeMapInfo -> PipeMapInfo {..}) = length loop `div` 2
  where
    loop = getLoop pipeMap startPos

updatePipeMapInfo :: PipeMapInfo -> PipeMapInfo
updatePipeMapInfo pipeMapInfo@(PipeMapInfo startPos pipeMap) = PipeMapInfo startPos pipeMap'
  where
    startPosPipe = determineStartPosPipe pipeMapInfo
    pipeMap' = insert startPos startPosPipe pipeMap

-- Assumption: only 2 pipes in adjacents of start position are connected to start position
determineStartPosPipe :: PipeMapInfo -> Pipe
determineStartPosPipe (PipeMapInfo startPos pipeMap) = startPosPipe
  where
    checkDirConnected :: Dir -> Maybe Dir
    checkDirConnected dir = pipeMap !? (startPos `moveTo` dir) >>= \pipe -> if connected pipe (opposite dir) then Just dir else Nothing
    startPosPipe = Pipe . fromList . mapMaybe checkDirConnected $ allDirs

getLoop :: PipeMap -> Pos -> LoopPath
getLoop pipeMap startPos = (startState :) . takeWhile ((/=) startPos . fst) . drop 1 $ traversals
  where
    startDir = Unsafe.fromJust $ (\(Pipe dirs) -> Unsafe.head $ toList dirs) <$> pipeMap !? startPos
    startState = (startPos, startDir)

    traversals :: [(Pos, Dir)]
    traversals = iterate iterateFn startState
    iterateFn :: (Pos, Dir) -> (Pos, Dir)
    iterateFn (pos, dir) = (nextPos, nextDir)
      where
        nextPos = pos `moveTo` dir
        nextDir = reversePipeDir pipeMap nextPos (opposite dir)

reversePipeDir :: PipeMap -> Pos -> Dir -> Dir
reversePipeDir pipeMap pos dir = fromMaybe (error "reversePipeDir failed") $ do
  (Pipe dirs) <- pipeMap !? pos
  find (/= dir) dirs

opposite :: Dir -> Dir
opposite North = South
opposite South = North
opposite East = West
opposite West = East

connected :: Pipe -> Dir -> Bool
connected (Pipe dirs) dir = dir `member` dirs
connected PEmpty _ = False

moveTo :: Pos -> Dir -> Pos
moveTo (Pos (x, y)) North = Pos (x, y - 1)
moveTo (Pos (x, y)) South = Pos (x, y + 1)
moveTo (Pos (x, y)) East = Pos (x + 1, y)
moveTo (Pos (x, y)) West = Pos (x - 1, y)

------------
-- Part 2 --
------------

solve2 :: PipeMapInfo -> Int
solve2 (updatePipeMapInfo -> PipeMapInfo {..}) = length enclosure
  where
    loop = getLoop pipeMap startPos
    pipePos = fromList $ map fst loop :: Set Pos
    loopPipeMap = mapWithKey (\pos tile -> if pos `member` pipePos then tile else PEmpty) pipeMap
    enclosure = getEnclosure loopPipeMap loop

getEnclosure :: PipeMap -> LoopPath -> Set Pos
getEnclosure pipeMap loop = foldl' foldFn mempty loop'
  where
    loop' = makePositivelyOriented pipeMap loop

    foldFn :: Set Pos -> (Pos, Dir) -> Set Pos
    foldFn enclosure (pos, dir) = enclosure <> newEnclosure
      where
        newEnclosureTiles = getEnclosureTile (pipeMap ! pos) pos dir
        newEnclosureTiles' = filter (not . flip member enclosure) newEnclosureTiles
        newEnclosure = foldMap (floodFill pipeMap) newEnclosureTiles'

    getEnclosureTile :: Pipe -> Pos -> Dir -> [Pos]
    getEnclosureTile (Pipe dirs) p North | member South dirs = moveTo p <$> [West]
    getEnclosureTile (Pipe dirs) p North | member East dirs = moveTo p <$> [West, South]
    getEnclosureTile (Pipe dirs) p South | member North dirs = moveTo p <$> [East]
    getEnclosureTile (Pipe dirs) p South | member West dirs = moveTo p <$> [East, North]
    getEnclosureTile (Pipe dirs) p East | member West dirs = moveTo p <$> [North]
    getEnclosureTile (Pipe dirs) p East | member South dirs = moveTo p <$> [North, West]
    getEnclosureTile (Pipe dirs) p West | member East dirs = moveTo p <$> [South]
    getEnclosureTile (Pipe dirs) p West | member North dirs = moveTo p <$> [South, East]
    getEnclosureTile _ _ _ = []

data TurnDir = TLeft | TRight deriving (Eq)

-- Make loop path turn counter-clockwise
-- so that enclosure is always on the left
makePositivelyOriented :: PipeMap -> LoopPath -> LoopPath
makePositivelyOriented pipeMap loop = loop'
  where
    loop' = if isPositivelyOriented loop then loop else reversedLoop
    reversedLoop = (\(pos, dir) -> (pos, reversePipeDir pipeMap pos dir)) <$> reverse loop

    isPositivelyOriented :: LoopPath -> Bool
    isPositivelyOriented loop = turnLeftCount > turnRightCount
      where
        turnDirs :: [TurnDir]
        turnDirs = mapMaybe (uncurry (getTurnDir pipeMap)) loop
        turnLeftCount = length $ filter (== TLeft) turnDirs
        turnRightCount = length $ filter (== TRight) turnDirs

    getTurnDir :: PipeMap -> Pos -> Dir -> Maybe TurnDir
    getTurnDir pipeMap pos dir = go (reversePipeDir pipeMap pos dir) dir
      where
        go :: Dir -> Dir -> Maybe TurnDir
        go North East = Just TLeft
        go North West = Just TRight
        go South East = Just TRight
        go South West = Just TLeft
        go East North = Just TRight
        go East South = Just TLeft
        go West North = Just TLeft
        go West South = Just TRight
        go _ _ = Nothing

floodFill :: PipeMap -> Pos -> Set Pos
floodFill pipeMap = go mempty
  where
    go :: Set Pos -> Pos -> Set Pos
    go s p | member p s = s
    go s p | not (isEmptyTile p) = s
    go s p = foldl' go (Set.insert p s) (adjacent p)

    isEmptyTile :: Pos -> Bool
    isEmptyTile p = case pipeMap !? p of
      Just PEmpty -> True
      _ -> False

adjacent :: Pos -> [Pos]
adjacent p = (p `moveTo`) <$> allDirs

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  pipeMapInfo <- parsePipeMapInfo <$> readFileLines "inputs/Y2023/Day10.txt" :: IO PipeMapInfo
  print $ solve1 pipeMapInfo
  print $ solve2 pipeMapInfo

parsePipeMapInfo :: [Text] -> PipeMapInfo
parsePipeMapInfo lines = PipeMapInfo startPos pipeMap
  where
    charMap = readMap [0 ..] [0 ..] id lines :: Map Pos Char
    startPos = fst . Unsafe.fromJust . find ((==) 'S' . snd) . toPairs $ charMap
    pipeMap = parsePipe <$> charMap

parsePipe :: Char -> Pipe
parsePipe 'S' = PEmpty
parsePipe '.' = PEmpty
parsePipe '|' = Pipe [North, South]
parsePipe '-' = Pipe [East, West]
parsePipe 'L' = Pipe [North, East]
parsePipe 'J' = Pipe [North, West]
parsePipe '7' = Pipe [South, West]
parsePipe 'F' = Pipe [South, East]
parsePipe _ = error "Invalid pipe"
