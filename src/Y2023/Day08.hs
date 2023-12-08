module Y2023.Day08 (main') where

import Lib.IO
import Lib.Types
import qualified Lib.Parser as P
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import qualified Data.Text as T
import Data.Map.Strict ((!))

-----------------------
-- Type declarations --
-----------------------

data NetworkMap = NetworkMap {
  initDirs :: [Direction],
  graph :: Map Node (Direction -> Node)
}
newtype Node = Node Text deriving (Show, Eq, Ord) via Text
data Direction = DLeft | DRight deriving (Show, Eq)

------------
-- Part 1 --
------------

solve1 :: NetworkMap -> Int
solve1 networkMap = length path where
  path = getPath networkMap (cycle $ initDirs networkMap) (Node "AAA") (== Node "ZZZ")

getPath :: NetworkMap -> [Direction] -> Node -> (Node -> Bool) -> [Node]
getPath NetworkMap{..} dirs start isEnd = takeWhile (not . isEnd) $ scanl (graph !) start dirs

------------
-- Part 2 --
------------

-- Assumption 1. Each nodes are reachable at most one cycle path
-- Assumption 2. In each cycle paths there are only one start node and end node
-- Assumption 3. In each cycle paths there exists N such that end node is visited every N steps and not in other steps 
solve2 :: NetworkMap -> Integer
solve2 networkMap = un $ fold cycleLengths where
  startNodes :: [Node]
  startNodes = filter isStartNode2 . keys . graph $ networkMap
  cycleLengths :: [CycleLength]
  cycleLengths = getCycleLength networkMap <$> startNodes

getCycleLength :: NetworkMap -> Node -> CycleLength
getCycleLength networkMap start = CycleLength (toInteger $ length path) where
  path = getPath networkMap (cycle $ initDirs networkMap) start isEndNode2

isStartNode2 :: Node -> Bool
isStartNode2 (Node name) = "A" `T.isSuffixOf` name
isEndNode2 :: Node -> Bool
isEndNode2 (Node name) = "Z" `T.isSuffixOf` name

newtype CycleLength = CycleLength Integer deriving (Show) via Integer

instance Semigroup CycleLength where
  CycleLength a <> CycleLength b = CycleLength (lcm a b)

instance Monoid CycleLength where
  mempty = CycleLength 1

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  networkMap <- parseNetworkMap <$> readFileLines "inputs/Y2023/Day08.txt" :: IO NetworkMap
  print $ solve1 networkMap
  print $ solve2 networkMap

parseNetworkMap :: [Text] -> NetworkMap
parseNetworkMap (initDirStr:_:graphStr) = NetworkMap {..} where
  initDirs = parseDir <$> toString initDirStr
  graph = fromList $ parseEdge <$> graphStr
parseNetworkMap _ = error "Invalid input"

parseDir :: Char -> Direction
parseDir 'L' = DLeft
parseDir 'R' = DRight
parseDir _ = error "Invalid direction"

parseEdge :: Text -> (Node, Direction -> Node)
parseEdge line = (Node start, endMapping) where
  [start, _, endLeftStr, rightEndStr] = words line
  endLeft = T.take 3 . T.drop 1 $ endLeftStr
  endRight = T.take 3 rightEndStr
  endMapping = \case
    DLeft -> Node endLeft
    DRight -> Node endRight
