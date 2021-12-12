module Y2021.Day12 where

import qualified Algebra.Graph.Undirected as U
import Data.Char (isLower)
import qualified Data.Set as S
import Data.Text (split)
import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))

-----------------------
-- Type declarations --
-----------------------

data Cave = CStart | CEnd | CBig String | CSmall String deriving (Eq, Ord, Show)
type CaveMap = U.Graph Cave
type CavePath = [Cave]
type Visited = Set Cave

------------
-- Part 1 --
------------

solve1 :: CaveMap -> Int
solve1 caveMap = length $ dfsPath caveMap mempty CStart

dfsPath :: CaveMap -> Visited -> Cave -> [CavePath]
dfsPath map visited vertex
    | vertex == CEnd = [[CEnd]]
    | alreadyVisited && isOneTimeVisit vertex = []
    | otherwise = appendCurrentVertex $ foldMap (dfsPath map visited') neighbors
  where
    alreadyVisited = vertex `member` visited
    neighbors      = U.neighbours vertex map
    visited'       = S.insert vertex visited
    appendCurrentVertex :: [CavePath] -> [CavePath]
    appendCurrentVertex = fmap (vertex :)
    isOneTimeVisit CStart     = True
    isOneTimeVisit (CSmall _) = True
    isOneTimeVisit _          = False

------------
-- Part 2 --
------------

solve2 :: CaveMap -> Int
solve2 caveMap = length $ dfsPath2 caveMap mempty False CStart

-- 'smallTwice' tracks whether visiting small cave twice was performed
dfsPath2 :: CaveMap -> Visited -> Bool -> Cave -> [CavePath]
dfsPath2 map visited smallTwice vertex
    | vertex == CEnd = [[CEnd]]
    | alreadyVisited && isStart vertex = []
    | alreadyVisited && isSmall vertex && smallTwice = []
    | alreadyVisited && isSmall vertex && not smallTwice = appendCurrentVertex
    $ foldMap (dfsPath2 map visited' True) neighbors
    | otherwise = appendCurrentVertex $ foldMap (dfsPath2 map visited' smallTwice) neighbors
  where
    alreadyVisited = vertex `member` visited
    neighbors      = U.neighbours vertex map
    visited'       = S.insert vertex visited
    appendCurrentVertex :: [CavePath] -> [CavePath]
    appendCurrentVertex = fmap (vertex :)
    isStart CStart = True
    isStart _      = False
    isSmall (CSmall _) = True
    isSmall _          = False

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    caveMap <- parseCaveMap <$> readFileLines "inputs/Y2021/Day12.txt" :: IO CaveMap
    print $ solve1 caveMap
    print $ solve2 caveMap

parseCaveMap :: [Text] -> CaveMap
parseCaveMap lines = U.edges edgeList where edgeList = parseCaveEdge <$> lines

parseCaveEdge :: Text -> (Cave, Cave)
parseCaveEdge line = (parseCave startText, parseCave endText)  where
    (startText, endText) = getStartEnd $ split (== '-') line
    getStartEnd [a, b] = (a, b)
    getStartEnd _      = error "Invalid input!"

parseCave :: Text -> Cave
parseCave text
    | text == "start"              = CStart
    | text == "end"                = CEnd
    | isLower (toString text !! 0) = CSmall (toString text)
    | otherwise                    = CBig (toString text)
