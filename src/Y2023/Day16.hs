module Y2023.Day16 (main') where

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

-----------------------
-- Type declarations --
-----------------------

type Contraption = Map Pos Tile

data Tile = Empty | MirrorPositive | MirrorNegative | SplitterHorizontal | SplitterVertical deriving (Show, Eq)

newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord, Pos2D) via (Int, Int)

data Dir = DUp | DDown | DLeft | DRight deriving (Show, Eq, Ord)

-- (position, starting direction in position)
type BeamPath = Set (Pos, Dir)

------------
-- Part 1 --
------------

solve1 :: Contraption -> Int
solve1 contraption = length (energizedPos beamPath)
  where
    beamPath = getBeamPath contraption (Pos (0, 0), DRight)

energizedPos :: BeamPath -> Set Pos
energizedPos = fromList . fmap fst . toList

getBeamPath :: Contraption -> (Pos, Dir) -> BeamPath
getBeamPath contraption initPosDir = go (one initPosDir) initPosDir
  where
    go beamPath (pos, dir) = result
      where
        nextPaths = case contraption !? pos of
          Just tile -> filter isInsideContraption $ nextPath tile dir pos
          Nothing -> []
        isInsideContraption (p, _) = p `member` contraption
        newBeamPath = beamPath <> (fromList nextPaths :: BeamPath)
        result = if beamPath == newBeamPath then beamPath else foldl' go newBeamPath nextPaths

nextPath :: Tile -> Dir -> Pos -> [(Pos, Dir)]
nextPath Empty dir = pure . move dir
nextPath MirrorPositive DUp = pure . move DRight
nextPath MirrorPositive DDown = pure . move DLeft
nextPath MirrorPositive DLeft = pure . move DDown
nextPath MirrorPositive DRight = pure . move DUp
nextPath MirrorNegative DUp = pure . move DLeft
nextPath MirrorNegative DDown = pure . move DRight
nextPath MirrorNegative DLeft = pure . move DUp
nextPath MirrorNegative DRight = pure . move DDown
nextPath SplitterHorizontal DUp = \pos -> [move DLeft pos, move DRight pos]
nextPath SplitterHorizontal DDown = \pos -> [move DLeft pos, move DRight pos]
nextPath SplitterHorizontal dir = pure . move dir
nextPath SplitterVertical DLeft = \pos -> [move DUp pos, move DDown pos]
nextPath SplitterVertical DRight = \pos -> [move DUp pos, move DDown pos]
nextPath SplitterVertical dir = pure . move dir

move :: Dir -> Pos -> (Pos, Dir)
move DUp (Pos (x, y)) = (Pos (x, y - 1), DUp)
move DDown (Pos (x, y)) = (Pos (x, y + 1), DDown)
move DLeft (Pos (x, y)) = (Pos (x - 1, y), DLeft)
move DRight (Pos (x, y)) = (Pos (x + 1, y), DRight)

------------
-- Part 2 --
------------

solve2 :: Contraption -> Int
solve2 contraption = maximum1 . nonEmpty' . fmap (length . energizedPos) $ beamPaths
  where
    beamPaths :: [BeamPath]
    beamPaths = getBeamPath contraption <$> initBeamPaths contraption

initBeamPaths :: Contraption -> [(Pos, Dir)]
initBeamPaths contraption = rightPaths <> leftPaths <> upPaths <> downPaths
  where
    contraptionPos = nonEmpty' . keys $ contraption
    Pos (minX, minY) = minimum1 contraptionPos
    Pos (maxX, maxY) = maximum1 contraptionPos
    rightPaths = [(Pos (minX, y), DRight) | y <- [minY .. maxY]]
    leftPaths = [(Pos (maxX, y), DLeft) | y <- [minY .. maxY]]
    upPaths = [(Pos (x, maxY), DUp) | x <- [minX .. maxX]]
    downPaths = [(Pos (x, minY), DDown) | x <- [minX .. maxX]]

nonEmpty' :: [a] -> NonEmpty a
nonEmpty' = Unsafe.fromJust . nonEmpty

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  contraption <- parseContraption <$> readFileLines "inputs/Y2023/Day16.txt" :: IO Contraption
  print $ solve1 contraption
  print $ solve2 contraption

parseContraption :: [Text] -> Contraption
parseContraption = readMap [0 ..] [0 ..] parseTile
  where
    parseTile '.' = Empty
    parseTile '/' = MirrorPositive
    parseTile '\\' = MirrorNegative
    parseTile '-' = SplitterHorizontal
    parseTile '|' = SplitterVertical
    parseTile _ = error "Invalid tile"
