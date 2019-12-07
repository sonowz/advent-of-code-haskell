module Y2019.Day06 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.Text
import Data.Tree
import Data.Maybe (fromJust)
import Lib.IO
import Lib.Types
import Lib.NonEmpty

-----------------------
-- Type declarations --
-----------------------

data LocalOrbit = LOrbit Body Body deriving (Show)
type Orbit = Tree Body
newtype Body = Body Text deriving (Show, Eq) via Text

makeOrbits :: [LocalOrbit] -> Body -> Orbit
makeOrbits localOrbits root = Node { rootLabel = root, subForest = orbitBodies }  where
    orbitBodyNames = filter ((==) root . oCenter) localOrbits
    orbitBodies    = toList $ fmap (makeOrbits localOrbits . oOrbit) orbitBodyNames

oCenter (LOrbit center orbit) = center
oOrbit (LOrbit center orbit) = orbit

------------
-- Part 1 --
------------

solve1 :: [LocalOrbit] -> Int
solve1 localOrbits = sum depths  where
    orbits      = makeOrbits localOrbits (Body "COM")
    orbitDepths = dfsDepth 0 orbits
    depths      = map snd (flatten orbitDepths) :: [Int]

dfsDepth :: Int -> Orbit -> Tree (Body, Int)
dfsDepth d orbit = Node { rootLabel = retLabel, subForest = retForest }  where
    retLabel  = (rootLabel orbit, d)
    retForest = map (dfsDepth (d + 1)) (subForest orbit)

------------
-- Part 2 --
------------

solve2 :: [LocalOrbit] -> Int
solve2 localOrbits = distance  where
    orbits      = makeOrbits localOrbits (Body "COM")
    orbitDepths = dfsDepth 0 orbits
    lcaBody'    = lcaBody localOrbits (Body "YOU") (Body "SAN")
    distance =
        (depthOf (Body "YOU") - depthOf lcaBody') + (depthOf (Body "SAN") - depthOf lcaBody') - 2
    depthOf body = snd . fromJust $ find ((==) body . fst) (flatten orbitDepths)



-- Lowest Common Ancestor
lcaBody :: [LocalOrbit] -> Body -> Body -> Body
lcaBody localOrbits body1 body2 = lca body1Path body2Path  where
    body1Path = orbitFromCOM localOrbits body1
    body2Path = orbitFromCOM localOrbits body2
    orbitFromCOM :: [LocalOrbit] -> Body -> [Body]
    orbitFromCOM localOrbits (Body "COM") = [Body "COM"]
    orbitFromCOM localOrbits body         = orbitFromCOM localOrbits center <> [center]
        where center = oCenter . fromJust $ find ((==) body . oOrbit) localOrbits
    lca (x1 : x2 : xs) (y1 : y2 : ys) = if x2 /= y2 then x1 else lca (x2 : xs) (y2 : ys)
    lca _              _              = Body "COM"

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    localOrbits <- parseLocalOrbit <<$>> readFileLines "inputs/Y2019/Day06.txt" :: IO [LocalOrbit]
    print $ solve1 localOrbits
    print $ solve2 localOrbits

parserLocalOrbit :: Parser LocalOrbit
parserLocalOrbit = do
    center <- toText <$> many1 alphaNum
    char ')'
    orbit <- toText <$> many1 alphaNum
    return $ LOrbit (Body center) (Body orbit)

parseLocalOrbit text = fromRight (error "parse error") $ parse parserLocalOrbit "" text
