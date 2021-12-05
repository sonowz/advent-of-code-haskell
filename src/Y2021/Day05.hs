module Y2021.Day05 where

import Data.Text (split)
import Lib.IO
import Lib.Types
import Lib.Vector2D
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text

-----------------------
-- Type declarations --
-----------------------

data VentLine = VentLine Pos Pos
    deriving Show
newtype Pos = Pos (Int, Int) deriving (Pos2D, Ord, Eq, Show) via (Int, Int)
type VentMap = Map Pos Int

instance Num Pos where
    Pos (x1, y1) + Pos (x2, y2) = Pos (x1 + x2, y1 + y2)
    Pos (x1, y1) - Pos (x2, y2) = Pos (x1 - x2, y1 - y2)
    Pos (x1, y1) * Pos (x2, y2) = Pos (x1 * x2, y1 * y2)
    abs (Pos (x, y)) = Pos (abs x, abs y)
    signum (Pos (x, y)) = Pos (signum x, signum y)
    fromInteger x = Pos (fromInteger x, fromInteger x)

getTile :: Pos -> VentMap -> Int
getTile key map = map !? key ?: 0
setTile :: Pos -> Int -> VentMap -> VentMap
setTile = insert
updateTile :: Pos -> (Int -> Int) -> VentMap -> VentMap
updateTile key f map = setTile key (f $ getTile key map) map

------------
-- Part 1 --
------------

solve1 :: [VentLine] -> Int
solve1 vents = overlapCount  where
    filtered = filter horizontalVertical vents
    horizontalVertical (VentLine (Pos (x1, y1)) (Pos (x2, y2)))
        | x1 == x2  = True
        | y1 == y2  = True
        | otherwise = False
    ventMap      = createVentMap filtered
    overlapCount = length $ filter (> 1) (elems ventMap)

createVentMap :: [VentLine] -> VentMap
createVentMap = foldl' insertLine mempty  where
    insertLine ventMap line = foldl' updatePos ventMap (getPosList line)
    updatePos ventMap pos = updateTile pos (+ 1) ventMap

-- Assumption: only horizonal, vertical, diagonal
getPosList :: VentLine -> [Pos]
getPosList (VentLine p1 p2) = takeWhileOneMore (/= p2) $ iterate (+ Pos (diffX, diffY)) p1  where
    sgn a b = case compare a b of
        EQ -> 0
        LT -> 1
        GT -> -1
    Pos (x1, y1) = p1
    Pos (x2, y2) = p2
    diffX        = sgn x1 x2
    diffY        = sgn y1 y2

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x : ys else [x]) []

------------
-- Part 2 --
------------

solve2 :: [VentLine] -> Int
solve2 vents = overlapCount  where
    ventMap      = createVentMap vents
    overlapCount = length $ filter (> 1) (elems ventMap)

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    vents <- parseVentLine <<$>> readFileLines "inputs/Y2021/Day05.txt" :: IO [VentLine]
    print $ solve1 vents
    print $ solve2 vents

number :: Parser Int
number = readInt . toText <$> many1 digit

parserVentLine :: Parser VentLine
parserVentLine = VentLine <$> parserPos <*> (string " -> " >> parserPos)
  where
    parserPos :: Parser Pos
    parserPos = do
        x <- number
        char ','
        y <- number
        return $ Pos (x, y)

parseVentLine :: Text -> VentLine
parseVentLine text = fromRight (error "parse error") $ parse parserVentLine "" text
