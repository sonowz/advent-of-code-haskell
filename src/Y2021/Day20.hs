module Y2021.Day20 where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Lib.IO
import Lib.Types
import Lib.Vector2D
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import qualified Relude.Unsafe as Unsafe
import qualified Text.Show as S

-----------------------
-- Type declarations --
-----------------------

data Binary = Zero | One deriving (Eq, Ord, Show)
newtype BinNumber = BN [Binary]

data Pixel = Dark | Light deriving (Eq)
data Image = Image
    { getMap        :: Map Pos Pixel
    , getBackground :: Pixel
    }
type EnhanceAlgorithm = Vector Pixel

newtype Pos = Pos (Int, Int) deriving (Pos2D, Ord, Eq, Show) via (Int, Int)

instance S.Show Pixel where
    show Dark  = " "
    show Light = "█"

instance Num Pos where
    Pos (x1, y1) + Pos (x2, y2) = Pos (x1 + x2, y1 + y2)
    Pos (x1, y1) - Pos (x2, y2) = Pos (x1 - x2, y1 - y2)
    Pos (x1, y1) * Pos (x2, y2) = Pos (x1 * x2, y1 * y2)
    abs (Pos (x, y)) = Pos (abs x, abs y)
    signum (Pos (x, y)) = Pos (signum x, signum y)
    fromInteger x = Pos (fromInteger x, fromInteger x)

getPixel :: Pos -> Image -> Pixel
getPixel key image = getMap image !? key ?: getBackground image
setPixel :: Pos -> Pixel -> Image -> Image
setPixel key value image = image { getMap = insert key value (getMap image) }

binToInt :: Num a => BinNumber -> a
binToInt num = go (un num) 0  where
    go []          acc = acc
    go [Zero     ] acc = acc
    go [One      ] acc = acc + 1
    go (Zero : bs) acc = go bs (2 * acc)
    go (One  : bs) acc = go bs (2 * (acc + 1))

pixelToBin :: Pixel -> Binary
pixelToBin Dark  = Zero
pixelToBin Light = One

-- For debugging & visualization
showMap' :: Image -> String
showMap' image = showMap (getMap image) (show $ getBackground image)


------------
-- Part 1 --
------------

solve1 :: EnhanceAlgorithm -> Image -> Int
solve1 algo image = fromMaybe (error "Infinite pixel!") $ countPixel Light enhanced
    where enhanced = enhance algo . enhance algo $ image

enhance :: EnhanceAlgorithm -> Image -> Image
enhance algo image = image { getMap = newMap, getBackground = backgroundPixel }  where
    !posSet = getNonBackgroundPos image
    updatePixel :: Map Pos Pixel -> Pos -> Map Pos Pixel
    updatePixel map pos = insert pos (algo V.! intIndex) map      where
        pixels   = flip getPixel image <$> window pos :: [Pixel]
        intIndex = binToInt . BN $ pixelToBin <$> pixels :: Int
    newMap          = foldl' updatePixel mempty posSet
    backgroundPixel = case getBackground image of
        Dark  -> algo V.! 0
        Light -> algo V.! (512 - 1)

getNonBackgroundPos :: Image -> [Pos]
getNonBackgroundPos (getMap -> image) = nubBySet $ concatMap window (keys image)  where
    -- 'nub' function is slow, so made faster version
    nubBySet :: forall a . Ord a => [a] -> [a]
    nubBySet = toList . (fromList :: [a] -> Set a)

window :: Pos -> [Pos]
window (Pos (x, y)) = [ Pos (x + dx, y + dy) | dy <- [(-1) .. 1], dx <- [(-1) .. 1] ]

countPixel :: Pixel -> Image -> Maybe Int
countPixel pixel image = if getBackground image == pixel
    then Nothing -- Infinite pixel!
    else Just $ count (== pixel) (elems $ getMap image)
    where count f = length . filter f


------------
-- Part 2 --
------------

solve2 :: EnhanceAlgorithm -> Image -> Int
solve2 algo image = fromMaybe (error "Infinite pixel!") $ countPixel Light enhanced
    where enhanced = iterate (enhance algo) image Unsafe.!! 50

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    (algorithm, image) <-
        parseImageAndAlgo <$> readFileLines "inputs/Y2021/Day20.txt" :: IO (EnhanceAlgorithm, Image)
    print $ solve1 algorithm image
    print $ solve2 algorithm image

parseImageAndAlgo :: [Text] -> (EnhanceAlgorithm, Image)
parseImageAndAlgo lines =
    (parseAlgorithm (lines Unsafe.!! 0), Image (parseImage (drop 2 lines)) Dark)  where
    parsePixel '.' = Dark
    parsePixel '#' = Light
    parsePixel _   = error "Parse error!"
    parseAlgorithm = V.fromList . fmap parsePixel . toString
    parseImage     = fromList . join . zipWith zipFn [0 ..]
    zipFn          = \j line ->
        zipWith (\i pixel -> (Pos (i, j), pixel)) [0 ..] . fmap parsePixel . toString $ line
