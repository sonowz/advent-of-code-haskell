module Y2019.Day08 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import qualified Text.Show as S
import Lib.IO
import Lib.Types

-----------------------
-- Type declarations --
-----------------------

type LayerSize = (Int, Int)

fixedLayerSize :: LayerSize
fixedLayerSize = (25, 6)

data SpaceImage = SpaceImage [Int] LayerSize deriving (Show)
newtype Image = Image (Vector ImageLayer)
newtype ImageLayer = ImageLayer (Vector (Vector Pixel))
data Pixel = Transparent | Black | White deriving (Eq)

intToPixel 0 = Black
intToPixel 1 = White
intToPixel 2 = Transparent

------------
-- Part 1 --
------------

-- Note : pixel colors were revealed in Part 2
solve1 :: SpaceImage -> Int
solve1 spaceImage = pixelCount White fewestZeroLayer * pixelCount Transparent fewestZeroLayer  where
    Image imageLayers = decodeSpaceImage spaceImage
    pixelCount px (ImageLayer layer) = V.length $ V.filter (== px) (join layer :: Vector Pixel)
    fewestZeroLayer = V.minimumBy (compare `on` pixelCount Black) imageLayers

decodeSpaceImage :: SpaceImage -> Image
decodeSpaceImage (SpaceImage rawImage (w, h)) = Image imageLayers  where
    layerSizeInt   = w * h :: Int
    rawImageLayers = chunksOf layerSizeInt rawImage
    imageLayers    = V.fromList $ map decodeImageLayer rawImageLayers
    decodeImageLayer rawLayer =
        ImageLayer . V.fromList $ V.fromList <$> (intToPixel <<$>> chunksOf w rawLayer)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n l  = let (chunk, others) = splitAt n l in chunk : chunksOf n others

------------
-- Part 2 --
------------

solve2 :: SpaceImage -> Text
solve2 = show . decodeSpaceImage

-- Define monoidal laws to merge layers
-- These monoidal multiplications are not commutative!

instance Semigroup Pixel where
    Transparent <> px = px
    Black       <> _  = Black
    White       <> _  = White
instance Semigroup ImageLayer where
    (ImageLayer up) <> (ImageLayer down) = ImageLayer $ V.zipWith (V.zipWith (<>)) up down

instance Monoid Pixel where
    mempty = Transparent
instance Monoid ImageLayer where
    mempty =
        ImageLayer $ V.replicate (snd fixedLayerSize) $ V.replicate (fst fixedLayerSize) Transparent

-- Define 'Show' instances

instance S.Show Pixel where
    show Transparent = "//"
    show Black       = "██"
    show White       = "  "
instance S.Show ImageLayer where
    show (ImageLayer pixels) = intercalate "\n" . map (intercalate "" . map show) $ pixels'
        where pixels' = V.toList $ V.toList <$> pixels
instance S.Show Image where
    show (Image layers) = show (fold layers :: ImageLayer)

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    spaceImage <- parseSpaceImage <$> readFile "inputs/Y2019/Day08.txt" :: IO SpaceImage
    print $ solve1 spaceImage
    putText $ solve2 spaceImage

parseSpaceImage :: String -> SpaceImage
parseSpaceImage str = SpaceImage (parseSpaceImage' str) fixedLayerSize  where
    parseSpaceImage' :: String -> [Int]
    parseSpaceImage' (c : cs) = readInt [c] : parseSpaceImage' cs
    parseSpaceImage' _        = []
