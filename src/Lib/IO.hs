module Lib.IO where

import Relude
import Relude.Extra.Map ((!?))
import Relude.Extra.Tuple (mapBoth)
import Data.List (minimum, maximum)
import Data.Map (assocs)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Lib.Exception (libExEither_, libEx_)
import Lib.Vector2D (Pos2D, to2DTuple, from2DTuple)

readFileLines :: String -> IO [Text]
readFileLines filename = lines <$> readFileText filename -- where
    -- Remove CR(Carriage Return) character
    -- clean = filter (/= '\r')
readInts = readWords_ @Int :: Text -> NonEmpty Int
readIntegers = readWords_ @Integer :: Text -> NonEmpty Integer
readDoubles = readWords_ @Double :: Text -> NonEmpty Double
readInt = libExEither_ . readEither :: Text -> Int
readInteger = libExEither_ . readEither :: Text -> Integer
readDouble = libExEither_ . readEither :: Text -> Double

printLines :: Show a => [a] -> IO ()
printLines = mapM_ print
printStrs = mapM_ print :: [Text] -> IO ()

showGrid :: Show a => Vector (Vector a) -> String
showGrid grid = intercalate "\n" . map (intercalate "" . map show) $ grid'
    where grid' = V.toList $ V.toList <$> grid
showMap :: (Ord pos, Pos2D pos, Show a) => Map pos a -> String -> String
showMap m _default = intercalate "\n" . map (intercalate "") $ list  where
    xyKeys       = (unzip . map (to2DTuple . fst) $ assocs m) :: ([Int], [Int])
    (minX, minY) = minimum `mapBoth` xyKeys
    (maxX, maxY) = maximum `mapBoth` xyKeys
    getS pos = maybe _default show (m !? pos)
    list = [ [ getS (from2DTuple (x, y)) | x <- [minX .. maxX] ] | y <- [minY .. maxY] ]


-- Private Functions --

readWords_ :: forall  a . Read a => Text -> NonEmpty a
readWords_ = fromMaybe libEx_ . nonEmpty . map (libExEither_ . readEither) . words
