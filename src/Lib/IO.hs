module Lib.IO
  ( readFileLines,
    readInts,
    readIntegers,
    readDoubles,
    readInt,
    readInteger,
    readDouble,
    readGrid,
    readMap,
    printLines,
    printStrs,
    showGrid,
    showMap,
  )
where

import Data.List (maximum, minimum)
import Data.Map (assocs)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lib.Exception (libExEither_, libEx_)
import Lib.Vector2D (Pos2D, from2DTuple, to2DTuple)
import Relude
import Relude.Extra.Bifunctor (bimapBoth)
import Relude.Extra.Map ((!?))

readFileLines :: String -> IO [Text]
readFileLines filename = lines . decodeUtf8 <$> readFileBS filename -- where
-- Remove CR(Carriage Return) character
-- clean = filter (/= '\r')

readInts = readWords_ @Int :: Text -> NonEmpty Int

readIntegers = readWords_ @Integer :: Text -> NonEmpty Integer

readDoubles = readWords_ @Double :: Text -> NonEmpty Double

readInt = libExEither_ . readEither . toString :: Text -> Int

readInteger = libExEither_ . readEither . toString :: Text -> Integer

readDouble = libExEither_ . readEither . toString :: Text -> Double

readGrid :: (Char -> a) -> [Text] -> Vector (Vector a)
readGrid parser lines = V.fromList (V.fromList . map parser . toString <$> lines)

readMap :: Pos2D p => [Int] -> [Int] -> (Char -> a) -> [Text] -> Map p a
readMap xIndices yIndices parser lines = fromList $ do
  (y, line) <- zip yIndices lines
  (x, c) <- zip xIndices (toString line)
  return (from2DTuple (x, y), parser c)

printLines :: Show a => [a] -> IO ()
printLines = mapM_ print

printStrs = mapM_ print :: [Text] -> IO ()

showGrid :: Show a => Vector (Vector a) -> String
showGrid grid = intercalate "\n" . map (intercalate "" . map show) $ grid'
  where
    grid' = V.toList $ V.toList <$> grid

showMap :: (Ord pos, Pos2D pos, Show a) => Map pos a -> String -> String
showMap m _default = intercalate "\n" . map (intercalate "") $ list
  where
    xyKeys = (unzip . map (to2DTuple . fst) $ assocs m) :: ([Int], [Int])
    (minX, minY) = minimum `bimapBoth` xyKeys
    (maxX, maxY) = maximum `bimapBoth` xyKeys
    getS pos = maybe _default show (m !? pos)
    list = [[getS (from2DTuple (x, y)) | x <- [minX .. maxX]] | y <- [minY .. maxY]]

-- Private Functions --

readWords_ :: forall a. Read a => Text -> NonEmpty a
readWords_ = fromMaybe libEx_ . nonEmpty . map (libExEither_ . readEither . toString) . words
