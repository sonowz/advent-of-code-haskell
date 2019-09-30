module Lib.IO where

import ClassyPrelude
import Lib.Exception (libEx_)

readLines :: String -> IO [Text]
readLines filename = map clean . lines <$> readFileUtf8 filename where
    -- Remove CR(Carriage Return) character
    clean = filter (/= '\r')
readInts    = readWords_ :: Text -> [Int]
readDoubles = readWords_ :: Text -> [Double]
readInt     = fromMaybe libEx_ . readMay :: Text -> Int
readDouble  = fromMaybe libEx_ . readMay :: Text -> Double

printLines :: (MonoFoldable mono, Show (Element mono)) => mono -> IO ()
printLines = mapM_ print
printStrs = mapM_ putStrLn :: [Text] -> IO ()
printList = putStrLn . unwords . map tshow :: [Int64] -> IO ()

-- Private Functions --

readWords_ :: forall a . Read a => Text -> [a]
readWords_ = map (fromMaybe libEx_ . readMay) . words
