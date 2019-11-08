module Lib.IO where

import           Relude
import           Lib.Exception                  ( libExEither_
                                                , libEx_
                                                )

readLines :: String -> IO [Text]
readLines filename = lines <$> readFileText filename -- where
    -- Remove CR(Carriage Return) character
    -- clean = filter (/= '\r')
readInts = readWords_ @Int :: Text -> NonEmpty Int
readDoubles = readWords_ @Double :: Text -> NonEmpty Double
readInt = libExEither_ . readEither :: Text -> Int
readDouble = libExEither_ . readEither :: Text -> Double

printLines :: Show a => [a] -> IO ()
printLines = mapM_ print
printStrs = mapM_ print :: [Text] -> IO ()

-- Private Functions --

readWords_ :: forall a . Read a => Text -> NonEmpty a
readWords_ =
    fromMaybe libEx_ . nonEmpty . map (libExEither_ . readEither) . words
