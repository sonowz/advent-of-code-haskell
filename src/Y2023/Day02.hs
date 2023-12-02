module Y2023.Day02 (main') where

import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Text

-----------------------
-- Type declarations --
-----------------------

data GameResult = GameResult
  { gameId :: Int,
    revealSets :: [RevealSet]
  }

newtype RevealSet = RevealSet (CubeColor -> Int)

data CubeColor = Red | Green | Blue deriving (Show, Eq, Ord, Enum, Bounded)

------------
-- Part 1 --
------------

solve1 :: [GameResult] -> Int
solve1 gameResults = sum $ gameId <$> possibleGames
  where
    possibleGames = filter isPossibleGame gameResults

isPossibleGame :: GameResult -> Bool
isPossibleGame gameResult = all checkRevealSet (revealSets gameResult)
  where
    checkRevealSet :: RevealSet -> Bool
    checkRevealSet (RevealSet reveal) = reveal Red <= 12 && reveal Green <= 13 && reveal Blue <= 14

------------
-- Part 2 --
------------

solve2 :: [GameResult] -> Int
solve2 = sum . fmap calcCubePower

mergeRevealSet :: RevealSet -> RevealSet -> RevealSet
mergeRevealSet (RevealSet reveal1) (RevealSet reveal2) = RevealSet $ \color -> max (reveal1 color) (reveal2 color)

calcCubePower :: GameResult -> Int
calcCubePower gameResult = cubePower
  where
    mergedRevealSet = foldl1' mergeRevealSet (nonEmpty' $ revealSets gameResult)
    cubePower = product $ un mergedRevealSet <$> (universe :: [CubeColor])
    nonEmpty' = fromMaybe (error "empty list") . nonEmpty

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  gameResults <- parseGameResult <<$>> readFileLines "inputs/Y2023/Day02.txt" :: IO [GameResult]
  print $ solve1 gameResults
  print $ solve2 gameResults

parseGameResult :: Text -> GameResult
parseGameResult = either (error . show) id . parse parserGameResult ""

number :: Parser Int
number = readInt . toText <$> many1 digit

parserGameResult :: Parser GameResult
parserGameResult = do
  string "Game "
  gameId <- number
  string ": "
  revealSets <- sepBy1 parserRevealSet (string "; ")
  return GameResult {..}

parserRevealSet :: Parser RevealSet
parserRevealSet = do
  reveals <- fromList <$> sepBy1 parserReveal (string ", ") :: Parser (Map CubeColor Int)
  return $ RevealSet (\color -> fromMaybe 0 (reveals !? color))

parserReveal :: Parser (CubeColor, Int)
parserReveal = do
  count <- number
  space
  color <- parserColor
  return (color, count)

parserColor :: Parser CubeColor
parserColor = do
  color <- choice [string "red", string "green", string "blue"]
  return $ case color of
    "red" -> Red
    "green" -> Green
    "blue" -> Blue
    _ -> error "invalid color"
