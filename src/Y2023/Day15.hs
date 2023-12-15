module Y2023.Day15 (main') where

import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lib.IO
import Lib.Parser qualified as P
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple

-----------------------
-- Type declarations --
-----------------------

type InitSequence = [InitStep]

newtype InitStep = InitStep Text deriving (Eq)

data InitStepCommand = InsertLens Lens | RemoveLens Label

data Lens = Lens
  { label :: Label,
    focalLength :: Int
  }
  deriving (Show, Eq)

newtype Label = Label Text deriving (Show, Eq) via Text

type LensBox = [Lens]

newtype HASHMAP = HASHMAP (Vector LensBox) deriving (Show)

initHASHMAP :: HASHMAP
initHASHMAP = HASHMAP $ V.replicate 256 []

------------
-- Part 1 --
------------

solve1 :: InitSequence -> Int
solve1 = sum . fmap (hashWithSalt 0)

instance Hashable InitStep where
  hashWithSalt :: Int -> InitStep -> Int
  hashWithSalt initValue (InitStep step) = doHASH initValue step

doHASH :: Int -> Text -> Int
doHASH initValue t = foldl' doHASHStep initValue (toString t)
  where
    doHASHStep value c = 17 * (value + ord c) `mod` 256

------------
-- Part 2 --
------------

solve2 :: InitSequence -> Int
solve2 initSequence = getFocusingPower initializedHASHMAP
  where
    initCommands = initStepToCommand <$> initSequence
    initializedHASHMAP = flipfoldl' runStep initHASHMAP initCommands

instance Hashable Label where
  hashWithSalt :: Int -> Label -> Int
  hashWithSalt initValue (Label l) = doHASH initValue l

initStepToCommand :: InitStep -> InitStepCommand
initStepToCommand (InitStep step) = fromMaybe (error "Invalid init step") $ maybeInsert <|> maybeRemove
  where
    maybeRemove = if T.isSuffixOf "-" step then Just (RemoveLens (Label (T.init step))) else Nothing
    maybeInsert =
      if T.any (== '=') step
        then let (label, value) = T.breakOn "=" step in Just (InsertLens (Lens (Label label) (readInt $ T.tail value)))
        else Nothing

updateHASHMAPBox :: Label -> (LensBox -> LensBox) -> HASHMAP -> HASHMAP
updateHASHMAPBox label updateFn (HASHMAP hashmap) = HASHMAP $ hashmap V.// [(hashIndex, updateFn box)]
  where
    hashIndex = hashWithSalt 0 label
    box = hashmap V.! hashIndex

runStep :: InitStepCommand -> HASHMAP -> HASHMAP
runStep (InsertLens lens) = updateHASHMAPBox (label lens) (runInsertLens lens)
  where
    runInsertLens :: Lens -> LensBox -> LensBox
    runInsertLens lens box =
      if any (isLabelled (label lens)) box
        then fmap (\l -> if isLabelled (label lens) l then lens else l) box
        else box <> [lens]
runStep (RemoveLens label) = updateHASHMAPBox label (runRemoveLens label)
  where
    runRemoveLens :: Label -> LensBox -> LensBox
    runRemoveLens label = filter (not . isLabelled label)

getFocusingPower :: HASHMAP -> Int
getFocusingPower (HASHMAP v) = sum . zipWith (*) [1 ..] . fmap getFocusingPowerBox . toList $ v
  where
    getFocusingPowerBox :: LensBox -> Int
    getFocusingPowerBox = sum . zipWith (*) [1 ..] . fmap focalLength

isLabelled :: Label -> Lens -> Bool
isLabelled label (Lens l _) = l == label

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  initSequence <- parseInitSequence <$> readFileLines "inputs/Y2023/Day15.txt" :: IO InitSequence
  print $ solve1 initSequence
  print $ solve2 initSequence

parseInitSequence :: [Text] -> InitSequence
parseInitSequence [line] = InitStep <$> T.splitOn "," line
