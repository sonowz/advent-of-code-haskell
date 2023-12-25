module Y2023.Day19 (main') where

import Data.Map ((!))
import Lib.IO
import Lib.Parser qualified as P
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe qualified as Unsafe
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Text

-----------------------
-- Type declarations --
-----------------------

data Workflow = Workflow
  { name :: Text,
    rules :: [Rule],
    fallback :: WorkState
  }

data WorkState = WorkflowNamed Text | Terminal TerminalState deriving (Show)

data TerminalState = Accepted | Rejected deriving (Show, Eq)

data Rule = Rule GearCategory RuleOp Int WorkState

newtype Gear = Gear (GearCategory -> Int)

data GearCategory = ExtremelyCool | Musical | Aerodynamic | Shiny deriving (Show, Eq, Ord, Enum, Bounded)

data RuleOp = OpLT | OpGT deriving (Show)

------------
-- Part 1 --
------------

solve1 :: [Workflow] -> [Gear] -> Int
solve1 workflows gears = sum . fmap addRating $ acceptedGears
  where
    workflows' = optimizeWorkflows workflows
    acceptedGears = filter ((== Accepted) . runWorkflows workflows') gears

data OptimizedWorkflows = OptimizedWorkflows
  { initWorkflow :: Text,
    optimizedWorkflows :: Map Text (Gear -> WorkState)
  }

addRating :: Gear -> Int
addRating (Gear gear) = gear ExtremelyCool + gear Musical + gear Aerodynamic + gear Shiny

optimizeWorkflows :: [Workflow] -> OptimizedWorkflows
optimizeWorkflows workflows = OptimizedWorkflows "in" optimized
  where
    optimized = fromList $ (\workflow -> (name workflow, getNextStateFn workflow)) <$> workflows

    getNextStateFn :: Workflow -> Gear -> WorkState
    getNextStateFn (Workflow _ rules fallback) = nextStateFn
      where
        applyRule :: Rule -> Gear -> Maybe WorkState
        applyRule (Rule gearCat OpLT x nextState) (Gear gear) = if gear gearCat < x then Just nextState else Nothing
        applyRule (Rule gearCat OpGT x nextState) (Gear gear) = if gear gearCat > x then Just nextState else Nothing
        nextStateFn gear = fromMaybe fallback $ asum $ applyRule <$> rules <*> pure gear

runWorkflows :: OptimizedWorkflows -> Gear -> TerminalState
runWorkflows workflows gear = go gear (WorkflowNamed (initWorkflow workflows))
  where
    workflowMap = optimizedWorkflows workflows
    go :: Gear -> WorkState -> TerminalState
    go gear (WorkflowNamed name) = go gear ((workflowMap ! name) gear)
    go _ (Terminal state) = state

------------
-- Part 2 --
------------

solve2 :: [Workflow] -> Int
solve2 = sum . fmap gearCombinations . getAcceptedGearRanges

type GearRange = GearCategory -> Range

data Range = Range Int Int deriving (Show) -- Inclusive

initGearRange :: GearRange
initGearRange _ = Range 1 4000

splitRange :: GearCategory -> Int -> GearRange -> (Maybe GearRange, Maybe GearRange)
splitRange gearCat x gearRange = (updateRange <$> gearRange1, updateRange <$> gearRange2)
  where
    Range l r = gearRange gearCat
    gearRange1 = if x <= l then Nothing else Just $ Range l (x - 1)
    gearRange2 = if x > r then Nothing else Just $ Range x r
    updateRange range cat = if cat == gearCat then range else gearRange cat

type WorkflowStep = (WorkState, GearRange)

runWorkflowWithRange :: Workflow -> GearRange -> [WorkflowStep]
runWorkflowWithRange workflow = go (rules workflow)
  where
    applyRule :: Rule -> GearRange -> (Maybe WorkflowStep, Maybe GearRange)
    applyRule (Rule gearCat OpLT n nextState) gearRange = first (fmap (nextState,)) $ splitRange gearCat n gearRange
    applyRule (Rule gearCat OpGT n nextState) gearRange = swap . second (fmap (nextState,)) $ splitRange gearCat (n + 1) gearRange

    go :: [Rule] -> GearRange -> [WorkflowStep]
    go [] gearRange = [(fallback workflow, gearRange)]
    go (rule : rules) gearRange = case applyRule rule gearRange of
      (Nothing, Nothing) -> []
      (Nothing, Just gearRange') -> go rules gearRange'
      (Just workflowStep, Nothing) -> [workflowStep]
      (Just workflowStep, Just gearRange') -> workflowStep : go rules gearRange'

getAcceptedGearRanges :: [Workflow] -> [GearRange]
getAcceptedGearRanges workflows = go (WorkflowNamed "in", initGearRange)
  where
    workflowMap :: Map Text Workflow
    workflowMap = fromList $ (\workflow -> (name workflow, workflow)) <$> workflows

    go :: WorkflowStep -> [GearRange]
    go (Terminal Accepted, gearRange) = [gearRange]
    go (Terminal Rejected, _) = []
    go (WorkflowNamed name, gearRange) = foldMap go nextSteps
      where
        nextSteps = runWorkflowWithRange (workflowMap ! name) gearRange

gearCombinations :: GearRange -> Int
gearCombinations gearRange = product $ (\(Range l r) -> r - l + 1) . gearRange <$> universe

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  (workflows, gears) <- parseWorkflowsAndGears <$> readFileLines "inputs/Y2023/Day19.txt" :: IO ([Workflow], [Gear])
  print $ solve1 workflows gears
  print $ solve2 workflows

parseWorkflowsAndGears :: [Text] -> ([Workflow], [Gear])
parseWorkflowsAndGears lines = (workflows, gears)
  where
    (workflowLines, gearLines) = second Unsafe.tail $ span (/= "") lines
    workflows = parseWorkflow <$> workflowLines
    gears = parseGear <$> gearLines

parseWorkflow :: Text -> Workflow
parseWorkflow = either (error . show) id . parse parserWorkflow ""

parseGear :: Text -> Gear
parseGear = either (error . show) id . parse parserGear ""

parserWorkflow :: Parser Workflow
parserWorkflow = do
  name <- toText <$> many1 letter
  string "{"
  rules <- try parserRule `endBy1` string "," :: Parser [Rule]
  fallback <- parserWorkState
  string "}"
  return Workflow {..}

parserRule :: Parser Rule
parserRule = Rule <$> parserGearCategory <*> parserOp <*> P.number <* string ":" <*> parserWorkState
  where
    parserOp = (OpLT <$ string "<") <|> (OpGT <$ string ">")

parserGearCategory :: Parser GearCategory
parserGearCategory = go <$> oneOf "xmas"
  where
    go 'x' = ExtremelyCool
    go 'm' = Musical
    go 'a' = Aerodynamic
    go 's' = Shiny

parserWorkState :: Parser WorkState
parserWorkState = pAccepted <|> pRejected <|> pWorkflow
  where
    pWorkflow = WorkflowNamed . toText <$> many1 letter
    pAccepted = Terminal Accepted <$ string "A"
    pRejected = Terminal Rejected <$ string "R"

parserGear :: Parser Gear
parserGear = do
  string "{x="
  x <- P.number
  string ",m="
  m <- P.number
  string ",a="
  a <- P.number
  string ",s="
  s <- P.number
  string "}"
  return . Gear $ \case
    ExtremelyCool -> x
    Musical -> m
    Aerodynamic -> a
    Shiny -> s
