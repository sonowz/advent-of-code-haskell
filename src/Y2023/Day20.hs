module Y2023.Day20 (main') where

import Data.List (findIndex)
import Data.Map.Strict ((!))
import Data.Sequence (Seq (Empty, (:<|)), (|>))
import Data.Text qualified as T
import Lib.IO
import Relude
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Tuple
import Relude.Unsafe qualified as Unsafe

-----------------------
-- Type declarations --
-----------------------

data Module = FlipFlop Text | Conjunction Text | Rx | Broadcast | Button deriving (Show, Eq, Ord)

type ModuleConfiguration = [(Module, NonEmpty Text)]

data ModuleGraph = ModuleGraph
  { getInputModules :: Module -> [Module],
    getOutputModules :: Module -> [Module]
  }

data Pulse = HighPulse | LowPulse deriving (Show, Eq, Ord)

data PulseMessage = PulseMessage
  { origin :: Module,
    pulse :: Pulse
  }
  deriving (Show)

data ModuleState = FlipFlopState Pulse | ConjunctionState (Map Module Pulse) | NoState deriving (Show, Eq)

type ModuleStates = Map Module ModuleState

initModuleStates :: ModuleGraph -> [Module] -> ModuleStates
initModuleStates graph modules = fromList $ (\m -> (m, getInitState m)) <$> modules
  where
    getInitState m@(Conjunction _) = ConjunctionState . fromList $ (,LowPulse) <$> getInputModules graph m
    getInitState (FlipFlop _) = FlipFlopState LowPulse
    getInitState _ = NoState

-- State monad for counting pulses
data PulseCountM a = PulseCountM
  { getHighPulseCount :: Int,
    getLowPulseCount :: Int,
    getValue :: a
  }
  deriving (Show, Eq, Ord, Functor)

instance Applicative PulseCountM where
  pure = PulseCountM 0 0
  (PulseCountM high1 low1 f) <*> (PulseCountM high2 low2 a) =
    PulseCountM (high1 + high2) (low1 + low2) (f a)

instance Monad PulseCountM where
  (PulseCountM high1 low1 a) >>= f = PulseCountM (high1 + high2) (low1 + low2) b
    where
      (PulseCountM high2 low2 b) = f a

incrHighPulseCount :: Int -> PulseCountM ()
incrHighPulseCount n = PulseCountM n 0 ()

incrLowPulseCount :: Int -> PulseCountM ()
incrLowPulseCount n = PulseCountM 0 n ()

------------
-- Part 1 --
------------

solve1 :: ModuleConfiguration -> Int
solve1 moduleConfig = lowPulseCount * highPulseCount
  where
    (moduleGraph, initState) = constructModuleGraph moduleConfig
    states = iterate (\s -> s >>= pushButton moduleGraph) (pure initState)
    lowPulseCount = getLowPulseCount (states Unsafe.!! 1000)
    highPulseCount = getHighPulseCount (states Unsafe.!! 1000)

constructModuleGraph :: ModuleConfiguration -> (ModuleGraph, ModuleStates)
constructModuleGraph moduleConfig = (moduleGraph, initModuleStates moduleGraph (elems moduleMap))
  where
    moduleMap :: Map Text Module
    moduleMap = insertExtras . fromList $ mapMaybe (extractName . fst) moduleConfig
      where
        extractName (FlipFlop name) = Just (name, FlipFlop name)
        extractName (Conjunction name) = Just (name, Conjunction name)
        extractName _ = Nothing
        insertExtras m =
          flipfoldl'
            (uncurry insert)
            m
            ( [ ("broadcaster", Broadcast),
                ("button", Button),
                ("rx", Rx),
                ("output", Rx) -- Used in the example
              ] ::
                [(Text, Module)]
            )
    moduleConfig' = (Button, one "broadcaster") : moduleConfig

    graph :: (Map Module [Module], Map Module [Module])
    graph = foldl' insertLine (mempty, mempty) moduleConfig'
    insertLine (incomingMap, outgoingMap) (srcModule, destModuleStrs) = (incomingMap', outgoingMap')
      where
        destModules = fmap (moduleMap !) destModuleStrs
        incomingMap' = flipfoldl' (\dest -> insertWith (<>) dest [srcModule]) incomingMap destModules
        outgoingMap' = insert srcModule (toList destModules) outgoingMap

    moduleGraph =
      ModuleGraph
        { getInputModules = \m -> fst graph !? m ?: [],
          getOutputModules = \m -> snd graph !? m ?: []
        }

pushButton :: ModuleGraph -> ModuleStates -> PulseCountM ModuleStates
pushButton graph = go initQueue
  where
    initQueue = one (Button, PulseMessage Button LowPulse)
    go :: Seq (Module, PulseMessage) -> ModuleStates -> PulseCountM ModuleStates
    go Empty moduleStates = pure moduleStates
    go ((m, input) :<| queue) moduleStates = doLoop
      where
        moduleState = moduleStates ! m
        moduleProcessResult = processModule m moduleState input

        doLoop = case moduleProcessResult of
          Nothing -> go queue moduleStates
          Just (moduleState', output) -> do
            let outputMessage = PulseMessage m output
                nextModules = getOutputModules graph m
                queue' = foldl' (\q nextModule -> q |> (nextModule, outputMessage)) queue nextModules
                moduleStates' = insert m moduleState' moduleStates
                n = length nextModules
            case (m, output) of
              (Rx, _) -> go queue moduleStates'
              (_, HighPulse) -> incrHighPulseCount n >> go queue' moduleStates'
              (_, LowPulse) -> incrLowPulseCount n >> go queue' moduleStates'

processFlipFlop :: Pulse -> Pulse -> Maybe Pulse
processFlipFlop moduleState input = case (moduleState, input) of
  (_, HighPulse) -> Nothing
  (LowPulse, LowPulse) -> Just HighPulse
  (HighPulse, LowPulse) -> Just LowPulse

processConjunction :: Map Module Pulse -> PulseMessage -> (Map Module Pulse, Pulse)
processConjunction moduleState input = (moduleState', output)
  where
    moduleState' = insert (origin input) (pulse input) moduleState
    output = if all (== HighPulse) (elems moduleState') then LowPulse else HighPulse

processModule :: Module -> ModuleState -> PulseMessage -> Maybe (ModuleState, Pulse)
processModule (FlipFlop _) (FlipFlopState s) (pulse -> input) =
  (\x -> (FlipFlopState x, x)) <$> processFlipFlop s input
processModule (Conjunction _) (ConjunctionState s) pulseMessage =
  Just . first ConjunctionState $ processConjunction s pulseMessage
processModule Broadcast NoState (pulse -> input) = Just (NoState, input)
processModule Rx NoState (pulse -> input) = Just (NoState, input)
processModule Button NoState _ = Just (NoState, LowPulse)
processModule _ _ _ = error "Invalid module parameters"

------------
-- Part 2 --
------------

-- Assumption about input data:
-- - Rx only receives input from only one module, and it is a Conjunction
-- - This Conjunction module only receives one input from each "cluster"
-- - A "cluster" is a strongly connected component of modules, and the Broadcast module sends initial pulse to each "cluster"
-- Therefore, Rx outputs low pulse iff all "cluster"s output high pulse at the same time
solve2 :: ModuleConfiguration -> Int
solve2 moduleConfig = foldl1' lcm cycles
  where
    (moduleGraph, initState) = constructModuleGraph moduleConfig
    modules = getClusterOutputModules moduleGraph
    cycles = getModuleHighPulseCycle moduleGraph initState <$> modules

-- Counts pulse of a specific module
pushButton2 :: ModuleGraph -> Module -> ModuleStates -> PulseCountM ModuleStates
pushButton2 graph countModule = go initQueue
  where
    initQueue = one (Button, PulseMessage Button LowPulse)
    go :: Seq (Module, PulseMessage) -> ModuleStates -> PulseCountM ModuleStates
    go Empty moduleStates = pure moduleStates
    go ((m, input) :<| queue) moduleStates = doLoop
      where
        moduleState = moduleStates ! m
        moduleProcessResult = processModule m moduleState input

        doLoop = case moduleProcessResult of
          Nothing -> go queue moduleStates
          Just (moduleState', output) -> do
            let outputMessage = PulseMessage m output
                nextModules = getOutputModules graph m
                queue' = foldl' (\q nextModule -> q |> (nextModule, outputMessage)) queue nextModules
                moduleStates' = insert m moduleState' moduleStates
                n = length nextModules
            case (m, output) of
              (Rx, _) -> go queue moduleStates'
              (_, HighPulse) | m == countModule -> incrHighPulseCount n >> go queue' moduleStates'
              (_, LowPulse) | m == countModule -> incrLowPulseCount n >> go queue' moduleStates'
              _ -> go queue' moduleStates'

-- Assumption: high pulse is sent every N presses, without initial offset
getModuleHighPulseCycle :: ModuleGraph -> ModuleStates -> Module -> Int
getModuleHighPulseCycle graph initState m = findIndex ((==) 1 . snd) steps ?: error "Infinite loop"
  where
    steps = iterate iterateFn (initState, 0) :: [(ModuleStates, Int)]
    iterateFn = bimap getValue getHighPulseCount . dup . pushButton2 graph m . fst

getClusterOutputModules :: ModuleGraph -> NonEmpty Module
getClusterOutputModules graph = nonEmpty' $ getInputModules graph lastConjunctionModule
  where
    [lastConjunctionModule] = getInputModules graph Rx
    nonEmpty' = fromMaybe (error "Empty cluster output module") . nonEmpty

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  moduleConfig <- parseModuleLine <<$>> readFileLines "inputs/Y2023/Day20.txt" :: IO ModuleConfiguration
  print $ solve1 moduleConfig
  print $ solve2 moduleConfig

parseModuleLine :: Text -> (Module, NonEmpty Text)
parseModuleLine line = (srcModule, destModules)
  where
    srcModuleStr : "->" : destModuleStrs = words line
    destModules = nonEmpty' $ fmap (T.dropWhileEnd (== ',')) destModuleStrs
    srcModule = parseSourceModule srcModuleStr

    nonEmpty' = fromMaybe (error "Empty list") . nonEmpty

parseSourceModule :: Text -> Module
parseSourceModule str = case T.splitAt 1 str of
  ("%", name) -> FlipFlop name
  ("&", name) -> Conjunction name
  ("b", "roadcaster") -> Broadcast
  _ -> error "Invalid source module"
