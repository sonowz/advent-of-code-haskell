module Y2019.Day14 where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe ((!!))
import Algebra.Graph
import Algebra.Graph.ToGraph
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.Text
import Lib.IO
import Lib.Types

-----------------------
-- Type declarations --
-----------------------

type ReactionMap = Map Chemical Reaction
data Reaction = Reaction [Chemicals] Chemicals -- Inputs, output
type Chemical = Text
type Quantity = Int
type Chemicals = (Chemical, Quantity)

inputs (Reaction i o) = i
output (Reaction i o) = o

------------
-- Part 1 --
------------

type ChemicalState = Map Chemical Int

-- Calculate required ores by triggering reaction in reverse order
solve1 :: ReactionMap -> Int
solve1 reactions = lookup "ORE" calcResult ?: 0  where
    inverseOrder  = reverse $ reactionOrder reactions
    chemicalState = insert "FUEL" 1 mempty :: ChemicalState
    calcResult    = foldl'
        (\m chem -> (inverseReaction m <$> lookup chem reactions) ?: m)
        chemicalState
        inverseOrder

reactionOrder :: ReactionMap -> [Chemical]
reactionOrder (elems -> reactionList) = fromRight (error "graph has cycle") $ topSort fullGraph  where
    constructLocalGraph :: Reaction -> Graph Chemical
    constructLocalGraph (Reaction inputs' output') = ingredients `connect` product      where
        ingredients = vertices (map fst inputs') :: Graph Chemical
        product     = (vertex . fst) output' :: Graph Chemical
    fullGraph = overlays $ map constructLocalGraph reactionList

inverseReaction :: ChemicalState -> Reaction -> ChemicalState
inverseReaction chemicals (Reaction ingredients (productChem, productQuantity)) =
    (addIngredients . removeProduct) chemicals  where
    reqQuantity    = lookup productChem chemicals ?: 0 :: Quantity
    reactionAmount = ceiling (realToFrac reqQuantity / realToFrac productQuantity) :: Int
    addIngredients :: ChemicalState -> ChemicalState
    addIngredients chems = foldl'
        (\m (chem, quantity) -> insertWith (+) chem (reactionAmount * quantity) m)
        chems
        ingredients
    removeProduct :: ChemicalState -> ChemicalState
    removeProduct = insertWith (+) productChem (reactionAmount * productQuantity)

------------
-- Part 2 --
------------

solve2 :: ReactionMap -> Int
solve2 reactions = fuelOverloadQuantity - 1  where
    inverseOrder = reverse $ reactionOrder reactions
    searchFunction :: Quantity -> Bool
    searchFunction fuelQuantity = oreQuantity > oneTrillion      where
        chemicalState = insert "FUEL" fuelQuantity mempty :: ChemicalState
        calcResult =
            foldl'
                (\m chem -> (inverseReaction m <$> lookup chem reactions) ?: m)
                chemicalState
                inverseOrder :: ChemicalState
        oreQuantity = lookup "ORE" calcResult ?: 0
    fuelOverloadQuantity = binarySearch searchFunction 0 oneTrillion

-- Find least number which satisfies the predicate
binarySearch :: (Show a, Integral a, Eq a) => (a -> Bool) -> a -> a -> a
binarySearch f start end
    | start == end   = start
    | f mid == True  = binarySearch f start mid
    | f mid == False = binarySearch f (mid + 1) end
    where mid = (start + end) `div` 2

oneTrillion :: Quantity
oneTrillion = 1000000000000

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    reactionList <- parseReaction <<$>> readFileLines "inputs/Y2019/Day14.txt" :: IO [Reaction]
    let reactions = buildReactionMap reactionList :: ReactionMap
    print $ solve1 reactions
    print $ solve2 reactions

buildReactionMap :: [Reaction] -> ReactionMap
buildReactionMap = foldl' (\m r -> insert (fst $ output r) r m) mempty

parserReaction :: Parser Reaction
parserReaction = do
    inputs <- parserChemicals `sepBy1` char ','
    string " => "
    output <- parserChemicals
    return (Reaction inputs output)
  where
    parserChemicals :: Parser Chemicals
    parserChemicals = do
        spaces
        quantity <- readInt . toText <$> many1 digit
        spaces
        chemical <- toText <$> many1 letter
        return (chemical, quantity)

parseReaction text = fromRight (error "parse error") $ parse parserReaction "" text
