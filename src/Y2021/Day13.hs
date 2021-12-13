module Y2021.Day13 where

import Data.List (maximum, minimum)
import qualified Data.Set as S
import Lib.IO
import Lib.Types
import Lib.Vector2D
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

type Paper = Set Pos
data FoldInstruction = FoldX Int | FoldY Int
newtype Pos = Pos (Int, Int) deriving (Pos2D, Ord, Eq, Show) via (Int, Int)

------------
-- Part 1 --
------------

solve1 :: Paper -> FoldInstruction -> Int
solve1 paper = length . foldPaper paper

-- If 'FoldX 100', then domain of x becomes (-inf, 100)
foldPaper :: Paper -> FoldInstruction -> Paper
foldPaper paper (FoldX foldX) =
    S.map (\(Pos (x, y)) -> if x > foldX then Pos (2 * foldX - x, y) else Pos (x, y)) paper
foldPaper paper (FoldY foldY) =
    S.map (\(Pos (x, y)) -> if y > foldY then Pos (x, 2 * foldY - y) else Pos (x, y)) paper

------------
-- Part 2 --
------------

solve2 :: Paper -> [FoldInstruction] -> Paper
solve2 = foldl' foldPaper

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    (paper, instruction) <-
        parseManual <$> readFileLines "inputs/Y2021/Day13.txt" :: IO (Paper, [FoldInstruction])
    let firstInstruction : _ = instruction
    print $ solve1 paper firstInstruction
    putStrLn $ showSet (solve2 paper instruction) "⬛" "  "

parseManual :: [Text] -> (Paper, [FoldInstruction])
parseManual text = fromRight (error "parse error") $ parse parserManual "" (unlines text)

number :: Parser Int
number = readInt . toText <$> many1 digit

parserManual :: Parser (Paper, [FoldInstruction])
parserManual = do
    posList <- parserPos `sepEndBy1` space :: Parser [Pos]
    void newline -- newline separator
    instructions <- parserInstruction `sepEndBy1` space
    return (fromList posList, instructions)

parserPos :: Parser Pos
parserPos = do
    x <- number
    char ','
    y <- number
    return $ Pos (x, y)

parserInstruction :: Parser FoldInstruction
parserInstruction = string "fold along " >> (foldX <|> foldY)  where
    foldX = string "x=" >> FoldX <$> number
    foldY = string "y=" >> FoldY <$> number

showSet :: (Ord pos, Pos2D pos) => Set pos -> String -> String -> String
showSet m pointStr nonPointStr = intercalate "\n" . map (intercalate "") $ list  where
    xyKeys       = (unzip . map to2DTuple $ toList m) :: ([Int], [Int])
    (minX, minY) = minimum `bimapBoth` xyKeys
    (maxX, maxY) = maximum `bimapBoth` xyKeys
    getS pos = if member pos m then pointStr else nonPointStr
    list = [ [ getS (from2DTuple (x, y)) | x <- [minX .. maxX] ] | y <- [minY .. maxY] ]
