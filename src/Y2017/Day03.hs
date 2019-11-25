module Y2017.Day03 where

import           Relude
import           Relude.Extra.Bifunctor
import           Relude.Extra.CallStack
import           Relude.Extra.Foldable1
import           Relude.Extra.Map
import           Relude.Extra.Newtype
import           Relude.Extra.Tuple
import           Lib.IO

-----------------------
-- Type declarations --
-----------------------

type Pos = (Int, Int)

------------
-- Part 1 --
------------

spiralMemory :: HashMap Int Pos
spiralMemory = fromList $ iterate nextKeyValue (1, (0, 0)) where
    nextKeyValue (i, (x, y)) = undefined

solve1 :: Int -> Int
solve1 n = let (x, y) = spiralMemory !? n ?: (0, 0) in x + y

------------
-- Part 2 --
------------

solve2 :: a -> Int
solve2 = undefined

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    template <- readInt <$> readFileText "inputs/Y2017/Day03.txt" :: IO Int
    print $ solve1 template
    -- print $ solve2 template

parseTemplate :: Text -> Int
parseTemplate = undefined
