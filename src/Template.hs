module Template where

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


------------
-- Part 1 --
------------

solve1 :: a -> Int
solve1 = undefined

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
    template <- parseTemplate <<$>> readFileLines "inputs/Template.txt" :: IO [Int]
    print $ solve1 template
    -- print $ solve2 template

parseTemplate :: Text -> Int
parseTemplate = undefined
