module Template where

import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.CallStack
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Lib.IO

-----------------------
-- Type declarations --
-----------------------

newtype InputType = InputType Int deriving (Read) via Int
newtype OutputType = OutputType Int deriving (Show) via Int

------------
-- Part 1 --
------------

solve1 :: [InputType] -> OutputType
solve1 = undefined

------------
-- Part 2 --
------------

solve2 :: [InputType] -> OutputType
solve2 = undefined

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    templates <- parseTemplate <<$>> readFileLines "inputs/Template.txt" :: IO [InputType]
    print $ solve1 templates
    -- print $ solve2 template

parseTemplate :: Text -> InputType
parseTemplate = undefined
