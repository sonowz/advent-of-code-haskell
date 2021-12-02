module Y2021.Day02 where

import Lib.IO
import Lib.Types
import Lib.Vector2D
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple

-----------------------
-- Type declarations --
-----------------------

data Command = CForward Int | CUp Int | CDown Int
newtype Pos = Pos (Int, Int) deriving (Pos2D, Ord, Eq, Show) via (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Pos2D, Ord, Eq, Num) via Pos
newtype Aim = Aim Int deriving (Ord, Eq, Num) via Int

instance Num Pos where
    Pos (x1, y1) + Pos (x2, y2) = Pos (x1 + x2, y1 + y2)
    Pos (x1, y1) - Pos (x2, y2) = Pos (x1 - x2, y1 - y2)
    Pos (x1, y1) * Pos (x2, y2) = Pos (x1 * x2, y1 * y2)
    abs (Pos (x, y)) = Pos (abs x, abs y)
    signum (Pos (x, y)) = Pos (signum x, signum y)
    fromInteger x = Pos (fromInteger x, fromInteger x)

------------
-- Part 1 --
------------

solve1 :: [Command] -> Int
solve1 commands = posToAnswer $ moveByDirs (Pos (0, 0)) dirs
    where dirs = commandToDir <$> commands

posToAnswer :: Pos -> Int
posToAnswer (Pos (x, y)) = x * y

-- Note that down is positive
commandToDir :: Command -> Dir
commandToDir (CForward n) = Dir (n, 0)
commandToDir (CUp      n) = Dir (0, -n)
commandToDir (CDown    n) = Dir (0, n)

moveByDirs :: Pos -> [Dir] -> Pos
moveByDirs = foldl' (\pos dir -> pos + coerce dir)

------------
-- Part 2 --
------------

solve2 :: [Command] -> Int
solve2 = posToAnswer . moveByCommands (Pos (0, 0), Aim 0)

moveByCommands :: (Pos, Aim) -> [Command] -> Pos
moveByCommands posAim = fst . foldl' move posAim where
    move (Pos (x, y), Aim a) (CForward n) = (Pos (x + n, y + a * n), Aim a)
    move (p         , Aim a) (CUp      n) = (p, Aim (a - n))
    move (p         , Aim a) (CDown    n) = (p, Aim (a + n))

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    commands <- parseCommand <<$>> readFileLines "inputs/Y2021/Day02.txt" :: IO [Command]
    print $ solve1 commands
    print $ solve2 commands

replace :: Char -> Char -> String -> String
replace a b = map (\c -> if c == a then b else c)

parseCommand :: Text -> Command
parseCommand line = case words line of
    ["forward", n] -> CForward (readInt n)
    ["up"     , n] -> CUp (readInt n)
    ["down"   , n] -> CDown (readInt n)
    _              -> error "unexpected command"
