module Lib.Vector2D
  ( Pos2D (..),
    at,
    unsafeAt,
    set,
    imap2D,
    imapMaybeL2D,
    size2D,
  )
where

import Data.Vector (Vector, (!), (!?), (//))
import Data.Vector qualified as V
import Lib.Exception (libExText_)
import Relude

class Ord pos => Pos2D pos where
  to2DTuple :: pos -> (Int, Int)
  from2DTuple :: (Int, Int) -> pos

instance Pos2D (Int, Int) where
  to2DTuple = id
  from2DTuple = id

at :: Pos2D pos => Vector (Vector a) -> pos -> Maybe a
at v (to2DTuple -> (x, y)) = v !? x >>= flip (!?) y

unsafeAt :: Pos2D pos => Vector (Vector a) -> pos -> a
unsafeAt v (to2DTuple -> (x, y)) = v ! x ! y

set :: Pos2D pos => Vector (Vector a) -> pos -> a -> Vector (Vector a)
set v (to2DTuple -> (x, y)) a = v // [(x, v ! x // [(y, a)])]

imap2D :: Pos2D pos => (pos -> a -> b) -> Vector (Vector a) -> Vector (Vector b)
imap2D f = V.imap (\x -> V.imap (\y t -> f (from2DTuple (x, y)) t))

imapMaybeL2D :: Pos2D pos => (pos -> a -> Maybe b) -> Vector (Vector a) -> [b]
imapMaybeL2D f v = catMaybes . concat . V.toList $ V.toList <$> imap2D f v

size2D :: Vector (Vector a) -> (Int, Int)
size2D v = (length v, length $ V.head v)
