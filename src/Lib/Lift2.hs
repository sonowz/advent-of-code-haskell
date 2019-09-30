module Lib.Lift2 where

import ClassyPrelude

-- extract
l2ext :: (IsSequence a, IsSequence (Element a)) => (forall a1. IsSequence a1 => a1 -> Index a1 -> Element a1)
    -> a -> Index a -> Index (Element a) -> Element (Element a)
l2ext f vv x y = let v = fix1 vv x in fix2 v y where
    fix1 = f
    fix2 = f


-- mono
l2m :: Functor t => (forall a . t a -> a) -> (t (t a) -> a)
l2m f = f . f

