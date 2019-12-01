module Lib.NonEmpty where

import Relude

nfilter :: (a -> Bool) -> NonEmpty a -> Maybe (NonEmpty a)
nfilter f = nonEmpty . foldr (\x l -> if f x then x : l else l) []
