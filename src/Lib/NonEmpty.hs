module Lib.NonEmpty
  ( nfilter,
    ntake,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Lib.Exception (libExText_)
import Relude

nfilter :: (a -> Bool) -> NonEmpty a -> Maybe (NonEmpty a)
nfilter f = nonEmpty . foldr (\x l -> if f x then x : l else l) []

ntake :: Int -> NonEmpty a -> NonEmpty a
ntake 0 _ = libExText_ "ntake: must be nonzero"
ntake n l = fromJust . nonEmpty $ NE.take n l
