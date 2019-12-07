module Lib.NonEmpty where

import Relude
import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE
import Lib.Exception (libExText_)

nfilter :: (a -> Bool) -> NonEmpty a -> Maybe (NonEmpty a)
nfilter f = nonEmpty . foldr (\x l -> if f x then x : l else l) []

ntake :: Int -> NonEmpty a -> NonEmpty a
ntake 0 _ = libExText_ "ntake: must be nonzero"
ntake n l = fromJust . nonEmpty $ NE.take n l
