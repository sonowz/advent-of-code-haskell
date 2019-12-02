module Lib.Types where

import Relude
import qualified Data.Vector as V

-- Note: keep this file minimal!

instance One (V.Vector a) where
    type OneItem (V.Vector a) = a
    one = V.singleton

