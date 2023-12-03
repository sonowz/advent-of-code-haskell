module Lib.Types where

import Data.Vector qualified as V
import Relude

-- Note: keep this file minimal!

instance One (V.Vector a) where
  type OneItem (V.Vector a) = a
  one = V.singleton
