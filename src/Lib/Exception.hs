module Lib.Exception where

import ClassyPrelude
import Control.Exception

-- Private Functions --

data LibException = LibException
instance Exception LibException
instance Show LibException where
    show LibException = "Library assertion failed. Check library modules."
libEx_ = throw LibException
