module Lib.Exception where

import           Relude
import           Control.Exception
import           UnliftIO.Exception
import qualified Text.Show

-- Private Functions --

data LibException = LibException
instance Exception LibException
instance Show LibException where
    show LibException = "Library assertion failed. Check library modules."
newtype LibExceptionMsg = LibExceptionMsg Text
instance Exception LibExceptionMsg
instance Show LibExceptionMsg where
    show (LibExceptionMsg msg) =
        toString $ "Library assertion failed : " <> msg

libEx_ = impureThrow LibException
libExText_ t = impureThrow (LibExceptionMsg t)
libExEither_ = either libExText_ id
