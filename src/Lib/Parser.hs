module Lib.Parser (ne, number) where

import Lib.IO
import Relude
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Text

ne :: Parser [a] -> Parser (NonEmpty a)
ne p = p >>= flip whenNothing (fail "empty list") . nonEmpty

number :: Parser Int
number = readInt . toText <$> many1 digit
