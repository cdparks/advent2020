module Advent.Parse
  ( parseOrDie
  , token
  , twoNewlines
  , oneSpace
  , module X
  ) where

import Advent.Prelude

import Data.Attoparsec.Text as X

parseOrDie :: Parser a -> IO a
parseOrDie parser = do
  result <- run <$> getContents
  either die pure result
  where run = parseOnly $ skipSpace *> parser <* skipSpace <* endOfInput

token :: Parser a -> Parser a
token parser = parser <* skipSpace

oneSpace :: Parser ()
oneSpace = void (char ' ') <|> endOfLine

twoNewlines :: Parser ()
twoNewlines = endOfLine *> endOfLine
