module Advent.Parse
  ( parseOrDie
  , parseAll
  , token
  , sym
  , twoNewlines
  , oneSpace
  , module X
  ) where

import Advent.Prelude

import Data.Attoparsec.Text as X

parseOrDie :: Parser a -> IO a
parseOrDie parser = either die pure =<< parseAll parser <$> getContents

parseAll :: Parser a -> Text -> Either String a
parseAll parser = parseOnly $ skipSpace *> parser <* skipSpace <* endOfInput

token :: Parser a -> Parser a
token parser = parser <* skipSpace

sym :: Text -> Parser ()
sym = void . token . string

oneSpace :: Parser ()
oneSpace = void (char ' ') <|> endOfLine

twoNewlines :: Parser ()
twoNewlines = endOfLine *> endOfLine
