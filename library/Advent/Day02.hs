{-# LANGUAGE StrictData #-}

module Advent.Day02
  ( main
  )
where

import Advent.Prelude

import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, letter, parseOnly, skipSpace)
import Data.Bits (xor)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: Part -> IO ()
main part = do
  result <- parseInput <$> Text.getContents
  either die (print . length . filter (uncurry check)) result
 where
  check = case part of
    Part1 -> checkOccurrences
    Part2 -> checkPositions

checkOccurrences :: Policy -> Text -> Bool
checkOccurrences Policy {..} password = lo <= n && n <= hi
  where n = Text.count el password

checkPositions :: Policy -> Text -> Bool
checkPositions Policy {..} password = xor (pick lo == el) (pick hi == el)
  where pick = Text.singleton . Text.index password . pred

parseInput :: Text -> Either String [(Policy, Text)]
parseInput = parseOnly $ skipSpace *> many parseLine <* endOfInput

parseLine :: Parser (Policy, Text)
parseLine = (,) <$> parsePolicy <*> parsePassword

parsePolicy :: Parser Policy
parsePolicy = do
  (lo, hi) <- token $ (,) <$> decimal <* char '-' <*> decimal
  el <- token $ Text.singleton <$> letter <* char ':'
  pure Policy { lo, hi, el }

parsePassword :: Parser Text
parsePassword = token $ pack <$> some letter

token :: Parser a -> Parser a
token parser = parser <* skipSpace

data Policy = Policy
  { lo :: Int
  , hi :: Int
  , el :: Text
  }
