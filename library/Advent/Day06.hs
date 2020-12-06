module Advent.Day06
  ( main
  ) where

import Advent.Prelude

import Advent.Bits
import Advent.Parse

main :: Part -> IO ()
main part = do
  groups <- parseOrDie parseGroups
  print $ sum $ ones <$> groups
 where
  ones = popCount . case part of
    Part1 -> bitor
    Part2 -> bitand

parseGroups :: Parser [[Word]]
parseGroups = parseGroup `sepBy1` twoNewlines
  where parseGroup = parseLine `sepBy1` oneSpace

parseLine :: Parser Word
parseLine = toWord <$> takeWhile1 isAsciiLower
 where
  toWord = foldl' setBit 0 . fmap toBit . unpack
  toBit = subtract (ord 'a') . ord
