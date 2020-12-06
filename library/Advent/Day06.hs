module Advent.Day06
  ( main
  ) where

import Advent.Prelude hiding (fold)

import Advent.Bits
import Advent.Parse

main :: Part -> IO ()
main part = do
  groups <- parseOrDie $ parseGroups fold
  print $ sum $ popCount <$> groups
 where
  fold = case part of
    Part1 -> bitor
    Part2 -> bitand

parseGroups :: ([Word] -> Word) -> Parser [Word]
parseGroups fold = parseGroup `sepBy1` twoNewlines
  where parseGroup = fold <$> parseLine `sepBy1` oneSpace

parseLine :: Parser Word
parseLine = toWord <$> takeWhile1 isAsciiLower
 where
  toWord = foldl' setBit 0 . fmap toBit . unpack
  toBit = subtract (ord 'a') . ord
