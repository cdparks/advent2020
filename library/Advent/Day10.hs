module Advent.Day10
  ( main
  ) where

import Advent.Prelude hiding (HashMap, HashSet)

import Advent.Parse hiding (count)
import Data.HashMap.Lazy as HashMap

main :: Part -> IO ()
main part = print . count . sort =<< parseOrDie ints
 where
  count = case part of
    Part1 -> countDiffs
    Part2 -> countPaths

countDiffs :: [Int] -> Int
countDiffs = product . HashMap.fromListWith (+) . fmap (, 1) . diffs
 where
  diffs :: [Int] -> [Int]
  diffs xs = zipWith (-) xs (0 : xs) <> [3]

countPaths :: [Int] -> Int
countPaths xs = cache ! 0
 where
  cache = HashMap.fromList $ do
    i <- 0 : xs <> [target]
    pure (i, if i == target then 1 else choose i)

  choose i = sum $ do
    j <- [1 .. 3]
    pure $ HashMap.lookupDefault 0 (i + j) cache

  target = maximum xs + 3

ints :: Parser [Int]
ints = decimal `sepBy1` endOfLine
