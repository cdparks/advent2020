module Advent.Day01
  ( main
  ) where

import Advent.Prelude

import qualified Data.HashSet as HashSet

main :: Part -> IO ()
main part = do
  entries <- toEntries <$> getContents
  let result = listToMaybe $ sumsTo 2020 size entries
  maybe (die "no valid set") (print . product) result
 where
  size = case part of
    Part1 -> 2
    Part2 -> 3

toEntries :: Text -> HashSet Int
toEntries contents = HashSet.fromList $ do
  line <- lines contents
  maybeToList $ readMaybe line

sumsTo :: Int -> Int -> HashSet Int -> [[Int]]
sumsTo total n entries
  | n <= 0 = []
  | n == 1 = [total] <$ guard (total `HashSet.member` entries)
  | otherwise = do
    entry <- HashSet.toList entries
    (entry :) <$> sumsTo (total - entry) (pred n) (HashSet.delete entry entries)
