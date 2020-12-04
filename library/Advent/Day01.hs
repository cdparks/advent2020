module Advent.Day01
  ( main
  )
where

import Advent.Prelude

import qualified Data.HashSet as HashSet

main :: Part -> IO ()
main part = do
  entries <- toEntries <$> getContents
  let result = listToMaybe $ findEntries 2020 entries
  maybe (die "no valid set") (print . product) result
 where
  findEntries = case part of
    Part1 -> pairSumsTo
    Part2 -> tripleSumsTo

toEntries :: Text -> HashSet Int
toEntries contents = HashSet.fromList $ do
  line <- lines contents
  maybeToList $ readMaybe @Int $ unpack line

pairSumsTo :: Int -> HashSet Int -> [[Int]]
pairSumsTo total entries = do
  entry <- HashSet.toList entries
  let other = total - entry
  guard $ other `HashSet.member` entries
  pure [entry, other]

tripleSumsTo :: Int -> HashSet Int -> [[Int]]
tripleSumsTo total entries = do
  entry <- HashSet.toList entries
  (entry :) <$> pairSumsTo (total - entry) (HashSet.delete entry entries)
