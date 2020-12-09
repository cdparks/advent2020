module Advent.Day09
  ( main
  ) where

import Advent.Prelude

import Advent.Parse
import qualified Data.HashSet as HashSet
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq

main :: Part -> IO ()
main part = do
  input <- parseOrDie ints
  invalid <- unwrap "no invalid input" $ firstInvalid 25 input
  case part of
    Part1 -> print invalid
    Part2 -> print =<< unwrap "no range" (findRange invalid input)

firstInvalid :: Int -> [Int] -> Maybe Int
firstInvalid n input = slide
  (Seq.fromList prefix)
  (HashSet.fromList prefix)
  suffix
  where ~(prefix, suffix) = splitAt n input

slide :: Seq Int -> HashSet Int -> [Int] -> Maybe Int
slide (e :<| tape) seen (x : xs)
  | any valid seen = slide nextTape nextSeen xs
  | otherwise = pure x
 where
  valid y = (x - y) `HashSet.member` HashSet.delete y seen
  nextSeen = HashSet.insert x $ HashSet.delete e seen
  nextTape = tape |> x
slide _ _ _ = Nothing

findRange :: Int -> [Int] -> Maybe Int
findRange total = loop 0 Seq.empty
 where
  loop !acc xs (y : ys) | acc < total = loop (acc + y) (xs |> y) ys
  loop !acc xs _ | acc == total, length xs > 1 = pure $ minimum xs + maximum xs
  loop !acc (x :<| xs) ys = loop (acc - x) xs ys
  loop _ _ _ = Nothing

ints :: Parser [Int]
ints = decimal `sepBy1` endOfLine
