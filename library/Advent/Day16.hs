module Advent.Day16
  ( main
  ) where

import Advent.Prelude

import Advent.Interval (Interval)
import qualified Advent.Interval as Interval
import Advent.Parse
import qualified Data.HashSet as HashSet
import Data.IntervalMap.FingerTree (IntervalMap)
import qualified Data.IntervalMap.FingerTree as IntervalMap
import Data.List ((!!))
import qualified Data.Text as Text

main :: Part -> IO ()
main part = do
  Notes {..} <- parseOrDie parseNotes
  case part of
    Part1 -> print $ sum $ errorRate rules <$> theirs
    Part2 -> print $ product $ (mine !!) <$> solve rules theirs

errorRate :: [Rule] -> Ticket -> Int
errorRate rules = sum . mapMaybe invalid
 where
  invalid x = x <$ guard (not $ valid x)
  valid x = any (`Interval.contains` Interval.unit x) intervals
  intervals = concatMap snd rules

solve :: [Rule] -> [Ticket] -> [Int]
solve rules =
  mapMaybe (uncurry departureIndex)
    . reconcile
    . fmap (candidates intervals)
    . transpose
 where
  intervals = toIntervalMap $ do
    (name, rs) <- rules
    (, name) <$> rs
  departureIndex index key = index <$ guard ("departure" `Text.isPrefixOf` key)

candidates :: IntervalMap Int Text -> [Int] -> HashSet Text
candidates intervals = intersections . filter ((> 0) . length) . fmap
  (HashSet.fromList . fmap snd . (`IntervalMap.search` intervals))

reconcile :: [HashSet Text] -> [(Int, Text)]
reconcile orig = pick sorted
 where
  sorted = sortOn (length . snd) $ zip [0 ..] orig
  one = listToMaybe . HashSet.toList
  pick :: [(Int, HashSet Text)] -> [(Int, Text)]
  pick = \case
    [] -> []
    (i, x) : xs
      | length x == 1, Just key <- one x -> (i, key)
      : pick (fmap (HashSet.delete key) <$> xs)
      | otherwise -> error "no solution"

intersections :: (Eq a, Hashable a) => [HashSet a] -> HashSet a
intersections = \case
  [] -> HashSet.empty
  x : xs -> foldr HashSet.intersection x xs

toIntervalMap :: [(Interval, v)] -> IntervalMap Int v
toIntervalMap = foldr add IntervalMap.empty
  where add (k, v) m = IntervalMap.insert (Interval.convert k) v m

parseNotes :: Parser Notes
parseNotes = do
  rules <- many1 parseRule
  endOfLine *> sym "your ticket:"
  mine <- parseTicket
  endOfLine *> sym "nearby tickets:"
  theirs <- many1 parseTicket
  pure Notes { .. }

parseRule :: Parser Rule
parseRule = do
  name <- takeWhile1 (/= ':')
  sym ":"
  x <- parseInterval
  sym " or"
  y <- parseInterval
  endOfLine
  pure (name, [x, y])

parseInterval :: Parser Interval
parseInterval = Interval.new <$> decimal <* char '-' <*> decimal

parseTicket :: Parser Ticket
parseTicket = decimal `sepBy1` char ',' <* endOfLine

data Notes = Notes
  { rules :: [Rule]
  , mine :: Ticket
  , theirs :: [Ticket]
  }
  deriving stock Show

type Ticket = [Int]
type Rule = (Text, [Interval])
