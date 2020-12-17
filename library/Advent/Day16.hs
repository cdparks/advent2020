module Advent.Day16 where

import Advent.Prelude

import Advent.Interval (Interval)
import qualified Advent.Interval as Interval
import Advent.Parse

main :: Part -> IO ()
main part = do
  Notes {..} <- parseOrDie parseNotes
  print $ sum $ errorRate rules <$> theirs
 where
  _ = case part of
    Part1 -> ()
    Part2 -> ()

errorRate :: [Interval] -> Ticket -> Int
errorRate rules = sum . mapMaybe invalid
 where
  invalid x = x <$ guard (not $ valid x)
  valid x = any (`Interval.contains` Interval.unit x) rules

mergeRules :: [[Interval]] -> [Interval]
mergeRules = foldr merge [] . sortOn Interval.lo . concat
 where
  merge x = \case
    [] -> [x]
    y : ys -> maybe (x : y : ys) (: ys) $ Interval.combine x y

parseNotes :: Parser Notes
parseNotes = do
  rules <- mergeRules <$> many1 parseRule
  endOfLine *> sym "your ticket:"
  mine <- parseTicket
  endOfLine *> sym "nearby tickets:"
  theirs <- many1 parseTicket
  pure Notes { .. }

parseRule :: Parser [Interval]
parseRule = do
  skipWhile (/= ':')
  sym ":"
  x <- parseInterval
  sym " or"
  y <- parseInterval
  endOfLine
  pure [x, y]

parseInterval :: Parser Interval
parseInterval = Interval.new <$> decimal <* char '-' <*> decimal

parseTicket :: Parser Ticket
parseTicket = decimal `sepBy1` char ',' <* endOfLine

data Notes = Notes
  { rules :: [Interval]
  , mine :: Ticket
  , theirs :: [Ticket]
  }
  deriving stock (Show)

type Ticket = [Int]
