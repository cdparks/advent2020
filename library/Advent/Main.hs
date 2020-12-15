module Advent.Main
  ( main
  ) where

import Advent.Prelude

import qualified Advent.Day01 as Day01
import qualified Advent.Day02 as Day02
import qualified Advent.Day03 as Day03
import qualified Advent.Day04 as Day04
import qualified Advent.Day05 as Day05
import qualified Advent.Day06 as Day06
import qualified Advent.Day07 as Day07
import qualified Advent.Day08 as Day08
import qualified Advent.Day09 as Day09
import qualified Advent.Day10 as Day10
import qualified Advent.Day11 as Day11
import qualified Advent.Day12 as Day12
import qualified Advent.Day13 as Day13
import qualified Advent.Day14 as Day14
import qualified Advent.Day15 as Day15

main :: IO ()
main = do
  day <- parseDay
  part <- parsePart
  case day of
    1 -> Day01.main part
    2 -> Day02.main part
    3 -> Day03.main part
    4 -> Day04.main part
    5 -> Day05.main part
    6 -> Day06.main part
    7 -> Day07.main part
    8 -> Day08.main part
    9 -> Day09.main part
    10 -> Day10.main part
    11 -> Day11.main part
    12 -> Day12.main part
    13 -> Day13.main part
    14 -> Day14.main part
    15 -> Day15.main part
    _
      | day <= 25 -> usage "Day not implemented yet"
      | otherwise -> usage "Day out of range"

parseDay :: IO Natural
parseDay = do
  day <- expect "Must specify day" =<< lookupEnv "DAY"
  expect "Could not parse day" $ readMaybe day

parsePart :: IO Part
parsePart = do
  mPart <- lookupEnv "PART"
  case mPart of
    Nothing -> pure Part1
    Just part -> expect "Could not parse part" $ do
      n <- readMaybe @Natural part
      Part1 <$ (guard $ n == 1) <|> Part2 <$ (guard $ n == 2)

usage :: Text -> IO a
usage message = do
  name <- getProgName
  die $ unlines
    [ message
    , ""
    , "usage: DAY=N [PART=P] " <> name
    , "  where 1 <= N <= 25"
    , "  where 1 <= P <= 2"
    ]

expect :: Text -> Maybe a -> IO a
expect message = maybe (usage message) pure
