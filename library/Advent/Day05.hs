{-# LANGUAGE StrictData #-}

module Advent.Day05
  ( main
  ) where

import Advent.Prelude

main :: Part -> IO ()
main part = do
  passes <- toPasses <$> getContents
  let seatId = pick $ uncurry toSeatId . findSeat <$> passes
  maybe (die "no seat found") print seatId
 where
  pick = case part of
    Part1 -> maximumMay
    Part2 -> missingSeat

toPasses :: Text -> [Pass]
toPasses = fmap (bimap parse parse . splitAt 7 . unpack) . lines
 where
  parse = mapMaybe $ \case
    'F' -> pure Lo
    'B' -> pure Hi
    'L' -> pure Lo
    'R' -> pure Hi
    _ -> Nothing

findSeat :: Pass -> (Int, Int)
findSeat = bimap (search 127) (search 7)

missingSeat :: [Int] -> Maybe Int
missingSeat = headMay . mapMaybe (uncurry skipped) . pairs . sort
 where
  pairs xs = zip xs $ drop 1 xs
  skipped before after = do
    guard $ after - before == 2
    pure $ before + 1

search :: Int -> [Half] -> Int
search x = done . foldl' step (start x)
 where
  start = State min 0
  done State {..} = pick lo hi
  step !State {..} = \case
    Lo -> State min lo (hi - half lo hi)
    Hi -> State max (lo + half lo hi) hi

half :: Int -> Int -> Int
half lo hi = (hi - lo + 1) `div` 2

toSeatId :: Int -> Int -> Int
toSeatId row col = 8 * row + col

type Pass = ([Half], [Half])

data Half = Hi | Lo

data State = State
  { pick :: Int -> Int -> Int
  , lo :: Int
  , hi :: Int
  }
