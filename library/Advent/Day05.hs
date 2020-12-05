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
findSeat = bimap (search min 0 127) (search min 0 7)

missingSeat :: [Int] -> Maybe Int
missingSeat = headMay . mapMaybe (uncurry skipped) . pairs . sort
 where
  pairs xs = zip xs $ drop 1 xs
  skipped before after = do
    guard $ after - before == 2
    pure $ before + 1

search :: (Int -> Int -> Int) -> Int -> Int -> [Half] -> Int
search pick lo hi = \case
  [] -> pick lo hi
  Lo : xs -> search min lo (hi - half lo hi) xs
  Hi : xs -> search max (lo + half lo hi) hi xs

half :: Int -> Int -> Int
half lo hi = (hi - lo + 1) `div` 2

toSeatId :: Int -> Int -> Int
toSeatId row col = 8 * row + col

type Pass = ([Half], [Half])
data Half = Hi | Lo
