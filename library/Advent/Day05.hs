module Advent.Day05
  ( main
  ) where

import Advent.Prelude

import Advent.Bits (Word8, shift, (.|.))
import Advent.Tuple (both, pairs)

main :: Part -> IO ()
main part = do
  seatId <- pick . fmap (uncurry toId . toSeat) . lines <$> getContents
  maybe (die "no seat found") print seatId
 where
  pick = case part of
    Part1 -> maximumMay
    Part2 -> missingSeat

missingSeat :: [Int] -> Maybe Int
missingSeat = headMay . mapMaybe (uncurry skipped) . pairs . sort
 where
  skipped before after = do
    guard $ after - before == 2
    pure $ before + 1

toId :: Int -> Int -> Int
toId row col = row * 8 + col

-- brittany-disable-next-binding

toSeat :: Text -> (Int, Int)
toSeat = both toInt . splitAt 7 . unpack
 where
  toInt = fromBits . toBits

toBits :: String -> [Word8]
toBits = fmap $ \case
  'R' -> 1
  'B' -> 1
  _ -> 0

-- brittany-disable-next-binding

fromBits :: [Word8] -> Int
fromBits = foldl' step 0
 where
  step !n bit = shift n 1 .|. fromIntegral bit
