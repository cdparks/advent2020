module Advent.Day05 where

import Advent.Prelude

import Data.Bits (shift, (.|.))
import Data.Word (Word8)

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
  pairs xs = zip xs $ drop 1 xs
  skipped before after = do
    guard $ after - before == 2
    pure $ before + 1

toId :: Int -> Int -> Int
toId row col = row * 8 + col

toSeat :: Text -> (Int, Int)
toSeat = bimap toInt toInt . splitAt 7 . unpack
  where toInt = fromBits . toBits

toBits :: String -> [Word8]
toBits = fmap $ \case
  'R' -> 1
  'B' -> 1
  _ -> 0

fromBits :: [Word8] -> Int
fromBits = foldl' step 0 where step !n bit = shift n 1 .|. fromIntegral bit
