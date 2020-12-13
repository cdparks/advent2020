module Advent.Day13
  ( main
  ) where

import Advent.Prelude

import qualified Data.Text as Text

main :: Part -> IO ()
main part = do
  ts <- readOrDie "no timestamp" =<< getLine
  busIds <- parse <$> getLine
  print $ f ts busIds
 where
  f ts busIds = case part of
    Part1 -> uncurry (*) $ minimumOn fst $ (after ts &&& id) . snd <$> busIds
    Part2 -> crt (snd <$> busIds) (uncurry subtract <$> busIds)

parse :: Text -> [(Int, Int)]
parse = mapMaybe toInt . zip [0 ..] . Text.split (== ',')
 where
  toInt (t, s) = do
    guard $ s /= "x"
    i <- readMaybe s
    pure (t, i)

after :: Int -> Int -> Int
after timestamp n = n - timestamp `mod` n

-- Everything below transliterated from several languages on Rosetta
-- Code because number theory hurts my brain

-- Modular inverse for coprime integers:
--   modInv a m = x such that a * x == 1 mod m
modInv :: Int -> Int -> Maybe Int
modInv a m
  | coprime && ax < 0 = pure $ ax + m
  | coprime = pure ax
  | otherwise = Nothing
 where
  (ax, _, g) = egcd a m
  coprime = 1 == g

-- Extended gcd:
--   egcd a b = (x, y, g) such that ax + by = g
egcd :: Int -> Int -> (Int, Int, Int)
egcd a b
  | b <= 0 = (1, 0, a)
  | otherwise = (by, ax - q * by, g)
 where
  (q, r) = a `divMod` b
  (ax, by, g) = egcd b r

-- Chinese remainder theorem
crt :: [Int] -> [Int] -> Int
crt ns as = (`mod` p) $ sum $ do
  (n, a) <- zip ns as
  let d = p `div` n
  i <- maybeToList $ modInv d n
  pure $ i * a * d
  where p = product ns
