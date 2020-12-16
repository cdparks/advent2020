{-# LANGUAGE StrictData #-}

module Advent.Interval
  ( Interval
  , new
  , lo
  , hi
  , overlaps
  , combine
  , contains
  , unit
  )
where

import Advent.Prelude hiding (combine)

data Interval = Interval
  { _lo :: Int
  , _hi :: Int
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass Hashable

instance Show Interval where
  showsPrec _ Interval {..} =
    showChar '(' . shows _lo . showString ", " . shows _hi . showChar ')'

new :: Int -> Int -> Interval
new x y
  | x <= y = Interval x y
  | otherwise = Interval y x

lo :: Interval -> Int
lo = _lo

hi :: Interval -> Int
hi = _hi

overlaps :: Interval -> Interval -> Bool
overlaps i = maybe False (const True) . combine i

combine :: Interval -> Interval -> Maybe Interval
combine (Interval lo1 hi1) (Interval lo2 hi2) = do
  guard $ max lo1 lo2 <= min hi1 hi2
  pure $ Interval (min lo1 lo2) (max hi1 hi2)

unit :: Int -> Interval
unit x = Interval x x

contains :: Interval -> Interval -> Bool
contains (Interval lo1 hi1) (Interval lo2 hi2) = lo1 <= lo2 && hi2 <= hi1
