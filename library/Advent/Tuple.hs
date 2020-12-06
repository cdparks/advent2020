module Advent.Tuple
  ( both
  , dup
  , swap
  , pairs
  ) where

import Advent.Prelude

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f

dup :: a -> (a, a)
dup a = (a, a)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ drop 1 xs
