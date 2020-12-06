module Advent.Bits
  ( bitor
  , bitand
  , module X
  ) where

import Advent.Prelude

import Data.Bits as X
import Data.Word as X

bitor :: (Bits a, Foldable f) => f a -> a
bitor = foldl' (.|.) zeroBits

bitand :: (Bits a, Foldable f) => f a -> a
bitand = foldl' (.&.) $ complement zeroBits
