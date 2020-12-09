module Advent.Set
  ( HasSet(..)
  , insert
  , delete
  , member
  ) where

import Advent.Prelude

import qualified Data.HashSet as HashSet
import Lens.Micro
import Lens.Micro.Mtl

class HasSet s k | s -> k where
  setLens :: Lens' s (HashSet k)

instance HasSet (HashSet k) k where
  setLens = id

insert :: (MonadState s m, HasSet s k, Hashable k, Eq k) => k -> m ()
insert key = setLens %= HashSet.insert key

delete :: (MonadState s m, HasSet s k, Hashable k, Eq k) => k -> m ()
delete key = setLens %= HashSet.delete key

member :: (MonadState s m, HasSet s k, Hashable k, Eq k) => k -> m Bool
member key = HashSet.member key <$> use setLens
