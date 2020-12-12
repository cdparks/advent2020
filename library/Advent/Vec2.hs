{-# LANGUAGE StrictData #-}

module Advent.Vec2
  ( Vec2(..)
  , x
  , y
  , scalars
  , scale
  , right90
  , left90
  , neighbors4
  , neighbors8
  , directions4
  , directions8
  , north
  , east
  , west
  , south
  )
where

import Advent.Prelude

import Lens.Micro

data Vec2 a = Vec2
  { _x :: a
  , _y :: a
  }
  deriving stock (Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass Hashable

instance Applicative Vec2 where
  pure a = Vec2 a a
  Vec2 f g <*> Vec2 a b = Vec2 (f a) (g b)

instance Show a => Show (Vec2 a) where
  showsPrec _ Vec2 {..} =
    showChar '(' . shows _x . showString ", " . shows _y . showChar ')'

x :: Lens' (Vec2 a) a
x = lens _x $ \p v -> p { _x = v }

y :: Lens' (Vec2 a) a
y = lens _y $ \p v -> p { _y = v }

scale :: Num a => a -> Vec2 a -> Vec2 a
scale s = fmap (s *)

scalars :: Vec2 a -> [a]
scalars v = (v ^.) <$> [x, y]

right90 :: Num a => Vec2 a -> Vec2 a
right90 v = Vec2 (v ^. y) (negate $ v ^. x)

left90 :: Num a => Vec2 a -> Vec2 a
left90 v = Vec2 (negate $ v ^. y) (v ^. x)

neighbors4 :: Num a => Vec2 a -> [Vec2 a]
neighbors4 v = (+ v) <$> directions4

neighbors8 :: Num a => Vec2 a -> [Vec2 a]
neighbors8 v = (+ v) <$> directions8

directions4 :: Num a => [Vec2 a]
directions4 = [north 1, east 1, south 1, west 1]

directions8 :: Num a => [Vec2 a]
directions8 = directions4 <> zipWith (+) directions4 rotated
  where rotated = take 4 $ drop 1 $ cycle directions4

north :: Num a => a -> Vec2 a
north n = Vec2 0 n

east :: Num a => a -> Vec2 a
east n = Vec2 n 0

west :: Num a => a -> Vec2 a
west n = Vec2 (-n) 0

south :: Num a => a -> Vec2 a
south n = Vec2 0 (-n)

-- brittany-disable-next-binding

instance Num a => Num (Vec2 a) where
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
