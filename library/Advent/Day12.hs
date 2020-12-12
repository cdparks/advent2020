module Advent.Day12
  ( main
  ) where

import Advent.Prelude hiding (Left, Right)

import Advent.Vec2 (Vec2(..))
import qualified Advent.Vec2 as Vec2
import Lens.Micro (Lens', lens)
import Lens.Micro.Mtl (use, (%=), (+=))

main :: Part -> IO ()
main part = print . sum . Vec2.scalars . abs . go . parse =<< getContents
 where
  go = case part of
    Part1 -> follow (Vec2 1 0) pos
    Part2 -> follow (Vec2 10 1) heading

parse :: Text -> [Instruction]
parse = mapMaybe parseLine . lines
 where
  parseLine line = do
    (c, rest) <- uncons $ unpack line
    n <- readMaybe rest
    case c of
      'F' -> pure $ Head n
      'N' -> pure $ Move $ Vec2.north n
      'S' -> pure $ Move $ Vec2.south n
      'E' -> pure $ Move $ Vec2.east n
      'W' -> pure $ Move $ Vec2.west n
      'L' -> pure $ Left $ n `div` 90
      'R' -> pure $ Right $ n `div` 90
      _ -> Nothing

follow :: Vec2 Int -> Lens' Ship (Vec2 Int) -> [Instruction] -> Vec2 Int
follow heading0 target = _pos . flip execState ship . traverse_ step
 where
  ship = Ship 0 heading0
  step = \case
    Head n -> (pos +=) . Vec2.scale n =<< use heading
    Move v -> target += v
    Left n -> heading %= applyN n Vec2.left90
    Right n -> heading %= applyN n Vec2.right90

applyN :: Int -> (a -> a) -> a -> a
applyN n f = loop n
 where
  loop i !a
    | i <= 0 = a
    | otherwise = loop (pred i) $ f a

data Ship = Ship
  { _pos :: Vec2 Int
  , _heading :: Vec2 Int
  }
  deriving stock Show

pos :: Lens' Ship (Vec2 Int)
pos = lens _pos $ \s p -> s { _pos = p }

heading :: Lens' Ship (Vec2 Int)
heading = lens _heading $ \s h -> s { _heading = h }

data Instruction
  = Head Int
  | Move (Vec2 Int)
  | Left Int
  | Right Int
  deriving stock Show
