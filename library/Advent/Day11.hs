module Advent.Day11
  ( main
  ) where

import Advent.Prelude

import Advent.Vec2 (Vec2(..))
import qualified Advent.Vec2 as Vec2
import qualified Data.HashMap.Strict as HashMap

main :: Part -> IO ()
main part = print . occupied . evolve step . parse =<< getContents
 where
  step = case part of
    Part1 -> stepUsing Adjacent 4
    Part2 -> stepUsing Infinite 5
  occupied = length . filter (== Full) . toList

parse :: Text -> Space
parse text = HashMap.fromList $ do
  (y, line) <- zip [0 ..] $ lines text
  (x, c) <- zip [0 ..] $ unpack line
  t <- asum
    [ Full <$ guard (c == '#')
    , Empty <$ guard (c == 'L')
    , Floor <$ guard (c == '.')
    ]
  pure (Vec2 x y, t)

evolve :: (a -> Maybe a) -> a -> a
evolve f z = loop z
 where
  loop !prev = case f prev of
    Nothing -> prev
    Just x -> loop x

stepUsing :: Vision -> Int -> Space -> Maybe Space
stepUsing vision threshold space = flip evalState False $ do
  newSpace <- HashMap.traverseWithKey stepTile space
  bool Nothing (Just newSpace) <$> get
 where
  stepTile p = \case
    Empty
      | neighbors vision space p == 0 -> Full <$ put True
      | otherwise -> pure Empty
    Full
      | neighbors vision space p >= threshold -> Empty <$ put True
      | otherwise -> pure Full
    Floor -> pure Floor

neighbors :: Vision -> Space -> Vec2 Int -> Int
neighbors vision space p0 = length $ do
  dxy <- Vec2.directions8
  guard $ seesFull dxy $ p0 + dxy
 where
  seesFull !dxy !p = case HashMap.lookup p space of
    Just Full -> True
    Just Empty -> False
    Just Floor | vision == Infinite -> seesFull dxy $ p + dxy
    _ -> False

type Space = HashMap (Vec2 Int) Tile

data Vision = Adjacent | Infinite
  deriving stock Eq

data Tile = Floor | Empty | Full
  deriving stock Eq
