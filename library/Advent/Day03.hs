module Advent.Day03
  ( main
  )
where

import Advent.Prelude

import Advent.Vec2 (Vec2(Vec2))
import qualified Advent.Vec2 as Vec2
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.IO as Text
import Lens.Micro ((%~), (&), (^.))

main :: Part -> IO ()
main part = do
  graph <- parse <$> Text.getContents
  print $ product $ count '#' graph <$> slopes
 where
  slopes = case part of
    Part1 -> [Vec2 3 1]
    Part2 -> [Vec2 1 1, Vec2 3 1, Vec2 5 1, Vec2 7 1, Vec2 1 2]

parse :: Text -> Graph
parse text = HashMap.fromList $ do
  (y, line) <- zip [0 ..] $ lines text
  (x, tile) <- zip [0 ..] $ unpack line
  pure (Vec2 x y, tile)

count :: Char -> Graph -> Slope -> Int
count item graph slope = loop 0 0
 where
  modulus = 1 + maximum ((^. Vec2.x) <$> HashMap.keys graph)
  loop pos !acc = maybe
    acc
    (loop (move pos slope modulus) . (acc +) . bool 0 1 . (item ==))
    (HashMap.lookup pos graph)

move :: Pos -> Slope -> Int -> Pos
move pos slope modulus = (pos + slope) & Vec2.x %~ (`mod` modulus)

type Graph = HashMap (Vec2 Int) Char
type Pos = Vec2 Int
type Slope = Vec2 Int
