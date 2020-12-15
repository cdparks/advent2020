module Advent.Day15
  ( main
  ) where

import Advent.Prelude

import Control.Monad.ST (runST)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Unboxed.Mutable as V

main :: Part -> IO ()
main part = print . play n =<< parse =<< getContents
 where
  n = case part of
    Part1 -> 2020
    Part2 -> 30000000

play :: Int -> NonEmpty Int -> Int
play n xs = runST $ do
  mem <- V.new n
  traverse_ (uncurry $ V.unsafeWrite mem) pairs
  forAccum (NE.last xs) turns $ \last turn -> do
    prev <- V.unsafeRead mem last
    V.unsafeWrite mem last turn
    pure $ bool (turn - prev) 0 $ prev == 0
 where
  pairs = zip (NE.toList xs) [1 ..]
  turns = [length xs .. pred n]

parse :: Text -> IO (NonEmpty Int)
parse = unwrap "no input" . NE.nonEmpty . readCommaSep

-- Previous lazy implementation worked for Part 1, but was too slow for Part 2

{-

pick :: Int -> [Game] -> Game
pick n = List.head . dropWhile ((< n) . turn)

play :: NonEmpty Int -> [Game]
play xs = iterate step game
 where
  game = Game
    { turn = length xs
    , last = NE.last xs
    , mem = HashMap.fromList $ zip (NE.toList xs) [1 ..]
    }
  step Game {..} = do
    let n = maybe 0 (turn -) $ HashMap.lookup last mem
    Game { turn = succ turn, last = n, mem = HashMap.insert last turn mem }

data Game = Game
  { turn :: Int
  , last :: Int
  , mem :: HashMap Int Int
  }

-}
