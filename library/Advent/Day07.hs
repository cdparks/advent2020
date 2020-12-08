module Advent.Day07
  ( main
  ) where

import Advent.Prelude

import Advent.Parse hiding (count)
import Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Data.Monoid (Sum(..))

main :: Part -> IO ()
main part = print . count =<< parseOrDie parseGraph
 where
  root = "shiny gold"
  count = case part of
    Part1 -> length . uniqueDescendents root . invert
    Part2 -> countDescendents root

uniqueDescendents :: Text -> Graph -> HashSet Text
uniqueDescendents = foldDescendents wrap step
 where
  wrap = HashSet.singleton
  step _ acc = acc

countDescendents :: Text -> Graph -> Int
countDescendents root = getSum . foldDescendents wrap step root
 where
  wrap = const 1
  step !n !acc = Sum n * acc

foldDescendents
  :: Monoid m => (Text -> m) -> (Int -> m -> m) -> Text -> Graph -> m
foldDescendents wrap step root graph = without root
 where
  with label = wrap label <> without label
  without label = mconcat $ do
    children <- maybeToList $ HashMap.lookup label graph
    (n, child) <- children
    pure $ step n $ with child

invert :: Graph -> Graph
invert graph = HashMap.fromListWith (<>) $ do
  (k, vs) <- HashMap.toList graph
  (_, v) <- vs
  [(v, [(1, k)]), (k, [])]

parseGraph :: Parser Graph
parseGraph = HashMap.fromList <$> many parseNode

parseNode :: Parser (Text, [(Int, Text)])
parseNode = (,) <$> bag <* sym "contain" <*> bags <* sym "."

bag :: Parser Text
bag = do
  adj1 <- word
  adj2 <- word
  sym "bags" <|> sym "bag"
  pure $ adj1 <> " " <> adj2

-- brittany-disable-next-binding

bags :: Parser [(Int, Text)]
bags = asum
  [ [] <$ sym "no other bags"
  , child `sepBy1` sym ","
  ]
 where
  child = (,) <$> token decimal <*> bag

word :: Parser Text
word = token $ takeWhile1 isAsciiLower

type Graph = HashMap Text [(Int, Text)]
