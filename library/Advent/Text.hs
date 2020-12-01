module Advent.Text
  ( readCommaSep
  )
where

import Advent.Prelude

import qualified Data.Text as T

readCommaSep :: Read a => Text -> [a]
readCommaSep = mapMaybe (readMaybe . T.unpack) . T.splitOn ","
