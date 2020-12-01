module Advent.Prelude
  ( module X
  , Part(..)
  )
where

import Prelude as X hiding (head, init, last, lines, tail, unlines, unwords, words, (!!))

import Control.Applicative as X
import Control.Monad as X
import Control.Monad.IO.Unlift as X
import Data.Either as X
import Data.Foldable as X
import Data.Hashable as X
import Data.HashMap.Monoidal as X (MonoidalHashMap)
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet as X (HashSet)
import Data.List.Extra as X (nubOrd)
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Map.Strict as X (Map)
import Data.Maybe as X
import Data.Proxy as X (Proxy(..))
import Data.Set as X (Set)
import Data.Text as X (Text, lines, pack, unlines, unpack, unwords, words)
import Data.Time.Clock as X
import Data.Traversable as X
import Data.Vector as X (Vector)
import Data.Void as X (Void)
import GHC.Generics as X (Generic)
import Numeric.Natural as X
import System.Environment as X
import System.Exit as X
import System.FilePath as X
import System.IO as X
import Text.Read as X (readMaybe)
import UnliftIO.Directory as X
import UnliftIO.Exception as X
import UnliftIO.IORef as X

data Part = Part1 | Part2
