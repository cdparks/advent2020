module Advent.Prelude
  ( module X
  , unwrap
  , Part(..)
  ) where

import Prelude as X hiding (getContents, head, init, last, lines, tail, takeWhile, unlines, unwords, words, (!!))

import Control.Applicative as X
import Control.Monad as X
import Control.Monad.IO.Unlift as X
import Control.Monad.State.Strict as X
import Data.Bifunctor as X
import Data.Bool as X (bool)
import Data.Char as X
import Data.Either as X
import Data.Foldable as X
import Data.Hashable as X
import Data.HashMap.Monoidal as X (MonoidalHashMap)
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet as X (HashSet)
import Data.List as X (sort)
import Data.List.Extra as X (nubOrd)
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Map.Strict as X (Map)
import Data.Maybe as X
import Data.Proxy as X (Proxy(..))
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.Text as X (Text, lines, pack, unlines, unpack, unwords, words)
import Data.Text.IO as X (getContents)
import Data.Time.Clock as X
import Data.Traversable as X
import Data.Vector as X (Vector)
import Data.Void as X (Void)
import Debug.Trace as X
import GHC.Generics as X (Generic)
import Numeric.Natural as X
import Safe as X
import System.Environment as X
import System.Exit as X
import System.FilePath as X
import System.IO as X hiding (getContents)
import Text.Read as X (readMaybe)
import UnliftIO.Directory as X
import UnliftIO.Exception as X
import UnliftIO.IORef as X

unwrap :: String -> Maybe a -> IO a
unwrap message = maybe (die message) pure

data Part = Part1 | Part2
