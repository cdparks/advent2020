module Advent.Prelude
  ( module X
  , Part(..)
  , die
  , forAccum
  , getProgName
  , lookupEnv
  , print
  , readCommaSep
  , readMaybe
  , readOrDie
  , tshow
  , unwrap
  ) where

import Prelude as X hiding
  ( appendFile
  , getContents
  , getContents
  , getLine
  , getLine
  , head
  , init
  , interact
  , last
  , lines
  , print
  , putStr
  , putStrLn
  , putStrLn
  , readFile
  , tail
  , takeWhile
  , unlines
  , unwords
  , words
  , writeFile
  , (!!)
  )

import Control.Applicative as X
import Control.Arrow as X ((&&&))
import Control.Monad as X
import Control.Monad.IO.Unlift as X
import Control.Monad.State.Strict as X
import Data.Bifunctor as X
import Data.Bool as X (bool)
import Data.Char as X
import Data.Either as X
import Data.Either.Extra as X (mapLeft)
import Data.Foldable as X
import Data.Hashable as X
import Data.HashMap.Monoidal as X (MonoidalHashMap)
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet as X (HashSet)
import Data.IntMap.Strict as X (IntMap)
import Data.List as X (sort, uncons)
import Data.List.Extra as X (maximumOn, minimumOn, nubOrd)
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Map.Strict as X (Map)
import Data.Maybe as X
import Data.Proxy as X (Proxy(..))
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.Text as X (Text, lines, pack, split, splitOn, unlines, unpack, unwords, words)
import Data.Text.IO as X
import Data.Time.Clock as X
import Data.Traversable as X
import Data.Vector as X (Vector)
import Data.Void as X (Void)
import Debug.Trace as X
import GHC.Generics as X (Generic)
import Numeric.Natural as X
import Safe as X
import qualified System.Environment as Env
import qualified System.Exit as Exit
import System.FilePath as X
import qualified Text.Read as Read
import UnliftIO.Directory as X
import UnliftIO.Exception as X
import UnliftIO.IORef as X

data Part = Part1 | Part2

-- | foldM with the functional argument last
forAccum :: (Foldable t, Monad m) => s -> t a -> (s -> a -> m s) -> m s
forAccum s t f = foldM f s t

-- | Unwrap a 'Maybe' or crash with message
unwrap :: forall a . Text -> Maybe a -> IO a
unwrap message = maybe (die message) pure

-- | 'Show' to 'Text'
tshow :: forall a . Show a => a -> Text
tshow = pack . show

-- | Print using 'tshow'
print :: forall a . Show a => a -> IO ()
print = putStrLn . tshow

-- | 'Read' 'Text' to 'Maybe'
readMaybe :: forall a . Read a => Text -> Maybe a
readMaybe = Read.readMaybe . unpack

-- | 'Read' or crash with message
readOrDie :: forall a . Read a => Text -> Text -> IO a
readOrDie message = unwrap message . readMaybe

-- | Parse comma-separate data from 'Text'
readCommaSep :: Read a => Text -> [a]
readCommaSep = mapMaybe readMaybe . split (== ',')

-- | Lookup environment variable
lookupEnv :: Text -> IO (Maybe Text)
lookupEnv = fmap (fmap pack) . Env.lookupEnv . unpack

-- | Get program name
getProgName :: IO Text
getProgName = pack <$> Env.getProgName

-- | Crash with message
die :: Text -> IO a
die = Exit.die . unpack
