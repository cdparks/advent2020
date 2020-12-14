module Advent.Day14
  ( main
  ) where


import Advent.Prelude hiding (mask)

import Advent.Bits hiding (bit)
import Advent.Parse
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid (Endo(..))
import Lens.Micro (Lens', lens)
import Lens.Micro.Mtl (use, (%=), (.=))

main :: Part -> IO ()
main part = print . run step =<< parseOrDie parseProgram
 where
  step = case part of
    Part1 -> step1
    Part2 -> step2

run :: (Instruction -> State Machine ()) -> Program -> Word64
run step = sum . _mem . flip execState start . traverse_ step
  where start = Machine mempty $ Mask 0 0 0 []

step1 :: MonadState Machine m => Instruction -> m ()
step1 = \case
  Set m -> mask .= m
  Write addr i -> do
    Mask { on, off } <- use mask
    mem %= HashMap.insert addr (i .&. off .|. on)

step2 :: MonadState Machine m => Instruction -> m ()
step2 = \case
  Set m -> mask .= m
  Write addr i -> do
    Mask { on, floating } <- use mask
    let
      masked = addr .|. on
      pairs = HashMap.fromList $ do
        fs <- replicateM (length floating) [setBit, clearBit]
        let f = foldMap Endo $ zipWith flip fs floating
        pure (appEndo f masked, i)
    mem %= HashMap.union pairs

parseProgram :: Parser Program
parseProgram = (parseWrite <|> parseMask) `sepBy1` endOfLine

parseWrite :: Parser Instruction
parseWrite = Write <$> (sym "mem[" *> decimal <* sym "] =") <*> decimal

parseMask :: Parser Instruction
parseMask = Set . toMask <$> (sym "mask =" *> takeWhile1 bitOrX)
  where bitOrX = (`elem` ("01X" :: String))

-- brittany-disable-next-binding

toMask :: Text -> Mask
toMask = foldl' step (Mask 0 0 35 []) . unpack
 where
  step Mask {..} c = Mask
    { on  = on  `shiftL` 1 .|. bool 0 1 (c == '1')
    , off = off `shiftL` 1 .|. bool 0 1 (c /= '0')
    , bit = bit - 1
    , floating = floating <> [bit | c == 'X']
    }

type Program = [Instruction]

data Mask = Mask
  { on :: Word64
  , off :: Word64
  , bit :: Int
  , floating :: [Int]
  }
  deriving stock (Eq, Show)

data Instruction
  = Set Mask
  | Write Word64 Word64
  deriving stock (Eq, Show)

data Machine = Machine
  { _mem :: HashMap Word64 Word64
  , _mask :: Mask
  }

mem :: Lens' Machine (HashMap Word64 Word64)
mem = lens _mem $ \s m -> s { _mem = m }

mask :: Lens' Machine Mask
mask = lens _mask $ \s m -> s { _mask = m }
