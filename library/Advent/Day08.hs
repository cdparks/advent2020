{-# LANGUAGE StrictData #-}

module Advent.Day08
  ( main
  ) where

import Advent.Prelude

import Advent.Parse hiding (Result)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

main :: Part -> IO ()
main part = do
  program <- parseOrDie parseProgram
  print . headNote "no result" $ case part of
    Part1 -> looped $ run program
    Part2 -> do
      (i, instruction) <- HashMap.toList program
      halted $ run $ HashMap.insert i (swap instruction) program

parseProgram :: Parser Program
parseProgram = HashMap.fromList . zip [0 ..] <$> many parseInstruction
 where
  parseInstruction = do
    instruction <- asum [Nop <$ sym "nop", Acc <$ sym "acc", Jmp <$ sym "jmp"]
    instruction <$> token (signed decimal)

run :: HashMap Int Instruction -> Result
run program = loop $ Machine 0 0 HashSet.empty
 where
  loop m@Machine {..}
    | pc `HashSet.member` seen = Looped acc
    | pc == length program = Halted acc
    | otherwise = case HashMap.lookup pc program of
      Nothing -> OutOfBounds acc
      Just instruction -> step (save m) instruction

  save m@Machine {..} = m { seen = HashSet.insert pc seen }

  step m@Machine {..} = \case
    Acc i -> loop m { pc = pc + 1, acc = acc + i }
    Jmp i -> loop m { pc = pc + i }
    Nop{} -> loop m { pc = pc + 1 }

data Machine = Machine
  { acc :: Int
  , pc :: Int
  , seen :: HashSet Int
  }

type Program = HashMap Int Instruction

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int

swap :: Instruction -> Instruction
swap = \case
  Nop i -> Jmp i
  Jmp i -> Nop i
  Acc i -> Acc i

data Result
  = Halted Int
  | Looped Int
  | OutOfBounds Int

halted :: Alternative f => Result -> f Int
halted = \case
  Halted i -> pure i
  _ -> empty

looped :: Alternative f => Result -> f Int
looped = \case
  Looped i -> pure i
  _ -> empty
