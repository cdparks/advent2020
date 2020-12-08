{-# LANGUAGE StrictData #-}

module Advent.Day08
  ( main
  )
where

import Advent.Prelude

import Advent.Parse
import Data.HashMap.Strict ((!))
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

parseProgram :: Parser (HashMap Int Instruction)
parseProgram = HashMap.fromList . zip [0 ..] <$> many instruction
 where
  instruction = asum
    [ Acc <$ sym "acc" <*> operand
    , Jmp <$ sym "jmp" <*> operand
    , Nop <$ sym "nop" <*> operand
    ]
  operand = token $ signed decimal

run :: HashMap Int Instruction -> Either Int Int
run program = loop $ Machine 0 0 HashSet.empty
 where
  loop m@Machine {..}
    | pc `HashSet.member` seen = Left acc
    | pc == length program = Right acc
    | otherwise = step (save m) $ program ! pc

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

data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int

swap :: Instruction -> Instruction
swap = \case
  Acc i -> Acc i
  Jmp i -> Nop i
  Nop i -> Jmp i

halted :: Alternative f => Either Int Int -> f Int
halted = either (const empty) pure

looped :: Alternative f => Either Int Int -> f Int
looped = either pure (const empty)
