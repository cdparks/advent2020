{-# LANGUAGE StrictData #-}

module Advent.Day04 where

import Advent.Prelude

import Advent.Parse
import Data.Bits ((.|.))
import qualified Data.Text as Text
import Data.Word (Word8)

main :: Part -> IO ()
main = print . length . filter (== 127) <=< parseOrDie . parsePassports

parsePassports :: Part -> Parser [Word8]
parsePassports part = parsePassport part `sepBy1` twoNewlines
  where twoNewlines = endOfLine *> endOfLine

parsePassport :: Part -> Parser Word8
parsePassport part = bitor <$> (parseFlag part `sepBy1` oneSpace)
 where
  oneSpace = void (char ' ') <|> endOfLine
  bitor = foldl' (.|.) 0

parseFlag :: Part -> Parser Word8
parseFlag part = do
  key <- takeWhile1 isAsciiLower
  void $ char ':'
  parseValue key part

parseValue :: Text -> Part -> Parser Word8
parseValue = \case
  "byr" -> validate 64 $ parseYear 1920 2002
  "iyr" -> validate 32 $ parseYear 2010 2020
  "eyr" -> validate 16 $ parseYear 2020 2030
  "hgt" -> validate 8 parseHeight
  "hcl" -> validate 4 parseHairColor
  "ecl" -> validate 2 parseEyeColor
  "pid" -> validate 1 parsePassportId
  "cid" -> validate 0 parseAny
  other -> const $ fail $ "Unrecognized key " <> show other

validate :: Word8 -> Parser () -> Part -> Parser Word8
validate flag parser = \case
  Part1 -> flag <$ parseAny
  Part2 -> flag <$ parser <|> 0 <$ parseAny

parseAny :: Parser ()
parseAny = void $ takeWhile1 $ not . isSpace

parseYear :: Int -> Int -> Parser ()
parseYear lo hi = parseRange $ pure (lo, hi)

-- brittany-disable-next-binding

parseHeight :: Parser ()
parseHeight = parseRange $ asum
  [ (150, 193) <$ string "cm"
  , ( 59,  76) <$ string "in"
  ]

parseHairColor :: Parser ()
parseHairColor = char '#' *> parseN 6 isHexDigit

-- brittany-disable-next-binding

parseEyeColor :: Parser ()
parseEyeColor = void $ foldMap string
  [ "amb"
  , "blu"
  , "brn"
  , "gry"
  , "grn"
  , "hzl"
  , "oth"
  ]

parsePassportId :: Parser ()
parsePassportId = parseN 9 isDigit

parseRange :: Parser (Int, Int) -> Parser ()
parseRange getRange = do
  n <- decimal
  (lo, hi) <- getRange
  guard $ lo <= n
  guard $ n <= hi

parseN :: Int -> (Char -> Bool) -> Parser ()
parseN n = guard . (== n) . Text.length <=< takeWhile1
