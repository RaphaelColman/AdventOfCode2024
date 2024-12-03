{-# LANGUAGE LambdaCase #-}
module Solutions.Day3
    ( aoc3
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Control.Applicative             (Alternative ((<|>)))
import           Control.Applicative.Combinators (skipManyTill, someTill)
import           Control.Monad.Extra             (skip)
import           Data.Foldable                   (Foldable (foldl'))
import           Data.Functor                    (($>))
import           GHC.Read                        (paren)
import           Text.Parser.Combinators         (choice, try)
import           Text.Parser.Token               (comma, parens)
import           Text.Trifecta                   (CharParsing (anyChar, string),
                                                  Parser, Parsing (skipSome),
                                                  integer, some)


data Instruction
  = Mul Integer Integer
  | Enable Bool
  deriving (Eq, Show)

aoc3 :: IO ()
aoc3 = do
  printSolutions 3 $ MkAoCSolution parseInput part1
  printSolutions 3 $ MkAoCSolution parseInput part2


-- | Keep skipping characters until we are able to parse the enable or mul instruction.
-- Do this until the end of the file.
parseInput :: Parser [Instruction]
parseInput = do
  some $ try $ skipManyTill anyChar parseEnabledOrInstruction
  --We have to put this extra try in because EOF doesn't count as an anyChar, so it fails when it hits EOF because
  --there's no parser for it.

-- | Parse the string "mul(x, y)" where x and y are integers and return the corresponding 'Mul' instruction.
-- Rollback the parser if we failed
parseMulInstruction :: Parser Instruction
parseMulInstruction = try $ do
  string "mul"
  parens $ do
    x <- integer
    comma
    Mul x <$> integer

-- | Try parsing the the enabled instruction, and if that failed try parsing the mul instruction.
parseEnabledOrInstruction :: Parser Instruction
parseEnabledOrInstruction = do
  parseEnabled <|> parseMulInstruction

-- | Parse the string "do()" or "don't()" and return the corresponding 'Enable' instruction.
-- Rollback the parser if we failed
parseEnabled :: Parser Instruction
parseEnabled = do
  try (string "do()" $> Enable True) <|> try (string "don't()" $> Enable False)

part1 :: [Instruction] -> Integer
part1 input = solve justMuls
  where justMuls = filter (\case
                        Mul _ _ -> True
                        Enable _ -> False) input

part2 :: [Instruction] -> Integer
part2 = solve

solve :: [Instruction] -> Integer
solve xs = fst $ foldl' go (0, True) xs
  where go (acc, enabled) (Mul a b) = if enabled then (acc + a * b, enabled) else (acc, enabled)
        go (acc, enabled) (Enable b) = (acc, b)
