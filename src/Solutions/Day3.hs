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

parseInput :: Parser [Instruction]
parseInput = do
  some $ try $ skipManyTill anyChar (try parseEnabledOrInstruction)

parseMulInstruction :: Parser Instruction
parseMulInstruction = do
  string "mul"
  parens $ do
    x <- integer
    comma
    Mul x <$> integer

parseEnabledOrInstruction :: Parser Instruction
parseEnabledOrInstruction = do
  choice [try parseEnabled, try parseMulInstruction]

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
solve = go True 0
  where go enabled accum (x:xs) = case x of
          Mul a b -> if enabled then go enabled (accum + a * b) xs else go enabled accum xs
          Enable b -> go b accum xs
        go _ accum [] = accum
