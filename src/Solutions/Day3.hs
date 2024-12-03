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
import           Text.Parser.Combinators         (try, choice)
import           Text.Parser.Token               (comma, parens)
import           Text.Trifecta                   (CharParsing (anyChar, string),
                                                  Parser, Parsing (skipSome),
                                                  integer, some)

aoc3 :: IO ()
aoc3 = do
  --printSolutions 3 $ MkAoCSolution parseInput part1
  printSolutions 3 $ MkAoCSolution parseInput part2

parseInput :: Parser [Either Instruction Bool]
parseInput = do
  some $ try $ skipManyTill anyChar (try parseEnabledOrInstruction)

parseInstruction :: Parser (Integer, Integer)
parseInstruction = do
  string "mul"
  parens $ do
    x <- integer
    comma
    y <- integer
    return (x, y)

parseEnabledOrInstruction :: Parser (Either Instruction Bool)
parseEnabledOrInstruction = do
  choice [try $ Right <$> parseEnabled, try $ Left <$> parseInstruction]

parseEnabled :: Parser Bool
parseEnabled = do
  try (string "do()" $> True) <|> try (string "don't()" $> False)

type Instruction = (Integer, Integer)

part1 :: [(Integer, Integer)] -> Integer
part1 = sum . map (uncurry (*))

part2 :: [Either Instruction Bool] -> Integer
part2 = solve

solve :: [Either Instruction Bool] -> Integer
solve = go True 0 
  where go :: Bool -> Integer -> [Either Instruction Bool] -> Integer
        go enabled accum (x:xs) = case x of
          Left (a, b) -> if enabled then go enabled (accum + a * b) xs else go enabled accum xs
          Right b -> go b accum xs
        go _ accum [] = accum
