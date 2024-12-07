module Solutions.Day7
  ( aoc7
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, sepBy, some, newline, try, manyTill, Parsing (eof), TokenParsing (token))
import Text.Parser.Token ( integer, integer' )
import Text.Parser.Char (CharParsing(char, string))

aoc7 :: IO ()
aoc7 = do
  printTestSolutions 7 $ MkAoCSolution parseInput part1

type Equation = (Integer, [Integer])

parseInput :: Parser [Equation]
parseInput = some $ token parseEquation

parseEquation :: Parser Equation
parseEquation = do
  x <- integer <* string ": "
  inputs <- sepBy integer' $ char ' '
  pure (x, inputs)

part1 = id

part2 :: String -> String
part2 = undefined
