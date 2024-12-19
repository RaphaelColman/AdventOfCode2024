module Solutions.Day19
  ( aoc19
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, commaSep, CharParsing (anyChar, string), manyTill, letter, TokenParsing (token))
import qualified Data.Set as S
import Text.Parser.Combinators (some)

aoc19 :: IO ()
aoc19 = do
  printTestSolutions 19 $ MkAoCSolution parseInput part1
  --printSolutions 19 $ MkAoCSolution parseInput part2

parseInput :: Parser ([String], [String])
parseInput = do
  towels <- parseTowels
  string "\n\n"
  stripeCombos <- some $ token $ some letter
  pure $ (towels, stripeCombos)

parseTowels :: Parser [String]
parseTowels = commaSep $ some letter

part1 = id
