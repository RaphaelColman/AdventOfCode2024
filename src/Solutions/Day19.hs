module Solutions.Day19
  ( aoc19,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import qualified Data.Set as S
import Text.Parser.Combinators (some)
import Text.Trifecta (CharParsing (anyChar, string), Parser, TokenParsing (token), commaSep, letter, manyTill)

aoc19 :: IO ()
aoc19 = do
  printTestSolutions 19 $ MkAoCSolution parseInput part1

-- printSolutions 19 $ MkAoCSolution parseInput part2

parseInput :: Parser ([String], [String])
parseInput = do
  towels <- parseTowels
  string "\n\n"
  stripeCombos <- some $ token $ some letter
  pure (towels, stripeCombos)

parseTowels :: Parser [String]
parseTowels = commaSep $ some letter

part1 = id


placeTowel :: String -> String -> Maybe String
placeTowel pattern towel = undefined
