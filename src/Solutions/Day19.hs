module Solutions.Day19
  ( aoc19,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Data.Function ((&))
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Text.Parser.Combinators (some)
import Text.Trifecta (CharParsing (anyChar, string), Parser, TokenParsing (token), commaSep, letter, manyTill)

aoc19 :: IO ()
aoc19 = do
  printSolutions 19 $ MkAoCSolution parseInput part1

-- printSolutions 19 $ MkAoCSolution parseInput part2

parseInput :: Parser ([String], [String])
parseInput = do
  towels <- parseTowels
  string "\n\n"
  stripeCombos <- some $ token $ some letter
  pure (towels, stripeCombos)

parseTowels :: Parser [String]
parseTowels = commaSep $ some letter

part1 (towels, patterns) = length $ filter (possiblePattern towels) patterns

placeTowel :: String -> String -> Maybe String
placeTowel pattern towel
  | towel `isPrefixOf` pattern = Just $ drop (length towel) pattern
  | otherwise = Nothing

possiblePattern :: [String] -> String -> Bool
possiblePattern towels = go
  where
    go :: String -> Bool
    go pattern'
      | null pattern' = True
      | otherwise = mapMaybe (placeTowel pattern') towels & any go
