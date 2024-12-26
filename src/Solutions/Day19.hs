{-# LANGUAGE ScopedTypeVariables #-}

module Solutions.Day19
  ( aoc19,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import qualified Common.Geometry
import Control.Parallel.Strategies
import Data.Function (fix, (&))
import Data.List (inits, isPrefixOf, tails)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Debug.Trace
import Text.Parser.Combinators (some)
import Text.Printf (printf)
import Text.Trifecta (CharParsing (anyChar, string), Parser, TokenParsing (token), commaSep, letter, manyTill)

aoc19 :: IO ()
aoc19 = do
  printSolutions 19 $ MkAoCSolution parseInput part1
  printSolutions 19 $ MkAoCSolution parseInput part2

parseInput :: Parser ([String], [String])
parseInput = do
  towels <- parseTowels
  string "\n\n"
  stripeCombos <- some $ token $ some letter
  pure (towels, stripeCombos)

parseTowels :: Parser [String]
parseTowels = commaSep $ some letter

part1 :: ([String], [String]) -> Int
part1  = length . filter (> 0) . solve

part2 :: ([String], [String]) -> Integer
part2 = sum . solve

solve :: ([String], [String]) -> [Integer]
solve (towels, patterns) = map (countPatterns towels) patterns

placeTowel :: String -> String -> Maybe String
placeTowel pattern towel
  | towel `isPrefixOf` pattern = Just $ drop (length towel) pattern
  | otherwise = Nothing

countPatterns :: [String] -> String -> Integer
countPatterns towels pattern = memo M.! pattern
  where
    memo = M.fromList instr
    instr = map go $ tails pattern
    go [] = ("", 1)
    go k = (k, total)
      where
        remaining = mapMaybe (placeTowel k) towels
        total = sum $ map (memo M.!) remaining
