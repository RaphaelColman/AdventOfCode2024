module Solutions.Day7
  ( aoc7,
  )
where

import Combinatorics (permuteRep, variateRep)
import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Control.Monad (guard)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, mapMaybe)
import Text.Parser.Char (CharParsing (char, string))
import Text.Parser.Token (integer, integer')
import Text.Trifecta (Parser, Parsing (eof), TokenParsing (token), manyTill, newline, sepBy, some, try)

aoc7 :: IO ()
aoc7 = do
  printSolutions 7 $ MkAoCSolution parseInput part1
  printSolutions 7 $ MkAoCSolution parseInput part2

type Equation = (Integer, [Integer])

type OpTest = Integer -> Integer -> Maybe Integer

parseInput :: Parser [Equation]
parseInput = some $ token parseEquation

parseEquation :: Parser Equation
parseEquation = do
  x <- integer <* string ": "
  inputs <- sepBy integer' $ char ' '
  pure (x, inputs)

part1 :: [Equation] -> Integer
part1 = solve [testMultiply, testAdd]

part2 :: [Equation] -> Integer
part2 = solve [testMultiply, testAdd, testConcat]

solve :: [OpTest] -> [Equation] -> Integer
solve tests input = sum $ map fst validEquations
  where
    validEquations = filter (equationIsValid tests) input

equationIsValid :: [OpTest] -> Equation -> Bool
equationIsValid tests (target, first : rest) = elem first $ foldr go [target] rest
  where
    go :: Integer -> [Integer] -> [Integer]
    go a acc = do
      current <- acc
      mapMaybe (\f -> f current a) tests

testMultiply :: OpTest
testMultiply x y = let (quotient, remainder) = x `quotRem` y in if remainder == 0 then Just quotient else Nothing

testAdd :: OpTest
testAdd x y = let result = x - y in if result >= 0 then Just result else Nothing

testConcat :: OpTest
testConcat x y = do
  guard $ x /= y
  if yString `isSuffixOf` xString then Just (read (take numCharsToKeep xString)) else Nothing
  where
    xString = show x
    yString = show y
    numCharsToKeep = length xString - length yString
