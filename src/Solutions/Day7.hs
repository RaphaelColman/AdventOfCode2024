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
import Text.Parser.Char (CharParsing (char, string))
import Text.Parser.Token (integer, integer')
import Text.Trifecta (Parser, Parsing (eof), TokenParsing (token), manyTill, newline, sepBy, some, try)
import Data.Maybe (catMaybes, mapMaybe)

aoc7 :: IO ()
aoc7 = do
  printTestSolutions 7 $ MkAoCSolution parseInput part1
  printTestSolutions 7 $ MkAoCSolution parseInput part2

type Equation = (Integer, [Integer])

parseInput :: Parser [Equation]
parseInput = some $ token parseEquation

parseEquation :: Parser Equation
parseEquation = do
  x <- integer <* string ": "
  inputs <- sepBy integer' $ char ' '
  pure (x, inputs)

-- At the moment this just does part 2.
part1 input = sum $ map fst validEquations
  where
    validEquations = filter equationIsValid input

part2 input = sum $ map fst validEquations
  where
    validEquations = filter equationIsValid input

data Expression
  = Const Integer
  | Add
  | Mul
  | Concat

instance Show Expression where
  show (Const x) = show x
  show Add = "+"
  show Mul = "*"
  show Concat = "||"

-- >>> allOrders 2
-- [[+,+],[+,*],[*,+],[*,*]]
allOrders :: Int -> [[Expression]]
allOrders length = variateRep length [Add, Concat, Mul]

generateExpressionLists :: [Integer] -> [[Expression]]
generateExpressionLists xs = map equation $ allOrders (length xs - 1)
  where
    equation = alternate (map Const xs)

evaluate :: Integer -> [Expression] -> Maybe Integer
evaluate target [Const i] = Just i
evaluate target (Const x : Add : Const y : rest) = if x > target then Nothing else evaluate target $ Const (x + y) : rest
evaluate target (Const x : Mul : Const y : rest) = if x > target then Nothing else evaluate target $ Const (x * y) : rest
evaluate target (Const x : Concat : Const y : rest) = if x > target then Nothing else evaluate target $ Const (concatenateIntegers x y) : rest
--There has GOT to be a neater way of doing this!

-- | Can the test values be evaluated to the target value?
equationIsValid :: Equation -> Bool
equationIsValid (target, ints) = target `elem` results
  where
    results = mapMaybe (evaluate target) $ generateExpressionLists ints

-- >>> alternate [1..3] [7..10]
-- [1,7,2,8,3,9,10]
alternate :: [a] -> [a] -> [a]
alternate [] [] = []
alternate xs [] = xs
alternate [] ys = ys
alternate (x : xs) (y : ys) = x : y : alternate xs ys

concatenateIntegers :: Integer -> Integer -> Integer
concatenateIntegers x y = read $ show x ++ show y
