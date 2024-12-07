module Solutions.Day7
  ( aoc7,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Text.Parser.Char (CharParsing (char, string))
import Text.Parser.Token (integer, integer')
import Text.Trifecta (Parser, Parsing (eof), TokenParsing (token), manyTill, newline, sepBy, some, try)
import Combinatorics (permuteRep, variateRep)

aoc7 :: IO ()
aoc7 = do
  printSolutions 7 $ MkAoCSolution parseInput part1

type Equation = (Integer, [Integer])

parseInput :: Parser [Equation]
parseInput = some $ token parseEquation

parseEquation :: Parser Equation
parseEquation = do
  x <- integer <* string ": "
  inputs <- sepBy integer' $ char ' '
  pure (x, inputs)

part1 input = sum $ map fst validEquations
  where validEquations = filter equationIsValid input

part2 :: String -> String
part2 = undefined

data Expression
  = Const Integer
  | Add
  | Mul

instance Show Expression where
  show (Const x) = show x
  show Add = "+"
  show Mul = "*"


allOrders :: Int -> [[Expression]]
allOrders length = variateRep length [Add, Mul]

generateExpressionLists :: [Integer] -> [[Expression]]
generateExpressionLists xs = map equation $ allOrders (length xs - 1)
  where equation = alternate (map Const xs)

evaluate :: [Expression] -> Integer
evaluate [Const i] = i
evaluate (Const x : Add : Const y : rest) = evaluate $ Const (x + y) : rest
evaluate (Const x : Mul : Const y : rest) = evaluate $ Const (x * y) : rest

-- | Can the test values be evaluated to the target value?
equationIsValid :: Equation -> Bool
equationIsValid (target, ints) = target `elem` results
  where results = evaluate <$> generateExpressionLists ints

-- >>> alternate [1..3] [7..10]
-- [1,7,2,8,3,9,10]
alternate :: [a] -> [a] -> [a]
alternate [] [] = []
alternate xs [] = xs
alternate [] ys = ys
alternate (x : xs) (y : ys) = x : y : alternate xs ys
