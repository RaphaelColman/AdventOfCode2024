module Solutions.Day17
  ( aoc17
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, CharParsing (string), alphaNum, letter, upper, integer, some, commaSep)

data Computer = MkComputer
  { _aReg :: !Integer,
    _bReg :: !Integer,
    _cReg :: !Integer,
    _program :: ![Integer]
  }
  deriving (Show, Eq)

aoc17 :: IO ()
aoc17 = do
  printTestSolutions 17 $ MkAoCSolution parseInput part1
  -- printSolutions 17 $ MkAoCSolution parseInput part2

parseInput :: Parser Computer
parseInput = do
  [a, b, c] <- some parseRegister
  string "Program: "
  xs <- commaSep integer
  pure $ MkComputer a b c xs

parseRegister :: Parser Integer
parseRegister = do
  string "Register " >> upper >> string ": "
  integer

part1 = id
