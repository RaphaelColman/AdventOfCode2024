module Solutions.Day3
    ( aoc3
    ) where

import           Common.AoCSolutions             (AoCSolution (MkAoCSolution),
                                                  printSolutions,
                                                  printTestSolutions)
import           Control.Applicative             (Alternative ((<|>)))
import           Control.Applicative.Combinators (skipManyTill)
import           Control.Monad.Extra             (skip)
import           GHC.Read                        (paren)
import           Text.Parser.Combinators         (try)
import           Text.Parser.Token               (comma, parens)
import           Text.Trifecta                   (CharParsing (anyChar, string),
                                                  Parser, Parsing (skipSome),
                                                  integer, some)

aoc3 :: IO ()
aoc3 = do
  printSolutions 3 $ MkAoCSolution parseInput part1
  --printSolutions 3 $ MkAoCSolution parseInput part2

parseInput :: Parser [(Integer, Integer)]
parseInput = do
  some $ try $ skipManyTill anyChar (try parseInstruction)

parseInstruction :: Parser (Integer, Integer)
parseInstruction = do
  string "mul"
  parens $ do
    x <- integer
    comma
    y <- integer
    return (x, y)

part1 = sum . map (uncurry (*))

part2 :: String -> String
part2 = undefined
