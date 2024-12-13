{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day13
  ( aoc13,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Control.Applicative (Alternative ((<|>)))
import Control.Lens (makeLenses)
import Linear (V2 (V2))
import Text.Trifecta (CharParsing (char, string), Parser, count, integer, some)
import Common.MathUtils (isInteger, rationalToInteger)
import Control.Monad (guard)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Lens.Combinators (over)

data Machine = MkMachine
  { _buttonA :: !(V2 Integer), -- Costs 3 tokens
    _buttonB :: !(V2 Integer), -- Costs 1 token
    _prizeLocation :: !(V2 Integer)
  }
  deriving (Eq, Show)

makeLenses ''Machine

aoc13 :: IO ()
aoc13 = do
  printSolutions 13 $ MkAoCSolution parseInput part1
  printSolutions 13 $ MkAoCSolution parseInput part2

--
parseInput :: Parser [Machine]
parseInput = some parseMachine

parseMachine :: Parser Machine
parseMachine = do
  [a, b] <- count 2 parseButton
  MkMachine a b <$> parsePrize

parseButton :: Parser (V2 Integer)
parseButton = do
  string "Button " >> (char 'A' <|> char 'B') >> string ": "
  x <- string "X+" *> integer <* string ", "
  y <- string "Y+" *> integer
  pure $ V2 x y

parsePrize :: Parser (V2 Integer)
parsePrize = do
  string "Prize: "
  x <- string "X=" *> integer <* string ", "
  y <- string "Y=" *> integer
  pure $ V2 x y

part1 input = sum $ map (uncurry tokenCost) solved
  where solved = mapMaybe resolveMachine input

part2 input = sum $ map (uncurry tokenCost) solved
  where solved = mapMaybe (resolveMachine . modifyMachine) input

resolveMachine :: Machine -> Maybe (Integer, Integer)
resolveMachine (MkMachine (V2 ax ay) (V2 bx by) (V2 tx ty)) = do
  a <- rationalToInteger a'
  b <- rationalToInteger b'
  pure (a, b)
  where
    b' :: Rational = fromIntegral (tx * ay - ty * ax) / fromIntegral (bx * ay - by * ax)
    a' = (fromIntegral ty - (b' * fromIntegral by)) / fromIntegral ay

tokenCost :: Integer -> Integer -> Integer
tokenCost a b = 3 * a + b

modifyMachine :: Machine -> Machine
modifyMachine (MkMachine a b (V2 x y)) = MkMachine a b (V2 (x+10000000000000) (y+10000000000000))
