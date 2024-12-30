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
import Common.MathUtils (isInteger, rationalToInteger)
import Control.Applicative (Alternative ((<|>)))
import Control.Lens
import Control.Lens.Combinators (over)
import Control.Monad (guard)
import Data.Maybe (catMaybes, mapMaybe)
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Trifecta (CharParsing (char, string), Parser, count, integer, some)

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

part1 :: [Machine] -> Integer
part1 = solve 0 

part2 :: [Machine] -> Integer
part2 = solve 10000000000000

solve :: Integer -> [Machine] -> Integer
solve amount machines = sum $ map (uncurry tokenCost) solved
  where solved = mapMaybe (resolveMachine . modifyTarget amount) machines

-- | These are simultaneous equations.
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

modifyTarget :: Integer -> Machine -> Machine
modifyTarget amount machine = machine & prizeLocation %~ (+ V2 amount amount)
