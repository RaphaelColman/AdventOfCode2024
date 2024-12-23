{-# LANGUAGE ScopedTypeVariables #-}

module Solutions.Day21
  ( aoc21,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry (Point)
import Common.ListUtils (window2)
import Data.Char (isDigit)
import qualified Data.Map as M
import Data.MemoTrie (memo2)
import Debug.Trace (traceShow)
import Linear (unit, _x, _y)
import Linear.V2 (V2 (V2))
import Text.Parser.Combinators (some)
import Text.Parser.Token (token)
import Text.Trifecta (CharParsing (anyChar), Parser, alphaNum, letter)
import Combinatorics (permute)

type Keymap = M.Map Char (V2 Int)

aoc21 :: IO ()
aoc21 = do
  printTestSolutions 21 $ MkAoCSolution parseInput part1

-- printSolutions 21 $ MkAoCSolution parseInput part2

parseInput :: Parser [String]
parseInput = some $ token $ some alphaNum

part1 input = numpadKeyPaths '3' '7'

numpadKeyPath :: Char -> Char -> [Char]
numpadKeyPath start end = rights ++ ups ++ lefts ++ downs ++ "A"
  where
    (V2 x y) = numpadKeys M.! end - numpadKeys M.! start
    rights = replicate x '>'
    downs = replicate y 'v'
    lefts = replicate (-x) '<'
    ups = replicate (-y) '^'
    numpadKeys = M.fromList $ zip "789456123 0A" pts
      where
        pts = [V2 x y | y <- [0 .. 3], x <- [0 .. 2]]

numpadKeyPaths :: Char -> Char -> [String]
numpadKeyPaths start end = permute asChars --dang. <<vv appears twice (technically two different combos)
--Maybe we can do this with some sort of cartesian
  where
    (V2 x y) = numpadKeys M.! end - numpadKeys M.! start
    xMoves = replicate (abs x) (V2 (signum x) 0)
    yMoves = replicate (abs y) (V2 0 (signum y))
    asChars = map unitVectorToChar $ xMoves ++ yMoves
    numpadKeys = M.fromList $ zip "789456123 0A" pts
      where
        pts = [V2 x y | y <- [0 .. 3], x <- [0 .. 2]]

dirpadKeyPath :: Char -> Char -> [Char]
dirpadKeyPath start end = downs ++ rights ++ ups ++ lefts ++ "A"
  where
    (V2 x y) = dirpadKeys M.! end - dirpadKeys M.! start
    rights = replicate x '>'
    downs = replicate y 'v'
    lefts = replicate (-x) '<'
    ups = replicate (-y) '^'
    dirpadKeys = M.fromList $ zip " ^A<v>" pts
      where
        pts = [V2 x y | y <- [0 .. 1], x <- [0 .. 2]]

expandCode :: String -> String
expandCode code = concatMap (uncurry numpadKeyPath) $ window2 ('A' : code)

expandDirpath :: String -> String
expandDirpath dirpath = concatMap (uncurry dirpadKeyPath) $ window2 ('A' : dirpath)

fullSequence :: String -> String
fullSequence = expandDirpath . expandDirpath . expandCode

complexity :: String -> (Int, Int)
complexity code = (length dirpath, numPart)
  where
    numPart :: Int = read $ takeWhile isDigit code
    dirpath = fullSequence code

unitVectorToChar :: V2 Int -> Char
unitVectorToChar v = case v of
  V2 0 1 -> '^'
  V2 0 (-1) -> 'v'
  V2 1 0 -> '>'
  V2 (-1) 0 -> '<'
