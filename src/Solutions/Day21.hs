{-# LANGUAGE ScopedTypeVariables #-}

module Solutions.Day21
  ( aoc21,
  )
where

import Combinatorics (permute, permuteRep, variate, variateRep)
import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.FunctorUtils (fmap2)
import Common.Geometry (Point)
import Common.ListUtils (window2)
import Data.Char (isDigit)
import Data.Function ((&))
import Data.List (minimumBy, scanl')
import qualified Data.Map as M
import Data.MemoTrie (memo2)
import Data.Ord (comparing)
import Debug.Trace (trace, traceShow)
import Linear (unit, _x, _y)
import Linear.V2 (V2 (V2))
import Text.Parser.Combinators (some)
import Text.Parser.Token (token)
import Text.Printf (printf)
import Text.Trifecta (CharParsing (anyChar), Parser, alphaNum, letter)

type Keymap = M.Map Char (V2 Int)

aoc21 :: IO ()
aoc21 = printTestSolutions 21 $ MkAoCSolution parseInput part1

-- printSolutions 21 $ MkAoCSolution parseInput part2

parseInput :: Parser [String]
parseInput = some $ token $ some alphaNum

part1 input = sum $ map complexity input

bestInput :: Integer -> String -> String
bestInput level input
  | level == 0 = input
  | otherwise = concatMap (minimumBy (comparing length)) asBestInput
  where
    pairs = window2 ('A' : input)
    m = map (\(a, b) -> allPaths M.! (a, b)) pairs
    asBestInput = fmap2 (bestInput (level - 1)) m

complexity :: String -> Integer
complexity code = toInteger (length dirpath) * numPart
  where
    numPart :: Integer = read $ takeWhile isDigit code
    dirpath = bestInput 3 code

unitVectorToChar :: V2 Int -> Char
unitVectorToChar v = case v of
  V2 0 1 -> 'v'
  V2 0 (-1) -> '^'
  V2 1 0 -> '>'
  V2 (-1) 0 -> '<'

numpadKeyPaths :: Char -> Char -> [String]
numpadKeyPaths start end = (++ "A") <$> fmap2 unitVectorToChar validPaths
  where
    (V2 x y) = numpadKeys M.! end - numpadKeys M.! start
    paths = permuteRep [(V2 (signum x) 0, abs x), (V2 0 (signum y), abs y)]
    validPaths = filter (\path -> V2 0 3 `notElem` nodesVisited path) paths
    nodesVisited = scanl' (+) (numpadKeys M.! start)
    numpadKeys = M.fromList $ zip "789456123 0A" pts
      where
        pts = [V2 x y | y <- [0 .. 3], x <- [0 .. 2]]

dirpadKeyPaths :: Char -> Char -> [String]
dirpadKeyPaths start end = (++ "A") <$> fmap2 unitVectorToChar validPaths
  where
    (V2 x y) = dirpadKeys M.! end - dirpadKeys M.! start
    paths = permuteRep [(V2 (signum x) 0, abs x), (V2 0 (signum y), abs y)]
    validPaths = filter (\path -> V2 0 0 `notElem` nodesVisited path) paths
    nodesVisited = scanl' (+) (dirpadKeys M.! start)
    dirpadKeys = M.fromList $ zip " ^A<v>" pts
      where
        pts = [V2 x y | y <- [0 .. 1], x <- [0 .. 2]]

allPaths :: M.Map (Char, Char) [String]
allPaths = M.union numMap dirMap
  where
    numKeys = variateRep 2 "1234567890A"
    numMap = M.fromList $ map (\[a, b] -> ((a, b), numpadKeyPaths a b)) numKeys
    dirKeys = variateRep 2 " ^A<v>^"
    dirMap = M.fromList $ map (\[a, b] -> ((a, b), dirpadKeyPaths a b)) dirKeys
