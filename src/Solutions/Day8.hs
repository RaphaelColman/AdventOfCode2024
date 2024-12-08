{-# LANGUAGE TupleSections #-}

module Solutions.Day8
  ( aoc8,
  )
where

import Combinatorics (tuples)
import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry
import qualified Data.Map as M
import qualified Data.Set as S
import Linear.V2 (V2 (V2))
import Text.Parser.Char (anyChar)
import Text.Trifecta (Parser, some)

type AntennaLocations = M.Map Char [Point]

aoc8 :: IO ()
aoc8 = do
  printSolutions 8 $ MkAoCSolution parseInput part1
  printSolutions 8 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  enumerateMultilineStringToVectorMap <$> some anyChar

part1 input = length restrictedAntinodes
  where
    allAntinodes = findAllAntinodes $ findAntennaLocations input
    (V2 maxX maxY) = gridSize input
    restrictedAntinodes = S.filter (\(V2 x y) -> x >= 0 && y >= 0 && x <= maxX && y <= maxY) allAntinodes

part2 input = length antiNodes
  where
    antennaLocations = findAntennaLocations input
    bounds = gridSize input
    antiNodes = S.fromList $ concat $ M.elems $ M.map (calculateAntinodes' bounds) antennaLocations

findAntennaLocations :: M.Map Point Char -> AntennaLocations
findAntennaLocations grid = M.fromList $ map getPts $ S.toList distinctChars
  where
    distinctChars = S.delete '.' $ S.fromList $ M.elems grid
    getPts c = (c,) $ M.keys $ M.filter (== c) grid

calculateAntinodes :: [Point] -> [Point]
calculateAntinodes points = concatMap antinodesForPair allPairs
  where
    allPairs = map (\[a, b] -> (a, b)) $ tuples 2 points
    antinodesForPair (a, b) =
      let diff = b - a
       in [b + diff, a - diff]

findAllAntinodes :: AntennaLocations -> S.Set Point
findAllAntinodes al = S.fromList $ concat $ M.elems $ M.map calculateAntinodes al

calculateAntinodes' :: Point -> [Point] -> [Point]
calculateAntinodes' bounds points = concatMap antinodesForPair allPairs
  where
    allPairs = map (\[a, b] -> (a, b)) $ tuples 2 points
    antinodesForPair (a, b) = plusses ++ minuses
      where
        diff = simplify $ b - a
        plusses = takeWhile (inBounds bounds) $ iterate (+ diff) b
        minuses = takeWhile (inBounds bounds) $ iterate (subtract diff) a

inBounds :: Point -> Point -> Bool
inBounds (V2 maxX maxY) (V2 x y) = x >= 0 && y >= 0 && x <= maxX && y <= maxY
