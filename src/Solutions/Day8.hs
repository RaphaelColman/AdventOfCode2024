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
  ( Grid,
    Point,
    enumerateMultilineStringToVectorMap,
    gridSize,
    simplify,
  )
import Common.MapUtils (flipMap)
import qualified Data.Map as M
import Data.Range (inRange, (+=+))
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
    restrictedAntinodes = S.filter (inBounds (gridSize input)) allAntinodes

part2 input = length antiNodes
  where
    antennaLocations = findAntennaLocations input
    bounds = gridSize input
    antiNodes = S.fromList $ concat $ M.elems $ M.map (calculateAntinodes bounds) antennaLocations

findAntennaLocations :: M.Map Point Char -> AntennaLocations
findAntennaLocations grid = flipMap $ M.filter (/= '.') grid

calculateImmediateAntinodes :: [Point] -> [Point]
calculateImmediateAntinodes points = do
  (a, b) <- map (\[a, b] -> (a, b)) $ tuples 2 points
  let diff = b - a
  [b + diff, a - diff]

findAllAntinodes :: AntennaLocations -> S.Set Point
findAllAntinodes al = S.fromList $ concat $ M.elems $ M.map calculateImmediateAntinodes al

calculateAntinodes :: Point -> [Point] -> [Point]
calculateAntinodes bounds points = do
  (a, b) <- map (\[a, b] -> (a, b)) $ tuples 2 points
  let diff = simplify $ b - a
  let plusses = takeWhile (inBounds bounds) $ iterate (+ diff) b
  let minuses = takeWhile (inBounds bounds) $ iterate (subtract diff) a
  plusses ++ minuses

inBounds :: Point -> Point -> Bool
inBounds (V2 maxX maxY) (V2 x y) = 0 +=+ maxX `inRange` x && 0 +=+ maxY `inRange` y
