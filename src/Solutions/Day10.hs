module Solutions.Day10
  ( aoc10,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry (Grid, Point, allOrthogonalNeighbours, enumerateMultilineStringToVectorMap, gridOrthogonalNeighbours)
import Data.Char (digitToInt)
import Data.Function ((&))
import Data.Graph.AStar (aStar)
import qualified Data.HashSet as HS
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Linear.V2 (V2 (..))
import Text.Trifecta (CharParsing (anyChar), Parser, some)

type TrailMap = Grid Int

aoc10 :: IO ()
aoc10 = do
  printSolutions 10 $ MkAoCSolution parseInput part1

-- printSolutions 10 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Int)
parseInput = do
  charGrid <- enumerateMultilineStringToVectorMap <$> some anyChar
  pure $ M.map digitToInt charGrid

part1 input = sum $ trailHeadScores input

part2 :: String -> String
part2 = undefined

validNeighbours :: Grid Int -> V2 Int -> S.Set (V2 Int)
validNeighbours grid pos =
  gridOrthogonalNeighbours grid pos
    & M.keysSet
    & S.filter (\p -> (grid M.! p) - (grid M.! pos) == 1)

destinations :: TrailMap -> [V2 Int]
destinations tm = M.filter (== 9) tm & M.keys

starts :: TrailMap -> [V2 Int]
starts tm = M.filter (== 0) tm & M.keys

shortestPath :: TrailMap -> V2 Int -> V2 Int -> Maybe [V2 Int]
shortestPath tm start end =
  aStar
    (HS.fromList . S.toList . validNeighbours tm) -- graph representation
    (\a b -> 1) -- cost
    (manhattanDistance end) -- heuristic
    (== end) -- goal
    start

manhattanDistance :: V2 Int -> V2 Int -> Int
manhattanDistance (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

trailheadScore :: TrailMap -> V2 Int -> Int
trailheadScore tm start = length $ mapMaybe (shortestPath tm start) $ destinations tm

trailHeadScores :: TrailMap -> [Int]
trailHeadScores tm = map (trailheadScore tm) $ starts tm

--test starts: [V2 0 7,V2 1 6,V2 2 1,V2 3 0,V2 3 5,V2 4 1,V2 5 0,V2 6 5,V2 7 6]
