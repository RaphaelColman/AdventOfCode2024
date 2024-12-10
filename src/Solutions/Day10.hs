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
import Debug.Trace

type TrailMap = Grid Int

aoc10 :: IO ()
aoc10 = do
  -- printSolutions 10 $ MkAoCSolution parseInput part1

  printSolutions 10 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Int)
parseInput = do
  charGrid <- enumerateMultilineStringToVectorMap <$> some anyChar
  pure $ M.map digitToInt charGrid

-- part1 input = sum $ trailHeadScores input

part2 input = sum $ map (trailHeadRating input) sts
  where sts = starts input

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

trailHeadRating :: TrailMap -> V2 Int -> Int
trailHeadRating tm = go
  where
    go node
      | tm M.! node == 9 = 1
      | null nextNodes = 0
      | otherwise = sum $ map go nextNodes
      where
        nextNodes = S.toList $ validNeighbours tm node
