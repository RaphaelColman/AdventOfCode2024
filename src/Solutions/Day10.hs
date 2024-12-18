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
import qualified Data.HashSet as HS
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Debug.Trace
import Linear.V2 (V2 (..))
import Text.Trifecta (CharParsing (anyChar), Parser, some)

type TrailMap = Grid Int

aoc10 :: IO ()
aoc10 = do
  printSolutions 10 $ MkAoCSolution parseInput part1
  printSolutions 10 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Int)
parseInput = do
  charGrid <- enumerateMultilineStringToVectorMap <$> some anyChar
  pure $ M.map digitToInt charGrid

part1 input = sum $ trailHeadScores input

part2 input = sum $ map (trailHeadRating input) sts
  where
    sts = starts input

validNeighbours :: Grid Int -> V2 Int -> S.Set (V2 Int)
validNeighbours grid pos =
  gridOrthogonalNeighbours grid pos
    & M.keysSet
    & S.filter (\p -> (grid M.! p) - (grid M.! pos) == 1)

destinations :: TrailMap -> [V2 Int]
destinations tm = M.filter (== 9) tm & M.keys

starts :: TrailMap -> [V2 Int]
starts tm = M.filter (== 0) tm & M.keys

trailheadScore :: TrailMap -> V2 Int -> Int
trailheadScore tm node = length $ memo M.! node
  where
    memo = M.mapWithKey go tm
    go k v
      | v == 9 = S.singleton k
      | null nextNodes = S.empty
      | otherwise = S.unions $ map (memo M.!) nextNodes
      where
        nextNodes = S.toList $ validNeighbours tm k

trailHeadScores :: TrailMap -> [Int]
trailHeadScores tm = map (trailheadScore tm) $ starts tm

trailHeadRating :: TrailMap -> V2 Int -> Int
trailHeadRating tm node = memo M.! node
  where
    memo = M.mapWithKey go tm
    go k v
      | v == 9 = 1
      | null nextNodes = 0
      | otherwise = sum $ map (memo M.!) nextNodes
      where
        nextNodes = S.toList $ validNeighbours tm k
