{-# LANGUAGE ScopedTypeVariables #-}

module Solutions.Day20
  ( aoc20,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry (Grid, Point, allOrthogonalNeighbours, enumerateMultilineStringToVectorMap, gridOrthogonalNeighbours, manhattanDistance)
import Common.ListUtils (freqs, windowN)
import Control.Lens (makeLenses)
import Control.Lens.Getter ((^.))
import Data.Function ((&))
import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Debug.Trace
import Linear.V2 (V2 (V2), _x, _y)
import Linear.Vector (unit)
import Safe (headMay)
import Text.Parser.Combinators (some)
import Text.Printf (printf)
import Text.Trifecta (CharParsing (anyChar), Parser)

aoc20 :: IO ()
aoc20 = do
  printSolutions 20 $ MkAoCSolution parseInput part1
  printSolutions 20 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> some anyChar

part1 :: M.Map Point Char -> Int
part1 = solve 2 100

part2 :: M.Map Point Char -> Int
part2 = solve 20 100

solve :: Int -> Int -> M.Map Point Char -> Int
solve maxSkip minSaving input = length $ filter (>= minSaving) saved
  where
    cheatless = runRoute input
    saved = concatMap (savings cheatless maxSkip) [2 .. (length cheatless - 1)]

-- | Run the route with no cheats
runRoute :: Grid Char -> [V2 Int]
runRoute grid = start : theRun
  where
    start = M.filter (== 'S') grid & M.keys & head
    theRun = unfoldr go (start, start)
    go :: (Point, Point) -> Maybe (Point, (Point, Point))
    go (prev, pos) = do
      let neighbours = gridOrthogonalNeighbours grid pos
      nextStep <-
        M.filterWithKey
          (\k v -> (v `elem` ".E") && k /= prev)
          neighbours
          & M.keys
          & headMay -- If we got nothing we hit the end
      pure (nextStep, (pos, nextStep))

manhattanDistanceWithMax :: Int -> Point -> Point -> Maybe Int
manhattanDistanceWithMax max p1 p2 =
  let d = manhattanDistance p1 p2
   in if d > max
        then Nothing
        else Just d

pairsNApart :: Int -> [a] -> [(a, a)]
pairsNApart n xs = zip xs l2
  where
    l2 = drop n xs

-- | Returns a list of the possible ps saved for this route and skipLength
savings :: [V2 Int] -> Int -> Int -> [Int]
savings route maxSkip skipLength = map (skipLength -) mDistances
  where
    mDistances =
      pairsNApart skipLength route
        & mapMaybe (uncurry (manhattanDistanceWithMax maxSkip))
