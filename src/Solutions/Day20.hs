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
import Common.Geometry (Grid, Point, allOrthogonalNeighbours, enumerateMultilineStringToVectorMap, gridOrthogonalNeighbours)
import Common.ListUtils (windowN)
import Control.Lens (makeLenses)
import Control.Lens.Getter ((^.))
import Data.Function ((&))
import Data.List (unfoldr)
import qualified Data.Map as M
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

-- printSolutions 20 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> some anyChar

part1 input = sum cheats
  where
    cheatless = runRoute input
    cheats = map (findCheat cheatless) [100 .. (length cheatless - 1)]

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

twoSpacesApart :: Point -> Point -> Bool
twoSpacesApart p1 p2 = p2 - p1 `elem` [V2 0 2, V2 0 (-2), V2 2 0, V2 (-2) 0]

pairsNApart :: Int -> [a] -> [(a, a)]
pairsNApart n xs = zip xs l2
  where
    l2 = drop n xs

findCheat :: [V2 Int] -> Int -> Int
findCheat route saved =
  pairsNApart (saved + 2) route
    & filter (uncurry twoSpacesApart)
    & length
