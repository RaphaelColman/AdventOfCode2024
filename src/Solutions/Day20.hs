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
import Common.ListUtils (windowN, freqs)
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
import Data.Maybe (mapMaybe)

aoc20 :: IO ()
aoc20 = do
  -- printSolutions 20 $ MkAoCSolution parseInput part1
  printSolutions 20 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> some anyChar

part1 = solve 2

part2 input = length $ filter (>= 100) foo
  where
    cheatless = runRoute input
    cheats = map (numberOfCheats cheatless 20) examples
    examples = [50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76]
    foo = concatMap (savings cheatless) [2..(length cheatless - 1)]

solve cheatDistance input = sum cheats
  where
    cheatless = runRoute input
    cheats = map (numberOfCheats cheatless cheatDistance) [76 .. (length cheatless - 1)]

solve' input = length $ filter (>= 100) foo
  where
    cheatless = runRoute input
    foo = concatMap (savings cheatless) [2..(length cheatless - 1)]

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

manhattanDistanceWithMax :: Int -> Point -> Point -> Maybe Int
manhattanDistanceWithMax max p1 p2 = let d = manhattanDistance p1 p2 in if d > max then Nothing else Just d

pairsNApart :: Int -> [a] -> [(a, a)]
pairsNApart n xs = zip xs l2
  where
    l2 = drop n xs

numberOfCheats :: [V2 Int] -> Int -> Int -> Int
numberOfCheats route cheatDistance saved =
  pairsNApart (saved + 2) route
    & filter (uncurry twoSpacesApart)
    & length

-- | Returns a list of the possible ps saved for this route and skipLength
savings :: [V2 Int] -> Int -> [Int]
savings route skipLength = map (skipLength -) mDistances
  where
    mDistances =
      pairsNApart skipLength route
        & mapMaybe (uncurry (manhattanDistanceWithMax 20))
