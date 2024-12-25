module Solutions.Day12
  ( aoc12,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Debugging (traceLns)
import Common.Geometry (Grid, Point, allOrthogonalDirections, allOrthogonalNeighbours, enumerateMultilineStringToVectorMap, gridOrthogonalNeighbours, renderVectorSet)
import Common.ListUtils (window2)
import Common.MapUtils (associateBy)
import Control.Lens ((^.))
import Data.Function ((&))
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable (for)
import Debug.Trace
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Safe (headMay)
import Text.Printf (printf)
import Text.Trifecta (CharParsing (anyChar), Parser, some)

type Region = S.Set Point

type Side = [Point]

type PlantType = Char

aoc12 :: IO ()
aoc12 = do
  printTestSolutions 12 $ MkAoCSolution parseInput part1
  printSolutions 12 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> some anyChar

part1 input = sum $ map cost regions
  where
    regions = findRegions input

part2 input = sum $ map costWithSides regions
  where
    regions = findRegions input

-- | Given a grid and a point, expand the region of the same plant type as the point
-- Seems like we should be able to do this using unfold or ana
expandToRegion :: Grid PlantType -> Point -> Region
expandToRegion grid point = go (S.singleton point) (S.singleton point)
  where
    currentType = grid M.! point
    go :: Region -> S.Set Point -> Region
    go region toExplore
      | null newNeighbours = region -- No new points to add
      | otherwise = go newRegion newNeighbours
      where
        plantNeighbours p =
          gridOrthogonalNeighbours grid p
            & M.filter (== currentType)
            & M.keysSet
        newNeighbours =
          S.map plantNeighbours toExplore
            & S.unions
            & (`S.difference` region)
        newRegion = region `S.union` newNeighbours

findRegions :: Grid PlantType -> [Region]
findRegions grid = go [] (M.keys grid)
  where
    go :: [Region] -> [Point] -> [Region]
    go regions [] = regions
    go regions (pt : pts) = go newRegions (S.toList remainingPoints)
      where
        thisRegion = expandToRegion grid pt
        remainingPoints = S.fromList pts `S.difference` S.unions newRegions
        newRegions = thisRegion : regions

perimeter :: Region -> Int
perimeter region = length $ filter (`S.notMember` region) surroundingPoints
  where
    surroundingPoints = concatMap (S.toList . allOrthogonalNeighbours) (S.toList region)

cost :: Region -> Int
cost region = perimeter region * S.size region

costWithSides :: Region -> Int
costWithSides region = countSides region * S.size region

countSidesForDirection :: Region -> V2 Int -> Int
countSidesForDirection region direction = sum $ M.map countSidesForGroup grouped
  where
    neighboursInDirection = S.map (+ direction) region & (`S.difference` region)
    groupByDimension = if isVertical direction then (^. _y) else (^. _x)
    grouped = associateBy groupByDimension neighboursInDirection
    countSidesForGroup pts =
      let xs = sort $ map (\(V2 x y) -> if isVertical direction then x else y) pts
       in window2 xs
            & filter (\(a, b) -> b - a > 1)
            & length
            & (+ 1) -- There is always at least one

countSides :: Region -> Int
countSides region =
  allOrthogonalDirections
    & map (countSidesForDirection region)
    & sum

isVertical :: V2 Int -> Bool
isVertical (V2 x y) = x == 0
