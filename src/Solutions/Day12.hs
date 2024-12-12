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
import Common.Geometry (Grid, Point, allOrthogonalNeighbours, enumerateMultilineStringToVectorMap, gridOrthogonalNeighbours, renderVectorSet)
import Data.Function ((&))
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import Linear.V2 (V2 (V2))
import Text.Printf (printf)
import Text.Trifecta (CharParsing (anyChar), Parser, some)

type Region = S.Set Point

type PlantType = Char

aoc12 :: IO ()
aoc12 = do
  printSolutions 12 $ MkAoCSolution parseInput part1

-- printSolutions 12 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> some anyChar

part1 input = sum $ map cost regions
  where
    regions = findRegions input

part2 :: String -> String
part2 = undefined

-- | Given a grid and a point, expand the region of the same plant type as the point
-- Seems like we should be able to do this using unfold or cata
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
