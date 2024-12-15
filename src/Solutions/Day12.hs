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
import Data.Traversable (for)

type Region = S.Set Point
type Side = [Point]
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


-- | This has to take all "outer" points to work
countSides :: Region -> Int
countSides region = go [[]] region
  where
    point = S.findMin region
    go :: [Side] -> Region -> Int
    go sides remainingPoints
      | null sides = undefined go [[pt]] newRemainingPoints
      | null remainingPoints = length sides
      | null vertNeighbours && null horizontalNeighbours = go ([pt] : sides) (S.delete pt remainingPoints)
      | not (null vertNeighbours) && not (null horizontalNeighbours) = error "Both horizontal and vertical neighbours found"
      | not (null vertNeighbours) = go [[]] (remainingPoints `S.difference` vertNeighbours)
      --This won't work. Once we've added the vert neighbours we need to repeat the algorithm for
      -- the 1 or 2 points we've added. but this will just go and look in remainingPoints.
      -- So maybe we have a function which is "expand pt to side" which returns a side from a point
      where
        pt = S.findMin remainingPoints -- Will this be slow? Let's try it and see
        vertNeighbours = S.fromList [pt + V2 0 1, pt + V2 0 (-1)] `S.intersection` remainingPoints
        horizontalNeighbours = S.fromList [pt + V2 1 0, pt + V2 (-1) 0] `S.intersection` remainingPoints
        newRemainingPoints = S.delete pt remainingPoints

{-
 - Finding the number of sides
 - 1. find all the points outside of the region (including duplicates). Call this oRegion
 - 2. Pick a point. Expand either Horizontally or Vertically using points in oRegion (bomb out if you can do both, this should never happen)
 - 3. Keep expanding until you can't anymore. You've found a side.
 - 4. Pick another point and repeat
 -}
