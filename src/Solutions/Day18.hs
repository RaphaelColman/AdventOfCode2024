{-# LANGUAGE TupleSections #-}

module Solutions.Day18
  ( aoc18,
  )
where

import Algorithm.Search (aStarAssoc)
import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.FunctorUtils ( fmap2 )
import Common.Geometry (Point, allOrthogonalNeighbours, manhattanDistance, gridSize)
import Control.Lens (makeLenses)
import Control.Lens.Getter ((^.))
import Control.Monad.Reader (MonadReader (ask), Reader)
import Data.Function ((&))
import Data.Range (inRange, (+=+))
import qualified Data.Set as S
import Linear.V2 (V2 (..), _x, _y)
import Text.Parser.Combinators ( some )
import Text.Trifecta (Parser, commaSep, integer)
import Data.List (inits, find)
import Data.Maybe (isJust)

{-
 - Test input: 6,6 grid
 - Real input 70,70 grid
 -}

type Bounds = V2 Int

aoc18 :: IO ()
aoc18 = do
  printSolutions 18 $ MkAoCSolution parseInput part1
  printSolutions 18 $ MkAoCSolution parseInput part2

parseInput :: Parser [V2 Int]
parseInput = some pt
  where
    pt :: Parser (V2 Int)
    pt = do
      [x, y] <- fmap2 fromIntegral $ commaSep integer
      pure (V2 x y)

part1 :: [Point] -> Maybe Int
part1 input = findShortestPath bounds corruptedPts
  where
    corruptedPts = S.fromList $ take 1024 input
    bounds = V2 70 70

-- | We're better off starting with the completely corrupted grid
-- and working backwards, because for those almostly nearly-full grids
-- it's quite fast to determine that there is no path to the destination.
part2 :: [Point] -> Maybe Point
part2 input = fmap (input !!) $ length <$> find hasPath corruptedPts
  where
    corruptedPts = reverse $ inits input
    bounds = V2 70 70
    hasPath = isJust . (findShortestPath bounds . S.fromList)

neighbours :: Bounds -> S.Set Point -> Point -> [Point]
neighbours bounds corrupted position =
  S.toList $
    allOrthogonalNeighbours position
      & S.filter (\p -> (p `S.notMember` corrupted) && p `inBounds` bounds)

inBounds :: Point -> Bounds -> Bool
inBounds (V2 x y) bounds = inRange xRange x && inRange yRange y
  where
    xRange = 0 +=+ (bounds ^. _x)
    yRange = 0 +=+ (bounds ^. _y)

findShortestPath :: Bounds -> S.Set Point -> Maybe Int
findShortestPath bounds corrupted = fst <$> aStarAssoc next (manhattanDistance bounds) (== bounds) (V2 0 0)
  where
    next = map (,1 :: Int) . neighbours bounds corrupted
