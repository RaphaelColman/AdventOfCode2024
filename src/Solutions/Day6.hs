{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Solutions.Day6
  ( aoc6,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry (Grid, Point, enumerateMultilineStringToVectorMap, renderVectorMap)
import Control.Lens ((^.))
import Data.Foldable (maximumBy)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Set as S
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Trifecta (Parser, anyChar, some)
import Linear (unit)
import Common.EnumUtils (enumNext)

data State = MkState
  { _guard :: !(V2 Int),
    _direction :: !Direction,
    _obstacles :: !(S.Set Point),
    _grid :: !Point -- Represents the maximum x and maximum y
  }
  deriving (Eq, Show)

data Direction = North | East | South | West
  deriving (Eq, Show, Bounded, Enum)

aoc6 :: IO ()
aoc6 = do
  printTestSolutions 6 $ MkAoCSolution parseInput part1

-- printSolutions 6 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  enumerateMultilineStringToVectorMap <$> some anyChar

part1 input = state
  where state = initState input

-- part2 :: String -> String
-- part2 = undefined

initState :: Grid Char -> State
initState grid = MkState guard North obstacles (V2 xMax yMax)
  where
    [(guard, _)] = M.toList $ M.filter (== '^') grid
    obstacles = M.keysSet $ M.filter (== '#') grid
    (V2 xMax _) = maximumBy (compare `on` (^. _x)) $ M.keys grid
    (V2 _ yMax) = maximumBy (compare `on` (^. _y)) $ M.keys grid


stepState :: State -> Maybe State
stepState state
  | not (isInBounds state) = Nothing
  | isTouchingObstacle state = Just $ turnRight state
  | otherwise = walk

isTouchingObstacle :: State -> Bool
isTouchingObstacle MkState{..} = nextPoint `S.member` _obstacles
  where nextPoint = _guard + directionToUnitVector _direction

turnRight :: State -> State
turnRight s@MkState{..} = s { _direction = enumNext _direction }

isInBounds :: State -> Bool
isInBounds (MkState (V2 gx gy) _ _ (V2 xMax yMax)) = gx >= 0 && gx <= xMax && gy >= 0 && gy <= yMax

walk :: State -> State
walk s@MkState{..} = s { _guard = _guard + directionToUnitVector _direction }

directionToUnitVector :: Direction -> V2 Int
directionToUnitVector North     = -(unit _y)
directionToUnitVector South     = unit _y
directionToUnitVector East      = unit _x
directionToUnitVector West      = -(unit _x)
