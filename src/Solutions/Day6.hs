{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day6
  ( aoc6,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Debugging
import Common.EnumUtils (enumNext)
import Common.Floyd (CycleData, hareAndTortoiseM)
import Common.Geometry (Grid, Point, enumerateMultilineStringToVectorMap, renderVectorMap, renderVectorSet)
import Control.Lens ((^.), makeLenses)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import Linear (unit)
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Trifecta (Parser, anyChar, some)

data Direction = North | East | South | West
  deriving (Eq, Show, Bounded, Enum, Ord)

data State = MkState
  { _guard :: !(V2 Int),
    _direction :: !Direction,
    _obstacles :: !(S.Set Point),
    _grid :: !Point -- Represents the maximum x and maximum y
  }
  deriving (Show)

instance Eq State where
  (==) s1 s2 = _guard s1 == _guard s2 && _direction s1 == _direction s2

instance Ord State where
  compare s1 s2 = compare (_guard s1, _direction s1) (_guard s2, _direction s2)

makeLenses ''State

aoc6 :: IO ()
aoc6 = do
  --printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  enumerateMultilineStringToVectorMap <$> some anyChar

part1 input = numberOfDistinctGuardPositions $ runMachine state
  where
    state = initState input

-- Dang this goes forever at the moment
part2 input =  length $ filter (hasCycle . addObstacleToState state) newObstacles
  where
    state = initState input
    newObstacles = nub $ map _guard $ runMachine state

initState :: Grid Char -> State
initState grid = MkState guard North obstacles (V2 xMax yMax)
  where
    [(guard, _)] = M.toList $ M.filter (== '^') grid
    obstacles = M.keysSet $ M.filter (== '#') grid
    (V2 xMax _) = maximumBy (compare `on` (^. _x)) $ M.keys grid
    (V2 _ yMax) = maximumBy (compare `on` (^. _y)) $ M.keys grid

stepState :: State -> State
stepState state
  | isTouchingObstacle state = turnRight state
  | otherwise = walk state

isTouchingObstacle :: State -> Bool
isTouchingObstacle MkState {..} = nextPoint `S.member` _obstacles
  where
    nextPoint = _guard + directionToUnitVector _direction

turnRight :: State -> State
turnRight s@MkState {..} = s {_direction = enumNext _direction}

isInBounds :: State -> Bool
isInBounds (MkState (V2 gx gy) _ _ (V2 xMax yMax)) = gx >= 0 && gx <= xMax && gy >= 0 && gy <= yMax

walk :: State -> State
walk s@MkState {..} = s {_guard = _guard + directionToUnitVector _direction}

runMachine :: State -> [State]
runMachine = takeWhile isInBounds . iterate stepState

numberOfDistinctGuardPositions :: [State] -> Int
numberOfDistinctGuardPositions state = length $ nub $ map _guard state
  where
    allPositions = nub $ map _guard state

addObstacle :: State -> Point -> State
addObstacle s@MkState {..} point = s {_obstacles = S.insert point _obstacles}

enumeratePossibleStartingStates :: State -> [State]
enumeratePossibleStartingStates s@MkState {..} = map (addObstacle s) locations
  where
    locations = [V2 x y | x <- [0 .. _grid ^. _x], y <- [0 .. _grid ^. _y]]

directionToUnitVector :: Direction -> V2 Int
directionToUnitVector North = -(unit _y)
directionToUnitVector South = unit _y
directionToUnitVector East = unit _x
directionToUnitVector West = -(unit _x)

stepStateAndCheckInBounds :: State -> Maybe State
stepStateAndCheckInBounds state =
  let newState = stepState state
   in if isInBounds newState then Just newState else Nothing

hasCycle :: State -> Bool
hasCycle s = go (S.singleton s) s
  where
    go states current = let next = stepStateAndCheckInBounds current
                        in case next of
                          Nothing -> False
                          Just n -> S.member n states || go (S.insert n states) n


renderState :: State -> String
renderState MkState {..} = rendered
  where
    guardChar = case _direction of
      North -> '^'
      South -> 'v'
      East -> '>'
      West -> '<'
    gridMap = M.fromList [(_guard, guardChar)]
    allObstacles = M.fromSet (const '#') _obstacles
    allPoints = M.union gridMap allObstacles
    rendered = renderVectorMap allPoints

addObstacleToState :: State -> Point -> State
addObstacleToState s@MkState {..} loc = s {_obstacles = S.insert loc _obstacles}
