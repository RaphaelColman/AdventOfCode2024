{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Solutions.Day6
  ( aoc6,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.EnumUtils (enumNext)
import Common.Geometry (Grid, Point, enumerateMultilineStringToVectorMap, renderVectorMap, renderVectorSet)
import Control.Lens ((^.))
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import Linear (unit)
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Trifecta (Parser, anyChar, some)
import Common.Floyd (CycleData, hareAndTortoiseM)
import Common.Debugging

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
  --printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  enumerateMultilineStringToVectorMap <$> some anyChar

part1 input = numberOfDistinctGuardPositions $ runMachine state
  where
    state = initState input

--Dang this goes forever at the moment
part2 input = length $ filter (hasCycle . addObstacleToState state) newObstacles 
  where
    state = initState input
    newObstacles = enumeratePossibleNewObstacles state
    testObstacles = [V2 3 6, V2 6 7, V2 7 7, V2 1 8, V2 3 8, V2 7 9]
    --[True,False,True,False,False,False] (so only two of these are cycle-inducing according to my code!)
    startingStates = map (addObstacleToState state) $ enumeratePossibleNewObstacles state

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
addObstacle s@MkState{..} point = s { _obstacles = S.insert point _obstacles }

enumeratePossibleStartingStates :: State -> [State]
enumeratePossibleStartingStates s@MkState{..} = map (addObstacle s) locations
  where locations = [V2 x y | x <- [0.._grid ^. _x], y <- [0.._grid ^. _y]]

directionToUnitVector :: Direction -> V2 Int
directionToUnitVector North = -(unit _y)
directionToUnitVector South = unit _y
directionToUnitVector East = unit _x
directionToUnitVector West = -(unit _x)

stepStateAndCheckInBounds :: State -> Maybe State
stepStateAndCheckInBounds state = let newState = stepState state
                                  --in traceLns (renderState newState) $ if isInBounds newState then Just newState else Nothing
                                  in if isInBounds newState then Just newState else Nothing

--Test adding an obstacle at (4,9) which should cause a cycle
hasCycle :: State -> Bool
hasCycle s = hareAndTortoiseM stepStateAndCheckInBounds s id

renderState :: State -> String
renderState MkState{..} = rendered
  where guardChar = case _direction of
                      North -> '^'
                      South -> 'v'
                      East -> '>'
                      West -> '<'
        gridMap = M.fromList [(_guard, guardChar)]
        allObstacles = M.fromSet (const '#') _obstacles
        allPoints = M.union gridMap allObstacles
        rendered = renderVectorMap allPoints

enumeratePossibleNewObstacles :: State -> [Point]
enumeratePossibleNewObstacles s = S.toList newObstacles
  where allPoints = S.fromList $ [V2 x y | x <- [0..s._grid^._x], y <- [0..s._grid^._y]]
        newObstacles = S.difference allPoints (S.insert s._guard s._obstacles)

addObstacleToState :: State -> Point -> State
addObstacleToState s@MkState{..} loc = s { _obstacles = S.insert loc _obstacles }


-- Coords of cycle-inducing obstacles according to puzzle
-- [V2 3 6, V2 6 7, V2 7 7, V2 1 8, V2 3 8, V2 7 9]
-- The ones I think are cycle inducing but aren't: (4 4), (5 1), (7 1)
