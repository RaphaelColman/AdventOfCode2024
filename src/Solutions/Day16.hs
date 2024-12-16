{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day16
  ( aoc16,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry (Grid, Point, enumerateMultilineStringToVectorMap, renderVectorMap, gridOrthogonalNeighbours)
import Control.Lens (makeLenses, to, (%~), (^.))
import Control.Lens.Setter ((.~))
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Reader (Reader, ask, lift, liftM, runReader)
import Data.Function ((&))
import Data.Graph.AStar (aStar, aStarM)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics (Generic)
import Linear.V2 (V2 (V2), perp, _x, _y)
import Text.Parser.Combinators (some)
import Text.Trifecta (CharParsing (anyChar), Parser)
import Common.ApplicativeUtils (liftResult)
import Debug.Trace

data ReindeerState = MkReindeerState
  { _position :: !Point,
    _direction :: !Point,
    _costSoFar :: !Int,
    _target :: !Point,
    _pointsVisited :: !(HS.HashSet Point)
  }
  deriving (Show, Eq, Generic, Ord)

instance Hashable ReindeerState

type GridChar = Grid Char -- So I don't need to keep putting parentheses around Grid Char

makeLenses ''ReindeerState

aoc16 :: IO ()
aoc16 = do
  printTestSolutions 16 $ MkAoCSolution parseInput part1

-- printSolutions 16 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  enumerateMultilineStringToVectorMap <$> some anyChar

part1 input = _costSoFar . last <$> solved
  where
    (initialState, grid) = initState input
    solved = runReader (solve initialState) grid

findStart :: Grid Char -> Point
findStart g = M.filter (== 'S') g & M.keys & head

findEnd :: Grid Char -> Point
findEnd g = M.filter (== 'E') g & M.keys & head

initState :: Grid Char -> (ReindeerState, Grid Char)
initState g = (state, newGrid)
  where
    start = findStart g
    end = findEnd g
    state = MkReindeerState start (V2 1 0) 0 end (HS.singleton start)
    newGrid = g & M.adjust (const '.') start & M.adjust (const '.') end

neighbours :: ReindeerState -> Reader GridChar (HS.HashSet ReindeerState)
neighbours state = do
  nextStates <- mapMaybeM (\f -> f state) [moveForward, turnRight, turnLeft]
  pure $ HS.fromList nextStates

-- Should this be a MaybeT? Or a ReaderT? Not sure yet
moveForward :: ReindeerState -> Reader GridChar (Maybe ReindeerState)
moveForward state = do
  grid <- ask
  let nextPos = state ^. position + state ^. direction
  if (grid M.! nextPos) == '#' || nextPos `HS.member` (state ^. pointsVisited)
    then pure Nothing
    else pure $ Just $ state & position .~ nextPos & costSoFar %~ (+ 1) & pointsVisited %~ HS.insert nextPos

turnRight :: ReindeerState -> Reader GridChar (Maybe ReindeerState)
turnRight state = do
  let newDirection = state ^. direction & perp
  moveForward $ state & costSoFar %~ (+ 1000) & direction .~ newDirection

turnLeft :: ReindeerState -> Reader GridChar (Maybe ReindeerState)
turnLeft state = do
  let newDirection = state ^. direction & (* (-1)) & perp
  moveForward $ state & costSoFar %~ (+ 1000) & direction .~ newDirection

-- | https://hackage.haskell.org/package/search-algorithms-0.3.
-- Comes with base. I should probably switch to that because it has way more algorithms
solve :: ReindeerState -> Reader GridChar (Maybe [ReindeerState])
solve state = do
  aStarM neighbours costFun (liftResult heuristic) (liftResult goalReached) (pure state)

-- There must be a way to lift this into the Reader monad
costFun :: ReindeerState -> ReindeerState -> Reader GridChar Int
costFun state1 state2 = pure $ state2 ^. costSoFar - state1 ^. costSoFar

-- | The heuristic will be the manhattan distance from current position to target
heuristic :: ReindeerState -> Int
heuristic state = stepCost + state ^. costSoFar
  where
    (V2 currentX currentY) = state ^. position
    (V2 targetX targetY) = state ^. target
    stepCost = abs (currentX - targetX) + abs (currentY - targetY)

goalReached :: ReindeerState -> Bool
goalReached state = state ^. position == state ^. target


render :: ReindeerState -> Reader GridChar String
render state = do
  grid <- ask
  let grid' = M.insert (state ^. position) dChar grid
  pure $ renderVectorMap grid'
  where dChar = case state ^. direction of
          (V2 0 1) -> 'v'
          (V2 0 (-1)) -> '^'
          (V2 1 0) -> '>'
          (V2 (-1) 0) -> '<'
          _ -> 'X'

isAtJunctionOrEnd :: ReindeerState -> Reader GridChar Bool
isAtJunctionOrEnd state = do
  grid <- ask
  let neighbouringPoints = gridOrthogonalNeighbours grid (state ^. position)
  pure undefined
