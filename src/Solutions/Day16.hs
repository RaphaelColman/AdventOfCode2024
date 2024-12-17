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
import Algorithm.Search (aStarM, aStarAssoc)
import Control.Monad (when)
import Common.Debugging (traceLns)
import qualified Data.Set as S

data ReindeerState = MkReindeerState
  { _position :: !Point,
    _direction :: !Point,
    _costSoFar :: !Int,
    _target :: !Point,
    _visited :: !(S.Set Point)
  }
  deriving (Show)

type GridChar = Grid Char -- So I don't need to keep putting parentheses around Grid Char

makeLenses ''ReindeerState

instance Ord ReindeerState where
  compare s1 s2 = compare (s1 ^. position) (s2 ^. position)

instance Eq ReindeerState where
  (==) s1 s2 = s1 ^. position == s2 ^. position

aoc16 :: IO ()
aoc16 = do
  printSolutions 16 $ MkAoCSolution parseInput part1

-- printSolutions 16 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  enumerateMultilineStringToVectorMap <$> some anyChar

part1 input = fst <$> solved
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
    state = MkReindeerState start (V2 1 0) 0 end (S.singleton start)
    newGrid = g & M.adjust (const '.') start & M.adjust (const '.') end

neighbours :: ReindeerState -> Reader GridChar [ReindeerState]
neighbours state = do
  nextStates <- mapMaybeM (\f -> f state) [moveForward, turnRight, turnLeft]
  let c = state ^. costSoFar
  pure nextStates

-- Should this be a MaybeT? Or a ReaderT? Not sure yet
moveForward :: ReindeerState -> Reader GridChar (Maybe ReindeerState)
moveForward state = do
  grid <- ask
  let nextPos = state ^. position + state ^. direction
  if (grid M.! nextPos) == '#' || S.member nextPos (state ^. visited)
    then pure Nothing
    else pure $ Just $ state & position .~ nextPos & costSoFar %~ (+ 1) 

turnRight :: ReindeerState -> Reader GridChar (Maybe ReindeerState)
turnRight state = do
  let newDirection = state ^. direction & perp
  moveForward $ state & costSoFar %~ (+ 1000) & direction .~ newDirection

turnLeft :: ReindeerState -> Reader GridChar (Maybe ReindeerState)
turnLeft state = do
  let newDirection = state ^. direction & (* (-1)) & perp
  moveForward $ state & costSoFar %~ (+ 1000) & direction .~ newDirection

solve :: ReindeerState -> Reader GridChar (Maybe (Int, [ReindeerState]))
solve state = do
  aStarM neighbours costFun (liftResult heuristic) (liftResult goalReached) state

-- There must be a way to lift this into the Reader monad
costFun :: ReindeerState -> ReindeerState -> Reader GridChar Int
costFun state1 state2 = pure $ state2 ^. costSoFar - state1 ^. costSoFar

-- | The heuristic will be the manhattan distance from current position to target
heuristic :: ReindeerState -> Int
heuristic state = stepCost
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
