{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day15
  ( aoc15,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Debugging (traceLns)
import Common.Geometry (Grid, enumerateMultilineStringToVectorMap, renderVectorMap, renderVectorSet)
import Control.Lens (makeLenses, (.~))
import Control.Lens.Getter ((^.))
import Control.Monad.Loops (unfoldrM)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), State, StateT (runStateT), execState, gets, guard, modify)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Foldable
import Data.Function ((&))
import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Debug.Trace (traceShow, traceShowM)
import Linear (unit)
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Parser.Char (CharParsing (string), oneOf)
import Text.Parser.Combinators (manyTill, some)
import Text.Trifecta (Parser)
import Text.Parser.Token (token)

data WarehouseState = MkWarehouseState
  { _robot :: !(V2 Int),
    _walls :: !(S.Set (V2 Int)),
    _crates :: !(S.Set (V2 Int))
  }
  deriving (Eq, Show)

type Direction = V2 Int

makeLenses ''WarehouseState

aoc15 :: IO ()
aoc15 = do
  printSolutions 15 $ MkAoCSolution parseInput part1

-- printSolutions 15 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char, [V2 Int])
parseInput = do
  gr <- parseGridCharacters
  ds <- fmap concat $ some $ token $ some parseDirection
  pure (gr, ds)

parseGridCharacters :: Parser (Grid Char)
parseGridCharacters = do
  gridAsString <- manyTill (oneOf "#.O@\n") (string "\n\n")
  pure $ enumerateMultilineStringToVectorMap gridAsString

parseDirection :: Parser (V2 Int)
parseDirection = do
  c <- oneOf "<^>v"
  case c of
    '<' -> return $ -(unit _x)
    '^' -> return (-unit _y)
    '>' -> return (unit _x)
    'v' -> return $ unit _y
    _ -> fail "Invalid direction character"

part1 input = traceLns rendered $ gpsChecksum solved
  where
    (grid, directions) = input
    state = initState grid
    solved = doMoves directions state
    rendered = renderVectorMap $ stateToMap solved
    firstMove = moveRobot (V2 (-1) 0) state

findRobot :: Grid Char -> V2 Int
findRobot grid =
  M.filter (== '@') grid
    & M.keys
    & head

initState :: Grid Char -> WarehouseState
initState grid = MkWarehouseState robot walls crates
  where
    robot = findRobot grid
    walls = M.keysSet $ M.filter (== '#') grid
    crates = M.keysSet $ M.filter (== 'O') grid

doMoves :: [Direction] -> WarehouseState -> WarehouseState
doMoves dirs state = foldl' (flip moveRobot) state dirs

moveRobot :: V2 Int -> WarehouseState -> WarehouseState
moveRobot direction state = fromMaybe state (moveRobotMaybe direction state)
  where
    rendered = renderVectorMap $ stateToMap state

moveRobotMaybe :: V2 Int -> WarehouseState -> Maybe WarehouseState
moveRobotMaybe direction state = do
  cratesToMove <- getCratesToMove direction state
  let newCrates =
        S.map
          ( \c ->
              if c `elem` cratesToMove
                then c + direction
                else c
          )
          (state ^. crates)
  -- If the next space were a wall we'd have returned a Nothing already
  let newRobot = state ^. robot + direction
  pure $ MkWarehouseState newRobot (state ^. walls) newCrates

getCratesToMove :: V2 Int -> WarehouseState -> Maybe [V2 Int]
getCratesToMove direction state = do
  let nextPos = state ^. robot + direction
  unfoldrM go nextPos
  where
    go :: V2 Int -> Maybe (Maybe (V2 Int, V2 Int))
    go pt = do
      guard $ not $ pt `S.member` (state ^. walls) -- Stop entirely if we've hit a wall
      if pt `S.member` (state ^. crates)
        then Just (Just (pt, pt + direction)) -- continue unfolding
        else Just Nothing -- Stop unfolding.

stateToMap :: WarehouseState -> Grid Char
stateToMap (MkWarehouseState robot walls crates) = M.unions [wallMap, crateMap, robotMap]
  where
    wallMap = M.fromSet (const '#') walls
    crateMap = M.fromSet (const 'O') crates
    robotMap = M.singleton robot '@'

gpsChecksum :: WarehouseState -> Int
gpsChecksum state = state ^. crates
  & S.map (\(V2 x y) -> x + y * 100)
  & sum
