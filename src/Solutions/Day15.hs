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
import Common.Geometry (Grid, Point, enumerateMultilineStringToVectorMap, renderVectorMap, renderVectorSet)
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
import Debug.Trace (traceM, traceShow, traceShowM)
import Linear (unit)
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Parser.Char (CharParsing (string), oneOf)
import Text.Parser.Combinators (manyTill, some)
import Text.Parser.Token (token)
import Text.Trifecta (Parser)

data WarehouseState = MkWarehouseState
  { _robot :: !(V2 Int),
    _walls :: !(S.Set (V2 Int)),
    _crates :: !(S.Set (V2 Int)),
    _expanded :: !Bool
  }
  deriving (Eq, Show)

type Direction = V2 Int

makeLenses ''WarehouseState

aoc15 :: IO ()
aoc15 = do
  printTestSolutions 15 $ MkAoCSolution (parseInput False) part1
  printTestSolutions 15 $ MkAoCSolution (parseInput True) part2

parseInput :: Bool -> Parser (Grid Char, [V2 Int])
parseInput expand = do
  gr <- parseGridCharacters expand
  ds <- fmap concat $ some $ token $ some parseDirection
  pure (gr, ds)

parseGridCharacters :: Bool -> Parser (Grid Char)
parseGridCharacters shouldExpand = do
  gridAsString <- manyTill (oneOf "#.O@\n") (string "\n\n")
  pure $ enumerateMultilineStringToVectorMap $ if shouldExpand then expandGrid gridAsString else gridAsString
  where
    expandGrid :: String -> String
    expandGrid str = do
      c <- str
      case c of
        '#' -> "##"
        'O' -> "[]"
        '.' -> ".."
        '@' -> "@."
        '\n' -> "\n"

parseDirection :: Parser (V2 Int)
parseDirection = do
  c <- oneOf "<^>v"
  case c of
    '<' -> return $ -(unit _x)
    '^' -> return (-unit _y)
    '>' -> return (unit _x)
    'v' -> return $ unit _y
    _ -> fail "Invalid direction character"

part1 = solve False

part2 = solve True

solve :: Bool -> (Grid Char, [Direction]) -> Int
solve expanded input = gpsChecksum solved
  where
    (grid, directions) = input
    state = initState expanded grid
    solved = doMoves directions state

findRobot :: Grid Char -> V2 Int
findRobot grid =
  M.filter (== '@') grid
    & M.keys
    & head

initState :: Bool -> Grid Char -> WarehouseState
initState expanded grid = MkWarehouseState robot walls crates expanded
  where
    robot = findRobot grid
    walls = M.keysSet $ M.filter (== '#') grid
    crates = M.keysSet $ M.filter (`elem` ['O', '[']) grid

doMoves :: [Direction] -> WarehouseState -> WarehouseState
doMoves dirs state = foldl' (flip moveRobot) state dirs

moveRobot :: V2 Int -> WarehouseState -> WarehouseState
moveRobot direction state = fromMaybe state (moveRobotMaybe direction state)

moveRobotMaybe :: V2 Int -> WarehouseState -> Maybe WarehouseState
moveRobotMaybe direction state = do
  let getCratesFunc = if state ^. expanded then getCratesToMoveExpanded else getCratesToMove
  cratesToMove <- getCratesFunc direction state
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
  pure $ MkWarehouseState newRobot (state ^. walls) newCrates (state ^. expanded)

getCratesToMove :: V2 Int -> WarehouseState -> Maybe (S.Set Point)
getCratesToMove direction state = do
  let nextPos = state ^. robot + direction
  S.fromList <$> unfoldrM go nextPos
  where
    go :: V2 Int -> Maybe (Maybe (V2 Int, V2 Int))
    go pt = do
      guard $ not $ pt `S.member` (state ^. walls) -- Stop entirely if we've hit a wall
      if pt `S.member` (state ^. crates)
        then Just (Just (pt, pt + direction)) -- continue unfolding
        else Just Nothing -- Stop unfolding.

getCratesToMoveExpanded :: V2 Int -> WarehouseState -> Maybe (S.Set Point)
getCratesToMoveExpanded direction state = do
  let nextPos = state ^. robot + direction
  sets <- unfoldrM go $ S.singleton nextPos
  pure $ S.unions sets
  where
    go :: S.Set Point -> Maybe (Maybe (S.Set Point, S.Set Point))
    go pts = do
      guard $ pts `S.disjoint` (state ^. walls)
      let matchingCrates =
            S.filter
              ( \cratePt ->
                  any (`S.member` pts) [cratePt, cratePt + V2 1 0]
              )
              $ state ^. crates
      let nextPositions = nextCratePositions direction matchingCrates
      if not (null matchingCrates)
        then Just (Just (matchingCrates, nextPositions)) -- continue unfolding
        else Just Nothing -- Stop unfolding.

-- | A setof the next position to look in once we've found some crates we're moving
nextCratePositions :: V2 Int -> S.Set Point -> S.Set Point
nextCratePositions direction pts = case direction of
  V2 0 (-1) -> S.foldr (\v acc -> S.insert (v + V2 1 0) acc) pts pts & S.map (+ direction)
  V2 0 1 -> S.foldr (\v acc -> S.insert (v + V2 1 0) acc) pts pts & S.map (+ direction)
  V2 (-1) 0 -> S.map (+ direction) pts
  V2 1 0 -> S.map (+ 2 * direction) pts
  _ -> error "Invalid direction"

gpsChecksum :: WarehouseState -> Int
gpsChecksum state =
  state ^. crates
    & S.map (\(V2 x y) -> x + y * 100)
    & sum

stateToMap :: WarehouseState -> Grid Char
stateToMap (MkWarehouseState robot walls crates expanded) =
  if expanded
    then M.unions [wallMap, robotMap, cl, cr]
    else M.unions [wallMap, crateMap, robotMap]
  where
    wallMap = M.fromSet (const '#') walls
    robotMap = M.singleton robot '@'
    crateMap = M.fromSet (const 'O') crates
    cl = M.fromSet (const '[') crates
    cr = M.fromSet (const ']') $ S.map (\v -> v + V2 1 0) crates
