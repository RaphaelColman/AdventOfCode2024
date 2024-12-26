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
    _crates :: !(S.Set (V2 Int))
  }
  deriving (Eq, Show)

type Direction = V2 Int

makeLenses ''WarehouseState

aoc15 :: IO ()
aoc15 = do
  -- printSolutions 15 $ MkAoCSolution (parseInput False) part1
  printSolutions 15 $ MkAoCSolution (parseInput True) part2

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

part1 input = traceLns rendered $ gpsChecksum solved
  where
    (grid, directions) = input
    state = initState grid
    solved = doMoves directions state
    rendered = renderVectorMap $ stateToMap False solved

part2 input = gpsChecksum solved
  where
    (grid, directions) = input
    state = initState grid
    solved = doMoves directions state
    rendered = renderVectorMap $ stateToMap True solved
    g = gpsLocationOfExpandedCrate state (V2 5 1)

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
    crates = M.keysSet $ M.filter (`elem` ['O', '[']) grid

doMoves :: [Direction] -> WarehouseState -> WarehouseState
doMoves dirs state = foldl' (flip moveRobot) state dirs

moveRobot :: V2 Int -> WarehouseState -> WarehouseState
moveRobot direction state = fromMaybe state (moveRobotMaybe direction state)

moveRobotMaybe :: V2 Int -> WarehouseState -> Maybe WarehouseState
moveRobotMaybe direction state = do
  cratesToMove <- getCratesToMove2 direction state
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

getCratesToMove2 :: V2 Int -> WarehouseState -> Maybe (S.Set Point)
getCratesToMove2 direction state = do
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

stateToMap :: Bool -> WarehouseState -> Grid Char
stateToMap expanded (MkWarehouseState robot walls crates) =
  if expanded
    then M.unions [wallMap, robotMap, cl, cr]
    else M.unions [wallMap, crateMap, robotMap]
  where
    wallMap = M.fromSet (const '#') walls
    robotMap = M.singleton robot '@'
    crateMap = M.fromSet (const 'O') crates
    cl = M.fromSet (const '[') crates
    cr = M.fromSet (const ']') $ S.map (\v -> v + V2 1 0) crates

gpsChecksum :: WarehouseState -> Int
gpsChecksum state =
  state ^. crates
    & S.map (\(V2 x y) -> x + y * 100)
    & sum

expandedGpsChecksum :: WarehouseState -> Int
expandedGpsChecksum state =
  state ^. crates
    & S.map (\pt -> let (V2 x y) = gpsLocationOfExpandedCrate state pt
                    in x + y * 100
            )
    & sum

gpsLocationOfExpandedCrate :: WarehouseState -> Point -> Point
gpsLocationOfExpandedCrate state (V2 leftX topY) = V2 x y
  where
    maxX = maximum $ S.map (^. _x) (state ^. walls)
    maxY = maximum $ S.map (^. _y) (state ^. walls)
    rightX = maxX - leftX + 1
    bottomY = maxY - topY
    x = min leftX rightX
    y = min topY bottomY

directionToString :: V2 Int -> String
directionToString v = case v of
  V2 0 (-1) -> "^"
  V2 0 1 -> "v"
  V2 (-1) 0 -> "<"
  V2 1 0 -> ">"
  _ -> "?"
