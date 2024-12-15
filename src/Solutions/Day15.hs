{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Solutions.Day15
  ( aoc15,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry (Grid, enumerateMultilineStringToVectorMap)
import Control.Lens (makeLenses, (.~))
import Data.Foldable
import Data.Function ((&))
import qualified Data.Map as M
import qualified Data.Set as S
import Linear (unit)
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Parser.Char (CharParsing (string), oneOf)
import Text.Parser.Combinators (manyTill, some)
import Text.Trifecta (Parser)
import Control.Monad.State (State, MonadState (get, put), StateT (runStateT), gets, MonadTrans (lift), modify, execState)
import Control.Lens.Getter ((^.))
import Data.List (unfoldr)
import Control.Monad.Loops (unfoldrM)

data WarehouseState = MkWarehouseState
  { _robot :: !(V2 Int),
    _grid :: !(M.Map (V2 Int) Char)
  }
  deriving (Eq, Show)

makeLenses ''WarehouseState

aoc15 :: IO ()
aoc15 = do
  printTestSolutions 15 $ MkAoCSolution parseInput part1

-- printSolutions 15 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char, [V2 Int])
parseInput = do
  gr <- parseGridCharacters
  ds <- some parseDirection
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

part1 input = getCratesToMove' (V2 1 0) (V2 2 1) gr
  where
    (grid, directions) = input
    MkWarehouseState robot gr = initState grid

findRobot :: Grid Char -> V2 Int
findRobot grid =
  M.filter (== '@') grid
    & M.keys
    & head

initState :: Grid Char -> WarehouseState
initState grid = MkWarehouseState robot gridWithoutRobot
  where
    robot = findRobot grid
    gridWithoutRobot = M.adjust (const '.') robot grid

moveRobot :: V2 Int -> State WarehouseState ()
moveRobot direction = do
  state <- get
  let nextPos = state._robot + direction
  let nextChar = state._grid M.! nextPos
  pure ()

getCratesToMove :: V2 Int -> StateT WarehouseState Maybe [V2 Int]
getCratesToMove direction = do
  MkWarehouseState robotPos grid <- get
  positionsToMove <- lift $ unfoldrM (go grid) robotPos
  pure undefined
    where go grid' pos = do
            let next = pos + direction
            l <- M.lookup next grid'
            case l of
              'O' -> Just $ Just (next, next)
              '.' -> Just Nothing
              '#' -> Nothing

getCratesToMove' :: V2 Int -> V2 Int -> Grid Char -> Maybe [V2 Int]
getCratesToMove' direction robot grid = objects
  where
    objects = unfoldrM go robot
    go :: V2 Int -> Maybe (Maybe (V2 Int, V2 Int))
    go pos = do
      let next = pos + direction
      l <- M.lookup next grid
      case l of
        'O' -> Just $ Just (next, next)
        '.' -> Just Nothing
        '#' -> Nothing

moveCrate :: V2 Int -> V2 Int -> State WarehouseState ()
moveCrate from direction = do
  state <- get
  let g = state._grid
  let newGrid = flip execState g $ do
                            modify $ M.adjust (const '.') from
                            modify $ M.adjust (const 'O') (from + direction)
  put $ state & grid .~ newGrid

-- (4,1) down should get some results
