{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day14
  ( aoc14,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.FunctorUtils (fmap2)
import Common.ListUtils (freqs)
import Control.Lens (makeLenses, over, (^.))
import Data.Finite
import Data.Foldable (find)
import Data.Function ((&))
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Range
import Linear (V2 (V2))
import Text.Parser.Combinators (some)
import Text.Trifecta (CharParsing (string), Parser, commaSep, integer)

type Bounds = V2 Int

type Quadrant = Finite 4

data Robot = MkRobot
  { _position :: !(V2 Int),
    _direction :: !(V2 Int)
  }
  deriving (Eq, Show)

makeLenses ''Robot

aoc14 :: IO ()
aoc14 = do
  printSolutions 14 $ MkAoCSolution parseInput part1

-- printSolutions 14 $ MkAoCSolution parseInput part2

parseInput :: Parser [Robot]
parseInput = some parseRobot

parseRobot :: Parser Robot
parseRobot = do
  [pX, pY] <- string "p=" *> commaSep integer & fmap2 fromIntegral
  [vX, vY] <- string "v=" *> commaSep integer & fmap2 fromIntegral
  pure $ MkRobot (V2 pX pY) (V2 vX vY)

part1 input = product $ M.elems quadrantCounts
  where
    bounds = V2 101 103
    finalLocations = iterate (moveRobots bounds) input !! 100
    quadrantCounts = freqs $ mapMaybe (quadrant bounds . _position) finalLocations

part2 :: String -> String
part2 = undefined

moveRobot :: Bounds -> Robot -> Robot
moveRobot (V2 maxX maxY) robot = robot & over position (wrapVector . (+ (robot ^. direction)))
  where
    wrapVector (V2 x y) = V2 (x `mod` maxX) (y `mod` maxY)

moveRobots :: Bounds -> [Robot] -> [Robot]
moveRobots bounds = fmap (moveRobot bounds)

quadrant :: V2 Int -> V2 Int -> Maybe Quadrant
quadrant (V2 maxX maxY) p = finite . fst <$> found
  where
    tl = (0 +=* halfX, 0 +=* halfY)
    tr = (halfX *=* maxX, 0 +=* halfY)
    br = (0 +=* halfX, halfY *=* maxY)
    bl = (halfX *=* maxX, halfY *=* maxY)
    halfX = maxX `div` 2
    halfY = maxY `div` 2
    satisfiesRanges (V2 x y) (xRange, yRange) = xRange `inRange` x && yRange `inRange` y
    found = find (\(i, ranges) -> satisfiesRanges p ranges) $ zip [0 ..] [tl, tr, br, bl]
