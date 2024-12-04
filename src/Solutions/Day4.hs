module Solutions.Day4
    ( aoc4
    ) where

import qualified Combinatorics.Coin      as M
import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Geometry         (Grid, Point,
                                          enumerateMultilineStringToVectorMap)
import           Control.Lens            ((^.))
import           Control.Monad.Reader    (Reader, ReaderT (runReaderT), asks,
                                          filterM, runReader)
import           Control.Monad.RWS       (MonadReader (ask))
import           Data.Function           (on)
import           Data.List               (maximumBy, transpose)
import qualified Data.Map                as M
import qualified Data.Set                as S
import           Linear                  (unit)
import           Linear.V2               (R1 (_x), R2 (_y), V2 (..))
import           Text.Parser.Char        (CharParsing (anyChar))
import           Text.Parser.Combinators (some)
import           Text.Trifecta           (Parser)

aoc4 :: IO ()
aoc4 = do
  printSolutions 4 $ MkAoCSolution parseInput part1
  printSolutions 4 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> some anyChar

part1 :: M.Map Point Char -> Int
part1 input = flip runReader input $ do
  let allXs = M.keys $ M.filter (== 'X') input
  strs <- concat <$> traverse exploreAllDirections allXs
  pure $ length $ filter (== "XMAS") strs

part2 :: M.Map Point Char -> Int
part2 input = flip runReader input $ do
  let allAs = M.keys $ M.filter (== 'A') input
  length <$> filterM isValidXmas allAs


isValidXmas :: Point -> Reader (Grid Char) Bool
isValidXmas point = do
  diags <- getImmediateDiagonals point
  pure $ diags `elem` ["MSSM", "MMSS", "SSMM", "SMMS"]


travel :: Int -> Point -> Direction -> Reader (Grid Char) [Char]
travel amount point direction = do
  grid <- ask
  let points = take amount $ iterate (+ directionToUnitVector direction) point
  let r = filter (`M.member` grid) points
  pure $ map (grid M.!) r


exploreAllDirections :: Point -> Reader (Grid Char) [[Char]]
exploreAllDirections p = traverse (travel 4 p) [North .. SouthWest]

getImmediateDiagonals :: Point -> Reader (Grid Char) [Char]
getImmediateDiagonals p = do
  grid <- ask
  let pts = filter (`M.member` grid) $ map ((+ p) . directionToUnitVector) [NorthWest .. SouthWest]
  pure $ map (grid M.!) pts

data Direction = North | South | East | West | NorthWest | NorthEast | SouthEast | SouthWest
  deriving (Bounded, Enum, Eq, Show)

directionToUnitVector :: Direction -> V2 Int
directionToUnitVector North     = -(unit _y)
directionToUnitVector South     = unit _y
directionToUnitVector East      = unit _x
directionToUnitVector West      = -(unit _x)
directionToUnitVector NorthEast = directionToUnitVector North + directionToUnitVector East
directionToUnitVector NorthWest = directionToUnitVector North + directionToUnitVector West
directionToUnitVector SouthEast = directionToUnitVector South + directionToUnitVector East
directionToUnitVector SouthWest = directionToUnitVector South + directionToUnitVector West
