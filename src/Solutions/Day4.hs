module Solutions.Day4
    ( aoc4
    ) where

import qualified Combinatorics.Coin      as M
import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Geometry         (Grid, Point,
                                          enumerateMultilineStringToVectorMap)
import           Control.Lens            ((^.))
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
part1 input = length $ filter (== "XMAS") strs
  where allXs = M.keys $ M.filter (== 'X') input
        strs = concatMap (exploreAllDirections input) allXs


part2 :: M.Map Point Char -> Int
part2 input = length f
  where allAs = M.keys $ M.filter (== 'A') input
        f = M.filterWithKey (\k a -> a == 'A' && isValidXmas input k) input


isValidXmas :: Grid Char -> Point -> Bool
isValidXmas grid point = diags `elem` ["MSSM", "MMSS", "SSMM", "SMMS"]
  where diags = getImmediateDiagonals grid point
        


travel :: Grid a -> Int -> Point -> Direction -> [a]
travel grid amount point direction = map (grid M.!) r
  where points = take amount $ iterate (+ directionToUnitVector direction) point
        r = filter (`M.member` grid) points


exploreAllDirections :: Grid a -> Point -> [[a]]
exploreAllDirections grid p = map (travel grid 4 p) [North .. SouthWest]

getImmediateDiagonals :: Grid a -> Point -> [a]
getImmediateDiagonals grid p = map (grid M.!) pts
  where pts = filter (`M.member` grid) $ map ((+ p) . directionToUnitVector) [NorthWest .. SouthWest] 

data Direction = North | South | East | West | NorthWest | NorthEast | SouthEast | SouthWest
  deriving (Bounded, Enum, Eq, Show)

directionToUnitVector :: Direction -> V2 Int
directionToUnitVector North     = V2 0 (-1)
directionToUnitVector South     = V2 0 1
directionToUnitVector East      = V2 1 0
directionToUnitVector West      = V2 (-1) 0
directionToUnitVector NorthEast = V2 1 (-1)
directionToUnitVector NorthWest = V2 (-1) (-1)
directionToUnitVector SouthEast = V2 1 1
directionToUnitVector SouthWest = V2 (-1) 1
