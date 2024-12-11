{-# LANGUAGE TupleSections #-}

module Solutions.Day11
  ( aoc11,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Debugging (withNewLines)
import Common.FunctorUtils (fmap2)
import Common.ListUtils (freqs)
import Common.MapUtils (MonoidIntMap (MkMonoidIntMap, _map))
import Data.Foldable (find)
import Data.Function ((&))
import qualified Data.IntMap as IM
import Data.List (findIndex)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum, getSum))
import Text.Parser.Combinators (some)
import Text.Trifecta (Parser, integer)

aoc11 :: IO ()
aoc11 = do
  printSolutions 11 $ MkAoCSolution parseInput part1
  printSolutions 11 $ MkAoCSolution parseInput part2

parseInput :: Parser [Int]
parseInput = fmap2 fromIntegral $ some integer

part1 input = lnMap $ iterate stepStoneIM (initStones input) !! 25

part2 input = lnMap $ iterate stepStoneIM (initStones input) !! 75

stepStone :: Int -> [Int]
stepStone i
  | i == 0 = [1]
  | even (length asDigitString) =
      let (a, b) = splitAt (length asDigitString `div` 2) asDigitString
       in [read a, read b]
  | otherwise = [i * 2024]
  where
    asDigitString = show i

stepStoneIM :: IM.IntMap (Sum Int) -> IM.IntMap (Sum Int)
stepStoneIM mp = _map $ IM.foldMapWithKey go mp
  where
    go k (Sum numOccurences) = MkMonoidIntMap im
      where
        im = IM.fromListWith (<>) $ map (,Sum numOccurences) $ stepStone k

initStones :: [Int] -> IM.IntMap (Sum Int)
initStones = IM.fromList . map (,Sum 1)

lnMap :: IM.IntMap (Sum Int) -> Int
lnMap = getSum . foldr1 (<>) . IM.elems
