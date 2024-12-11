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
import Data.Function ((&))
import qualified Data.IntMap as IM
import Data.Monoid (Sum (Sum))
import Debug.Trace
import Text.Parser.Combinators (some)
import Text.Trifecta (Parser, integer)

aoc11 :: IO ()
aoc11 = do
  -- printSolutions 11 $ MkAoCSolution parseInput part1
  printSolutions 11 $ MkAoCSolution parseInput part2

parseInput :: Parser [Int]
parseInput = fmap2 fromIntegral $ some integer

part1 input = length $ iterate blink input !! 25

part2 input = m1 <> m2
  where
    l = take 10 $ iterate blink [5]
    m1 = MkMonoidIntMap $ IM.fromList [(1, Sum 10), (2, Sum 20)]
    m2 = MkMonoidIntMap $ IM.fromList [(1, Sum 11), (2, Sum 21)]

stepStone :: Int -> [Int]
stepStone i
  | i == 0 = [1]
  | even (length asDigitString) =
      let (a, b) = splitAt (length asDigitString `div` 2) asDigitString
       in [read a, read b]
  | otherwise = [i * 2024]
  where
    asDigitString = show i

blink :: [Int] -> [Int]
blink = concatMap stepStone

-- Use FoldMapWithKey - pretty sure IM is monoidal so you can just
-- keep unioning the resulting maps
stepStoneIM :: IM.IntMap Int -> IM.IntMap (Sum Int)
stepStoneIM mp = _map $ IM.foldMapWithKey go mp
  where
    go :: Int -> Int -> MonoidIntMap (Sum Int)
    go k numOccurences = MkMonoidIntMap im
      where
        im = IM.fromList $ map (,Sum numOccurences) $ stepStone k

newtype MonoidIntMap a = MkMonoidIntMap {_map :: IM.IntMap a} deriving (Show)

instance (Monoid a) => Monoid (MonoidIntMap a) where
  mempty = MkMonoidIntMap IM.empty
  mappend = (<>)

instance (Semigroup a) => Semigroup (MonoidIntMap a) where
  (<>) (MkMonoidIntMap mapA) (MkMonoidIntMap mapB) = MkMonoidIntMap $ IM.unionWith (<>) mapA mapB
