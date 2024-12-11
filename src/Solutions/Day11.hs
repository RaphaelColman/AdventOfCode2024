{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Function ((&))
import qualified Data.IntMap as IM
import Data.Monoid (Sum (Sum, getSum))
import Debug.Trace
import Text.Parser.Combinators (some)
import Text.Trifecta (Parser, integer)
import qualified Data.Map as M
import Data.Foldable (find)
import Data.List (findIndex)

aoc11 :: IO ()
aoc11 = do
  printSolutions 11 $ MkAoCSolution parseInput part1
  printSolutions 11 $ MkAoCSolution parseInput part2

parseInput :: Parser [Int]
parseInput = fmap2 fromIntegral $ some integer

part1 input = length $ iterate blink input !! 25

part2 input = lnMap $ iterate stepStoneIM i !! 75
  where
    i = initStones input
    correctAnswer = map freqs $ take 11 $ iterate blink input
    c :: [IM.IntMap Int] = correctAnswer & map (IM.fromList . M.toList)
    tst2 = map (\m -> IM.map getSum m) $ take 11 $ iterate stepStoneIM' i
    firstDifference = find (\(m1, m2) -> m1 /= m2) $ zip c tst2

-- This is giving me the wrong answer for 25 blinks. It's too low

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
stepStoneIM :: IM.IntMap (Sum Int) -> IM.IntMap (Sum Int)
stepStoneIM mp = _map $ IM.foldMapWithKey go mp
  where
    go :: Int -> Sum Int -> MonoidIntMap (Sum Int)
    go k (Sum numOccurences) = MkMonoidIntMap im
      where
        im = IM.fromListWith (<>) $ map (,Sum numOccurences) $ stepStone k

stepStoneIM' :: IM.IntMap (Sum Int) -> IM.IntMap (Sum Int)
stepStoneIM' mp = m
  where
    m =
      mp
        & IM.toList
        & map
          ( \(k, count) ->
              let xs = stepStone k
               in IM.fromListWith (+) $ map (,count) xs
          )
        & IM.unionsWith (+)

-- Ok this gets me the same wrong answer. There's some multiplication I'm forgetting to do

newtype MonoidIntMap a = MkMonoidIntMap {_map :: IM.IntMap a} deriving (Show)

instance (Monoid a) => Monoid (MonoidIntMap a) where
  mempty = MkMonoidIntMap IM.empty
  mappend = (<>)

instance (Semigroup a) => Semigroup (MonoidIntMap a) where
  (<>) (MkMonoidIntMap mapA) (MkMonoidIntMap mapB) = MkMonoidIntMap $ IM.unionWith (<>) mapA mapB

initStones :: [Int] -> IM.IntMap (Sum Int)
initStones xs = IM.fromList $ map (,Sum 1) xs

lnMap :: IM.IntMap (Sum Int) -> Int
lnMap = getSum . foldr1 (<>) . IM.elems
