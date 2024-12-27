module Solutions.Day22
  ( aoc22,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.ListUtils (window2, windowN)
import Control.Parallel.Strategies
import Data.Bits (xor)
import Data.Char (digitToInt)
import Data.Function ((&))
import Data.Hashable
import qualified Data.IntMap as IM
import Data.List (foldl1', scanl')
import qualified Data.Map as M
import GHC.List (foldl')
import Text.Parser.Combinators (some)
import Text.Trifecta (Parser, integer)

aoc22 :: IO ()
aoc22 = do
  printSolutions 22 $ MkAoCSolution parseInput part1
  printSolutions 22 $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = some integer

part1 input = sum (map (runXTimes 2000) input `using` parList rdeepseq)

part2 input = maximum combined
  where
    mps = map calculateChangeMap input
    combined = foldl1' (IM.unionWith (+)) mps

nextSecretNumber :: Integer -> Integer
nextSecretNumber = step3 . step2 . step1
  where
    step1 x = x * 64 & mix x & prune -- 2^6 (so bung 6 0s on the end and XOR. That means the last 6 binary digits stay the same. Then keep the last 24 bits
    step2 x = x `div` 32 & mix x & prune -- 2^5 (so remove 5 digits from the end and XOR). Then keep the last 24 bits
    step3 x = x * 2048 & mix x & prune -- 2^11 (so bung 11 0s on the end, XOR and keep the last 24 bits

mix :: Integer -> Integer -> Integer
mix a b = a `xor` b

prune :: Integer -> Integer
prune x = x `mod` 16777216

-- This is 2^24
-- So we're just keeping the last 24 bits

runXTimes :: Integer -> Integer -> Integer
runXTimes n x = iterate nextSecretNumber x !! fromIntegral n

secretNumbers :: Int -> Integer -> [Integer]
secretNumbers n x = take n $ iterate nextSecretNumber x

finalDigits :: [Integer] -> [Int]
finalDigits = map $ digitToInt . last . show

differences :: [Int] -> [Int]
differences ints = window2 ints & map (uncurry (flip (-)))

calculateChangeMap :: Integer -> IM.IntMap Int
calculateChangeMap secretNumber = IM.fromListWith (\_ a -> a) $ map go allChanges
  where
    prices = secretNumber & (finalDigits . secretNumbers 2000)
    diffs = differences prices
    allChanges = windowN 5 $ zip prices diffs
    go changes = (hash (map snd (take 4 changes)), last changes & fst)

