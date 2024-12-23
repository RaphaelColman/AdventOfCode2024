module Solutions.Day22
  ( aoc22,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Data.Bits (xor)
import Data.Function ((&))
import Text.Parser.Combinators (some)
import Text.Trifecta (Parser, integer)

aoc22 :: IO ()
aoc22 = do
  printSolutions 22 $ MkAoCSolution parseInput part1

-- printSolutions 22 $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = some integer

part1 input = sum $ map (runXTimes 2000) input

nextSecretNumber :: Integer -> Integer
nextSecretNumber = step3 . step2 . step1
  where
    step1 x = x * 64 & mix x & prune
    step2 x = x `div` 32 & mix x & prune
    step3 x = x * 2048 & mix x & prune

mix :: Integer -> Integer -> Integer
mix a b = a `xor` b

prune :: Integer -> Integer
prune x = x `mod` 16777216

runXTimes :: Integer -> Integer -> Integer
runXTimes n x = iterate nextSecretNumber x !! fromIntegral n
