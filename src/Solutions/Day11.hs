module Solutions.Day11
  ( aoc11,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Data.Function ((&))
import Text.Parser.Combinators (some)
import Text.Trifecta (Parser, integer)

aoc11 :: IO ()
aoc11 = do
  --printSolutions 11 $ MkAoCSolution parseInput part1
  printSolutions 11 $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = some integer

part1 input = length $ (iterate blink input) !! 25

part2 input = length $ (iterate blink input) !! 75
--lol that's not coming back any time soon!

stepStone :: Integer -> [Integer]
stepStone i
  | i == 0 = [1]
  | even (length asDigitString) =
      let (a, b) = splitAt (length asDigitString `div` 2) asDigitString
       in [read a, read b]
  | otherwise = [i * 2024]
  where
    asDigitString = show i

blink :: [Integer] -> [Integer]
blink = concatMap stepStone
