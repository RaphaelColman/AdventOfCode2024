module Solutions.Day2
    ( aoc2
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.EnumUtils        (enumNext, stepEnum)
import           Common.ListUtils        (window2)
import           Data.List
import           Debug.Trace
import           Text.Parser.Char
import           Text.Parser.Combinators (sepBy, some)
import           Text.Parser.Token
import           Text.Trifecta           (Parser, space)

aoc2 :: IO ()
aoc2 = do
  printSolutions 2 $ MkAoCSolution parseInput part1
  printSolutions 2 $ MkAoCSolution parseInput part2

parseInput :: Parser [[Integer]]
parseInput = do
  xs <- sepBy (sepBy integer' (char ' ')) newline
  pure $ filter (not . null) xs --Because we're doing the newline separation ourselves it picks up an extra empty list at the end

part1 :: [[Integer]] -> Int
part1 = length . filter isValid

part2 :: [[Integer]] -> Int
part2 = length . filter isValidWithToleration


isValid :: [Integer] -> Bool
isValid xs = (all (>0) diffs || all (<0) diffs) &&
              all (\d -> let a = abs d
                          in a >= 1 && a <= 3) diffs
  where diffs = map (uncurry (-)) $ window2 xs

isValidWithToleration :: [Integer] -> Bool
isValidWithToleration xs = any isValid $ xs : withAnElementRemoved xs

withAnElementRemoved :: [a] -> [[a]]
withAnElementRemoved xs = zipWith (++) (init $ inits xs) (tail $ tails xs)
