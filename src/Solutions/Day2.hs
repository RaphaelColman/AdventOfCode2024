{-# LANGUAGE RecordWildCards #-}
module Solutions.Day2
    ( aoc2
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.ListUtils        (window2)
import           Text.Parser.Char
import           Text.Parser.Combinators (sepBy, some)
import           Text.Parser.Token
import           Text.Trifecta           (Parser, space)
import Debug.Trace

aoc2 :: IO ()
aoc2 = do
  --printSolutions 2 $ MkAoCSolution parseInput part1
  printSolutions 2 $ MkAoCSolution parseInput part2

parseInput :: Parser [[Integer]]
parseInput = do
  xs <- sepBy (sepBy integer' (char ' ')) newline
  pure $ filter (not . null) xs --Because we're doing the newline separation ourselves it picks up an extra empty list at the end

part1 = length . filter isValid

part2 = length . filter testReport


isValid :: [Integer] -> Bool
isValid xs = (all (>0) diffs || all (<0) diffs) &&
              all (\d -> let a = abs d
                          in a >= 1 && a <= 3) diffs
  where diffs = map (uncurry (-)) $ window2 xs

testReport :: [Integer] -> Bool
testReport [] = error "Empty report"
testReport [x] = error "Report with only one value"
testReport (x:y:xs) = let direction = if y - x > 0 then Ascending else Descending
                    in go $ MkReportValidity direction x (y:xs) False
  where go rv@MkReportValidity{..}
            | null _remainingNumbers = True
            | not (valid rv) = not _alreadySkippedNumber
                              && go (MkReportValidity _direction _previousNumber (tail _remainingNumbers) True)
            | otherwise = go (MkReportValidity _direction (head _remainingNumbers) (tail _remainingNumbers) _alreadySkippedNumber)

valid :: ReportValidity -> Bool
valid rv@MkReportValidity{..} = currentDirection == _direction
                              && (currentDifference >= 1 && currentDifference <= 3)
  where currentDirection = if currentNumber - _previousNumber > 0 then Ascending else Descending
        (currentNumber:xs) = _remainingNumbers
        currentDifference = abs $ currentNumber - _previousNumber


data ReportValidity = MkReportValidity {
  _direction :: !Direction,
  _previousNumber :: !Integer,
  _remainingNumbers :: ![Integer],
  _alreadySkippedNumber :: !Bool
} deriving (Eq, Show)

data Direction = Ascending | Descending deriving (Eq, Show, Enum, Bounded)

shouldBeTrue = [7,6,4,2,1]
shouldBeTrueWithSkip = [1,3,2,4,5]
shouldBeFalse = [1,2,7,8,9]
