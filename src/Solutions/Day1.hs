module Solutions.Day1
    ( aoc1
    ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.ListUtils    (freqs, window2, window3)
import           Data.List           (tails)
import qualified Data.Map.Strict     as M
import           GHC.OldList         (sort)
import           Text.Parser.Char    (digit)
import           Text.Trifecta       (CharParsing (anyChar), Parser,
                                      TokenParsing (token), count, integer,
                                      some)

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

parseInput :: Parser [(Integer, Integer)]
parseInput = do
  some $ token parsePair
  where parsePair :: Parser (Integer, Integer)
        parsePair = do
            [a,b] <- count 2 integer
            pure (a,b)

part1 :: [(Integer, Integer)] -> Integer
part1 input = sum $ abs <$> zipWith (-) l2 l1
  where (l1, l2) = convetToSeparateLists input


part2 :: [(Integer, Integer)] -> Integer
part2 input = sum $ map (\x -> x * M.findWithDefault 0 x freqMap) l1
  where (l1, l2) = convetToSeparateLists input
        freqMap = freqs l2

convetToSeparateLists :: [(Integer, Integer)] -> ([Integer], [Integer])
convetToSeparateLists input = (l1, l2)
  where l1 = sort $ map fst input
        l2 = sort $ map snd input
