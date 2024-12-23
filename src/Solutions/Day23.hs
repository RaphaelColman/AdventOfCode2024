module Solutions.Day23
  ( aoc23,
  )
where

import Combinatorics (tuples)
import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Data.Function ((&))
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Trifecta (CharParsing (char), Parser, TokenParsing (token), count, letter, some)
import Data.List (sort, nub)

type Connection = (String, String)

type ConnectionMap = M.Map String (S.Set String)

aoc23 :: IO ()
aoc23 = do
  printSolutions 23 $ MkAoCSolution parseInput part1

-- printSolutions 23 $ MkAoCSolution parseInput part2

parseInput :: Parser [Connection]
parseInput = some $ token parseConnection

parseConnection :: Parser Connection
parseConnection = do
  a <- count 2 letter
  char '-'
  b <- count 2 letter
  pure (a, b)

part1 input = length tComputers
  where
    mp = makeIntoMap input
    cliquesSize3 = cliqueSize3 mp
    tComputers = S.filter (\(a, b, c) -> any (\x -> head x == 't') [a,b,c]) cliquesSize3

makeIntoMap :: [Connection] -> ConnectionMap
makeIntoMap = foldr f M.empty
  where
    f (a, b) m =
      m
        & M.insertWith S.union a (S.singleton b)
        & M.insertWith S.union b (S.singleton a)

cliqueSize3 :: ConnectionMap -> S.Set (String, String, String)
cliqueSize3 mp = S.fromList $ map (\[a,b,c] -> (a,b,c)) $ [sort [n1, n2, n3] |
                  n1 <- M.keys mp,
                  n2 <- S.toList (mp M.! n1),
                  n3 <- S.toList (mp M.! n2),
                  n1 `S.member` (mp M.! n3)]

maximalCliques :: ConnectionMap -> [[String]]
maximalCliques mp = undefined

