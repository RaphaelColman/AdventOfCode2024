module Solutions.Day25
  ( aoc25,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry (Point, enumerateMultilineStringToVectorMap)
import Data.Function ((&))
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parser.Combinators (some)
import Text.Trifecta (CharParsing (anyChar), Parser, count)
import Linear.V2 (V2(V2))
import Data.List (partition)

aoc25 :: IO ()
aoc25 = do
  printSolutions 25 $ MkAoCSolution parseInput part1

type Key = S.Set Point

type Lock = S.Set Point

parseInput :: Parser [S.Set Point]
parseInput = do
  allChars <- some anyChar
  let grids = enumerateMultilineStringToVectorMap <$> splitOn "\n\n" allChars
  pure $ map (M.keysSet . M.filter (== '#')) grids
  

part1 :: [S.Set Point] -> Int
part1 input = length $ [(k,l) | k <- keys, l <- locks, fits k l]
  where
    (keys, locks) = partition isKey input

isKey :: S.Set Point -> Bool
isKey pts = all (`S.member` pts ) [V2 x 6 | x <- [0..4]]

fits :: Key -> Lock -> Bool
fits key lock = S.null $ S.intersection key lock
