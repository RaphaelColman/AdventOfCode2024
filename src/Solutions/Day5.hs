{-# LANGUAGE TupleSections #-}

module Solutions.Day5 (aoc5)
where

import Common.AoCSolutions (
  AoCSolution (MkAoCSolution),
  printSolutions,
  printTestSolutions,
 )
import Data.List (tails)
import qualified Data.Set as S
import GHC.OldList (sort)
import Text.Parser.Char (CharParsing (char))
import Text.Parser.Token (commaSep, integer)
import Text.Trifecta (
  Parser,
  Parsing (eof, try),
  many,
  manyTill,
  some,
 )

aoc5 :: IO ()
aoc5 = do
  printSolutions 5 $ MkAoCSolution parseInput part1

-- printSolutions 5 $ MkAoCSolution parseInput part2

type Rule = (Integer, Integer)
type Rules = S.Set Rule
type Update = [Integer]
type Input = (S.Set Rule, [Update])

parseInput :: Parser (S.Set Rule, [Update])
parseInput = do
  rules <- S.fromList <$> many (try parseRule)
  updates <- parseUpdates
  pure (rules, updates)

parseRule :: Parser Rule
parseRule = do
  before <- integer
  char '|'
  after <- integer
  return (before, after)

parseUpdates :: Parser [Update]
parseUpdates = do
  manyTill (commaSep integer) eof -- I'm sure there is a way of doing this withing having to handle eof

-- part1 (rules, updates) = sum $ map middleElement $ filter (validateUpdate rules) updates
part1 (rules, updates) = sum $ map middleElement $ filter (\update -> orderUpdate rules update == update) updates

part2 :: String -> String
part2 = undefined

validateUpdate :: S.Set Rule -> Update -> Bool
validateUpdate rules update = all validateOrder $ allUpdatePairs update
 where
  validateOrder :: (Integer, Integer) -> Bool
  validateOrder (before, after) = not ((after, before) `S.member` rules)

allUpdatePairs :: [a] -> [(a, a)]
allUpdatePairs update = concat $ zipWith (\x -> map (x,)) update $ map tail (tails update)

middleElement :: [a] -> a
middleElement xs = xs !! (length xs `div` 2)

data Page = MkPage
  { _pageNumber :: !Integer
  , _rules :: !(S.Set Rule)
  }
  deriving (Eq)

instance Show Page where
  show (MkPage n r) = show n

instance Ord Page where
  compare (MkPage n1 r) (MkPage n2 _)
    | (n1, n2) `S.member` r = LT
    | (n2, n1) `S.member` r = GT
    | otherwise = EQ

orderUpdate :: Rules -> Update -> Update
orderUpdate rules update = map _pageNumber sorted
 where
  asPages = map (`MkPage` rules) update
  sorted = sort asPages
