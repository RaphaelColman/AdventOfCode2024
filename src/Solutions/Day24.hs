{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Solutions.Day24
  ( aoc24,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.BinaryUtils (boolsToBinaryString, toDecimal, pad0)
import Common.EitherUtils (withLeft)
import Data.Bits (xor)
import Data.Function ((&))
import Data.List (sort)
import qualified Data.Map as M
import Debug.Trace (traceM, traceShow, traceShowM)
import Text.Parser.Combinators (try)
import Text.Parser.Token (TokenParsing (token))
import Text.Trifecta (CharParsing (char, string), Parser, alphaNum, integer, letter, manyTill, some, upper)
import Data.Foldable (foldlM)

type WireLabel = String

data Op = AND | OR | XOR deriving (Eq, Show, Enum, Bounded, Ord)

-- Map of the two wires in a gate (in alphabetical order), op to target wire
type WireMap = M.Map (WireLabel, WireLabel, Op) WireLabel

data Wire
  = Leaf
      { _label :: WireLabel,
        _value :: Bool
      }
  | AndGate
      { _label :: String,
        _wire1 :: WireLabel,
        _wire2 :: WireLabel
      }
  | OrGate
      { _label :: String,
        _wire1 :: WireLabel,
        _wire2 :: WireLabel
      }
  | XorGate
      { _label :: String,
        _wire1 :: WireLabel,
        _wire2 :: WireLabel
      }
  deriving (Eq, Ord, Show)

data Assertion
  = MkOutputAssertion -- Assertion where the output label is known
      { _w1 :: WireLabel,
        _w2 :: WireLabel,
        _op :: Op,
        _output :: WireLabel -- Just represent this as a Maybe?
      }
  | MkAssertion
      { _w1 :: WireLabel,
        _w2 :: WireLabel,
        _op :: Op
      }
  deriving (Eq, Show)

aoc24 :: IO ()
aoc24 = do
  -- printSolutions 24 $ MkAoCSolution parseInput part1
  printSolutions 24 $ MkAoCSolution parseInput part2

parseInput :: Parser [Wire]
parseInput = do
  wires <- some $ try parseWire
  gates <- some $ token parseGate
  pure $ wires ++ gates

parseGate :: Parser Wire
parseGate = do
  w1 <- some alphaNum
  opString <- char ' ' *> some upper <* char ' '
  let op = case opString of
        "AND" -> AndGate
        "OR" -> OrGate
        "XOR" -> XorGate
  w2 <- some alphaNum
  string " -> "
  w3 <- some alphaNum
  pure $ op w3 w1 w2

parseWire :: Parser Wire
parseWire = do
  name <- some alphaNum
  string ": "
  value :: Bool <- toEnum . fromIntegral <$> integer
  pure $ Leaf name value

part1 input = result
  where
    zWires = filter ((== 'z') . head) $ map _label input
    mp = resolveWires input
    result =
      M.filterWithKey (\k v -> head k == 'z') mp
        & M.elems
        & reverse
        & boolsToBinaryString
        & toDecimal

part2 input = findBrokenAdder wiremap
  where
    wiremap = makeWireMap input
    corrected = undefined

findBrokenAdder :: WireMap -> Either Assertion WireLabel
findBrokenAdder wiremap = foldlM go "kvj" [1..44]
  where
    go overflow sig = analyseFullAdder wiremap sig overflow

resolveWires :: [Wire] -> M.Map WireLabel Bool
resolveWires wires = mp
  where
    mp = M.fromList entries
    entries = map go wires
    go (Leaf label val) = (label, val)
    go AndGate {..} = let val = (mp M.! _wire1) && (mp M.! _wire2) in (_label, val)
    go OrGate {..} = let val = (mp M.! _wire1) || (mp M.! _wire2) in (_label, val)
    go XorGate {..} = let val = (mp M.! _wire1) `xor` (mp M.! _wire2) in (_label, val)

analyseFullAdder :: WireMap -> Integer -> WireLabel -> Either Assertion WireLabel
analyseFullAdder mp sig overflow = do
  let sigString = pad0 2 (show sig)
  let x = 'x' : sigString
  let y = 'y' : sigString
  let z = 'z' : sigString
  a <- assertGate mp $ MkAssertion x y XOR
  zN <- assertGate mp $ MkOutputAssertion a overflow XOR z
  b <- assertGate mp $ MkAssertion x y AND
  c <- assertGate mp $ MkAssertion a overflow AND
  assertGate mp $ MkAssertion b c OR

assertGate :: WireMap -> Assertion -> Either Assertion WireLabel
assertGate mp a = case a of
  MkAssertion {..} -> do
    M.lookup (asMapKey _w1 _w2 _op) mp & withLeft a
  MkOutputAssertion {..} -> do
    dest <- M.lookup (asMapKey _w1 _w2 _op) mp & withLeft a
    if dest == _output
      then Right dest
      else traceShow dest $ Left a

asMapKey a b op = let [a', b'] = sort [a, b] in (a', b', op)

makeWireMap :: [Wire] -> WireMap
makeWireMap = foldr go M.empty
  where
    go :: Wire -> WireMap -> WireMap
    go wire mp = case wire of
                  Leaf _ _ -> mp
                  AndGate l w1 w2 -> M.insert (asMapKey w1 w2 AND) l mp
                  OrGate l w1 w2 -> M.insert (asMapKey w1 w2 OR) l mp
                  XorGate l w1 w2 -> M.insert (asMapKey w1 w2 XOR) l mp

{-
 - So some validation algorithm: given generation N and the prev overflow wire (called OF):
 - Get the input wires: xN and yN
 - xN XOR yN -> A. 
 - A XOR OF -> zN
 - xN AND yN -> B
 - A AND OF -> C
 - B OR C -> NOF (new overflow)
 - As long as we found NOF we're good
 -}
