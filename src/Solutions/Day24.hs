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
import qualified Data.Map as M
import Debug.Trace (traceM)
import Text.Parser.Combinators (try)
import Text.Parser.Token (TokenParsing (token))
import Text.Trifecta (CharParsing (char, string), Parser, alphaNum, integer, letter, manyTill, some, upper)
import Data.Bits (xor)
import Data.Function ((&))
import Common.BinaryUtils (boolsToBinaryString, toDecimal)

type WireLabel = String
type Gates = M.Map WireLabel Wire

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
  deriving (Eq, Show, Ord)

aoc24 :: IO ()
aoc24 = do
  printSolutions 24 $ MkAoCSolution parseInput part1

-- printSolutions 24 $ MkAoCSolution parseInput part2

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
    mp = makeWireMap input
    result = M.filterWithKey (\k v -> head k == 'z') mp
            & M.elems
            & reverse
            & boolsToBinaryString
            & toDecimal

makeWireMap :: [Wire] -> M.Map WireLabel Bool
makeWireMap wires = mp
  where
    mp = M.fromList entries
    entries = map go wires
    go (Leaf label val) = (label, val)
    go AndGate{..} = let val = (mp M.! _wire1) && (mp M.! _wire2) in (_label, val)
    go OrGate{..} = let val = (mp M.! _wire1) || (mp M.! _wire2) in (_label, val)
    go XorGate{..} = let val = (mp M.! _wire1) `xor` (mp M.! _wire2) in (_label, val)
