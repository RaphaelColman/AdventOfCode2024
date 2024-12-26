{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day17
  ( aoc17,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.FunctorUtils (fmap2)
import Common.MaybeUtils (runUntilNothing)
import Control.Lens (ASetter, makeLenses, (%~))
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((.~))
import Data.Bits
import Data.Finite (Finite, finite, getFinite)
import Data.Function ((&))
import Data.Range
import qualified Data.Sequence as Seq
import Debug.Trace (trace, traceM, traceShowM)
import Text.Printf (printf)
import Text.Trifecta (CharParsing (string), Parser, alphaNum, commaSep, integer, letter, some, upper)
import Data.Foldable (Foldable(toList, foldl'))
import qualified Data.Map as M
import Data.List (isPrefixOf, isSuffixOf)
import Common.BinaryUtils
import Control.Monad (guard)

type ThreeBit = Finite 8

data Opcode = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Show, Eq, Bounded, Enum, Ord)

data Computer = MkComputer
  { _aReg :: !Integer,
    _bReg :: !Integer,
    _cReg :: !Integer,
    _program :: !(Seq.Seq ThreeBit),
    _pointer :: !Int,
    _output :: ![Integer]
  }
  deriving (Eq)

makeLenses ''Computer

instance Show Computer where
  show c = printf "Register A: %d\nRegister B: %d\nRegister C: %d\n\nProgram:%s\nPointer: %d\nOutput: %s\n"
    (_aReg c) (_bReg c) (_cReg c) showProgram (_pointer c) showOutput
    where
      showProgram = show $ map getFinite $ toList $ c ^. program
      showOutput = show $ toList $ c ^. output

aoc17 :: IO ()
aoc17 = do
  printSolutions 17 $ MkAoCSolution parseInput part1
  printSolutions 17 $ MkAoCSolution parseInput part2

parseInput :: Parser Computer
parseInput = do
  [a, b, c] <- some parseRegister
  string "Program: "
  xs <- fmap2 finite $ commaSep integer
  pure $ MkComputer a b c (Seq.fromList xs) 0 []

parseRegister :: Parser Integer
parseRegister = do
  string "Register " >> upper >> string ": "
  integer

part1 = runComplete

part2 = solve

{- For my input, the least 3 significant bits produces some output, and then the program repeats having
-- removed those three bits.
-- The program will also look at 3 other bits that are more significant depending on what the `xor` with 3
-- of these three bits is (ie, if the xor of the 3 bits is 7, it will remove 7 bits from the end and use
-- the 3 new most significants bits to influence the output).
--
-- That means we need fold over the target output (the program) and proces 3 bits at a time from the end.
-- We'll keep any which match our output and then continue.
-- -}
solve :: Computer -> Integer
solve computer = minimum $ foldl' go [0] target
  where
    target = reverse $ map getFinite . toList $ computer ^. program
    go nums singleOutput = do
      n <- nums
      a <- [0..7]
      let next = n * 8 + a --Shift n to the left by 3 bits and add a (the next 3 bits)
      let result = runComplete $ computer & aReg .~ next
      guard $ head result == singleOutput
      pure next


runComplete :: Computer -> [Integer]
runComplete = getOutput . runUntilNothing step

-- | Map of desired output to a list of fst, snd and distance
combos :: M.Map Integer [(Integer, Integer, Integer)]
combos = undefined
  where
    prs :: [(Integer, Integer)] = [(a, b) | a <- [0 .. 7], b <- [0 .. 7]]
    getResult a b = a `xor` b `xor` 5


step :: Computer -> Maybe Computer
step c = do
  instr <- (c ^. program) Seq.!? (c ^. pointer)
  let opcode :: Opcode = toEnum $ fromIntegral $ getFinite instr
  let advancedC = c & pointer %~ (+ 1)
  case opcode of
    Adv -> adv advancedC -- 0
    Bxl -> bxl advancedC -- 1
    Bst -> bst advancedC -- 2
    Jnz -> jnz advancedC -- 3
    Bxc -> bxc advancedC -- 4
    Out -> out advancedC -- 5
    Bdv -> bdv advancedC -- 6
    Cdv -> cdv advancedC -- 7

-- | Expect the pointer to be moved to read out the operand
adv :: Computer -> Maybe Computer
adv c = dv c aReg

bxl :: Computer -> Maybe Computer
bxl c = do
  operand <- (c ^. program) Seq.!? (c ^. pointer)
  let result = c ^. bReg `xor` getFinite operand
  pure $ c & bReg .~ result & pointer %~ (+ 1)

bst :: Computer -> Maybe Computer
bst c = do
  comboOperand <- resolveComboOperand c
  let result = comboOperand `mod` 8
  pure $ c & bReg .~ result & pointer %~ (+ 1)

jnz :: Computer -> Maybe Computer
jnz c = do
  let a = c ^. aReg
  operand <- (c ^. program) Seq.!? (c ^. pointer)
  if a == 0
    then pure $ c & pointer %~ (+ 1)
    else pure $ c & pointer .~ fromIntegral (getFinite operand)

bxc :: Computer -> Maybe Computer
bxc c = do
  let result = c ^. bReg `xor` c ^. cReg
  pure $ c & bReg .~ result & pointer %~ (+ 1)

out :: Computer -> Maybe Computer
out c = do
  comboOperand <- resolveComboOperand c
  let result = comboOperand `mod` 8
  pure $ c & output %~ (result :) & pointer %~ (+ 1)

bdv :: Computer -> Maybe Computer
bdv c = dv c bReg

cdv :: Computer -> Maybe Computer
cdv c = dv c cReg

dv :: Computer -> ASetter Computer Computer a Integer -> Maybe Computer
dv c regLens = do
  comboOperand <- resolveComboOperand c
  let numerator = c ^. aReg
  let denominator = 2 ^ comboOperand
  let result = numerator `div` denominator
  pure $ c & regLens .~ result & pointer %~ (+ 1)

resolveComboOperand :: Computer -> Maybe Integer
resolveComboOperand c = do
  operand <- (c ^. program) Seq.!? (c ^. pointer)
  pure $ resolve operand
  where
    resolve operand
      | 0 +=+ 3 `inRange` getFinite operand = getFinite operand
      | getFinite operand == 4 = c ^. aReg
      | getFinite operand == 5 = c ^. bReg
      | getFinite operand == 6 = c ^. cReg
      | otherwise = error "Invalid operand"

getOutput :: Computer -> [Integer]
getOutput = reverse . _output
