{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day9
  ( aoc9,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.ListUtils (window2)
import Control.Lens (makeLenses, Bifunctor (bimap))
import Control.Monad.State (execState, modify, runState)
import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'), find, foldrM)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (isNothing)
import Debug.Trace
import Text.Parser.Combinators (some)
import Text.Trifecta (Parser, digit, integer)

data ReadState = MkReadState
  { _readFileBlock :: !Bool, -- If this is false we're reading free space
    _flattenedDisk :: !Disk,
    _currentFileId :: !Integer,
    _currentIndex :: !Int
  }
  deriving (Eq, Show)

type Disk = IM.IntMap (Integer, Integer) -- (FileId, Length)

makeLenses ''ReadState

aoc9 :: IO ()
aoc9 = do
  -- printSolutions 9 $ MkAoCSolution parseInput part1
  printSolutions 9 $ MkAoCSolution parseInput part2

--
--
type DiskMap = [Integer]

parseInput :: Parser DiskMap
parseInput = do
  digits <- some digit
  pure $ map (toInteger . digitToInt) digits

-- part1 input = checkSum solved
--   where
--     solved = compress $ readDiskMap input

part2 input = checkSum solved
  where
    solved = compressPreservingFiles $ readDiskMap input

readDiskMap :: DiskMap -> Disk
readDiskMap dm = _flattenedDisk $ foldl' go (MkReadState True IM.empty 0 0) dm
  where
    go MkReadState {..} i =
      if _readFileBlock
        then
          let newMp = insertFileId _currentFileId _currentIndex (fromIntegral i) _flattenedDisk
           in MkReadState False newMp (_currentFileId + 1) (_currentIndex + fromIntegral i)
        else MkReadState True _flattenedDisk _currentFileId (_currentIndex + fromIntegral i)

insertFileId :: Integer -> Int -> Integer -> Disk -> Disk
insertFileId fileId startIdx times = IM.insert startIdx (fileId, times)

isContiguous :: IM.IntMap a -> Bool
isContiguous m = length keys == length [minKey .. maxKey]
  where
    keys = IM.keys m
    (minKey, _) = IM.findMin m
    (maxKey, _) = IM.findMax m

-- I can see from the profile that we're spending most of our time in here.
-- It would be faster to keep track of where the last free space was and search from there
-- instead
firstFreeSpace :: IM.IntMap a -> Maybe IM.Key
firstFreeSpace m = find (\k -> isNothing (IM.lookup k m)) [0 .. maxKey]
  where
    (maxKey, _) = IM.findMax m

firstFreeSpace' :: Int -> Int -> Disk -> Maybe (Int, Int)
firstFreeSpace' spaceRequired originalStartingIndex m = find (\(idx, size) -> size >= spaceRequired && idx < originalStartingIndex) gaps
  where
    gaps = calculateGaps m

compressStep :: Disk -> Maybe Disk
compressStep d = do
  firstFree <- firstFreeSpace d
  (maxKey, val) <- IM.lookupMax d
  let (_, newDisk) = flip runState d $ do
        modify $ IM.delete maxKey
        modify $ IM.insert firstFree val
  pure newDisk

-- compress :: Disk -> Maybe Disk
-- compress d = maybe d compress (compress d)

-- I think foldrWithKey doesn't actually do it in reverse which is what I thought it would do
compressPreservingFiles :: Disk -> Disk
compressPreservingFiles disk = IM.foldrWithKey go disk disk
  where
    go :: Int -> (Integer, Integer) -> Disk -> Disk
    go idx (fileId, size) d = case firstFreeSpace' (fromIntegral size) idx d of
      Just (gapIdx, gapSize) -> flip execState d $ do
        modify $ IM.delete idx
        modify $ IM.insert gapIdx (fileId, size)
      Nothing -> d
--- nearly. This doesn't care if the gap is actually to the right of the original location

checkSum :: Disk -> Integer
checkSum = IM.foldrWithKey go 0
  where
    go :: Int -> (Integer, Integer) -> Integer -> Integer
    go idx (fileId, size) acc = acc + sum (map (fileId *) indexes)
      where indexes = map fromIntegral [idx .. (idx + fromIntegral size) - 1]

-- This needs to be redefined

-- | List of gaps of the form [(idx of gap, size of gap)]
calculateGaps :: Disk -> [(Int, Int)]
calculateGaps d = g
  where
    pairs = window2 $ IM.toList d
    g =
      map
        ( \((idx, (_, size)), (nextIdx, _)) ->
            let gapSize = (idx + fromIntegral size)
             in (idx + fromIntegral size, nextIdx - gapSize)
        )
        pairs

---Gaps seem wrong for this
--[(0,(0,2)),(2,(9,2)),(5,(1,3)),(11,(2,1)),(15,(3,3)),(19,(4,2)),(22,(5,4)),(27,(6,4)),(32,(7,3)),(36,(8,4))]
--calculated: [(2,0),(6,1),(13,3),(23,3),(33,1),(40,1),(48,1),(58,1),(67,1)]
--this is definitely wrong. There should be a gap of (4,1)
-- >>> testGaps
-- [(2,0),(4,1),(8,3),(12,3),(18,1),(21,1),(26,1),(31,1),(35,1)]
testGaps :: [(Int, Int)]
testGaps = calculateGaps test
  where test = IM.fromList [(0, (0, 2)), (2, (9, 2)), (5, (1, 3)), (11, (2, 1)), (15, (3, 3)), (19, (4, 2)), (22, (5, 4)), (27, (6, 4)), (32, (7, 3)), (36, (8, 4))]
