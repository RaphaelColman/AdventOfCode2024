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
import Control.Lens (makeLenses)
import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'), find)
import qualified Data.IntMap as IM
import Text.Parser.Combinators (some)
import Text.Trifecta (Parser, digit, integer)
import Data.Maybe (isNothing)
import Control.Monad.State (runState, modify)

data ReadState = MkReadState
  { _readFileBlock :: !Bool, -- If this is false we're reading free space
    _flattenedDisk :: !(IM.IntMap Integer),
    _currentFileId :: !Integer,
    _currentIndex :: !Int
  }
  deriving (Eq, Show)

type Disk = IM.IntMap Integer

makeLenses ''ReadState

aoc9 :: IO ()
aoc9 = do
  printTestSolutions 9 $ MkAoCSolution parseInput part1

-- printSolutions 9 $ MkAoCSolution parseInput part2
--
--
type DiskMap = [Integer]

parseInput :: Parser DiskMap
parseInput = do
  digits <- some digit
  pure $ map (toInteger . digitToInt) digits

part1 input = renderDiskMap solved
  where
    test = [9, 0, 9, 0, 9]
    solved = compress $ readDiskMap input

part2 :: String -> String
part2 = undefined

readDiskMap :: DiskMap -> IM.IntMap Integer
readDiskMap dm = _flattenedDisk $ foldl' go (MkReadState True IM.empty 0 0) dm
  where
    go MkReadState {..} i =
      if _readFileBlock
        then
          let newMp = insertFileId _currentFileId _currentIndex (fromIntegral i) _flattenedDisk
           in MkReadState False newMp (_currentFileId + 1) (_currentIndex + fromIntegral i)
        else MkReadState True _flattenedDisk _currentFileId (_currentIndex + fromIntegral i)

insertFileId :: Integer -> Int -> Int -> IM.IntMap Integer -> IM.IntMap Integer
insertFileId fileId startIdx times mp = foldl' (\m i -> IM.insert i fileId m) mp [startIdx .. startIdx + times - 1]

-- | For debug purposes
renderDiskMap :: IM.IntMap Integer -> String
renderDiskMap m = concatMap (\k -> IM.findWithDefault "." k m') [0 .. maxKey]
  where
    (maxKey, a) = IM.findMax m
    m' = IM.map show m

isContiguous :: IM.IntMap a -> Bool
isContiguous m = length keys == length [minKey .. maxKey]
  where
    keys = IM.keys m
    (minKey, _) = IM.findMin m
    (maxKey, _) = IM.findMax m

firstFreeSpace :: IM.IntMap a -> Maybe IM.Key
firstFreeSpace m = find (\k -> isNothing (IM.lookup k m)) [0..maxKey]
  where
    (maxKey, _) = IM.findMax m

compressStep :: Disk -> Maybe Disk
compressStep d = do
  firstFree <- firstFreeSpace d
  (maxKey, val) <- IM.lookupMax d
  let (_, newDisk) = flip runState d $ do
                          modify $ IM.delete maxKey
                          modify $ IM.insert firstFree val
  pure newDisk

compress :: Disk -> Disk
compress d = maybe d compress (compressStep d)
