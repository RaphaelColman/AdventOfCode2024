{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Floyd where

import Common.ListUtils (dropUntil)
import Control.Lens (makeLenses, (^.))
import Control.Monad ((>=>))
import Control.Monad.Extra (iterateM)
import Data.Foldable (find)
import Data.List (elemIndex)
import Debug.Trace (traceM, traceShow, traceShowM)
import GHC.OldList (findIndex)

data CycleData a
  = MkCycleData
  { _index :: !Int,
    _length :: !Int,
    _node :: !a
  }
  deriving (Eq, Show)

data HTState a = MkHTState
  { _tortoise :: !a,
    _hare :: !a
  }
  deriving (Eq, Show)

makeLenses ''CycleData
makeLenses ''HTState

hareAndTortoise ::
  (Eq b) =>
  (a -> a) -> -- Function move from one node to the next
  a -> -- Initial state
  (a -> b) -> -- Function to compare if two nodes are equal
  Maybe (CycleData a)
hareAndTortoise f start eqFun = do
  let hare = drop 1 $ iterate (f . f) start
  let tortoise = drop 1 $ iterate f start
  let pairs = zip hare tortoise
  (t, h) <- find (uncurry areEqual) pairs -- This will go forever unless I impose a limit of some sort
  (startIndex, (t', _)) <- find (\(_, y) -> uncurry areEqual y) $ zip [0 ..] $ zip (iterate f start) (iterate f h)
  length <- findIndex (areEqual t') $ drop 1 $ iterate f t' -- This will be too low by 1 because I had to drop the first element
  pure $ MkCycleData startIndex (length + 1) t'
  where
    areEqual x y = eqFun x == eqFun y

hareAndTortoiseM ::
  forall a b.
  (Eq b) =>
  (a -> Maybe a) -> -- Function move from one node to the next. If it returns a Nothing then we stop the search
  a -> -- Initial state
  (a -> b) -> -- Function to compare if two nodes are equal
  Bool -- Just if there was a cycle. Can't be bothered to do the whole algorithm right now
hareAndTortoiseM f start eqFun = go (MkHTState start start)
  where
    go htState = case progressState htState of
      Nothing -> False --There was no cycle before we hit the end
      Just newState -> ((newState ^. tortoise) `areEqual` (newState ^. hare)) || go newState
    progressState :: HTState a -> Maybe (HTState a)
    progressState htState = do
      tortoise <- f (htState ^. tortoise)
      hare <- (f >=> f ) (htState ^. hare)
      pure $ MkHTState tortoise hare
    areEqual x y = eqFun x == eqFun y
