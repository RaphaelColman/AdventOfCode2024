module Common.MaybeUtils where

loopMaybe :: (a -> Maybe a) -> (a -> Bool) -> a -> Maybe a
loopMaybe step shouldFinish state
  | shouldFinish state = pure state
  | otherwise = step state >>= loopMaybe step shouldFinish

runUntilNothing :: (a -> Maybe a) -> a -> a
runUntilNothing f a = maybe a (runUntilNothing f) (f a)
