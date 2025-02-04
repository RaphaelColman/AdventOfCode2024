module Common.MapUtils where

import           Data.Either   (partitionEithers)
import           Data.Foldable (minimumBy, Foldable (toList))
import           Data.Function (on, (&))
import qualified Data.Map      as M
import qualified Data.IntMap as IM

mapIf :: (a -> Bool) -> (a -> a) -> M.Map k a -> M.Map k a
mapIf condition f =
  M.map
    (\a ->
       if condition a
         then f a
         else a)

partitionKeys ::
     (Ord k, Ord b, Ord c)
  => (k -> Either b c)
  -> (a -> a -> a)
  -> M.Map k a
  -> (M.Map b a, M.Map c a)
partitionKeys f combining map' =
  (M.fromListWith combining lefts, M.fromListWith combining rights)
  where
    (lefts, rights) =
      partitionEithers $
      map
        (\(k, a) ->
           let e = f k
            in either (\x -> Left (x, a)) (\x -> Right (x, a)) e) $
      M.toList map'

minimumValue :: (Ord a) => M.Map k a -> (k, a)
minimumValue = minimumBy (compare `on` snd) . M.toList


-- | A newtype wrapper around IntMap that allows it to behave more sensibly
-- as a monoid if its underlying type is also monoidal.
newtype MonoidIntMap a = MkMonoidIntMap {_map :: IM.IntMap a} deriving (Show)

instance (Monoid a) => Monoid (MonoidIntMap a) where
  mempty = MkMonoidIntMap IM.empty
  mappend = (<>)

instance (Semigroup a) => Semigroup (MonoidIntMap a) where
  (<>) (MkMonoidIntMap mapA) (MkMonoidIntMap mapB) = MkMonoidIntMap $ IM.unionWith (<>) mapA mapB

associateBy :: (Ord b, Foldable f) => (a -> b) -> f a -> M.Map b [a]
associateBy fn xs = M.fromListWith (++) $ map (\a -> (fn a, [a])) $ toList xs

flipMap :: (Ord a, Ord b) => M.Map a b -> M.Map b [a]
flipMap = M.foldrWithKey (\k v acc -> M.insertWith (++) v [k] acc) M.empty
