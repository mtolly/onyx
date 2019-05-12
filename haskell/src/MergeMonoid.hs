{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module MergeMonoid where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           GHC.Generics
import           GHC.TypeLits
import qualified Numeric.NonNegative.Class        as NNC

class Mergeable a where
  mergeEmpty :: a
  merge :: a -> a -> a

instance (NNC.C t, Ord a) => Mergeable (RTB.T t a) where
  mergeEmpty = RTB.empty
  merge = RTB.merge

instance (Ord k, Mergeable v) => Mergeable (Map.Map k v) where
  mergeEmpty = Map.empty
  merge = Map.unionWith merge

-- Generic techniques borrowed from the `generic-monoid` package

newtype GenericMerge a = GenericMerge a

instance (Generic a, MergeProduct (Rep a)) => Mergeable (GenericMerge a) where
  GenericMerge a `merge` GenericMerge b = GenericMerge $ genericMerge a b
  mergeEmpty = GenericMerge genericMergeEmpty

instance (Generic a, MergeProduct (Rep a)) => Semigroup (GenericMerge a) where
  (<>) = merge

instance (Generic a, MergeProduct (Rep a)) => Monoid (GenericMerge a) where
  mempty = mergeEmpty

genericMerge :: (Generic a, MergeProduct (Rep a)) => a -> a -> a
genericMerge a b = to $ from a `genericMerge'` from b

genericMergeEmpty :: (Generic a, MergeProduct (Rep a)) => a
genericMergeEmpty = to genericMergeEmpty'

class MergeProduct f where
  genericMerge' :: f k -> f k -> f k
  genericMergeEmpty' :: f k

instance (TypeError ('Text "You can't use `genericMerge` for sum types"))
  => MergeProduct (a :+: b) where
  genericMerge' = undefined
  genericMergeEmpty' = undefined

instance MergeProduct c => MergeProduct (D1 md c) where
  genericMerge' (M1 a) (M1 b) = M1 (genericMerge' a b)
  genericMergeEmpty' = M1 genericMergeEmpty'

instance MergeProduct s => MergeProduct (C1 mc s) where
  genericMerge' (M1 a) (M1 b) = M1 (genericMerge' a b)
  genericMergeEmpty' = M1 genericMergeEmpty'

instance (MergeProduct a, MergeProduct b) => MergeProduct (a :*: b) where
  genericMerge' (a :*: b) (a' :*: b') = genericMerge' a a' :*: genericMerge' b b'
  genericMergeEmpty' = genericMergeEmpty' :*: genericMergeEmpty'

instance Mergeable t => MergeProduct (S1 m (Rec0 t)) where
  genericMerge' (M1 (K1 a)) (M1 (K1 b)) = M1 (K1 (a `merge` b))
  genericMergeEmpty' = M1 (K1 mergeEmpty)
