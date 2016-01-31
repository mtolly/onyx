-- | This module defines a type of maps as balanced 2-3 trees, based on
-- | <http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf>

module OnyxMap
  ( Map()
  , showTree
  , empty
  , isEmpty
  , singleton
  , checkValid
  , insert
  , lookup
  , lookupLE
  , lookupLT
  , lookupGE
  , lookupGT
  , findMin
  , findMax
  , fromFoldable
  , fromFoldableWith
  , toList
  , toAscList
  , toDescList
  , fromList
  , fromListWith
  , delete
  , member
  , alter
  , update
  , keys
  , values
  , union
  , unionWith
  , unions
  , size
  , zoomAscDo
  , zoomDescDo
  , doTupleList
  ) where

import Prelude

import Data.Foldable (foldl, foldMap, foldr, Foldable)
import Data.List (List(..), length, nub)
import Data.Maybe (Maybe(..), maybe, isJust, fromMaybe)
import Data.Maybe.Unsafe (unsafeThrow)
import Data.Monoid (Monoid)
import Data.Traversable (traverse, Traversable)
import Data.Tuple (Tuple(..), uncurry)
import Control.Monad (when)

import Control.Monad.Eff (runPure, Eff())
import Data.Array.ST (runSTArray, emptySTArray, pushSTArray)

-- | `Map k v` represents maps from keys of type `k` to values of type `v`.
data Map k v
  = Leaf
  | Two (Map k v) k v (Map k v)
  | Three (Map k v) k v (Map k v) k v (Map k v)

instance eqMap :: (Eq k, Eq v) => Eq (Map k v) where
  eq m1 m2 = toList m1 == toList m2

instance showMap :: (Show k, Show v) => Show (Map k v) where
  show m = "fromList " ++ show (toList m)

instance ordMap :: (Ord k, Ord v) => Ord (Map k v) where
  compare m1 m2 = compare (toList m1) (toList m2)

instance semigroupMap :: (Ord k) => Semigroup (Map k v) where
  append = union

instance monoidMap :: (Ord k) => Monoid (Map k v) where
  mempty = empty

instance functorMap :: Functor (Map k) where
  map _ Leaf = Leaf
  map f (Two left k v right) = Two (map f left) k (f v) (map f right)
  map f (Three left k1 v1 mid k2 v2 right) = Three (map f left) k1 (f v1) (map f mid) k2 (f v2) (map f right)

instance foldableMap :: Foldable (Map k) where
  foldl   f z m = foldl   f z (values m)
  foldr   f z m = foldr   f z (values m)
  foldMap f   m = foldMap f   (values m)

instance traversableMap :: (Ord k) => Traversable (Map k) where
  traverse f ms = foldr (\x acc -> union <$> x <*> acc) (pure empty) (((<$>) (uncurry singleton)) <$> (traverse f <$> toList ms))
  sequence = traverse id

-- | Render a `Map` as a `String`
showTree :: forall k v. (Show k, Show v) => Map k v -> String
showTree Leaf = "Leaf"
showTree (Two left k v right) =
  "Two (" ++ showTree left ++
  ") (" ++ show k ++
  ") (" ++ show v ++
  ") (" ++ showTree right ++ ")"
showTree (Three left k1 v1 mid k2 v2 right) =
  "Three (" ++ showTree left ++
  ") (" ++ show k1 ++
  ") (" ++ show v1 ++
  ") (" ++ showTree mid ++
  ") (" ++ show k2 ++
  ") (" ++ show v2 ++
  ") (" ++ showTree right ++ ")"

-- | An empty map
empty :: forall k v. Map k v
empty = Leaf

-- | Test if a map is empty
isEmpty :: forall k v. Map k v -> Boolean
isEmpty Leaf = true
isEmpty _ = false

-- | Create a map with one key/value pair
singleton :: forall k v. k -> v -> Map k v
singleton k v = Two Leaf k v Leaf

-- | Check whether the underlying tree satisfies the 2-3 invariant
-- |
-- | This function is provided for internal use.
checkValid :: forall k v. Map k v -> Boolean
checkValid tree = length (nub (allHeights tree)) == one
  where
  allHeights :: Map k v -> List Int
  allHeights Leaf = pure zero
  allHeights (Two left _ _ right) = map (\n -> n + one) (allHeights left ++ allHeights right)
  allHeights (Three left _ _ mid _ _ right) = map (\n -> n + one) (allHeights left ++ allHeights mid ++ allHeights right)

-- | Lookup a value for the specified key
lookup :: forall k v. (Ord k) => k -> Map k v -> Maybe v
lookup _ Leaf = Nothing
lookup k (Two _ k1 v _) | k == k1 = Just v
lookup k (Two left k1 _ _) | k < k1 = lookup k left
lookup k (Two _ _ _ right) = lookup k right
lookup k (Three _ k1 v1 _ _ _ _) | k == k1 = Just v1
lookup k (Three _ _ _ _ k2 v2 _) | k == k2 = Just v2
lookup k (Three left k1 _ _ _ _ _) | k < k1 = lookup k left
lookup k (Three _ k1 _ mid k2 _ _) | k1 < k && k <= k2 = lookup k mid
lookup k (Three _ _ _ _ _ _ right) = lookup k right

-- | Lookup a value for the specified key, or the greatest one less than it
lookupLE :: forall k v. (Ord k) => k -> Map k v -> Maybe { key :: k, value :: v }
lookupLE _ Leaf = Nothing
lookupLE k (Two left k1 v1 right) = case compare k k1 of
  EQ -> Just { key: k1, value: v1 }
  GT -> Just $ fromMaybe { key: k1, value: v1 } $ lookupLE k right
  LT -> lookupLE k left
lookupLE k (Three left k1 v1 mid k2 v2 right) = case compare k k2 of
  EQ -> Just { key: k2, value: v2 }
  GT -> Just $ fromMaybe { key: k2, value: v2 } $ lookupLE k right
  LT -> lookupLE k $ Two left k1 v1 mid

-- | Lookup a value for the greatest key less than the specified key
lookupLT :: forall k v. (Ord k) => k -> Map k v -> Maybe { key :: k, value :: v }
lookupLT _ Leaf = Nothing
lookupLT k (Two left k1 v1 right) = case compare k k1 of
  EQ -> findMax left
  GT -> Just $ fromMaybe { key: k1, value: v1 } $ lookupLT k right
  LT -> lookupLT k left
lookupLT k (Three left k1 v1 mid k2 v2 right) = case compare k k2 of
  EQ -> findMax $ Two left k1 v1 mid
  GT -> Just $ fromMaybe { key: k2, value: v2 } $ lookupLT k right
  LT -> lookupLT k $ Two left k1 v1 mid

-- | Lookup a value for the specified key, or the least one greater than it
lookupGE :: forall k v. (Ord k) => k -> Map k v -> Maybe { key :: k, value :: v }
lookupGE _ Leaf = Nothing
lookupGE k (Two left k1 v1 right) = case compare k k1 of
  EQ -> Just { key: k1, value: v1 }
  LT -> Just $ fromMaybe { key: k1, value: v1 } $ lookupGE k left
  GT -> lookupGE k right
lookupGE k (Three left k1 v1 mid k2 v2 right) = case compare k k1 of
  EQ -> Just { key: k1, value: v1 }
  LT -> Just $ fromMaybe { key: k1, value: v1 } $ lookupGE k left
  GT -> lookupGE k $ Two mid k2 v2 right

-- | Lookup a value for the least key greater than the specified key
lookupGT :: forall k v. (Ord k) => k -> Map k v -> Maybe { key :: k, value :: v }
lookupGT _ Leaf = Nothing
lookupGT k (Two left k1 v1 right) = case compare k k1 of
  EQ -> findMin right
  LT -> Just $ fromMaybe { key: k1, value: v1 } $ lookupGT k left
  GT -> lookupGT k right
lookupGT k (Three left k1 v1 mid k2 v2 right) = case compare k k1 of
  EQ -> findMin $ Two mid k2 v2 right
  LT -> Just $ fromMaybe { key: k1, value: v1 } $ lookupGT k left
  GT -> lookupGT k $ Two mid k2 v2 right

-- | Returns the pair with the greatest key
findMax :: forall k v. Map k v -> Maybe { key :: k, value :: v }
findMax Leaf = Nothing
findMax (Two _ k1 v1 right) = Just $ fromMaybe { key: k1, value: v1 } $ findMax right
findMax (Three _ _ _ _ k2 v2 right) = Just $ fromMaybe { key: k2, value: v2 } $ findMax right

-- | Returns the pair with the least key
findMin :: forall k v. Map k v -> Maybe { key :: k, value :: v }
findMin Leaf = Nothing
findMin (Two left k1 v1 _) = Just $ fromMaybe { key: k1, value: v1 } $ findMin left
findMin (Three left k1 v1 _ _ _ _) = Just $ fromMaybe { key: k1, value: v1 } $ findMin left

-- | Test if a key is a member of a map
member :: forall k v. (Ord k) => k -> Map k v -> Boolean
member k m = isJust (k `lookup` m)

data TreeContext k v
  = TwoLeft k v (Map k v)
  | TwoRight (Map k v) k v
  | ThreeLeft k v (Map k v) k v (Map k v)
  | ThreeMiddle (Map k v) k v k v (Map k v)
  | ThreeRight (Map k v) k v (Map k v) k v

fromZipper :: forall k v. (Ord k) => List (TreeContext k v) -> Map k v -> Map k v
fromZipper Nil tree = tree
fromZipper (Cons (TwoLeft k1 v1 right) ctx) left = fromZipper ctx (Two left k1 v1 right)
fromZipper (Cons (TwoRight left k1 v1) ctx) right = fromZipper ctx (Two left k1 v1 right)
fromZipper (Cons (ThreeLeft k1 v1 mid k2 v2 right) ctx) left = fromZipper ctx (Three left k1 v1 mid k2 v2 right)
fromZipper (Cons (ThreeMiddle left k1 v1 k2 v2 right) ctx) mid = fromZipper ctx (Three left k1 v1 mid k2 v2 right)
fromZipper (Cons (ThreeRight left k1 v1 mid k2 v2) ctx) right = fromZipper ctx (Three left k1 v1 mid k2 v2 right)

data KickUp k v = KickUp (Map k v) k v (Map k v)

-- | Insert a key/value pair into a map
insert :: forall k v. (Ord k) => k -> v -> Map k v -> Map k v
insert = down Nil
  where
  down :: List (TreeContext k v) -> k -> v -> Map k v -> Map k v
  down ctx k v Leaf = up ctx (KickUp Leaf k v Leaf)
  down ctx k v (Two left k1 _ right) | k == k1 = fromZipper ctx (Two left k v right)
  down ctx k v (Two left k1 v1 right) | k < k1 = down (Cons (TwoLeft k1 v1 right) ctx) k v left
  down ctx k v (Two left k1 v1 right) = down (Cons (TwoRight left k1 v1) ctx) k v right
  down ctx k v (Three left k1 _ mid k2 v2 right) | k == k1 = fromZipper ctx (Three left k v mid k2 v2 right)
  down ctx k v (Three left k1 v1 mid k2 _ right) | k == k2 = fromZipper ctx (Three left k1 v1 mid k v right)
  down ctx k v (Three left k1 v1 mid k2 v2 right) | k < k1 = down (Cons (ThreeLeft k1 v1 mid k2 v2 right) ctx) k v left
  down ctx k v (Three left k1 v1 mid k2 v2 right) | k1 < k && k <= k2 = down (Cons (ThreeMiddle left k1 v1 k2 v2 right) ctx) k v mid
  down ctx k v (Three left k1 v1 mid k2 v2 right) = down (Cons (ThreeRight left k1 v1 mid k2 v2) ctx) k v right

  up :: List (TreeContext k v) -> KickUp k v -> Map k v
  up Nil (KickUp left k v right) = Two left k v right
  up (Cons (TwoLeft k1 v1 right) ctx) (KickUp left k v mid) = fromZipper ctx (Three left k v mid k1 v1 right)
  up (Cons (TwoRight left k1 v1) ctx) (KickUp mid k v right) = fromZipper ctx (Three left k1 v1 mid k v right)
  up (Cons (ThreeLeft k1 v1 c k2 v2 d) ctx) (KickUp a k v b) = up ctx (KickUp (Two a k v b) k1 v1 (Two c k2 v2 d))
  up (Cons (ThreeMiddle a k1 v1 k2 v2 d) ctx) (KickUp b k v c) = up ctx (KickUp (Two a k1 v1 b) k v (Two c k2 v2 d))
  up (Cons (ThreeRight a k1 v1 b k2 v2) ctx) (KickUp c k v d) = up ctx (KickUp (Two a k1 v1 b) k2 v2 (Two c k v d))

-- | Delete a key and its corresponding value from a map
delete :: forall k v. (Ord k) => k -> Map k v -> Map k v
delete = down Nil
  where
  down :: List (TreeContext k v) -> k -> Map k v -> Map k v
  down ctx _ Leaf = fromZipper ctx Leaf
  down ctx k (Two Leaf k1 _ Leaf)
    | k == k1 = up ctx Leaf
  down ctx k (Two left k1 v1 right)
    | k == k1   = let max = maxNode left
                    in removeMaxNode (Cons (TwoLeft max.key max.value right) ctx) left
    | k < k1    = down (Cons (TwoLeft k1 v1 right) ctx) k left
    | otherwise = down (Cons (TwoRight left k1 v1) ctx) k right
  down ctx k (Three Leaf k1 v1 Leaf k2 v2 Leaf)
    | k == k1 = fromZipper ctx (Two Leaf k2 v2 Leaf)
    | k == k2 = fromZipper ctx (Two Leaf k1 v1 Leaf)
  down ctx k (Three left k1 v1 mid k2 v2 right)
    | k == k1 = let max = maxNode left
                  in removeMaxNode (Cons (ThreeLeft max.key max.value mid k2 v2 right) ctx) left
    | k == k2 = let max = maxNode mid
                  in removeMaxNode (Cons (ThreeMiddle left k1 v1 max.key max.value right) ctx) mid
    | k < k1               = down (Cons (ThreeLeft k1 v1 mid k2 v2 right) ctx) k left
    | k1 < k && k < k2 = down (Cons (ThreeMiddle left k1 v1 k2 v2 right) ctx) k mid
    | otherwise            = down (Cons (ThreeRight left k1 v1 mid k2 v2) ctx) k right

  up :: List (TreeContext k v) -> Map k v -> Map k v
  up Nil tree = tree
  up (Cons (TwoLeft k1 v1 Leaf) ctx) Leaf = fromZipper ctx (Two Leaf k1 v1 Leaf)
  up (Cons (TwoRight Leaf k1 v1) ctx) Leaf = fromZipper ctx (Two Leaf k1 v1 Leaf)
  up (Cons (TwoLeft k1 v1 (Two m k2 v2 r)) ctx) l = up ctx (Three l k1 v1 m k2 v2 r)
  up (Cons (TwoRight (Two l k1 v1 m) k2 v2) ctx) r = up ctx (Three l k1 v1 m k2 v2 r)
  up (Cons (TwoLeft k1 v1 (Three b k2 v2 c k3 v3 d)) ctx) a = fromZipper ctx (Two (Two a k1 v1 b) k2 v2 (Two c k3 v3 d))
  up (Cons (TwoRight (Three a k1 v1 b k2 v2 c) k3 v3) ctx) d = fromZipper ctx (Two (Two a k1 v1 b) k2 v2 (Two c k3 v3 d))
  up (Cons (ThreeLeft k1 v1 Leaf k2 v2 Leaf) ctx) Leaf = fromZipper ctx (Three Leaf k1 v1 Leaf k2 v2 Leaf)
  up (Cons (ThreeMiddle Leaf k1 v1 k2 v2 Leaf) ctx) Leaf = fromZipper ctx (Three Leaf k1 v1 Leaf k2 v2 Leaf)
  up (Cons (ThreeRight Leaf k1 v1 Leaf k2 v2) ctx) Leaf = fromZipper ctx (Three Leaf k1 v1 Leaf k2 v2 Leaf)
  up (Cons (ThreeLeft k1 v1 (Two b k2 v2 c) k3 v3 d) ctx) a = fromZipper ctx (Two (Three a k1 v1 b k2 v2 c) k3 v3 d)
  up (Cons (ThreeMiddle (Two a k1 v1 b) k2 v2 k3 v3 d) ctx) c = fromZipper ctx (Two (Three a k1 v1 b k2 v2 c) k3 v3 d)
  up (Cons (ThreeMiddle a k1 v1 k2 v2 (Two c k3 v3 d)) ctx) b = fromZipper ctx (Two a k1 v1 (Three b k2 v2 c k3 v3 d))
  up (Cons (ThreeRight a k1 v1 (Two b k2 v2 c) k3 v3) ctx) d = fromZipper ctx (Two a k1 v1 (Three b k2 v2 c k3 v3 d))
  up (Cons (ThreeLeft k1 v1 (Three b k2 v2 c k3 v3 d) k4 v4 e) ctx) a = fromZipper ctx (Three (Two a k1 v1 b) k2 v2 (Two c k3 v3 d) k4 v4 e)
  up (Cons (ThreeMiddle (Three a k1 v1 b k2 v2 c) k3 v3 k4 v4 e) ctx) d = fromZipper ctx (Three (Two a k1 v1 b) k2 v2 (Two c k3 v3 d) k4 v4 e)
  up (Cons (ThreeMiddle a k1 v1 k2 v2 (Three c k3 v3 d k4 v4 e)) ctx) b = fromZipper ctx (Three a k1 v1 (Two b k2 v2 c) k3 v3 (Two d k4 v4 e))
  up (Cons (ThreeRight a k1 v1 (Three b k2 v2 c k3 v3 d) k4 v4) ctx) e = fromZipper ctx (Three a k1 v1 (Two b k2 v2 c) k3 v3 (Two d k4 v4 e))
  up _ _ = unsafeThrow "Impossible case in 'up'"

  maxNode :: Map k v -> { key :: k, value :: v }
  maxNode m = case findMax m of
    Just kv -> kv
    Nothing -> unsafeThrow "Impossible case in 'maxNode'"

  removeMaxNode :: List (TreeContext k v) -> Map k v -> Map k v
  removeMaxNode ctx (Two Leaf _ _ Leaf) = up ctx Leaf
  removeMaxNode ctx (Two left k v right) = removeMaxNode (Cons (TwoRight left k v) ctx) right
  removeMaxNode ctx (Three Leaf k1 v1 Leaf _ _ Leaf) = up (Cons (TwoRight Leaf k1 v1) ctx) Leaf
  removeMaxNode ctx (Three left k1 v1 mid k2 v2 right) = removeMaxNode (Cons (ThreeRight left k1 v1 mid k2 v2) ctx) right
  removeMaxNode _ Leaf = unsafeThrow "Impossible case in 'removeMaxNode'"


-- | Insert the value, delete a value, or update a value for a key in a map
alter :: forall k v. (Ord k) => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k m = case f (k `lookup` m) of
  Nothing -> delete k m
  Just v -> insert k v m

-- | Update or delete the value for a key in a map
update :: forall k v. (Ord k) => (v -> Maybe v) -> k -> Map k v -> Map k v
update f k m = alter (maybe Nothing f) k m

-- | Convert any foldable collection of key/value pairs to a map.
-- | On key collision, later values take precedence over earlier ones.
fromFoldable :: forall f k v. (Ord k, Foldable f) => f (Tuple k v) -> Map k v
fromFoldable = foldl (\m (Tuple k v) -> insert k v m) empty

-- | Convert any foldable collection of key/value pairs to a map.
-- | On key collision, the values are configurably combined.
fromFoldableWith :: forall f k v. (Ord k, Foldable f) => (v -> v -> v) -> f (Tuple k v) -> Map k v
fromFoldableWith f = foldl (\m (Tuple k v) -> alter (combine v) k m) empty where
  combine v (Just v') = Just $ f v v'
  combine v Nothing = Just v

-- | Convert a map to a list of key/value pairs
toList :: forall k v. Map k v -> List (Tuple k v)
toList = toAscList

toAscList :: forall k v. Map k v -> List (Tuple k v)
toAscList Leaf = Nil
toAscList (Two left k v right) = toAscList left ++ pure (Tuple k v) ++ toAscList right
toAscList (Three left k1 v1 mid k2 v2 right) = toAscList left ++ pure (Tuple k1 v1) ++ toAscList mid ++ pure (Tuple k2 v2) ++ toAscList right

toDescList :: forall k v. Map k v -> List (Tuple k v)
toDescList Leaf = Nil
toDescList (Two left k v right) = toDescList right ++ pure (Tuple k v) ++ toDescList left
toDescList (Three left k1 v1 mid k2 v2 right) = toDescList right ++ pure (Tuple k2 v2) ++ toDescList mid ++ pure (Tuple k1 v1) ++ toDescList left

-- | Create a map from a list of key/value pairs
fromList :: forall k v. (Ord k) => List (Tuple k v) -> Map k v
fromList = fromFoldable

-- | Create a map from a list of key/value pairs, using the specified function
-- | to combine values for duplicate keys.
fromListWith :: forall k v. (Ord k) => (v -> v -> v) -> List (Tuple k v) -> Map k v
fromListWith = fromFoldableWith

-- | Get a list of the keys contained in a map
keys :: forall k v. Map k v -> List k
keys Leaf = Nil
keys (Two left k _ right) = keys left ++ pure k ++ keys right
keys (Three left k1 _ mid k2 _ right) = keys left ++ pure k1 ++ keys mid ++ pure k2 ++ keys right

-- | Get a list of the values contained in a map
values :: forall k v. Map k v -> List v
values Leaf = Nil
values (Two left _ v right) = values left ++ pure v ++ values right
values (Three left _ v1 mid _ v2 right) = values left ++ pure v1 ++ values mid ++ pure v2 ++ values right

-- | Compute the union of two maps, using the specified function
-- | to combine values for duplicate keys.
unionWith :: forall k v. (Ord k) => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f m1 m2 = foldl go m2 (toList m1)
  where
  go m (Tuple k v) = alter (Just <<< maybe v (f v)) k m

-- | Compute the union of two maps, preferring values from the first map in the case
-- | of duplicate keys
union :: forall k v. (Ord k) => Map k v -> Map k v -> Map k v
union = unionWith const

-- | Compute the union of a collection of maps
unions :: forall k v f. (Ord k, Foldable f) => f (Map k v) -> Map k v
unions = foldl union empty

-- | Calculate the number of key/value pairs in a map
size :: forall k v. Map k v -> Int
size = length <<< values

-- | Executes an action for key-value pairs with the key within certain bounds,
-- in ascending key order
zoomAscDo :: forall k v m. (Ord k, Monad m) => k -> k -> Map k v -> (k -> v -> m Unit) -> m Unit
zoomAscDo kmin kmax m act = case m of
  Leaf -> return unit
  Two left k v right -> do
    when (kmin < k) $ zoomAscDo kmin kmax left act
    when (kmin < k && k < kmax) $ act k v
    when (k < kmax) $ zoomAscDo kmin kmax right act
  Three left k1 v1 mid k2 v2 right -> do
    when (kmin < k1) $ zoomAscDo kmin kmax left act
    when (kmin < k1 && k1 < kmax) $ act k1 v1
    when (kmin < k2 && k1 < kmax) $ zoomAscDo kmin kmax mid act
    when (kmin < k2 && k2 < kmax) $ act k2 v2
    when (k2 < kmax) $ zoomAscDo kmin kmax right act

-- | Executes an action for key-value pairs with the key within certain bounds,
-- in descending key order
zoomDescDo :: forall k v m. (Ord k, Monad m) => k -> k -> Map k v -> (k -> v -> m Unit) -> m Unit
zoomDescDo kmin kmax m act = case m of
  Leaf -> return unit
  Two left k v right -> do
    when (k < kmax) $ zoomDescDo kmin kmax right act
    when (kmin < k && k < kmax) $ act k v
    when (kmin < k) $ zoomDescDo kmin kmax left act
  Three left k1 v1 mid k2 v2 right -> do
    when (k2 < kmax) $ zoomDescDo kmin kmax right act
    when (kmin < k2 && k2 < kmax) $ act k2 v2
    when (kmin < k2 && k1 < kmax) $ zoomDescDo kmin kmax mid act
    when (kmin < k1 && k1 < kmax) $ act k1 v1
    when (kmin < k1) $ zoomDescDo kmin kmax left act

-- | Converts `zoomAscDo` or `zoomDescDo` into an array-generating function
doTupleList :: forall k v. (forall e. (k -> v -> Eff e Unit) -> Eff e Unit) -> Array (Tuple k v)
doTupleList f = runPure $ runSTArray (do
  arr <- emptySTArray
  f $ \k v -> void $ pushSTArray arr $ Tuple k v
  return arr
)
