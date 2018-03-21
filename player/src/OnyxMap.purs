module OnyxMap
  ( module Data.Map
  , zoomAscDo
  , zoomDescDo
  , doTupleArray
  ) where

import Prelude (class Monad, class Ord, Unit, bind, discard, pure, void, ($))

import Data.Map

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Control.Monad.Eff (Eff, runPure)
import Data.Array.ST (runSTArray, emptySTArray, pushSTArray)

import Data.Monoid.Dual (Dual(..))
import Data.Functor.App (App(..))

-- | Executes an action for key-value pairs with the key within certain bounds,
-- in ascending key order
zoomAscDo :: forall k v m. (Ord k) => (Monad m) => k -> k -> Map k v -> (k -> v -> m Unit) -> m Unit
zoomAscDo kmin kmax m act = case foldSubmap (Just kmin) (Just kmax) (\k v -> App $ act k v) m of
  App f -> f

-- | Executes an action for key-value pairs with the key within certain bounds,
-- in descending key order
zoomDescDo :: forall k v m. (Ord k) => (Monad m) => k -> k -> Map k v -> (k -> v -> m Unit) -> m Unit
zoomDescDo kmin kmax m act = case foldSubmap (Just kmin) (Just kmax) (\k v -> Dual $ App $ act k v) m of
  Dual (App f) -> f

-- | Converts `zoomAscDo` or `zoomDescDo` into an array-generating function
doTupleArray :: forall k v. (forall e. (k -> v -> Eff e Unit) -> Eff e Unit) -> Array (Tuple k v)
doTupleArray f = runPure $ runSTArray (do
  arr <- emptySTArray
  f $ \k v -> void $ pushSTArray arr $ Tuple k v
  pure arr
)
