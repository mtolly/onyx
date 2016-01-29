module Song where

import Prelude
import Data.Time
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Data.Either

newtype Song = Song
  { end   :: Seconds
  , drums :: Maybe Drums
  }

newtype Drums = Drums
  { notes  :: Map.Map Seconds (Array Gem)
  , solo   :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  }

instance isForeignSong :: IsForeign Song where
  read f = do
    end <- readProp "end" f
    NullOrUndefined drums <- readProp "drums" f
    return $ Song { end: Seconds end, drums: drums }

readTimedMap :: forall a. (IsForeign a) => Foreign -> F (Map.Map Seconds a)
readTimedMap f = Map.fromFoldable <$> (readArray f >>= traverse readTimedPair)

readTimedPair :: forall a. (IsForeign a) => Foreign -> F (Tuple Seconds a)
readTimedPair pair = Tuple <$> (Seconds <$> readProp 0 pair) <*> readProp 1 pair

instance isForeignDrums :: IsForeign Drums where
  read f = do
    notes  <- readProp "notes"  f >>= readTimedMap
    solo   <- readProp "solo"   f >>= readTimedMap
    energy <- readProp "energy" f >>= readTimedMap
    return $ Drums { notes: notes, solo: solo, energy: energy }

data Gem = Kick | Red | YCym | YTom | BCym | BTom | GCym | GTom

instance isForeignGem :: IsForeign Gem where
  read f = read f >>= \s -> case s of
    "kick"  -> return Kick
    "red"   -> return Red
    "y-cym" -> return YCym
    "y-tom" -> return YTom
    "b-cym" -> return BCym
    "b-tom" -> return BTom
    "g-cym" -> return GCym
    "g-tom" -> return GTom
    _ -> Left $ TypeMismatch "drum gem name" $ show s

instance showGem :: Show Gem where
  show g = case g of
    Kick -> "Kick"
    Red  -> "Red"
    YCym -> "YCym"
    YTom -> "YTom"
    BCym -> "BCym"
    BTom -> "BTom"
    GCym -> "GCym"
    GTom -> "GTom"
