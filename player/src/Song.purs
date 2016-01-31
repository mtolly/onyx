module Song where

import Prelude
import Data.Time
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined
import qualified OnyxMap as Map
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Data.Either
import Data.Generic (Generic, gShow, gEq, gCompare)

newtype Song = Song
  { end   :: Seconds
  , beats :: Beats
  , drums :: Maybe Drums
  , guitar :: Maybe Five
  , bass :: Maybe Five
  , keys :: Maybe Five
  }

newtype Drums = Drums
  { notes  :: Map.Map Seconds (Array Gem)
  , solo   :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  }

data Sustainable a
  = SustainEnd
  | Note a
  | Sustain a

data GuitarNoteType = Strum | HOPO

newtype Five = Five
  { notes ::
    { green  :: Map.Map Seconds (Sustainable GuitarNoteType)
    , red    :: Map.Map Seconds (Sustainable GuitarNoteType)
    , yellow :: Map.Map Seconds (Sustainable GuitarNoteType)
    , blue   :: Map.Map Seconds (Sustainable GuitarNoteType)
    , orange :: Map.Map Seconds (Sustainable GuitarNoteType)
    }
  , solo :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  }

instance isForeignFiveNote :: IsForeign (Sustainable GuitarNoteType) where
  read f = read f >>= \s -> case s of
    "end"  -> return SustainEnd
    "strum" -> return $ Note Strum
    "hopo" -> return $ Note HOPO
    "strum-sust" -> return $ Sustain Strum
    "hopo-sust" -> return $ Sustain HOPO
    _ -> Left $ TypeMismatch "grybo note event" $ show s

instance isForeignPKNote :: IsForeign (Sustainable Unit) where
  read f = read f >>= \s -> case s of
    "end"  -> return SustainEnd
    "note" -> return $ Note unit
    "sust" -> return $ Sustain unit
    _ -> Left $ TypeMismatch "pro keys note event" $ show s

instance isForeignFive :: IsForeign Five where
  read f = do
    notes <- readProp "notes" f
    let readColor s = readProp s notes >>= readTimedMap
    green  <- readColor "green"
    red    <- readColor "red"
    yellow <- readColor "yellow"
    blue   <- readColor "blue"
    orange <- readColor "orange"
    solo <- readProp "solo" f >>= readTimedMap
    energy <- readProp "energy" f >>= readTimedMap
    return $ Five
      { notes:
        { green: green
        , red: red
        , yellow: yellow
        , blue: blue
        , orange: orange
        }
      , solo: solo
      , energy: energy
      }

instance isForeignSong :: IsForeign Song where
  read f = do
    end <- readProp "end" f
    beats <- readProp "beats" f
    NullOrUndefined drums <- readProp "drums" f
    NullOrUndefined guitar <- readProp "guitar" f
    NullOrUndefined bass <- readProp "bass" f
    NullOrUndefined keys <- readProp "keys" f
    return $ Song
      { end: Seconds end
      , beats: beats
      , drums: drums
      , guitar: guitar
      , bass: bass
      , keys: keys
      }

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

derive instance genGem :: Generic Gem

instance showGem :: Show Gem where
  show = gShow

instance eqGem :: Eq Gem where
  eq = gEq

instance ordGem :: Ord Gem where
  compare = gCompare

newtype Beats = Beats
  { lines :: Map.Map Seconds Beat
  }

instance isForeignBeats :: IsForeign Beats where
  read f = do
    lines <- readProp "lines"  f >>= readTimedMap
    return $ Beats { lines: lines }

data Beat
  = Bar
  | Beat
  | HalfBeat

instance isForeignBeat :: IsForeign Beat where
  read f = read f >>= \s -> case s of
    "bar"      -> return Bar
    "beat"     -> return Beat
    "halfbeat" -> return HalfBeat
    _          -> Left $ TypeMismatch "bar/beat/halfbeat" $ show s

instance showBeat :: Show Beat where
  show g = case g of
    Bar      -> "Bar"
    Beat     -> "Beat"
    HalfBeat -> "HalfBeat"
