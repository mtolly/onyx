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
  , prokeys :: Maybe ProKeys
  , vocal :: Maybe Vocal
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

newtype ProKeys = ProKeys
  { notes  :: Map.Map Pitch (Map.Map Seconds (Sustainable Unit))
  , ranges :: Map.Map Seconds Range
  , solo   :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  }

data Range
  = RangeC
  | RangeD
  | RangeE
  | RangeF
  | RangeG
  | RangeA

instance isForeignRange :: IsForeign Range where
  read f = read f >>= \s -> case s of
    "c" -> return RangeC
    "d" -> return RangeD
    "e" -> return RangeE
    "f" -> return RangeF
    "g" -> return RangeG
    "a" -> return RangeA
    _ -> Left $ TypeMismatch "pro keys range" $ show s

data Pitch
  = RedC
  | RedCs
  | RedD
  | RedDs
  | RedE
  | YellowF
  | YellowFs
  | YellowG
  | YellowGs
  | YellowA
  | YellowAs
  | YellowB
  | BlueC
  | BlueCs
  | BlueD
  | BlueDs
  | BlueE
  | GreenF
  | GreenFs
  | GreenG
  | GreenGs
  | GreenA
  | GreenAs
  | GreenB
  | OrangeC

derive instance genPitch :: Generic Pitch
instance showPitch :: Show Pitch where
  show = gShow
instance eqPitch :: Eq Pitch where
  eq = gEq
instance ordPitch :: Ord Pitch where
  compare = gCompare

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

instance isForeignProKeys :: IsForeign ProKeys where
  read f = do
    notes <- readProp "notes" f
    let readPitch p s = map (Tuple p) $ readProp s notes >>= readTimedMap
    pitches <- sequence
      [ readPitch RedC "ry-c"
      , readPitch RedCs "ry-cs"
      , readPitch RedD "ry-d"
      , readPitch RedDs "ry-ds"
      , readPitch RedE "ry-e"
      , readPitch YellowF "ry-f"
      , readPitch YellowFs "ry-fs"
      , readPitch YellowG "ry-g"
      , readPitch YellowGs "ry-gs"
      , readPitch YellowA "ry-a"
      , readPitch YellowAs "ry-as"
      , readPitch YellowB "ry-b"
      , readPitch BlueC "bg-c"
      , readPitch BlueCs "bg-cs"
      , readPitch BlueD "bg-d"
      , readPitch BlueDs "bg-ds"
      , readPitch BlueE "bg-e"
      , readPitch GreenF "bg-f"
      , readPitch GreenFs "bg-fs"
      , readPitch GreenG "bg-g"
      , readPitch GreenGs "bg-gs"
      , readPitch GreenA "bg-a"
      , readPitch GreenAs "bg-as"
      , readPitch GreenB "bg-b"
      , readPitch OrangeC "o-c"
      ]
    solo <- readProp "solo" f >>= readTimedMap
    energy <- readProp "energy" f >>= readTimedMap
    ranges <- readProp "ranges" f >>= readTimedMap
    return $ ProKeys
      { notes: Map.fromFoldable pitches
      , solo: solo
      , energy: energy
      , ranges: ranges
      }

instance isForeignSong :: IsForeign Song where
  read f = do
    end <- readProp "end" f
    beats <- readProp "beats" f
    NullOrUndefined drums <- readProp "drums" f
    NullOrUndefined guitar <- readProp "guitar" f
    NullOrUndefined bass <- readProp "bass" f
    NullOrUndefined keys <- readProp "keys" f
    NullOrUndefined prokeys <- readProp "prokeys" f
    NullOrUndefined vocal <- readProp "vocal" f
    return $ Song
      { end: Seconds end
      , beats: beats
      , drums: drums
      , guitar: guitar
      , bass: bass
      , keys: keys
      , prokeys: prokeys
      , vocal: vocal
      }

readTimedSet :: Foreign -> F (Map.Map Seconds Unit)
readTimedSet f = map (map $ \(_ :: Foreign) -> unit) $ readTimedMap f

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

data Vocal = Vocal
  { harm1 :: Map.Map Seconds VocalNote
  , harm2 :: Map.Map Seconds VocalNote
  , harm3 :: Map.Map Seconds VocalNote
  , percussion :: Map.Map Seconds Unit
  , phrases :: Map.Map Seconds Unit
  , ranges :: Map.Map Seconds VocalRange
  , energy :: Map.Map Seconds Boolean
  , tonic :: Maybe Int
  }

instance isForeignVocal :: IsForeign Vocal where
  read f = do
    harm1 <- readProp "harm1"  f >>= readTimedMap
    harm2 <- readProp "harm2"  f >>= readTimedMap
    harm3 <- readProp "harm3"  f >>= readTimedMap
    energy <- readProp "energy" f >>= readTimedMap
    ranges <- readProp "ranges" f >>= readTimedMap
    NullOrUndefined tonic <- readProp "tonic" f
    percussion <- readProp "percussion" f >>= readTimedSet
    phrases <- readProp "phrases" f >>= readTimedSet
    return $ Vocal { harm1: harm1, harm2: harm2, harm3: harm3, energy: energy, ranges: ranges, tonic: tonic, percussion: percussion, phrases: phrases }

data VocalRange
  = VocalRangeShift    -- ^ Start of a range shift
  | VocalRange Int Int -- ^ The starting range, or the end of a range shift

instance isForeignVocalRange :: IsForeign VocalRange where
  read f = if isNull f
    then return VocalRangeShift
    else VocalRange <$> readProp 0 f <*> readProp 1 f

data VocalNote
  = VocalStart String (Maybe Int)
  | VocalEnd

instance isForeignVocalNote :: IsForeign VocalNote where
  read f = if isNull f
    then return VocalEnd
    else do
      lyric <- readProp 0 f
      NullOrUndefined pitch <- readProp 1 f
      return $ VocalStart lyric pitch
