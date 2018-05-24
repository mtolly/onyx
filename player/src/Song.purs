module Song where

import Prelude (class Eq, class Ord, class Show, Unit, bind, map, pure, show, unit, ($), (<$>), (<*>), (>>=), (+), (<), (<<<))

import Data.Time.Duration (Seconds(..))
import Data.Foreign (F, Foreign, ForeignError(..), isNull, readArray, readBoolean, readInt, readNullOrUndefined, readNumber, readString)
import Data.Foreign.Index (readProp, readIndex)
import OnyxMap as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Generic (class Generic, gShow, gEq, gCompare)
import Control.Monad.Except (throwError)
import Data.String as Str
import Data.Array ((:))

newtype Song = Song
  { end    :: Seconds
  , beats  :: Beats
  , parts  :: Array (Tuple String Flex)
  , title  :: String
  , artist :: String
  }

newtype Flex = Flex
  { five :: Maybe Five
  , six :: Maybe Six
  , drums :: Maybe Drums
  , prokeys :: Maybe ProKeys
  , protar :: Maybe Protar
  , vocal :: Maybe Vocal
  }

data FlexPart
  = FlexFive
  | FlexSix
  | FlexDrums
  | FlexProKeys
  | FlexProtar
  | FlexVocal

derive instance genFlexPart :: Generic FlexPart
instance showFlexPart :: Show FlexPart where
  show = gShow
instance eqFlexPart :: Eq FlexPart where
  eq = gEq
instance ordFlexPart :: Ord FlexPart where
  compare = gCompare

newtype Drums = Drums
  { notes  :: Map.Map Seconds (Array Gem)
  , lanes  :: Map.Map Gem (Map.Map Seconds Boolean)
  , solo   :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  , bre    :: Map.Map Seconds Boolean
  , mode5  :: Boolean
  }

data Sustainable a
  = SustainEnd
  | Note a
  | Sustain a

data GuitarNoteType = Strum | HOPO | Tap

type FiveEach a =
  { open   :: a
  , green  :: a
  , red    :: a
  , yellow :: a
  , blue   :: a
  , orange :: a
  }

newtype Five = Five
  { notes :: FiveEach (Map.Map Seconds (Sustainable GuitarNoteType))
  , lanes :: FiveEach (Map.Map Seconds Boolean)
  , solo :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  , bre :: Map.Map Seconds Boolean
  }

newtype Six = Six
  { notes ::
    { open :: Map.Map Seconds (Sustainable GuitarNoteType)
    , b1   :: Map.Map Seconds (Sustainable GuitarNoteType)
    , b2   :: Map.Map Seconds (Sustainable GuitarNoteType)
    , b3   :: Map.Map Seconds (Sustainable GuitarNoteType)
    , w1   :: Map.Map Seconds (Sustainable GuitarNoteType)
    , w2   :: Map.Map Seconds (Sustainable GuitarNoteType)
    , w3   :: Map.Map Seconds (Sustainable GuitarNoteType)
    , bw1  :: Map.Map Seconds (Sustainable GuitarNoteType)
    , bw2  :: Map.Map Seconds (Sustainable GuitarNoteType)
    , bw3  :: Map.Map Seconds (Sustainable GuitarNoteType)
    }
  , solo :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  , bre :: Map.Map Seconds Boolean
  }

newtype ProtarNote = ProtarNote
  { noteType :: GuitarNoteType
  , fret     :: Maybe Int
  }

type ProtarEach a =
  { s1 :: a
  , s2 :: a
  , s3 :: a
  , s4 :: a
  , s5 :: a
  , s6 :: a
  }

data ChordLine = Baseline | Superscript

newtype Protar = Protar
  { notes :: ProtarEach (Map.Map Seconds (Sustainable ProtarNote))
  , lanes :: ProtarEach (Map.Map Seconds Boolean)
  , solo :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  , bre :: Map.Map Seconds Boolean
  , chords :: Map.Map Seconds (Sustainable (Array (Tuple ChordLine String)))
  }

newtype ProKeys = ProKeys
  { notes  :: Map.Map Pitch (Map.Map Seconds (Sustainable Unit))
  , lanes  :: Map.Map Pitch (Map.Map Seconds Boolean)
  , ranges :: Map.Map Seconds Range
  , solo   :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  , gliss :: Map.Map Seconds Boolean
  , bre :: Map.Map Seconds Boolean
  }

data Range
  = RangeC
  | RangeD
  | RangeE
  | RangeF
  | RangeG
  | RangeA

isForeignRange :: Foreign -> F Range
isForeignRange f = readString f >>= \s -> case s of
  "c" -> pure RangeC
  "d" -> pure RangeD
  "e" -> pure RangeE
  "f" -> pure RangeF
  "g" -> pure RangeG
  "a" -> pure RangeA
  _ -> throwError $ pure $ TypeMismatch "pro keys range" $ show s

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

isForeignFiveNote :: Foreign -> F (Sustainable GuitarNoteType)
isForeignFiveNote f = readString f >>= \s -> case s of
  "end"  -> pure SustainEnd
  "strum" -> pure $ Note Strum
  "hopo" -> pure $ Note HOPO
  "tap" -> pure $ Note Tap
  "strum-sust" -> pure $ Sustain Strum
  "hopo-sust" -> pure $ Sustain HOPO
  "tap-sust" -> pure $ Sustain Tap
  _ -> throwError $ pure $ TypeMismatch "grybo/ghl note event" $ show s

isForeignProtarNote :: Foreign -> F (Sustainable ProtarNote)
isForeignProtarNote f = readString f >>= \s -> case s of
  "end"  -> pure SustainEnd
  "strum-x" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Nothing }
  "strum-0" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 0 }
  "strum-1" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 1 }
  "strum-2" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 2 }
  "strum-3" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 3 }
  "strum-4" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 4 }
  "strum-5" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 5 }
  "strum-6" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 6 }
  "strum-7" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 7 }
  "strum-8" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 8 }
  "strum-9" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 9 }
  "strum-10" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 10 }
  "strum-11" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 11 }
  "strum-12" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 12 }
  "strum-13" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 13 }
  "strum-14" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 14 }
  "strum-15" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 15 }
  "strum-16" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 16 }
  "strum-17" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 17 }
  "strum-18" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 18 }
  "strum-19" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 19 }
  "strum-20" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 20 }
  "strum-21" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 21 }
  "strum-22" -> pure $ Note $ ProtarNote { noteType: Strum, fret: Just 22 }
  "hopo-x" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Nothing }
  "hopo-0" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 0 }
  "hopo-1" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 1 }
  "hopo-2" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 2 }
  "hopo-3" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 3 }
  "hopo-4" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 4 }
  "hopo-5" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 5 }
  "hopo-6" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 6 }
  "hopo-7" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 7 }
  "hopo-8" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 8 }
  "hopo-9" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 9 }
  "hopo-10" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 10 }
  "hopo-11" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 11 }
  "hopo-12" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 12 }
  "hopo-13" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 13 }
  "hopo-14" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 14 }
  "hopo-15" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 15 }
  "hopo-16" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 16 }
  "hopo-17" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 17 }
  "hopo-18" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 18 }
  "hopo-19" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 19 }
  "hopo-20" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 20 }
  "hopo-21" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 21 }
  "hopo-22" -> pure $ Note $ ProtarNote { noteType: HOPO, fret: Just 22 }
  "tap-x" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Nothing }
  "tap-0" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 0 }
  "tap-1" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 1 }
  "tap-2" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 2 }
  "tap-3" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 3 }
  "tap-4" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 4 }
  "tap-5" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 5 }
  "tap-6" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 6 }
  "tap-7" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 7 }
  "tap-8" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 8 }
  "tap-9" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 9 }
  "tap-10" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 10 }
  "tap-11" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 11 }
  "tap-12" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 12 }
  "tap-13" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 13 }
  "tap-14" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 14 }
  "tap-15" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 15 }
  "tap-16" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 16 }
  "tap-17" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 17 }
  "tap-18" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 18 }
  "tap-19" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 19 }
  "tap-20" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 20 }
  "tap-21" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 21 }
  "tap-22" -> pure $ Note $ ProtarNote { noteType: Tap, fret: Just 22 }
  "strum-sust-x" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Nothing }
  "strum-sust-0" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 0 }
  "strum-sust-1" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 1 }
  "strum-sust-2" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 2 }
  "strum-sust-3" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 3 }
  "strum-sust-4" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 4 }
  "strum-sust-5" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 5 }
  "strum-sust-6" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 6 }
  "strum-sust-7" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 7 }
  "strum-sust-8" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 8 }
  "strum-sust-9" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 9 }
  "strum-sust-10" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 10 }
  "strum-sust-11" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 11 }
  "strum-sust-12" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 12 }
  "strum-sust-13" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 13 }
  "strum-sust-14" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 14 }
  "strum-sust-15" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 15 }
  "strum-sust-16" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 16 }
  "strum-sust-17" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 17 }
  "strum-sust-18" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 18 }
  "strum-sust-19" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 19 }
  "strum-sust-20" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 20 }
  "strum-sust-21" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 21 }
  "strum-sust-22" -> pure $ Sustain $ ProtarNote { noteType: Strum, fret: Just 22 }
  "hopo-sust-x" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Nothing }
  "hopo-sust-0" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 0 }
  "hopo-sust-1" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 1 }
  "hopo-sust-2" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 2 }
  "hopo-sust-3" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 3 }
  "hopo-sust-4" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 4 }
  "hopo-sust-5" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 5 }
  "hopo-sust-6" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 6 }
  "hopo-sust-7" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 7 }
  "hopo-sust-8" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 8 }
  "hopo-sust-9" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 9 }
  "hopo-sust-10" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 10 }
  "hopo-sust-11" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 11 }
  "hopo-sust-12" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 12 }
  "hopo-sust-13" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 13 }
  "hopo-sust-14" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 14 }
  "hopo-sust-15" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 15 }
  "hopo-sust-16" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 16 }
  "hopo-sust-17" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 17 }
  "hopo-sust-18" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 18 }
  "hopo-sust-19" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 19 }
  "hopo-sust-20" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 20 }
  "hopo-sust-21" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 21 }
  "hopo-sust-22" -> pure $ Sustain $ ProtarNote { noteType: HOPO, fret: Just 22 }
  "tap-sust-x" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Nothing }
  "tap-sust-0" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 0 }
  "tap-sust-1" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 1 }
  "tap-sust-2" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 2 }
  "tap-sust-3" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 3 }
  "tap-sust-4" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 4 }
  "tap-sust-5" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 5 }
  "tap-sust-6" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 6 }
  "tap-sust-7" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 7 }
  "tap-sust-8" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 8 }
  "tap-sust-9" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 9 }
  "tap-sust-10" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 10 }
  "tap-sust-11" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 11 }
  "tap-sust-12" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 12 }
  "tap-sust-13" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 13 }
  "tap-sust-14" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 14 }
  "tap-sust-15" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 15 }
  "tap-sust-16" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 16 }
  "tap-sust-17" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 17 }
  "tap-sust-18" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 18 }
  "tap-sust-19" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 19 }
  "tap-sust-20" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 20 }
  "tap-sust-21" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 21 }
  "tap-sust-22" -> pure $ Sustain $ ProtarNote { noteType: Tap, fret: Just 22 }
  _ -> throwError $ pure $ TypeMismatch "protar note event" $ show s

isForeignPKNote :: Foreign -> F (Sustainable Unit)
isForeignPKNote f = readString f >>= \s -> case s of
  "end"  -> pure SustainEnd
  "note" -> pure $ Note unit
  "sust" -> pure $ Sustain unit
  _ -> throwError $ pure $ TypeMismatch "pro keys note event" $ show s

isForeignFive :: Foreign -> F Five
isForeignFive f = do
  notesF <- readProp "notes" f
  lanesF <- readProp "lanes" f
  let readEach :: forall a. (String -> F a) -> F (FiveEach a)
      readEach g = do
        open <- g "open"
        green <- g "green"
        red <- g "red"
        yellow <- g "yellow"
        blue <- g "blue"
        orange <- g "orange"
        pure
          { open: open
          , green: green
          , red: red
          , yellow: yellow
          , blue: blue
          , orange: orange
          }
  notes <- readEach \s -> readProp s notesF >>= readTimedMap isForeignFiveNote
  lanes <- readEach \s -> readProp s lanesF >>= readTimedMap readBoolean
  solo   <- readProp "solo" f >>= readTimedMap readBoolean
  energy <- readProp "energy" f >>= readTimedMap readBoolean
  bre <- readProp "bre" f >>= readTimedMap readBoolean
  pure $ Five
    { notes: notes
    , lanes: lanes
    , solo: solo
    , energy: energy
    , bre: bre
    }

isForeignSix :: Foreign -> F Six
isForeignSix f = do
  notesF  <- readProp "notes" f
  let readLane s = readProp s notesF >>= readTimedMap isForeignFiveNote
  open <- readLane "open"
  b1   <- readLane "b1"
  b2   <- readLane "b2"
  b3   <- readLane "b3"
  w1   <- readLane "w1"
  w2   <- readLane "w2"
  w3   <- readLane "w3"
  bw1  <- readLane "bw1"
  bw2  <- readLane "bw2"
  bw3  <- readLane "bw3"
  solo   <- readProp "solo" f >>= readTimedMap readBoolean
  energy <- readProp "energy" f >>= readTimedMap readBoolean
  bre <- readProp "bre" f >>= readTimedMap readBoolean
  pure $ Six
    { notes:
      { open: open
      , b1: b1
      , b2: b2
      , b3: b3
      , w1: w1
      , w2: w2
      , w3: w3
      , bw1: bw1
      , bw2: bw2
      , bw3: bw3
      }
    , solo: solo
    , energy: energy
    , bre: bre
    }

isForeignChord :: Foreign -> F (Sustainable (Array (Tuple ChordLine String)))
isForeignChord f = readString f >>= \s -> case Str.take 1 s of
  "e" -> pure SustainEnd
  "s" -> pure $ Sustain $ splitChord Baseline $ specialChars $ Str.drop 2 s
  "b" -> pure $ Note    $ splitChord Baseline $ specialChars $ Str.drop 2 s
  _   -> throwError $ pure $ TypeMismatch "pro guitar chord name event" $ show s
  where splitChord _    "" = []
        splitChord mode s  = case Str.indexOf (Str.Pattern "<gtr>") s of
          Nothing -> case Str.indexOf (Str.Pattern "</gtr>") s of
            Nothing -> [Tuple mode s]
            Just closeAt -> Tuple mode (Str.take closeAt s) : splitChord Baseline (Str.drop (closeAt + 6) s)
          Just openAt -> case Str.indexOf (Str.Pattern "</gtr>") s of
            Nothing -> Tuple mode (Str.take openAt s) : splitChord Superscript (Str.drop (openAt + 5) s)
            Just closeAt -> if openAt < closeAt
              then Tuple mode (Str.take openAt s) : splitChord Superscript (Str.drop (openAt + 5) s)
              else Tuple mode (Str.take closeAt s) : splitChord Baseline (Str.drop (closeAt + 6) s)
        specialChars
          =   Str.replaceAll (Str.Pattern "#") (Str.Replacement "♯")
          <<< Str.replaceAll (Str.Pattern "b") (Str.Replacement "♭")
          <<< Str.replaceAll (Str.Pattern "0") (Str.Replacement "○")

isForeignProtar :: Foreign -> F Protar
isForeignProtar f = do
  notesF <- readProp "notes" f
  lanesF <- readProp "lanes" f
  let readEach :: forall a. (String -> F a) -> F (ProtarEach a)
      readEach g = do
        s1 <- g "s1"
        s2 <- g "s2"
        s3 <- g "s3"
        s4 <- g "s4"
        s5 <- g "s5"
        s6 <- g "s6"
        pure
          { s1: s1
          , s2: s2
          , s3: s3
          , s4: s4
          , s5: s5
          , s6: s6
          }
  notes <- readEach \s -> readProp s notesF >>= readTimedMap isForeignProtarNote
  lanes <- readEach \s -> readProp s lanesF >>= readTimedMap readBoolean
  solo <- readProp "solo" f >>= readTimedMap readBoolean
  energy <- readProp "energy" f >>= readTimedMap readBoolean
  bre <- readProp "bre" f >>= readTimedMap readBoolean
  chords <- readProp "chords" f >>= readTimedMap isForeignChord
  pure $ Protar
    { notes: notes
    , lanes: lanes
    , solo: solo
    , energy: energy
    , bre: bre
    , chords: chords
    }

isForeignProKeys :: Foreign -> F ProKeys
isForeignProKeys f = do
  notesF <- readProp "notes" f
  lanesF <- readProp "lanes" f
  let readEach :: forall a. (String -> F a) -> F (Map.Map Pitch a)
      readEach g = let
        g' p s = map (Tuple p) $ g s
        in map Map.fromFoldable $ sequence
          [ g' RedC "ry-c"
          , g' RedCs "ry-cs"
          , g' RedD "ry-d"
          , g' RedDs "ry-ds"
          , g' RedE "ry-e"
          , g' YellowF "ry-f"
          , g' YellowFs "ry-fs"
          , g' YellowG "ry-g"
          , g' YellowGs "ry-gs"
          , g' YellowA "ry-a"
          , g' YellowAs "ry-as"
          , g' YellowB "ry-b"
          , g' BlueC "bg-c"
          , g' BlueCs "bg-cs"
          , g' BlueD "bg-d"
          , g' BlueDs "bg-ds"
          , g' BlueE "bg-e"
          , g' GreenF "bg-f"
          , g' GreenFs "bg-fs"
          , g' GreenG "bg-g"
          , g' GreenGs "bg-gs"
          , g' GreenA "bg-a"
          , g' GreenAs "bg-as"
          , g' GreenB "bg-b"
          , g' OrangeC "o-c"
          ]
  notes <- readEach \s -> readProp s notesF >>= readTimedMap isForeignPKNote
  lanes <- readEach \s -> readProp s lanesF >>= readTimedMap readBoolean
  solo <- readProp "solo" f >>= readTimedMap readBoolean
  energy <- readProp "energy" f >>= readTimedMap readBoolean
  bre <- readProp "bre" f >>= readTimedMap readBoolean
  gliss <- readProp "gliss" f >>= readTimedMap readBoolean
  ranges <- readProp "ranges" f >>= readTimedMap isForeignRange
  pure $ ProKeys
    { notes: notes
    , lanes: lanes
    , solo: solo
    , energy: energy
    , bre: bre
    , gliss: gliss
    , ranges: ranges
    }

isForeignFlex :: Foreign -> F Flex
isForeignFlex f = do
  five <- readProp "five" f >>= readNullOrUndefined >>= traverse isForeignFive
  six <- readProp "six" f >>= readNullOrUndefined >>= traverse isForeignSix
  drums <- readProp "drums" f >>= readNullOrUndefined >>= traverse isForeignDrums
  prokeys <- readProp "prokeys" f >>= readNullOrUndefined >>= traverse isForeignProKeys
  protar <- readProp "protar" f >>= readNullOrUndefined >>= traverse isForeignProtar
  vocal <- readProp "vocal" f >>= readNullOrUndefined >>= traverse isForeignVocal
  pure $ Flex
    { five: five
    , six: six
    , drums: drums
    , prokeys: prokeys
    , protar: protar
    , vocal: vocal
    }

isForeignSong :: Foreign -> F Song
isForeignSong f = do
  title <- readProp "title" f >>= readString
  artist <- readProp "artist" f >>= readString
  end <- readProp "end" f >>= readNumber
  beats <- readProp "beats" f >>= isForeignBeats
  parts <- readProp "parts" f >>= readArray >>= traverse \pair ->
    Tuple <$> (readIndex 0 pair >>= readString) <*> (readIndex 1 pair >>= isForeignFlex)
  pure $ Song
    { end: Seconds end
    , beats: beats
    , parts: parts
    , title: title
    , artist: artist
    }

readTimedSet :: Foreign -> F (Map.Map Seconds Unit)
readTimedSet f = map (map $ \(_ :: Foreign) -> unit) $ readTimedMap pure f

readTimedMap :: forall a. (Foreign -> F a) -> Foreign -> F (Map.Map Seconds a)
readTimedMap g f = Map.fromFoldable <$> (readArray f >>= traverse (readTimedPair g))

readTimedPair :: forall a. (Foreign -> F a) -> Foreign -> F (Tuple Seconds a)
readTimedPair g pair = Tuple <$> (Seconds <$> (readIndex 0 pair >>= readNumber)) <*> (readIndex 1 pair >>= g)

isForeignDrums :: Foreign -> F Drums
isForeignDrums f = do
  notes  <- readProp "notes"  f >>= readTimedMap (\frn -> readArray frn >>= traverse isForeignGem)
  lanesF <- readProp "lanes" f
  let readLane s = readProp s lanesF >>= readTimedMap readBoolean
  lanes <- map Map.fromFoldable $ sequence
    [ map (Tuple Kick) $ readLane "kick"
    , map (Tuple Red ) $ readLane "red"
    , map (Tuple YCym) $ readLane "y-cym"
    , map (Tuple YTom) $ readLane "y-tom"
    , map (Tuple BCym) $ readLane "b-cym"
    , map (Tuple BTom) $ readLane "b-tom"
    , map (Tuple GCym) $ readLane "g-cym"
    , map (Tuple GTom) $ readLane "g-tom"
    ]
  solo   <- readProp "solo"   f >>= readTimedMap readBoolean
  energy <- readProp "energy" f >>= readTimedMap readBoolean
  bre    <- readProp "bre"    f >>= readTimedMap readBoolean
  mode5  <- readProp "mode-5" f >>= readBoolean
  pure $ Drums { notes: notes, lanes: lanes, solo: solo, energy: energy, bre: bre, mode5: mode5 }

data Gem = Kick | Red | YCym | YTom | BCym | BTom | OCym | GCym | GTom

isForeignGem :: Foreign -> F Gem
isForeignGem f = readString f >>= \s -> case s of
  "kick"  -> pure Kick
  "red"   -> pure Red
  "y-cym" -> pure YCym
  "y-tom" -> pure YTom
  "b-cym" -> pure BCym
  "b-tom" -> pure BTom
  "o-cym" -> pure OCym
  "g-cym" -> pure GCym
  "g-tom" -> pure GTom
  _ -> throwError $ pure $ TypeMismatch "drum gem name" $ show s

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

isForeignBeats :: Foreign -> F Beats
isForeignBeats f = do
  lines <- readProp "lines"  f >>= readTimedMap isForeignBeat
  pure $ Beats { lines: lines }

data Beat
  = Bar
  | Beat
  | HalfBeat

isForeignBeat :: Foreign -> F Beat
isForeignBeat f = readString f >>= \s -> case s of
  "bar"      -> pure Bar
  "beat"     -> pure Beat
  "halfbeat" -> pure HalfBeat
  _          -> throwError $ pure $ TypeMismatch "bar/beat/halfbeat" $ show s

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

isForeignVocal :: Foreign -> F Vocal
isForeignVocal f = do
  harm1 <- readProp "harm1"  f >>= readTimedMap isForeignVocalNote
  harm2 <- readProp "harm2"  f >>= readTimedMap isForeignVocalNote
  harm3 <- readProp "harm3"  f >>= readTimedMap isForeignVocalNote
  energy <- readProp "energy" f >>= readTimedMap readBoolean
  ranges <- readProp "ranges" f >>= readTimedMap isForeignVocalRange
  tonic <- readProp "tonic" f >>= readNullOrUndefined >>= traverse readInt
  percussion <- readProp "percussion" f >>= readTimedSet
  phrases <- readProp "phrases" f >>= readTimedSet
  pure $ Vocal { harm1: harm1, harm2: harm2, harm3: harm3, energy: energy, ranges: ranges, tonic: tonic, percussion: percussion, phrases: phrases }

data VocalRange
  = VocalRangeShift    -- ^ Start of a range shift
  | VocalRange Int Int -- ^ The starting range, or the end of a range shift

isForeignVocalRange :: Foreign -> F VocalRange
isForeignVocalRange f = if isNull f
  then pure VocalRangeShift
  else VocalRange <$> (readIndex 0 f >>= readInt) <*> (readIndex 1 f >>= readInt)

data VocalNote
  = VocalStart String (Maybe Int)
  | VocalEnd

isForeignVocalNote :: Foreign -> F VocalNote
isForeignVocalNote f = if isNull f
  then pure VocalEnd
  else do
    lyric <- readIndex 0 f >>= readString
    pitch <- readIndex 1 f >>= readNullOrUndefined >>= traverse readInt
    pure $ VocalStart lyric pitch
