module Song where

import Prelude (class Eq, class Ord, class Show, Unit, bind, map, pure, show, unit, ($), (<$>), (<*>), (>>=), (+), (-), (*), (/), (<), (<<<), div)

import Data.Time.Duration (Seconds(..))
import Foreign (F, Foreign, ForeignError(..), isNull, readArray, readBoolean, readInt, readNullOrUndefined, readNumber, readString)
import Foreign.Index (readProp, readIndex)
import OnyxMap as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse, for)
import Data.Tuple (Tuple(..))
import Control.Monad.Except (throwError)
import Data.String as Str
import Data.String.CodeUnits as CU
import Data.Array ((:), (..), length, index)
import Control.Monad.State (get, modify, lift, evalStateT)
import Data.Int (toNumber, fromString)

newtype Song = Song
  { end      :: Seconds
  , beats    :: Beats
  , parts    :: Array (Tuple String Flex)
  , title    :: String
  , artist   :: String
  , author   :: String
  , sections :: Map.Map Seconds String
  }

type Difficulties a = Array (Tuple String a)

newtype Flex = Flex
  { five      :: Maybe (Difficulties Five)
  , six       :: Maybe (Difficulties Six)
  , drums     :: Maybe (Difficulties Drums)
  , prokeys   :: Maybe (Difficulties ProKeys)
  , protar    :: Maybe (Difficulties Protar)
  , amplitude :: Maybe (Difficulties Amplitude)
  , vocal     :: Maybe (Difficulties Vocal)
  }

data FlexPart
  = FlexFive
  | FlexSix
  | FlexDrums
  | FlexProKeys
  | FlexProtar
  | FlexVocal

derive instance eqFlexPart :: Eq FlexPart
derive instance ordFlexPart :: Ord FlexPart

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
  , phantom  :: Boolean
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
  , arpeggio :: Map.Map Seconds Boolean
  , strings :: Int
  , chordsWidth :: Int
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

derive instance eqPitch :: Eq Pitch
derive instance ordPitch :: Ord Pitch

newtype Amplitude = Amplitude
  { notes      :: Map.Map Seconds AmpNote
  , instrument :: String
  }

data AmpNote = L | M | R

isForeignFiveNote :: Foreign -> F (Sustainable GuitarNoteType)
isForeignFiveNote f = readString f >>= \s -> case s of
  "e" -> pure SustainEnd
  "s" -> pure $ Note Strum
  "h" -> pure $ Note HOPO
  "t" -> pure $ Note Tap
  "S" -> pure $ Sustain Strum
  "H" -> pure $ Sustain HOPO
  "T" -> pure $ Sustain Tap
  _   -> throwError $ pure $ TypeMismatch "grybo/ghl note event" $ show s

isForeignProtarNote :: Foreign -> F (Sustainable ProtarNote)
isForeignProtarNote f = do
  s <- readString f
  let readFret "x" g = pure $ g { fret: Nothing, phantom: false }
      readFret n   g = let
        o = case Str.take 1 n of
          "p" -> { phantom: true , rest: Str.drop 1 n }
          _   -> { phantom: false, rest: n            }
        in case fromString o.rest of
          Nothing -> throwError $ pure $ TypeMismatch "protar note event" $ show s
          Just ft -> pure $ g { fret: Just ft, phantom: o.phantom }
  case Str.take 1 s of
    "e" -> pure SustainEnd
    "s" -> readFret (Str.drop 1 s) \o -> Note    $ ProtarNote { noteType: Strum, fret: o.fret, phantom: o.phantom }
    "h" -> readFret (Str.drop 1 s) \o -> Note    $ ProtarNote { noteType: HOPO , fret: o.fret, phantom: o.phantom }
    "t" -> readFret (Str.drop 1 s) \o -> Note    $ ProtarNote { noteType: Tap  , fret: o.fret, phantom: o.phantom }
    "S" -> readFret (Str.drop 1 s) \o -> Sustain $ ProtarNote { noteType: Strum, fret: o.fret, phantom: o.phantom }
    "H" -> readFret (Str.drop 1 s) \o -> Sustain $ ProtarNote { noteType: HOPO , fret: o.fret, phantom: o.phantom }
    "T" -> readFret (Str.drop 1 s) \o -> Sustain $ ProtarNote { noteType: Tap  , fret: o.fret, phantom: o.phantom }
    _   -> throwError $ pure $ TypeMismatch "protar note event" $ show s

isForeignPKNote :: Foreign -> F (Sustainable Unit)
isForeignPKNote f = readString f >>= \s -> case s of
  "e" -> pure SustainEnd
  "n" -> pure $ Note unit
  "N" -> pure $ Sustain unit
  _   -> throwError $ pure $ TypeMismatch "pro keys note event" $ show s

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

isForeignAmplitude :: Foreign -> F Amplitude
isForeignAmplitude f = do
  notesF <- readProp "notes" f
  let readGem g = readInt g >>= \i -> case i of
        1 -> pure L
        2 -> pure M
        3 -> pure R
        _ -> throwError $ pure $ TypeMismatch "Amplitude note" $ show i
  notes <- readTimedMap readGem notesF
  instrument <- readProp "instrument" f >>= readString
  pure $ Amplitude
    { notes: notes
    , instrument: instrument
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
  "C" -> pure $ Sustain $ splitChord Baseline $ specialChars $ Str.drop 2 s
  "c" -> pure $ Note    $ splitChord Baseline $ specialChars $ Str.drop 2 s
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
  arpeggio <- readProp "arpeggio" f >>= readTimedMap readBoolean
  strings <- readProp "strings" f >>= readInt
  pure $ Protar
    { notes: notes
    , lanes: lanes
    , solo: solo
    , energy: energy
    , bre: bre
    , chords: chords
    , arpeggio: arpeggio
    , strings: strings
    , chordsWidth: 0 -- to be computed later
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
  five <- readProp "five" f >>= readNullOrUndefined >>= traverse (difficulties isForeignFive)
  six <- readProp "six" f >>= readNullOrUndefined >>= traverse (difficulties isForeignSix)
  drums <- readProp "drums" f >>= readNullOrUndefined >>= traverse (difficulties isForeignDrums)
  prokeys <- readProp "prokeys" f >>= readNullOrUndefined >>= traverse (difficulties isForeignProKeys)
  protar <- readProp "protar" f >>= readNullOrUndefined >>= traverse (difficulties isForeignProtar)
  amplitude <- readProp "catch" f >>= readNullOrUndefined >>= traverse (difficulties isForeignAmplitude)
  vocal <- readProp "vocal" f >>= readNullOrUndefined >>= traverse (difficulties isForeignVocal)
  pure $ Flex
    { five: five
    , six: six
    , drums: drums
    , prokeys: prokeys
    , protar: protar
    , amplitude: amplitude
    , vocal: vocal
    }

isForeignSong :: Foreign -> F Song
isForeignSong f = do
  title <- readProp "title" f >>= readString
  artist <- readProp "artist" f >>= readString
  author <- readProp "author" f >>= readString
  end <- readProp "end" f >>= readNumber
  beats <- readProp "beats" f >>= isForeignBeats
  parts <- readProp "parts" f >>= readArray >>= traverse \pair ->
    Tuple <$> (readIndex 0 pair >>= readString) <*> (readIndex 1 pair >>= isForeignFlex)
  sections <- readProp "sections" f >>= readTimedMap readString
  pure $ Song
    { end: Seconds end
    , beats: beats
    , parts: parts
    , title: title
    , artist: artist
    , author: author
    , sections: sections
    }

readTimedSet :: Foreign -> F (Map.Map Seconds Unit)
readTimedSet f = map (map $ \(_ :: Foreign) -> unit) $ readTimedMap pure f

readTimedMap :: forall a. (Foreign -> F a) -> Foreign -> F (Map.Map Seconds a)
readTimedMap g f = Map.fromFoldable <$> (readArray f >>= readPairs) where
  readPairs ary = evalStateT (traverse (readPair ary) (everyOtherIndex ary)) 0
  everyOtherIndex ary = if length ary < 2 then [] else 0 .. (div (length ary) 2 - 1)
  readPair ary i = do
    prevMS <- get
    relMS <- lift $ index' ary (i * 2) >>= readInt
    x <- lift $ index' ary (i * 2 + 1) >>= g
    thisMS <- modify (_ + relMS)
    pure $ Tuple (Seconds $ toNumber thisMS / 1000.0) x
  index' ary i = maybe (throwError $ pure $ ForeignError "index error, shouldn't happen") pure $ index ary i

difficulties :: forall a. (Foreign -> F a) -> Foreign -> F (Difficulties a)
difficulties g f = readArray f >>= traverse eachPair where
  eachPair pair = Tuple <$> (readIndex 0 pair >>= readString) <*> (readIndex 1 pair >>= g)

isForeignDrums :: Foreign -> F Drums
isForeignDrums f = do
  notes  <- readProp "notes"  f >>= readTimedMap isForeignGems
  lanesF <- readProp "lanes" f
  let readLane s = readProp s lanesF >>= readTimedMap readBoolean
  lanes <- map Map.fromFoldable $ traverse
    (\(Tuple ch gem) -> map (Tuple gem) $ readLane $ CU.singleton ch)
    allDrums
  solo   <- readProp "solo"   f >>= readTimedMap readBoolean
  energy <- readProp "energy" f >>= readTimedMap readBoolean
  bre    <- readProp "bre"    f >>= readTimedMap readBoolean
  mode5  <- readProp "mode-5" f >>= readBoolean
  pure $ Drums { notes: notes, lanes: lanes, solo: solo, energy: energy, bre: bre, mode5: mode5 }

data Gem = Kick | Red | YCym | YTom | BCym | BTom | OCym | GCym | GTom

allDrums :: Array (Tuple Char Gem)
allDrums =
  [ Tuple 'k' Kick
  , Tuple 'r' Red
  , Tuple 'Y' YCym
  , Tuple 'y' YTom
  , Tuple 'B' BCym
  , Tuple 'b' BTom
  , Tuple 'O' OCym
  , Tuple 'G' GCym
  , Tuple 'g' GTom
  ]

allDrumsMap :: Map.Map Char Gem
allDrumsMap = Map.fromFoldable allDrums

isForeignGems :: Foreign -> F (Array Gem)
isForeignGems f = do
  str <- readString f
  for (CU.toCharArray str) \ch -> case Map.lookup ch allDrumsMap of
    Just v  -> pure v
    Nothing -> throwError $ pure $ TypeMismatch "drum gem" $ show ch

derive instance eqGem :: Eq Gem
derive instance ordGem :: Ord Gem

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
isForeignBeat f = readNumber f >>= \n -> case n of
  0.0 -> pure Bar
  1.0 -> pure Beat
  2.0 -> pure HalfBeat
  _   -> throwError $ pure $ TypeMismatch "bar (0) / beat (1) / halfbeat (2)" $ show n

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
