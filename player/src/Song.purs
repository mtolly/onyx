module Song where

import Prelude (class Eq, class Ord, Unit, not, otherwise, bind, map, pure, show, unit, ($), (<$>), (<*>), (>>=), (+), (-), (*), (/), (<), (<<<), div, flip)

import Data.Time.Duration (Seconds(..))
import Foreign (F, Foreign, ForeignError(..), isNull, readArray, readBoolean, readInt, readNullOrUndefined, readNumber, readString)
import Foreign.Index (readProp, readIndex)
import OnyxMap as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (sequence, traverse, for)
import Data.Tuple (Tuple(..))
import Control.Monad.Except (throwError)
import Data.String as Str
import Data.String.CodeUnits as CU
import Data.Array ((:), (..), length, index)
import Control.Monad.State (get, modify, lift, evalStateT)
import Data.Int (toNumber, fromString)
import Data.List as List
import Data.Set as Set
import Data.Foldable (class Foldable, maximum, foldrDefault, foldlDefault)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Newtype (class Newtype)
import Data.Functor (class Functor)
import Control.Apply (class Apply, lift2)
import Control.Applicative (class Applicative)
import Data.Semigroup ((<>))

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
  { five      :: Maybe (Difficulties FiveFast)
  , six       :: Maybe (Difficulties Six)
  , drums     :: Array (Difficulties Drums)
  , prokeys   :: Maybe (Difficulties ProKeysFast)
  , protar    :: Maybe (Difficulties Protar)
  , amplitude :: Maybe (Difficulties Amplitude)
  , vocal     :: Maybe (Difficulties Vocal)
  , dance     :: Maybe (Difficulties Dance)
  }

newtype Drums = Drums
  { notes  :: Map.Map Seconds (Array Gem)
  , lanes  :: Map.Map Gem (Map.Map Seconds Boolean)
  , solo   :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  , bre    :: Map.Map Seconds Boolean
  , mode   :: DrumMode
  , disco  :: Map.Map Seconds Boolean
  }

data DrumMode = Drums4 | Drums5 | DrumsPro | DrumsReal

drumsProTo4 :: Drums -> Drums
drumsProTo4 (Drums o) = Drums o
  { notes = flip mapWithIndex o.notes \t gems -> let
    disco = maybe false _.value $ Map.lookupLE t o.disco
    in flip map gems \gem -> case gem of
      Red -> if disco then YTom else Red
      YCym -> if disco then Red else YTom
      BCym -> BTom
      GCym -> GTom
      _ -> gem -- PS gems shouldn't happen
  , mode = Drums4
  }

data Sustainable a
  = SustainEnd
  | Note a
  | Sustain a

data GuitarNoteType = Strum | HOPO | Tap

newtype FiveEach a = FiveEach
  { open   :: a
  , green  :: a
  , red    :: a
  , yellow :: a
  , blue   :: a
  , orange :: a
  }
derive instance newtypeFiveEach :: Newtype (FiveEach a) _
derive instance functorFiveEach :: Functor FiveEach

instance applyFiveEach :: Apply FiveEach where
  apply (FiveEach f) (FiveEach x) = FiveEach
    { open:   f.open   x.open
    , green:  f.green  x.green
    , red:    f.red    x.red
    , yellow: f.yellow x.yellow
    , blue:   f.blue   x.blue
    , orange: f.orange x.orange
    }

instance applicativeFiveEach :: Applicative FiveEach where
  pure x = FiveEach
    { open:   x
    , green:  x
    , red:    x
    , yellow: x
    , blue:   x
    , orange: x
    }

instance foldableFiveEach :: Foldable FiveEach where
  foldMap f (FiveEach o)
    =  f o.open
    <> f o.green
    <> f o.red
    <> f o.yellow
    <> f o.blue
    <> f o.orange
  foldr f = foldrDefault f
  foldl f = foldlDefault f

newtype FiveState = FiveState
  { notes  :: FiveEach (SustainOD GuitarNoteType)
  , lanes  :: FiveEach SustainBool
  , solo   :: SustainBool
  , energy :: SustainBool
  , bre    :: SustainBool
  }

emptyFiveState :: FiveState
emptyFiveState = FiveState
  { notes: pure {past: Nothing, now: Nothing, future: Nothing}
  , lanes: pure {past: false, now: false, future: false}
  , solo: {past: false, now: false, future: false}
  , energy: {past: false, now: false, future: false}
  , bre: {past: false, now: false, future: false}
  }

extendFive :: (forall p n. TimeState p n p -> p) -> FiveState -> FiveState
extendFive fn (FiveState fs) = FiveState
  { notes: map (\s -> {past: fn s, now: Nothing, future: fn s}) fs.notes
  , lanes: map (\s -> {past: fn s, now: false, future: fn s}) fs.lanes
  , solo: {past: fn fs.solo, now: false, future: fn fs.solo}
  , energy: {past: fn fs.energy, now: false, future: fn fs.energy}
  , bre: {past: fn fs.bre, now: false, future: fn fs.bre}
  }

type FiveFast = Map.Map Seconds FiveState

processFive :: Five -> FiveFast
processFive (Five fv) = let
  allSecs = List.fromFoldable $ Set.unions
    [ Set.unions $ map Map.keys $ List.fromFoldable fv.notes
    , Set.unions $ map Map.keys $ List.fromFoldable fv.lanes
    , Map.keys fv.solo
    , Map.keys fv.energy
    , Map.keys fv.bre
    ]
  updateBool prevTS tmap t = case Map.lookup t tmap of
    Just b -> {past: prevTS.future, now: true, future: b}
    Nothing -> {past: prevTS.future, now: false, future: prevTS.future}
  go _                List.Nil           = List.Nil
  go (FiveState prev) (List.Cons t rest) = let
    energy = updateBool prev.energy fv.energy t
    v =
      { notes: let
        f m prevPitch = case Map.lookup t m of
          Nothing -> {past: prevPitch.future, now: Nothing, future: prevPitch.future}
          Just SustainEnd -> {past: prevPitch.future, now: Nothing, future: Nothing}
          Just (Note sht) -> {past: prevPitch.future, now: Just sht, future: Nothing}
          Just (Sustain sht) -> {past: prevPitch.future, now: Just sht, future: Just energy.future}
        in lift2 f fv.notes prev.notes
      , lanes: let
        f m prevPitch = updateBool prevPitch m t
        in lift2 f fv.lanes prev.lanes
      , solo: updateBool prev.solo fv.solo t
      , energy: energy
      , bre: updateBool prev.bre fv.bre t
      }
    in List.Cons (Tuple t $ FiveState v) $ go (FiveState v) rest
  in Map.fromFoldable $ go emptyFiveState allSecs

newtype Five = Five
  { notes :: FiveEach (Map.Map Seconds (Sustainable GuitarNoteType))
  , lanes :: FiveEach (Map.Map Seconds Boolean)
  , solo :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  , bre :: Map.Map Seconds Boolean
  }

newtype SixEach a = SixEach
  { open :: a
  , b1   :: a
  , b2   :: a
  , b3   :: a
  , w1   :: a
  , w2   :: a
  , w3   :: a
  , bw1  :: a
  , bw2  :: a
  , bw3  :: a
  }
derive instance newtypeSixEach :: Newtype (SixEach a) _
derive instance functorSixEach :: Functor SixEach

newtype SixState = SixState
  { notes  :: SixEach (SustainOD GuitarNoteType)
  , solo   :: SustainBool
  , energy :: SustainBool
  , bre    :: SustainBool
  }

newtype Six = Six
  { notes :: SixEach (Map.Map Seconds (Sustainable GuitarNoteType))
  , solo :: Map.Map Seconds Boolean
  , energy :: Map.Map Seconds Boolean
  , bre :: Map.Map Seconds Boolean
  }

newtype ProtarNote = ProtarNote
  { noteType :: GuitarNoteType
  , fret     :: Maybe Int
  , phantom  :: Boolean
  , slide    :: Maybe Slide
  }

data Slide = SlideUp | SlideDown

type ProtarEach a =
  { s1 :: a
  , s2 :: a
  , s3 :: a
  , s4 :: a
  , s5 :: a
  , s6 :: a
  , s7 :: a
  , s8 :: a
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

type TimeState p n f =
  { past   :: p
  , now    :: n
  , future :: f
  }
type SustainOD a = TimeState (Maybe Boolean) (Maybe a) (Maybe Boolean)
-- above bools are whether sustain was under OD
type SustainBool = TimeState Boolean Boolean Boolean

newtype ProKeysState = ProKeysState
  { notes  :: Map.Map Pitch (SustainOD Unit)
  , lanes  :: Map.Map Pitch SustainBool
  , ranges :: TimeState (Maybe Range) Unit (Maybe Range)
  , solo   :: SustainBool
  , energy :: SustainBool
  , gliss  :: SustainBool
  , bre    :: SustainBool
  }

emptyProKeysState :: ProKeysState
emptyProKeysState = ProKeysState
  { notes: Map.fromFoldable $ map (\p -> Tuple p {past: Nothing, now: Nothing, future: Nothing}) allPitches
  , lanes: Map.fromFoldable $ map (\p -> Tuple p {past: false, now: false, future: false}) allPitches
  , ranges: {past: Nothing, now: unit, future: Nothing}
  , solo: {past: false, now: false, future: false}
  , energy: {past: false, now: false, future: false}
  , gliss: {past: false, now: false, future: false}
  , bre: {past: false, now: false, future: false}
  }

extendPK :: (forall p n. TimeState p n p -> p) -> ProKeysState -> ProKeysState
extendPK fn (ProKeysState pk) = ProKeysState
  { notes: map (\s -> {past: fn s, now: Nothing, future: fn s}) pk.notes
  , lanes: map (\s -> {past: fn s, now: false, future: fn s}) pk.lanes
  , ranges: {past: fn pk.ranges, now: unit, future: fn pk.ranges}
  , solo: {past: fn pk.solo, now: false, future: fn pk.solo}
  , energy: {past: fn pk.energy, now: false, future: fn pk.energy}
  , gliss: {past: fn pk.gliss, now: false, future: fn pk.gliss}
  , bre: {past: fn pk.bre, now: false, future: fn pk.bre}
  }

type ProKeysFast = Map.Map Seconds ProKeysState

processProKeys :: ProKeys -> ProKeysFast
processProKeys (ProKeys pk) = let
  allSecs = List.fromFoldable $ Set.unions
    [ Set.unions $ map Map.keys $ Map.values pk.notes
    , Set.unions $ map Map.keys $ Map.values pk.lanes
    , Map.keys pk.ranges
    , Map.keys pk.solo
    , Map.keys pk.energy
    , Map.keys pk.gliss
    , Map.keys pk.bre
    ]
  updateBool prevTS tmap t = case Map.lookup t tmap of
    Just b -> {past: prevTS.future, now: true, future: b}
    Nothing -> {past: prevTS.future, now: false, future: prevTS.future}
  go _                   List.Nil           = List.Nil
  go (ProKeysState prev) (List.Cons t rest) = let
    energy = updateBool prev.energy pk.energy t
    v =
      { notes: flip mapWithIndex prev.notes \pitch prevPitch ->
        case Map.lookup pitch pk.notes >>= Map.lookup t of
          Nothing -> {past: prevPitch.future, now: Nothing, future: prevPitch.future}
          Just SustainEnd -> {past: prevPitch.future, now: Nothing, future: Nothing}
          Just (Note _) -> {past: prevPitch.future, now: Just unit, future: Nothing}
          Just (Sustain _) -> {past: prevPitch.future, now: Just unit, future: Just energy.future}
      , lanes: flip mapWithIndex prev.lanes \pitch prevPitch ->
        updateBool prevPitch (fromMaybe Map.empty $ Map.lookup pitch pk.lanes) t
      , ranges:
        { past: prev.ranges.future
        , now: unit
        , future: maybe prev.ranges.future Just $ Map.lookup t pk.ranges
        }
      , solo: updateBool prev.solo pk.solo t
      , energy: energy
      , gliss: updateBool prev.gliss pk.gliss t
      , bre: updateBool prev.bre pk.bre t
      }
    in List.Cons (Tuple t $ ProKeysState v) $ go (ProKeysState v) rest
  in Map.fromFoldable $ go emptyProKeysState allSecs

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

newtype ProKeysEach a = ProKeysEach
  { redC     :: a
  , redCs    :: a
  , redD     :: a
  , redDs    :: a
  , redE     :: a
  , yellowF  :: a
  , yellowFs :: a
  , yellowG  :: a
  , yellowGs :: a
  , yellowA  :: a
  , yellowAs :: a
  , yellowB  :: a
  , blueC    :: a
  , blueCs   :: a
  , blueD    :: a
  , blueDs   :: a
  , blueE    :: a
  , greenF   :: a
  , greenFs  :: a
  , greenG   :: a
  , greenGs  :: a
  , greenA   :: a
  , greenAs  :: a
  , greenB   :: a
  , orangeC  :: a
  }
derive instance newtypeProKeysEach :: Newtype (ProKeysEach a) _
derive instance functorProKeysEach :: Functor ProKeysEach

allPitches :: Array Pitch
allPitches =
  [ RedC, RedCs, RedD, RedDs, RedE, YellowF, YellowFs, YellowG, YellowGs
  , YellowA, YellowAs, YellowB, BlueC, BlueCs, BlueD, BlueDs, BlueE, GreenF
  , GreenFs, GreenG, GreenGs, GreenA, GreenAs, GreenB, OrangeC
  ]

newtype Amplitude = Amplitude
  { notes      :: Map.Map Seconds AmpNote
  , instrument :: String
  }

data AmpNote = L | M | R

data DanceType
  = NoteNormal
  | NoteMine
  | NoteLift
  | NoteRoll

newtype Dance = Dance
  { notes ::
    { left  :: Map.Map Seconds (Sustainable DanceType)
    , down  :: Map.Map Seconds (Sustainable DanceType)
    , up    :: Map.Map Seconds (Sustainable DanceType)
    , right :: Map.Map Seconds (Sustainable DanceType)
    }
  , energy :: Map.Map Seconds Boolean
  }

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
  let readFret1 o = readFret2 case Str.take 1 o.str of
        "u" -> { slide: Just SlideUp  , str: Str.drop 1 o.str }
        "d" -> { slide: Just SlideDown, str: Str.drop 1 o.str }
        _   -> { slide: Nothing       , str: o.str            }
      readFret2 o = case o.str of
        "x" -> \g -> pure $ g { fret: Nothing, phantom: false, slide: o.slide }
        _   -> readFret3 o
      readFret3 o = readFret4 case Str.take 1 o.str of
        "p" -> { phantom: true , slide: o.slide, str: Str.drop 1 o.str }
        _   -> { phantom: false, slide: o.slide, str: o.str            }
      readFret4 o = case fromString o.str of
        Nothing -> \_ -> throwError $ pure $ TypeMismatch "protar note event" $ show s
        Just ft -> \g -> pure $ g { fret: Just ft, phantom: o.phantom, slide: o.slide }
  case Str.take 1 s of
    "e" -> pure SustainEnd
    "s" -> readFret1 { str: Str.drop 1 s } \o -> Note    $ ProtarNote { noteType: Strum, fret: o.fret, phantom: o.phantom, slide: o.slide }
    "h" -> readFret1 { str: Str.drop 1 s } \o -> Note    $ ProtarNote { noteType: HOPO , fret: o.fret, phantom: o.phantom, slide: o.slide }
    "t" -> readFret1 { str: Str.drop 1 s } \o -> Note    $ ProtarNote { noteType: Tap  , fret: o.fret, phantom: o.phantom, slide: o.slide }
    "S" -> readFret1 { str: Str.drop 1 s } \o -> Sustain $ ProtarNote { noteType: Strum, fret: o.fret, phantom: o.phantom, slide: o.slide }
    "H" -> readFret1 { str: Str.drop 1 s } \o -> Sustain $ ProtarNote { noteType: HOPO , fret: o.fret, phantom: o.phantom, slide: o.slide }
    "T" -> readFret1 { str: Str.drop 1 s } \o -> Sustain $ ProtarNote { noteType: Tap  , fret: o.fret, phantom: o.phantom, slide: o.slide }
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
        pure $ FiveEach
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

isForeignDance :: Foreign -> F Dance
isForeignDance f = do
  notesF <- readProp "notes" f
  let readLane s = readProp s notesF >>= readTimedMap isForeignDanceType
      isForeignDanceType x = readString x >>= \s -> case s of
        "e" -> pure SustainEnd
        "n" -> pure $ Note    NoteNormal
        "m" -> pure $ Note    NoteMine
        "l" -> pure $ Note    NoteLift
        "r" -> pure $ Note    NoteRoll
        "N" -> pure $ Sustain NoteNormal
        "M" -> pure $ Sustain NoteMine
        "L" -> pure $ Sustain NoteLift
        "R" -> pure $ Sustain NoteRoll
        _   -> throwError $ pure $ TypeMismatch "dance note event" $ show s
  arrowL <- readLane "L"
  arrowD <- readLane "D"
  arrowU <- readLane "U"
  arrowR <- readLane "R"
  energy <- readProp "energy" f >>= readTimedMap readBoolean
  pure $ Dance
    { notes:
      { left:  arrowL
      , down:  arrowD
      , up:    arrowU
      , right: arrowR
      }
    , energy: energy
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
    { notes: SixEach
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
        s7 <- g "s7"
        s8 <- g "s8"
        pure
          { s1: s1
          , s2: s2
          , s3: s3
          , s4: s4
          , s5: s5
          , s6: s6
          , s7: s7
          , s8: s8
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
  drums' <- readProp "drums" f >>= readNullOrUndefined
  drums <- case drums' of
    Nothing -> pure []
    Just ds -> readArray ds >>= traverse (difficulties isForeignDrums)
  prokeys <- readProp "prokeys" f >>= readNullOrUndefined >>= traverse (difficulties isForeignProKeys)
  protar <- readProp "protar" f >>= readNullOrUndefined >>= traverse (difficulties isForeignProtar)
  amplitude <- readProp "catch" f >>= readNullOrUndefined >>= traverse (difficulties isForeignAmplitude)
  vocal <- readProp "vocal" f >>= readNullOrUndefined >>= traverse (difficulties isForeignVocal)
  dance <- readProp "dance" f >>= readNullOrUndefined >>= traverse (difficulties isForeignDance)
  pure $ Flex
    { five: map (map (map processFive)) five
    , six: six
    , drums: drums
    , prokeys: map (map (map processProKeys)) prokeys
    , protar: protar
    , amplitude: amplitude
    , vocal: vocal
    , dance: dance
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
  mode   <- readProp "mode" f >>= readString >>= \s -> case s of
    "4"    -> pure Drums4
    "5"    -> pure Drums5
    "pro"  -> pure DrumsPro
    "real" -> pure DrumsReal
    _      -> throwError $ pure $ TypeMismatch "drums mode" $ show s
  disco  <- readProp "disco" f >>= readTimedMap readBoolean
  pure $ Drums { notes: notes, lanes: lanes, solo: solo, energy: energy, bre: bre, mode: mode, disco: disco }

data Gem
  = Kick
  | Red | Rimshot
  | HHOpen | HHSizzle | HHPedal | YCym | YTom
  | BCym | BTom
  | OCym
  | GCym | GTom

allDrums :: Array (Tuple Char Gem)
allDrums =
  [ Tuple 'k' Kick
  , Tuple 'r' Red
  , Tuple 'R' Rimshot
  , Tuple 'H' HHOpen
  , Tuple 'h' HHSizzle
  , Tuple 'p' HHPedal
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

newtype Vocal = Vocal
  { harm1 :: Map.Map Seconds VocalNote
  , harm2 :: Map.Map Seconds VocalNote
  , harm3 :: Map.Map Seconds VocalNote
  , percussion :: Map.Map Seconds Unit
  , phrases1 :: Map.Map Seconds Unit
  , phrases2 :: Map.Map Seconds Unit
  , ranges :: Map.Map Seconds VocalRange
  , energy :: Map.Map Seconds Boolean
  , tonic :: Maybe Int
  , lyrics1 :: Map.Map Seconds LyricPhrase
  , lyrics2 :: Map.Map Seconds LyricPhrase
  , lyrics3 :: Map.Map Seconds LyricPhrase
  }

type LyricPhrase = Map.Map Seconds (Maybe String)

vocalCount :: Difficulties Vocal -> Int
vocalCount diffs = let
  eachDiff (Tuple _ (Vocal v))
    | not $ Map.isEmpty v.harm3 = 3
    | not $ Map.isEmpty v.harm2 = 2
    | otherwise                 = 1
  in fromMaybe 1 $ maximum $ map eachDiff diffs

extractLyrics
  :: Map.Map Seconds Unit
  -> Map.Map Seconds VocalNote
  -> Map.Map Seconds LyricPhrase
extractLyrics phrases notes = let
  lyrics = Map.fromFoldable $ removeSlides $ Map.toUnfoldable $ map stripPitches notes
  stripPitches VocalEnd         = Nothing
  stripPitches (VocalStart s _) = Just s
  removeSlides xs = case xs of
    List.Cons
      (Tuple _ Nothing)
      (List.Cons (Tuple _ (Just "+")) rest)
      -> removeSlides rest
    List.Cons
      (Tuple _ Nothing)
      (List.Cons (Tuple _ (Just "+$")) rest)
      -> removeSlides rest
    List.Cons tx rest -> List.Cons tx $ removeSlides rest
    List.Nil -> List.Nil
  phraseList = Set.toUnfoldable $ Map.keys phrases
  phrasePairs = List.zip (List.Cons Nothing $ map Just phraseList) phraseList
  getPhrase start end = let
    sub = Map.submap start (Just end) lyrics
    -- below line fixes a bug in RB3. maybe we should have an option to replicate it...
    removeLyricAtEnd (Just (Just _)) = Nothing
    removeLyricAtEnd x               = x
    in Map.alter removeLyricAtEnd end sub
  in Map.fromFoldable $ map (\(Tuple start end) -> Tuple end $ getPhrase start end) phrasePairs

isForeignVocal :: Foreign -> F Vocal
isForeignVocal f = do
  harm1 <- readProp "harm1"  f >>= readTimedMap isForeignVocalNote
  harm2 <- readProp "harm2"  f >>= readTimedMap isForeignVocalNote
  harm3 <- readProp "harm3"  f >>= readTimedMap isForeignVocalNote
  energy <- readProp "energy" f >>= readTimedMap readBoolean
  ranges <- readProp "ranges" f >>= readTimedMap isForeignVocalRange
  tonic <- readProp "tonic" f >>= readNullOrUndefined >>= traverse readInt
  percussion <- readProp "percussion" f >>= readTimedSet
  phrases1 <- readProp "phrases1" f >>= readTimedSet
  phrases2 <- readProp "phrases2" f >>= readTimedSet
  pure $ Vocal
    { harm1, harm2, harm3, energy, ranges, tonic, percussion, phrases1, phrases2
    , lyrics1: extractLyrics phrases1 harm1
    , lyrics2: extractLyrics phrases2 harm2
    , lyrics3: extractLyrics phrases2 harm3
    }

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
