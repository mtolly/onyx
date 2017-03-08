{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.File where

import           Control.Monad                    (forM, forM_, unless)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (partition, sortOn)
import           Data.Maybe                       (catMaybes, fromJust)
import qualified Numeric.NonNegative.Class        as NNC
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U

import           Control.Monad.Trans.StackTrace
import qualified MelodysEscape                    as Melody
import qualified RockBand.Beat                    as Beat
import           RockBand.Common
import qualified RockBand.Drums                   as Drums
import qualified RockBand.Events                  as Events
import qualified RockBand.FiveButton              as FiveButton
import           RockBand.Parse
import qualified RockBand.PhaseShiftKeys          as PSKeys
import           RockBand.PhaseShiftMessage       (PSWrap (..))
import qualified RockBand.ProGuitar               as ProGuitar
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Venue                   as Venue
import qualified RockBand.Vocals                  as Vocals

data Song t = Song
  { s_tempos     :: U.TempoMap
  , s_signatures :: U.MeasureMap
  , s_tracks     :: t
  } deriving (Eq, Show, Functor, Foldable, Traversable)

class MIDIFileFormat f where
  readMIDITracks :: (Monad m) => Song [RTB.T U.Beats E.T] -> StackTraceT m (Song (f U.Beats))
  showMIDITracks :: Song (f U.Beats) -> Song [RTB.T U.Beats E.T]

parseTracks :: (MIDIEvent a, Ord a, Monad m) =>
  U.MeasureMap -> [RTB.T U.Beats E.T] -> [String] -> StackTraceT m (RTB.T U.Beats a)
parseTracks mmap trks names = do
  let merged = foldr RTB.merge RTB.empty $ flip filter trks $ \trk -> case U.trackName trk of
        Nothing   -> False
        Just name -> elem name names
  inside ("MIDI tracks named " ++ show names) $ makeTrackParser parseOne mmap merged

knownTracks :: (Monad m) => [RTB.T U.Beats E.T] -> [String] -> StackTraceT m ()
knownTracks trks names = forM_ (zip ([1..] :: [Int]) trks) $ \(i, trk) -> do
  inside ("MIDI track #" ++ show i ++ " (0 is tempo track)") $ case U.trackName trk of
    Nothing   -> warn "track with no name"
    Just name -> unless (elem name names) $ warn $ "unrecognized track name " ++ show name

showMIDITrack :: (MIDIEvent a) => String -> RTB.T U.Beats a -> [RTB.T U.Beats E.T]
showMIDITrack name trk =
  [fixEventOrder $ U.setTrackName name $ unparseAll trk | not $ RTB.null trk]

data RB3File t = RB3File
  { rb3PartDrums        :: RTB.T t      Drums.Event
  , rb3PartGuitar       :: RTB.T t FiveButton.Event
  , rb3PartBass         :: RTB.T t FiveButton.Event
  , rb3PartKeys         :: RTB.T t FiveButton.Event
  , rb3PartRealGuitar   :: RTB.T t  ProGuitar.Event
  , rb3PartRealGuitar22 :: RTB.T t  ProGuitar.Event
  , rb3PartRealBass     :: RTB.T t  ProGuitar.Event
  , rb3PartRealBass22   :: RTB.T t  ProGuitar.Event
  , rb3PartRealKeysE    :: RTB.T t    ProKeys.Event
  , rb3PartRealKeysM    :: RTB.T t    ProKeys.Event
  , rb3PartRealKeysH    :: RTB.T t    ProKeys.Event
  , rb3PartRealKeysX    :: RTB.T t    ProKeys.Event
  , rb3PartKeysAnimLH   :: RTB.T t    ProKeys.Event
  , rb3PartKeysAnimRH   :: RTB.T t    ProKeys.Event
  , rb3PartVocals       :: RTB.T t     Vocals.Event
  , rb3Harm1            :: RTB.T t     Vocals.Event
  , rb3Harm2            :: RTB.T t     Vocals.Event
  , rb3Harm3            :: RTB.T t     Vocals.Event
  , rb3Events           :: RTB.T t     Events.Event
  , rb3Beat             :: RTB.T t       Beat.Event
  , rb3Venue            :: RTB.T t      Venue.Event
  } deriving (Eq, Ord, Show)

instance MIDIFileFormat RB3File where
  readMIDITracks = undefined
  showMIDITracks = undefined

data RB2File t = RB2File
  { rb2PartDrums  :: RTB.T t      Drums.Event
  , rb2PartGuitar :: RTB.T t FiveButton.Event
  , rb2PartBass   :: RTB.T t FiveButton.Event
  , rb2PartVocals :: RTB.T t     Vocals.Event
  , rb2Events     :: RTB.T t     Events.Event
  , rb2Beat       :: RTB.T t       Beat.Event
  , rb2Venue      :: RTB.T t              E.T -- TODO
  } deriving (Eq, Ord, Show)

instance MIDIFileFormat RB2File where
  readMIDITracks (Song tempos mmap trks) = do
    rb2PartDrums  <- parseTracks mmap trks ["PART DRUMS"]
    rb2PartGuitar <- parseTracks mmap trks ["PART GUITAR"]
    rb2PartBass   <- parseTracks mmap trks ["PART BASS"]
    rb2PartVocals <- parseTracks mmap trks ["PART VOCALS"]
    rb2Events     <- parseTracks mmap trks ["EVENTS"]
    rb2Beat       <- parseTracks mmap trks ["BEAT"]
    rb2Venue      <- parseTracks mmap trks ["VENUE"]
    knownTracks trks ["PART DRUMS", "PART GUITAR", "PART BASS", "PART VOCALS", "EVENTS", "BEAT", "VENUE"]
    return $ Song tempos mmap $ RB2File{..}
  showMIDITracks (Song tempos mmap RB2File{..}) = Song tempos mmap $ concat
    [ showMIDITrack "PART DRUMS"  rb2PartDrums
    , showMIDITrack "PART GUITAR" rb2PartGuitar
    , showMIDITrack "PART BASS"   rb2PartBass
    , showMIDITrack "PART VOCALS" rb2PartVocals
    , showMIDITrack "EVENTS"      rb2Events
    , showMIDITrack "BEAT"        rb2Beat
    , showMIDITrack "VENUE"       rb2Venue
    ]

data PSFile t = PSFile
  { psPartDrums        :: RTB.T t (PSWrap      Drums.Event)
  , psPartGuitar       :: RTB.T t (PSWrap FiveButton.Event)
  , psPartBass         :: RTB.T t (PSWrap FiveButton.Event)
  , psPartKeys         :: RTB.T t (PSWrap FiveButton.Event)
  , psPartRealGuitar   :: RTB.T t (PSWrap  ProGuitar.Event)
  , psPartRealGuitar22 :: RTB.T t (PSWrap  ProGuitar.Event)
  , psPartRealBass     :: RTB.T t (PSWrap  ProGuitar.Event)
  , psPartRealBass22   :: RTB.T t (PSWrap  ProGuitar.Event)
  , psPartRealKeysE    :: RTB.T t (PSWrap    ProKeys.Event)
  , psPartRealKeysM    :: RTB.T t (PSWrap    ProKeys.Event)
  , psPartRealKeysH    :: RTB.T t (PSWrap    ProKeys.Event)
  , psPartRealKeysX    :: RTB.T t (PSWrap    ProKeys.Event)
  , psPartRealKeysPsE  :: RTB.T t (PSWrap     PSKeys.Event)
  , psPartRealKeysPsM  :: RTB.T t (PSWrap     PSKeys.Event)
  , psPartRealKeysPsH  :: RTB.T t (PSWrap     PSKeys.Event)
  , psPartRealKeysPsX  :: RTB.T t (PSWrap     PSKeys.Event)
  , psPartKeysAnimLH   :: RTB.T t (PSWrap    ProKeys.Event)
  , psPartKeysAnimRH   :: RTB.T t (PSWrap    ProKeys.Event)
  , psPartVocals       :: RTB.T t (PSWrap     Vocals.Event)
  , psHarm1            :: RTB.T t (PSWrap     Vocals.Event)
  , psHarm2            :: RTB.T t (PSWrap     Vocals.Event)
  , psHarm3            :: RTB.T t (PSWrap     Vocals.Event)
  , psEvents           :: RTB.T t (PSWrap     Events.Event)
  , psBeat             :: RTB.T t (PSWrap       Beat.Event)
  , psVenue            :: RTB.T t (PSWrap      Venue.Event)
  } deriving (Eq, Ord, Show)

instance MIDIFileFormat PSFile where
  -- TODO: remember to allow "PART DRUM"
  readMIDITracks = undefined
  showMIDITracks = undefined

data OnyxFile t = OnyxFile
  { onyxPartDrums        :: RTB.T t (PSWrap      Drums.Event)
  , onyxPartDrums2x      :: RTB.T t (PSWrap      Drums.Event)
  , onyxPartGuitar       :: RTB.T t (PSWrap FiveButton.Event)
  , onyxPartBass         :: RTB.T t (PSWrap FiveButton.Event)
  , onyxPartKeys         :: RTB.T t (PSWrap FiveButton.Event)
  , onyxPartRealGuitar   :: RTB.T t (PSWrap  ProGuitar.Event)
  , onyxPartRealGuitar22 :: RTB.T t (PSWrap  ProGuitar.Event)
  , onyxPartRealBass     :: RTB.T t (PSWrap  ProGuitar.Event)
  , onyxPartRealBass22   :: RTB.T t (PSWrap  ProGuitar.Event)
  , onyxPartRealKeysE    :: RTB.T t (PSWrap    ProKeys.Event)
  , onyxPartRealKeysM    :: RTB.T t (PSWrap    ProKeys.Event)
  , onyxPartRealKeysH    :: RTB.T t (PSWrap    ProKeys.Event)
  , onyxPartRealKeysX    :: RTB.T t (PSWrap    ProKeys.Event)
  , onyxPartKeysAnimLH   :: RTB.T t (PSWrap    ProKeys.Event)
  , onyxPartKeysAnimRH   :: RTB.T t (PSWrap    ProKeys.Event)
  , onyxPartVocals       :: RTB.T t (PSWrap     Vocals.Event)
  , onyxHarm1            :: RTB.T t (PSWrap     Vocals.Event)
  , onyxHarm2            :: RTB.T t (PSWrap     Vocals.Event)
  , onyxHarm3            :: RTB.T t (PSWrap     Vocals.Event)
  , onyxEvents           :: RTB.T t (PSWrap     Events.Event)
  , onyxBeat             :: RTB.T t (PSWrap       Beat.Event)
  , onyxVenue            :: RTB.T t (PSWrap      Venue.Event)
  , onyxMelodysEscape    :: RTB.T t (PSWrap     Melody.Event)
  } deriving (Eq, Ord, Show)

instance MIDIFileFormat OnyxFile where
  readMIDITracks = undefined
  showMIDITracks = undefined

newtype RawFile t = RawFile { rawTracks :: [RTB.T t E.T] }
  deriving (Eq, Ord, Show)

instance MIDIFileFormat RawFile where
  readMIDITracks = return . fmap RawFile
  showMIDITracks = fmap rawTracks

-- | Temporary format until we get rid of 'Track'
newtype ParsedFile t = ParsedFile { parsedTracks :: [Track t] }
  deriving (Eq, Ord, Show)

instance MIDIFileFormat ParsedFile where
  readMIDITracks (Song tempos mmap trks) =
    Song tempos mmap . ParsedFile <$> mapM (parseTrack mmap) trks
  showMIDITracks (Song tempos mmap (ParsedFile trks)) =
    Song tempos mmap $ map showTrack trks

data Track t
  = PartDrums                 (RTB.T t      Drums.Event)
  | PartDrums2x               (RTB.T t      Drums.Event)
  | PartGuitar                (RTB.T t FiveButton.Event)
  | PartBass                  (RTB.T t FiveButton.Event)
  | PartKeys                  (RTB.T t FiveButton.Event)
  | PartRealGuitar            (RTB.T t  ProGuitar.Event)
  | PartRealGuitar22          (RTB.T t  ProGuitar.Event)
  | PartRealBass              (RTB.T t  ProGuitar.Event)
  | PartRealBass22            (RTB.T t  ProGuitar.Event)
  | PartRealKeys   Difficulty (RTB.T t    ProKeys.Event)
  | PartRealKeysPS Difficulty (RTB.T t     PSKeys.Event)
  | PartKeysAnimLH            (RTB.T t    ProKeys.Event)
  | PartKeysAnimRH            (RTB.T t    ProKeys.Event)
  | PartVocals                (RTB.T t     Vocals.Event)
  | Harm1                     (RTB.T t     Vocals.Event)
  | Harm2                     (RTB.T t     Vocals.Event)
  | Harm3                     (RTB.T t     Vocals.Event)
  | Events                    (RTB.T t     Events.Event)
  | Beat                      (RTB.T t       Beat.Event)
  | Venue                     (RTB.T t      Venue.Event)
  | RawTrack                  (RTB.T t              E.T)
  | MelodysEscape             (RTB.T t     Melody.Event)
  deriving (Eq, Ord, Show)

showMIDIFile :: Song [Track U.Beats] -> F.T
showMIDIFile s = let
  tempos = U.unmakeTempoMap $ s_tempos s
  sigs = case mapM U.showSignatureFull $ U.measureMapToTimeSigs $ s_signatures s of
    Nothing   -> RTB.singleton 0 $ fromJust $ U.showSignature 4
    Just evts -> evts
  tempoTrk = U.setTrackName "notes" $ RTB.merge tempos sigs
  in U.encodeFileBeats F.Parallel 480 $ tempoTrk : map showTrack (s_tracks s)

showMIDIFile' :: (MIDIFileFormat f) => Song (f U.Beats) -> F.T
showMIDIFile' s = let
  tempos = U.unmakeTempoMap $ s_tempos s
  sigs = case mapM U.showSignatureFull $ U.measureMapToTimeSigs $ s_signatures s of
    Nothing   -> RTB.singleton 0 $ fromJust $ U.showSignature 4
    Just evts -> evts
  tempoTrk = U.setTrackName "notes" $ RTB.merge tempos sigs
  in U.encodeFileBeats F.Parallel 480 $ tempoTrk : s_tracks (showMIDITracks s)

{- |
Work around two bugs by making sure higher note pitches come before lower ones.

First is a Phase Shift (v1.27) bug.
Phase Shift won't apply a tom/cymbal switch to gems simultaneous with it
unless the tom marker event comes before the gem event in the MIDI.
Oddly this same problem does not affect guitar/bass HOPO force notes.

Second is a Magma v1 bug.
If you have an overdrive phrase simultaneous with the only note in it,
and the phrase event comes after the note in the MIDI, Magma v1 will
complain that there are no notes under the phrase.
-}
fixEventOrder :: (NNC.C t) => RTB.T t E.T -> RTB.T t E.T
fixEventOrder = RTB.flatten . fmap (sortOn f) . RTB.collectCoincident
  where f x@(E.MetaEvent (Meta.TrackName _              )) = (-3, 0, x)
        f x@(E.MetaEvent (Meta.TextEvent "[lighting ()]")) = (-1, 0, x) -- magma v1: ensure [lighting ()] comes after simultaneous [verse]
        f x@(E.MetaEvent (Meta.TextEvent _              )) = (-2, 0, x)
        f x = case isNoteEdge x of
          Nothing         -> (0 :: Int, 0       , x)
          Just (p, False) -> (1       , negate p, x)
          Just (p, True ) -> (2       , negate p, x)

showTrack :: Track U.Beats -> RTB.T U.Beats E.T
showTrack = fixEventOrder . \case
  PartDrums             t -> U.setTrackName "PART DRUMS"          $ unparseAll t
  PartDrums2x           t -> U.setTrackName "PART DRUMS_2X"       $ unparseAll t
  PartGuitar            t -> U.setTrackName "PART GUITAR"         $ unparseAll t
  PartBass              t -> U.setTrackName "PART BASS"           $ unparseAll t
  PartKeys              t -> U.setTrackName "PART KEYS"           $ unparseAll t
  PartRealGuitar        t -> U.setTrackName "PART REAL_GUITAR"    $ unparseAll t
  PartRealGuitar22      t -> U.setTrackName "PART REAL_GUITAR_22" $ unparseAll t
  PartRealBass          t -> U.setTrackName "PART REAL_BASS"      $ unparseAll t
  PartRealBass22        t -> U.setTrackName "PART REAL_BASS_22"   $ unparseAll t
  PartRealKeys   Easy   t -> U.setTrackName "PART REAL_KEYS_E"    $ unparseAll t
  PartRealKeys   Medium t -> U.setTrackName "PART REAL_KEYS_M"    $ unparseAll t
  PartRealKeys   Hard   t -> U.setTrackName "PART REAL_KEYS_H"    $ unparseAll t
  PartRealKeys   Expert t -> U.setTrackName "PART REAL_KEYS_X"    $ unparseAll t
  PartRealKeysPS Easy   t -> U.setTrackName "PART REAL_KEYS_PS_E" $ unparseAll t
  PartRealKeysPS Medium t -> U.setTrackName "PART REAL_KEYS_PS_M" $ unparseAll t
  PartRealKeysPS Hard   t -> U.setTrackName "PART REAL_KEYS_PS_H" $ unparseAll t
  PartRealKeysPS Expert t -> U.setTrackName "PART REAL_KEYS_PS_X" $ unparseAll t
  PartKeysAnimLH        t -> U.setTrackName "PART KEYS_ANIM_LH"   $ unparseAll t
  PartKeysAnimRH        t -> U.setTrackName "PART KEYS_ANIM_RH"   $ unparseAll t
  PartVocals            t -> U.setTrackName "PART VOCALS"         $ unparseAll t
  Harm1                 t -> U.setTrackName "HARM1"               $ unparseAll t
  Harm2                 t -> U.setTrackName "HARM2"               $ unparseAll t
  Harm3                 t -> U.setTrackName "HARM3"               $ unparseAll t
  Events                t -> U.setTrackName "EVENTS"              $ unparseAll t
  Beat                  t -> U.setTrackName "BEAT"                $ unparseAll t
  Venue                 t -> U.setTrackName "VENUE"               $ unparseAll t
  MelodysEscape         t -> U.setTrackName "MELODY'S ESCAPE"     $ unparseAll t
  RawTrack              t -> t

readMIDIFile' :: (Monad m, MIDIFileFormat f) => F.T -> StackTraceT m (Song (f U.Beats))
readMIDIFile' mid = case U.decodeFile mid of
  Right _ -> fatal "SMPTE tracks not supported"
  Left trks -> let
    (tempoTrk, restTrks) = case trks of
      t : ts -> (t, ts)
      []     -> (RTB.empty, [])
    mmap = U.makeMeasureMap U.Truncate tempoTrk
    in readMIDITracks Song
      { s_tempos     = U.makeTempoMap tempoTrk
      , s_signatures = mmap
      , s_tracks     = restTrks
      }

readMIDIFile :: (Monad m) => F.T -> StackTraceT m (Song [Track U.Beats])
readMIDIFile mid = case U.decodeFile mid of
  Right _ -> fatal "SMPTE tracks not supported"
  Left trks -> let
    (tempoTrk, restTrks) = case trks of
      t : ts -> (t, ts)
      []     -> (RTB.empty, [])
    mmap = U.makeMeasureMap U.Truncate tempoTrk
    in do
      songTrks <- forM (zip ([1..] :: [Int]) restTrks) $ \(i, trk) ->
        inside ("track " ++ show i ++ " (0 is tempo track)") $ errorToWarning $ parseTrack mmap trk
      return Song
        { s_tempos     = U.makeTempoMap tempoTrk
        , s_signatures = mmap
        , s_tracks     = catMaybes songTrks
        }

-- | Does not attempt to parse any RB events; all tracks are kept as 'RawTrack'.
readMIDIRaw :: F.T -> Song [Track U.Beats]
readMIDIRaw mid = case U.decodeFile mid of
  Right _ -> error "RockBand.File.readMIDIRaw: SMPTE tracks not supported"
  Left trks -> let
    (tempoTrk, restTrks) = case trks of
      t : ts -> (t, ts)
      []     -> (RTB.empty, [])
    mmap = U.makeMeasureMap U.Truncate tempoTrk
    in Song
      { s_tempos     = U.makeTempoMap tempoTrk
      , s_signatures = mmap
      , s_tracks     = map RawTrack restTrks
      }

-- | Strips comments and track names from the track before handing it to a track parser.
stripTrack :: (NNC.C t) => RTB.T t E.T -> RTB.T t E.T
stripTrack = RTB.filter $ \e -> case e of
  E.MetaEvent (Meta.TextEvent ('#' : _)) -> False
  E.MetaEvent (Meta.TrackName _        ) -> False
  _                                      -> True

makeTrackParser :: (Monad m, Ord a) =>
  ParseOne U.Beats E.T a -> U.MeasureMap -> RTB.T U.Beats E.T -> StackTraceT m (RTB.T U.Beats a)
makeTrackParser p mmap trk = do
  let (good, bad) = parseAll p $ stripTrack trk
  forM_ (ATB.toPairList $ RTB.toAbsoluteEventList 0 bad) $ \(bts, e) ->
    inside (showPosition $ U.applyMeasureMap mmap bts) $ warn $ "Unrecognized event: " ++ show e
  return good

parseTrack :: (Monad m) => U.MeasureMap -> RTB.T U.Beats E.T -> StackTraceT m (Track U.Beats)
parseTrack mmap t = case U.trackName t of
  Nothing -> do
    warn "Track with no name"
    return $ RawTrack t
  Just s -> inside ("track named " ++ show s) $ case s of
    "PART DRUM"           -> PartDrums             <$> makeTrackParser parseOne mmap t
    "PART DRUMS"          -> PartDrums             <$> makeTrackParser parseOne mmap t
    "PART DRUMS_2X"       -> PartDrums2x           <$> makeTrackParser parseOne mmap t
    "PART GUITAR"         -> PartGuitar            <$> makeTrackParser parseOne mmap t
    "PART BASS"           -> PartBass              <$> makeTrackParser parseOne mmap t
    "PART KEYS"           -> PartKeys              <$> makeTrackParser parseOne mmap t
    "PART REAL_GUITAR"    -> PartRealGuitar        <$> makeTrackParser parseOne mmap t
    "PART REAL_GUITAR_22" -> PartRealGuitar22      <$> makeTrackParser parseOne mmap t
    "PART REAL_BASS"      -> PartRealBass          <$> makeTrackParser parseOne mmap t
    "PART REAL_BASS_22"   -> PartRealBass22        <$> makeTrackParser parseOne mmap t
    "PART REAL_KEYS_E"    -> PartRealKeys   Easy   <$> makeTrackParser parseOne mmap t
    "PART REAL_KEYS_M"    -> PartRealKeys   Medium <$> makeTrackParser parseOne mmap t
    "PART REAL_KEYS_H"    -> PartRealKeys   Hard   <$> makeTrackParser parseOne mmap t
    "PART REAL_KEYS_X"    -> PartRealKeys   Expert <$> makeTrackParser parseOne mmap t
    "PART REAL_KEYS_PS_E" -> PartRealKeysPS Easy   <$> makeTrackParser parseOne mmap t
    "PART REAL_KEYS_PS_M" -> PartRealKeysPS Medium <$> makeTrackParser parseOne mmap t
    "PART REAL_KEYS_PS_H" -> PartRealKeysPS Hard   <$> makeTrackParser parseOne mmap t
    "PART REAL_KEYS_PS_X" -> PartRealKeysPS Expert <$> makeTrackParser parseOne mmap t
    "PART KEYS_ANIM_LH"   -> PartKeysAnimLH        <$> makeTrackParser parseOne mmap t
    "PART KEYS_ANIM_RH"   -> PartKeysAnimRH        <$> makeTrackParser parseOne mmap t
    "PART VOCALS"         -> PartVocals            <$> makeTrackParser parseOne mmap t
    "HARM1"               -> Harm1                 <$> makeTrackParser parseOne mmap t
    "HARM2"               -> Harm2                 <$> makeTrackParser parseOne mmap t
    "HARM3"               -> Harm3                 <$> makeTrackParser parseOne mmap t
    "EVENTS"              -> Events                <$> makeTrackParser parseOne mmap t
    "BEAT"                -> Beat                  <$> makeTrackParser parseOne mmap t
    "VENUE"               -> Venue                 <$> makeTrackParser parseOne mmap t
    "MELODY'S ESCAPE"     -> MelodysEscape         <$> makeTrackParser parseOne mmap t
    _ -> do
      warn "Unrecognized track name"
      return $ RawTrack t

-- | midiscript format, where both measure and beats start from zero
showPosition :: U.MeasureBeats -> String
showPosition (m, b) = show m ++ "|" ++ show (realToFrac b :: Double)

playGuitarFile :: [Int] -> [Int] -> Song [Track U.Beats] -> Song [Track U.Beats]
playGuitarFile goffs boffs s =
  s { s_tracks = map RawTrack $ s_tracks s >>= playGuitarTrack goffs boffs }

playGuitarTrack :: [Int] -> [Int] -> Track U.Beats -> [RTB.T U.Beats E.T]
playGuitarTrack goffs boffs = \case
  PartRealGuitar   t -> gtr "GTR" t
  PartRealGuitar22 t -> gtr "GTR22" t
  PartRealBass     t -> bass "BASS" t
  PartRealBass22   t -> bass "BASS22" t
  _ -> []
  where gtr = go ProGuitar.standardGuitar goffs
        bass = go ProGuitar.standardBass boffs
        go stdtuning offs name trk = let
          tuning = zipWith (+) stdtuning $ offs ++ repeat 0
          expert = flip RTB.mapMaybe trk $ \case
            ProGuitar.DiffEvent Expert evt -> Just evt
            _                              -> Nothing
          in do
            (str, notes) <- ProGuitar.playGuitar tuning expert
            return $ U.setTrackName (name ++ "_" ++ show str) notes

copyExpert :: (NNC.C t) => Track t -> Track t
copyExpert = \case
  PartDrums        t -> PartDrums        $      Drums.copyExpert t
  PartDrums2x      t -> PartDrums2x      $      Drums.copyExpert t
  PartGuitar       t -> PartGuitar       $ FiveButton.copyExpert t
  PartBass         t -> PartBass         $ FiveButton.copyExpert t
  PartKeys         t -> PartKeys         $ FiveButton.copyExpert t
  PartRealGuitar   t -> PartRealGuitar   $  ProGuitar.copyExpert t
  PartRealGuitar22 t -> PartRealGuitar22 $  ProGuitar.copyExpert t
  PartRealBass     t -> PartRealBass     $  ProGuitar.copyExpert t
  PartRealBass22   t -> PartRealBass22   $  ProGuitar.copyExpert t
  trk                -> trk

-- | True if there are any playable notes in the first 2.5 seconds.
needsPad :: Song [Track U.Beats] -> Bool
needsPad (Song temps _ trks) = let
  sec2_5 = U.unapplyTempoMap temps (2.5 :: U.Seconds)
  early = \case
    PartDrums        t -> earlyDrums   t
    PartDrums2x      t -> earlyDrums   t
    PartGuitar       t -> earlyFive    t
    PartBass         t -> earlyFive    t
    PartKeys         t -> earlyFive    t
    PartRealGuitar   t -> earlyProGtr  t
    PartRealGuitar22 t -> earlyProGtr  t
    PartRealBass     t -> earlyProGtr  t
    PartRealBass22   t -> earlyProGtr  t
    PartRealKeys   _ t -> earlyProKeys t
    PartRealKeysPS _ t -> earlyPSKeys  t
    PartVocals       t -> earlyVox     t
    Harm1            t -> earlyVox     t
    Harm2            t -> earlyVox     t
    Harm3            t -> earlyVox     t
    _                  -> False
  earlyDrums = earlyPred $ \case
    Drums.DiffEvent _ (Drums.Note _) -> True
    _ -> False
  earlyFive = earlyPred $ \case
    FiveButton.DiffEvent _ (FiveButton.Note _) -> True
    _ -> False
  earlyProGtr = earlyPred $ \case
    ProGuitar.DiffEvent _ (ProGuitar.Note _) -> True
    _ -> False
  earlyProKeys = earlyPred $ \case
    ProKeys.Note{} -> True
    _ -> False
  earlyPSKeys = earlyPred $ \case
    PSKeys.Note{} -> True
    _ -> False
  earlyVox = earlyPred $ \case
    Vocals.Note{} -> True
    _ -> False
  earlyPred fn t = any fn $ U.trackTake sec2_5 t
  in any early trks

-- | Adds a given amount of 1 second increments to the start of the MIDI.
padMIDI :: Int -> Song [Track U.Beats] -> Song [Track U.Beats]
padMIDI 0       song                   = song
padMIDI seconds (Song temps sigs trks) = let
  beats = fromIntegral seconds * 2
  temps'
    = U.tempoMapFromBPS
    $ RTB.cons 0 2 -- 120 bpm
    $ RTB.delay beats
    $ U.tempoMapToBPS temps
  sigs'
    = U.measureMapFromTimeSigs U.Error
    $ RTB.cons 0 (U.TimeSig 1 1) -- 1/4
    $ RTB.delay beats
    $ U.measureMapToTimeSigs sigs
  trks' = flip map trks $ \case
    PartDrums         t -> PartDrums         $ padTrack t
    PartDrums2x       t -> PartDrums2x       $ padTrack t
    PartGuitar        t -> PartGuitar        $ padTrack t
    PartBass          t -> PartBass          $ padTrack t
    PartKeys          t -> PartKeys          $ padTrack t
    PartRealGuitar    t -> PartRealGuitar    $ padTrack t
    PartRealGuitar22  t -> PartRealGuitar22  $ padTrack t
    PartRealBass      t -> PartRealBass      $ padTrack t
    PartRealBass22    t -> PartRealBass22    $ padTrack t
    PartRealKeys   df t -> PartRealKeys   df $ padTrack t
    PartRealKeysPS df t -> PartRealKeysPS df $ padTrack t
    PartKeysAnimLH    t -> PartKeysAnimLH    $ padTrack t
    PartKeysAnimRH    t -> PartKeysAnimRH    $ padTrack t
    PartVocals        t -> PartVocals        $ padTrack t
    Harm1             t -> Harm1             $ padTrack t
    Harm2             t -> Harm2             $ padTrack t
    Harm3             t -> Harm3             $ padTrack t
    Events            t -> Events            $ padTrack t
    Beat              t -> Beat              $ padBeat  t
    Venue             t -> Venue             $ padTrack t
    RawTrack          t -> RawTrack          $ padRaw   t
    MelodysEscape     t -> MelodysEscape     $ padTrack t
  padTrack = RTB.delay beats
  padRaw t = let
    (z, nz) = U.trackSplitZero t
    (names, notNames) = partition (\case E.MetaEvent (Meta.TrackName _) -> True; _ -> False) z
    in U.trackGlueZero names $ RTB.delay beats $ U.trackGlueZero notNames nz
  padBeat
    = RTB.cons  0 Beat.Bar
    . foldr (.) id (replicate (seconds * 2 - 1) $ RTB.cons 1 Beat.Beat)
    . RTB.delay 1
  in Song temps' sigs' trks'
