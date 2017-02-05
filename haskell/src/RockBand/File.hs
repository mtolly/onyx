{-# LANGUAGE LambdaCase #-}
module RockBand.File where

import           Control.Monad                    (forM, forM_)
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
import qualified RockBand.ProGuitar               as ProGuitar
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Venue                   as Venue
import qualified RockBand.Vocals                  as Vocals

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

data Song t = Song
  { s_tempos     :: U.TempoMap
  , s_signatures :: U.MeasureMap
  , s_tracks     :: [Track t]
  } deriving (Eq, Show)

showMIDIFile :: Song U.Beats -> F.T
showMIDIFile s = let
  tempos = U.unmakeTempoMap $ s_tempos s
  sigs = case mapM U.showSignatureFull $ U.measureMapToTimeSigs $ s_signatures s of
    Nothing   -> RTB.singleton 0 $ fromJust $ U.showSignature 4
    Just evts -> evts
  tempoTrk = U.setTrackName "notes" $ RTB.merge tempos sigs
  in U.encodeFileBeats F.Parallel 480 $ tempoTrk : map showTrack (s_tracks s)

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
  PartDrums             t -> U.setTrackName "PART DRUMS"          $ Drums.unparseNice (1/8) t
  PartDrums2x           t -> U.setTrackName "PART DRUMS_2X"       $ Drums.unparseNice (1/8) t
  PartGuitar            t -> U.setTrackName "PART GUITAR"         $ unparseAll unparseOne $ FiveButton.showBlipsNice (1/8) t
  PartBass              t -> U.setTrackName "PART BASS"           $ unparseAll unparseOne $ FiveButton.showBlipsNice (1/8) t
  PartKeys              t -> U.setTrackName "PART KEYS"           $ unparseAll unparseOne $ FiveButton.showBlipsNice (1/8) t
  PartRealGuitar        t -> U.setTrackName "PART REAL_GUITAR"    $ unparseAll unparseOne t
  PartRealGuitar22      t -> U.setTrackName "PART REAL_GUITAR_22" $ unparseAll unparseOne t
  PartRealBass          t -> U.setTrackName "PART REAL_BASS"      $ unparseAll unparseOne t
  PartRealBass22        t -> U.setTrackName "PART REAL_BASS_22"   $ unparseAll unparseOne t
  PartRealKeys   Easy   t -> U.setTrackName "PART REAL_KEYS_E"    $ ProKeys.unparseNice (1/8) t
  PartRealKeys   Medium t -> U.setTrackName "PART REAL_KEYS_M"    $ ProKeys.unparseNice (1/8) t
  PartRealKeys   Hard   t -> U.setTrackName "PART REAL_KEYS_H"    $ ProKeys.unparseNice (1/8) t
  PartRealKeys   Expert t -> U.setTrackName "PART REAL_KEYS_X"    $ ProKeys.unparseNice (1/8) t
  PartRealKeysPS Easy   t -> U.setTrackName "PART REAL_KEYS_PS_E" $ unparseAll unparseOne t
  PartRealKeysPS Medium t -> U.setTrackName "PART REAL_KEYS_PS_M" $ unparseAll unparseOne t
  PartRealKeysPS Hard   t -> U.setTrackName "PART REAL_KEYS_PS_H" $ unparseAll unparseOne t
  PartRealKeysPS Expert t -> U.setTrackName "PART REAL_KEYS_PS_X" $ unparseAll unparseOne t
  PartKeysAnimLH        t -> U.setTrackName "PART KEYS_ANIM_LH"   $ unparseAll unparseOne t
  PartKeysAnimRH        t -> U.setTrackName "PART KEYS_ANIM_RH"   $ unparseAll unparseOne t
  PartVocals            t -> U.setTrackName "PART VOCALS"         $ unparseAll unparseOne t
  Harm1                 t -> U.setTrackName "HARM1"               $ unparseAll unparseOne t
  Harm2                 t -> U.setTrackName "HARM2"               $ unparseAll unparseOne t
  Harm3                 t -> U.setTrackName "HARM3"               $ unparseAll unparseOne t
  Events                t -> U.setTrackName "EVENTS"              $ unparseAll unparseOne t
  Beat                  t -> U.setTrackName "BEAT"                $ unparseAll unparseOne t
  Venue                 t -> U.setTrackName "VENUE"               $ unparseAll unparseOne t
  MelodysEscape         t -> U.setTrackName "MELODY'S ESCAPE"     $ unparseAll unparseOne t
  RawTrack              t -> t

readMIDIFile :: (Monad m) => F.T -> StackTraceT m (Song U.Beats)
readMIDIFile mid = case U.decodeFile mid of
  Right _ -> fatal "SMPTE tracks not supported"
  Left trks -> let
    (tempoTrk, restTrks) = case trks of
      t : ts -> (t, ts)
      []     -> (RTB.empty, [])
    mmap = U.makeMeasureMap U.Truncate tempoTrk
    in do
      songTrks <- forM (zip ([1..] :: [Int]) restTrks) $ \(i, trk) ->
        inside ("track " ++ show i ++ " (0 is tempo track)") $ optional $ parseTrack mmap trk
      return Song
        { s_tempos     = U.makeTempoMap tempoTrk
        , s_signatures = mmap
        , s_tracks     = catMaybes songTrks
        }

-- | Does not attempt to parse any RB events; all tracks are kept as 'RawTrack'.
readMIDIRaw :: F.T -> Song U.Beats
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

playGuitarFile :: [Int] -> [Int] -> Song U.Beats -> Song U.Beats
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

eachTrack :: (Track t -> Track t) -> Song t -> Song t
eachTrack f s = s { s_tracks = map f $ s_tracks s }

-- | True if there are any playable notes in the first 2.5 seconds.
needsPad :: Song U.Beats -> Bool
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
padMIDI :: Int -> Song U.Beats -> Song U.Beats
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
