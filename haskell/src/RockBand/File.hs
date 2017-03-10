{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.File where

import           Control.Monad                    (forM_, unless)
import           Control.Monad.Trans.StackTrace
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (partition, sortOn)
import           Data.Maybe                       (fromJust, mapMaybe)
import qualified MelodysEscape                    as Melody
import qualified Numeric.NonNegative.Class        as NNC
import qualified RockBand.Beat                    as Beat
import           RockBand.Common
import qualified RockBand.Drums                   as Drums
import qualified RockBand.Events                  as Events
import qualified RockBand.FiveButton              as FiveButton
import           RockBand.Parse
import qualified RockBand.PhaseShiftKeys          as PSKeys
import           RockBand.PhaseShiftMessage       (PSWrap (..), discardPS)
import qualified RockBand.ProGuitar               as ProGuitar
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Venue                   as Venue
import qualified RockBand.Vocals                  as Vocals
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U

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
  readMIDITracks (Song tempos mmap trks) = do
    rb3PartDrums        <- parseTracks mmap trks ["PART DRUMS"]
    rb3PartGuitar       <- parseTracks mmap trks ["PART GUITAR"]
    rb3PartBass         <- parseTracks mmap trks ["PART BASS"]
    rb3PartKeys         <- parseTracks mmap trks ["PART KEYS"]
    rb3PartRealGuitar   <- parseTracks mmap trks ["PART REAL_GUITAR"]
    rb3PartRealGuitar22 <- parseTracks mmap trks ["PART REAL_GUITAR_22"]
    rb3PartRealBass     <- parseTracks mmap trks ["PART REAL_BASS"]
    rb3PartRealBass22   <- parseTracks mmap trks ["PART REAL_BASS_22"]
    rb3PartRealKeysE    <- parseTracks mmap trks ["PART REAL_KEYS_E"]
    rb3PartRealKeysM    <- parseTracks mmap trks ["PART REAL_KEYS_M"]
    rb3PartRealKeysH    <- parseTracks mmap trks ["PART REAL_KEYS_H"]
    rb3PartRealKeysX    <- parseTracks mmap trks ["PART REAL_KEYS_X"]
    rb3PartKeysAnimLH   <- parseTracks mmap trks ["PART KEYS_ANIM_LH"]
    rb3PartKeysAnimRH   <- parseTracks mmap trks ["PART KEYS_ANIM_RH"]
    rb3PartVocals       <- parseTracks mmap trks ["PART VOCALS"]
    rb3Harm1            <- parseTracks mmap trks ["HARM1"]
    rb3Harm2            <- parseTracks mmap trks ["HARM2"]
    rb3Harm3            <- parseTracks mmap trks ["HARM3"]
    rb3Events           <- parseTracks mmap trks ["EVENTS"]
    rb3Beat             <- parseTracks mmap trks ["BEAT"]
    rb3Venue            <- parseTracks mmap trks ["VENUE"]
    knownTracks trks ["PART DRUMS", "PART GUITAR", "PART BASS", "PART KEYS", "PART REAL_GUITAR", "PART REAL_GUITAR_22", "PART REAL_BASS", "PART REAL_BASS_22", "PART REAL_KEYS_E", "PART REAL_KEYS_M", "PART REAL_KEYS_H", "PART REAL_KEYS_X", "PART KEYS_ANIM_LH", "PART KEYS_ANIM_RH", "PART VOCALS", "HARM1", "HARM2", "HARM3", "EVENTS", "BEAT", "VENUE"]
    return $ Song tempos mmap $ RB3File{..}
  showMIDITracks (Song tempos mmap RB3File{..}) = Song tempos mmap $ concat
    [ showMIDITrack "PART DRUMS" rb3PartDrums
    , showMIDITrack "PART GUITAR" rb3PartGuitar
    , showMIDITrack "PART BASS" rb3PartBass
    , showMIDITrack "PART KEYS" rb3PartKeys
    , showMIDITrack "PART REAL_GUITAR" rb3PartRealGuitar
    , showMIDITrack "PART REAL_GUITAR_22" rb3PartRealGuitar22
    , showMIDITrack "PART REAL_BASS" rb3PartRealBass
    , showMIDITrack "PART REAL_BASS_22" rb3PartRealBass22
    , showMIDITrack "PART REAL_KEYS_E" rb3PartRealKeysE
    , showMIDITrack "PART REAL_KEYS_M" rb3PartRealKeysM
    , showMIDITrack "PART REAL_KEYS_H" rb3PartRealKeysH
    , showMIDITrack "PART REAL_KEYS_X" rb3PartRealKeysX
    , showMIDITrack "PART KEYS_ANIM_LH" rb3PartKeysAnimLH
    , showMIDITrack "PART KEYS_ANIM_RH" rb3PartKeysAnimRH
    , showMIDITrack "PART VOCALS" rb3PartVocals
    , showMIDITrack "HARM1" rb3Harm1
    , showMIDITrack "HARM2" rb3Harm2
    , showMIDITrack "HARM3" rb3Harm3
    , showMIDITrack "EVENTS" rb3Events
    , showMIDITrack "BEAT" rb3Beat
    , showMIDITrack "VENUE" rb3Venue
    ]

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
  readMIDITracks (Song tempos mmap trks) = do
    psPartDrums        <- parseTracks mmap trks ["PART DRUMS", "PART DRUM"]
    psPartGuitar       <- parseTracks mmap trks ["PART GUITAR"]
    psPartBass         <- parseTracks mmap trks ["PART BASS"]
    psPartKeys         <- parseTracks mmap trks ["PART KEYS"]
    psPartRealGuitar   <- parseTracks mmap trks ["PART REAL_GUITAR"]
    psPartRealGuitar22 <- parseTracks mmap trks ["PART REAL_GUITAR_22"]
    psPartRealBass     <- parseTracks mmap trks ["PART REAL_BASS"]
    psPartRealBass22   <- parseTracks mmap trks ["PART REAL_BASS_22"]
    psPartRealKeysE    <- parseTracks mmap trks ["PART REAL_KEYS_E"]
    psPartRealKeysM    <- parseTracks mmap trks ["PART REAL_KEYS_M"]
    psPartRealKeysH    <- parseTracks mmap trks ["PART REAL_KEYS_H"]
    psPartRealKeysX    <- parseTracks mmap trks ["PART REAL_KEYS_X"]
    psPartRealKeysPsE  <- parseTracks mmap trks ["PART REAL_KEYS_PS_E"]
    psPartRealKeysPsM  <- parseTracks mmap trks ["PART REAL_KEYS_PS_M"]
    psPartRealKeysPsH  <- parseTracks mmap trks ["PART REAL_KEYS_PS_H"]
    psPartRealKeysPsX  <- parseTracks mmap trks ["PART REAL_KEYS_PS_X"]
    psPartKeysAnimLH   <- parseTracks mmap trks ["PART KEYS_ANIM_LH"]
    psPartKeysAnimRH   <- parseTracks mmap trks ["PART KEYS_ANIM_RH"]
    psPartVocals       <- parseTracks mmap trks ["PART VOCALS"]
    psHarm1            <- parseTracks mmap trks ["HARM1"]
    psHarm2            <- parseTracks mmap trks ["HARM2"]
    psHarm3            <- parseTracks mmap trks ["HARM3"]
    psEvents           <- parseTracks mmap trks ["EVENTS"]
    psBeat             <- parseTracks mmap trks ["BEAT"]
    psVenue            <- parseTracks mmap trks ["VENUE"]
    knownTracks trks ["PART DRUMS", "PART GUITAR", "PART BASS", "PART KEYS", "PART REAL_GUITAR", "PART REAL_GUITAR_22", "PART REAL_BASS", "PART REAL_BASS_22", "PART REAL_KEYS_E", "PART REAL_KEYS_M", "PART REAL_KEYS_H", "PART REAL_KEYS_X", "PART REAL_KEYS_PS_E", "PART REAL_KEYS_PS_M", "PART REAL_KEYS_PS_H", "PART REAL_KEYS_PS_X", "PART KEYS_ANIM_LH", "PART KEYS_ANIM_RH", "PART VOCALS", "HARM1", "HARM2", "HARM3", "EVENTS", "BEAT", "VENUE"]
    return $ Song tempos mmap $ PSFile{..}
  showMIDITracks (Song tempos mmap PSFile{..}) = Song tempos mmap $ concat
    [ showMIDITrack "PART DRUMS" psPartDrums
    , showMIDITrack "PART GUITAR" psPartGuitar
    , showMIDITrack "PART BASS" psPartBass
    , showMIDITrack "PART KEYS" psPartKeys
    , showMIDITrack "PART REAL_GUITAR" psPartRealGuitar
    , showMIDITrack "PART REAL_GUITAR_22" psPartRealGuitar22
    , showMIDITrack "PART REAL_BASS" psPartRealBass
    , showMIDITrack "PART REAL_BASS_22" psPartRealBass22
    , showMIDITrack "PART REAL_KEYS_E" psPartRealKeysE
    , showMIDITrack "PART REAL_KEYS_M" psPartRealKeysM
    , showMIDITrack "PART REAL_KEYS_H" psPartRealKeysH
    , showMIDITrack "PART REAL_KEYS_X" psPartRealKeysX
    , showMIDITrack "PART REAL_KEYS_PS_E" psPartRealKeysPsE
    , showMIDITrack "PART REAL_KEYS_PS_M" psPartRealKeysPsM
    , showMIDITrack "PART REAL_KEYS_PS_H" psPartRealKeysPsH
    , showMIDITrack "PART REAL_KEYS_PS_X" psPartRealKeysPsX
    , showMIDITrack "PART KEYS_ANIM_LH" psPartKeysAnimLH
    , showMIDITrack "PART KEYS_ANIM_RH" psPartKeysAnimRH
    , showMIDITrack "PART VOCALS" psPartVocals
    , showMIDITrack "HARM1" psHarm1
    , showMIDITrack "HARM2" psHarm2
    , showMIDITrack "HARM3" psHarm3
    , showMIDITrack "EVENTS" psEvents
    , showMIDITrack "BEAT" psBeat
    , showMIDITrack "VENUE" psVenue
    ]

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
  , onyxMelodysEscape    :: RTB.T t             Melody.Event
  -- , onyxRaw              :: [RTB.T t E.T]
  } deriving (Eq, Ord, Show)

instance MIDIFileFormat OnyxFile where
  readMIDITracks (Song tempos mmap trks) = do
    onyxPartDrums        <- parseTracks mmap trks ["PART DRUMS"]
    onyxPartDrums2x      <- parseTracks mmap trks ["PART DRUMS_2X"]
    onyxPartGuitar       <- parseTracks mmap trks ["PART GUITAR"]
    onyxPartBass         <- parseTracks mmap trks ["PART BASS"]
    onyxPartKeys         <- parseTracks mmap trks ["PART KEYS"]
    onyxPartRealGuitar   <- parseTracks mmap trks ["PART REAL_GUITAR"]
    onyxPartRealGuitar22 <- parseTracks mmap trks ["PART REAL_GUITAR_22"]
    onyxPartRealBass     <- parseTracks mmap trks ["PART REAL_BASS"]
    onyxPartRealBass22   <- parseTracks mmap trks ["PART REAL_BASS_22"]
    onyxPartRealKeysE    <- parseTracks mmap trks ["PART REAL_KEYS_E"]
    onyxPartRealKeysM    <- parseTracks mmap trks ["PART REAL_KEYS_M"]
    onyxPartRealKeysH    <- parseTracks mmap trks ["PART REAL_KEYS_H"]
    onyxPartRealKeysX    <- parseTracks mmap trks ["PART REAL_KEYS_X"]
    onyxPartKeysAnimLH   <- parseTracks mmap trks ["PART KEYS_ANIM_LH"]
    onyxPartKeysAnimRH   <- parseTracks mmap trks ["PART KEYS_ANIM_RH"]
    onyxPartVocals       <- parseTracks mmap trks ["PART VOCALS"]
    onyxHarm1            <- parseTracks mmap trks ["HARM1"]
    onyxHarm2            <- parseTracks mmap trks ["HARM2"]
    onyxHarm3            <- parseTracks mmap trks ["HARM3"]
    onyxEvents           <- parseTracks mmap trks ["EVENTS"]
    onyxBeat             <- parseTracks mmap trks ["BEAT"]
    onyxVenue            <- parseTracks mmap trks ["VENUE"]
    onyxMelodysEscape    <- parseTracks mmap trks ["MELODY'S ESCAPE"]
    knownTracks trks ["PART DRUMS", "PART DRUMS_2X", "PART GUITAR", "PART BASS", "PART KEYS", "PART REAL_GUITAR", "PART REAL_GUITAR_22", "PART REAL_BASS", "PART REAL_BASS_22", "PART REAL_KEYS_E", "PART REAL_KEYS_M", "PART REAL_KEYS_H", "PART REAL_KEYS_X", "PART KEYS_ANIM_LH", "PART KEYS_ANIM_RH", "PART VOCALS", "HARM1", "HARM2", "HARM3", "EVENTS", "BEAT", "VENUE", "MELODY'S ESCAPE"]
    return $ Song tempos mmap OnyxFile{..}
  showMIDITracks (Song tempos mmap OnyxFile{..}) = Song tempos mmap $ concat
    [ showMIDITrack "PART DRUMS" onyxPartDrums
    , showMIDITrack "PART DRUMS_2X" onyxPartDrums2x
    , showMIDITrack "PART GUITAR" onyxPartGuitar
    , showMIDITrack "PART BASS" onyxPartBass
    , showMIDITrack "PART KEYS" onyxPartKeys
    , showMIDITrack "PART REAL_GUITAR" onyxPartRealGuitar
    , showMIDITrack "PART REAL_GUITAR_22" onyxPartRealGuitar22
    , showMIDITrack "PART REAL_BASS" onyxPartRealBass
    , showMIDITrack "PART REAL_BASS_22" onyxPartRealBass22
    , showMIDITrack "PART REAL_KEYS_E" onyxPartRealKeysE
    , showMIDITrack "PART REAL_KEYS_M" onyxPartRealKeysM
    , showMIDITrack "PART REAL_KEYS_H" onyxPartRealKeysH
    , showMIDITrack "PART REAL_KEYS_X" onyxPartRealKeysX
    , showMIDITrack "PART KEYS_ANIM_LH" onyxPartKeysAnimLH
    , showMIDITrack "PART KEYS_ANIM_RH" onyxPartKeysAnimRH
    , showMIDITrack "PART VOCALS" onyxPartVocals
    , showMIDITrack "HARM1" onyxHarm1
    , showMIDITrack "HARM2" onyxHarm2
    , showMIDITrack "HARM3" onyxHarm3
    , showMIDITrack "EVENTS" onyxEvents
    , showMIDITrack "BEAT" onyxBeat
    , showMIDITrack "VENUE" onyxVenue
    , showMIDITrack "MELODY'S ESCAPE" onyxMelodysEscape
    ]

newtype RawFile t = RawFile { rawTracks :: [RTB.T t E.T] }
  deriving (Eq, Ord, Show)

instance MIDIFileFormat RawFile where
  readMIDITracks = return . fmap RawFile
  showMIDITracks = fmap rawTracks

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

-- | midiscript format, where both measure and beats start from zero
showPosition :: U.MeasureBeats -> String
showPosition (m, b) = show m ++ "|" ++ show (realToFrac b :: Double)

playGuitarFile :: [Int] -> [Int] -> Song (OnyxFile U.Beats) -> Song (RawFile U.Beats)
playGuitarFile goffs boffs (Song tempos mmap trks) = Song tempos mmap $ RawFile $ let
  gtr = go ProGuitar.standardGuitar goffs
  bass = go ProGuitar.standardBass boffs
  go stdtuning offs name trk = let
    tuning = zipWith (+) stdtuning $ offs ++ repeat 0
    expert = flip RTB.mapMaybe trk $ \case
      ProGuitar.DiffEvent Expert evt -> Just evt
      _                              -> Nothing
    in do
      (str, notes) <- ProGuitar.playGuitar tuning expert
      return $ U.setTrackName (name ++ "_" ++ show str) notes
  in concat
    [ gtr  "GTR"    $ discardPS $ onyxPartRealGuitar   trks
    , gtr  "GTR22"  $ discardPS $ onyxPartRealGuitar22 trks
    , bass "BASS"   $ discardPS $ onyxPartRealBass     trks
    , bass "BASS22" $ discardPS $ onyxPartRealBass22   trks
    ]

-- | True if there are any playable notes in the first 2.5 seconds.
needsPad :: Song (OnyxFile U.Beats) -> Bool
needsPad (Song temps _ trks) = let
  sec2_5 = U.unapplyTempoMap temps (2.5 :: U.Seconds)
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
  earlyVox = earlyPred $ \case
    Vocals.Note{} -> True
    _ -> False
  earlyPred fn t = any fn $ U.trackTake sec2_5 t
  in or
    [ earlyDrums   $ discardPS $ onyxPartDrums        trks
    , earlyDrums   $ discardPS $ onyxPartDrums2x      trks
    , earlyFive    $ discardPS $ onyxPartGuitar       trks
    , earlyFive    $ discardPS $ onyxPartBass         trks
    , earlyFive    $ discardPS $ onyxPartKeys         trks
    , earlyProGtr  $ discardPS $ onyxPartRealGuitar   trks
    , earlyProGtr  $ discardPS $ onyxPartRealGuitar22 trks
    , earlyProGtr  $ discardPS $ onyxPartRealBass     trks
    , earlyProGtr  $ discardPS $ onyxPartRealBass22   trks
    , earlyProKeys $ discardPS $ onyxPartRealKeysE    trks
    , earlyProKeys $ discardPS $ onyxPartRealKeysM    trks
    , earlyProKeys $ discardPS $ onyxPartRealKeysH    trks
    , earlyProKeys $ discardPS $ onyxPartRealKeysX    trks
    , earlyVox     $ discardPS $ onyxPartVocals       trks
    , earlyVox     $ discardPS $ onyxHarm1            trks
    , earlyVox     $ discardPS $ onyxHarm2            trks
    , earlyVox     $ discardPS $ onyxHarm3            trks
    ]

-- | Adds a given amount of 1 second increments to the start of the MIDI.
padMIDI :: Int -> Song (RawFile U.Beats) -> Song (RawFile U.Beats)
padMIDI 0       song                   = song
padMIDI seconds (Song temps sigs (RawFile trks)) = let
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
  trks' = flip mapMaybe trks $ \trk -> case U.trackName trk of
    Just "BEAT" -> Nothing -- TODO
    _           -> Just $ padRaw trk
  padRaw t = let
    (z, nz) = U.trackSplitZero t
    (names, notNames) = partition (\case E.MetaEvent (Meta.TrackName _) -> True; _ -> False) z
    in U.trackGlueZero names $ RTB.delay beats $ U.trackGlueZero notNames nz
  {-
  padBeat
    = RTB.cons  0 Beat.Bar
    . foldr (.) id (replicate (seconds * 2 - 1) $ RTB.cons 1 Beat.Beat)
    . RTB.delay 1
  -}
  in Song temps' sigs' $ RawFile trks'
