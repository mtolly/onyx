{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.File where

import           Control.Monad                    (forM, forM_, unless)
import           Control.Monad.Trans.StackTrace
import           Data.Default.Class               (Default (..))
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Hashable                    (Hashable (..))
import           Data.List                        (nub, sortOn, stripPrefix)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromJust, fromMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import qualified MelodysEscape                    as Melody
import qualified Numeric.NonNegative.Class        as NNC
import qualified RockBand.Beat                    as Beat
import           RockBand.Common
import qualified RockBand.Drums                   as Drums
import qualified RockBand.Events                  as Events
import qualified RockBand.FiveButton              as FiveButton
import           RockBand.Parse
import qualified RockBand.PhaseShiftKeys          as PSKeys
import           RockBand.PhaseShiftMessage       (PSWrap (..), discardPS,
                                                   withRB)
import qualified RockBand.ProGuitar               as ProGuitar
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Venue                   as Venue
import qualified RockBand.VenueRB2                as VenueRB2
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
    return $ Song tempos mmap RB3File{..}
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
  , rb2Venue      :: RTB.T t   VenueRB2.Event
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
    return $ Song tempos mmap RB2File{..}
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
  , psPartDrums2x      :: RTB.T t (PSWrap      Drums.Event) -- hack for import
  , psPartRealDrumsPS  :: RTB.T t (PSWrap      Drums.Event)
  , psPartGuitar       :: RTB.T t (PSWrap FiveButton.Event)
  , psPartBass         :: RTB.T t (PSWrap FiveButton.Event)
  , psPartKeys         :: RTB.T t (PSWrap FiveButton.Event)
  , psPartRhythm       :: RTB.T t (PSWrap FiveButton.Event)
  , psPartGuitarCoop   :: RTB.T t (PSWrap FiveButton.Event)
  , psPartRealGuitar   :: RTB.T t (PSWrap  ProGuitar.Event)
  , psPartRealGuitar22 :: RTB.T t (PSWrap  ProGuitar.Event)
  , psPartRealBass     :: RTB.T t (PSWrap  ProGuitar.Event)
  , psPartRealBass22   :: RTB.T t (PSWrap  ProGuitar.Event)
  , psPartRealKeysE    :: RTB.T t (PSWrap    ProKeys.Event)
  , psPartRealKeysM    :: RTB.T t (PSWrap    ProKeys.Event)
  , psPartRealKeysH    :: RTB.T t (PSWrap    ProKeys.Event)
  , psPartRealKeysX    :: RTB.T t (PSWrap    ProKeys.Event)
  , psPartRealKeysPS_E :: RTB.T t (PSWrap     PSKeys.Event)
  , psPartRealKeysPS_M :: RTB.T t (PSWrap     PSKeys.Event)
  , psPartRealKeysPS_H :: RTB.T t (PSWrap     PSKeys.Event)
  , psPartRealKeysPS_X :: RTB.T t (PSWrap     PSKeys.Event)
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
    psPartDrums2x      <- parseTracks mmap trks ["PART DRUMS_2X"]
    psPartRealDrumsPS  <- parseTracks mmap trks ["PART REAL_DRUMS_PS"]
    psPartGuitar       <- parseTracks mmap trks ["PART GUITAR"]
    psPartBass         <- parseTracks mmap trks ["PART BASS"]
    psPartKeys         <- parseTracks mmap trks ["PART KEYS"]
    psPartRhythm       <- parseTracks mmap trks ["PART RHYTHM"]
    psPartGuitarCoop   <- parseTracks mmap trks ["PART GUITAR COOP"]
    psPartRealGuitar   <- parseTracks mmap trks ["PART REAL_GUITAR"]
    psPartRealGuitar22 <- parseTracks mmap trks ["PART REAL_GUITAR_22"]
    psPartRealBass     <- parseTracks mmap trks ["PART REAL_BASS"]
    psPartRealBass22   <- parseTracks mmap trks ["PART REAL_BASS_22"]
    psPartRealKeysE    <- parseTracks mmap trks ["PART REAL_KEYS_E"]
    psPartRealKeysM    <- parseTracks mmap trks ["PART REAL_KEYS_M"]
    psPartRealKeysH    <- parseTracks mmap trks ["PART REAL_KEYS_H"]
    psPartRealKeysX    <- parseTracks mmap trks ["PART REAL_KEYS_X"]
    psPartRealKeysPS_E <- parseTracks mmap trks ["PART REAL_KEYS_PS_E"]
    psPartRealKeysPS_M <- parseTracks mmap trks ["PART REAL_KEYS_PS_M"]
    psPartRealKeysPS_H <- parseTracks mmap trks ["PART REAL_KEYS_PS_H"]
    psPartRealKeysPS_X <- parseTracks mmap trks ["PART REAL_KEYS_PS_X"]
    psPartKeysAnimLH   <- parseTracks mmap trks ["PART KEYS_ANIM_LH"]
    psPartKeysAnimRH   <- parseTracks mmap trks ["PART KEYS_ANIM_RH"]
    psPartVocals       <- parseTracks mmap trks ["PART VOCALS"]
    psHarm1            <- parseTracks mmap trks ["HARM1"]
    psHarm2            <- parseTracks mmap trks ["HARM2"]
    psHarm3            <- parseTracks mmap trks ["HARM3"]
    psEvents           <- parseTracks mmap trks ["EVENTS"]
    psBeat             <- parseTracks mmap trks ["BEAT"]
    psVenue            <- parseTracks mmap trks ["VENUE"]
    knownTracks trks ["PART DRUMS", "PART DRUM", "PART REAL_DRUMS_PS", "PART GUITAR", "PART BASS", "PART KEYS", "PART RHYTHM", "PART GUITAR COOP", "PART REAL_GUITAR", "PART REAL_GUITAR_22", "PART REAL_BASS", "PART REAL_BASS_22", "PART REAL_KEYS_E", "PART REAL_KEYS_M", "PART REAL_KEYS_H", "PART REAL_KEYS_X", "PART REAL_KEYS_PS_E", "PART REAL_KEYS_PS_M", "PART REAL_KEYS_PS_H", "PART REAL_KEYS_PS_X", "PART KEYS_ANIM_LH", "PART KEYS_ANIM_RH", "PART VOCALS", "HARM1", "HARM2", "HARM3", "EVENTS", "BEAT", "VENUE"]
    return $ Song tempos mmap PSFile{..}
  showMIDITracks (Song tempos mmap PSFile{..}) = Song tempos mmap $ concat
    [ showMIDITrack "PART DRUMS" psPartDrums
    , showMIDITrack "PART DRUMS_2X" psPartDrums2x
    , showMIDITrack "PART REAL_DRUMS_PS" psPartRealDrumsPS
    , showMIDITrack "PART GUITAR" psPartGuitar
    , showMIDITrack "PART BASS" psPartBass
    , showMIDITrack "PART KEYS" psPartKeys
    , showMIDITrack "PART RHYTHM" psPartRhythm
    , showMIDITrack "PART GUITAR COOP" psPartGuitarCoop
    , showMIDITrack "PART REAL_GUITAR" psPartRealGuitar
    , showMIDITrack "PART REAL_GUITAR_22" psPartRealGuitar22
    , showMIDITrack "PART REAL_BASS" psPartRealBass
    , showMIDITrack "PART REAL_BASS_22" psPartRealBass22
    , showMIDITrack "PART REAL_KEYS_E" psPartRealKeysE
    , showMIDITrack "PART REAL_KEYS_M" psPartRealKeysM
    , showMIDITrack "PART REAL_KEYS_H" psPartRealKeysH
    , showMIDITrack "PART REAL_KEYS_X" psPartRealKeysX
    , showMIDITrack "PART REAL_KEYS_PS_E" psPartRealKeysPS_E
    , showMIDITrack "PART REAL_KEYS_PS_M" psPartRealKeysPS_M
    , showMIDITrack "PART REAL_KEYS_PS_H" psPartRealKeysPS_H
    , showMIDITrack "PART REAL_KEYS_PS_X" psPartRealKeysPS_X
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

data FlexPartName
  = FlexGuitar
  | FlexBass
  | FlexDrums
  | FlexKeys
  | FlexVocal
  | FlexExtra T.Text
  deriving (Eq, Ord, Show, Read)

readPartName :: T.Text -> FlexPartName
readPartName = \case
  "guitar" -> FlexGuitar
  "bass"   -> FlexBass
  "drums"  -> FlexDrums
  "keys"   -> FlexKeys
  "vocal"  -> FlexVocal
  t        -> FlexExtra t

getPartName :: FlexPartName -> T.Text
getPartName = \case
  FlexGuitar  -> "guitar"
  FlexBass    -> "bass"
  FlexDrums   -> "drums"
  FlexKeys    -> "keys"
  FlexVocal   -> "vocal"
  FlexExtra t -> t

instance Hashable FlexPartName where
  hashWithSalt salt = hashWithSalt salt . getPartName

data FlexPart t = FlexPart
  { flexPartDrums        :: RTB.T t (PSWrap      Drums.Event)
  , flexPartDrums2x      :: RTB.T t (PSWrap      Drums.Event)
  , flexFiveButton       :: RTB.T t (PSWrap FiveButton.Event)
  , flexFiveIsKeys       :: Bool
  , flexPartRealGuitar   :: RTB.T t (PSWrap  ProGuitar.Event)
  , flexPartRealGuitar22 :: RTB.T t (PSWrap  ProGuitar.Event)
  , flexPartRealKeysE    :: RTB.T t (PSWrap    ProKeys.Event)
  , flexPartRealKeysM    :: RTB.T t (PSWrap    ProKeys.Event)
  , flexPartRealKeysH    :: RTB.T t (PSWrap    ProKeys.Event)
  , flexPartRealKeysX    :: RTB.T t (PSWrap    ProKeys.Event)
  , flexPartKeysAnimLH   :: RTB.T t (PSWrap    ProKeys.Event)
  , flexPartKeysAnimRH   :: RTB.T t (PSWrap    ProKeys.Event)
  , flexPartVocals       :: RTB.T t (PSWrap     Vocals.Event)
  , flexHarm1            :: RTB.T t (PSWrap     Vocals.Event)
  , flexHarm2            :: RTB.T t (PSWrap     Vocals.Event)
  , flexHarm3            :: RTB.T t (PSWrap     Vocals.Event)
  } deriving (Eq, Ord, Show)

instance Default (FlexPart t) where
  def = FlexPart
    RTB.empty RTB.empty RTB.empty False
    RTB.empty RTB.empty RTB.empty RTB.empty
    RTB.empty RTB.empty RTB.empty RTB.empty
    RTB.empty RTB.empty RTB.empty RTB.empty

data OnyxFile t = OnyxFile
  { onyxFlexParts     :: Map.Map FlexPartName (FlexPart t)
  , onyxEvents        :: RTB.T t (PSWrap     Events.Event)
  , onyxBeat          :: RTB.T t (PSWrap       Beat.Event)
  , onyxVenue         :: RTB.T t (PSWrap      Venue.Event)
  , onyxVenueRB2      :: RTB.T t (PSWrap   VenueRB2.Event)
  , onyxMelodysEscape :: RTB.T t             Melody.Event
  } deriving (Eq, Ord, Show)

instance Default (OnyxFile t) where
  def = OnyxFile Map.empty RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty

getFlexPart :: FlexPartName -> OnyxFile t -> FlexPart t
getFlexPart part = fromMaybe def . Map.lookup part . onyxFlexParts

instance MIDIFileFormat OnyxFile where
  readMIDITracks (Song tempos mmap trks) = do
    let extraNames = map FlexExtra $ nub $ flip mapMaybe trks $ \trk -> do
          name <- U.trackName trk
          name' <- stripPrefix "[" name
          return $ T.pack $ takeWhile (/= ']') name'
        allNames = [FlexGuitar, FlexBass, FlexDrums, FlexKeys, FlexVocal] ++ extraNames
    onyxFlexParts <- fmap Map.fromList $ forM allNames $ \partName -> do
      let prefix = "[" ++ T.unpack (getPartName partName) ++ "] "
          optPrefix defPart name = if partName == defPart
            then [name, prefix ++ name]
            else [prefix ++ name]
      flexPartDrums        <- parseTracks mmap trks $ optPrefix FlexDrums  "PART DRUMS"
      flexPartDrums2x      <- parseTracks mmap trks $ optPrefix FlexDrums  "PART DRUMS_2X"
      gtr                  <- parseTracks mmap trks $ optPrefix FlexGuitar "PART GUITAR"
      bass                 <- parseTracks mmap trks $ optPrefix FlexBass   "PART BASS"
      keys                 <- parseTracks mmap trks $ optPrefix FlexKeys   "PART KEYS"
      (flexFiveButton, flexFiveIsKeys) <- case (RTB.null gtr, RTB.null bass, RTB.null keys) of
        (False,  True,  True) -> return (      gtr, False)
        ( True, False,  True) -> return (     bass, False)
        ( True,  True, False) -> return (     keys,  True)
        ( True,  True,  True) -> return (RTB.empty, False)
        _ -> fatal $ show partName ++ " has more than one GRYBO track authored!"
      progtr  <- parseTracks mmap trks $ optPrefix FlexGuitar "PART REAL_GUITAR"
      probass <- parseTracks mmap trks $ optPrefix FlexBass   "PART REAL_BASS"
      let flexPartRealGuitar = RTB.merge progtr probass
      progtr22  <- parseTracks mmap trks $ optPrefix FlexGuitar "PART REAL_GUITAR_22"
      probass22 <- parseTracks mmap trks $ optPrefix FlexBass   "PART REAL_BASS_22"
      let flexPartRealGuitar22 = RTB.merge progtr22 probass22
      flexPartRealKeysE    <- parseTracks mmap trks $ optPrefix FlexKeys  "PART REAL_KEYS_E"
      flexPartRealKeysM    <- parseTracks mmap trks $ optPrefix FlexKeys  "PART REAL_KEYS_M"
      flexPartRealKeysH    <- parseTracks mmap trks $ optPrefix FlexKeys  "PART REAL_KEYS_H"
      flexPartRealKeysX    <- parseTracks mmap trks $ optPrefix FlexKeys  "PART REAL_KEYS_X"
      flexPartKeysAnimLH   <- parseTracks mmap trks $ optPrefix FlexKeys  "PART KEYS_ANIM_LH"
      flexPartKeysAnimRH   <- parseTracks mmap trks $ optPrefix FlexKeys  "PART KEYS_ANIM_RH"
      flexPartVocals       <- parseTracks mmap trks $ optPrefix FlexVocal "PART VOCALS"
      flexHarm1            <- parseTracks mmap trks $ optPrefix FlexVocal "HARM1"
      flexHarm2            <- parseTracks mmap trks $ optPrefix FlexVocal "HARM2"
      flexHarm3            <- parseTracks mmap trks $ optPrefix FlexVocal "HARM3"
      return (partName, FlexPart{..})
    onyxEvents           <- parseTracks mmap trks ["EVENTS"]
    onyxBeat             <- parseTracks mmap trks ["BEAT"]
    onyxVenue            <- parseTracks mmap trks ["VENUE"]
    onyxVenueRB2         <- parseTracks mmap trks ["VENUE RB2"]
    onyxMelodysEscape    <- parseTracks mmap trks ["MELODY'S ESCAPE"]
    knownTracks trks $ ["EVENTS", "BEAT", "VENUE", "MELODY'S ESCAPE"] ++ do
      trkName <- ["PART DRUMS", "PART DRUMS_2X", "PART GUITAR", "PART BASS", "PART KEYS", "PART REAL_GUITAR", "PART REAL_GUITAR_22", "PART REAL_BASS", "PART REAL_BASS_22", "PART REAL_KEYS_E", "PART REAL_KEYS_M", "PART REAL_KEYS_H", "PART REAL_KEYS_X", "PART KEYS_ANIM_LH", "PART KEYS_ANIM_RH", "PART VOCALS", "HARM1", "HARM2", "HARM3"]
      prefix <- "" : map (\flex -> "[" ++ T.unpack (getPartName flex) ++ "] ") extraNames
      return $ prefix ++ trkName
    return $ Song tempos mmap OnyxFile{..}
  showMIDITracks (Song tempos mmap OnyxFile{..}) = Song tempos mmap $ concat
    [ showMIDITrack "EVENTS" onyxEvents
    , showMIDITrack "BEAT" onyxBeat
    , showMIDITrack "VENUE" onyxVenue
    , showMIDITrack "MELODY'S ESCAPE" onyxMelodysEscape
    , Map.toList onyxFlexParts >>= \(partName, FlexPart{..}) -> let
      prefix str = "[" ++ T.unpack (getPartName partName) ++ "] " ++ str
      prefixSwitch inst str = if partName == inst then str else prefix str
      in concat
        [ flip showMIDITrack flexPartDrums        $ prefixSwitch FlexDrums "PART DRUMS"
        , flip showMIDITrack flexPartDrums2x      $ prefixSwitch FlexDrums "PART DRUMS_2X"
        , flip showMIDITrack flexFiveButton       $ if flexFiveIsKeys
          then prefixSwitch FlexKeys "PART KEYS"
          else case partName of
            FlexGuitar ->        "PART GUITAR"
            FlexBass   ->        "PART BASS"
            _          -> prefix "PART GUITAR"
        , flip showMIDITrack flexPartRealGuitar   $ case partName of
          FlexGuitar ->        "PART REAL_GUITAR"
          FlexBass   ->        "PART REAL_BASS"
          _          -> prefix "PART REAL_GUITAR"
        , flip showMIDITrack flexPartRealGuitar22 $ case partName of
          FlexGuitar ->        "PART REAL_GUITAR_22"
          FlexBass   ->        "PART REAL_BASS_22"
          _          -> prefix "PART REAL_GUITAR_22"
        , flip showMIDITrack flexPartRealKeysE    $ prefixSwitch FlexKeys "PART REAL_KEYS_E"
        , flip showMIDITrack flexPartRealKeysM    $ prefixSwitch FlexKeys "PART REAL_KEYS_M"
        , flip showMIDITrack flexPartRealKeysH    $ prefixSwitch FlexKeys "PART REAL_KEYS_H"
        , flip showMIDITrack flexPartRealKeysX    $ prefixSwitch FlexKeys "PART REAL_KEYS_X"
        , flip showMIDITrack flexPartKeysAnimLH   $ prefixSwitch FlexKeys "PART KEYS_ANIM_LH"
        , flip showMIDITrack flexPartKeysAnimRH   $ prefixSwitch FlexKeys "PART KEYS_ANIM_RH"
        , flip showMIDITrack flexPartVocals       $ prefixSwitch FlexVocal "PART VOCALS"
        , flip showMIDITrack flexHarm1            $ prefixSwitch FlexVocal "HARM1"
        , flip showMIDITrack flexHarm2            $ prefixSwitch FlexVocal "HARM2"
        , flip showMIDITrack flexHarm3            $ prefixSwitch FlexVocal "HARM3"
        ]
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
    [ gtr  "GTR"    $ discardPS $ flexPartRealGuitar   $ getFlexPart FlexGuitar trks
    , gtr  "GTR22"  $ discardPS $ flexPartRealGuitar22 $ getFlexPart FlexGuitar trks
    , bass "BASS"   $ discardPS $ flexPartRealGuitar   $ getFlexPart FlexBass   trks
    , bass "BASS22" $ discardPS $ flexPartRealGuitar22 $ getFlexPart FlexBass   trks
    ]

data TrackOffset = TrackPad U.Seconds | TrackDrop U.Seconds
  deriving (Eq, Ord, Show)

-- | Copies tracks from the second file into the first, repositioning events by timestamp.
mergeCharts :: TrackOffset -> Song (RawFile U.Beats) -> Song (RawFile U.Beats) -> Song (RawFile U.Beats)
mergeCharts offset base new = let
  newTracks = flip map (rawTracks $ s_tracks new) $ \trk -> let
    name = U.trackName trk
    applyOffset = case offset of
      TrackPad  t -> RTB.delay t
      TrackDrop t -> U.trackDrop t
    removeTrackName = RTB.filter $ \case E.MetaEvent (Meta.TrackName _) -> False; _ -> True
    -- TODO: need to ensure notes don't have 0 length
    in maybe id U.setTrackName name
      $ removeTrackName
      $ U.unapplyTempoTrack (s_tempos base)
      $ applyOffset
      $ U.applyTempoTrack (s_tempos new) trk
  in base { s_tracks = RawFile newTracks }

-- | Adds a given amount of 1 second increments to the start of the MIDI.
padRB3MIDI :: Int -> Song (RB3File U.Beats) -> Song (RB3File U.Beats)
padRB3MIDI 0       song                         = song
padRB3MIDI seconds (Song temps sigs RB3File{..}) = let
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
  padSimple = RTB.delay beats
  padBeat
    = RTB.cons  0 Beat.Bar
    . foldr (.) id (replicate (seconds * 2 - 1) $ RTB.cons 1 Beat.Beat)
    . RTB.delay 1
  in Song temps' sigs' RB3File
    { rb3PartDrums        = padSimple rb3PartDrums
    , rb3PartGuitar       = padSimple rb3PartGuitar
    , rb3PartBass         = padSimple rb3PartBass
    , rb3PartKeys         = padSimple rb3PartKeys
    , rb3PartRealGuitar   = padSimple rb3PartRealGuitar
    , rb3PartRealGuitar22 = padSimple rb3PartRealGuitar22
    , rb3PartRealBass     = padSimple rb3PartRealBass
    , rb3PartRealBass22   = padSimple rb3PartRealBass22
    , rb3PartRealKeysE    = padSimple rb3PartRealKeysE
    , rb3PartRealKeysM    = padSimple rb3PartRealKeysM
    , rb3PartRealKeysH    = padSimple rb3PartRealKeysH
    , rb3PartRealKeysX    = padSimple rb3PartRealKeysX
    , rb3PartKeysAnimLH   = padSimple rb3PartKeysAnimLH
    , rb3PartKeysAnimRH   = padSimple rb3PartKeysAnimRH
    , rb3PartVocals       = padSimple rb3PartVocals
    , rb3Harm1            = padSimple rb3Harm1
    , rb3Harm2            = padSimple rb3Harm2
    , rb3Harm3            = padSimple rb3Harm3
    , rb3Events           = padSimple rb3Events
    , rb3Beat             = if RTB.null rb3Beat then RTB.empty else padBeat rb3Beat
    , rb3Venue            = padSimple rb3Venue
    }

-- | Adds a given amount of 1 second increments to the start of the MIDI.
padPSMIDI :: Int -> Song (PSFile U.Beats) -> Song (PSFile U.Beats)
padPSMIDI 0       song                         = song
padPSMIDI seconds (Song temps sigs PSFile{..}) = let
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
  padSimple = RTB.delay beats
  padBeat = withRB
    $ RTB.cons  0 Beat.Bar
    . foldr (.) id (replicate (seconds * 2 - 1) $ RTB.cons 1 Beat.Beat)
    . RTB.delay 1
  in Song temps' sigs' PSFile
    { psPartDrums        = padSimple psPartDrums
    , psPartDrums2x      = padSimple psPartDrums2x
    , psPartRealDrumsPS  = padSimple psPartRealDrumsPS
    , psPartGuitar       = padSimple psPartGuitar
    , psPartBass         = padSimple psPartBass
    , psPartKeys         = padSimple psPartKeys
    , psPartRhythm       = padSimple psPartRhythm
    , psPartGuitarCoop   = padSimple psPartGuitarCoop
    , psPartRealGuitar   = padSimple psPartRealGuitar
    , psPartRealGuitar22 = padSimple psPartRealGuitar22
    , psPartRealBass     = padSimple psPartRealBass
    , psPartRealBass22   = padSimple psPartRealBass22
    , psPartRealKeysE    = padSimple psPartRealKeysE
    , psPartRealKeysM    = padSimple psPartRealKeysM
    , psPartRealKeysH    = padSimple psPartRealKeysH
    , psPartRealKeysX    = padSimple psPartRealKeysX
    , psPartRealKeysPS_E = padSimple psPartRealKeysPS_E
    , psPartRealKeysPS_M = padSimple psPartRealKeysPS_M
    , psPartRealKeysPS_H = padSimple psPartRealKeysPS_H
    , psPartRealKeysPS_X = padSimple psPartRealKeysPS_X
    , psPartKeysAnimLH   = padSimple psPartKeysAnimLH
    , psPartKeysAnimRH   = padSimple psPartKeysAnimRH
    , psPartVocals       = padSimple psPartVocals
    , psHarm1            = padSimple psHarm1
    , psHarm2            = padSimple psHarm2
    , psHarm3            = padSimple psHarm3
    , psEvents           = padSimple psEvents
    , psBeat             = if RTB.null psBeat then RTB.empty else padBeat psBeat
    , psVenue            = padSimple psVenue
}
