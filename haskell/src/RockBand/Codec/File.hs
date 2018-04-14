{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.Codec.File where

import           Control.Monad                    (forM, forM_, (>=>))
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State        (StateT, get, put, runStateT)
import           Control.Monad.Trans.Writer       (Writer, execWriter, tell)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor.Identity            (Identity)
import           Data.List.Extra                  (nubOrd, partition)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified RockBand.Beat                    as Beat
import           RockBand.Codec
import           RockBand.Codec.Beat
import           RockBand.Codec.Drums
import           RockBand.Codec.Events
import           RockBand.Codec.Five
import           RockBand.Codec.ProGuitar
import           RockBand.Codec.ProKeys
import           RockBand.Codec.Six
import           RockBand.Codec.Venue
import           RockBand.Codec.Vocal
import qualified RockBand.File                    as RBFile
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U

type FileParser m t = StackTraceT (StateT [RTB.T t E.T] m)
type FileBuilder t = Writer [RTB.T t E.T]
type FileCodec m t a = Codec (FileParser m t) (FileBuilder t) a

fileId :: FileCodec (PureLog Identity) t a -> FileCodec (PureLog Identity) t a
fileId = id

class ParseFile f where
  parseFile :: (SendMessage m) => FileCodec m U.Beats (f U.Beats)

fileTrack :: (SendMessage m, ParseTrack trk) => T.Text -> [T.Text] -> FileCodec m U.Beats (trk U.Beats)
fileTrack name otherNames = Codec
  { codecIn = do
    trks <- lift get
    let (match, rest) = partition matchTrack trks
        match' = RBFile.stripTrack $ foldr RTB.merge RTB.empty match
        name' = fromMaybe (T.unpack name) $ U.trackName match'
    lift $ put rest
    inside ("Parsing track: " ++ name') $ do
      (parsedTrk, unrec) <- flip mapStackTraceT (codecIn parseTrack) $ \input -> do
        (errorOrParsed, unrec) <- lift $ runStateT input match'
        case errorOrParsed of
          Left  msgs      -> return $ Left msgs
          Right parsedTrk -> return $ Right (parsedTrk, unrec)
      forM_ (nubOrd $ toList unrec) $ \e -> warn $ "Unrecognized MIDI event: " ++ show e
      return parsedTrk
  , codecOut
    = fmapArg
    $ tell
    . (: [])
    . U.setTrackName (T.unpack name)
    . runMergeTrack
    . execWriter
    . codecOut (forcePure parseTrack)
  } where
    matchTrack trk = case U.trackName trk of
      Nothing -> False
      Just n  -> elem (T.pack n) $ name : otherNames
    forcePure
      :: TrackCodec (PureLog Identity) U.Beats (trk U.Beats)
      -> TrackCodec (PureLog Identity) U.Beats (trk U.Beats)
    forcePure = id

-- | Combined MIDI format for RB2, RB3, PS, CH
data FixedFile t = FixedFile
  { fixedPartDrums        :: DrumTrack t
  , fixedPartDrums2x      :: DrumTrack t
  , fixedPartRealDrumsPS  :: DrumTrack t
  , fixedPartGuitar       :: FiveTrack t
  , fixedPartBass         :: FiveTrack t
  , fixedPartKeys         :: FiveTrack t
  , fixedPartRhythm       :: FiveTrack t
  , fixedPartGuitarCoop   :: FiveTrack t
  , fixedPartRealGuitar   :: ProGuitarTrack t
  , fixedPartRealGuitar22 :: ProGuitarTrack t
  , fixedPartRealBass     :: ProGuitarTrack t
  , fixedPartRealBass22   :: ProGuitarTrack t
  , fixedPartGuitarGHL    :: SixTrack t
  , fixedPartBassGHL      :: SixTrack t
  , fixedPartRealKeysE    :: ProKeysTrack t
  , fixedPartRealKeysM    :: ProKeysTrack t
  , fixedPartRealKeysH    :: ProKeysTrack t
  , fixedPartRealKeysX    :: ProKeysTrack t
  , fixedPartKeysAnimLH   :: ProKeysTrack t
  , fixedPartKeysAnimRH   :: ProKeysTrack t
  , fixedPartVocals       :: VocalTrack t
  -- TODO PS Real Keys
  , fixedHarm1            :: VocalTrack t
  , fixedHarm2            :: VocalTrack t
  , fixedHarm3            :: VocalTrack t
  , fixedEvents           :: EventsTrack t
  , fixedBeat             :: BeatTrack t
  , fixedVenue            :: VenueTrack t
  } deriving (Eq, Ord, Show)

instance TraverseTrack FixedFile where
  traverseTrack fn
    (FixedFile a b c d e f g h i j k l m n o p q r s t u v w x y z aa)
    = FixedFile
      <$> traverseTrack fn a <*> traverseTrack fn b <*> traverseTrack fn c
      <*> traverseTrack fn d <*> traverseTrack fn e <*> traverseTrack fn f
      <*> traverseTrack fn g <*> traverseTrack fn h <*> traverseTrack fn i
      <*> traverseTrack fn j <*> traverseTrack fn k <*> traverseTrack fn l
      <*> traverseTrack fn m <*> traverseTrack fn n <*> traverseTrack fn o
      <*> traverseTrack fn p <*> traverseTrack fn q <*> traverseTrack fn r
      <*> traverseTrack fn s <*> traverseTrack fn t <*> traverseTrack fn u
      <*> traverseTrack fn v <*> traverseTrack fn w <*> traverseTrack fn x
      <*> traverseTrack fn y <*> traverseTrack fn z <*> traverseTrack fn aa

instance ParseFile FixedFile where
  parseFile = do
    fixedPartDrums        <- fixedPartDrums        =. fileTrack "PART DRUMS"          ["PART DRUM"]
    fixedPartDrums2x      <- fixedPartDrums2x      =. fileTrack "PART DRUMS_2X"       []
    fixedPartRealDrumsPS  <- fixedPartRealDrumsPS  =. fileTrack "PART REAL_DRUMS_PS"  []
    fixedPartGuitar       <- fixedPartGuitar       =. fileTrack "PART GUITAR"         ["T1 GEMS"]
    fixedPartBass         <- fixedPartBass         =. fileTrack "PART BASS"           []
    fixedPartKeys         <- fixedPartKeys         =. fileTrack "PART KEYS"           []
    fixedPartRhythm       <- fixedPartRhythm       =. fileTrack "PART RHYTHM"         []
    fixedPartGuitarCoop   <- fixedPartGuitarCoop   =. fileTrack "PART GUITAR COOP"    []
    fixedPartRealGuitar   <- fixedPartRealGuitar   =. fileTrack "PART REAL_GUITAR"    []
    fixedPartRealGuitar22 <- fixedPartRealGuitar22 =. fileTrack "PART REAL_GUITAR_22" []
    fixedPartRealBass     <- fixedPartRealBass     =. fileTrack "PART REAL_BASS"      []
    fixedPartRealBass22   <- fixedPartRealBass22   =. fileTrack "PART REAL_BASS_22"   []
    fixedPartGuitarGHL    <- fixedPartGuitarGHL    =. fileTrack "PART GUITAR GHL"     []
    fixedPartBassGHL      <- fixedPartBassGHL      =. fileTrack "PART BASS GHL"       []
    fixedPartRealKeysE    <- fixedPartRealKeysE    =. fileTrack "PART REAL_KEYS_E"    []
    fixedPartRealKeysM    <- fixedPartRealKeysM    =. fileTrack "PART REAL_KEYS_M"    []
    fixedPartRealKeysH    <- fixedPartRealKeysH    =. fileTrack "PART REAL_KEYS_H"    []
    fixedPartRealKeysX    <- fixedPartRealKeysX    =. fileTrack "PART REAL_KEYS_X"    []
    fixedPartKeysAnimLH   <- fixedPartKeysAnimLH   =. fileTrack "PART KEYS_ANIM_LH"   []
    fixedPartKeysAnimRH   <- fixedPartKeysAnimRH   =. fileTrack "PART KEYS_ANIM_RH"   []
    fixedPartVocals       <- fixedPartVocals       =. fileTrack "PART VOCALS"         []
    fixedHarm1            <- fixedHarm1            =. fileTrack "HARM1"               []
    fixedHarm2            <- fixedHarm2            =. fileTrack "HARM2"               []
    fixedHarm3            <- fixedHarm3            =. fileTrack "HARM3"               []
    fixedEvents           <- fixedEvents           =. fileTrack "EVENTS"              []
    fixedBeat             <- fixedBeat             =. fileTrack "BEAT"                []
    fixedVenue            <- fixedVenue            =. fileTrack "VENUE"               []
    return FixedFile{..}

data OnyxFile t = OnyxFile
  { onyxParts  :: Map.Map RBFile.FlexPartName (OnyxPart t)
  , onyxEvents :: EventsTrack t
  , onyxBeat   :: BeatTrack t
  , onyxVenue  :: VenueTrack t
  } deriving (Eq, Ord, Show)

instance TraverseTrack OnyxFile where
  traverseTrack fn
    (OnyxFile a b c d)
    = OnyxFile
      <$> traverse (traverseTrack fn) a
      <*> traverseTrack fn b <*> traverseTrack fn c <*> traverseTrack fn d

data OnyxPart t = OnyxPart
  { onyxPartDrums        :: DrumTrack t
  , onyxPartDrums2x      :: DrumTrack t
  , onyxPartRealDrumsPS  :: DrumTrack t
  , onyxPartGuitar       :: FiveTrack t -- for guitars
  , onyxPartKeys         :: FiveTrack t -- for keyboards
  , onyxPartSix          :: SixTrack t
  , onyxPartRealGuitar   :: ProGuitarTrack t
  , onyxPartRealGuitar22 :: ProGuitarTrack t
  , onyxPartRealKeysE    :: ProKeysTrack t
  , onyxPartRealKeysM    :: ProKeysTrack t
  , onyxPartRealKeysH    :: ProKeysTrack t
  , onyxPartRealKeysX    :: ProKeysTrack t
  , onyxPartKeysAnimLH   :: ProKeysTrack t
  , onyxPartKeysAnimRH   :: ProKeysTrack t
  , onyxPartVocals       :: VocalTrack t
  , onyxHarm1            :: VocalTrack t
  , onyxHarm2            :: VocalTrack t
  , onyxHarm3            :: VocalTrack t
  } deriving (Eq, Ord, Show)

instance TraverseTrack OnyxPart where
  traverseTrack fn
    (OnyxPart a b c d e f g h i j k l m n o p q r)
    = OnyxPart
      <$> traverseTrack fn a <*> traverseTrack fn b <*> traverseTrack fn c
      <*> traverseTrack fn d <*> traverseTrack fn e <*> traverseTrack fn f
      <*> traverseTrack fn g <*> traverseTrack fn h <*> traverseTrack fn i
      <*> traverseTrack fn j <*> traverseTrack fn k <*> traverseTrack fn l
      <*> traverseTrack fn m <*> traverseTrack fn n <*> traverseTrack fn o
      <*> traverseTrack fn p <*> traverseTrack fn q <*> traverseTrack fn r

parseOnyxPart :: (SendMessage m) => RBFile.FlexPartName -> FileCodec m U.Beats (OnyxPart U.Beats)
parseOnyxPart partName = do
  let names defPair otherPairs = case lookup partName $ defPair : otherPairs of
        Just rawName -> fileTrack rawName $ map adorn $ defPair : otherPairs
        Nothing      -> fileTrack (adorn defPair) (map adorn otherPairs)
      adorn (_, trkName) = "[" <> RBFile.getPartName partName <> "] " <> trkName
  onyxPartDrums        <- onyxPartDrums        =. names (RBFile.FlexDrums, "PART DRUMS") []
  onyxPartDrums2x      <- onyxPartDrums2x      =. names (RBFile.FlexDrums, "PART DRUMS_2X") []
  onyxPartRealDrumsPS  <- onyxPartRealDrumsPS  =. names (RBFile.FlexDrums, "PART REAL_DRUMS_PS") []
  onyxPartGuitar       <- onyxPartGuitar       =. names
    (RBFile.FlexGuitar, "PART GUITAR")
    [ (RBFile.FlexBass, "PART BASS")
    , (RBFile.FlexExtra "rhythm", "PART RHYTHM")
    , (RBFile.FlexExtra "guitar-coop", "PART GUITAR COOP")
    ]
  onyxPartKeys         <- onyxPartKeys         =. names (RBFile.FlexKeys, "PART KEYS") []
  onyxPartSix          <- onyxPartSix          =. names
    (RBFile.FlexGuitar, "PART GUITAR GHL")
    [ (RBFile.FlexBass, "PART BASS GHL") ]
  onyxPartRealGuitar   <- onyxPartRealGuitar   =. names
    (RBFile.FlexGuitar, "PART REAL_GUITAR")
    [ (RBFile.FlexBass, "PART REAL_BASS") ]
  onyxPartRealGuitar22 <- onyxPartRealGuitar22 =. names
    (RBFile.FlexGuitar, "PART REAL_GUITAR_22")
    [ (RBFile.FlexBass, "PART REAL_BASS_22") ]
  onyxPartRealKeysE    <- onyxPartRealKeysE    =. names (RBFile.FlexKeys, "PART REAL_KEYS_E") []
  onyxPartRealKeysM    <- onyxPartRealKeysM    =. names (RBFile.FlexKeys, "PART REAL_KEYS_M") []
  onyxPartRealKeysH    <- onyxPartRealKeysH    =. names (RBFile.FlexKeys, "PART REAL_KEYS_H") []
  onyxPartRealKeysX    <- onyxPartRealKeysX    =. names (RBFile.FlexKeys, "PART REAL_KEYS_X") []
  onyxPartKeysAnimLH   <- onyxPartKeysAnimLH   =. names (RBFile.FlexKeys, "PART KEYS_ANIM_LH") []
  onyxPartKeysAnimRH   <- onyxPartKeysAnimRH   =. names (RBFile.FlexKeys, "PART KEYS_ANIM_RH") []
  onyxPartVocals       <- onyxPartVocals       =. names (RBFile.FlexVocal, "PART VOCALS") []
  onyxHarm1            <- onyxHarm1            =. names (RBFile.FlexVocal, "HARM1") []
  onyxHarm2            <- onyxHarm2            =. names (RBFile.FlexVocal, "HARM2") []
  onyxHarm3            <- onyxHarm3            =. names (RBFile.FlexVocal, "HARM3") []
  return OnyxPart{..}

instance ParseFile OnyxFile where
  parseFile = do
    onyxParts  <- onyxParts =. Codec
      { codecIn = do
        trks <- lift get
        let partNames = nubOrd $ mapMaybe (U.trackName >=> RBFile.identifyFlexTrack) trks
        results <- forM partNames $ \partName -> do
          part <- codecIn $ parseOnyxPart partName
          return (partName, part)
        return $ Map.fromList results
      , codecOut = fmapArg $ \parts -> forM_ (Map.toAscList parts) $ \(partName, trk) ->
        codecOut (fileId $ parseOnyxPart partName) trk
      }
    onyxEvents <- onyxEvents =. fileTrack "EVENTS" []
    onyxBeat   <- onyxBeat   =. fileTrack "BEAT"   []
    onyxVenue  <- onyxVenue  =. fileTrack "VENUE"  []
    return OnyxFile{..}

instance ParseFile RBFile.RawFile where
  parseFile = Codec
    { codecIn = lift $ do
      trks <- get
      put []
      return $ RBFile.RawFile trks
    , codecOut = fmapArg $ tell . RBFile.rawTracks
    }

readMIDIFile' :: (SendMessage m, ParseFile f) => F.T -> StackTraceT m (RBFile.Song (f U.Beats))
readMIDIFile' mid = do
  RBFile.Song tempos mmap trks <- RBFile.readMIDIFile mid
  (file, _unrec) <- flip mapStackTraceT (codecIn parseFile) $ \f -> do
    flip fmap (runStateT f trks) $ \case
      (Left  err , _    ) -> Left  err
      (Right file, unrec) -> Right (file, unrec)
  return $ RBFile.Song tempos mmap file

showMIDIFile' :: (ParseFile f) => RBFile.Song (f U.Beats) -> F.T
showMIDIFile' (RBFile.Song tempos mmap trks)
  = RBFile.showMIDIFile $ RBFile.Song tempos mmap $ execWriter $ codecOut (fileId parseFile) trks

-- | Adds a given amount of 1 second increments to the start of the MIDI.
padFixedFile :: Int -> RBFile.Song (FixedFile U.Beats) -> RBFile.Song (FixedFile U.Beats)
padFixedFile 0       song                        = song
padFixedFile seconds (RBFile.Song temps sigs ff) = let
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
  in RBFile.Song temps' sigs' $ (mapTrack padSimple ff)
    { fixedBeat = BeatTrack $ if RTB.null $ beatLines $ fixedBeat ff
      then RTB.empty
      else padBeat $ beatLines $ fixedBeat ff
    }
