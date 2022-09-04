{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module RockBand.Codec.File where

import           Amplitude.Track
import           Control.Monad                     (forM, forM_, guard, unless,
                                                    (>=>))
import           Control.Monad.Codec
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State.Strict  (StateT, execState, get, put,
                                                    runStateT)
import           Control.Monad.Trans.Writer.Strict (Writer, execWriter, tell)
import           Data.DTA.Serialize.Magma          (Percussion)
import qualified Data.EventList.Relative.TimeBody  as RTB
import           Data.Foldable                     (find, toList)
import           Data.Functor                      (void)
import           Data.Functor.Identity             (Identity)
import           Data.Hashable                     (Hashable (..))
import           Data.List.Extra                   (nubOrd, partition, sortOn)
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                as NE
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromJust,
                                                    fromMaybe, isJust,
                                                    isNothing, listToMaybe,
                                                    mapMaybe)
import           Data.SimpleHandle                 (Readable (..), fileReadable,
                                                    handleToByteString,
                                                    useHandle)
import qualified Data.Text                         as T
import           DeriveHelpers
import           Development.Shake                 (Action, need)
import           GHC.Generics                      (Generic)
import           Guitars                           (HOPOsAlgorithm (..),
                                                    noExtendedSustains',
                                                    standardBlipThreshold,
                                                    standardSustainGap)
import           MelodysEscape                     (MelodyTrack)
import qualified Numeric.NonNegative.Class         as NNC
import           PhaseShift.Dance
import           PhaseShift.Message
import           RockBand.Codec
import           RockBand.Codec.Beat
import           RockBand.Codec.Drums
import           RockBand.Codec.Events
import qualified RockBand.Codec.Five               as Five
import           RockBand.Codec.Five
import           RockBand.Codec.FullDrums          (FullDrumTrack)
import           RockBand.Codec.Lipsync
import           RockBand.Codec.ProGuitar
import           RockBand.Codec.ProKeys
import           RockBand.Codec.Six
import           RockBand.Codec.Venue
import           RockBand.Codec.VenueGen
import           RockBand.Codec.Vocal
import           RockBand.Common
import           Rocksmith.MIDI
import qualified Sound.MIDI.File                   as F
import qualified Sound.MIDI.File.Event             as E
import qualified Sound.MIDI.File.Event.Meta        as Meta
import           Sound.MIDI.File.FastParse         (getMIDI)
import qualified Sound.MIDI.File.Save              as Save
import qualified Sound.MIDI.Util                   as U
import           STFS.Package                      (runGetM)

type FileParser m t = StackTraceT (StateT [RTB.T t E.T] m)
type FileBuilder t = Writer [RTB.T t E.T]
type FileCodec m t a = Codec (FileParser m t) (FileBuilder t) a

fileId :: FileCodec (PureLog Identity) t a -> FileCodec (PureLog Identity) t a
fileId = id

class ParseFile f where
  parseFile :: (SendMessage m) => FileCodec m U.Beats (f U.Beats)

parseTrackReport :: (SendMessage m, ParseTrack trk) => RTB.T U.Beats E.T -> StackTraceT m (trk U.Beats)
parseTrackReport trk = do
  (parsedTrk, unrec) <- flip mapStackTraceT (codecIn parseTrack) $ \input -> do
    (errorOrParsed, unrec) <- runStateT input
      $ fixZeroLengthNotes (1/480 :: U.Beats)
      $ mapMIDITrack RTB.fromAbsoluteEventList
      $ getMIDITrack $ RTB.toAbsoluteEventList 0 trk
    case errorOrParsed of
      Left  msgs      -> return $ Left msgs
      Right parsedTrk -> return $ Right (parsedTrk, unrec)
  let unrec' = mapMIDITrack toList unrec
  forM_ (nubOrd $ putMIDITrack (++) unrec') $ \e -> warn $ "Unrecognized MIDI event: " ++ show e
  return parsedTrk

fileTrack :: (SendMessage m, ParseTrack trk) => NonEmpty T.Text -> FileCodec m U.Beats (trk U.Beats)
fileTrack (name :| otherNames) = Codec
  { codecIn = do
    trks <- lift get
    let (match, rest) = partition matchTrack trks
        match' = stripTrack $ foldr RTB.merge RTB.empty match
        name' = fromMaybe (T.unpack name) $ U.trackName match'
    lift $ put rest
    inside ("Parsing track: " ++ name') $ parseTrackReport match'
  , codecOut = fmapArg $ \trk -> let
    evs = (`execState` RTB.empty) $ codecOut (forcePure parseTrack) trk
    in unless (RTB.null evs) $ tell [U.setTrackName (T.unpack name) evs]
  } where
    matchTrack trk = case U.trackName trk of
      Nothing -> False
      Just n  -> elem (T.pack n) $ name : otherNames
    forcePure
      :: TrackCodec (PureLog Identity) U.Beats (trk U.Beats)
      -> TrackCodec (PureLog Identity) U.Beats (trk U.Beats)
    forcePure = id

class HasEvents f where
  getEventsTrack :: f t -> EventsTrack t
  getBeatTrack :: f t -> BeatTrack t

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
  , fixedPartDance        :: DanceTrack t
  -- TODO PS Real Keys
  , fixedHarm1            :: VocalTrack t
  , fixedHarm2            :: VocalTrack t
  , fixedHarm3            :: VocalTrack t
  , fixedLipsync1         :: LipsyncTrack t
  , fixedLipsync2         :: LipsyncTrack t
  , fixedLipsync3         :: LipsyncTrack t
  , fixedLipsync4         :: LipsyncTrack t
  , fixedLipsyncJohn      :: LipsyncTrack t
  , fixedLipsyncPaul      :: LipsyncTrack t
  , fixedLipsyncGeorge    :: LipsyncTrack t
  , fixedLipsyncRingo     :: LipsyncTrack t
  , fixedEvents           :: EventsTrack t
  , fixedBeat             :: BeatTrack t
  , fixedVenue            :: VenueTrack t
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (FixedFile t)

instance HasEvents FixedFile where
  getEventsTrack = fixedEvents
  getBeatTrack = fixedBeat

instance TraverseTrack FixedFile where
  traverseTrack fn
    (FixedFile a b c d e f g h i j k l m n o p q r s t u v w x y z aa bb cc dd ee ff gg hh ii jj)
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
      <*> traverseTrack fn bb
      <*> traverseTrack fn cc
      <*> traverseTrack fn dd
      <*> traverseTrack fn ee
      <*> traverseTrack fn ff
      <*> traverseTrack fn gg
      <*> traverseTrack fn hh
      <*> traverseTrack fn ii
      <*> traverseTrack fn jj

instance ParseFile FixedFile where
  parseFile = do
    fixedPartDrums        <- fixedPartDrums        =. fileTrack ("PART DRUMS"          :| ["PART DRUM"])
    fixedPartDrums2x      <- fixedPartDrums2x      =. fileTrack ("PART DRUMS_2X"       :| [])
    fixedPartRealDrumsPS  <- fixedPartRealDrumsPS  =. fileTrack ("PART REAL_DRUMS_PS"  :| [])
    fixedPartGuitar       <- fixedPartGuitar       =. fileTrack ("PART GUITAR"         :| ["T1 GEMS", "Click"])
    fixedPartBass         <- fixedPartBass         =. fileTrack ("PART BASS"           :| [])
    fixedPartKeys         <- fixedPartKeys         =. fileTrack ("PART KEYS"           :| [])
    fixedPartRhythm       <- fixedPartRhythm       =. fileTrack ("PART RHYTHM"         :| [])
    fixedPartGuitarCoop   <- fixedPartGuitarCoop   =. fileTrack ("PART GUITAR COOP"    :| [])
    fixedPartRealGuitar   <- fixedPartRealGuitar   =. fileTrack ("PART REAL_GUITAR"    :| [])
    fixedPartRealGuitar22 <- fixedPartRealGuitar22 =. fileTrack ("PART REAL_GUITAR_22" :| [])
    fixedPartRealBass     <- fixedPartRealBass     =. fileTrack ("PART REAL_BASS"      :| [])
    fixedPartRealBass22   <- fixedPartRealBass22   =. fileTrack ("PART REAL_BASS_22"   :| [])
    fixedPartGuitarGHL    <- fixedPartGuitarGHL    =. fileTrack ("PART GUITAR GHL"     :| [])
    fixedPartBassGHL      <- fixedPartBassGHL      =. fileTrack ("PART BASS GHL"       :| [])
    fixedPartRealKeysE    <- fixedPartRealKeysE    =. fileTrack ("PART REAL_KEYS_E"    :| [])
    fixedPartRealKeysM    <- fixedPartRealKeysM    =. fileTrack ("PART REAL_KEYS_M"    :| [])
    fixedPartRealKeysH    <- fixedPartRealKeysH    =. fileTrack ("PART REAL_KEYS_H"    :| [])
    fixedPartRealKeysX    <- fixedPartRealKeysX    =. fileTrack ("PART REAL_KEYS_X"    :| [])
    fixedPartKeysAnimLH   <- fixedPartKeysAnimLH   =. fileTrack ("PART KEYS_ANIM_LH"   :| [])
    fixedPartKeysAnimRH   <- fixedPartKeysAnimRH   =. fileTrack ("PART KEYS_ANIM_RH"   :| [])
    fixedPartVocals       <- fixedPartVocals       =. fileTrack ("PART VOCALS"         :| [])
    fixedPartDance        <- fixedPartDance        =. fileTrack ("PART DANCE"          :| [])
    fixedHarm1            <- fixedHarm1            =. fileTrack ("HARM1"               :| ["PART HARM1"]) -- PART is used in TBRB
    fixedHarm2            <- fixedHarm2            =. fileTrack ("HARM2"               :| ["PART HARM2"])
    fixedHarm3            <- fixedHarm3            =. fileTrack ("HARM3"               :| ["PART HARM3"])
    fixedLipsync1         <- fixedLipsync1         =. fileTrack ("LIPSYNC1"            :| [])
    fixedLipsync2         <- fixedLipsync2         =. fileTrack ("LIPSYNC2"            :| [])
    fixedLipsync3         <- fixedLipsync3         =. fileTrack ("LIPSYNC3"            :| [])
    fixedLipsync4         <- fixedLipsync4         =. fileTrack ("LIPSYNC4"            :| [])
    fixedLipsyncJohn      <- fixedLipsyncJohn      =. fileTrack ("LIPSYNC_JOHN"        :| [])
    fixedLipsyncPaul      <- fixedLipsyncPaul      =. fileTrack ("LIPSYNC_PAUL"        :| [])
    fixedLipsyncGeorge    <- fixedLipsyncGeorge    =. fileTrack ("LIPSYNC_GEORGE"      :| [])
    fixedLipsyncRingo     <- fixedLipsyncRingo     =. fileTrack ("LIPSYNC_RINGO"       :| [])
    fixedEvents           <- fixedEvents           =. fileTrack ("EVENTS"              :| [])
    fixedBeat             <- fixedBeat             =. fileTrack ("BEAT"                :| [])
    fixedVenue            <- fixedVenue            =. fileTrack ("VENUE"               :| [])
    return FixedFile{..}

data FlexPartName
  = FlexGuitar
  | FlexBass
  | FlexDrums
  | FlexKeys
  | FlexVocal
  | FlexExtra T.Text
  deriving (Eq, Ord, Show)

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

data OnyxFile t = OnyxFile
  { onyxParts    :: Map.Map FlexPartName (OnyxPart t)
  , onyxEvents   :: EventsTrack t
  , onyxBeat     :: BeatTrack t
  , onyxVenue    :: VenueTrack t
  , onyxLighting :: LightingTrack t
  , onyxCamera   :: CameraTrack t
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (OnyxFile t)

instance HasEvents OnyxFile where
  getEventsTrack = onyxEvents
  getBeatTrack = onyxBeat

instance TraverseTrack OnyxFile where
  traverseTrack fn
    (OnyxFile a b c d e f)
    = OnyxFile
      <$> traverse (traverseTrack fn) a
      <*> traverseTrack fn b <*> traverseTrack fn c
      <*> traverseTrack fn d <*> traverseTrack fn e
      <*> traverseTrack fn f

data OnyxPart t = OnyxPart
  { onyxPartDrums        :: DrumTrack t
  , onyxPartDrums2x      :: DrumTrack t
  , onyxPartRealDrumsPS  :: DrumTrack t
  , onyxPartFullDrums    :: FullDrumTrack t
  , onyxPartGuitar       :: FiveTrack t
  , onyxPartKeys         :: FiveTrack t
  , onyxPartGuitarExt    :: FiveTrack t
  , onyxPartSix          :: SixTrack t
  , onyxPartRealGuitar   :: ProGuitarTrack t
  , onyxPartRealGuitar22 :: ProGuitarTrack t
  , onyxPartRSGuitar     :: RocksmithTrack t
  , onyxPartRSBass       :: RocksmithTrack t
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
  , onyxCatch            :: CatchTrack t
  , onyxLipsync1         :: LipsyncTrack t
  , onyxLipsync2         :: LipsyncTrack t
  , onyxLipsync3         :: LipsyncTrack t
  , onyxLipsync4         :: LipsyncTrack t
  , onyxMelody           :: MelodyTrack t
  , onyxPartDance        :: DanceTrack t
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (OnyxPart t)

editOnyxPart :: (NNC.C t) => FlexPartName -> (OnyxPart t -> OnyxPart t) -> OnyxFile t -> OnyxFile t
editOnyxPart pname edit onyx = onyx
  { onyxParts = Map.alter
    (Just . edit . fromMaybe mempty)
    pname
    (onyxParts onyx)
  }

data FiveType
  = FiveTypeGuitar
  | FiveTypeKeys
  | FiveTypeGuitarExt
  deriving (Eq, Show)

selectGuitarTrack :: (NNC.C t) => FiveType -> OnyxPart t -> (FiveTrack t, HOPOsAlgorithm)
selectGuitarTrack typ part = let
  gtr  = (onyxPartGuitar    part, HOPOsRBGuitar)
  keys = (onyxPartKeys      part, HOPOsRBKeys  )
  ext  = (onyxPartGuitarExt part, HOPOsRBGuitar)
  trks = case typ of
    FiveTypeGuitar    -> [gtr, ext, keys]
    FiveTypeKeys      -> [keys, ext, gtr] -- prefer ext due to sustains? or gtr due to no opens? dunno
    FiveTypeGuitarExt -> [ext, gtr, keys]
  in fromMaybe (mempty, HOPOsRBGuitar) $ find (not . nullFive . fst) trks
  -- TODO maybe fill in lower difficulties from secondary tracks

instance TraverseTrack OnyxPart where
  traverseTrack fn
    (OnyxPart a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac)
    = OnyxPart
      <$> traverseTrack fn a <*> traverseTrack fn b <*> traverseTrack fn c
      <*> traverseTrack fn d <*> traverseTrack fn e <*> traverseTrack fn f
      <*> traverseTrack fn g <*> traverseTrack fn h <*> traverseTrack fn i
      <*> traverseTrack fn j <*> traverseTrack fn k <*> traverseTrack fn l
      <*> traverseTrack fn m <*> traverseTrack fn n <*> traverseTrack fn o
      <*> traverseTrack fn p <*> traverseTrack fn q <*> traverseTrack fn r
      <*> traverseTrack fn s <*> traverseTrack fn t <*> traverseTrack fn u
      <*> traverseTrack fn v <*> traverseTrack fn w <*> traverseTrack fn x
      <*> traverseTrack fn y <*> traverseTrack fn z <*> traverseTrack fn aa
      <*> traverseTrack fn ab <*> traverseTrack fn ac

getFlexPart :: (NNC.C t) => FlexPartName -> OnyxFile t -> OnyxPart t
getFlexPart part = fromMaybe mempty . Map.lookup part . onyxParts

identifyFlexTrack :: T.Text -> Maybe FlexPartName
identifyFlexTrack name = case T.stripPrefix "[" name of
  Just name' -> Just $ readPartName $ T.takeWhile (/= ']') name'
  Nothing
    | "RHYTHM"      `T.isInfixOf` name -> Just $ FlexExtra "rhythm"
    | "GUITAR COOP" `T.isInfixOf` name -> Just $ FlexExtra "guitar-coop"
    | "DRUM"        `T.isInfixOf` name -> Just FlexDrums
    | "GUITAR"      `T.isInfixOf` name -> Just FlexGuitar
    | "LEAD"        `T.isInfixOf` name -> Just FlexGuitar
    | "T1 GEMS"     `T.isInfixOf` name -> Just FlexGuitar
    | "Click"       `T.isInfixOf` name -> Just FlexGuitar
    | "BASS"        `T.isInfixOf` name -> Just FlexBass
    | "KEYS"        `T.isInfixOf` name -> Just FlexKeys
    | "VOCAL"       `T.isInfixOf` name -> Just FlexVocal
    | "HARM"        `T.isInfixOf` name -> Just FlexVocal
    | "MELODY"      `T.isInfixOf` name -> Just $ FlexExtra "global"
    | "KONGA"       `T.isInfixOf` name -> Just $ FlexExtra "global"
    | "DANCE"       `T.isInfixOf` name -> Just $ FlexExtra "global"
    | otherwise                        -> Nothing

parseOnyxPart :: (SendMessage m) => FlexPartName -> FileCodec m U.Beats (OnyxPart U.Beats)
parseOnyxPart partName = do
  let names pairs = let
        rawNames = fmap snd $ NE.filter ((== partName) . fst) pairs
        in fileTrack $ foldr NE.cons (fmap adorn pairs) rawNames
      adorn (_, trkName) = "[" <> getPartName partName <> "] " <> trkName
  onyxPartDrums        <- onyxPartDrums        =. names (pure (FlexDrums, "PART DRUMS"))
  onyxPartDrums2x      <- onyxPartDrums2x      =. names (pure (FlexDrums, "PART DRUMS_2X"))
  onyxPartRealDrumsPS  <- onyxPartRealDrumsPS  =. names (pure (FlexDrums, "PART REAL_DRUMS_PS"))
  onyxPartFullDrums    <- onyxPartFullDrums    =. names (pure (FlexDrums, "PART FULL DRUMS"))
  onyxPartGuitar       <- onyxPartGuitar       =. names
    ( (FlexGuitar, "PART GUITAR") :|
    [ (FlexBass, "PART BASS")
    , (FlexExtra "rhythm", "PART RHYTHM")
    , (FlexExtra "guitar-coop", "PART GUITAR COOP")
    ])
  onyxPartKeys         <- onyxPartKeys         =. names (pure (FlexKeys, "PART KEYS"))
  onyxPartGuitarExt    <- onyxPartGuitarExt    =. names
    ( (FlexGuitar, "PART GUITAR EXT") :|
    [ (FlexBass, "PART BASS EXT")
    , (FlexExtra "rhythm", "PART RHYTHM EXT")
    , (FlexExtra "guitar-coop", "PART GUITAR COOP EXT")
    ])
  onyxPartSix          <- onyxPartSix          =. names
    ((FlexGuitar, "PART GUITAR GHL") :|
    [ (FlexBass, "PART BASS GHL") ])
  onyxPartRealGuitar   <- onyxPartRealGuitar   =. names
    ( (FlexGuitar, "PART REAL_GUITAR") :|
    [ (FlexBass, "PART REAL_BASS") ])
  onyxPartRealGuitar22 <- onyxPartRealGuitar22 =. names
    ( (FlexGuitar, "PART REAL_GUITAR_22") :|
    [ (FlexBass, "PART REAL_BASS_22") ])
  onyxPartRSGuitar <- onyxPartRSGuitar =. names
    ( (FlexGuitar, "PART RS LEAD") :|
    [ (FlexExtra "rhythm", "PART RS RHYTHM") ])
  onyxPartRSBass <- onyxPartRSBass =. names (pure (FlexBass, "PART RS BASS"))
  onyxPartRealKeysE    <- onyxPartRealKeysE    =. names (pure (FlexKeys, "PART REAL_KEYS_E"))
  onyxPartRealKeysM    <- onyxPartRealKeysM    =. names (pure (FlexKeys, "PART REAL_KEYS_M"))
  onyxPartRealKeysH    <- onyxPartRealKeysH    =. names (pure (FlexKeys, "PART REAL_KEYS_H"))
  onyxPartRealKeysX    <- onyxPartRealKeysX    =. names (pure (FlexKeys, "PART REAL_KEYS_X"))
  onyxPartKeysAnimLH   <- onyxPartKeysAnimLH   =. names (pure (FlexKeys, "PART KEYS_ANIM_LH"))
  onyxPartKeysAnimRH   <- onyxPartKeysAnimRH   =. names (pure (FlexKeys, "PART KEYS_ANIM_RH"))
  onyxPartVocals       <- onyxPartVocals       =. names (pure (FlexVocal, "PART VOCALS"))
  onyxHarm1            <- onyxHarm1            =. names ((FlexVocal, "HARM1") :| [(FlexVocal, "PART HARM1")])
  onyxHarm2            <- onyxHarm2            =. names ((FlexVocal, "HARM2") :| [(FlexVocal, "PART HARM2")])
  onyxHarm3            <- onyxHarm3            =. names ((FlexVocal, "HARM3") :| [(FlexVocal, "PART HARM3")])
  onyxCatch            <- onyxCatch            =. names (pure (FlexExtra "undefined", "CATCH"))
  onyxMelody           <- onyxMelody           =. names (pure (FlexExtra "global", "MELODY'S ESCAPE"))
  onyxLipsync1         <- onyxLipsync1         =. names (pure (FlexVocal, "LIPSYNC1"))
  onyxLipsync2         <- onyxLipsync2         =. names (pure (FlexVocal, "LIPSYNC2"))
  onyxLipsync3         <- onyxLipsync3         =. names (pure (FlexVocal, "LIPSYNC3"))
  onyxLipsync4         <- onyxLipsync4         =. names (pure (FlexVocal, "LIPSYNC4"))
  onyxPartDance        <- onyxPartDance        =. names (pure (FlexExtra "global", "PART DANCE"))
  return OnyxPart{..}

instance ParseFile OnyxFile where
  parseFile = do
    onyxParts    <- onyxParts =. Codec
      { codecIn = do
        trks <- lift get
        let partNames = nubOrd $ mapMaybe (U.trackName >=> identifyFlexTrack . T.pack) trks
        results <- forM partNames $ \partName -> do
          part <- codecIn $ parseOnyxPart partName
          return (partName, part)
        return $ Map.fromList results
      , codecOut = fmapArg $ \parts -> forM_ (Map.toAscList parts) $ \(partName, trk) ->
        codecOut (fileId $ parseOnyxPart partName) trk
      }
    onyxEvents   <- onyxEvents   =. fileTrack (pure "EVENTS"  )
    onyxBeat     <- onyxBeat     =. fileTrack (pure "BEAT"    )
    onyxVenue    <- onyxVenue    =. fileTrack (pure "VENUE"   )
    onyxLighting <- onyxLighting =. fileTrack (pure "LIGHTING")
    onyxCamera   <- onyxCamera   =. fileTrack (pure "CAMERA"  )
    return OnyxFile{..}

newtype RawFile t = RawFile { rawTracks :: [RTB.T t E.T] }
  deriving (Eq, Ord, Show)

instance ParseFile RawFile where
  parseFile = Codec
    { codecIn = lift $ do
      trks <- get
      put []
      return $ RawFile trks
    , codecOut = fmapArg $ tell . rawTracks
    }

data Song t = Song
  { s_tempos     :: U.TempoMap
  , s_signatures :: U.MeasureMap
  , s_tracks     :: t
  } deriving (Eq, Show, Functor, Foldable, Traversable)

{- |
This is a hack because Moonscraper and thus Clone Hero currently don't
parse the PS tap event properly; they look for 255 in the difficulty byte,
and don't actually look for the phrase ID of 4. So we need to only emit
255 (all-difficulty) tap phrases.
(This same weirdness applies to the open note modifier, where it looks for
a non-255 byte and not the phrase ID of 1, but we happen to emit those in
the working format already.)
-}
fixMoonTaps :: (NNC.C t) => RTB.T t E.T -> RTB.T t E.T
fixMoonTaps = RTB.mapMaybe $ \e -> case parsePSSysEx e of
  Just (PSMessage diff TapNotes b) -> case diff of
    Nothing     -> Just $ unparsePSSysEx $ PSMessage Nothing TapNotes b
    Just Expert -> Just $ unparsePSSysEx $ PSMessage Nothing TapNotes b
    Just _      -> Nothing
  _ -> Just e

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

readMIDIFile :: (SendMessage m) => F.T -> StackTraceT m (Song [RTB.T U.Beats E.T])
readMIDIFile mid = do
  (s_tempos, s_signatures, s_tracks_nodupe) <- case U.decodeFile mid of
    Right trks -> let
      tempos = U.tempoMapFromBPS $ RTB.singleton 0 2
      sigs = U.measureMapFromTimeSigs U.Truncate $ RTB.singleton 0 $ U.TimeSig 4 1
      trks' = map (U.unapplyTempoTrack tempos) trks
      in do
        warn "Converting from SMPTE MIDI file. This is not tested, please report bugs!"
        return (tempos, sigs, trks')
    Left trks -> do
      (tempoTrk, restTrks) <- case mid of
        F.Cons F.Mixed _ _ -> do
          -- hack for very old FoF charts that used this midi format
          warn "Loading 'mixed' (type-0) MIDI file as single guitar track."
          let t = case trks of trk : _ -> trk; [] -> RTB.empty
          return (t, [U.setTrackName "PART GUITAR" t])
        _ -> return $ case trks of
          t : ts -> (t, ts)
          []     -> (RTB.empty, [])
      return (U.makeTempoMap tempoTrk, U.makeMeasureMap U.Truncate tempoTrk, restTrks)
  let s_tracks = map (RTB.flatten . fmap nubOrd . RTB.collectCoincident) s_tracks_nodupe
  return Song{..}

-- | Used for reading midis we expect to be type-0, for Rock Revolution
readMixedMIDI :: (Monad m) => F.T -> StackTraceT m (Song (RTB.T U.Beats E.T))
readMixedMIDI mid = case mid of
  F.Cons F.Mixed _ _ -> case U.decodeFile mid of
    Left [trk] -> let
      s_tempos = U.makeTempoMap trk
      s_signatures = U.makeMeasureMap U.Truncate trk
      s_tracks = flip RTB.filter trk $ \case
        -- various events we don't need to look at
        E.MetaEvent Meta.TrackName{}      -> False
        E.MetaEvent Meta.SetTempo{}       -> False
        E.MetaEvent Meta.TimeSig{}        -> False
        E.MetaEvent Meta.SMPTEOffset{}    -> False
        E.MetaEvent Meta.KeySig{}         -> False
        E.MetaEvent Meta.InstrumentName{} -> False
        _                                 -> True
      in return Song{..}
    _ -> fatal "Not exactly 1 track in 'mixed' (type-0) MIDI file"
  F.Cons typ _ _ -> fatal $ "Expected a 'mixed' (type-0) MIDI file but found: " <> show typ

-- | Strips comments and track names from the track before handing it to a track parser.
stripTrack :: (NNC.C t) => RTB.T t E.T -> RTB.T t E.T
stripTrack = RTB.filter $ \e -> case e of
  E.MetaEvent (Meta.TextEvent ('#' : _)) -> False
  E.MetaEvent (Meta.TrackName _        ) -> False
  _                                      -> True

data TrackOffset = TrackPad U.Seconds | TrackDrop U.Seconds
  deriving (Eq, Ord, Show)

-- | Copies tracks from the second file into the first, repositioning events by timestamp.
mergeCharts :: TrackOffset -> Song (RawFile U.Beats) -> Song (RawFile U.Beats) -> Song (RawFile U.Beats)
mergeCharts offset base new = let
  newTracks = flip map (rawTracks $ s_tracks new) $ \trk -> let
    name = U.trackName trk
    applyOffset = case offset of
      TrackPad  t -> RTB.delay t
      TrackDrop t -> U.trackDrop t -- TODO should this instead shove events forward?
    removeTrackName = RTB.filter $ \case E.MetaEvent (Meta.TrackName _) -> False; _ -> True
    -- TODO: need to ensure notes don't have 0 length
    in maybe id U.setTrackName name
      $ removeTrackName
      $ U.unapplyTempoTrack (s_tempos base)
      $ applyOffset
      $ U.applyTempoTrack (s_tempos new) trk
  in base { s_tracks = RawFile newTracks }

parseTracks :: (SendMessage m, ParseTrack trk) => [RTB.T U.Beats E.T] -> T.Text -> StackTraceT m (trk U.Beats)
parseTracks trks name = do
  (file, _unrec) <- flip mapStackTraceT (codecIn $ fileTrack $ pure name) $ \f -> do
    flip fmap (runStateT f trks) $ \case
      (Left  err , _    ) -> Left  err
      (Right file, unrec) -> Right (file, unrec)
  return file

interpretMIDIFile
  :: (SendMessage m, ParseFile f)
  => Song [RTB.T U.Beats E.T]
  -> StackTraceT m (Song (f U.Beats))
interpretMIDIFile (Song tempos mmap trks) = do
  (file, unrec) <- flip mapStackTraceT (codecIn parseFile) $ \f -> do
    flip fmap (runStateT f trks) $ \case
      (Left  err , _    ) -> Left  err
      (Right file, unrec) -> Right (file, unrec)
  let mnames = map U.trackName unrec
  case catMaybes mnames of
    []    -> return ()
    names -> warn $ "Unrecognized MIDI track names: " ++ show names
  case filter isNothing mnames of
    [] -> return ()
    unnamed -> warn $ show (length unnamed) ++ " MIDI track(s) with no track name"
  return $ Song tempos mmap file

readMIDIFile' :: (SendMessage m, ParseFile f) => F.T -> StackTraceT m (Song (f U.Beats))
readMIDIFile' mid = readMIDIFile mid >>= interpretMIDIFile

loadRawMIDI :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m F.T
loadRawMIDI = loadRawMIDIReadable . fileReadable

loadRawMIDIReadable :: (SendMessage m, MonadIO m) => Readable -> StackTraceT m F.T
loadRawMIDIReadable r = do
  maybe id (\f -> inside $ "loading MIDI: " <> f) (rFilePath r) $ do
    bs <- stackIO $ useHandle r handleToByteString
    (mid, warnings) <- runGetM getMIDI bs
    mapM_ warn warnings
    return mid

loadMIDI :: (SendMessage m, MonadIO m, ParseFile f) => FilePath -> StackTraceT m (Song (f U.Beats))
loadMIDI f = loadRawMIDI f >>= readMIDIFile'

loadMIDIReadable :: (SendMessage m, MonadIO m, ParseFile f) => Readable -> StackTraceT m (Song (f U.Beats))
loadMIDIReadable r = loadRawMIDIReadable r >>= readMIDIFile'

showMIDIFile :: Song [RTB.T U.Beats E.T] -> F.T
showMIDIFile s = let
  tempos = U.unmakeTempoMap $ s_tempos s
  sigs = case mapM U.showSignatureFull $ U.measureMapToTimeSigs $ s_signatures s of
    Nothing   -> RTB.singleton 0 $ fromJust $ U.showSignature 4
    Just evts -> evts
  tempoTrk = U.setTrackName "notes" $ RTB.merge tempos sigs
  in U.encodeFileBeats F.Parallel 480 $ map (fixMoonTaps . fixEventOrder) $ tempoTrk : s_tracks s

showMIDITracks :: (ParseFile f) => Song (f U.Beats) -> Song [RTB.T U.Beats E.T]
showMIDITracks (Song tempos mmap trks)
  = Song tempos mmap $ execWriter $ codecOut (fileId parseFile) trks

showMIDIFile' :: (ParseFile f) => Song (f U.Beats) -> F.T
showMIDIFile' = showMIDIFile . showMIDITracks

-- | Adds a given amount of 1 second increments to the start of the MIDI.
padAnyFile :: (TraverseTrack f) => Int -> Song (f U.Beats) -> Song (f U.Beats)
padAnyFile 0       song                 = song
padAnyFile seconds (Song temps sigs ff) = let
  beats = fromIntegral seconds * 2
  temps'
    = U.tempoMapFromBPS
    $ RTB.cons 0 2 -- 120 bpm
    $ RTB.delay beats
    $ U.tempoMapToBPS temps
  -- we only modify the first bar to have the extra time,
  -- so printed positions after that bar are unchanged from the source midi
  sigs' = U.measureMapFromTimeSigs U.Error $ case U.measureMapToTimeSigs sigs of
    Wait 0 sig@(U.TimeSig len unit) rest -> Wait 0 (U.TimeSig (len + beats) unit) $ case rest of
      Wait dt next rest'
        | dt == len -> Wait (dt + beats) next rest'
        | otherwise -> Wait (len + beats) sig $ Wait (dt - len) next rest'
      RNil -> Wait (len + beats) sig RNil
    -- TODO: timesig numerator can only go up to 255 so this could fail for long delay values
    _ -> error "RockBand.Codec.File.padAnyFile: internal error (no time signature at MIDI start)"
  padSimple = RTB.delay beats
  in Song temps' sigs' $ mapTrack padSimple ff

-- | Adds a given amount of 1 second increments to the start of the MIDI.
-- Also puts new beatlines in front of the BEAT track.
padFixedFile :: Int -> Song (FixedFile U.Beats) -> Song (FixedFile U.Beats)
padFixedFile 0       song               = song
padFixedFile seconds song@(Song _ _ ff) = let
  Song temps' sigs' ff' = padAnyFile seconds song
  padBeat
    = RTB.cons  0 Bar
    . foldr (.) id (replicate (seconds * 2 - 1) $ RTB.cons 1 Beat)
    . RTB.delay 1
  in Song temps' sigs' ff'
    { fixedBeat = BeatTrack $ if RTB.null $ beatLines $ fixedBeat ff
      then RTB.empty
      else padBeat $ beatLines $ fixedBeat ff
    }

wiiNoFills :: (NNC.C t) => Song (FixedFile t) -> Song (FixedFile t)
wiiNoFills (Song temps sigs ff) = let
  f drums = case RTB.viewL $ eventsCoda $ fixedEvents ff of
    Nothing           -> drums
      { drumActivation = RTB.empty }
    Just ((dt, _), _) -> drums
      { drumActivation = RTB.delay dt $ U.trackDrop dt $ drumActivation drums }
  in Song temps sigs ff
    { fixedPartDrums       = f $ fixedPartDrums       ff
    , fixedPartDrums2x     = f $ fixedPartDrums2x     ff
    , fixedPartRealDrumsPS = f $ fixedPartRealDrumsPS ff
    }

wiiMustang22 :: Song (FixedFile t) -> Song (FixedFile t)
wiiMustang22 (Song temps sigs ff) = let
  g17 = fixedPartRealGuitar   ff
  g22 = fixedPartRealGuitar22 ff
  b17 = fixedPartRealBass     ff
  b22 = fixedPartRealBass22   ff
  in Song temps sigs ff
    { fixedPartRealGuitar = if all (not . nullPG) [g17, g22] then g22 else g17
    , fixedPartRealBass   = if all (not . nullPG) [b17, b22] then b22 else b17
    }

-- | Unmutes protar notes with a velocity above 22.
wiiUnmute22 :: (NNC.C t) => Song (FixedFile t) -> Song (FixedFile t)
wiiUnmute22 (Song temps sigs ff) = let
  unmuteTrack pg = pg
    { pgDifficulties = fmap unmuteDiff $ pgDifficulties pg
    }
  unmuteDiff diff = diff
    { pgNotes
      = splitEdgesSimple
      $ fmap (\case
        (fret, (str, Muted), len) | fret > 22 -> (fret, (str, NormalNote), len)
        x                                     -> x
      )
      $ joinEdgesSimple
      $ pgNotes diff
    }
  in Song temps sigs ff
    { fixedPartRealGuitar   = unmuteTrack $ fixedPartRealGuitar   ff
    , fixedPartRealGuitar22 = unmuteTrack $ fixedPartRealGuitar22 ff
    , fixedPartRealBass     = unmuteTrack $ fixedPartRealBass     ff
    , fixedPartRealBass22   = unmuteTrack $ fixedPartRealBass22   ff
    }

convertToVenueGen :: Song (OnyxFile U.Beats) -> Song (OnyxFile U.Beats)
convertToVenueGen (Song temps sigs trks) = Song temps sigs trks
  { onyxLighting = onyxLighting trks <> unbuildLighting 1 (onyxVenue trks)
  , onyxCamera   = onyxCamera   trks <> unbuildCamera   1 (onyxVenue trks)
  , onyxVenue = (onyxVenue trks)
    { venueCameraRB3        = RTB.empty
    , venuePostProcessRB3   = RTB.empty
    , venueLighting         = RTB.empty
    , venueLightingCommands = RTB.empty
    , venueBonusFX          = RTB.empty
    , venueBonusFXOptional  = RTB.empty
    }
  }

instance ChopTrack OnyxFile where
  chopTake t o = OnyxFile
    { onyxParts    = chopTake t <$> onyxParts o
    , onyxEvents   = chopTake t $ onyxEvents o
    , onyxBeat     = chopTake t $ onyxBeat o
    , onyxVenue    = mapTrack (U.trackTake t) $ onyxVenue o -- TODO
    , onyxLighting = mapTrack (U.trackTake t) $ onyxLighting o -- TODO
    , onyxCamera   = mapTrack (U.trackTake t) $ onyxCamera o -- TODO
    }
  chopDrop t o = OnyxFile
    { onyxParts    = chopDrop t <$> onyxParts o
    , onyxEvents   = chopDrop t $ onyxEvents o
    , onyxBeat     = chopDrop t $ onyxBeat o
    , onyxVenue    = mapTrack (U.trackDrop t) $ onyxVenue o -- TODO
    , onyxLighting = mapTrack (U.trackDrop t) $ onyxLighting o -- TODO
    , onyxCamera   = mapTrack (U.trackDrop t) $ onyxCamera o -- TODO
    }

instance ChopTrack OnyxPart where
  chopTake t op = OnyxPart
    { onyxPartDrums        = chopTake t               $ onyxPartDrums        op
    , onyxPartDrums2x      = chopTake t               $ onyxPartDrums2x      op
    , onyxPartRealDrumsPS  = chopTake t               $ onyxPartRealDrumsPS  op
    , onyxPartFullDrums    = mapTrack (U.trackTake t) $ onyxPartFullDrums    op -- TODO
    , onyxPartGuitar       = mapTrack (U.trackTake t) $ onyxPartGuitar       op -- TODO
    , onyxPartKeys         = mapTrack (U.trackTake t) $ onyxPartKeys         op -- TODO
    , onyxPartGuitarExt    = mapTrack (U.trackTake t) $ onyxPartGuitarExt    op -- TODO
    , onyxPartSix          = mapTrack (U.trackTake t) $ onyxPartSix          op -- TODO
    , onyxPartRealGuitar   = chopTake t               $ onyxPartRealGuitar   op
    , onyxPartRealGuitar22 = chopTake t               $ onyxPartRealGuitar22 op
    , onyxPartRSGuitar     = mapTrack (U.trackTake t) $ onyxPartRSGuitar     op -- TODO
    , onyxPartRSBass       = mapTrack (U.trackTake t) $ onyxPartRSBass       op -- TODO
    , onyxPartRealKeysE    = mapTrack (U.trackTake t) $ onyxPartRealKeysE    op -- TODO
    , onyxPartRealKeysM    = mapTrack (U.trackTake t) $ onyxPartRealKeysM    op -- TODO
    , onyxPartRealKeysH    = mapTrack (U.trackTake t) $ onyxPartRealKeysH    op -- TODO
    , onyxPartRealKeysX    = mapTrack (U.trackTake t) $ onyxPartRealKeysX    op -- TODO
    , onyxPartKeysAnimLH   = mapTrack (U.trackTake t) $ onyxPartKeysAnimLH   op -- TODO
    , onyxPartKeysAnimRH   = mapTrack (U.trackTake t) $ onyxPartKeysAnimRH   op -- TODO
    , onyxPartVocals       = mapTrack (U.trackTake t) $ onyxPartVocals       op -- TODO
    , onyxHarm1            = mapTrack (U.trackTake t) $ onyxHarm1            op -- TODO
    , onyxHarm2            = mapTrack (U.trackTake t) $ onyxHarm2            op -- TODO
    , onyxHarm3            = mapTrack (U.trackTake t) $ onyxHarm3            op -- TODO
    , onyxCatch            = mapTrack (U.trackTake t) $ onyxCatch            op -- TODO
    , onyxLipsync1         = mapTrack (U.trackTake t) $ onyxLipsync1         op -- TODO
    , onyxLipsync2         = mapTrack (U.trackTake t) $ onyxLipsync2         op -- TODO
    , onyxLipsync3         = mapTrack (U.trackTake t) $ onyxLipsync3         op -- TODO
    , onyxLipsync4         = mapTrack (U.trackTake t) $ onyxLipsync4         op -- TODO
    , onyxMelody           = mapTrack (U.trackTake t) $ onyxMelody           op -- TODO
    , onyxPartDance        = mapTrack (U.trackTake t) $ onyxPartDance        op -- TODO
    }
  chopDrop t op = OnyxPart
    { onyxPartDrums        = chopDrop t               $ onyxPartDrums        op
    , onyxPartDrums2x      = chopDrop t               $ onyxPartDrums2x      op
    , onyxPartRealDrumsPS  = chopDrop t               $ onyxPartRealDrumsPS  op
    , onyxPartFullDrums    = mapTrack (U.trackDrop t) $ onyxPartFullDrums    op -- TODO
    , onyxPartGuitar       = mapTrack (U.trackDrop t) $ onyxPartGuitar       op -- TODO
    , onyxPartKeys         = mapTrack (U.trackDrop t) $ onyxPartKeys         op -- TODO
    , onyxPartGuitarExt    = mapTrack (U.trackDrop t) $ onyxPartGuitarExt    op -- TODO
    , onyxPartSix          = mapTrack (U.trackDrop t) $ onyxPartSix          op -- TODO
    , onyxPartRealGuitar   = chopDrop t               $ onyxPartRealGuitar   op
    , onyxPartRealGuitar22 = chopDrop t               $ onyxPartRealGuitar22 op
    , onyxPartRSGuitar     = mapTrack (U.trackDrop t) $ onyxPartRSGuitar     op -- TODO
    , onyxPartRSBass       = mapTrack (U.trackDrop t) $ onyxPartRSBass       op -- TODO
    , onyxPartRealKeysE    = mapTrack (U.trackDrop t) $ onyxPartRealKeysE    op -- TODO
    , onyxPartRealKeysM    = mapTrack (U.trackDrop t) $ onyxPartRealKeysM    op -- TODO
    , onyxPartRealKeysH    = mapTrack (U.trackDrop t) $ onyxPartRealKeysH    op -- TODO
    , onyxPartRealKeysX    = mapTrack (U.trackDrop t) $ onyxPartRealKeysX    op -- TODO
    , onyxPartKeysAnimLH   = mapTrack (U.trackDrop t) $ onyxPartKeysAnimLH   op -- TODO
    , onyxPartKeysAnimRH   = mapTrack (U.trackDrop t) $ onyxPartKeysAnimRH   op -- TODO
    , onyxPartVocals       = mapTrack (U.trackDrop t) $ onyxPartVocals       op -- TODO
    , onyxHarm1            = mapTrack (U.trackDrop t) $ onyxHarm1            op -- TODO
    , onyxHarm2            = mapTrack (U.trackDrop t) $ onyxHarm2            op -- TODO
    , onyxHarm3            = mapTrack (U.trackDrop t) $ onyxHarm3            op -- TODO
    , onyxCatch            = mapTrack (U.trackDrop t) $ onyxCatch            op -- TODO
    , onyxLipsync1         = mapTrack (U.trackDrop t) $ onyxLipsync1         op -- TODO
    , onyxLipsync2         = mapTrack (U.trackDrop t) $ onyxLipsync2         op -- TODO
    , onyxLipsync3         = mapTrack (U.trackDrop t) $ onyxLipsync3         op -- TODO
    , onyxLipsync4         = mapTrack (U.trackDrop t) $ onyxLipsync4         op -- TODO
    , onyxMelody           = mapTrack (U.trackDrop t) $ onyxMelody           op -- TODO
    , onyxPartDance        = mapTrack (U.trackDrop t) $ onyxPartDance        op -- TODO
    }

onyxToFixed :: OnyxFile U.Beats -> FixedFile U.Beats
onyxToFixed o = FixedFile
  { fixedPartDrums        = inPart FlexDrums                 onyxPartDrums
  , fixedPartDrums2x      = inPart FlexDrums                 onyxPartDrums2x
  , fixedPartRealDrumsPS  = inPart FlexDrums                 onyxPartRealDrumsPS
  , fixedPartGuitar       = inPart FlexGuitar                onyxPartGuitar
  , fixedPartBass         = inPart FlexBass                  onyxPartGuitar
  , fixedPartKeys         = inPart FlexKeys                  onyxPartKeys
  , fixedPartRhythm       = inPart (FlexExtra "rhythm")      onyxPartGuitar
  , fixedPartGuitarCoop   = inPart (FlexExtra "guitar-coop") onyxPartGuitar
  , fixedPartRealGuitar   = inPart FlexGuitar                onyxPartRealGuitar
  , fixedPartRealGuitar22 = inPart FlexGuitar                onyxPartRealGuitar22
  , fixedPartRealBass     = inPart FlexBass                  onyxPartRealGuitar
  , fixedPartRealBass22   = inPart FlexBass                  onyxPartRealGuitar22
  , fixedPartGuitarGHL    = inPart FlexGuitar                onyxPartSix
  , fixedPartBassGHL      = inPart FlexBass                  onyxPartSix
  , fixedPartRealKeysE    = inPart FlexKeys                  onyxPartRealKeysE
  , fixedPartRealKeysM    = inPart FlexKeys                  onyxPartRealKeysM
  , fixedPartRealKeysH    = inPart FlexKeys                  onyxPartRealKeysH
  , fixedPartRealKeysX    = inPart FlexKeys                  onyxPartRealKeysX
  , fixedPartKeysAnimLH   = inPart FlexKeys                  onyxPartKeysAnimLH
  , fixedPartKeysAnimRH   = inPart FlexKeys                  onyxPartKeysAnimRH
  , fixedPartVocals       = inPart FlexVocal                 onyxPartVocals
  , fixedPartDance        = inPart (FlexExtra "global")      onyxPartDance
  , fixedHarm1            = inPart FlexVocal                 onyxHarm1
  , fixedHarm2            = inPart FlexVocal                 onyxHarm2
  , fixedHarm3            = inPart FlexVocal                 onyxHarm3
  , fixedLipsync1         = inPart FlexVocal                 onyxLipsync1
  , fixedLipsync2         = inPart FlexVocal                 onyxLipsync2
  , fixedLipsync3         = inPart FlexVocal                 onyxLipsync3
  , fixedLipsync4         = inPart FlexVocal                 onyxLipsync4
  , fixedLipsyncJohn      = mempty
  , fixedLipsyncPaul      = mempty
  , fixedLipsyncGeorge    = mempty
  , fixedLipsyncRingo     = mempty
  , fixedEvents           = onyxEvents o
  , fixedBeat             = onyxBeat o
  , fixedVenue            = onyxVenue o
  } where inPart p f = maybe mempty f $ Map.lookup p $ onyxParts o

fixedToOnyx :: FixedFile U.Beats -> OnyxFile U.Beats
fixedToOnyx f = OnyxFile
  { onyxParts    = Map.fromList
    [ (FlexGuitar, mempty
      { onyxPartGuitar       = fixedPartGuitar       f
      , onyxPartRealGuitar   = fixedPartRealGuitar   f
      , onyxPartRealGuitar22 = fixedPartRealGuitar22 f
      , onyxPartSix          = fixedPartGuitarGHL    f
      })
    , (FlexBass, mempty
      { onyxPartGuitar       = fixedPartBass       f
      , onyxPartRealGuitar   = fixedPartRealBass   f
      , onyxPartRealGuitar22 = fixedPartRealBass22 f
      , onyxPartSix          = fixedPartBassGHL    f
      })
    , (FlexKeys, mempty
      { onyxPartKeys       = fixedPartKeys       f
      , onyxPartRealKeysE  = fixedPartRealKeysE  f
      , onyxPartRealKeysM  = fixedPartRealKeysM  f
      , onyxPartRealKeysH  = fixedPartRealKeysH  f
      , onyxPartRealKeysX  = fixedPartRealKeysX  f
      , onyxPartKeysAnimLH = fixedPartKeysAnimLH f
      , onyxPartKeysAnimRH = fixedPartKeysAnimRH f
      })
    , (FlexDrums, mempty
      { onyxPartDrums       = fixedPartDrums       f
      , onyxPartDrums2x     = fixedPartDrums2x     f
      , onyxPartRealDrumsPS = fixedPartRealDrumsPS f
      })
    , (FlexVocal, mempty
      { onyxPartVocals = fixedPartVocals f
      , onyxHarm1      = fixedHarm1      f
      , onyxHarm2      = fixedHarm2      f
      , onyxHarm3      = fixedHarm3      f
      , onyxLipsync1   = fixedLipsync1   f
      , onyxLipsync2   = fixedLipsync2   f
      , onyxLipsync3   = fixedLipsync3   f
      , onyxLipsync4   = fixedLipsync4   f
      })
    , (FlexExtra "rhythm", mempty
      { onyxPartGuitar = fixedPartRhythm f
      })
    , (FlexExtra "guitar-coop", mempty
      { onyxPartGuitar = fixedPartGuitarCoop f
      })
    , (FlexExtra "global", mempty
      { onyxPartDance = fixedPartDance f
      })
    ]
  , onyxEvents   = fixedEvents f
  , onyxBeat     = fixedBeat f
  , onyxVenue    = fixedVenue f
  , onyxLighting = mempty
  , onyxCamera   = mempty
  }

songLengthBeats :: (HasEvents f) => Song (f U.Beats) -> U.Beats
songLengthBeats s = case RTB.getTimes $ eventsEnd $ getEventsTrack $ s_tracks s of
  [bts] -> bts
  _     -> 0 -- eh

-- | Returns the time of the [end] event in milliseconds.
songLengthMS :: (HasEvents f) => Song (f U.Beats) -> Int
songLengthMS song = floor $ U.applyTempoMap (s_tempos song) (songLengthBeats song) * 1000

saveMIDI :: (MonadIO m, ParseFile f) => FilePath -> Song (f U.Beats) -> m ()
saveMIDI fp song = liftIO $ Save.toFile fp $ showMIDIFile' song

shakeMIDI :: (ParseFile f) => FilePath -> StackTraceT (QueueLog Action) (Song (f U.Beats))
shakeMIDI fp = lift (lift $ need [fp]) >> loadMIDI fp

hasSolo :: (NNC.C t) => RB3Instrument -> Song (FixedFile t) -> Bool
hasSolo Guitar song = any (not . null)
  [ fiveSolo $ fixedPartGuitar $ s_tracks song
  , pgSolo $ fixedPartRealGuitar $ s_tracks song
  , pgSolo $ fixedPartRealGuitar22 $ s_tracks song
  ]
hasSolo Bass song = any (not . null)
  [ fiveSolo $ fixedPartBass $ s_tracks song
  , pgSolo $ fixedPartRealBass $ s_tracks song
  , pgSolo $ fixedPartRealBass22 $ s_tracks song
  ]
hasSolo Drums song = any (not . null)
  [ drumSolo $ fixedPartDrums $ s_tracks song
  ]
hasSolo Keys song = any (not . null)
  [ fiveSolo $ fixedPartKeys $ s_tracks song
  , pkSolo $ fixedPartRealKeysX $ s_tracks song
  ]
hasSolo Vocal song = any (not . null)
  [ vocalPerc $ fixedPartVocals $ s_tracks song
  , vocalPerc $ fixedHarm1 $ s_tracks song
  ]

fixFreeformDrums :: DrumTrack U.Beats -> DrumTrack U.Beats
fixFreeformDrums ft = ft
  { drumSingleRoll = fixFreeform' gems $ drumSingleRoll ft
  , drumDoubleRoll = fixFreeform' gems $ drumDoubleRoll ft
  } where gems = maybe RTB.empty (void . drumGems) $ Map.lookup Expert $ drumDifficulties ft

fixFreeformFive :: FiveTrack U.Beats -> FiveTrack U.Beats
fixFreeformFive ft = ft
  { fiveTremolo = fixFreeform' gems $ fiveTremolo ft
  , fiveTrill   = fixFreeform' gems $ fiveTrill   ft
  } where gems = maybe RTB.empty (void . fiveGems) $ Map.lookup Expert $ fiveDifficulties ft

fixFreeformPK :: ProKeysTrack U.Beats -> ProKeysTrack U.Beats
fixFreeformPK ft = ft
  { pkGlissando = fixFreeform gems $ pkGlissando ft
  , pkTrill     = fixFreeform gems $ pkTrill     ft
  } where gems = void $ pkNotes ft

fixFreeformPG :: ProGuitarTrack U.Beats -> ProGuitarTrack U.Beats
fixFreeformPG ft = ft
  { pgTremolo = fixFreeform' gems $ pgTremolo ft
  , pgTrill   = fixFreeform' gems $ pgTrill   ft
  } where gems = maybe RTB.empty (void . pgNotes) $ Map.lookup Expert $ pgDifficulties ft

-- | Adjusts instrument tracks so rolls on notes 126/127 end just a tick after
--- their last gem note-on.
fixFreeform :: RTB.T U.Beats () -> RTB.T U.Beats Bool -> RTB.T U.Beats Bool
fixFreeform initGems = fmap isJust . fixFreeform' initGems . fmap (\b -> guard b >> Just ())

fixFreeform' :: (Ord a) => RTB.T U.Beats () -> RTB.T U.Beats (Maybe a) -> RTB.T U.Beats (Maybe a)
fixFreeform' initGems = go initGems . RTB.normalize where
  go gems lanes = case RTB.viewL lanes of
    Just ((dt, Just x), lanes') -> case RTB.viewL lanes' of
      Just ((len, Nothing), lanes'') -> let
        covered = U.trackTake len $ U.trackDrop dt gems
        len' = case sum $ RTB.getTimes covered of
          0 -> len -- no gems, shouldn't happen
          s -> s + 1/32
        in RTB.cons dt (Just x) $ RTB.insert len' Nothing $
          go (U.trackDrop dt gems) (RTB.delay len lanes'')
      _ -> lanes -- on not followed by off, abort
    Just ((_, Nothing), _) -> lanes -- off not preceded by on, abort
    Nothing -> RTB.empty -- done

getPercType :: (NNC.C t) => Song (FixedFile t) -> Maybe Percussion
getPercType song = listToMaybe $ do
  trk <-
    [ fixedPartVocals $ s_tracks song
    , fixedHarm1      $ s_tracks song
    , fixedHarm2      $ s_tracks song
    , fixedHarm3      $ s_tracks song
    ]
  (perc, _) <- RTB.getBodies $ vocalPercAnimation trk
  return perc

-- | Makes a dummy Basic Guitar/Bass track, for parts with only Pro Guitar/Bass charted.
protarToGrybo :: ProGuitarTrack U.Beats -> FiveTrack U.Beats
protarToGrybo pg = mempty
  { fiveDifficulties = flip fmap (pgDifficulties pg) $ \pgd -> mempty
    { fiveGems
      = blipEdgesRB_
      $ fmap head
      $ RTB.collectCoincident
      $ noExtendedSustains' standardBlipThreshold standardSustainGap
      $ fmap (\(_, _, len) -> (Five.Green, len))
      $ edgeBlips minSustainLengthRB
      $ pgNotes pgd
    }
  , fiveOverdrive    = pgOverdrive pg
  , fiveBRE          = fmap snd $ pgBRE pg
  , fiveSolo         = pgSolo pg
  }

-- | Makes a dummy Basic Keys track, for parts with only Pro Keys charted.
expertProKeysToKeys :: ProKeysTrack U.Beats -> FiveTrack U.Beats
expertProKeysToKeys pk = mempty
  { fiveDifficulties = let
    fd = mempty
      { fiveGems
        = blipEdgesRB_
        $ fmap head
        $ RTB.collectCoincident
        $ noExtendedSustains' standardBlipThreshold standardSustainGap
        $ fmap (\(_, len) -> (Five.Green, len))
        $ edgeBlips_ minSustainLengthRB
        $ pkNotes pk
      }
    in Map.fromList [ (diff, fd) | diff <- [minBound .. maxBound] ]
  , fiveOverdrive    = pkOverdrive pk
  , fiveBRE          = pkBRE pk
  , fiveSolo         = pkSolo pk
  }

-- | Makes a Pro Keys track, for parts with only Basic Keys charted.
keysToProKeys :: (NNC.C t) => Difficulty -> FiveTrack t -> ProKeysTrack t
keysToProKeys d ft = ProKeysTrack
  { pkLanes     = RTB.singleton NNC.zero RangeA
  , pkTrainer   = RTB.empty
  , pkMood      = RTB.empty
  , pkSolo      = if d == Expert then fiveSolo ft else RTB.empty
  , pkGlissando = RTB.empty
  , pkTrill     = case d of
    Expert -> isJust <$> fiveTrill ft
    -- TODO add Hard trills
    _      -> RTB.empty
  , pkOverdrive = if d == Expert then fiveOverdrive ft else RTB.empty
  , pkBRE       = if d == Expert then fiveBRE ft else RTB.empty
  , pkNotes     = case Map.lookup d $ fiveDifficulties ft of
    Nothing -> RTB.empty
    Just fd -> let
      colorToKey = BlueGreen . \case
        Five.Green  -> C
        Five.Red    -> D
        Five.Yellow -> E
        Five.Blue   -> F
        Five.Orange -> G
      in fmap colorToKey <$> fiveGems fd
  }
