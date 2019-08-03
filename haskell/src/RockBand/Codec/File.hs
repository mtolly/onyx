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
import           Control.Monad                     (forM, forM_, unless, (>=>))
import           Control.Monad.Codec
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State.Strict  (StateT, execState, get, put,
                                                    runStateT)
import           Control.Monad.Trans.Writer.Strict (Writer, execWriter, tell)
import qualified Data.EventList.Relative.TimeBody  as RTB
import           Data.Foldable                     (toList)
import           Data.Functor.Identity             (Identity)
import           Data.Hashable                     (Hashable (..))
import           Data.List.Extra                   (isInfixOf, nubOrd,
                                                    partition, sortOn,
                                                    stripPrefix)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromJust,
                                                    fromMaybe, isNothing,
                                                    mapMaybe)
import           Data.Monoid                       ((<>))
import qualified Data.Text                         as T
import           DeriveHelpers
import           GHC.Generics                      (Generic)
import           MelodysEscape                     (MelodyTrack)
import qualified Numeric.NonNegative.Class         as NNC
import           PhaseShift.Message
import           RockBand.Codec
import           RockBand.Codec.Beat
import           RockBand.Codec.Drums
import           RockBand.Codec.Events
import           RockBand.Codec.Five
import           RockBand.Codec.Lipsync
import           RockBand.Codec.ProGuitar
import           RockBand.Codec.ProKeys
import           RockBand.Codec.Six
import           RockBand.Codec.Venue
import           RockBand.Codec.VenueGen
import           RockBand.Codec.Vocal
import           RockBand.Common
import qualified Sound.MIDI.File                   as F
import qualified Sound.MIDI.File.Event             as E
import qualified Sound.MIDI.File.Event.Meta        as Meta
import qualified Sound.MIDI.Util                   as U

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
        match' = stripTrack $ foldr RTB.merge RTB.empty match
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
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (FixedFile t)

instance HasEvents FixedFile where
  getEventsTrack = fixedEvents

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
  , onyxCatch            :: CatchTrack t
  , onyxLipsync1         :: LipsyncTrack t
  , onyxLipsync2         :: LipsyncTrack t
  , onyxLipsync3         :: LipsyncTrack t
  , onyxMelody           :: MelodyTrack t
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (OnyxPart t)

instance TraverseTrack OnyxPart where
  traverseTrack fn
    (OnyxPart a b c d e f g h i j k l m n o p q r s t u v w)
    = OnyxPart
      <$> traverseTrack fn a <*> traverseTrack fn b <*> traverseTrack fn c
      <*> traverseTrack fn d <*> traverseTrack fn e <*> traverseTrack fn f
      <*> traverseTrack fn g <*> traverseTrack fn h <*> traverseTrack fn i
      <*> traverseTrack fn j <*> traverseTrack fn k <*> traverseTrack fn l
      <*> traverseTrack fn m <*> traverseTrack fn n <*> traverseTrack fn o
      <*> traverseTrack fn p <*> traverseTrack fn q <*> traverseTrack fn r
      <*> traverseTrack fn s <*> traverseTrack fn t <*> traverseTrack fn u
      <*> traverseTrack fn v <*> traverseTrack fn w

getFlexPart :: (NNC.C t) => FlexPartName -> OnyxFile t -> OnyxPart t
getFlexPart part = fromMaybe mempty . Map.lookup part . onyxParts

identifyFlexTrack :: String -> Maybe FlexPartName
identifyFlexTrack name = case stripPrefix "[" name of
  Just name' -> Just $ readPartName $ T.pack $ takeWhile (/= ']') name'
  Nothing
    | "RHYTHM"      `isInfixOf` name -> Just $ FlexExtra "rhythm"
    | "GUITAR COOP" `isInfixOf` name -> Just $ FlexExtra "guitar-coop"
    | "DRUM"        `isInfixOf` name -> Just FlexDrums
    | "GUITAR"      `isInfixOf` name -> Just FlexGuitar
    | "T1 GEMS"     `isInfixOf` name -> Just FlexGuitar
    | "BASS"        `isInfixOf` name -> Just FlexBass
    | "KEYS"        `isInfixOf` name -> Just FlexKeys
    | "VOCAL"       `isInfixOf` name -> Just FlexVocal
    | "HARM"        `isInfixOf` name -> Just FlexVocal
    | "MELODY"      `isInfixOf` name -> Just $ FlexExtra "global"
    | "KONGA"       `isInfixOf` name -> Just $ FlexExtra "global"
    | otherwise                      -> Nothing

parseOnyxPart :: (SendMessage m) => FlexPartName -> FileCodec m U.Beats (OnyxPart U.Beats)
parseOnyxPart partName = do
  let names defPair otherPairs = case lookup partName $ defPair : otherPairs of
        Just rawName -> fileTrack rawName $ map adorn $ defPair : otherPairs
        Nothing      -> fileTrack (adorn defPair) (map adorn otherPairs)
      adorn (_, trkName) = "[" <> getPartName partName <> "] " <> trkName
  onyxPartDrums        <- onyxPartDrums        =. names (FlexDrums, "PART DRUMS") []
  onyxPartDrums2x      <- onyxPartDrums2x      =. names (FlexDrums, "PART DRUMS_2X") []
  onyxPartRealDrumsPS  <- onyxPartRealDrumsPS  =. names (FlexDrums, "PART REAL_DRUMS_PS") []
  onyxPartGuitar       <- onyxPartGuitar       =. names
    (FlexGuitar, "PART GUITAR")
    [ (FlexBass, "PART BASS")
    , (FlexExtra "rhythm", "PART RHYTHM")
    , (FlexExtra "guitar-coop", "PART GUITAR COOP")
    ]
  onyxPartKeys         <- onyxPartKeys         =. names (FlexKeys, "PART KEYS") []
  onyxPartSix          <- onyxPartSix          =. names
    (FlexGuitar, "PART GUITAR GHL")
    [ (FlexBass, "PART BASS GHL") ]
  onyxPartRealGuitar   <- onyxPartRealGuitar   =. names
    (FlexGuitar, "PART REAL_GUITAR")
    [ (FlexBass, "PART REAL_BASS") ]
  onyxPartRealGuitar22 <- onyxPartRealGuitar22 =. names
    (FlexGuitar, "PART REAL_GUITAR_22")
    [ (FlexBass, "PART REAL_BASS_22") ]
  onyxPartRealKeysE    <- onyxPartRealKeysE    =. names (FlexKeys, "PART REAL_KEYS_E") []
  onyxPartRealKeysM    <- onyxPartRealKeysM    =. names (FlexKeys, "PART REAL_KEYS_M") []
  onyxPartRealKeysH    <- onyxPartRealKeysH    =. names (FlexKeys, "PART REAL_KEYS_H") []
  onyxPartRealKeysX    <- onyxPartRealKeysX    =. names (FlexKeys, "PART REAL_KEYS_X") []
  onyxPartKeysAnimLH   <- onyxPartKeysAnimLH   =. names (FlexKeys, "PART KEYS_ANIM_LH") []
  onyxPartKeysAnimRH   <- onyxPartKeysAnimRH   =. names (FlexKeys, "PART KEYS_ANIM_RH") []
  onyxPartVocals       <- onyxPartVocals       =. names (FlexVocal, "PART VOCALS") []
  onyxHarm1            <- onyxHarm1            =. names (FlexVocal, "HARM1") []
  onyxHarm2            <- onyxHarm2            =. names (FlexVocal, "HARM2") []
  onyxHarm3            <- onyxHarm3            =. names (FlexVocal, "HARM3") []
  onyxCatch            <- onyxCatch            =. names (FlexExtra "undefined", "CATCH") []
  onyxMelody           <- onyxMelody           =. names (FlexExtra "global", "MELODY'S ESCAPE") []
  onyxLipsync1         <- onyxLipsync1         =. names (FlexVocal, "LIPSYNC1") []
  onyxLipsync2         <- onyxLipsync2         =. names (FlexVocal, "LIPSYNC2") []
  onyxLipsync3         <- onyxLipsync3         =. names (FlexVocal, "LIPSYNC3") []
  return OnyxPart{..}

instance ParseFile OnyxFile where
  parseFile = do
    onyxParts    <- onyxParts =. Codec
      { codecIn = do
        trks <- lift get
        let partNames = nubOrd $ mapMaybe (U.trackName >=> identifyFlexTrack) trks
        results <- forM partNames $ \partName -> do
          part <- codecIn $ parseOnyxPart partName
          return (partName, part)
        return $ Map.fromList results
      , codecOut = fmapArg $ \parts -> forM_ (Map.toAscList parts) $ \(partName, trk) ->
        codecOut (fileId $ parseOnyxPart partName) trk
      }
    onyxEvents   <- onyxEvents   =. fileTrack "EVENTS"          []
    onyxBeat     <- onyxBeat     =. fileTrack "BEAT"            []
    onyxVenue    <- onyxVenue    =. fileTrack "VENUE"           []
    onyxLighting <- onyxLighting =. fileTrack "LIGHTING"        []
    onyxCamera   <- onyxCamera   =. fileTrack "CAMERA"          []
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

-- | Strips comments and track names from the track before handing it to a track parser.
stripTrack :: (NNC.C t) => RTB.T t E.T -> RTB.T t E.T
stripTrack = RTB.filter $ \e -> case e of
  E.MetaEvent (Meta.TextEvent ('#' : _)) -> False
  E.MetaEvent (Meta.TrackName _        ) -> False
  _                                      -> True

-- | Magma format, assuming 480 ticks per quarter note
showPosition :: U.MeasureMap -> U.Beats -> String
showPosition mmap bts = let
  (m, b) = U.applyMeasureMap mmap bts
  U.TimeSig _len unit = U.timeSigAt bts mmap
  whole = floor $ b / unit :: Int
  frac = b - fromIntegral whole * unit
  ticks = floor $ frac * 480 :: Int
  padded = reverse $ take 3 $ reverse (show ticks) ++ repeat '0'
  in "[" ++ show (m + 1) ++ ":" ++ show (whole + 1) ++ ":" ++ padded ++ "]"

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
  (file, _unrec) <- flip mapStackTraceT (codecIn $ fileTrack name []) $ \f -> do
    flip fmap (runStateT f trks) $ \case
      (Left  err , _    ) -> Left  err
      (Right file, unrec) -> Right (file, unrec)
  return file

readMIDIFile' :: (SendMessage m, ParseFile f) => F.T -> StackTraceT m (Song (f U.Beats))
readMIDIFile' mid = do
  Song tempos mmap trks <- readMIDIFile mid
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
padFixedFile :: Int -> Song (FixedFile U.Beats) -> Song (FixedFile U.Beats)
padFixedFile 0       song                        = song
padFixedFile seconds (Song temps sigs ff) = let
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
      RNil -> RNil
    -- TODO: timesig numerator can only go up to 255 so this could fail for long delay values
    _ -> error "RockBand.Codec.File.padFixedFile: internal error (no time signature at MIDI start)"
  padSimple = RTB.delay beats
  padBeat
    = RTB.cons  0 Bar
    . foldr (.) id (replicate (seconds * 2 - 1) $ RTB.cons 1 Beat)
    . RTB.delay 1
  in Song temps' sigs' $ (mapTrack padSimple ff)
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
wiiUnmute22 :: Song (FixedFile t) -> Song (FixedFile t)
wiiUnmute22 (Song temps sigs ff) = let
  unmuteTrack pg = pg
    { pgDifficulties = fmap unmuteDiff $ pgDifficulties pg
    }
  unmuteDiff diff = diff
    { pgNotes = flip fmap (pgNotes diff) $ \case
      (str, (Muted, fret, len)) | fret > 22 -> (str, (NormalNote, fret, len))
      x                                     -> x
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
