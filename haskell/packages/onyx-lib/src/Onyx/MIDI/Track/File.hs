{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns        #-}
module Onyx.MIDI.Track.File where

import           Control.Monad                     (forM, forM_, guard, unless,
                                                    (>=>))
import           Control.Monad.Codec
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.State.Strict  (StateT, execState, get, put,
                                                    runStateT)
import           Control.Monad.Trans.Writer.Strict (Writer, execWriter, tell)
import qualified Data.ByteString                   as B
import qualified Data.EventList.Relative.TimeBody  as RTB
import           Data.Foldable                     (toList)
import           Data.Functor                      (void)
import           Data.Functor.Identity             (Identity)
import           Data.Hashable                     (Hashable (..))
import           Data.List.Extra                   (nubOrd, nubSort, partition,
                                                    sortOn)
import           Data.List.HT                      (partitionMaybe)
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                as NE
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromJust,
                                                    fromMaybe, isJust,
                                                    isNothing, listToMaybe,
                                                    mapMaybe)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import           Development.Shake                 (Action, need)
import           GHC.Generics                      (Generic)
import qualified Numeric.NonNegative.Class         as NNC
import           Onyx.DeriveHelpers
import           Onyx.Guitar                       (noExtendedSustains',
                                                    standardBlipThreshold,
                                                    standardSustainGap)
import           Onyx.Harmonix.DTA.Serialize.Magma (Percussion)
import           Onyx.MelodysEscape                (MelodyTrack)
import           Onyx.MIDI.Common
import           Onyx.MIDI.Parse                   (getMIDI)
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.Beat
import           Onyx.MIDI.Track.Drums
import           Onyx.MIDI.Track.Drums.Elite       (EliteDrumTrack)
import           Onyx.MIDI.Track.Events
import           Onyx.MIDI.Track.FiveFret
import qualified Onyx.MIDI.Track.FiveFret          as Five
import           Onyx.MIDI.Track.Lipsync
import           Onyx.MIDI.Track.Mania
import           Onyx.MIDI.Track.ProGuitar
import           Onyx.MIDI.Track.ProKeys
import           Onyx.MIDI.Track.Rocksmith
import           Onyx.MIDI.Track.SixFret
import           Onyx.MIDI.Track.Venue
import           Onyx.MIDI.Track.VenueGen
import           Onyx.MIDI.Track.Vocal
import           Onyx.PhaseShift.Dance
import           Onyx.PhaseShift.Message
import           Onyx.StackTrace
import           Onyx.Util.Binary                  (runGetM)
import           Onyx.Util.Handle                  (Readable (..), fileReadable,
                                                    handleToByteString,
                                                    useHandle)
import           Onyx.Util.Text.Decode             (decodeGeneral, encodeLatin1)
import qualified Sound.MIDI.File                   as F
import qualified Sound.MIDI.File.Event             as E
import qualified Sound.MIDI.File.Event.Meta        as Meta
import qualified Sound.MIDI.File.Save              as Save
import qualified Sound.MIDI.Util                   as U

type FileParser m t = StackTraceT (StateT [RTB.T t (E.T T.Text)] m)
type FileBuilder t = Writer [RTB.T t (E.T T.Text)]
type FileCodec m t a = Codec (FileParser m t) (FileBuilder t) a

fileId :: FileCodec (PureLog Identity) t a -> FileCodec (PureLog Identity) t a
fileId = id

class ParseFile f where
  parseFile :: (SendMessage m) => FileCodec m U.Beats (f U.Beats)

parseTrackReport :: (SendMessage m, ParseTrack trk) => RTB.T U.Beats (E.T T.Text) -> StackTraceT m (trk U.Beats)
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
  forM_ (nubSort $ putMIDITrack (++) unrec') $ \e -> case isNoteEdgeCPV e of
    Just (_, _, Nothing) -> return () -- don't clutter with note-off warnings
    _                    -> warn $ "Unrecognized MIDI event: " ++ show e
  return parsedTrk

fileTrack :: (SendMessage m, ParseTrack trk) => NonEmpty T.Text -> FileCodec m U.Beats (trk U.Beats)
fileTrack (name :| otherNames) = Codec
  { codecIn = do
    trks <- lift get
    let (match, rest) = partition matchTrack trks
        match' = stripTrack $ foldr RTB.merge RTB.empty match
        name' = fromMaybe name $ U.trackName match'
    lift $ put rest
    inside ("Parsing track: " <> T.unpack name') $ parseTrackReport match'
  , codecOut = fmapArg $ \trk -> let
    evs = (`execState` RTB.empty) $ codecOut (forcePure parseTrack) trk
    in unless (RTB.null evs) $ tell [U.setTrackName name evs]
  } where
    matchTrack trk = case U.trackName trk of
      Nothing -> False
      Just n  -> elem n $ name : otherNames
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
  , fixedPartEliteDrums   :: EliteDrumTrack t
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
  getEventsTrack = (.fixedEvents)
  getBeatTrack = (.fixedBeat)

instance TraverseTrack FixedFile where
  traverseTrack fn
    (FixedFile a b c d e f g h i j k l m n o p q r s t u v w x y z aa bb cc dd ee ff gg hh ii jj kk)
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
      <*> traverseTrack fn kk

instance ParseFile FixedFile where
  parseFile = do
    fixedPartDrums        <- (.fixedPartDrums       ) =. fileTrack ("PART DRUMS"          :| ["PART DRUM"])
    fixedPartDrums2x      <- (.fixedPartDrums2x     ) =. fileTrack ("PART DRUMS_2X"       :| [])
    fixedPartRealDrumsPS  <- (.fixedPartRealDrumsPS ) =. fileTrack ("PART REAL_DRUMS_PS"  :| [])
    fixedPartEliteDrums   <- (.fixedPartEliteDrums  ) =. fileTrack ("PART ELITE_DRUMS"    :| [])
    fixedPartGuitar       <- (.fixedPartGuitar      ) =. fileTrack ("PART GUITAR"         :| ["T1 GEMS", "Click"])
    fixedPartBass         <- (.fixedPartBass        ) =. fileTrack ("PART BASS"           :| [])
    fixedPartKeys         <- (.fixedPartKeys        ) =. fileTrack ("PART KEYS"           :| [])
    fixedPartRhythm       <- (.fixedPartRhythm      ) =. fileTrack ("PART RHYTHM"         :| [])
    fixedPartGuitarCoop   <- (.fixedPartGuitarCoop  ) =. fileTrack ("PART GUITAR COOP"    :| [])
    fixedPartRealGuitar   <- (.fixedPartRealGuitar  ) =. fileTrack ("PART REAL_GUITAR"    :| [])
    fixedPartRealGuitar22 <- (.fixedPartRealGuitar22) =. fileTrack ("PART REAL_GUITAR_22" :| [])
    fixedPartRealBass     <- (.fixedPartRealBass    ) =. fileTrack ("PART REAL_BASS"      :| [])
    fixedPartRealBass22   <- (.fixedPartRealBass22  ) =. fileTrack ("PART REAL_BASS_22"   :| [])
    fixedPartGuitarGHL    <- (.fixedPartGuitarGHL   ) =. fileTrack ("PART GUITAR GHL"     :| [])
    fixedPartBassGHL      <- (.fixedPartBassGHL     ) =. fileTrack ("PART BASS GHL"       :| [])
    fixedPartRealKeysE    <- (.fixedPartRealKeysE   ) =. fileTrack ("PART REAL_KEYS_E"    :| [])
    fixedPartRealKeysM    <- (.fixedPartRealKeysM   ) =. fileTrack ("PART REAL_KEYS_M"    :| [])
    fixedPartRealKeysH    <- (.fixedPartRealKeysH   ) =. fileTrack ("PART REAL_KEYS_H"    :| [])
    fixedPartRealKeysX    <- (.fixedPartRealKeysX   ) =. fileTrack ("PART REAL_KEYS_X"    :| [])
    fixedPartKeysAnimLH   <- (.fixedPartKeysAnimLH  ) =. fileTrack ("PART KEYS_ANIM_LH"   :| [])
    fixedPartKeysAnimRH   <- (.fixedPartKeysAnimRH  ) =. fileTrack ("PART KEYS_ANIM_RH"   :| [])
    fixedPartVocals       <- (.fixedPartVocals      ) =. fileTrack ("PART VOCALS"         :| [])
    fixedPartDance        <- (.fixedPartDance       ) =. fileTrack ("PART DANCE"          :| [])
    fixedHarm1            <- (.fixedHarm1           ) =. fileTrack ("HARM1"               :| ["PART HARM1"]) -- PART is used in TBRB
    fixedHarm2            <- (.fixedHarm2           ) =. fileTrack ("HARM2"               :| ["PART HARM2"])
    fixedHarm3            <- (.fixedHarm3           ) =. fileTrack ("HARM3"               :| ["PART HARM3"])
    fixedLipsync1         <- (.fixedLipsync1        ) =. fileTrack ("LIPSYNC1"            :| [])
    fixedLipsync2         <- (.fixedLipsync2        ) =. fileTrack ("LIPSYNC2"            :| [])
    fixedLipsync3         <- (.fixedLipsync3        ) =. fileTrack ("LIPSYNC3"            :| [])
    fixedLipsync4         <- (.fixedLipsync4        ) =. fileTrack ("LIPSYNC4"            :| [])
    fixedLipsyncJohn      <- (.fixedLipsyncJohn     ) =. fileTrack ("LIPSYNC_JOHN"        :| [])
    fixedLipsyncPaul      <- (.fixedLipsyncPaul     ) =. fileTrack ("LIPSYNC_PAUL"        :| [])
    fixedLipsyncGeorge    <- (.fixedLipsyncGeorge   ) =. fileTrack ("LIPSYNC_GEORGE"      :| [])
    fixedLipsyncRingo     <- (.fixedLipsyncRingo    ) =. fileTrack ("LIPSYNC_RINGO"       :| [])
    fixedEvents           <- (.fixedEvents          ) =. fileTrack ("EVENTS"              :| [])
    fixedBeat             <- (.fixedBeat            ) =. fileTrack ("BEAT"                :| [])
    fixedVenue            <- (.fixedVenue           ) =. fileTrack ("VENUE"               :| [])
    return FixedFile{..}

newtype PartName = PartName T.Text
  deriving (Eq, Ord, Show)

pattern PartGuitar :: PartName
pattern PartGuitar = PartName "guitar"

pattern PartBass   :: PartName
pattern PartBass   = PartName "bass"

pattern PartDrums  :: PartName
pattern PartDrums  = PartName "drums"

pattern PartKeys   :: PartName
pattern PartKeys   = PartName "keys"

pattern PartVocal  :: PartName
pattern PartVocal  = PartName "vocal"

getPartName :: PartName -> T.Text
getPartName (PartName t) = t

instance Hashable PartName where
  hashWithSalt salt = hashWithSalt salt . getPartName

data OnyxFile t = OnyxFile
  { onyxParts    :: Map.Map PartName (OnyxPart t)
  , onyxEvents   :: EventsTrack t
  , onyxBeat     :: BeatTrack t
  , onyxVenue    :: VenueTrack t
  , onyxLighting :: LightingTrack t
  , onyxCamera   :: CameraTrack t
  , onyxCameraBG :: CameraTrack t
  , onyxCameraBK :: CameraTrack t
  , onyxCameraGK :: CameraTrack t
  , onyxSamples  :: Map.Map T.Text (SamplesTrack t)
  , onyxRaw      :: Map.Map T.Text (RTB.T t (E.T T.Text))
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (OnyxFile t)

instance HasEvents OnyxFile where
  getEventsTrack = (.onyxEvents)
  getBeatTrack = (.onyxBeat)

instance TraverseTrack OnyxFile where
  traverseTrack fn
    (OnyxFile a b c d e f g h i j k)
    = OnyxFile
      <$> traverse (traverseTrack fn) a
      <*> traverseTrack fn b <*> traverseTrack fn c
      <*> traverseTrack fn d <*> traverseTrack fn e
      <*> traverseTrack fn f
      <*> traverseTrack fn g
      <*> traverseTrack fn h
      <*> traverseTrack fn i
      <*> traverse (traverseTrack fn) j
      <*> traverse fn k

data OnyxPart t = OnyxPart
  { onyxPartDrums        :: DrumTrack t
  , onyxPartDrums2x      :: DrumTrack t
  , onyxPartRealDrumsPS  :: DrumTrack t
  , onyxPartEliteDrums   :: EliteDrumTrack t
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
  , onyxLipsync1         :: LipsyncTrack t
  , onyxLipsync2         :: LipsyncTrack t
  , onyxLipsync3         :: LipsyncTrack t
  , onyxLipsync4         :: LipsyncTrack t
  , onyxMelody           :: MelodyTrack t
  , onyxPartMania        :: Map.Map T.Text (ManiaTrack t)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (OnyxPart t)

editOnyxPart :: (NNC.C t) => PartName -> (OnyxPart t -> OnyxPart t) -> OnyxFile t -> OnyxFile t
editOnyxPart pname edit onyx = onyx
  { onyxParts = Map.alter
    (Just . edit . fromMaybe mempty)
    pname
    onyx.onyxParts
  }

instance TraverseTrack OnyxPart where
  traverseTrack fn
    (OnyxPart a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab)
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
      <*> traverse (traverseTrack fn) ab

getFlexPart :: (NNC.C t) => PartName -> OnyxFile t -> OnyxPart t
getFlexPart part = fromMaybe mempty . Map.lookup part . (.onyxParts)

identifyFlexTrack :: T.Text -> Maybe PartName
identifyFlexTrack name = case T.stripPrefix "[" name of
  Just name' -> Just $ PartName $ T.takeWhile (/= ']') name'
  Nothing
    | "RHYTHM"      `T.isInfixOf` name -> Just $ PartName "rhythm"
    | "GUITAR COOP" `T.isInfixOf` name -> Just $ PartName "guitar-coop"
    | "DRUM"        `T.isInfixOf` name -> Just PartDrums
    | "GUITAR"      `T.isInfixOf` name -> Just PartGuitar
    | "LEAD"        `T.isInfixOf` name -> Just PartGuitar
    | "T1 GEMS"     `T.isInfixOf` name -> Just PartGuitar
    | "Click"       `T.isInfixOf` name -> Just PartGuitar
    | "BASS"        `T.isInfixOf` name -> Just PartBass
    | "KEYS"        `T.isInfixOf` name -> Just PartKeys
    | "VOCAL"       `T.isInfixOf` name -> Just PartVocal
    | "HARM"        `T.isInfixOf` name -> Just PartVocal
    | "MELODY"      `T.isInfixOf` name -> Just $ PartName "global"
    | "KONGA"       `T.isInfixOf` name -> Just $ PartName "global"
    | "DANCE"       `T.isInfixOf` name -> Just $ PartName "dance"
    | "MANIA"       `T.isInfixOf` name -> Just PartKeys
    | otherwise                        -> Nothing

parseOnyxPart :: (SendMessage m) => PartName -> FileCodec m U.Beats (OnyxPart U.Beats)
parseOnyxPart partName = do
  let names pairs = let
        rawNames = fmap snd $ NE.filter ((== partName) . fst) pairs
        in fileTrack $ foldr NE.cons (fmap adorn pairs) rawNames
      adorn (_, trkName) = "[" <> getPartName partName <> "] " <> trkName
  onyxPartDrums        <- (.onyxPartDrums       ) =. names (pure (PartDrums, "PART DRUMS"))
  onyxPartDrums2x      <- (.onyxPartDrums2x     ) =. names (pure (PartDrums, "PART DRUMS_2X"))
  onyxPartRealDrumsPS  <- (.onyxPartRealDrumsPS ) =. names (pure (PartDrums, "PART REAL_DRUMS_PS"))
  onyxPartEliteDrums   <- (.onyxPartEliteDrums  ) =. names (pure (PartDrums, "PART ELITE_DRUMS"))
  onyxPartGuitar       <- (.onyxPartGuitar      ) =. names
    ( (PartGuitar, "PART GUITAR") :|
    [ (PartBass, "PART BASS")
    , (PartName "rhythm", "PART RHYTHM")
    , (PartName "guitar-coop", "PART GUITAR COOP")
    ])
  onyxPartKeys         <- (.onyxPartKeys        ) =. names (pure (PartKeys, "PART KEYS"))
  onyxPartGuitarExt    <- (.onyxPartGuitarExt   ) =. names
    ( (PartGuitar, "PART GUITAR EXT") :|
    [ (PartBass, "PART BASS EXT")
    , (PartName "rhythm", "PART RHYTHM EXT")
    , (PartName "guitar-coop", "PART GUITAR COOP EXT")
    ])
  onyxPartSix          <- (.onyxPartSix         ) =. names
    ((PartGuitar, "PART GUITAR GHL") :|
    [ (PartBass, "PART BASS GHL") ])
  onyxPartRealGuitar   <- (.onyxPartRealGuitar  ) =. names
    ( (PartGuitar, "PART REAL_GUITAR") :|
    [ (PartBass, "PART REAL_BASS") ])
  onyxPartRealGuitar22 <- (.onyxPartRealGuitar22) =. names
    ( (PartGuitar, "PART REAL_GUITAR_22") :|
    [ (PartBass, "PART REAL_BASS_22") ])
  onyxPartRSGuitar <- (.onyxPartRSGuitar) =. names
    ( (PartGuitar, "PART RS LEAD") :|
    [ (PartName "rhythm", "PART RS RHYTHM") ])
  onyxPartRSBass <- (.onyxPartRSBass) =. names (pure (PartBass, "PART RS BASS"))
  onyxPartRealKeysE    <- (.onyxPartRealKeysE ) =. names (pure (PartKeys, "PART REAL_KEYS_E"))
  onyxPartRealKeysM    <- (.onyxPartRealKeysM ) =. names (pure (PartKeys, "PART REAL_KEYS_M"))
  onyxPartRealKeysH    <- (.onyxPartRealKeysH ) =. names (pure (PartKeys, "PART REAL_KEYS_H"))
  onyxPartRealKeysX    <- (.onyxPartRealKeysX ) =. names (pure (PartKeys, "PART REAL_KEYS_X"))
  onyxPartKeysAnimLH   <- (.onyxPartKeysAnimLH) =. names (pure (PartKeys, "PART KEYS_ANIM_LH"))
  onyxPartKeysAnimRH   <- (.onyxPartKeysAnimRH) =. names (pure (PartKeys, "PART KEYS_ANIM_RH"))
  onyxPartVocals       <- (.onyxPartVocals    ) =. names (pure (PartVocal, "PART VOCALS"))
  onyxHarm1            <- (.onyxHarm1         ) =. names ((PartVocal, "HARM1") :| [(PartVocal, "PART HARM1")])
  onyxHarm2            <- (.onyxHarm2         ) =. names ((PartVocal, "HARM2") :| [(PartVocal, "PART HARM2")])
  onyxHarm3            <- (.onyxHarm3         ) =. names ((PartVocal, "HARM3") :| [(PartVocal, "PART HARM3")])
  onyxMelody           <- (.onyxMelody        ) =. names (pure (PartName "global", "MELODY'S ESCAPE"))
  onyxLipsync1         <- (.onyxLipsync1      ) =. names (pure (PartVocal, "LIPSYNC1"))
  onyxLipsync2         <- (.onyxLipsync2      ) =. names (pure (PartVocal, "LIPSYNC2"))
  onyxLipsync3         <- (.onyxLipsync3      ) =. names (pure (PartVocal, "LIPSYNC3"))
  onyxLipsync4         <- (.onyxLipsync4      ) =. names (pure (PartVocal, "LIPSYNC4"))
  onyxPartMania        <- (.onyxPartMania     ) =. let
    prefix = "[" <> getPartName partName <> "] PART MANIA "
    in Codec
      { codecIn = do
        trks <- lift get
        let maniaNames = nubOrd $ mapMaybe (U.trackName >=> T.stripPrefix prefix) trks
        results <- forM maniaNames $ \maniaName -> do
          trk <- codecIn $ fileTrack $ pure $ prefix <> maniaName
          return (maniaName, trk)
        return $ Map.fromList results
      , codecOut = fmapArg $ \m -> forM_ (Map.toAscList m) $ \(maniaName, trk) -> let
        name = prefix <> maniaName
        in codecOut (fileId $ fileTrack $ pure name) trk
      }
  return OnyxPart{..}

instance ParseFile OnyxFile where
  parseFile = do
    onyxParts    <- (.onyxParts) =. Codec
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
    onyxEvents   <- (.onyxEvents  ) =. fileTrack (pure "EVENTS"   )
    onyxBeat     <- (.onyxBeat    ) =. fileTrack (pure "BEAT"     )
    onyxVenue    <- (.onyxVenue   ) =. fileTrack (pure "VENUE"    )
    onyxLighting <- (.onyxLighting) =. fileTrack (pure "LIGHTING" )
    onyxCamera   <- (.onyxCamera  ) =. fileTrack (pure "CAMERA"   )
    onyxCameraBG <- (.onyxCameraBG) =. fileTrack (pure "CAMERA_BG")
    onyxCameraBK <- (.onyxCameraBK) =. fileTrack (pure "CAMERA_BK")
    onyxCameraGK <- (.onyxCameraGK) =. fileTrack (pure "CAMERA_GK")
    onyxSamples  <- (.onyxSamples ) =. Codec
      { codecIn = do
        trks <- lift get
        let audioNames = nubOrd $ mapMaybe (U.trackName >=> T.stripPrefix "AUDIO ") trks
        results <- forM audioNames $ \audioName -> do
          trk <- codecIn $ fileTrack $ pure $ "AUDIO " <> audioName
          return (audioName, trk)
        return $ Map.fromList results
      , codecOut = fmapArg $ \m -> forM_ (Map.toAscList m) $ \(audioName, trk) -> let
        name = "AUDIO " <> audioName
        in codecOut (fileId $ fileTrack $ pure name) trk
      }
    onyxRaw  <- (.onyxRaw) =. Codec
      { codecIn = do
        trks <- lift get
        let (yes, no) = flip partitionMaybe trks $ \trk -> case U.trackName trk of
              Just (T.stripPrefix "RAW " -> Just name) -> Just (name, stripTrack trk)
              _                                        -> Nothing
        lift $ put no
        return $ Map.fromList yes
      , codecOut = fmapArg $ \m -> tell
        $ map (\(name, trk) -> U.setTrackName ("RAW " <> name) trk)
        $ Map.toList m
      }
    return OnyxFile{..}

newtype RawFile t = RawFile { rawTracks :: [RTB.T t (E.T T.Text)] }
  deriving (Eq, Ord, Show)

instance ParseFile RawFile where
  parseFile = Codec
    { codecIn = lift $ do
      trks <- get
      put []
      return $ RawFile trks
    , codecOut = fmapArg $ tell . (.rawTracks)
    }

newtype RawTrack a t = RawTrack { rawTrack :: (RTB.T t a) }

instance TraverseTrack (RawTrack a) where
  traverseTrack f (RawTrack trk) = RawTrack <$> f trk

data Song t = Song
  { tempos   :: U.TempoMap
  , timesigs :: U.MeasureMap
  , tracks   :: t
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
fixMoonTaps :: (NNC.C t) => RTB.T t (E.T s) -> RTB.T t (E.T s)
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
fixEventOrder :: (NNC.C t) => RTB.T t (E.T T.Text) -> RTB.T t (E.T T.Text)
fixEventOrder = RTB.flatten . fmap (sortOn f) . RTB.collectCoincident
  where f x@(E.MetaEvent (Meta.TrackName _)) = (-3, 0, x)
        f x@(E.MetaEvent (Meta.TextEvent s))
          -- magma v1: ensure [lighting ...] comes after simultaneous [verse]/[chorus]
          | "[lighting" `T.isPrefixOf` s = (-1, 0, x)
          | otherwise                    = (-2, 0, x)
        f x = case isNoteEdge x of
          Nothing         -> (0 :: Int, 0       , x)
          Just (p, False) -> (1       , negate p, x)
          Just (p, True ) -> (2       , negate p, x)

readMIDIFile :: (SendMessage m) => F.T T.Text -> StackTraceT m (Song [RTB.T U.Beats (E.T T.Text)])
readMIDIFile mid = do
  (tempos, timesigs, tracks) <- case U.decodeFile mid of
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
  -- previously we ran nubOrd to remove duplicate midi events at the same time.
  -- was there a specific reason? removed to fix some usage of audio sample tracks (dtx hihat stuff)
  return Song{..}

-- | Used for reading midis we expect to be type-0, for Rock Revolution
readMixedMIDI :: (Monad m) => F.T s -> StackTraceT m (Song (RTB.T U.Beats (E.T s)))
readMixedMIDI mid = case mid of
  F.Cons F.Mixed _ _ -> case U.decodeFile mid of
    Left [trk] -> let
      tempos = U.makeTempoMap trk
      timesigs = U.makeMeasureMap U.Truncate trk
      tracks = flip RTB.filter trk $ \case
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

showMixedMIDI :: (Ord s) => Song (RTB.T U.Beats (E.T s)) -> F.T s
showMixedMIDI s = let
  tempos = U.unmakeTempoMap s.tempos
  sigs = case mapM U.showSignatureFull $ U.measureMapToTimeSigs s.timesigs of
    Nothing   -> RTB.singleton 0 $ fromJust $ U.showSignature 4
    Just evts -> evts
  trk = RTB.merge (RTB.merge tempos sigs) s.tracks
  in U.encodeFileBeats F.Mixed 480 [trk]

-- | Strips comments and track names from the track before handing it to a track parser.
stripTrack :: (NNC.C t) => RTB.T t (E.T T.Text) -> RTB.T t (E.T T.Text)
stripTrack = RTB.filter $ \e -> case e of
  E.MetaEvent (Meta.TextEvent (T.uncons -> Just ('#', _))) -> False
  E.MetaEvent (Meta.TrackName _                          ) -> False
  _                                                        -> True

data TrackOffset = TrackPad U.Seconds | TrackDrop U.Seconds
  deriving (Eq, Ord, Show)

-- | Copies tracks from the second file into the first, repositioning events by timestamp.
mergeCharts :: TrackOffset -> Song (RawFile U.Beats) -> Song (RawFile U.Beats) -> Song (RawFile U.Beats)
mergeCharts offset base new = let
  newTracks = flip map new.tracks.rawTracks $ \trk -> let
    name = U.trackName trk
    applyOffset = case offset of
      TrackPad  t -> RTB.delay t
      TrackDrop t -> U.trackDrop t -- TODO should this instead shove events forward?
    removeTrackName = RTB.filter $ \case E.MetaEvent (Meta.TrackName _) -> False; _ -> True
    -- TODO: need to ensure notes don't have 0 length
    in maybe id U.setTrackName name
      $ removeTrackName
      $ U.unapplyTempoTrack base.tempos
      $ applyOffset
      $ U.applyTempoTrack new.tempos trk
  in base { tracks = RawFile newTracks }

parseTracks :: (SendMessage m, ParseTrack trk) => [RTB.T U.Beats (E.T T.Text)] -> T.Text -> StackTraceT m (trk U.Beats)
parseTracks trks name = do
  (file, _unrec) <- flip mapStackTraceT (codecIn $ fileTrack $ pure name) $ \f -> do
    flip fmap (runStateT f trks) $ \case
      (Left  err , _    ) -> Left  err
      (Right file, unrec) -> Right (file, unrec)
  return file

interpretMIDIFile
  :: (SendMessage m, ParseFile f)
  => Song [RTB.T U.Beats (E.T T.Text)]
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

readMIDIFile' :: (SendMessage m, ParseFile f) => F.T T.Text -> StackTraceT m (Song (f U.Beats))
readMIDIFile' mid = readMIDIFile mid >>= interpretMIDIFile

loadRawMIDI :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m (F.T T.Text)
loadRawMIDI = loadRawMIDIReadable . fileReadable

loadRawMIDIReadable :: (SendMessage m, MonadIO m) => Readable -> StackTraceT m (F.T T.Text)
loadRawMIDIReadable = fmap (fmap decodeGeneral) . loadMIDIBytes

loadMIDIBytes :: (SendMessage m, MonadIO m) => Readable -> StackTraceT m (F.T B.ByteString)
loadMIDIBytes r = do
  maybe id (\f -> inside $ "loading MIDI: " <> f) (rFilePath r) $ do
    bs <- stackIO $ useHandle r handleToByteString
    (mid, warnings) <- runGetM getMIDI bs
    mapM_ warn warnings
    return mid

loadMIDI :: (SendMessage m, MonadIO m, ParseFile f) => FilePath -> StackTraceT m (Song (f U.Beats))
loadMIDI f = loadRawMIDI f >>= readMIDIFile'

loadMIDIReadable :: (SendMessage m, MonadIO m, ParseFile f) => Readable -> StackTraceT m (Song (f U.Beats))
loadMIDIReadable r = loadRawMIDIReadable r >>= readMIDIFile'

showMIDIFile :: Song [RTB.T U.Beats (E.T T.Text)] -> F.T T.Text
showMIDIFile s = let
  tempos = U.unmakeTempoMap s.tempos
  sigs = case mapM U.showSignatureFull $ U.measureMapToTimeSigs s.timesigs of
    Nothing   -> RTB.singleton 0 $ fromJust $ U.showSignature 4
    Just evts -> evts
  tempoTrk = U.setTrackName "notes" $ RTB.merge tempos sigs
  in U.encodeFileBeats F.Parallel 480 $ map (fixMoonTaps . fixEventOrder) $ tempoTrk : s.tracks

showMIDITracks :: (ParseFile f) => Song (f U.Beats) -> Song [RTB.T U.Beats (E.T T.Text)]
showMIDITracks (Song tempos mmap trks)
  = Song tempos mmap $ execWriter $ codecOut (fileId parseFile) trks

showMIDIFile' :: (ParseFile f) => Song (f U.Beats) -> F.T T.Text
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

padRawTrackFile :: Int -> Song (RTB.T U.Beats a) -> Song (RTB.T U.Beats a)
padRawTrackFile n song = fmap (.rawTrack) $ padAnyFile n $ RawTrack <$> song

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
    { fixedBeat = BeatTrack $ if RTB.null ff.fixedBeat.beatLines
      then RTB.empty
      else padBeat ff.fixedBeat.beatLines
    }

wiiNoFills :: (NNC.C t) => Song (FixedFile t) -> Song (FixedFile t)
wiiNoFills (Song temps sigs ff) = let
  f drums = case RTB.viewL ff.fixedEvents.eventsCoda of
    Nothing           -> drums
      { drumActivation = RTB.empty }
    Just ((dt, _), _) -> drums
      { drumActivation = RTB.delay dt $ U.trackDrop dt drums.drumActivation }
  in Song temps sigs ff
    { fixedPartDrums       = f ff.fixedPartDrums
    , fixedPartDrums2x     = f ff.fixedPartDrums2x
    , fixedPartRealDrumsPS = f ff.fixedPartRealDrumsPS
    }

wiiMustang22 :: Song (FixedFile t) -> Song (FixedFile t)
wiiMustang22 (Song temps sigs ff) = let
  g17 = ff.fixedPartRealGuitar
  g22 = ff.fixedPartRealGuitar22
  b17 = ff.fixedPartRealBass
  b22 = ff.fixedPartRealBass22
  in Song temps sigs ff
    { fixedPartRealGuitar = if all (not . nullPG) [g17, g22] then g22 else g17
    , fixedPartRealBass   = if all (not . nullPG) [b17, b22] then b22 else b17
    }

-- | Unmutes protar notes with a velocity above 22.
wiiUnmute22 :: (NNC.C t) => Song (FixedFile t) -> Song (FixedFile t)
wiiUnmute22 (Song temps sigs ff) = let
  unmuteTrack pg = pg
    { pgDifficulties = fmap unmuteDiff pg.pgDifficulties
    }
  unmuteDiff diff = diff
    { pgNotes
      = splitEdgesSimple
      $ fmap (\case
        (fret, (str, Muted), len) | fret > 22 -> (fret, (str, NormalNote), len)
        x                                     -> x
      )
      $ joinEdgesSimple diff.pgNotes
    }
  in Song temps sigs ff
    { fixedPartRealGuitar   = unmuteTrack ff.fixedPartRealGuitar
    , fixedPartRealGuitar22 = unmuteTrack ff.fixedPartRealGuitar22
    , fixedPartRealBass     = unmuteTrack ff.fixedPartRealBass
    , fixedPartRealBass22   = unmuteTrack ff.fixedPartRealBass22
    }

convertToVenueGen :: Song (OnyxFile U.Beats) -> Song (OnyxFile U.Beats)
convertToVenueGen (Song temps sigs trks) = Song temps sigs trks
  { onyxLighting = trks.onyxLighting <> unbuildLighting 1 trks.onyxVenue
  , onyxCamera   = trks.onyxCamera   <> unbuildCamera   1 trks.onyxVenue
  , onyxVenue = trks.onyxVenue
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
    { onyxParts    = chopTake t <$> o.onyxParts
    , onyxEvents   = chopTake t o.onyxEvents
    , onyxBeat     = chopTake t o.onyxBeat
    , onyxVenue    = chopTake t o.onyxVenue
    , onyxLighting = chopTake t o.onyxLighting
    , onyxCamera   = chopTake t o.onyxCamera
    , onyxCameraBG = chopTake t o.onyxCameraBG
    , onyxCameraBK = chopTake t o.onyxCameraBK
    , onyxCameraGK = chopTake t o.onyxCameraGK
    , onyxSamples  = chopTake t <$> o.onyxSamples
    , onyxRaw      = U.trackTake t <$> o.onyxRaw
    }
  chopDrop t o = OnyxFile
    { onyxParts    = chopDrop t <$> o.onyxParts
    , onyxEvents   = chopDrop t o.onyxEvents
    , onyxBeat     = chopDrop t o.onyxBeat
    , onyxVenue    = chopDrop t o.onyxVenue
    , onyxLighting = chopDrop t o.onyxLighting
    , onyxCamera   = chopDrop t o.onyxCamera
    , onyxCameraBG = chopDrop t o.onyxCameraBG
    , onyxCameraBK = chopDrop t o.onyxCameraBK
    , onyxCameraGK = chopDrop t o.onyxCameraGK
    , onyxSamples  = chopDrop t <$> o.onyxSamples
    , onyxRaw      = U.trackDrop t <$> o.onyxRaw
    }

instance ChopTrack OnyxPart where
  chopTake t op = OnyxPart
    { onyxPartDrums        = chopTake t               op.onyxPartDrums
    , onyxPartDrums2x      = chopTake t               op.onyxPartDrums2x
    , onyxPartRealDrumsPS  = chopTake t               op.onyxPartRealDrumsPS
    , onyxPartEliteDrums   = mapTrack (U.trackTake t) op.onyxPartEliteDrums   -- TODO
    , onyxPartGuitar       = chopTake t               op.onyxPartGuitar
    , onyxPartKeys         = chopTake t               op.onyxPartKeys
    , onyxPartGuitarExt    = chopTake t               op.onyxPartGuitarExt
    , onyxPartSix          = mapTrack (U.trackTake t) op.onyxPartSix          -- TODO
    , onyxPartRealGuitar   = chopTake t               op.onyxPartRealGuitar
    , onyxPartRealGuitar22 = chopTake t               op.onyxPartRealGuitar22
    , onyxPartRSGuitar     = mapTrack (U.trackTake t) op.onyxPartRSGuitar     -- TODO
    , onyxPartRSBass       = mapTrack (U.trackTake t) op.onyxPartRSBass       -- TODO
    , onyxPartRealKeysE    = chopTake t               op.onyxPartRealKeysE
    , onyxPartRealKeysM    = chopTake t               op.onyxPartRealKeysM
    , onyxPartRealKeysH    = chopTake t               op.onyxPartRealKeysH
    , onyxPartRealKeysX    = chopTake t               op.onyxPartRealKeysX
    , onyxPartKeysAnimLH   = mapTrack (U.trackTake t) op.onyxPartKeysAnimLH   -- TODO
    , onyxPartKeysAnimRH   = mapTrack (U.trackTake t) op.onyxPartKeysAnimRH   -- TODO
    , onyxPartVocals       = mapTrack (U.trackTake t) op.onyxPartVocals       -- TODO
    , onyxHarm1            = mapTrack (U.trackTake t) op.onyxHarm1            -- TODO
    , onyxHarm2            = mapTrack (U.trackTake t) op.onyxHarm2            -- TODO
    , onyxHarm3            = mapTrack (U.trackTake t) op.onyxHarm3            -- TODO
    , onyxLipsync1         = mapTrack (U.trackTake t) op.onyxLipsync1         -- TODO
    , onyxLipsync2         = mapTrack (U.trackTake t) op.onyxLipsync2         -- TODO
    , onyxLipsync3         = mapTrack (U.trackTake t) op.onyxLipsync3         -- TODO
    , onyxLipsync4         = mapTrack (U.trackTake t) op.onyxLipsync4         -- TODO
    , onyxMelody           = mapTrack (U.trackTake t) op.onyxMelody           -- TODO
    , onyxPartMania        = mapTrack (U.trackTake t) <$> op.onyxPartMania    -- TODO
    }
  chopDrop t op = OnyxPart
    { onyxPartDrums        = chopDrop t               op.onyxPartDrums
    , onyxPartDrums2x      = chopDrop t               op.onyxPartDrums2x
    , onyxPartRealDrumsPS  = chopDrop t               op.onyxPartRealDrumsPS
    , onyxPartEliteDrums   = mapTrack (U.trackDrop t) op.onyxPartEliteDrums -- TODO
    , onyxPartGuitar       = chopDrop t               op.onyxPartGuitar
    , onyxPartKeys         = chopDrop t               op.onyxPartKeys
    , onyxPartGuitarExt    = chopDrop t               op.onyxPartGuitarExt
    , onyxPartSix          = mapTrack (U.trackDrop t) op.onyxPartSix        -- TODO
    , onyxPartRealGuitar   = chopDrop t               op.onyxPartRealGuitar
    , onyxPartRealGuitar22 = chopDrop t               op.onyxPartRealGuitar22
    , onyxPartRSGuitar     = mapTrack (U.trackDrop t) op.onyxPartRSGuitar   -- TODO
    , onyxPartRSBass       = mapTrack (U.trackDrop t) op.onyxPartRSBass     -- TODO
    , onyxPartRealKeysE    = chopDrop t               op.onyxPartRealKeysE
    , onyxPartRealKeysM    = chopDrop t               op.onyxPartRealKeysM
    , onyxPartRealKeysH    = chopDrop t               op.onyxPartRealKeysH
    , onyxPartRealKeysX    = chopDrop t               op.onyxPartRealKeysX
    , onyxPartKeysAnimLH   = mapTrack (U.trackDrop t) op.onyxPartKeysAnimLH -- TODO
    , onyxPartKeysAnimRH   = mapTrack (U.trackDrop t) op.onyxPartKeysAnimRH -- TODO
    , onyxPartVocals       = mapTrack (U.trackDrop t) op.onyxPartVocals     -- TODO
    , onyxHarm1            = mapTrack (U.trackDrop t) op.onyxHarm1          -- TODO
    , onyxHarm2            = mapTrack (U.trackDrop t) op.onyxHarm2          -- TODO
    , onyxHarm3            = mapTrack (U.trackDrop t) op.onyxHarm3          -- TODO
    , onyxLipsync1         = mapTrack (U.trackDrop t) op.onyxLipsync1       -- TODO
    , onyxLipsync2         = mapTrack (U.trackDrop t) op.onyxLipsync2       -- TODO
    , onyxLipsync3         = mapTrack (U.trackDrop t) op.onyxLipsync3       -- TODO
    , onyxLipsync4         = mapTrack (U.trackDrop t) op.onyxLipsync4       -- TODO
    , onyxMelody           = mapTrack (U.trackDrop t) op.onyxMelody         -- TODO
    , onyxPartMania        = mapTrack (U.trackDrop t) <$> op.onyxPartMania  -- TODO
    }

onyxToFixed :: OnyxFile U.Beats -> FixedFile U.Beats
onyxToFixed o = FixedFile
  { fixedPartDrums        = inPart PartDrums                 (.onyxPartDrums)
  , fixedPartDrums2x      = inPart PartDrums                 (.onyxPartDrums2x)
  , fixedPartRealDrumsPS  = inPart PartDrums                 (.onyxPartRealDrumsPS)
  , fixedPartEliteDrums   = inPart PartDrums                 (.onyxPartEliteDrums)
  , fixedPartGuitar       = inPart PartGuitar                (.onyxPartGuitar)
  , fixedPartBass         = inPart PartBass                  (.onyxPartGuitar)
  , fixedPartKeys         = inPart PartKeys                  (.onyxPartKeys)
  , fixedPartRhythm       = inPart (PartName "rhythm")      (.onyxPartGuitar)
  , fixedPartGuitarCoop   = inPart (PartName "guitar-coop") (.onyxPartGuitar)
  , fixedPartRealGuitar   = inPart PartGuitar                (.onyxPartRealGuitar)
  , fixedPartRealGuitar22 = inPart PartGuitar                (.onyxPartRealGuitar22)
  , fixedPartRealBass     = inPart PartBass                  (.onyxPartRealGuitar)
  , fixedPartRealBass22   = inPart PartBass                  (.onyxPartRealGuitar22)
  , fixedPartGuitarGHL    = inPart PartGuitar                (.onyxPartSix)
  , fixedPartBassGHL      = inPart PartBass                  (.onyxPartSix)
  , fixedPartRealKeysE    = inPart PartKeys                  (.onyxPartRealKeysE)
  , fixedPartRealKeysM    = inPart PartKeys                  (.onyxPartRealKeysM)
  , fixedPartRealKeysH    = inPart PartKeys                  (.onyxPartRealKeysH)
  , fixedPartRealKeysX    = inPart PartKeys                  (.onyxPartRealKeysX)
  , fixedPartKeysAnimLH   = inPart PartKeys                  (.onyxPartKeysAnimLH)
  , fixedPartKeysAnimRH   = inPart PartKeys                  (.onyxPartKeysAnimRH)
  , fixedPartVocals       = inPart PartVocal                 (.onyxPartVocals)
  , fixedPartDance        = mempty
  , fixedHarm1            = inPart PartVocal                 (.onyxHarm1)
  , fixedHarm2            = inPart PartVocal                 (.onyxHarm2)
  , fixedHarm3            = inPart PartVocal                 (.onyxHarm3)
  , fixedLipsync1         = inPart PartVocal                 (.onyxLipsync1)
  , fixedLipsync2         = inPart PartVocal                 (.onyxLipsync2)
  , fixedLipsync3         = inPart PartVocal                 (.onyxLipsync3)
  , fixedLipsync4         = inPart PartVocal                 (.onyxLipsync4)
  , fixedLipsyncJohn      = mempty
  , fixedLipsyncPaul      = mempty
  , fixedLipsyncGeorge    = mempty
  , fixedLipsyncRingo     = mempty
  , fixedEvents           = o.onyxEvents
  , fixedBeat             = o.onyxBeat
  , fixedVenue            = o.onyxVenue
  } where inPart p f = maybe mempty f $ Map.lookup p o.onyxParts

fixedToOnyx :: FixedFile U.Beats -> OnyxFile U.Beats
fixedToOnyx f = OnyxFile
  { onyxParts    = Map.fromList
    [ (PartGuitar, mempty
      { onyxPartGuitar       = f.fixedPartGuitar
      , onyxPartRealGuitar   = f.fixedPartRealGuitar
      , onyxPartRealGuitar22 = f.fixedPartRealGuitar22
      , onyxPartSix          = f.fixedPartGuitarGHL
      })
    , (PartBass, mempty
      { onyxPartGuitar       = f.fixedPartBass
      , onyxPartRealGuitar   = f.fixedPartRealBass
      , onyxPartRealGuitar22 = f.fixedPartRealBass22
      , onyxPartSix          = f.fixedPartBassGHL
      })
    , (PartKeys, mempty
      { onyxPartKeys       = f.fixedPartKeys
      , onyxPartRealKeysE  = f.fixedPartRealKeysE
      , onyxPartRealKeysM  = f.fixedPartRealKeysM
      , onyxPartRealKeysH  = f.fixedPartRealKeysH
      , onyxPartRealKeysX  = f.fixedPartRealKeysX
      , onyxPartKeysAnimLH = f.fixedPartKeysAnimLH
      , onyxPartKeysAnimRH = f.fixedPartKeysAnimRH
      })
    , (PartDrums, mempty
      { onyxPartDrums       = f.fixedPartDrums
      , onyxPartDrums2x     = f.fixedPartDrums2x
      , onyxPartRealDrumsPS = f.fixedPartRealDrumsPS
      , onyxPartEliteDrums  = f.fixedPartEliteDrums
      })
    , (PartVocal, mempty
      { onyxPartVocals = f.fixedPartVocals
      , onyxHarm1      = f.fixedHarm1
      , onyxHarm2      = f.fixedHarm2
      , onyxHarm3      = f.fixedHarm3
      , onyxLipsync1   = f.fixedLipsync1
      , onyxLipsync2   = f.fixedLipsync2
      , onyxLipsync3   = f.fixedLipsync3
      , onyxLipsync4   = f.fixedLipsync4
      })
    , (PartName "rhythm", mempty
      { onyxPartGuitar = f.fixedPartRhythm
      })
    , (PartName "guitar-coop", mempty
      { onyxPartGuitar = f.fixedPartGuitarCoop
      })
    , (PartName "dance", mempty
      { onyxPartMania = danceToMania f.fixedPartDance
      })
    ]
  , onyxEvents   = f.fixedEvents
  , onyxBeat     = f.fixedBeat
  , onyxVenue    = f.fixedVenue
  , onyxLighting = mempty
  , onyxCamera   = mempty
  , onyxCameraBG = mempty
  , onyxCameraBK = mempty
  , onyxCameraGK = mempty
  , onyxSamples  = mempty
  , onyxRaw      = mempty
  }

songLengthBeats :: (HasEvents f) => Song (f U.Beats) -> U.Beats
songLengthBeats s = case RTB.getTimes $ eventsEnd $ getEventsTrack s.tracks of
  [bts] -> bts
  _     -> 0 -- eh

-- | Returns the time of the [end] event in milliseconds.
songLengthMS :: (HasEvents f) => Song (f U.Beats) -> Int
songLengthMS song = floor $ U.applyTempoMap song.tempos (songLengthBeats song) * 1000

saveMIDILatin1 :: (MonadIO m, ParseFile f) => FilePath -> Song (f U.Beats) -> m ()
saveMIDILatin1 fp song = liftIO $ Save.toFile fp $ fmap encodeLatin1 $ showMIDIFile' song

saveMIDIUtf8 :: (MonadIO m, ParseFile f) => FilePath -> Song (f U.Beats) -> m ()
saveMIDIUtf8 fp song = liftIO $ Save.toFile fp $ fmap TE.encodeUtf8 $ showMIDIFile' song

shakeMIDI :: (ParseFile f) => FilePath -> StackTraceT (QueueLog Action) (Song (f U.Beats))
shakeMIDI fp = lift (lift $ need [fp]) >> loadMIDI fp

hasSolo :: (NNC.C t) => RB3Instrument -> Song (FixedFile t) -> Bool
hasSolo Guitar song = any (not . null)
  [ song.tracks.fixedPartGuitar.fiveSolo
  , song.tracks.fixedPartRealGuitar.pgSolo
  , song.tracks.fixedPartRealGuitar22.pgSolo
  ]
hasSolo Bass song = any (not . null)
  [ song.tracks.fixedPartBass.fiveSolo
  , song.tracks.fixedPartRealBass.pgSolo
  , song.tracks.fixedPartRealBass22.pgSolo
  ]
hasSolo Drums song = any (not . null)
  [ song.tracks.fixedPartDrums.drumSolo
  ]
hasSolo Keys song = any (not . null)
  [ song.tracks.fixedPartKeys.fiveSolo
  , song.tracks.fixedPartRealKeysX.pkSolo
  ]
hasSolo Vocal song = any (not . null)
  [ song.tracks.fixedPartVocals.vocalPerc
  , song.tracks.fixedHarm1.vocalPerc
  ]

fixFreeformDrums :: DrumTrack U.Beats -> DrumTrack U.Beats
fixFreeformDrums trk = trk
  { drumSingleRoll = fixFreeform' gems trk.drumSingleRoll
  , drumDoubleRoll = fixFreeform' gems trk.drumDoubleRoll
  } where gems = maybe RTB.empty (void . (.drumGems)) $ Map.lookup Expert trk.drumDifficulties

fixFreeformFive :: FiveTrack U.Beats -> FiveTrack U.Beats
fixFreeformFive trk = trk
  { fiveTremolo = fixFreeform' gems trk.fiveTremolo
  , fiveTrill   = fixFreeform' gems trk.fiveTrill
  } where gems = maybe RTB.empty (void . (.fiveGems)) $ Map.lookup Expert trk.fiveDifficulties

fixFreeformPK :: ProKeysTrack U.Beats -> ProKeysTrack U.Beats
fixFreeformPK trk = trk
  { pkGlissando = fixFreeform gems trk.pkGlissando
  , pkTrill     = fixFreeform gems trk.pkTrill
  } where gems = void trk.pkNotes

fixFreeformPG :: ProGuitarTrack U.Beats -> ProGuitarTrack U.Beats
fixFreeformPG trk = trk
  { pgTremolo = fixFreeform' gems trk.pgTremolo
  , pgTrill   = fixFreeform' gems trk.pgTrill
  } where gems = maybe RTB.empty (void . (.pgNotes)) $ Map.lookup Expert trk.pgDifficulties

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
    [ song.tracks.fixedPartVocals
    , song.tracks.fixedHarm1
    , song.tracks.fixedHarm2
    , song.tracks.fixedHarm3
    ]
  (perc, _) <- RTB.getBodies $ vocalPercAnimation trk
  return perc

-- | Makes a dummy Basic Guitar/Bass track, for parts with only Pro Guitar/Bass charted.
protarToGrybo :: ProGuitarTrack U.Beats -> FiveTrack U.Beats
protarToGrybo pg = mempty
  { fiveDifficulties = flip fmap pg.pgDifficulties $ \pgd -> mempty
    { fiveGems
      = blipEdgesRB_
      $ fmap head
      $ RTB.collectCoincident
      $ noExtendedSustains' standardBlipThreshold standardSustainGap
      $ fmap (\(_, _, len) -> (Just Five.Green, len))
      $ edgeBlips minSustainLengthRB pgd.pgNotes
    }
  , fiveOverdrive    = pg.pgOverdrive
  , fiveBRE          = fmap snd pg.pgBRE
  , fiveSolo         = pg.pgSolo
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
        $ fmap (\(_, len) -> (Just Five.Green, len))
        $ edgeBlips_ minSustainLengthRB pk.pkNotes
      }
    in Map.fromList [ (diff, fd) | diff <- [minBound .. maxBound] ]
  , fiveOverdrive    = pk.pkOverdrive
  , fiveBRE          = pk.pkBRE
  , fiveSolo         = pk.pkSolo
  }

-- | Makes a Pro Keys track, for parts with only Basic Keys charted.
keysToProKeys :: (NNC.C t) => Difficulty -> FiveTrack t -> ProKeysTrack t
keysToProKeys d ft = ProKeysTrack
  { pkLanes     = RTB.singleton NNC.zero RangeA
  , pkTrainer   = RTB.empty
  , pkMood      = RTB.empty
  , pkSolo      = if d == Expert then ft.fiveSolo else RTB.empty
  , pkGlissando = RTB.empty
  , pkTrill     = case d of
    Expert -> isJust <$> ft.fiveTrill
    -- TODO add Hard trills
    _      -> RTB.empty
  , pkOverdrive = if d == Expert then ft.fiveOverdrive else RTB.empty
  , pkBRE       = if d == Expert then ft.fiveBRE else RTB.empty
  , pkNotes     = case Map.lookup d ft.fiveDifficulties of
    Nothing -> RTB.empty
    Just fd -> let
      colorToKey = \case
        Nothing          -> RedYellow B
        Just Five.Green  -> BlueGreen C
        Just Five.Red    -> BlueGreen D
        Just Five.Yellow -> BlueGreen E
        Just Five.Blue   -> BlueGreen F
        Just Five.Orange -> BlueGreen G
      in fmap colorToKey <$> fd.fiveGems
  }

newtype SamplesTrack t = SamplesTrack
  { sampleTriggers :: RTB.T t SampleTrigger
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (SamplesTrack t)

instance TraverseTrack SamplesTrack where
  traverseTrack fn (SamplesTrack a)
    = SamplesTrack <$> fn a

instance ChopTrack SamplesTrack where
  chopTake t = mapTrack $ U.trackTake t
  chopDrop t = mapTrack $ U.trackDrop t

instance ParseTrack SamplesTrack where
  parseTrack = do
    sampleTriggers <- (.sampleTriggers) =. command
    return SamplesTrack{..}

data SampleTrigger = SampleTrigger
  { sampleGroup :: T.Text
  , sampleAudio :: T.Text
  } deriving (Eq, Ord, Show)

instance Command SampleTrigger where
  toCommand = \case
    [x]    -> Just $ SampleTrigger "" x
    [x, y] -> Just $ SampleTrigger x y
    _      -> Nothing
  fromCommand = \case
    SampleTrigger "" x -> [x]
    SampleTrigger x  y -> [x, y]
