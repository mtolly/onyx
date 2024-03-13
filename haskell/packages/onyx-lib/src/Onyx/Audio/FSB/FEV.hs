{-

Parsing .fev (fmod event) files from Rock Revolution

Many thanks to
- HotPocketRemix's spec + parser for a later .fev version from Dark Souls 1
- xoreos's parser for an earlier .fev version used in BioWare's Aurora engine

-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Audio.FSB.FEV where

import           Control.Monad   (replicateM, unless)
import           Data.Binary.Get
import qualified Data.ByteString as B
import           Data.Int        (Int16, Int32)
import           Data.Word       (Word16, Word32)
import           Numeric         (showHex)

string :: Get B.ByteString
string = do
  len <- getWord32le
  case len of
    0 -> return ""
    _ -> do
      bs <- getByteString $ fromIntegral len
      unless (B.last bs == 0) $ fail $ "Invalid string: " <> show bs
      return $ B.init bs

lenArray :: Get a -> Get [a]
lenArray f = do
  len <- getWord32le
  replicateM (fromIntegral len) f

-- two versions show up in the rock revolution files...?
data FEVVersion
  = FEVVersion26 -- 00 00 26 00
  | FEVVersion2C -- 00 00 2C 00
  deriving (Show)

data FEV = FEV
  { version               :: FEVVersion
  , projectName           :: B.ByteString
  , waveBanks             :: [WaveBank]
  , topLevelEventCategory :: EventCategory
  , topLevelEventGroups   :: [EventGroup]
  , soundDefs             :: [SoundDef]
  , reverbs               :: [Reverb]
  } deriving (Show)

getFEV :: Get FEV
getFEV = do
  getByteString 4 >>= \case
    "FEV1" -> return ()
    magic  -> fail $ "Invalid .fev magic identifier: " <> show magic
  version               <- getWord32le >>= \case
    0x260000 -> return FEVVersion26
    0x2C0000 -> return FEVVersion2C
    n        -> fail $ "Unsupported .fev version: 0x" <> showHex n ""
  projectName           <- string
  waveBanks             <- lenArray getWaveBank
  topLevelEventCategory <- getEventCategory version
  topLevelEventGroups   <- lenArray $ getEventGroup version
  soundDefs             <- lenArray $ getSoundDef version
  reverbs               <- lenArray getReverb
  isEmpty >>= \case
    True  -> return ()
    False -> fail "Failed to parse all data in .fev file"
  return FEV{..}

data WaveBank = WaveBank
  { bankType       :: Word32
  , bankMaxStreams :: Word32
  , bankName       :: B.ByteString
  } deriving (Show)

getWaveBank :: Get WaveBank
getWaveBank = do
  bankType       <- getWord32le
  bankMaxStreams <- getWord32le
  bankName       <- string
  return WaveBank{..}

data EventCategory = EventCategory
  { name                :: B.ByteString
  , volumeFieldRatio    :: Float
  , pitch               :: Float
  , maxPlaybacks        :: Maybe Word32
  , maxPlaybackBehavior :: Maybe Word32
  , subcategories       :: [EventCategory]
  } deriving (Show)

getEventCategory :: FEVVersion -> Get EventCategory
getEventCategory version = do
  name                <- string
  volumeFieldRatio    <- getFloatle
  pitch               <- getFloatle
  maxPlaybacks        <- case version of
    FEVVersion26 -> return Nothing
    FEVVersion2C -> Just <$> getWord32le
  maxPlaybackBehavior <- case version of
    FEVVersion26 -> return Nothing
    FEVVersion2C -> Just <$> getWord32le
  subcategories       <- lenArray $ getEventCategory version
  return EventCategory{..}

data EventGroup = EventGroup
  { name           :: B.ByteString
  , userProperties :: [UserProperty]
  , subgroups      :: [EventGroup]
  , events         :: [Event]
  } deriving (Show)

getEventGroup :: FEVVersion -> Get EventGroup
getEventGroup version = do
  name           <- string
  userProperties <- lenArray $ do
    fail "Unsupported .fev feature: user properties"
  subgroupsCount <- getWord32le
  eventsCount    <- getWord32le
  subgroups      <- replicateM (fromIntegral subgroupsCount) $ getEventGroup version
  events         <- replicateM (fromIntegral eventsCount) $ getEvent version
  return EventGroup{..}

data UserProperty = UserProperty
  -- not known
  deriving (Show)

data Event = Event
  { name                     :: B.ByteString
  , unk1                     :: Float
  , unk2                     :: Word32
  , unk3                     :: Word32
  , unk4                     :: Word32
  , unk5                     :: Word32
  , unk6                     :: Word32
  , unk7                     :: Word16
  , unk8                     :: Word16
  , unk9                     :: Float -- min_distance_3d?
  , unk10                    :: Float -- max_distance_3d?
  , unk11                    :: Word16 -- padding?
  , unk12                    :: Word16 -- oneshot and pitch_rand_units?
  , unk13                    :: [Float] -- 11 floats listed in spec?
  , maxPlaybackBehavior      :: Word32
  , unk14                    :: [Float] -- 7 floats/ints but not sure which these are
  , layers                   :: [EventLayer]
  , parameters               :: [Parameter]
  , unk15                    :: Word32 -- user properties count?
  , parentEventCategoryNames :: [B.ByteString]
  } deriving (Show)

getEvent :: FEVVersion -> Get Event
getEvent version = do
  name                     <- string
  unk1                     <- getFloatle
  unk2                     <- getWord32le
  unk3                     <- getWord32le
  unk4                     <- getWord32le
  unk5                     <- getWord32le
  unk6                     <- getWord32le
  unk7                     <- getWord16le
  unk8                     <- getWord16le
  unk9                     <- getFloatle
  unk10                    <- getFloatle
  unk11                    <- getWord16le
  unk12                    <- getWord16le
  unk13                    <- replicateM 11 getFloatle
  maxPlaybackBehavior      <- getWord32le
  unk14                    <- case version of
    FEVVersion26 -> replicateM 7 getFloatle
    FEVVersion2C -> replicateM 9 getFloatle
  layers                   <- lenArray $ getEventLayer version
  parameters               <- lenArray getParameter
  unk15                    <- getWord32le
  parentEventCategoryNames <- lenArray string
  return Event{..}

data EventLayer = EventLayer
  { name              :: Maybe B.ByteString
  , magic             :: Word16 -- always 2?
  , priority          :: Int16
  , controlParameter  :: Either B.ByteString Int16
  , soundDefInstances :: [SoundDefInstance]
  , envelopes         :: [Envelope]
  } deriving (Show)

getEventLayer :: FEVVersion -> Get EventLayer
getEventLayer version = do
  name                 <- case version of
    FEVVersion26 -> Just <$> string
    FEVVersion2C -> return Nothing
  magic                <- getWord16le
  priority             <- getInt16le
  controlParameter     <- case version of
    FEVVersion26 -> Left <$> string
    FEVVersion2C -> Right <$> getInt16le
  numSoundDefInstances <- getWord16le
  numEnvelopes         <- getWord16le
  soundDefInstances    <- replicateM (fromIntegral numSoundDefInstances) $ getSoundDefInstance version
  envelopes            <- replicateM (fromIntegral numEnvelopes) $ getEnvelope version
  return EventLayer{..}

data SoundDefInstance = SoundDefInstance
  { nameOrIndex             :: Either B.ByteString Word16
  , soundStart              :: Float
  , soundLength             :: Float
  , unk1                    :: Word32
  , unk2                    :: Word32
  , unk3                    :: Int32
  , padding                 :: Word32
  , loopCount               :: Word32
  , autopitchEnabled        :: Word32
  , autopitchReferencePoint :: Word32
  , autopitchAtMin          :: Word32
  , fineTune                :: Word32
  , volume                  :: Float
  , crossfadeInLength       :: Float
  , crossfadeOutLength      :: Float
  , crossfadeInType         :: Word32
  , crossfadeOutType        :: Word32
  } deriving (Show)

getSoundDefInstance :: FEVVersion -> Get SoundDefInstance
getSoundDefInstance version = do
  nameOrIndex             <- case version of
    FEVVersion26 -> Left <$> string
    FEVVersion2C -> Right <$> getWord16le
  soundStart              <- getFloatle
  soundLength             <- getFloatle
  unk1                    <- getWord32le
  unk2                    <- getWord32le
  unk3                    <- getInt32le
  padding                 <- getWord32le
  loopCount               <- getWord32le
  autopitchEnabled        <- getWord32le
  autopitchReferencePoint <- getWord32le
  autopitchAtMin          <- getWord32le
  fineTune                <- getWord32le
  volume                  <- getFloatle
  crossfadeInLength       <- getFloatle
  crossfadeOutLength      <- getFloatle
  crossfadeInType         <- getWord32le
  crossfadeOutType        <- getWord32le
  return SoundDefInstance{..}

data Envelope = Envelope
  { envelopeID :: Maybe B.ByteString
  , parent     :: Either B.ByteString Int32
  , name       :: B.ByteString
  , unk1       :: Word32
  , unk2       :: Word32
  , points     :: [Point]
  , unk3       :: Word32
  , unk4       :: Word32
  } deriving (Show)

getEnvelope :: FEVVersion -> Get Envelope
getEnvelope version = do
  envelopeID <- case version of
    FEVVersion26 -> Just <$> string
    FEVVersion2C -> return Nothing
  parent     <- case version of
    FEVVersion26 -> Left <$> string
    FEVVersion2C -> Right <$> getInt32le
  name       <- string
  unk1       <- getWord32le
  unk2       <- getWord32le
  points     <- lenArray getPoint
  unk3       <- getWord32le
  unk4       <- getWord32le
  return Envelope{..}

data Point = Point
  { x              :: Float
  , y              :: Float
  , curveShapeType :: Word32
  } deriving (Show)

getPoint :: Get Point
getPoint = do
  x              <- getFloatle
  y              <- getFloatle
  curveShapeType <- getWord32le
  return Point{..}

data Parameter = Parameter
  { name                   :: B.ByteString
  , velocity               :: Float
  , paramMin               :: Float
  , paramMax               :: Float
  , flagsType              :: Word32
  , seekSpeed              :: Float
  , numEnvelopesControlled :: Word32
  , unk0                   :: Word32
  } deriving (Show)

getParameter :: Get Parameter
getParameter = do
  name                   <- string
  velocity               <- getFloatle
  paramMin               <- getFloatle
  paramMax               <- getFloatle
  flagsType              <- getWord32le
  seekSpeed              <- getFloatle
  numEnvelopesControlled <- getWord32le
  unk0                   <- getWord32le
  return Parameter{..}

data SoundDef = SoundDef
  { name      :: B.ByteString
  , unk1      :: Word32
  , unk2      :: Word32
  , unk3      :: Word32
  , unk4      :: Word32
  , unk5      :: Float
  , unk6      :: Word32
  , unk7      :: Float
  , unk8      :: Float
  , unk9      :: Float
  , unk10     :: [Word32]
  , waveforms :: [Waveform]
  } deriving (Show)

getSoundDef :: FEVVersion -> Get SoundDef
getSoundDef version = do
  name      <- string
  unk1      <- getWord32le
  unk2      <- getWord32le
  unk3      <- getWord32le
  unk4      <- getWord32le
  unk5      <- getFloatle
  unk6      <- getWord32le
  unk7      <- getFloatle
  unk8      <- getFloatle
  unk9      <- getFloatle
  unk10     <- case version of
    FEVVersion26 -> replicateM 5 getWord32le
    FEVVersion2C -> replicateM 6 getWord32le
  waveforms <- lenArray getWaveform
  return SoundDef{..}

data Waveform = Waveform
  { padding     :: Word32
  , weight      :: Word32
  , name        :: B.ByteString
  , bankName    :: B.ByteString
  , indexInBank :: Word32
  , playtime    :: Word32
  } deriving (Show)

getWaveform :: Get Waveform
getWaveform = do
  padding     <- getWord32le
  weight      <- getWord32le
  name        <- string
  bankName    <- string
  indexInBank <- getWord32le
  playtime    <- getWord32le
  return Waveform{..}

data Reverb = Reverb
  { name         :: B.ByteString
  , room         :: Int32
  , roomHF       :: Int32
  , roomRolloff  :: Float
  , decayTime    :: Float
  , decayHFRatio :: Float
  , reflections  :: Int32
  , reflectDelay :: Float
  , reverb       :: Int32
  , reverbDelay  :: Float
  , diffusion    :: Float
  , density      :: Float
  , hfReference  :: Float
  , roomLF       :: Int32
  , lfReference  :: Float
  , unk1         :: Word32
  , unk2         :: Word32
  , unk3         :: Float
  , unk4         :: Float
  , unk5         :: Word32
  , unk6         :: Float
  , unk7         :: Word32
  , unk8         :: Word32
  , unk9         :: Word32
  , unk10        :: Word32
  , unk11        :: Word32
  , unk12        :: Word32
  , unk13        :: Float
  , unk14        :: Word32
  , unk15        :: Float
  , unk16        :: Word32
  , unk17        :: Float
  , unk18        :: Float
  , unk19        :: Word32
  } deriving (Show)

getReverb :: Get Reverb
getReverb = do
  name         <- string
  room         <- getInt32le
  roomHF       <- getInt32le
  roomRolloff  <- getFloatle
  decayTime    <- getFloatle
  decayHFRatio <- getFloatle
  reflections  <- getInt32le
  reflectDelay <- getFloatle
  reverb       <- getInt32le
  reverbDelay  <- getFloatle
  diffusion    <- getFloatle
  density      <- getFloatle
  hfReference  <- getFloatle
  roomLF       <- getInt32le
  lfReference  <- getFloatle
  unk1         <- getWord32le
  unk2         <- getWord32le
  unk3         <- getFloatle
  unk4         <- getFloatle
  unk5         <- getWord32le
  unk6         <- getFloatle
  unk7         <- getWord32le
  unk8         <- getWord32le
  unk9         <- getWord32le
  unk10        <- getWord32le
  unk11        <- getWord32le
  unk12        <- getWord32le
  unk13        <- getFloatle
  unk14        <- getWord32le
  unk15        <- getFloatle
  unk16        <- getWord32le
  unk17        <- getFloatle
  unk18        <- getFloatle
  unk19        <- getWord32le
  return Reverb{..}
