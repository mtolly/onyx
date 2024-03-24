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
module Onyx.Audio.FSB.FEV

( binFEV

, FEVVersion(..)
, FEV(..)
, WaveBank(..)
, EventCategory(..)
, EventGroup(..)
, UserProperty(..)
, Event(..)
, EventLayer(..)
, SoundDefInstance(..)
, Envelope(..)
, Point(..)
, Parameter(..)
, SoundDef(..)
, Waveform(..)
, Reverb(..)

) where

import           Control.Monad                    (forM_, replicateM, unless,
                                                   void)
import           Data.Binary.Get
import qualified Data.ByteString                  as B
import           Numeric                          (showHex)
import           Onyx.Codec.Binary
import           Onyx.Harmonix.RockBand.SongCache (floatle)

fevString :: BinaryCodec B.ByteString
fevString = Codec
  { codecIn = do
    len <- getWord32le
    case len of
      0 -> return ""
      _ -> do
        bs <- getByteString $ fromIntegral len
        unless (B.last bs == 0) $ fail $ "Invalid string: " <> show bs
        return $ B.init bs
  , codecOut = fmapArg $ \bs -> if B.null bs
    then putWord32le 0
    else do
      putWord32le $ fromIntegral $ B.length bs + 1
      putByteString bs
      putWord8 0
  }

fevLenArray :: BinaryCodec a -> BinaryCodec [a]
fevLenArray c = Codec
  { codecIn = do
    len <- getWord32le
    replicateM (fromIntegral len) $ codecIn c
  , codecOut = fmapArg $ \xs -> do
    putWord32le $ fromIntegral $ length xs
    forM_ xs $ codecOut c
  }

fevLenArray2 :: (Integral len) => BinaryCodec len -> BinaryCodec a -> BinaryCodec b -> BinaryCodec ([a], [b])
fevLenArray2 clen c1 c2 = Codec
  { codecIn = do
    len1 <- codecIn clen
    len2 <- codecIn clen
    xs <- replicateM (fromIntegral len1) $ codecIn c1
    ys <- replicateM (fromIntegral len2) $ codecIn c2
    return (xs, ys)
  , codecOut = fmapArg $ \(xs, ys) -> do
    void $ codecOut clen $ fromIntegral $ length xs
    void $ codecOut clen $ fromIntegral $ length ys
    forM_ xs $ codecOut c1
    forM_ ys $ codecOut c2
  }

fevMaybe :: Bool -> BinaryCodec a -> BinaryCodec (Maybe a)
fevMaybe parseJust c = Codec
  { codecIn = if parseJust
    then Just <$> codecIn c
    else return Nothing
  , codecOut = fmapArg $ mapM_ $ codecOut c
  }

fevEither :: Bool -> BinaryCodec a -> BinaryCodec b -> BinaryCodec (Either a b)
fevEither parseRight c1 c2 = Codec
  { codecIn = if parseRight
    then Right <$> codecIn c2
    else Left <$> codecIn c1
  , codecOut = fmapArg $ either (void . codecOut c1) (void . codecOut c2)
  }

-- multiple versions show up in the rock revolution files...?
data FEVVersion
  = FEVVersion26 -- 00 00 26 00, seen on 360 disc
  | FEVVersion2C -- 00 00 2C 00, seen on 360 disc
  -- TODO other versions seen in ps3 disc: 00 00 32 00, 00 00 34 00
  | FEVVersion35 -- 00 00 35 00, seen on ps3 dlc (She's the Bug)
  deriving (Eq, Ord, Show)

data FEV = FEV
  { version               :: FEVVersion
  , unkOffset1            :: Maybe Word32
  , unkOffset2            :: Maybe Word32
  , projectName           :: B.ByteString
  , waveBanks             :: [WaveBank]
  , topLevelEventCategory :: EventCategory
  , topLevelEventGroups   :: [EventGroup]
  , soundDefProperties    :: Maybe [SoundDefProperty]
  , soundDefs             :: [SoundDef]
  , reverbs               :: Either [Reverb] ReverbNew
  } deriving (Show)

-- traceMe x = do
--   posn <- Codec { codecIn = bytesRead, codecOut = \_ -> return 0 }
--   traceShow ("0x" <> showHex posn "", x) $ return ()

binFEV :: BinaryCodec FEV
binFEV = do
  const () =. Codec
    { codecIn = getByteString 4 >>= \case
      "FEV1" -> return ()
      magic  -> fail $ "Invalid .fev magic identifier: " <> show magic
    , codecOut = fmapArg $ \() -> putByteString "FEV1"
    }
  version               <- (.version) =. Codec
    { codecIn = getWord32le >>= \case
      0x260000 -> return FEVVersion26
      0x2C0000 -> return FEVVersion2C
      0x350000 -> return FEVVersion35
      n        -> fail $ "Unsupported .fev version: 0x" <> showHex n ""
    , codecOut = fmapArg $ putWord32le . \case
      FEVVersion26 -> 0x260000
      FEVVersion2C -> 0x2C0000
      FEVVersion35 -> 0x350000
    }
  unkOffset1            <- (.unkOffset1           ) =. fevMaybe (version >= FEVVersion35) word32le
  unkOffset2            <- (.unkOffset2           ) =. fevMaybe (version >= FEVVersion35) word32le
  projectName           <- (.projectName          ) =. fevString
  waveBanks             <- (.waveBanks            ) =. fevLenArray binWaveBank
  topLevelEventCategory <- (.topLevelEventCategory) =. binEventCategory version
  topLevelEventGroups   <- (.topLevelEventGroups  ) =. fevLenArray (binEventGroup version)
  soundDefProperties    <- (.soundDefProperties   ) =. fevMaybe (version >= FEVVersion35) (fevLenArray $ binSoundDefProperty version)
  soundDefs             <- (.soundDefs            ) =. fevLenArray (binSoundDef   version)
  reverbs               <- (.reverbs              ) =. fevEither (version >= FEVVersion35)
    (fevLenArray binReverb)
    (binReverbNew version)
  const () =. Codec
    { codecIn = isEmpty >>= \case
      True  -> return ()
      False -> fail "Failed to parse all data in .fev file"
    , codecOut = fmapArg $ \_ -> return ()
    }
  return FEV{..}

data WaveBank = WaveBank
  { bankType       :: Word32
  , bankMaxStreams :: Word32
  , bankName       :: B.ByteString
  } deriving (Show)

binWaveBank :: BinaryCodec WaveBank
binWaveBank = do
  bankType       <- (.bankType      ) =. word32le
  bankMaxStreams <- (.bankMaxStreams) =. word32le
  bankName       <- (.bankName      ) =. fevString
  return WaveBank{..}

data EventCategory = EventCategory
  { name                :: B.ByteString
  , volumeFieldRatio    :: Float
  , pitch               :: Float
  , maxPlaybacks        :: Maybe Word32
  , maxPlaybackBehavior :: Maybe Word32
  , subcategories       :: [EventCategory]
  } deriving (Show)

binEventCategory :: FEVVersion -> BinaryCodec EventCategory
binEventCategory version = do
  name                <- (.name               ) =. fevString
  volumeFieldRatio    <- (.volumeFieldRatio   ) =. floatle
  pitch               <- (.pitch              ) =. floatle
  maxPlaybacks        <- (.maxPlaybacks       ) =. fevMaybe (version >= FEVVersion2C) word32le
  maxPlaybackBehavior <- (.maxPlaybackBehavior) =. fevMaybe (version >= FEVVersion2C) word32le
  subcategories       <- (.subcategories      ) =. fevLenArray (binEventCategory version)
  return EventCategory{..}

data EventGroup = EventGroup
  { name           :: B.ByteString
  , userProperties :: [UserProperty]
  , subgroups      :: [EventGroup]
  , events         :: [Event]
  } deriving (Show)

binEventGroup :: FEVVersion -> BinaryCodec EventGroup
binEventGroup version = do
  name           <- (.name) =. fevString
  userProperties <- (.userProperties) =. fevLenArray Codec
    { codecIn = fail "Unsupported .fev feature: user properties"
    , codecOut = fmapArg $ \UserProperty -> return ()
    }
  (subgroups, events)
    <- (\layer -> (layer.subgroups, layer.events))
    =. fevLenArray2 word32le
      (binEventGroup version)
      (binEvent version)
  return EventGroup{..}

data UserProperty = UserProperty
  -- not known
  deriving (Show)

data Event = Event
  { type_                    :: Maybe Word32
  , name                     :: B.ByteString
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

binEvent :: FEVVersion -> BinaryCodec Event
binEvent version = do
  -- TODO we can probably only parse COMPLEX (8) for type_
  type_                    <- (.type_                   ) =. fevMaybe (version >= FEVVersion35) word32le
  name                     <- (.name                    ) =. fevString
  unk1                     <- (.unk1                    ) =. floatle
  unk2                     <- (.unk2                    ) =. word32le
  unk3                     <- (.unk3                    ) =. word32le
  unk4                     <- (.unk4                    ) =. word32le
  unk5                     <- (.unk5                    ) =. word32le
  unk6                     <- (.unk6                    ) =. word32le
  unk7                     <- (.unk7                    ) =. word16le
  unk8                     <- (.unk8                    ) =. word16le
  unk9                     <- (.unk9                    ) =. floatle
  unk10                    <- (.unk10                   ) =. floatle
  unk11                    <- (.unk11                   ) =. word16le
  unk12                    <- (.unk12                   ) =. word16le
  unk13                    <- (.unk13                   ) =. fixedArray 11 floatle
  maxPlaybackBehavior      <- (.maxPlaybackBehavior     ) =. word32le
  unk14                    <- (.unk14                   ) =. case version of
    FEVVersion26 -> fixedArray 7 floatle
    FEVVersion2C -> fixedArray 9 floatle
    FEVVersion35 -> fixedArray 10 floatle
  layers                   <- (.layers                  ) =. fevLenArray (binEventLayer version)
  parameters               <- (.parameters              ) =. fevLenArray binParameter
  unk15                    <- (.unk15                   ) =. word32le
  parentEventCategoryNames <- (.parentEventCategoryNames) =. fevLenArray fevString
  return Event{..}

data EventLayer = EventLayer
  { name              :: Maybe B.ByteString
  , magic             :: Word16 -- always 2?
  , priority          :: Int16
  , controlParameter  :: Either B.ByteString Int16
  , soundDefInstances :: [SoundDefInstance]
  , envelopes         :: [Envelope]
  } deriving (Show)

binEventLayer :: FEVVersion -> BinaryCodec EventLayer
binEventLayer version = do
  name                 <- (.name            ) =. fevMaybe (version == FEVVersion26) fevString
  magic                <- (.magic           ) =. word16le
  priority             <- (.priority        ) =. int16le
  controlParameter     <- (.controlParameter) =. fevEither (version >= FEVVersion2C) fevString int16le
  (soundDefInstances, envelopes)
    <- (\layer -> (layer.soundDefInstances, layer.envelopes))
    =. fevLenArray2 word16le
      (binSoundDefInstance version)
      (binEnvelope version)
  return EventLayer{..}

data SoundDefInstance = SoundDefInstance
  { nameOrIndex             :: Either B.ByteString Word16
  , soundStart              :: Float
  , soundLength             :: Float
  , unk1                    :: Word32
  , unk2                    :: Word32
  , unk3                    :: Int32
  -- not sure exactly which fields change across versions
  , padding                 :: Maybe Word32
  , loopCount               :: Maybe Word32
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

binSoundDefInstance :: FEVVersion -> BinaryCodec SoundDefInstance
binSoundDefInstance version = do
  nameOrIndex             <- (.nameOrIndex            ) =. fevEither (version >= FEVVersion2C) fevString word16le
  soundStart              <- (.soundStart             ) =. floatle
  soundLength             <- (.soundLength            ) =. floatle
  unk1                    <- (.unk1                   ) =. word32le
  unk2                    <- (.unk2                   ) =. word32le
  unk3                    <- (.unk3                   ) =. int32le
  padding                 <- (.padding                ) =. fevMaybe (version < FEVVersion35) word32le
  loopCount               <- (.loopCount              ) =. fevMaybe (version < FEVVersion35) word32le
  autopitchEnabled        <- (.autopitchEnabled       ) =. word32le
  autopitchReferencePoint <- (.autopitchReferencePoint) =. word32le
  autopitchAtMin          <- (.autopitchAtMin         ) =. word32le
  fineTune                <- (.fineTune               ) =. word32le
  volume                  <- (.volume                 ) =. floatle
  crossfadeInLength       <- (.crossfadeInLength      ) =. floatle
  crossfadeOutLength      <- (.crossfadeOutLength     ) =. floatle
  crossfadeInType         <- (.crossfadeInType        ) =. word32le
  crossfadeOutType        <- (.crossfadeOutType       ) =. word32le
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

binEnvelope :: FEVVersion -> BinaryCodec Envelope
binEnvelope version = do
  envelopeID <- (.envelopeID) =. fevMaybe (version == FEVVersion26) fevString
  parent     <- (.parent    ) =. fevEither (version >= FEVVersion2C) fevString int32le
  name       <- (.name      ) =. fevString
  unk1       <- (.unk1      ) =. word32le
  unk2       <- (.unk2      ) =. word32le
  points     <- (.points    ) =. fevLenArray binPoint
  unk3       <- (.unk3      ) =. word32le
  unk4       <- (.unk4      ) =. word32le
  return Envelope{..}

data Point = Point
  { x              :: Float
  , y              :: Float
  , curveShapeType :: Word32
  } deriving (Show)

binPoint :: BinaryCodec Point
binPoint = do
  x              <- (.x             ) =. floatle
  y              <- (.y             ) =. floatle
  curveShapeType <- (.curveShapeType) =. word32le
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

binParameter :: BinaryCodec Parameter
binParameter = do
  name                   <- (.name                  ) =. fevString
  velocity               <- (.velocity              ) =. floatle
  paramMin               <- (.paramMin              ) =. floatle
  paramMax               <- (.paramMax              ) =. floatle
  flagsType              <- (.flagsType             ) =. word32le
  seekSpeed              <- (.seekSpeed             ) =. floatle
  numEnvelopesControlled <- (.numEnvelopesControlled) =. word32le
  unk0                   <- (.unk0                  ) =. word32le
  return Parameter{..}

data SoundDefProperty = SoundDefProperty
  { play_mode                   :: Word32
  , min_spawn_time              :: Word32
  , max_spawn_time              :: Word32
  , max_spawned_sounds          :: Word32
  , volume_field_ratio_1        :: Float
  , volume_rand_method          :: Word32
  , volume_rand_min_field_ratio :: Float
  , volume_rand_max_field_ratio :: Float
  , volume_field_ratio_2        :: Float
  , pitch                       :: Float
  , pitch_rand_method           :: Word32
  -- one of these fields is missing in version 35 relative to dark souls spec, not sure which though
  , pitch_rand_min_field_ratio  :: Float
  , pitch_rand_max_field_ratio  :: Float
  , pitch_rand                  :: Float
  , recalc_pitch_rand           :: Word32
  -- , position_3d_randomization   :: Float
  } deriving (Show)

binSoundDefProperty :: FEVVersion -> BinaryCodec SoundDefProperty
binSoundDefProperty _version = do
  play_mode                   <- (.play_mode                  ) =. word32le
  min_spawn_time              <- (.min_spawn_time             ) =. word32le
  max_spawn_time              <- (.max_spawn_time             ) =. word32le
  max_spawned_sounds          <- (.max_spawned_sounds         ) =. word32le
  volume_field_ratio_1        <- (.volume_field_ratio_1       ) =. floatle
  volume_rand_method          <- (.volume_rand_method         ) =. word32le
  volume_rand_min_field_ratio <- (.volume_rand_min_field_ratio) =. floatle
  volume_rand_max_field_ratio <- (.volume_rand_max_field_ratio) =. floatle
  volume_field_ratio_2        <- (.volume_field_ratio_2       ) =. floatle
  pitch                       <- (.pitch                      ) =. floatle
  pitch_rand_method           <- (.pitch_rand_method          ) =. word32le
  pitch_rand_min_field_ratio  <- (.pitch_rand_min_field_ratio ) =. floatle
  pitch_rand_max_field_ratio  <- (.pitch_rand_max_field_ratio ) =. floatle
  pitch_rand                  <- (.pitch_rand                 ) =. floatle
  recalc_pitch_rand           <- (.recalc_pitch_rand          ) =. word32le
  return SoundDefProperty{..}

data SoundDef = SoundDef
  { name                       :: B.ByteString
  -- not sure if one of these unks is the sound_def_properties_index seen in version 35
  , unk1                       :: Maybe Word32
  , unk2                       :: Maybe Word32
  , unk3                       :: Maybe Word32
  , unk4                       :: Maybe Word32
  , unk5                       :: Maybe Float
  , unk6                       :: Maybe Word32
  , unk7                       :: Maybe Float
  , unk8                       :: Maybe Float
  , unk9                       :: Maybe Float
  , unk10                      :: Maybe [Word32]
  , sound_def_properties_index :: Maybe Word32
  , waveforms                  :: [Waveform]
  } deriving (Show)

binSoundDef :: FEVVersion -> BinaryCodec SoundDef
binSoundDef version = do
  name      <- (.name     ) =. fevString
  unk1      <- (.unk1     ) =. fevMaybe (version < FEVVersion35) word32le
  unk2      <- (.unk2     ) =. fevMaybe (version < FEVVersion35) word32le
  unk3      <- (.unk3     ) =. fevMaybe (version < FEVVersion35) word32le
  unk4      <- (.unk4     ) =. fevMaybe (version < FEVVersion35) word32le
  unk5      <- (.unk5     ) =. fevMaybe (version < FEVVersion35) floatle
  unk6      <- (.unk6     ) =. fevMaybe (version < FEVVersion35) word32le
  unk7      <- (.unk7     ) =. fevMaybe (version < FEVVersion35) floatle
  unk8      <- (.unk8     ) =. fevMaybe (version < FEVVersion35) floatle
  unk9      <- (.unk9     ) =. fevMaybe (version < FEVVersion35) floatle
  unk10     <- (.unk10    ) =. fevMaybe (version < FEVVersion35) (case version of
    FEVVersion26 -> fixedArray 5 word32le
    FEVVersion2C -> fixedArray 6 word32le
    FEVVersion35 -> fixedArray 0 word32le -- shouldn't happen
    )
  sound_def_properties_index <- (.sound_def_properties_index) =. fevMaybe (version >= FEVVersion35) word32le
  waveforms <- (.waveforms) =. fevLenArray binWaveform
  return SoundDef{..}

data Waveform = Waveform
  { padding     :: Word32
  , weight      :: Word32
  , name        :: B.ByteString
  , bankName    :: B.ByteString
  , indexInBank :: Word32
  , playtime    :: Word32
  } deriving (Show)

binWaveform :: BinaryCodec Waveform
binWaveform = do
  padding     <- (.padding    ) =. word32le
  weight      <- (.weight     ) =. word32le
  name        <- (.name       ) =. fevString
  bankName    <- (.bankName   ) =. fevString
  indexInBank <- (.indexInBank) =. word32le
  playtime    <- (.playtime   ) =. word32le
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

binReverb :: BinaryCodec Reverb
binReverb = do
  name         <- (.name        ) =. fevString
  room         <- (.room        ) =. int32le
  roomHF       <- (.roomHF      ) =. int32le
  roomRolloff  <- (.roomRolloff ) =. floatle
  decayTime    <- (.decayTime   ) =. floatle
  decayHFRatio <- (.decayHFRatio) =. floatle
  reflections  <- (.reflections ) =. int32le
  reflectDelay <- (.reflectDelay) =. floatle
  reverb       <- (.reverb      ) =. int32le
  reverbDelay  <- (.reverbDelay ) =. floatle
  diffusion    <- (.diffusion   ) =. floatle
  density      <- (.density     ) =. floatle
  hfReference  <- (.hfReference ) =. floatle
  roomLF       <- (.roomLF      ) =. int32le
  lfReference  <- (.lfReference ) =. floatle
  unk1         <- (.unk1        ) =. word32le
  unk2         <- (.unk2        ) =. word32le
  unk3         <- (.unk3        ) =. floatle
  unk4         <- (.unk4        ) =. floatle
  unk5         <- (.unk5        ) =. word32le
  unk6         <- (.unk6        ) =. floatle
  unk7         <- (.unk7        ) =. word32le
  unk8         <- (.unk8        ) =. word32le
  unk9         <- (.unk9        ) =. word32le
  unk10        <- (.unk10       ) =. word32le
  unk11        <- (.unk11       ) =. word32le
  unk12        <- (.unk12       ) =. word32le
  unk13        <- (.unk13       ) =. floatle
  unk14        <- (.unk14       ) =. word32le
  unk15        <- (.unk15       ) =. floatle
  unk16        <- (.unk16       ) =. word32le
  unk17        <- (.unk17       ) =. floatle
  unk18        <- (.unk18       ) =. floatle
  unk19        <- (.unk19       ) =. word32le
  return Reverb{..}

data ReverbNew = ReverbNew
  { unk1  :: Word32
  , unk2  :: Word32
  , magic :: B.ByteString
  } deriving (Show)

binReverbNew :: FEVVersion -> BinaryCodec ReverbNew
binReverbNew _version = do
  unk1 <- (.unk1) =. word32le
  unk2 <- (.unk2) =. word32le
  magic <- (.magic) =. Codec
    { codecIn = do
      bs <- getByteString 4
      case bs of
        "comp" -> return bs
        _      -> fail $ "Unexpected reverb magic (not 'comp'): " <> show bs
    , codecOut = fmapArg putByteString
    }
  return ReverbNew{..}
