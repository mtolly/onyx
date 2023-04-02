{-
Ported from LibForge
https://github.com/maxton/LibForge
-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Harmonix.RockBand.RB4.RBMid where

import           Control.Monad                      (forM, when)
import           Data.Bits
import qualified Data.ByteString                    as B
import qualified Data.EventList.Absolute.TimeBody   as ATB
import qualified Data.EventList.Relative.TimeBody   as RTB
import           Data.Maybe                         (isJust, isNothing,
                                                     listToMaybe)
import           Debug.Trace
import           Numeric
import           Onyx.Codec.Binary
import           Onyx.Harmonix.RockBand.RB4.SongDTA (boolByte, skipBytes)
import           Onyx.Harmonix.RockBand.SongCache   (floatle, lenArray)
import           Onyx.StackTrace
import qualified Sound.MIDI.File                    as F
import qualified Sound.MIDI.File.Event              as E
import qualified Sound.MIDI.File.Event.Meta         as Meta
import qualified Sound.MIDI.Message.Channel         as C
import qualified Sound.MIDI.Message.Channel.Voice   as V

debugPosn :: String -> CodecFor Get PutM b ()
debugPosn s = const () =. Codec
  { codecOut = return
  , codecIn = do
    n <- bytesRead
    trace (s <> ": " <> showHex n "") $ return ()
  }

optionalData :: Bool -> BinaryCodec a -> BinaryCodec (Maybe a)
optionalData b c = Codec
  { codecIn = if b
    then Just <$> codecIn c
    else return Nothing
  , codecOut = fmapArg $ mapM_ $ codecOut c
  }

data RBMid = RBMid
  { rbmid_Format                :: Word32 -- should be 0x10 (RB4) or 0x2F (RBVR)
  , rbmid_TrackInfo             :: [TrackInfo]
  , rbmid_DrumFills             :: [DrumFillInfo]
  , rbmid_Anims                 :: [FakeTrack]
  , rbmid_ProMarkers            :: [TomMarker]
  , rbmid_LaneMarkers           :: [LaneMarkers]
  , rbmid_GtrTrills             :: [GtrTrills]
  , rbmid_DrumMixes             :: [DrumMixes]
  , rbmid_GemTracks             :: [GemTrack]
  , rbmid_OverdriveSoloSections :: [Sections]
  , rbmid_VocalTracks           :: [VocalTrack]
  , rbmid_UnknownOne            :: Int32
  , rbmid_UnknownNegOne         :: Int32
  , rbmid_UnknownHundred        :: Float
  , rbmid_Unknown4              :: [Ticked Float]
  , rbmid_VocalRange            :: [VocalTrackRange]
  , rbmid_HopoThreshold         :: Int32
  , rbmid_NumPlayableTracks     :: Word32
  , rbmid_FinalEventTick        :: Word32
  , rbmid_UnkVRTick             :: Maybe Word32
  , rbmid_UnknownZeroByte       :: Word8
  , rbmid_PreviewStartMillis    :: Float
  , rbmid_PreviewEndMillis      :: Float
  , rbmid_HandMaps              :: [[Ticked Int32]]
  , rbmid_GuitarLeftHandPos     :: [[HandPos]]
  , rbmid_StrumMaps             :: [[Ticked Int32]]
  , rbmid_MarkupSoloNotes1      :: [MarkupSoloNotes]
  , rbmid_MarkupLoop1           :: [Ticked Word32]
  , rbmid_MarkupChords1         :: [MarkupChord]
  , rbmid_MarkupSoloNotes2      :: [MarkupSoloNotes]
  , rbmid_MarkupSoloNotes3      :: [MarkupSoloNotes]
  , rbmid_MarkupLoop2           :: [Ticked Word32]
  , rbmid_VREvents              :: Maybe RBVREvents
  , rbmid_MidiFileResource      :: MidiFileResource
  } deriving (Show)

instance Bin RBMid where
  bin = do
    rbmid_Format                <- rbmid_Format                =. word32le
    let isRBVR = rbmid_Format == 0x2F
    rbmid_TrackInfo             <- rbmid_TrackInfo             =. lenArray bin
    rbmid_DrumFills             <- rbmid_DrumFills             =. lenArray bin
    rbmid_Anims                 <- rbmid_Anims                 =. lenArray bin
    rbmid_ProMarkers            <- rbmid_ProMarkers            =. lenArray bin
    rbmid_LaneMarkers           <- rbmid_LaneMarkers           =. lenArray bin
    rbmid_GtrTrills             <- rbmid_GtrTrills             =. lenArray bin
    rbmid_DrumMixes             <- rbmid_DrumMixes             =. lenArray bin
    rbmid_GemTracks             <- rbmid_GemTracks             =. lenArray bin
    rbmid_OverdriveSoloSections <- rbmid_OverdriveSoloSections =. lenArray bin
    rbmid_VocalTracks           <- rbmid_VocalTracks           =. lenArray bin
    rbmid_UnknownOne            <- rbmid_UnknownOne            =. int32le
    rbmid_UnknownNegOne         <- rbmid_UnknownNegOne         =. int32le
    rbmid_UnknownHundred        <- rbmid_UnknownHundred        =. floatle
    rbmid_Unknown4              <- rbmid_Unknown4              =. lenArray (ticked floatle)
    rbmid_VocalRange            <- rbmid_VocalRange            =. lenArray bin
    rbmid_HopoThreshold         <- rbmid_HopoThreshold         =. int32le
    rbmid_NumPlayableTracks     <- rbmid_NumPlayableTracks     =. word32le
    rbmid_FinalEventTick        <- rbmid_FinalEventTick        =. word32le
    rbmid_UnkVRTick             <- rbmid_UnkVRTick             =. optionalData isRBVR word32le
    rbmid_UnknownZeroByte       <- rbmid_UnknownZeroByte       =. word8
    rbmid_PreviewStartMillis    <- rbmid_PreviewStartMillis    =. floatle
    rbmid_PreviewEndMillis      <- rbmid_PreviewEndMillis      =. floatle
    rbmid_HandMaps              <- rbmid_HandMaps              =. lenArray (lenArray $ ticked int32le)
    rbmid_GuitarLeftHandPos     <- rbmid_GuitarLeftHandPos     =. lenArray (lenArray bin)
    rbmid_StrumMaps             <- rbmid_StrumMaps             =. lenArray (lenArray $ ticked int32le)
    rbmid_MarkupSoloNotes1      <- rbmid_MarkupSoloNotes1      =. lenArray bin
    rbmid_MarkupLoop1           <- rbmid_MarkupLoop1           =. lenArray (ticked word32le)
    rbmid_MarkupChords1         <- rbmid_MarkupChords1         =. lenArray bin
    rbmid_MarkupSoloNotes2      <- rbmid_MarkupSoloNotes2      =. lenArray bin
    rbmid_MarkupSoloNotes3      <- rbmid_MarkupSoloNotes3      =. lenArray bin
    rbmid_MarkupLoop2           <- rbmid_MarkupLoop2           =. lenArray (ticked word32le)
    rbmid_VREvents              <- rbmid_VREvents              =. optionalData isRBVR bin
    rbmid_MidiFileResource      <- rbmid_MidiFileResource      =. bin
    return RBMid{..}

data TrackInfo = TrackInfo
  { trk_Name   :: B.ByteString
  , trk_Lyrics :: [Ticked B.ByteString]
  , trk_Unk1   :: Word32
  , trk_Unk2   :: Word32
  , trk_Unk3   :: Word8
  } deriving (Show)

lenString :: BinaryCodec B.ByteString
lenString = Codec
  { codecIn = do
    len <- getWord32le
    getByteString $ fromIntegral len
  , codecOut = fmapArg $ \b -> do
    putWord32le $ fromIntegral $ B.length b
    putByteString b
  }

instance Bin TrackInfo where
  bin = do
    trk_Name   <- trk_Name   =. lenString
    trk_Lyrics <- trk_Lyrics =. lenArray (ticked lenString)
    trk_Unk1   <- trk_Unk1   =. word32le
    trk_Unk2   <- trk_Unk2   =. word32le
    trk_Unk3   <- trk_Unk3   =. word8
    return TrackInfo{..}

data Ticked a = Ticked
  { t_Tick :: Word32
  , t_Data :: a
  } deriving (Show)

ticked :: BinaryCodec a -> BinaryCodec (Ticked a)
ticked c = do
  t_Tick <- t_Tick =. word32le
  t_Data <- t_Data =. c
  return Ticked{..}

data DrumFillInfo = DrumFillInfo
  { df_Lanes :: [Ticked Word32]
  , df_Fills :: [FillExtent]
  } deriving (Show)

instance Bin DrumFillInfo where
  bin = do
    df_Lanes <- df_Lanes =. lenArray (ticked word32le)
    df_Fills <- df_Fills =. lenArray bin
    return DrumFillInfo{..}

data FillExtent = FillExtent
  { fe_FillStartTick :: Word32
  , fe_FillEndTick   :: Word32
  , fe_IsBRE         :: Bool
  } deriving (Show)

instance Bin FillExtent where
  bin = do
    fe_FillStartTick <- fe_FillStartTick =. word32le
    fe_FillEndTick   <- fe_FillEndTick   =. word32le
    fe_IsBRE         <- fe_IsBRE         =. boolByte
    return FillExtent{..}

data FakeTrack = FakeTrack
  { ft_Name      :: B.ByteString
  , ft_Animation :: GameGemDB
  } deriving (Show)

instance Bin FakeTrack where
  bin = do
    ft_Name      <- ft_Name      =. lenString
    ft_Animation <- ft_Animation =. bin
    return FakeTrack{..}

data GameGemDB = GameGemDB
  { ggdb_GemLists :: [GameGemList]
  , ggdb_End      :: Word32
  } deriving (Show)

instance Bin GameGemDB where
  bin = do
    ggdb_GemLists <- ggdb_GemLists =. lenArray bin
    ggdb_End      <- ggdb_End      =. word32le
    return GameGemDB{..}

data GameGemList = GameGemList
  { ggl_UnknownAA :: Word32
  , ggl_Gems      :: [GameGem]
  } deriving (Show)

instance Bin GameGemList where
  bin = do
    ggl_UnknownAA <- ggl_UnknownAA =. word32le
    ggl_Gems      <- ggl_Gems      =. lenArray bin
    return GameGemList{..}

data GameGem = GameGem
  { gg_StartMillis  :: Float
  , gg_StartTicks   :: Word32
  , gg_LengthMillis :: Word16
  , gg_LengthTicks  :: Word16
  , gg_Lanes        :: Word32
  {-
    lanes bits:
    int lane_1 : 1;
    int lane_2 : 1;
    int lane_3 : 1;
    int lane_4 : 1;
    int lane_5 : 1;
    int additional_real_keys_lanes : 27;
  -}
  , gg_IsHOPO       :: Bool
  , gg_NoTail       :: Bool
  , gg_Flag3        :: Word8
  , gg_Flag4        :: Word8
  , gg_Flag5        :: Word8
  , gg_Flag6        :: Word8
  } deriving (Show)

instance Bin GameGem where
  bin = do
    gg_StartMillis  <- gg_StartMillis  =. floatle
    gg_StartTicks   <- gg_StartTicks   =. word32le
    gg_LengthMillis <- gg_LengthMillis =. word16le
    gg_LengthTicks  <- gg_LengthTicks  =. word16le
    gg_Lanes        <- gg_Lanes        =. word32le
    gg_IsHOPO       <- gg_IsHOPO       =. boolByte
    gg_NoTail       <- gg_NoTail       =. boolByte
    gg_Flag3        <- gg_Flag3        =. word8
    gg_Flag4        <- gg_Flag4        =. word8
    gg_Flag5        <- gg_Flag5        =. word8
    gg_Flag6        <- gg_Flag6        =. word8
    return GameGem{..}

data TomMarker = TomMarker
  { dm_ProMarkers :: [Ticked Word32] -- bits 2,3,4 are Y,B,G pro markers
  , dm_Unknown1   :: Word32
  , dm_Unknown2   :: Word32
  } deriving (Show)

instance Bin TomMarker where
  bin = do
    dm_ProMarkers  <- dm_ProMarkers  =. lenArray (ticked word32le)
    dm_Unknown1    <- dm_Unknown1    =. word32le
    dm_Unknown2    <- dm_Unknown2    =. word32le
    return TomMarker{..}

data LaneMarkers = LaneMarkers
  { lms_Difficulties :: [[LaneMarker]]
  } deriving (Show)

instance Bin LaneMarkers where
  bin = do
    lms_Difficulties <- lms_Difficulties =. lenArray (lenArray bin)
    return LaneMarkers{..}

data LaneMarker = LaneMarker
  { lm_StartTick :: Word32
  , lm_EndTick   :: Word32
  , lm_Flags     :: Word32
  {-
    flags:
    int Glissando : 1;
    int Trill : 1;
    int Roll_1Lane : 1;
    int Roll_2Lane : 1;
    int Unk : 28;
  -}
  } deriving (Show)

instance Bin LaneMarker where
  bin = do
    lm_StartTick <- lm_StartTick =. word32le
    lm_EndTick   <- lm_EndTick   =. word32le
    lm_Flags     <- lm_Flags     =. word32le
    return LaneMarker{..}

data GtrTrills = GtrTrills
  { gt_Trills :: [[GtrTrill]]
  } deriving (Show)

instance Bin GtrTrills where
  bin = do
    gt_Trills <- gt_Trills =. lenArray (lenArray bin)
    return GtrTrills{..}

data GtrTrill = GtrTrill
  { gt_StartTick :: Word32
  , gt_EndTick   :: Word32
  , gt_Fret1     :: Int32
  , gt_Fret2     :: Int32
  } deriving (Show)

instance Bin GtrTrill where
  bin = do
    gt_StartTick <- gt_StartTick =. word32le
    gt_EndTick   <- gt_EndTick   =. word32le
    gt_Fret1     <- gt_Fret1     =. int32le
    gt_Fret2     <- gt_Fret2     =. int32le
    return GtrTrill{..}

data DrumMixes = DrumMixes
  { mix_Mixes :: [[Ticked B.ByteString]]
  } deriving (Show)

instance Bin DrumMixes where
  bin = do
    mix_Mixes <- mix_Mixes =. lenArray (lenArray $ ticked lenString)
    return DrumMixes{..}

data GemTrack = GemTrack
  { gt_Gems          :: [[Gem]]
  , gt_HopoThreshold :: Int32
  } deriving (Show)

instance Bin GemTrack where
  bin = do
    gt_Gems <- gt_Gems =. lenArray do
      skipBytes 4
      lenArray bin
    gt_HopoThreshold <- gt_HopoThreshold =. int32le
    return GemTrack{..}

data Gem = Gem
  { gem_StartMillis  :: Float
  , gem_StartTicks   :: Word32
  , gem_LengthMillis :: Word16
  , gem_LengthTicks  :: Word16
  , gem_Lanes        :: Int32
  , gem_IsHOPO       :: Bool
  , gem_NoTail       :: Bool
  , gem_ProCymbal    :: Int32
  } deriving (Show)

instance Bin Gem where
  bin = do
    gem_StartMillis  <- gem_StartMillis  =. floatle
    gem_StartTicks   <- gem_StartTicks   =. word32le
    gem_LengthMillis <- gem_LengthMillis =. word16le
    gem_LengthTicks  <- gem_LengthTicks  =. word16le
    gem_Lanes        <- gem_Lanes        =. int32le
    gem_IsHOPO       <- gem_IsHOPO       =. boolByte
    gem_NoTail       <- gem_NoTail       =. boolByte
    gem_ProCymbal    <- gem_ProCymbal    =. int32le
    return Gem{..}

data Sections = Sections
  { s_Sections :: [[[Ticked Word32]]]
  -- 1st dimension: difficulty, 2nd: section type, 3rd: list of sections
  } deriving (Show)

instance Bin Sections where
  bin = do
    s_Sections <- s_Sections =. lenArray (lenArray $ lenArray $ ticked word32le)
    return Sections{..}

data VocalTrack = VocalTrack
  { vt_FakePhraseMarkers     :: [PhraseMarker]
  , vt_AuthoredPhraseMarkers :: [PhraseMarker]
  , vt_Notes                 :: [VocalNote]
  , vt_Percussion            :: [Word32]
  , vt_FreestyleRegions      :: [ODRegion]
  } deriving (Show)

instance Bin VocalTrack where
  bin = do
    vt_FakePhraseMarkers     <- vt_FakePhraseMarkers     =. lenArray bin
    vt_AuthoredPhraseMarkers <- vt_AuthoredPhraseMarkers =. lenArray bin
    vt_Notes                 <- vt_Notes                 =. lenArray bin
    vt_Percussion            <- vt_Percussion            =. lenArray word32le
    vt_FreestyleRegions      <- vt_FreestyleRegions      =. lenArray bin
    return VocalTrack{..}

data PhraseMarker = PhraseMarker
  { pm_StartMillis       :: Float
  , pm_LengthMillis      :: Float
  , pm_StartTicks        :: Word32
  , pm_LengthTicks       :: Word32
  , pm_StartNoteIdx      :: Int32
  , pm_EndNoteIdx        :: Int32
  , pm_HasPitchedVox     :: Bool
  , pm_HasUnpitchedVox   :: Bool
  , pm_LowNote           :: Float
  , pm_HighNote          :: Float
  , pm_PhraseFlags       :: Word8
  , pm_PercussionSection :: Bool
  } deriving (Show)

instance Bin PhraseMarker where
  bin = do
    pm_StartMillis       <- pm_StartMillis        =. floatle
    pm_LengthMillis      <- pm_LengthMillis       =. floatle
    pm_StartTicks        <- pm_StartTicks         =. word32le
    pm_LengthTicks       <- pm_LengthTicks        =. word32le
    pm_StartNoteIdx      <- pm_StartNoteIdx       =. int32le
    pm_EndNoteIdx        <- pm_EndNoteIdx         =. int32le
    pm_HasPitchedVox     <- pm_HasPitchedVox      =. boolByte
    pm_HasUnpitchedVox   <- pm_HasUnpitchedVox    =. boolByte
    skipBytes 9
    pm_LowNote           <- pm_LowNote            =. floatle
    pm_HighNote          <- pm_HighNote           =. floatle
    pm_PhraseFlags       <- pm_PhraseFlags        =. word8
    pm_PercussionSection <- pm_PercussionSection  =. boolByte
    skipBytes 8
    return PhraseMarker{..}

data VocalNote = VocalNote
  { vn_PhraseIndex       :: Int32
  , vn_MidiNote          :: Int32
  , vn_MidiNote2         :: Int32
  , vn_StartMillis       :: Float
  , vn_StartTicks        :: Word32
  , vn_LengthMillis      :: Float
  , vn_LengthTicks       :: Word16
  , vn_Lyric             :: B.ByteString
  , vn_LastNoteInPhrase  :: Bool
  , vn_False1            :: Bool
  , vn_Unpitched         :: Bool
  , vn_UnpitchedGenerous :: Bool
  , vn_RangeDivider      :: Bool
  , vn_PhraseFlags       :: Word8
  , vn_Portamento        :: Bool
  , vn_LyricShift        :: Bool
  , vn_ShowLyric         :: Bool
  } deriving (Show)

instance Bin VocalNote where
  bin = do
    vn_PhraseIndex       <- vn_PhraseIndex        =. int32le
    vn_MidiNote          <- vn_MidiNote           =. int32le
    vn_MidiNote2         <- vn_MidiNote2          =. int32le
    vn_StartMillis       <- vn_StartMillis        =. floatle
    vn_StartTicks        <- vn_StartTicks         =. word32le
    vn_LengthMillis      <- vn_LengthMillis       =. floatle
    vn_LengthTicks       <- vn_LengthTicks        =. word16le
    vn_Lyric             <- vn_Lyric              =. lenString
    vn_LastNoteInPhrase  <- vn_LastNoteInPhrase   =. boolByte
    vn_False1            <- vn_False1             =. boolByte
    vn_Unpitched         <- vn_Unpitched          =. boolByte
    vn_UnpitchedGenerous <- vn_UnpitchedGenerous  =. boolByte
    vn_RangeDivider      <- vn_RangeDivider       =. boolByte
    vn_PhraseFlags       <- vn_PhraseFlags        =. word8
    vn_Portamento        <- vn_Portamento         =. boolByte
    vn_LyricShift        <- vn_LyricShift         =. boolByte
    vn_ShowLyric         <- vn_ShowLyric          =. boolByte
    return VocalNote{..}

data ODRegion = ODRegion
  { od_StartMillis :: Float
  , od_EndMillis   :: Float
  } deriving (Show)

instance Bin ODRegion where
  bin = do
    od_StartMillis <- od_StartMillis =. floatle
    od_EndMillis   <- od_EndMillis   =. floatle
    return ODRegion{..}

data VocalTrackRange = VocalTrackRange
  { vtr_StartMillis :: Float
  , vtr_StartTicks  :: Int32
  , vtr_LowNote     :: Float
  , vtr_HighNote    :: Float
  } deriving (Show)

instance Bin VocalTrackRange where
  bin = do
    vtr_StartMillis <- vtr_StartMillis =. floatle
    vtr_StartTicks  <- vtr_StartTicks  =. int32le
    vtr_LowNote     <- vtr_LowNote     =. floatle
    vtr_HighNote    <- vtr_HighNote    =. floatle
    return VocalTrackRange{..}

data HandPos = HandPos
  { hp_StartTime :: Float
  , hp_Length    :: Float
  , hp_Position  :: Int32
  , hp_Unknown   :: Word8
  } deriving (Show)

instance Bin HandPos where
  bin = do
    hp_StartTime <- hp_StartTime =. floatle
    hp_Length    <- hp_Length    =. floatle
    hp_Position  <- hp_Position  =. int32le
    hp_Unknown   <- hp_Unknown   =. word8
    return HandPos{..}

data MarkupSoloNotes = MarkupSoloNotes
  { msn_StartTick  :: Word32
  , msn_EndTick    :: Word32
  , msn_NoteOffset :: Int32
  } deriving (Show)

instance Bin MarkupSoloNotes where
  bin = do
    msn_StartTick  <- msn_StartTick  =. word32le
    msn_EndTick    <- msn_EndTick    =. word32le
    msn_NoteOffset <- msn_NoteOffset =. int32le
    return MarkupSoloNotes{..}

data MarkupChord = MarkupChord
  { mc_StartTick :: Word32
  , mc_EndTick   :: Word32
  , mc_Pitches   :: [Int32]
  } deriving (Show)

instance Bin MarkupChord where
  bin = do
    mc_StartTick <- mc_StartTick =. word32le
    mc_EndTick   <- mc_EndTick   =. word32le
    mc_Pitches   <- mc_Pitches   =. lenArray int32le
    return MarkupChord{..}

-- TODO

data RBVREvents = RBVREvents
  deriving (Show)

instance Bin RBVREvents where
  bin = do
    _ <- undefined
    return RBVREvents

--------------------------------------------------------------------------------

data MidiFileResource = MidiFileResource
  { mfr_MidiSongResourceMagic :: Int32 -- should be 2
  , mfr_LastTrackFinalTick    :: Word32
  , mfr_MidiTracks            :: [MidiTrack]
  , mfr_FuserRevision         :: Maybe Int32
  , mfr_FinalTick             :: Word32
  , mfr_Measures              :: Word32
  , mfr_Unknown               :: [Word32] -- fixed array of 6
  , mfr_FinalTickMinusOne     :: Word32
  , mfr_UnknownFloats         :: [Float] -- fixed array of 4
  , mfr_Tempos                :: [Tempo]
  , mfr_TimeSigs              :: [TimeSig]
  , mfr_Beats                 :: [Beat]
  , mfr_UnknownZero           :: Int32
  , mfr_FuserRevision2        :: Maybe Int32
  , mfr_FuserData             :: Maybe [FuserData]
  , mfr_MidiTrackNames        :: [B.ByteString]
  } deriving (Show)

instance Bin MidiFileResource where
  bin = do
    mfr_MidiSongResourceMagic <- mfr_MidiSongResourceMagic =. int32le
    mfr_LastTrackFinalTick    <- mfr_LastTrackFinalTick    =. word32le
    mfr_MidiTracks            <- mfr_MidiTracks            =. lenArray bin
    isFuser <- Codec
      { codecIn = do
        mrev <- lookAhead $ getByteString 4
        return $ mrev == "#REV"
      , codecOut = return . isJust . mfr_FuserRevision
      }
    mfr_FuserRevision         <- mfr_FuserRevision         =. optionalData isFuser int32le
    mfr_FinalTick             <- mfr_FinalTick             =. word32le
    mfr_Measures              <- mfr_Measures              =. word32le
    mfr_Unknown               <- mfr_Unknown               =. fixedArray 6 word32le
    mfr_FinalTickMinusOne     <- mfr_FinalTickMinusOne     =. word32le
    mfr_UnknownFloats         <- mfr_UnknownFloats         =. fixedArray 4 floatle
    mfr_Tempos                <- mfr_Tempos                =. lenArray bin
    mfr_TimeSigs              <- mfr_TimeSigs              =. lenArray bin
    mfr_Beats                 <- mfr_Beats                 =. lenArray bin
    mfr_UnknownZero           <- mfr_UnknownZero           =. int32le
    mfr_FuserRevision2        <- mfr_FuserRevision2        =. optionalData isFuser int32le
    mfr_FuserData             <- mfr_FuserData             =. optionalData isFuser (lenArray bin)
    mfr_MidiTrackNames        <- mfr_MidiTrackNames        =. lenArray lenString
    return MidiFileResource{..}

data MidiTrack = MidiTrack
  { mt_Unk     :: Word8
  , mt_Unk2    :: Int32
  , mt_Events  :: [Ticked Word32]
  , mt_Strings :: [B.ByteString]
  } deriving (Show)

instance Bin MidiTrack where
  bin = do
    mt_Unk     <- mt_Unk     =. word8
    mt_Unk2    <- mt_Unk2    =. int32le
    mt_Events  <- mt_Events  =. lenArray (ticked word32be) -- note, reading as BE
    mt_Strings <- mt_Strings =. lenArray lenString
    return MidiTrack{..}

convertMidiEvent :: [B.ByteString] -> Word32 -> Maybe (E.T B.ByteString)
convertMidiEvent strings w = let
  kind = (w .&. 0xFF000000) `shiftR` 24
  in case kind of
    1 -> let
      tc = (w .&. 0xFF0000) `shiftR` 16
      channel = C.toChannel $ fromIntegral $ tc .&. 0xF
      type_ = tc `shiftR` 4
      note = fromIntegral $ (w .&. 0xFF00) `shiftR` 8
      velocity = fromIntegral $ w .&. 0xFF
      in (E.MIDIEvent . C.Cons channel . C.Voice) <$> case type_ of
        8  -> Just $ V.NoteOff (V.toPitch note) $ V.toVelocity velocity
        9  -> Just $ V.NoteOn (V.toPitch note) $ V.toVelocity velocity
        11 -> Just $ V.Control (V.toController note) velocity
        12 -> Just $ V.ProgramChange $ V.toProgram note
        13 -> Just $ V.MonoAftertouch note
        14 -> Just $ V.PitchBend $ note .|. (velocity `shiftL` 8)
        _  -> Nothing
    2 -> Just $ E.MetaEvent $ Meta.SetTempo $ fromIntegral $ let
      high = w .&. 0xFF0000
      mid = (w .&. 0xFF00) `shiftR` 8
      low = (w .&. 0xFF) `shiftL` 8
      in high .|. mid .|. low
    4 -> do
      let num = fromIntegral $ (w .&. 0xFF0000) `shiftR` 16
          denom = (w .&. 0xFF00) `shiftR` 8
      denom_pow2 <- lookup denom $ takeWhile (\(x, _) -> x <= denom) [(2 ^ i, i) | i <- [0..]]
      Just $ E.MetaEvent $ Meta.TimeSig num denom_pow2 24 8
    8 -> do
      let ttype = (w .&. 0xFF0000) `shiftR` 16
          strIndex = ((w .&. 0xFF) `shiftL` 8) .|. ((w .&. 0xFF00) `shiftR` 8)
      txt <- listToMaybe $ drop (fromIntegral strIndex) strings
      E.MetaEvent <$> case ttype of
        1 -> Just $ Meta.TextEvent txt
        2 -> Just $ Meta.Copyright txt
        3 -> Just $ Meta.TrackName txt
        5 -> Just $ Meta.Lyric txt
        _ -> Nothing
    _ -> Nothing

data Tempo = Tempo
  { tempo_StartMillis :: Float
  , tempo_StartTick   :: Word32
  , tempo_Tempo       :: Int32
  } deriving (Show)

instance Bin Tempo where
  bin = do
    tempo_StartMillis <- tempo_StartMillis =. floatle
    tempo_StartTick   <- tempo_StartTick   =. word32le
    tempo_Tempo       <- tempo_Tempo       =. int32le
    return Tempo{..}

data TimeSig = TimeSig
  { ts_Measure     :: Int32
  , ts_Tick        :: Word32
  , ts_Numerator   :: Int16
  , ts_Denominator :: Int16
  } deriving (Show)

instance Bin TimeSig where
  bin = do
    ts_Measure     <- ts_Measure     =. int32le
    ts_Tick        <- ts_Tick        =. word32le
    ts_Numerator   <- ts_Numerator   =. int16le
    ts_Denominator <- ts_Denominator =. int16le
    return TimeSig{..}

data Beat = Beat
  { beat_Tick     :: Word32
  , beat_Downbeat :: Bool
  } deriving (Show)

instance Bin Beat where
  bin = do
    beat_Tick     <- beat_Tick     =. word32le
    beat_Downbeat <- beat_Downbeat =. boolByte
    skipBytes 3
    return Beat{..}

data FuserData = FuserData
  { fuser_Data :: B.ByteString
  } deriving (Show)

instance Bin FuserData where
  bin = undefined -- TODO

--------------------------------------------------------------------------------

extractMidi :: (SendMessage m) => RBMid -> StackTraceT m (F.T B.ByteString)
extractMidi rbmid = do
  let mfr = rbmid_MidiFileResource rbmid
  fmap (F.Cons F.Parallel $ F.Ticks 480) $ forM (mfr_MidiTracks mfr) $ \mt -> do
    let raw = RTB.fromAbsoluteEventList $ ATB.fromPairList $ map (\(Ticked delta x) -> (fromIntegral delta, x)) $ mt_Events mt
    fmap RTB.catMaybes $ forM raw $ \w -> do
      let res = convertMidiEvent (mt_Strings mt) w
      when (isNothing res) $ warn $ "Unrecognized MIDI event: " <> showHex w ""
      return res
