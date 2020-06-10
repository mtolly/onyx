{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE RecordWildCards #-}
module Rocksmith.Sng2014 where

import           Control.Monad
import           Data.Binary.Codec.Class
import qualified Data.ByteString         as B
import           Debug.Trace
import           Rocksmith.Crypt

lenArray :: (?endian :: ByteOrder) => BinaryCodec a -> BinaryCodec [a]
lenArray c = Codec
  { codecIn = do
    len <- codecIn codecLen
    replicateM (fromIntegral len) $ codecIn c
  , codecOut = fmapArg $ \xs -> do
    void $ codecOut codecLen $ fromIntegral $ length xs
    forM_ xs $ codecOut c
  } where codecLen = binEndian :: BinaryCodec Word32

nullTerm :: Int -> BinaryCodec B.ByteString
nullTerm n = Codec
  { codecIn = B.takeWhile (/= 0) <$> getByteString n
  , codecOut = fmapArg $ \b -> putByteString $ case compare n $ B.length b of
    EQ -> b
    LT -> B.take n b
    GT -> b <> B.replicate (B.length b - n) 0
  }

data BPM = BPM -- actually <ebeat>
  { bpm_Time            :: Float
  , bpm_Measure         :: Int16
  , bpm_Beat            :: Int16
  , bpm_PhraseIteration :: Int32
  , bpm_Mask            :: Int32
  } deriving (Eq, Show)

instance BinEndian BPM where
  binEndian = do
    bpm_Time            <- bpm_Time             =. binEndian
    bpm_Measure         <- bpm_Measure          =. binEndian
    bpm_Beat            <- bpm_Beat             =. binEndian
    bpm_PhraseIteration <- bpm_PhraseIteration  =. binEndian
    bpm_Mask            <- bpm_Mask             =. binEndian
    return BPM{..}

data Phrase = Phrase
  { phrase_Solo                 :: Word8
  , phrase_Disparity            :: Word8
  , phrase_Ignore               :: Word8
  , phrase_Padding              :: Word8
  , phrase_MaxDifficulty        :: Int32
  , phrase_PhraseIterationLinks :: Int32
  , phrase_Name                 :: B.ByteString
  } deriving (Eq, Show)

instance BinEndian Phrase where
  binEndian = do
    phrase_Solo                 <- phrase_Solo                 =. bin
    phrase_Disparity            <- phrase_Disparity            =. bin
    phrase_Ignore               <- phrase_Ignore               =. bin
    phrase_Padding              <- phrase_Padding              =. bin
    phrase_MaxDifficulty        <- phrase_MaxDifficulty        =. binEndian
    phrase_PhraseIterationLinks <- phrase_PhraseIterationLinks =. binEndian
    phrase_Name                 <- phrase_Name                 =. nullTerm 32
    return Phrase{..}

data Chord = Chord -- actually <chordTemplate>
  { chord_Mask    :: Word32
  , chord_Frets   :: [Int8]
  , chord_Fingers :: [Int8]
  , chord_Notes   :: [Int32]
  , chord_Name    :: B.ByteString
  } deriving (Eq, Show)

instance BinEndian Chord where
  binEndian = do
    chord_Mask    <- chord_Mask    =. binEndian
    chord_Frets   <- chord_Frets   =. fixedArray 6 bin
    chord_Fingers <- chord_Fingers =. fixedArray 6 bin
    chord_Notes   <- chord_Notes   =. fixedArray 6 binEndian
    chord_Name    <- chord_Name    =. nullTerm 32
    return Chord{..}

data BendData32 = BendData32
  { bd32_Time   :: Float
  , bd32_Step   :: Float
  , bd32_Unk3_0 :: Int16
  , bd32_Unk4_0 :: Word8
  , bd32_Unk5   :: Word8
  } deriving (Eq, Show)

instance BinEndian BendData32 where
  binEndian = do
    bd32_Time   <- bd32_Time   =. binEndian
    bd32_Step   <- bd32_Step   =. binEndian
    bd32_Unk3_0 <- bd32_Unk3_0 =. binEndian
    bd32_Unk4_0 <- bd32_Unk4_0 =. bin
    bd32_Unk5   <- bd32_Unk5   =. bin
    return BendData32{..}

data BendData = BendData
  { bd_BendData32 :: [BendData32]
  , bd_UsedCount  :: Int32
  } deriving (Eq, Show)

instance BinEndian BendData where
  binEndian = do
    bd_BendData32 <- bd_BendData32 =. fixedArray 32 binEndian
    bd_UsedCount  <- bd_UsedCount  =. binEndian
    return BendData{..}

data ChordNotes = ChordNotes
  { cn_NoteMask       :: [Word32]
  , cn_BendData       :: [BendData]
  , cn_SlideTo        :: [Int8]
  , cn_SlideUnpitchTo :: [Int8]
  , cn_Vibrato        :: [Int16]
  } deriving (Eq, Show)

instance BinEndian ChordNotes where
  binEndian = do
    cn_NoteMask       <- cn_NoteMask       =. fixedArray 6 binEndian
    cn_BendData       <- cn_BendData       =. fixedArray 6 binEndian
    cn_SlideTo        <- cn_SlideTo        =. fixedArray 6 bin
    cn_SlideUnpitchTo <- cn_SlideUnpitchTo =. fixedArray 6 bin
    cn_Vibrato        <- cn_Vibrato        =. fixedArray 6 binEndian
    return ChordNotes{..}

data Vocal = Vocal
  { vocal_Time   :: Float
  , vocal_Note   :: Int32
  , vocal_Length :: Float
  , vocal_Lyric  :: B.ByteString
  } deriving (Eq, Show)

instance BinEndian Vocal where
  binEndian = do
    vocal_Time   <- vocal_Time   =. binEndian
    vocal_Note   <- vocal_Note   =. binEndian
    vocal_Length <- vocal_Length =. binEndian
    vocal_Lyric  <- vocal_Lyric  =. nullTerm 48
    return Vocal{..}

writePosn :: String -> CodecFor Get PutM a ()
writePosn s = Codec
  { codecIn = do
    n <- bytesRead
    trace ("[" ++ s ++ "] " ++ show n) $ return ()
  , codecOut = \_ -> return ()
  }

data SymbolHeader = SymbolHeader
  { sh_Unk1 :: Int32
  , sh_Unk2 :: Int32
  , sh_Unk3 :: Int32
  , sh_Unk4 :: Int32
  , sh_Unk5 :: Int32
  , sh_Unk6 :: Int32
  , sh_Unk7 :: Int32
  , sh_Unk8 :: Int32
  } deriving (Eq, Show)

instance BinEndian SymbolHeader where
  binEndian = do
    sh_Unk1 <- sh_Unk1 =. binEndian
    sh_Unk2 <- sh_Unk2 =. binEndian
    sh_Unk3 <- sh_Unk3 =. binEndian
    sh_Unk4 <- sh_Unk4 =. binEndian
    sh_Unk5 <- sh_Unk5 =. binEndian
    sh_Unk6 <- sh_Unk6 =. binEndian
    sh_Unk7 <- sh_Unk7 =. binEndian
    sh_Unk8 <- sh_Unk8 =. binEndian
    return SymbolHeader{..}

data SymbolTexture = SymbolTexture
  { st_Font           :: B.ByteString
  , st_FontpathLength :: Int32
  , st_Unk1_0         :: Int32
  , st_Width          :: Int32
  , st_Height         :: Int32
  } deriving (Eq, Show)

instance BinEndian SymbolTexture where
  binEndian = do
    st_Font           <- st_Font           =. nullTerm 128
    st_FontpathLength <- st_FontpathLength =. binEndian
    st_Unk1_0         <- st_Unk1_0         =. binEndian
    st_Width          <- st_Width          =. binEndian
    st_Height         <- st_Height         =. binEndian
    return SymbolTexture{..}

data Rect = Rect
  { rect_yMin :: Float
  , rect_xMin :: Float
  , rect_yMax :: Float
  , rect_xMax :: Float
  } deriving (Eq, Show)

instance BinEndian Rect where
  binEndian = do
    rect_yMin <- rect_yMin =. binEndian
    rect_xMin <- rect_xMin =. binEndian
    rect_yMax <- rect_yMax =. binEndian
    rect_xMax <- rect_xMax =. binEndian
    return Rect{..}

data SymbolDefinition = SymbolDefinition
  { sd_Text       :: B.ByteString
  , sd_Rect_Outer :: Rect
  , sd_Rect_Inner :: Rect
  } deriving (Eq, Show)

instance BinEndian SymbolDefinition where
  binEndian = do
    sd_Text       <- sd_Text       =. nullTerm 12
    sd_Rect_Outer <- sd_Rect_Outer =. binEndian
    sd_Rect_Inner <- sd_Rect_Inner =. binEndian
    return SymbolDefinition{..}

data PhraseIteration = PhraseIteration
  { pi_PhraseId       :: Int32
  , pi_StartTime      :: Float
  , pi_NextPhraseTime :: Float
  , pi_Difficulty     :: [Int32]
  } deriving (Eq, Show)

instance BinEndian PhraseIteration where
  binEndian = do
    pi_PhraseId       <- pi_PhraseId       =. binEndian
    pi_StartTime      <- pi_StartTime      =. binEndian
    pi_NextPhraseTime <- pi_NextPhraseTime =. binEndian
    pi_Difficulty     <- pi_Difficulty     =. fixedArray 3 binEndian
    return PhraseIteration{..}

data PhraseExtraInfo = PhraseExtraInfo
  { pei_PhraseId   :: Int32
  , pei_Difficulty :: Int32
  , pei_Empty      :: Int32
  , pei_LevelJump  :: Word8
  , pei_Redundant  :: Int16
  , pei_Padding    :: Word8
  } deriving (Eq, Show)

instance BinEndian PhraseExtraInfo where
  binEndian = do
    pei_PhraseId   <- pei_PhraseId   =. binEndian
    pei_Difficulty <- pei_Difficulty =. binEndian
    pei_Empty      <- pei_Empty      =. binEndian
    pei_LevelJump  <- pei_LevelJump  =. bin
    pei_Redundant  <- pei_Redundant  =. binEndian
    pei_Padding    <- pei_Padding    =. bin
    return PhraseExtraInfo{..}

data NLinkedDifficulty = NLinkedDifficulty
  { nld_LevelBreak :: Int32
  , nld_Phrase     :: [Int32]
  } deriving (Eq, Show)

instance BinEndian NLinkedDifficulty where
  binEndian = do
    nld_LevelBreak <- nld_LevelBreak =. binEndian
    nld_Phrase     <- nld_Phrase     =. lenArray binEndian
    return NLinkedDifficulty{..}

data TimeName = TimeName
  { tn_Time :: Float
  , tn_Name :: B.ByteString
  } deriving (Eq, Show)

instance BinEndian TimeName where
  binEndian = do
    tn_Time <- tn_Time =. binEndian
    tn_Name <- tn_Name =. nullTerm 256
    return TimeName{..}

data TimeID = TimeID
  { tid_Time :: Float
  , tid_ID   :: Int32
  } deriving (Eq, Show)

instance BinEndian TimeID where
  binEndian = do
    tid_Time <- tid_Time =. binEndian
    tid_ID   <- tid_ID   =. binEndian
    return TimeID{..}

data Section = Section
  { sect_Name                   :: B.ByteString
  , sect_Number                 :: Int32
  , sect_StartTime              :: Float
  , sect_EndTime                :: Float
  , sect_StartPhraseIterationId :: Int32
  , sect_EndPhraseIterationId   :: Int32
  , sect_StringMask             :: B.ByteString
  } deriving (Eq, Show)

instance BinEndian Section where
  binEndian = do
    sect_Name                   <- sect_Name                   =. nullTerm 32
    sect_Number                 <- sect_Number                 =. binEndian
    sect_StartTime              <- sect_StartTime              =. binEndian
    sect_EndTime                <- sect_EndTime                =. binEndian
    sect_StartPhraseIterationId <- sect_StartPhraseIterationId =. binEndian
    sect_EndPhraseIterationId   <- sect_EndPhraseIterationId   =. binEndian
    sect_StringMask             <- sect_StringMask             =. byteString 36
    return Section{..}

data Anchor = Anchor
  { anchor_StartBeatTime      :: Float
  , anchor_EndBeatTime        :: Float
  , anchor_Unk3_FirstNoteTime :: Float
  , anchor_Unk4_LastNoteTime  :: Float
  , anchor_FretId             :: Word8
  , anchor_Padding            :: B.ByteString
  , anchor_Width              :: Int32
  , anchor_PhraseIterationId  :: Int32
  } deriving (Eq, Show)

instance BinEndian Anchor where
  binEndian = do
    anchor_StartBeatTime      <- anchor_StartBeatTime      =. binEndian
    anchor_EndBeatTime        <- anchor_EndBeatTime        =. binEndian
    anchor_Unk3_FirstNoteTime <- anchor_Unk3_FirstNoteTime =. binEndian
    anchor_Unk4_LastNoteTime  <- anchor_Unk4_LastNoteTime  =. binEndian
    anchor_FretId             <- anchor_FretId             =. bin
    anchor_Padding            <- anchor_Padding            =. byteString 3
    anchor_Width              <- anchor_Width              =. binEndian
    anchor_PhraseIterationId  <- anchor_PhraseIterationId  =. binEndian
    return Anchor{..}

data AnchorExtension = AnchorExtension
  { ae_BeatTime :: Float
  , ae_FretId   :: Word8
  , ae_Unk2_0   :: Int32
  , ae_Unk3_0   :: Int16
  , ae_Unk4_0   :: Word8
  } deriving (Eq, Show)

instance BinEndian AnchorExtension where
  binEndian = do
    ae_BeatTime <- ae_BeatTime =. binEndian
    ae_FretId   <- ae_FretId   =. bin
    ae_Unk2_0   <- ae_Unk2_0   =. binEndian
    ae_Unk3_0   <- ae_Unk3_0   =. binEndian
    ae_Unk4_0   <- ae_Unk4_0   =. bin
    return AnchorExtension{..}

data Fingerprint = Fingerprint
  { fp_ChordId            :: Int32
  , fp_StartTime          :: Float
  , fp_EndTime            :: Float
  , fp_Unk3_FirstNoteTime :: Float
  , fp_Unk4_LastNoteTime  :: Float
  } deriving (Eq, Show)

instance BinEndian Fingerprint where
  binEndian = do
    fp_ChordId            <- fp_ChordId            =. binEndian
    fp_StartTime          <- fp_StartTime          =. binEndian
    fp_EndTime            <- fp_EndTime            =. binEndian
    fp_Unk3_FirstNoteTime <- fp_Unk3_FirstNoteTime =. binEndian
    fp_Unk4_LastNoteTime  <- fp_Unk4_LastNoteTime  =. binEndian
    return Fingerprint{..}

data Notes = Notes
  { notes_NoteMask          :: Word32
  , notes_NoteFlags         :: Word32
  , notes_Hash              :: Word32
  , notes_Time              :: Float
  , notes_StringIndex       :: Int8
  , notes_FretId            :: Int8
  , notes_AnchorFretId      :: Word8
  , notes_AnchorWidth       :: Word8
  , notes_ChordId           :: Int32
  , notes_ChordNotesId      :: Int32
  , notes_PhraseId          :: Int32
  , notes_PhraseIterationId :: Int32
  , notes_FingerPrintId     :: [Int16]
  , notes_NextIterNote      :: Int16
  , notes_PrevIterNote      :: Int16
  , notes_ParentPrevNote    :: Int16
  , notes_SlideTo           :: Int8
  , notes_SlideUnpitchTo    :: Int8
  , notes_LeftHand          :: Int8
  , notes_Tap               :: Int8
  , notes_PickDirection     :: Word8
  , notes_Slap              :: Int8
  , notes_Pluck             :: Int8
  , notes_Vibrato           :: Int16
  , notes_Sustain           :: Float
  , notes_MaxBend           :: Float
  , notes_BendData          :: [BendData32]
  } deriving (Eq, Show)

instance BinEndian Notes where
  binEndian = do
    notes_NoteMask          <- notes_NoteMask          =. binEndian
    notes_NoteFlags         <- notes_NoteFlags         =. binEndian
    notes_Hash              <- notes_Hash              =. binEndian
    notes_Time              <- notes_Time              =. binEndian
    notes_StringIndex       <- notes_StringIndex       =. bin
    notes_FretId            <- notes_FretId            =. bin
    notes_AnchorFretId      <- notes_AnchorFretId      =. bin
    notes_AnchorWidth       <- notes_AnchorWidth       =. bin
    notes_ChordId           <- notes_ChordId           =. binEndian
    notes_ChordNotesId      <- notes_ChordNotesId      =. binEndian
    notes_PhraseId          <- notes_PhraseId          =. binEndian
    notes_PhraseIterationId <- notes_PhraseIterationId =. binEndian
    notes_FingerPrintId     <- notes_FingerPrintId     =. fixedArray 2 binEndian
    notes_NextIterNote      <- notes_NextIterNote      =. binEndian
    notes_PrevIterNote      <- notes_PrevIterNote      =. binEndian
    notes_ParentPrevNote    <- notes_ParentPrevNote    =. binEndian
    notes_SlideTo           <- notes_SlideTo           =. bin
    notes_SlideUnpitchTo    <- notes_SlideUnpitchTo    =. bin
    notes_LeftHand          <- notes_LeftHand          =. bin
    notes_Tap               <- notes_Tap               =. bin
    notes_PickDirection     <- notes_PickDirection     =. bin
    notes_Slap              <- notes_Slap              =. bin
    notes_Pluck             <- notes_Pluck             =. bin
    notes_Vibrato           <- notes_Vibrato           =. binEndian
    notes_Sustain           <- notes_Sustain           =. binEndian
    notes_MaxBend           <- notes_MaxBend           =. binEndian
    notes_BendData          <- notes_BendData          =. lenArray binEndian
    return Notes{..}

data Arrangement = Arrangement -- actually <level>
  { arr_Difficulty               :: Int32
  , arr_Anchors                  :: [Anchor]
  , arr_AnchorExtensions         :: [AnchorExtension]
  , arr_Fingerprints1            :: [Fingerprint]
  , arr_Fingerprints2            :: [Fingerprint]
  , arr_Notes                    :: [Notes]
  , arr_AverageNotesPerIteration :: [Float]
  , arr_NotesInIteration1        :: [Int32]
  , arr_NotesInIteration2        :: [Int32]
  } deriving (Eq, Show)

instance BinEndian Arrangement where
  binEndian = do
    arr_Difficulty               <- arr_Difficulty               =. binEndian
    arr_Anchors                  <- arr_Anchors                  =. lenArray binEndian
    arr_AnchorExtensions         <- arr_AnchorExtensions         =. lenArray binEndian
    arr_Fingerprints1            <- arr_Fingerprints1            =. lenArray binEndian
    arr_Fingerprints2            <- arr_Fingerprints2            =. lenArray binEndian
    arr_Notes                    <- arr_Notes                    =. lenArray binEndian
    arr_AverageNotesPerIteration <- arr_AverageNotesPerIteration =. lenArray binEndian
    arr_NotesInIteration1        <- arr_NotesInIteration1        =. lenArray binEndian
    arr_NotesInIteration2        <- arr_NotesInIteration2        =. lenArray binEndian
    return Arrangement{..}

data Metadata = Metadata
  { meta_MaxScore               :: Double
  , meta_MaxNotesAndChords      :: Double
  , meta_MaxNotesAndChords_Real :: Double
  , meta_PointsPerNote          :: Double
  , meta_FirstBeatLength        :: Float
  , meta_StartTime              :: Float
  , meta_CapoFretId             :: Int8
  , meta_LastConversionDateTime :: B.ByteString -- = new Byte[32];
  , meta_Part                   :: Int16
  , meta_SongLength             :: Float
  , meta_Tuning                 :: [Int16]
  , meta_Unk11_FirstNoteTime    :: Float
  , meta_Unk12_FirstNoteTime    :: Float
  , meta_MaxDifficulty          :: Int32
  } deriving (Eq, Show)

instance BinEndian Metadata where
  binEndian = do
    meta_MaxScore               <- meta_MaxScore                =. binEndian
    meta_MaxNotesAndChords      <- meta_MaxNotesAndChords       =. binEndian
    meta_MaxNotesAndChords_Real <- meta_MaxNotesAndChords_Real  =. binEndian
    meta_PointsPerNote          <- meta_PointsPerNote           =. binEndian
    meta_FirstBeatLength        <- meta_FirstBeatLength         =. binEndian
    meta_StartTime              <- meta_StartTime               =. binEndian
    meta_CapoFretId             <- meta_CapoFretId              =. bin
    meta_LastConversionDateTime <- meta_LastConversionDateTime  =. nullTerm 32
    meta_Part                   <- meta_Part                    =. binEndian
    meta_SongLength             <- meta_SongLength              =. binEndian
    meta_Tuning                 <- meta_Tuning                  =. lenArray binEndian
    meta_Unk11_FirstNoteTime    <- meta_Unk11_FirstNoteTime     =. binEndian
    meta_Unk12_FirstNoteTime    <- meta_Unk12_FirstNoteTime     =. binEndian
    meta_MaxDifficulty          <- meta_MaxDifficulty           =. binEndian
    return Metadata{..}

data SNG2014 = SNG2014
  { sng_BPMs              :: [BPM]
  , sng_Phrases           :: [Phrase]
  , sng_Chords            :: [Chord]
  , sng_ChordNotes        :: [ChordNotes]
  , sng_Vocals            :: [Vocal]
  , sng_SymbolHeaders     :: [SymbolHeader] -- only in vocals files
  , sng_SymbolTextures    :: [SymbolTexture] -- only in vocals files
  , sng_SymbolDefinitions :: [SymbolDefinition] -- only in vocals files
  , sng_PhraseIterations  :: [PhraseIteration]
  , sng_PhraseExtraInfo   :: [PhraseExtraInfo]
  , sng_NLinkedDifficulty :: [NLinkedDifficulty]
  , sng_Actions           :: [TimeName]
  , sng_Events            :: [TimeName]
  , sng_Tones             :: [TimeID]
  , sng_DNAs              :: [TimeID]
  , sng_Sections          :: [Section]
  , sng_Arrangements      :: [Arrangement]
  , sng_Metadata          :: Metadata
  } deriving (Eq, Show)

instance BinEndian SNG2014 where
  binEndian = do
    sng_BPMs              <- sng_BPMs              =. lenArray binEndian
    sng_Phrases           <- sng_Phrases           =. lenArray binEndian
    sng_Chords            <- sng_Chords            =. lenArray binEndian
    sng_ChordNotes        <- sng_ChordNotes        =. lenArray binEndian
    sng_Vocals            <- sng_Vocals            =. lenArray binEndian
    let onlyVox p = if null sng_Vocals then return [] else p
    sng_SymbolHeaders     <- onlyVox $ sng_SymbolHeaders     =. lenArray binEndian
    sng_SymbolTextures    <- onlyVox $ sng_SymbolTextures    =. lenArray binEndian
    sng_SymbolDefinitions <- onlyVox $ sng_SymbolDefinitions =. lenArray binEndian
    sng_PhraseIterations  <- sng_PhraseIterations  =. lenArray binEndian
    sng_PhraseExtraInfo   <- sng_PhraseExtraInfo   =. lenArray binEndian
    sng_NLinkedDifficulty <- sng_NLinkedDifficulty =. lenArray binEndian
    sng_Actions           <- sng_Actions           =. lenArray binEndian
    sng_Events            <- sng_Events            =. lenArray binEndian
    sng_Tones             <- sng_Tones             =. lenArray binEndian
    sng_DNAs              <- sng_DNAs              =. lenArray binEndian
    sng_Sections          <- sng_Sections          =. lenArray binEndian
    sng_Arrangements      <- sng_Arrangements      =. lenArray binEndian
    sng_Metadata          <- sng_Metadata          =. binEndian
    return SNG2014{..}

loadSNG :: GamePlatform -> FilePath -> IO SNG2014
loadSNG plat fp = do
  bs <- unpackSNG plat fp
  let ?endian = case plat of
        PC      -> LittleEndian
        Mac     -> LittleEndian
        Xbox360 -> BigEndian
        PS3     -> BigEndian
  return $ runGet (codecIn binEndian) bs
