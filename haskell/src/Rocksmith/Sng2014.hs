{-# LANGUAGE RecordWildCards #-}
module Rocksmith.Sng2014 where

import           Control.Monad
import           Control.Monad.Codec
import           Data.Binary.Codec
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString     as B
import           Data.Int
import           Data.Word
import           Debug.Trace
import           Rocksmith.Crypt

-- TODO PC/Mac .sng files are little endian, need to make these work on either

class Bin a where
  bin :: BinaryCodec a

floatbe :: BinaryCodec Float
floatbe = Codec getFloatbe (fmapArg putFloatbe)

doublebe :: BinaryCodec Double
doublebe = Codec getDoublebe (fmapArg putDoublebe)

lenArray :: BinaryCodec a -> BinaryCodec [a]
lenArray c = Codec
  { codecIn = do
    len <- getInt32be
    replicateM (fromIntegral len) $ codecIn c
  , codecOut = fmapArg $ \xs -> do
    putInt32be $ fromIntegral $ length xs
    forM_ xs $ codecOut c
  }

nullTerm :: Int -> BinaryCodec B.ByteString
nullTerm n = Codec
  { codecIn = B.takeWhile (/= 0) <$> getByteString n
  , codecOut = fmapArg $ \b -> putByteString $ case compare n $ B.length b of
    EQ -> b
    LT -> B.take n b
    GT -> b <> B.replicate (B.length b - n) 0
  }

fixedArray :: Int -> BinaryCodec a -> BinaryCodec [a]
fixedArray n c = Codec
  { codecIn = replicateM n $ codecIn c
  , codecOut = fmapArg $ \xs -> if length xs == n
    then forM_ xs $ codecOut c
    else error $ "Expected an array of size " <> show n
  }

data BPM = BPM
  { bpm_Time            :: Float
  , bpm_Measure         :: Int16
  , bpm_Beat            :: Int16
  , bpm_PhraseIteration :: Int32
  , bpm_Mask            :: Int32
  } deriving (Eq, Show)

instance Bin BPM where
  bin = do
    bpm_Time            <- bpm_Time             =. floatbe
    bpm_Measure         <- bpm_Measure          =. int16be
    bpm_Beat            <- bpm_Beat             =. int16be
    bpm_PhraseIteration <- bpm_PhraseIteration  =. int32be
    bpm_Mask            <- bpm_Mask             =. int32be
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

instance Bin Phrase where
  bin = do
    phrase_Solo                 <- phrase_Solo                 =. word8
    phrase_Disparity            <- phrase_Disparity            =. word8
    phrase_Ignore               <- phrase_Ignore               =. word8
    phrase_Padding              <- phrase_Padding              =. word8
    phrase_MaxDifficulty        <- phrase_MaxDifficulty        =. int32be
    phrase_PhraseIterationLinks <- phrase_PhraseIterationLinks =. int32be
    phrase_Name                 <- phrase_Name                 =. nullTerm 32
    return Phrase{..}

data Chord = Chord
  { chord_Mask    :: Word32
  , chord_Frets   :: [Int8]
  , chord_Fingers :: [Int8]
  , chord_Notes   :: [Int32]
  , chord_Name    :: B.ByteString
  } deriving (Eq, Show)

instance Bin Chord where
  bin = do
    chord_Mask    <- chord_Mask    =. word32be
    chord_Frets   <- chord_Frets   =. fixedArray 6 int8
    chord_Fingers <- chord_Fingers =. fixedArray 6 int8
    chord_Notes   <- chord_Notes   =. fixedArray 6 int32be
    chord_Name    <- chord_Name    =. nullTerm 32
    return Chord{..}

data BendData32 = BendData32
  { bd32_Time   :: Float
  , bd32_Step   :: Float
  , bd32_Unk3_0 :: Int16
  , bd32_Unk4_0 :: Word8
  , bd32_Unk5   :: Word8
  } deriving (Eq, Show)

instance Bin BendData32 where
  bin = do
    bd32_Time   <- bd32_Time   =. floatbe
    bd32_Step   <- bd32_Step   =. floatbe
    bd32_Unk3_0 <- bd32_Unk3_0 =. int16be
    bd32_Unk4_0 <- bd32_Unk4_0 =. word8
    bd32_Unk5   <- bd32_Unk5   =. word8
    return BendData32{..}

data BendData = BendData
  { bd_BendData32 :: [BendData32]
  , bd_UsedCount  :: Int32
  } deriving (Eq, Show)

instance Bin BendData where
  bin = do
    bd_BendData32 <- bd_BendData32 =. fixedArray 32 bin
    bd_UsedCount  <- bd_UsedCount  =. int32be
    return BendData{..}

data ChordNotes = ChordNotes
  { cn_NoteMask       :: [Word32]
  , cn_BendData       :: [BendData]
  , cn_SlideTo        :: [Int8]
  , cn_SlideUnpitchTo :: [Int8]
  , cn_Vibrato        :: [Int16]
  } deriving (Eq, Show)

instance Bin ChordNotes where
  bin = do
    cn_NoteMask       <- cn_NoteMask       =. fixedArray 6 word32be
    cn_BendData       <- cn_BendData       =. fixedArray 6 bin
    cn_SlideTo        <- cn_SlideTo        =. fixedArray 6 int8
    cn_SlideUnpitchTo <- cn_SlideUnpitchTo =. fixedArray 6 int8
    cn_Vibrato        <- cn_Vibrato        =. fixedArray 6 int16be
    return ChordNotes{..}

data Vocal = Vocal
  { vocal_Time   :: Float
  , vocal_Note   :: Int32
  , vocal_Length :: Float
  , vocal_Lyric  :: B.ByteString
  } deriving (Eq, Show)

instance Bin Vocal where
  bin = do
    vocal_Time   <- vocal_Time   =. floatbe
    vocal_Note   <- vocal_Note   =. int32be
    vocal_Length <- vocal_Length =. floatbe
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

instance Bin SymbolHeader where
  bin = do
    sh_Unk1 <- sh_Unk1 =. int32be
    sh_Unk2 <- sh_Unk2 =. int32be
    sh_Unk3 <- sh_Unk3 =. int32be
    sh_Unk4 <- sh_Unk4 =. int32be
    sh_Unk5 <- sh_Unk5 =. int32be
    sh_Unk6 <- sh_Unk6 =. int32be
    sh_Unk7 <- sh_Unk7 =. int32be
    sh_Unk8 <- sh_Unk8 =. int32be
    return SymbolHeader{..}

data SymbolTexture = SymbolTexture
  { st_Font           :: B.ByteString
  , st_FontpathLength :: Int32
  , st_Unk1_0         :: Int32
  , st_Width          :: Int32
  , st_Height         :: Int32
  } deriving (Eq, Show)

instance Bin SymbolTexture where
  bin = do
    st_Font           <- st_Font           =. nullTerm 128
    st_FontpathLength <- st_FontpathLength =. int32be
    st_Unk1_0         <- st_Unk1_0         =. int32be
    st_Width          <- st_Width          =. int32be
    st_Height         <- st_Height         =. int32be
    return SymbolTexture{..}

data Rect = Rect
  { rect_yMin :: Float
  , rect_xMin :: Float
  , rect_yMax :: Float
  , rect_xMax :: Float
  } deriving (Eq, Show)

instance Bin Rect where
  bin = do
    rect_yMin <- rect_yMin =. floatbe
    rect_xMin <- rect_xMin =. floatbe
    rect_yMax <- rect_yMax =. floatbe
    rect_xMax <- rect_xMax =. floatbe
    return Rect{..}

data SymbolDefinition = SymbolDefinition
  { sd_Text       :: B.ByteString
  , sd_Rect_Outer :: Rect
  , sd_Rect_Inner :: Rect
  } deriving (Eq, Show)

instance Bin SymbolDefinition where
  bin = do
    sd_Text       <- sd_Text       =. nullTerm 12
    sd_Rect_Outer <- sd_Rect_Outer =. bin
    sd_Rect_Inner <- sd_Rect_Inner =. bin
    return SymbolDefinition{..}

data PhraseIteration = PhraseIteration
  { pi_PhraseId       :: Int32
  , pi_StartTime      :: Float
  , pi_NextPhraseTime :: Float
  , pi_Difficulty     :: [Int32]
  } deriving (Eq, Show)

instance Bin PhraseIteration where
  bin = do
    pi_PhraseId       <- pi_PhraseId       =. int32be
    pi_StartTime      <- pi_StartTime      =. floatbe
    pi_NextPhraseTime <- pi_NextPhraseTime =. floatbe
    pi_Difficulty     <- pi_Difficulty     =. fixedArray 3 int32be
    return PhraseIteration{..}

data PhraseExtraInfo = PhraseExtraInfo
  { pei_PhraseId   :: Int32
  , pei_Difficulty :: Int32
  , pei_Empty      :: Int32
  , pei_LevelJump  :: Word8
  , pei_Redundant  :: Int16
  , pei_Padding    :: Word8
  } deriving (Eq, Show)

instance Bin PhraseExtraInfo where
  bin = do
    pei_PhraseId   <- pei_PhraseId   =. int32be
    pei_Difficulty <- pei_Difficulty =. int32be
    pei_Empty      <- pei_Empty      =. int32be
    pei_LevelJump  <- pei_LevelJump  =. word8
    pei_Redundant  <- pei_Redundant  =. int16be
    pei_Padding    <- pei_Padding    =. word8
    return PhraseExtraInfo{..}

data NLinkedDifficulty = NLinkedDifficulty
  { nld_LevelBreak :: Int32
  , nld_Phrase     :: [Int32]
  } deriving (Eq, Show)

instance Bin NLinkedDifficulty where
  bin = do
    nld_LevelBreak <- nld_LevelBreak =. int32be
    nld_Phrase     <- nld_Phrase     =. lenArray int32be
    return NLinkedDifficulty{..}

data TimeName = TimeName
  { tn_Time :: Float
  , tn_Name :: B.ByteString
  } deriving (Eq, Show)

instance Bin TimeName where
  bin = do
    tn_Time <- tn_Time =. floatbe
    tn_Name <- tn_Name =. nullTerm 256
    return TimeName{..}

data TimeID = TimeID
  { tid_Time :: Float
  , tid_ID   :: Int32
  } deriving (Eq, Show)

instance Bin TimeID where
  bin = do
    tid_Time <- tid_Time =. floatbe
    tid_ID   <- tid_ID   =. int32be
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

instance Bin Section where
  bin = do
    sect_Name                   <- sect_Name                   =. nullTerm 32
    sect_Number                 <- sect_Number                 =. int32be
    sect_StartTime              <- sect_StartTime              =. floatbe
    sect_EndTime                <- sect_EndTime                =. floatbe
    sect_StartPhraseIterationId <- sect_StartPhraseIterationId =. int32be
    sect_EndPhraseIterationId   <- sect_EndPhraseIterationId   =. int32be
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

instance Bin Anchor where
  bin = do
    anchor_StartBeatTime      <- anchor_StartBeatTime      =. floatbe
    anchor_EndBeatTime        <- anchor_EndBeatTime        =. floatbe
    anchor_Unk3_FirstNoteTime <- anchor_Unk3_FirstNoteTime =. floatbe
    anchor_Unk4_LastNoteTime  <- anchor_Unk4_LastNoteTime  =. floatbe
    anchor_FretId             <- anchor_FretId             =. word8
    anchor_Padding            <- anchor_Padding            =. byteString 3
    anchor_Width              <- anchor_Width              =. int32be
    anchor_PhraseIterationId  <- anchor_PhraseIterationId  =. int32be
    return Anchor{..}

data AnchorExtension = AnchorExtension
  { ae_BeatTime :: Float
  , ae_FretId   :: Word8
  , ae_Unk2_0   :: Int32
  , ae_Unk3_0   :: Int16
  , ae_Unk4_0   :: Word8
  } deriving (Eq, Show)

instance Bin AnchorExtension where
  bin = do
    ae_BeatTime <- ae_BeatTime =. floatbe
    ae_FretId   <- ae_FretId   =. word8
    ae_Unk2_0   <- ae_Unk2_0   =. int32be
    ae_Unk3_0   <- ae_Unk3_0   =. int16be
    ae_Unk4_0   <- ae_Unk4_0   =. word8
    return AnchorExtension{..}

data Fingerprint = Fingerprint
  { fp_ChordId            :: Int32
  , fp_StartTime          :: Float
  , fp_EndTime            :: Float
  , fp_Unk3_FirstNoteTime :: Float
  , fp_Unk4_LastNoteTime  :: Float
  } deriving (Eq, Show)

instance Bin Fingerprint where
  bin = do
    fp_ChordId            <- fp_ChordId            =. int32be
    fp_StartTime          <- fp_StartTime          =. floatbe
    fp_EndTime            <- fp_EndTime            =. floatbe
    fp_Unk3_FirstNoteTime <- fp_Unk3_FirstNoteTime =. floatbe
    fp_Unk4_LastNoteTime  <- fp_Unk4_LastNoteTime  =. floatbe
    return Fingerprint{..}

data Notes = Notes
  { notes_NoteMask          :: Word32
  , notes_NoteFlags         :: Word32
  , notes_Hash              :: Word32
  , notes_Time              :: Float
  , notes_StringIndex       :: Word8
  , notes_FretId            :: Word8
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

instance Bin Notes where
  bin = do
    notes_NoteMask          <- notes_NoteMask          =. word32be
    notes_NoteFlags         <- notes_NoteFlags         =. word32be
    notes_Hash              <- notes_Hash              =. word32be
    notes_Time              <- notes_Time              =. floatbe
    notes_StringIndex       <- notes_StringIndex       =. word8
    notes_FretId            <- notes_FretId            =. word8
    notes_AnchorFretId      <- notes_AnchorFretId      =. word8
    notes_AnchorWidth       <- notes_AnchorWidth       =. word8
    notes_ChordId           <- notes_ChordId           =. int32be
    notes_ChordNotesId      <- notes_ChordNotesId      =. int32be
    notes_PhraseId          <- notes_PhraseId          =. int32be
    notes_PhraseIterationId <- notes_PhraseIterationId =. int32be
    notes_FingerPrintId     <- notes_FingerPrintId     =. fixedArray 2 int16be
    notes_NextIterNote      <- notes_NextIterNote      =. int16be
    notes_PrevIterNote      <- notes_PrevIterNote      =. int16be
    notes_ParentPrevNote    <- notes_ParentPrevNote    =. int16be
    notes_SlideTo           <- notes_SlideTo           =. int8
    notes_SlideUnpitchTo    <- notes_SlideUnpitchTo    =. int8
    notes_LeftHand          <- notes_LeftHand          =. int8
    notes_Tap               <- notes_Tap               =. int8
    notes_PickDirection     <- notes_PickDirection     =. word8
    notes_Slap              <- notes_Slap              =. int8
    notes_Pluck             <- notes_Pluck             =. int8
    notes_Vibrato           <- notes_Vibrato           =. int16be
    notes_Sustain           <- notes_Sustain           =. floatbe
    notes_MaxBend           <- notes_MaxBend           =. floatbe
    notes_BendData          <- notes_BendData          =. lenArray bin
    return Notes{..}

data Arrangement = Arrangement
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

instance Bin Arrangement where
  bin = do
    arr_Difficulty               <- arr_Difficulty               =. int32be
    arr_Anchors                  <- arr_Anchors                  =. lenArray bin
    arr_AnchorExtensions         <- arr_AnchorExtensions         =. lenArray bin
    arr_Fingerprints1            <- arr_Fingerprints1            =. lenArray bin
    arr_Fingerprints2            <- arr_Fingerprints2            =. lenArray bin
    arr_Notes                    <- arr_Notes                    =. lenArray bin
    arr_AverageNotesPerIteration <- arr_AverageNotesPerIteration =. lenArray floatbe
    arr_NotesInIteration1        <- arr_NotesInIteration1        =. lenArray int32be
    arr_NotesInIteration2        <- arr_NotesInIteration2        =. lenArray int32be
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

instance Bin Metadata where
  bin = do
    meta_MaxScore               <- meta_MaxScore                =. doublebe
    meta_MaxNotesAndChords      <- meta_MaxNotesAndChords       =. doublebe
    meta_MaxNotesAndChords_Real <- meta_MaxNotesAndChords_Real  =. doublebe
    meta_PointsPerNote          <- meta_PointsPerNote           =. doublebe
    meta_FirstBeatLength        <- meta_FirstBeatLength         =. floatbe
    meta_StartTime              <- meta_StartTime               =. floatbe
    meta_CapoFretId             <- meta_CapoFretId              =. int8
    meta_LastConversionDateTime <- meta_LastConversionDateTime  =. nullTerm 32
    meta_Part                   <- meta_Part                    =. int16be
    meta_SongLength             <- meta_SongLength              =. floatbe
    meta_Tuning                 <- meta_Tuning                  =. lenArray int16be
    meta_Unk11_FirstNoteTime    <- meta_Unk11_FirstNoteTime     =. floatbe
    meta_Unk12_FirstNoteTime    <- meta_Unk12_FirstNoteTime     =. floatbe
    meta_MaxDifficulty          <- meta_MaxDifficulty           =. int32be
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

instance Bin SNG2014 where
  bin = do
    sng_BPMs              <- sng_BPMs              =. lenArray bin
    sng_Phrases           <- sng_Phrases           =. lenArray bin
    sng_Chords            <- sng_Chords            =. lenArray bin
    sng_ChordNotes        <- sng_ChordNotes        =. lenArray bin
    sng_Vocals            <- sng_Vocals            =. lenArray bin
    let onlyVox p = if null sng_Vocals then return [] else p
    sng_SymbolHeaders     <- onlyVox $ sng_SymbolHeaders     =. lenArray bin
    sng_SymbolTextures    <- onlyVox $ sng_SymbolTextures    =. lenArray bin
    sng_SymbolDefinitions <- onlyVox $ sng_SymbolDefinitions =. lenArray bin
    sng_PhraseIterations  <- sng_PhraseIterations  =. lenArray bin
    sng_PhraseExtraInfo   <- sng_PhraseExtraInfo   =. lenArray bin
    sng_NLinkedDifficulty <- sng_NLinkedDifficulty =. lenArray bin
    sng_Actions           <- sng_Actions           =. lenArray bin
    sng_Events            <- sng_Events            =. lenArray bin
    sng_Tones             <- sng_Tones             =. lenArray bin
    sng_DNAs              <- sng_DNAs              =. lenArray bin
    sng_Sections          <- sng_Sections          =. lenArray bin
    sng_Arrangements      <- sng_Arrangements      =. lenArray bin
    sng_Metadata          <- sng_Metadata          =. bin
    return SNG2014{..}

loadSNG :: GamePlatform -> FilePath -> IO SNG2014
loadSNG plat fp = do
  bs <- unpackSNG plat fp
  return $ runGet (codecIn bin) bs
