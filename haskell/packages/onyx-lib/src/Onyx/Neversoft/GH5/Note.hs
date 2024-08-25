{- |
Built from:
https://github.com/AerialX/rawksd
https://github.com/Nanook/Queen-Bee
expertarraytochart.bms by GHFear
-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE ViewPatterns        #-}
module Onyx.Neversoft.GH5.Note where

import           Control.Monad                    (forM, forM_, guard,
                                                   replicateM, void)
import           Control.Monad.Trans.Writer       (execWriter, tell)
import           Data.Bits
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (partition, sort)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Numeric                          (showHex)
import           Onyx.Codec.Binary
import           Onyx.Guitar                      (emit5')
import           Onyx.MIDI.Common                 (Difficulty (..), Edge (..),
                                                   StrumHOPOTap (..),
                                                   pattern ANil, pattern At,
                                                   splitEdgesSimple)
import qualified Onyx.MIDI.Track.Drums            as D
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import qualified Onyx.MIDI.Track.Vocal            as V
import           Onyx.Neversoft.CRC               (QBKey, getQBKeyBE,
                                                   putQBKeyBE, qbKeyCRC)
import           Onyx.Neversoft.Pak
import           Onyx.Neversoft.QB                (QBArray (QBArrayOfInteger),
                                                   QBSection (QBSectionArray),
                                                   parseQB)
import           Onyx.Sections                    (simpleSection)
import           Onyx.StackTrace                  (StackTraceT, inside)
import           Onyx.Util.Binary                 (runGetM)
import qualified Sound.MIDI.Util                  as U

data NoteEntry = NoteEntry
  { entryIdentifier  :: QBKey
  , entryCount       :: Word32
  , entryType        :: QBKey
  , entryElementSize :: Word32
  , entryContents    :: [B.ByteString]
  } deriving (Show)

getNote :: Get (QBKey, [NoteEntry])
getNote = do
  magic <- getWord32be
  case magic of
    0x40C001A3 -> return () -- GHWOR
    0x40A000D2 -> return () -- GH5
    _          -> fail $ "Unrecognized .note magic: 0x" <> showHex magic ""
  dlcKey <- getQBKeyBE -- qb key for e.g. "dlc784"
  count <- getWord32be
  "note" <- getQBKeyBE
  zeroes <- getByteString 12
  guard $ B.all (== 0) zeroes
  entries <- replicateM (fromIntegral count) $ do
    entryIdentifier <- getQBKeyBE
    entryCount <- getWord32be
    entryType <- getQBKeyBE
    entryElementSize <- getWord32be
    entryContents <- replicateM (fromIntegral entryCount)
      $ getByteString $ fromIntegral entryElementSize
    return NoteEntry{..}
  return (dlcKey, entries)

putNote :: QBKey -> [NoteEntry] -> Put
putNote dlcKey entries = do
  putWord32be 0x40C001A3
  putQBKeyBE  dlcKey
  putWord32be $ fromIntegral $ length entries
  putQBKeyBE  "note"
  putByteString $ B.replicate 12 0
  forM_ entries $ \entry -> do
    putQBKeyBE  entry.entryIdentifier
    putWord32be entry.entryCount
    putQBKeyBE  entry.entryType
    putWord32be entry.entryElementSize
    mapM_ putByteString entry.entryContents

data TimeSig = TimeSig
  { tsTimestamp   :: Word32
  , tsNumerator   :: Word8
  , tsDenominator :: Word8
  } deriving (Show)

instance Bin TimeSig where
  bin = do
    tsTimestamp   <- (.tsTimestamp  ) =. word32be
    tsNumerator   <- (.tsNumerator  ) =. word8
    tsDenominator <- (.tsDenominator) =. word8
    return TimeSig{..}

-- entryType = gh5_instrument_note
data Note = Note
  { noteTimeOffset :: Word32
  , noteDuration   :: Word16
  , noteBits       :: Word8
  -- 0 - green
  -- 1 - red
  -- 2 - yellow
  -- 3 - blue
  -- 4 - orange
  -- guitar/bass:
  --   5 - open
  --   6 - hopo
  -- drums:
  --   5 - kick
  --   6 - kick 2 (only X+)
  -- 7 - ???
  , noteAccent     :: Word8
  -- On drums, usually 0. Set bits 0-4 (matching noteBits) for accent.
  -- Official songs usually just set 31 for accent, and turn off exceptions.
  -- So e.g. 29 (0b11101) when red+yellow and yellow is accented.
  -- On guitar/bass, usually 31. Clear bits 0-4 (matching noteBits) for extended sustain support.
  -- A bit should be cleared if there's a separate note of that color that overlaps this one.
  } deriving (Show)

instance Bin Note where
  bin = do
    noteTimeOffset <- (.noteTimeOffset) =. word32be
    noteDuration   <- (.noteDuration  ) =. word16be
    noteBits       <- (.noteBits      ) =. word8
    noteAccent     <- (.noteAccent    ) =. word8
    return Note{..}

-- entryType = gh6_expert_drum_note
data NoteExpertDrums = NoteExpertDrums
  { xdNote  :: Note
  , xdGhost :: Word8
  -- Usually 0; 2 means ghost snare, probably matches noteBits
  } deriving (Show)

instance Bin NoteExpertDrums where
  bin = do
    xdNote  <- (.xdNote ) =. bin
    xdGhost <- (.xdGhost) =. word8
    return NoteExpertDrums{..}

-- entryType = gh5_star_note (singleValue :: Word16)
-- entryType = gh5_tapping_note (singleValue :: Word32)
-- others...
data Single a = Single
  { singleTimeOffset :: Word32
  , singleValue      :: a
  } deriving (Show)

binSingle :: BinaryCodec a -> BinaryCodec (Single a)
binSingle val = do
  singleTimeOffset <- (.singleTimeOffset) =. word32be
  singleValue      <- (.singleValue     ) =. val
  return Single{..}

data GuitarBass = GuitarBass
  { gb_instrument :: [Note]
  , gb_tapping    :: [Single Word32] -- values are durations
  , gb_starpower  :: [Single Word16] -- values are durations
  } deriving (Show)

data Drums = Drums
  { drums_instrument :: Either [Note] [NoteExpertDrums]
  , drums_starpower  :: [Single Word16] -- values are durations
  , drums_drumfill   :: [Single Word32] -- values are end points
  } deriving (Show)

data VocalNote = VocalNote
  { vnTimeOffset :: Word32
  , vnDuration   :: Word16
  , vnPitch      :: Word8
  } deriving (Show)

-- vocal slides representation
-- for a slide like the following where t1-t2 is p1 and t3-t4 is p2:
-- 4 timestamps, t1 ----- t2 ////// t3 ------ t4
-- there are 3 notes:
-- VocalNote t1 (t2 - t1) p1
-- VocalNote t2 (t3 - t2) 2
-- VocalNote t3 (t4 - t3) p2
-- and only 1 lyric (at t1)

instance Bin VocalNote where
  bin = do
    vnTimeOffset <- (.vnTimeOffset) =. word32be
    vnDuration   <- (.vnDuration  ) =. word16be
    vnPitch      <- (.vnPitch     ) =. word8
    return VocalNote{..}

binLyric :: Int -> BinaryCodec T.Text
binLyric size = Codec
  { codecIn  = T.takeWhile (/= '\0') . TE.decodeUtf16BE . BL.toStrict <$> getRemainingLazyByteString
  , codecOut = fmapArg $ \t -> putByteString $ B.take size $ TE.encodeUtf16BE t <> B.replicate size 0
  }

-- Latin-1 is a guess, might actually be UTF-8
binLyricWii :: Int -> BinaryCodec T.Text
binLyricWii size = Codec
  { codecIn  = T.takeWhile (/= '\0') . TE.decodeLatin1 . BL.toStrict <$> getRemainingLazyByteString
  , codecOut = fmapArg $ \t -> putByteString $ B.take size $ B8.pack (T.unpack t) <> B.replicate size 0
  }

data GHNoteFile = GHNoteFile

  { gh_guitareasy            :: GuitarBass
  , gh_guitarmedium          :: GuitarBass
  , gh_guitarhard            :: GuitarBass
  , gh_guitarexpert          :: GuitarBass

  , gh_basseasy              :: GuitarBass
  , gh_bassmedium            :: GuitarBass
  , gh_basshard              :: GuitarBass
  , gh_bassexpert            :: GuitarBass

  , gh_drumseasy             :: Drums
  , gh_drumsmedium           :: Drums
  , gh_drumshard             :: Drums
  , gh_drumsexpert           :: Drums

  , gh_vocals                :: [VocalNote]
  , gh_vocallyrics           :: [Single T.Text]
  , gh_vocalstarpower        :: [Single Word16] -- values are durations
  , gh_vocalphrase           :: [Word32]
  , gh_vocalfreeform         :: [B.ByteString] -- TODO real format. 10 bytes long

  -- harmonies were planned for GH apparently!?
  -- these are only in some songs like That's What You Get (dlc784)
  , gh_backup_vocalphrase    :: Maybe [Word32]
  , gh_backup_vocals         :: Maybe [VocalNote]
  , gh_backup_vocalfreeform  :: Maybe [B.ByteString]
  , gh_backup_vocallyrics    :: Maybe [Single T.Text]
  , gh_backup_vocalstarpower :: Maybe [Single Word16]

  , gh_fretbar               :: [Word32]
  , gh_timesig               :: [TimeSig]
  , gh_bandmoment            :: [Single Word32]

  , gh_markers               :: [Single Word32] -- these match IDs in .qs.en
  , gh_vocalmarkers          :: [Single T.Text]
  , gh_backup_vocalmarkers   :: Maybe [Single T.Text]

  } deriving (Show)

data SongPakContents = SongPakContents
  { stringBank :: HM.HashMap Word32 T.Text
  , noteFile   :: GHNoteFile
  , qbNotes    :: [(B.ByteString, RTB.T U.Seconds (Word8, Word8))]
  }

-- Load from an uncompressed _song.pak.xen
loadSongPakContents :: (Monad m) => B.ByteString -> BL.ByteString -> StackTraceT m SongPakContents
loadSongPakContents songKey bs = inside "loading _song.pak.*" $ do
  nodes <- inside "splitting pak nodes" $ splitPakNodes pakFormatWoR bs Nothing
  let findNodeKey = listToMaybe . nodesOfType
      nodesOfType t = filter (\(n, _) -> n.nodeFileType == t) nodes
      bank = qsBank $ nodesOfType ".qs.en"
  case findNodeKey ".note" of
    Nothing                -> fail ".note not found"
    Just (_node, noteData) -> do
      noteFile <- inside "loading .note file" $ loadNoteFile noteData
      let ?endian = BigEndian
      qbSections <- fmap concat $ forM (nodesOfType ".qb") $ \(_, qbBytes) -> runGetM parseQB qbBytes
      let noteTrackNames = do
            suffix <- ["anim_notes", "drums_notes", "cameras_notes", "crowd_notes", "lightshow_notes"]
            return (qbKeyCRC $ songKey <> "_" <> suffix, suffix)
          arrayToTrack (x : y : xs) = At (fromIntegral x / 1000) (fromNoteValue y) $ arrayToTrack xs
          arrayToTrack _            = ANil
          fromNoteValue noteValue = let
            pitch = fromIntegral $ noteValue `shiftR` 16
            velocity = min 127 $ fromIntegral noteValue
            in (pitch, velocity)
          qbNotes = do
            QBSectionArray k _ (QBArrayOfInteger ns) <- qbSections
            suffix <- toList $ lookup k noteTrackNames
            return (suffix, RTB.fromAbsoluteEventList $ arrayToTrack ns)
      return SongPakContents
        { stringBank = bank
        , noteFile = noteFile
        , qbNotes = qbNotes
        }

loadNoteFile :: (MonadFail m) => BL.ByteString -> m GHNoteFile
loadNoteFile noteData = do
  (_dlcKey, entries) <- runGetM getNote noteData
  let findEntryKey = findEntryCRC . qbKeyCRC
      findEntryCRC crc = listToMaybe $ filter ((== crc) . (.entryIdentifier)) entries
      getEntryKey k = case findEntryKey k of
        Nothing    -> fail $ ".note entry not found: " <> show k
        Just entry -> return entry
      readEntry :: (MonadFail m) => BinaryCodec a -> NoteEntry -> m [a]
      readEntry cdc entry = mapM (runGetM (codecIn cdc) . BL.fromStrict) entry.entryContents
      interpret :: (MonadFail m) => [(QBKey, Word32, NoteEntry -> m a)] -> NoteEntry -> m a
      interpret options entry = let
        isMatch (aType, len, reader) = do
          guard $ entry.entryType == aType
          guard $ entry.entryElementSize == len
          return reader
        in case mapMaybe isMatch options of
          []         -> fail $ "Couldn't match one of these types/sizes: " <> show [(aType, len) | (aType, len, _) <- options]
          reader : _ -> reader entry
      getGB prefix = do
        gb_instrument <- getEntryKey (prefix <> "instrument") >>= interpret [("gh5_instrument_note", 8, readEntry bin)]
        gb_tapping <- getEntryKey (prefix <> "tapping") >>= interpret [("gh5_tapping_note", 8, readEntry $ binSingle word32be)]
        gb_starpower <- getEntryKey (prefix <> "starpower") >>= interpret [("gh5_star_note", 6, readEntry $ binSingle word16be)]
        return GuitarBass{..}
      getDrums diff = do
        drums_instrument <- getEntryKey ("drums" <> diff <> "instrument") >>= interpret
          [ ("gh5_instrument_note", 8, fmap Left <$> readEntry bin)
          , ("gh6_expert_drum_note", 9, fmap Right <$> readEntry bin)
          ]
        drums_starpower <- getEntryKey ("drums" <> diff <> "starpower") >>= interpret [("gh5_star_note", 6, readEntry $ binSingle word16be)]
        drums_drumfill <- getEntryKey (diff <> "drumfill") >>= interpret [("gh5_drumfill_note", 8, readEntry $ binSingle word32be)]
        return Drums{..}
  gh_fretbar               <- getEntryKey "fretbar" >>= interpret [("gh5_fretbar_note", 4, readEntry word32be)]
  gh_timesig               <- getEntryKey "timesig" >>= interpret [("gh5_timesig_note", 6, readEntry bin)]
  gh_guitareasy            <- getGB "guitareasy"
  gh_guitarmedium          <- getGB "guitarmedium"
  gh_guitarhard            <- getGB "guitarhard"
  gh_guitarexpert          <- getGB "guitarexpert"
  gh_basseasy              <- getGB "basseasy"
  gh_bassmedium            <- getGB "bassmedium"
  gh_basshard              <- getGB "basshard"
  gh_bassexpert            <- getGB "bassexpert"
  gh_drumseasy             <- getDrums "easy"
  gh_drumsmedium           <- getDrums "medium"
  gh_drumshard             <- getDrums "hard"
  gh_drumsexpert           <- getDrums "expert"
  gh_vocals                <- getEntryKey "vocals" >>= interpret [("gh5_vocal_note", 7, readEntry bin)]
  gh_vocalstarpower        <- getEntryKey "vocalstarpower" >>= interpret [("gh5_star_note", 6, readEntry $ binSingle word16be)]
  gh_vocallyrics           <- getEntryKey "vocallyrics" >>= interpret
    [ ("gh5_vocal_lyric", 68, readEntry $ binSingle $ binLyric    64) -- typical
    , ("gh5_vocal_lyric", 36, readEntry $ binSingle $ binLyricWii 32) -- seen in gh5 wii free bird, maybe all wii?
    ]
  gh_vocalphrase           <- getEntryKey "vocalphrase" >>= interpret [("gh5_vocal_phrase", 4, readEntry word32be)]
  gh_vocalfreeform         <- getEntryKey "vocalfreeform" >>= interpret [("gh5_vocal_freeform_note", 10, readEntry $ byteString 10)]
  gh_bandmoment            <- getEntryKey "bandmoment" >>= interpret [("gh5_band_moment_note", 8, readEntry $ binSingle word32be)]
  gh_markers               <- getEntryKey "guitarmarkers" >>= interpret [("gh5_marker_note", 8, readEntry $ binSingle word32be)]
  gh_vocalmarkers          <- getEntryKey "vocalsmarkers" >>= interpret
    [ ("gh5_vocal_marker_note", 260, readEntry $ binSingle $ binLyric    256) -- typical
    , ("gh5_vocal_marker_note", 132, readEntry $ binSingle $ binLyricWii 128) -- seen in gh5 wii free bird, maybe all wii?
    ]
  gh_backup_vocalmarkers   <- forM (findEntryKey "backup_vocalsmarkers") $ interpret [("gh5_vocal_marker_note", 260, readEntry $ binSingle $ binLyric 256)]
  gh_backup_vocalphrase    <- forM (findEntryKey "backup_vocalphrase") $ interpret [("gh5_vocal_phrase", 4, readEntry word32be)]
  gh_backup_vocals         <- forM (findEntryKey "backup_vocals") $ interpret [("gh5_vocal_note", 7, readEntry bin)]
  gh_backup_vocalfreeform  <- forM (findEntryKey "backup_vocalfreeform") $ interpret [("gh5_vocal_freeform_note", 10, readEntry $ byteString 10)]
  gh_backup_vocallyrics    <- forM (findEntryKey "backup_vocallyrics") $ interpret [("gh5_vocal_lyric", 68, readEntry $ binSingle $ binLyric 64)]
  gh_backup_vocalstarpower <- forM (findEntryKey "backup_vocalstarpower") $ interpret [("gh5_star_note", 6, readEntry $ binSingle word16be)]

  return GHNoteFile{..}

makeWoRNoteFile :: GHNoteFile -> [NoteEntry]
makeWoRNoteFile GHNoteFile{..} = execWriter $ do
  let makeEntry n aType size cdc xs = tell $ return $ NoteEntry
        { entryIdentifier = n
        , entryCount = fromIntegral $ length xs
        , entryType = aType
        , entryElementSize = size
        , entryContents = map (BL.toStrict . runPut . void . codecOut cdc) xs
        }
      makeDrums k = \case
        Left  gh5  -> makeEntry k "gh5_instrument_note"  8 bin gh5
        Right gh6x -> makeEntry k "gh6_expert_drum_note" 9 bin gh6x


  makeEntry "basseasyinstrument"      "gh5_instrument_note"     8   bin                        gh_basseasy.gb_instrument
  makeEntry "bassmediuminstrument"    "gh5_instrument_note"     8   bin                        gh_bassmedium.gb_instrument
  makeEntry "basshardinstrument"      "gh5_instrument_note"     8   bin                        gh_basshard.gb_instrument
  makeEntry "bassexpertinstrument"    "gh5_instrument_note"     8   bin                        gh_bassexpert.gb_instrument

  makeEntry "basseasystarpower"       "gh5_star_note"           6   (binSingle word16be)       gh_basseasy.gb_starpower
  makeEntry "bassmediumstarpower"     "gh5_star_note"           6   (binSingle word16be)       gh_bassmedium.gb_starpower
  makeEntry "basshardstarpower"       "gh5_star_note"           6   (binSingle word16be)       gh_basshard.gb_starpower
  makeEntry "bassexpertstarpower"     "gh5_star_note"           6   (binSingle word16be)       gh_bassexpert.gb_starpower

  makeEntry "basseasytapping"         "gh5_tapping_note"        8   (binSingle word32be)       gh_basseasy.gb_tapping
  makeEntry "bassmediumtapping"       "gh5_tapping_note"        8   (binSingle word32be)       gh_bassmedium.gb_tapping
  makeEntry "basshardtapping"         "gh5_tapping_note"        8   (binSingle word32be)       gh_basshard.gb_tapping
  makeEntry "bassexperttapping"       "gh5_tapping_note"        8   (binSingle word32be)       gh_bassexpert.gb_tapping

  makeDrums "drumseasyinstrument"                                                              gh_drumseasy.drums_instrument
  makeDrums "drumsmediuminstrument"                                                            gh_drumsmedium.drums_instrument
  makeDrums "drumshardinstrument"                                                              gh_drumshard.drums_instrument
  makeDrums "drumsexpertinstrument"                                                            gh_drumsexpert.drums_instrument

  makeEntry "drumseasystarpower"      "gh5_star_note"           6   (binSingle word16be)       gh_drumseasy.drums_starpower
  makeEntry "drumsmediumstarpower"    "gh5_star_note"           6   (binSingle word16be)       gh_drumsmedium.drums_starpower
  makeEntry "drumshardstarpower"      "gh5_star_note"           6   (binSingle word16be)       gh_drumshard.drums_starpower
  makeEntry "drumsexpertstarpower"    "gh5_star_note"           6   (binSingle word16be)       gh_drumsexpert.drums_starpower

  makeEntry "easydrumfill"            "gh5_drumfill_note"       8   (binSingle word32be)       gh_drumseasy.drums_drumfill
  makeEntry "mediumdrumfill"          "gh5_drumfill_note"       8   (binSingle word32be)       gh_drumsmedium.drums_drumfill
  makeEntry "harddrumfill"            "gh5_drumfill_note"       8   (binSingle word32be)       gh_drumshard.drums_drumfill
  makeEntry "expertdrumfill"          "gh5_drumfill_note"       8   (binSingle word32be)       gh_drumsexpert.drums_drumfill

  makeEntry "guitareasyinstrument"    "gh5_instrument_note"     8   bin                        gh_guitareasy.gb_instrument
  makeEntry "guitarmediuminstrument"  "gh5_instrument_note"     8   bin                        gh_guitarmedium.gb_instrument
  makeEntry "guitarhardinstrument"    "gh5_instrument_note"     8   bin                        gh_guitarhard.gb_instrument
  makeEntry "guitarexpertinstrument"  "gh5_instrument_note"     8   bin                        gh_guitarexpert.gb_instrument

  makeEntry "guitareasystarpower"     "gh5_star_note"           6   (binSingle word16be)       gh_guitareasy.gb_starpower
  makeEntry "guitarmediumstarpower"   "gh5_star_note"           6   (binSingle word16be)       gh_guitarmedium.gb_starpower
  makeEntry "guitarhardstarpower"     "gh5_star_note"           6   (binSingle word16be)       gh_guitarhard.gb_starpower
  makeEntry "guitarexpertstarpower"   "gh5_star_note"           6   (binSingle word16be)       gh_guitarexpert.gb_starpower

  makeEntry "guitareasytapping"       "gh5_tapping_note"        8   (binSingle word32be)       gh_guitareasy.gb_tapping
  makeEntry "guitarmediumtapping"     "gh5_tapping_note"        8   (binSingle word32be)       gh_guitarmedium.gb_tapping
  makeEntry "guitarhardtapping"       "gh5_tapping_note"        8   (binSingle word32be)       gh_guitarhard.gb_tapping
  makeEntry "guitarexperttapping"     "gh5_tapping_note"        8   (binSingle word32be)       gh_guitarexpert.gb_tapping

  makeEntry "bandmoment"              "gh5_band_moment_note"    8   (binSingle word32be)       gh_bandmoment
  makeEntry "fretbar"                 "gh5_fretbar_note"        4   word32be                   gh_fretbar
  makeEntry "guitarmarkers"           "gh5_marker_note"         8   (binSingle word32be)       gh_markers
  makeEntry "timesig"                 "gh5_timesig_note"        6   bin                        gh_timesig

  makeEntry "vocals"                  "gh5_vocal_note"          7   bin                        gh_vocals
  makeEntry "vocalfreeform"           "gh5_vocal_freeform_note" 10  (byteString 10)            gh_vocalfreeform
  makeEntry "vocallyrics"             "gh5_vocal_lyric"         68  (binSingle $ binLyric 64)  gh_vocallyrics
  makeEntry "vocalphrase"             "gh5_vocal_phrase"        4   word32be                   gh_vocalphrase
  makeEntry "vocalsmarkers"           "gh5_vocal_marker_note"   260 (binSingle $ binLyric 256) gh_vocalmarkers
  makeEntry "vocalstarpower"          "gh5_star_note"           6   (binSingle word16be)       gh_vocalstarpower

  -- only in some songs
  mapM_ (makeEntry "backup_vocalphrase"    "gh5_vocal_phrase"        4   word32be                  ) gh_backup_vocalphrase
  mapM_ (makeEntry "backup_vocals"         "gh5_vocal_note"          7   bin                       ) gh_backup_vocals
  mapM_ (makeEntry "backup_vocalfreeform"  "gh5_vocal_freeform_note" 10  (byteString 10)           ) gh_backup_vocalfreeform
  mapM_ (makeEntry "backup_vocallyrics"    "gh5_vocal_lyric"         68  (binSingle $ binLyric 64) ) gh_backup_vocallyrics
  mapM_ (makeEntry "backup_vocalstarpower" "gh5_star_note"           6   (binSingle word16be)      ) gh_backup_vocalstarpower
  mapM_ (makeEntry "backup_vocalsmarkers"  "gh5_vocal_marker_note"   260 (binSingle $ binLyric 256)) gh_backup_vocalmarkers

tappingToFunction :: [Single Word32] -> (Word32 -> Bool)
-- TODO make a map or something to optimize this
tappingToFunction taps w = any
  (\tap -> tap.singleTimeOffset <= w && w < tap.singleTimeOffset + tap.singleValue)
  taps

convertVocals :: (Word32 -> U.Beats) -> [VocalNote] -> [Single T.Text] -> [Word32] -> [Single Word16] -> V.VocalTrack U.Beats
convertVocals toBeats vox lyrics phrases sp = let
  fromPairs ps = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort ps
  -- GH uses = before 2nd syllable, instead of - after 1st syllable like RB
  fixDash = \case
    (t1, lyric1) : (t2, T.uncons -> Just ('=', lyric2)) : rest
      -> fixDash $ (t1, lyric1 <> "-") : (t2, lyric2) : rest
    x : xs -> x : fixDash xs
    [] -> []
  -- Slides use a note connecting the two flat notes with special pitch 2,
  -- so we just remove those and put a + lyric at the end of each one
  (slideNotes, notSlides) = partition (\vn -> vn.vnPitch == 2) vox
  -- TODO problem on Fight Fire With Fire (Metallica):
  -- several "fire" in choruses have a slide between "fi" and "re",
  -- so the "re" gets a "+" and messes things up.
  -- probably we should just ignore the slide (remove "+")
  pluses = flip map slideNotes $ \vn ->
    ( toBeats $ vn.vnTimeOffset + fromIntegral vn.vnDuration
    , "+"
    )
  -- Talkies have a special pitch of 26, so we add the # to those
  talkyPositions = Set.fromList $ map (toBeats . (.vnTimeOffset)) $ filter (\vn -> vn.vnPitch == 26) vox
  addTalkies = map $ \pair@(t, lyric) -> if Set.member t talkyPositions
    then (t, lyric <> "#")
    else pair
  -- Each phrase will be drawn between two adjacent points in the phrase points list,
  -- if there are vocal notes in it. Then determine SP phrases so they match
  drawnPhrases = flip mapMaybe (zip phrases $ drop 1 phrases) $ \(start, end) -> let
    notesInPhrase = filter (\vn -> start <= vn.vnTimeOffset && vn.vnTimeOffset < end) vox
    isStarPower = case notesInPhrase of
      [] -> False
      vn : _ -> any (\(Single t len) -> t <= vn.vnTimeOffset && vn.vnTimeOffset < t + fromIntegral len) sp
    in do
      guard $ not $ null notesInPhrase
      return (start, end, isStarPower)
  in mempty
    { V.vocalLyrics = fromPairs $ (pluses <>) $ addTalkies $ fixDash $ flip map lyrics $ \single ->
      (toBeats single.singleTimeOffset, single.singleValue)
    , V.vocalPhrase1 = fromPairs $ drawnPhrases >>= \(start, end, _) ->
      [(toBeats start, True), (toBeats end, False)]
    , V.vocalOverdrive = fromPairs $ drawnPhrases >>= \(start, end, isSP) -> do
      guard isSP
      [(toBeats start, True), (toBeats end, False)]
    , V.vocalNotes = fmap (\case EdgeOn () p -> (p, True); EdgeOff p -> (p, False))
      $ splitEdgesSimple $ fromPairs $ flip map notSlides $ \vn -> let
        pos = toBeats vn.vnTimeOffset
        len = toBeats (vn.vnTimeOffset + fromIntegral vn.vnDuration) - pos
        pitch
          -- TODO GH might use a different octave reference? or maybe it's flexible
          | vn.vnPitch < 36 = minBound
          | vn.vnPitch > 84 = maxBound
          | otherwise       = toEnum $ fromIntegral vn.vnPitch - 36
        in (pos, ((), pitch, len))
    }

ghToMidi :: HM.HashMap Word32 T.Text -> GHNoteFile -> F.Song (F.FixedFile U.Beats)
ghToMidi bank nf = let
  toSeconds :: Word32 -> U.Seconds
  toSeconds = (/ 1000) . fromIntegral
  sigMap = Map.fromList [ (ts.tsTimestamp, (ts.tsNumerator, ts.tsDenominator)) | ts <- nf.gh_timesig ]
  barSigs = [ (t, maybe 4 (snd . snd) $ Map.lookupLE t sigMap) | t <- nf.gh_fretbar ]
  tempos = U.tempoMapFromBPS $ let
    makeTempo (t1, denom) (t2, _) = let
      secs = toSeconds t2 - toSeconds t1
      beats = 4 / fromIntegral denom
      in (U.makeTempo beats secs, beats)
    temposGaps = zipWith makeTempo barSigs (drop 1 barSigs)
    in RTB.fromPairList $ zip (0 : map snd temposGaps) (map fst temposGaps)
  toBeats :: Word32 -> U.Beats
  toBeats = U.unapplyTempoMap tempos . toSeconds
  fromPairs ps = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort ps
  getGB :: GuitarBass -> Five.FiveDifficulty U.Beats
  getGB gb = emit5' $ fromPairs $ let
    isTap = tappingToFunction gb.gb_tapping
    in gb.gb_instrument >>= \note -> do
      fret <- if note.noteBits `testBit` 5
        then [Nothing] -- open note
        else concat
          [ [Just Five.Green  | note.noteBits `testBit` 0]
          , [Just Five.Red    | note.noteBits `testBit` 1]
          , [Just Five.Yellow | note.noteBits `testBit` 2]
          , [Just Five.Blue   | note.noteBits `testBit` 3]
          , [Just Five.Orange | note.noteBits `testBit` 4]
          ]
      let pos = toBeats note.noteTimeOffset
          sht | isTap note.noteTimeOffset = Tap
              | note.noteBits `testBit` 6 = HOPO
              | otherwise                 = Strum
          len = toBeats (note.noteTimeOffset + fromIntegral note.noteDuration) - pos
      return (pos, ((fret, sht), Just len))
  getDrums :: Drums -> D.DrumDifficulty U.Beats
  getDrums drum = mempty
    { D.drumGems = let
      expert = either (map $ \note -> NoteExpertDrums note 0) id drum.drums_instrument
      in fromPairs $ expert >>= \note -> let
        noteBit gem b = do
          let ghost = note.xdGhost `testBit` b
          guard $ (note.xdNote.noteBits `testBit` b) || ghost
          let vel | note.xdNote.noteAccent `testBit` b = D.VelocityAccent
                  | ghost                              = D.VelocityGhost
                  | otherwise                          = D.VelocityNormal
          return (toBeats note.xdNote.noteTimeOffset, (gem, vel))
        -- TODO handle drum sustains, translate to lanes?
        in concat
          [ noteBit (D.Pro D.Green ())  0
          , noteBit D.Red               1
          , noteBit (D.Pro D.Yellow ()) 2
          , noteBit (D.Pro D.Blue ())   3
          , noteBit D.Orange            4
          , noteBit D.Kick              5
          ]
    }
  getKick2x :: Drums -> RTB.T U.Beats ()
  getKick2x drum = fromPairs $ either id (map (.xdNote)) drum.drums_instrument >>= \note -> do
    guard $ (note.noteBits `testBit` 6) && not (note.noteBits `testBit` 5)
    -- the not-bit-5 is because some songs like Tom Sawyer have both set for the Expert kicks
    -- TODO detect this and import as separate kick track, to fix songs like The Shortest Straw
    return (toBeats note.noteTimeOffset, ())
  getOD = RTB.fromAbsoluteEventList . ATB.fromPairList . sort . concatMap
    (\(Single t len) -> [(toBeats t, True), (toBeats $ t + fromIntegral len, False)])
  markers
    = fromPairs
    $ mapMaybe (\(Single t qsID) -> let
      mevent = case HM.lookup qsID bank of
        Nothing  -> Nothing
        Just str -> case T.stripPrefix "\\u[m]" str of
          Just sect -> Just $ Right $ simpleSection sect
          Nothing -> case str of
            "\\L_ENDOFSONG" -> Just $ Left ()
            _               -> Nothing
      in (\x -> (toBeats t, x)) <$> mevent)
    $ nf.gh_markers
  fixed = mempty
    { F.fixedPartGuitar = mempty
      { Five.fiveDifficulties = Map.fromList
        [ (Expert, getGB nf.gh_guitarexpert)
        , (Hard  , getGB nf.gh_guitarhard  )
        , (Medium, getGB nf.gh_guitarmedium)
        , (Easy  , getGB nf.gh_guitareasy  )
        ]
      , Five.fiveOverdrive = getOD nf.gh_guitarexpert.gb_starpower
      }
    , F.fixedPartBass = mempty
      { Five.fiveDifficulties = Map.fromList
        [ (Expert, getGB nf.gh_bassexpert)
        , (Hard  , getGB nf.gh_basshard  )
        , (Medium, getGB nf.gh_bassmedium)
        , (Easy  , getGB nf.gh_basseasy  )
        ]
      , Five.fiveOverdrive = getOD nf.gh_bassexpert.gb_starpower
      }
    , F.fixedPartDrums = mempty
      { D.drumDifficulties = Map.fromList
        [ (Expert, getDrums nf.gh_drumsexpert)
        , (Hard  , getDrums nf.gh_drumshard  )
        , (Medium, getDrums nf.gh_drumsmedium)
        , (Easy  , getDrums nf.gh_drumseasy  )
        ]
      , D.drumOverdrive = getOD nf.gh_drumsexpert.drums_starpower
      , D.drumKick2x = getKick2x nf.gh_drumsexpert
      }
    , F.fixedPartVocals = convertVocals toBeats
      nf.gh_vocals
      nf.gh_vocallyrics
      nf.gh_vocalphrase
      nf.gh_vocalstarpower
    , F.fixedHarm2      = fromMaybe mempty $ convertVocals toBeats
      <$> nf.gh_backup_vocals
      <*> nf.gh_backup_vocallyrics
      <*> nf.gh_backup_vocalphrase
      <*> nf.gh_backup_vocalstarpower
    , F.fixedEvents = mempty
      { eventsSections = RTB.mapMaybe (\case Right sect -> Just sect; _ -> Nothing) markers
      , eventsEnd      = RTB.mapMaybe (\case Left  ()   -> Just ()  ; _ -> Nothing) markers
      }
    }
  in F.Song
    { F.s_tempos = tempos
    , F.s_signatures = U.measureMapFromTimeSigs U.Truncate $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ do
      ts <- nf.gh_timesig
      let unit = 4 / fromIntegral ts.tsDenominator
          len = fromIntegral ts.tsNumerator * unit
      return (toBeats ts.tsTimestamp, U.TimeSig len unit)
    , F.s_tracks = fixed
    }
