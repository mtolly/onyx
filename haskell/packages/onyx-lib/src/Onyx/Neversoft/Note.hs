{- |
Built from:
https://github.com/AerialX/rawksd
https://github.com/Nanook/Queen-Bee
expertarraytochart.bms by GHFear
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Onyx.Neversoft.Note where

import           Control.Monad                    (forM, forM_, guard,
                                                   replicateM, void)
import           Control.Monad.Trans.Writer       (execWriter, tell)
import           Data.Bits
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
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
                                                   splitEdgesSimple)
import qualified Onyx.MIDI.Track.Drums            as D
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import qualified Onyx.MIDI.Track.Vocal            as V
import           Onyx.Neversoft.CRC               (qbKeyCRC)
import           Onyx.Neversoft.Pak
import           Onyx.Sections                    (simpleSection)
import           Onyx.Xbox.STFS                   (runGetM)
import qualified Sound.MIDI.Util                  as U

data NoteEntry = NoteEntry
  { entryIdentifier  :: Word32
  , entryCount       :: Word32
  , entryType        :: Word32
  , entryElementSize :: Word32
  , entryContents    :: [B.ByteString]
  } deriving (Show)

getNote :: Get (Word32, [NoteEntry])
getNote = do
  magic <- getWord32be
  case magic of
    0x40C001A3 -> return () -- GHWOR
    0x40A000D2 -> return () -- GH5
    _          -> fail $ "Unrecognized .note magic: 0x" <> showHex magic ""
  dlcKey <- getWord32be -- qb key for e.g. "dlc784"
  count <- getWord32be
  note <- getWord32be
  guard $ note == qbKeyCRC "note"
  zeroes <- getByteString 12
  guard $ B.all (== 0) zeroes
  entries <- replicateM (fromIntegral count) $ do
    entryIdentifier <- getWord32be
    entryCount <- getWord32be
    entryType <- getWord32be
    entryElementSize <- getWord32be
    entryContents <- replicateM (fromIntegral entryCount)
      $ getByteString $ fromIntegral entryElementSize
    return NoteEntry{..}
  return (dlcKey, entries)

putNote :: Word32 -> [NoteEntry] -> Put
putNote dlcKey entries = do
  putWord32be 0x40C001A3
  putWord32be dlcKey
  putWord32be $ fromIntegral $ length entries
  putWord32be $ qbKeyCRC "note"
  putByteString $ B.replicate 12 0
  forM_ entries $ \entry -> do
    putWord32be $ entryIdentifier entry
    putWord32be $ entryCount entry
    putWord32be $ entryType entry
    putWord32be $ entryElementSize entry
    mapM_ putByteString $ entryContents entry

data TimeSig = TimeSig
  { tsTimestamp   :: Word32
  , tsNumerator   :: Word8
  , tsDenominator :: Word8
  } deriving (Show)

instance Bin TimeSig where
  bin = do
    tsTimestamp   <- tsTimestamp   =. word32be
    tsNumerator   <- tsNumerator   =. word8
    tsDenominator <- tsDenominator =. word8
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
    noteTimeOffset <- noteTimeOffset =. word32be
    noteDuration   <- noteDuration   =. word16be
    noteBits       <- noteBits       =. word8
    noteAccent     <- noteAccent     =. word8
    return Note{..}

-- entryType = gh6_expert_drum_note
data NoteExpertDrums = NoteExpertDrums
  { xdNote  :: Note
  , xdGhost :: Word8
  -- Usually 0; 2 means ghost snare, probably matches noteBits
  } deriving (Show)

instance Bin NoteExpertDrums where
  bin = do
    xdNote  <- xdNote  =. bin
    xdGhost <- xdGhost =. word8
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
  singleTimeOffset <- singleTimeOffset =. word32be
  singleValue      <- singleValue      =. val
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
    vnTimeOffset <- vnTimeOffset =. word32be
    vnDuration   <- vnDuration   =. word16be
    vnPitch      <- vnPitch      =. word8
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

-- Load from an uncompressed _song.pak.xen
loadSongPak :: (MonadFail m) => BL.ByteString -> m (HM.HashMap Word32 T.Text, GHNoteFile)
loadSongPak bs = do
  nodes <- splitPakNodes BigEndian bs Nothing
  let findNodeKey = listToMaybe . nodesOfType
      nodesOfType t = filter (\(n, _) -> nodeFileType n == qbKeyCRC t) nodes
      bank = qsBank $ nodesOfType ".qs.en"
  case findNodeKey ".note" of
    Nothing                -> fail ".note not found"
    Just (_node, noteData) -> do
      noteFile <- loadNoteFile noteData
      return (bank, noteFile)

loadNoteFile :: (MonadFail m) => BL.ByteString -> m GHNoteFile
loadNoteFile noteData = do
  (_dlcKey, entries) <- runGetM getNote noteData
  let findEntryKey = findEntryCRC . qbKeyCRC
      findEntryCRC crc = listToMaybe $ filter ((== crc) . entryIdentifier) entries
      getEntryKey k = case findEntryKey k of
        Nothing    -> fail $ ".note entry not found: " <> show k
        Just entry -> return entry
      readEntry cdc entry = mapM (runGetM (codecIn cdc) . BL.fromStrict) $ entryContents entry
      interpret options entry = let
        isMatch (aType, len, reader) = do
          guard $ entryType entry == qbKeyCRC aType
          guard $ entryElementSize entry == len
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
  gh_fretbar <- getEntryKey "fretbar" >>= interpret [("gh5_fretbar_note", 4, readEntry word32be)]
  gh_timesig <- getEntryKey "timesig" >>= interpret [("gh5_timesig_note", 6, readEntry bin)]
  gh_guitareasy <- getGB "guitareasy"
  gh_guitarmedium <- getGB "guitarmedium"
  gh_guitarhard <- getGB "guitarhard"
  gh_guitarexpert <- getGB "guitarexpert"
  gh_basseasy <- getGB "basseasy"
  gh_bassmedium <- getGB "bassmedium"
  gh_basshard <- getGB "basshard"
  gh_bassexpert <- getGB "bassexpert"
  gh_drumseasy <- getDrums "easy"
  gh_drumsmedium <- getDrums "medium"
  gh_drumshard <- getDrums "hard"
  gh_drumsexpert <- getDrums "expert"
  gh_vocals <- getEntryKey "vocals" >>= interpret [("gh5_vocal_note", 7, readEntry bin)]
  gh_vocalstarpower <- getEntryKey "vocalstarpower" >>= interpret [("gh5_star_note", 6, readEntry $ binSingle word16be)]
  gh_vocallyrics <- getEntryKey "vocallyrics" >>= interpret
    [ ("gh5_vocal_lyric", 68, readEntry $ binSingle $ binLyric    64) -- typical
    , ("gh5_vocal_lyric", 36, readEntry $ binSingle $ binLyricWii 32) -- seen in gh5 wii free bird, maybe all wii?
    ]
  gh_vocalphrase <- getEntryKey "vocalphrase" >>= interpret [("gh5_vocal_phrase", 4, readEntry word32be)]
  gh_vocalfreeform <- getEntryKey "vocalfreeform" >>= interpret [("gh5_vocal_freeform_note", 10, readEntry $ byteString 10)]
  gh_bandmoment <- getEntryKey "bandmoment" >>= interpret [("gh5_band_moment_note", 8, readEntry $ binSingle word32be)]
  gh_markers <- getEntryKey "guitarmarkers" >>= interpret [("gh5_marker_note", 8, readEntry $ binSingle word32be)]
  gh_vocalmarkers <- getEntryKey "vocalsmarkers" >>= interpret
    [ ("gh5_vocal_marker_note", 260, readEntry $ binSingle $ binLyric    256) -- typical
    , ("gh5_vocal_marker_note", 132, readEntry $ binSingle $ binLyricWii 128) -- seen in gh5 wii free bird, maybe all wii?
    ]
  gh_backup_vocalmarkers <- forM (findEntryKey "backup_vocalsmarkers") $ interpret [("gh5_vocal_marker_note", 260, readEntry $ binSingle $ binLyric 256)]
  gh_backup_vocalphrase <- forM (findEntryKey "backup_vocalphrase") $ interpret [("gh5_vocal_phrase", 4, readEntry word32be)]
  gh_backup_vocals <- forM (findEntryKey "backup_vocals") $ interpret [("gh5_vocal_note", 7, readEntry bin)]
  gh_backup_vocalfreeform <- forM (findEntryKey "backup_vocalfreeform") $ interpret [("gh5_vocal_freeform_note", 10, readEntry $ byteString 10)]
  gh_backup_vocallyrics <- forM (findEntryKey "backup_vocallyrics") $ interpret [("gh5_vocal_lyric", 68, readEntry $ binSingle $ binLyric 64)]
  gh_backup_vocalstarpower <- forM (findEntryKey "backup_vocalstarpower") $ interpret [("gh5_star_note", 6, readEntry $ binSingle word16be)]

  return GHNoteFile{..}

makeWoRNoteFile :: GHNoteFile -> [NoteEntry]
makeWoRNoteFile GHNoteFile{..} = execWriter $ do
  let makeEntry = makeEntryCRC . qbKeyCRC
      makeEntryCRC n aType size cdc xs = tell $ return $ NoteEntry
        { entryIdentifier = n
        , entryCount = fromIntegral $ length xs
        , entryType = qbKeyCRC aType
        , entryElementSize = size
        , entryContents = map (BL.toStrict . runPut . void . codecOut cdc) xs
        }
      makeDrums k = \case
        Left  gh5  -> makeEntry k "gh5_instrument_note"  8 bin gh5
        Right gh6x -> makeEntry k "gh6_expert_drum_note" 9 bin gh6x
  -- this order probably doesn't matter but just matching what some official files have
  makeEntry "drumshardstarpower"      "gh5_star_note"           6   (binSingle word16be)       (drums_starpower gh_drumshard)
  makeEntry "bassmediumstarpower"     "gh5_star_note"           6   (binSingle word16be)       (gb_starpower gh_bassmedium)
  makeEntry "vocalsmarkers"           "gh5_vocal_marker_note"   260 (binSingle $ binLyric 256) gh_vocalmarkers
  makeEntry "drumsexpertstarpower"    "gh5_star_note"           6   (binSingle word16be)       (drums_starpower gh_drumsexpert)
  makeEntry "guitarhardinstrument"    "gh5_instrument_note"     8   bin                        (gb_instrument gh_guitarhard)
  makeEntry "guitarhardtapping"       "gh5_tapping_note"        8   (binSingle word32be)       (gb_tapping gh_guitarhard)
  makeEntry "mediumdrumfill"          "gh5_drumfill_note"       8   (binSingle word32be)       (drums_drumfill gh_drumsmedium)
  makeEntry "guitarmediumtapping"     "gh5_tapping_note"        8   (binSingle word32be)       (gb_tapping gh_guitarmedium)
  makeEntry "guitarexperttapping"     "gh5_tapping_note"        8   (binSingle word32be)       (gb_tapping gh_guitarexpert)
  makeEntry "basshardstarpower"       "gh5_star_note"           6   (binSingle word16be)       (gb_starpower gh_basshard)
  makeEntry "vocalstarpower"          "gh5_star_note"           6   (binSingle word16be)       gh_vocalstarpower
  makeEntry "vocalphrase"             "gh5_vocal_phrase"        4   word32be                   gh_vocalphrase
  makeEntry "guitarhardstarpower"     "gh5_star_note"           6   (binSingle word16be)       (gb_starpower gh_guitarhard)
  makeEntry "guitarmarkers"           "gh5_marker_note"         8   (binSingle word32be)       gh_markers
  makeEntry "fretbar"                 "gh5_fretbar_note"        4   word32be                   gh_fretbar
  makeEntry "vocals"                  "gh5_vocal_note"          7   bin                        gh_vocals
  makeEntry "harddrumfill"            "gh5_drumfill_note"       8   (binSingle word32be)       (drums_drumfill gh_drumshard)
  makeDrums "drumseasyinstrument"                                                              (drums_instrument gh_drumseasy)
  makeEntry "guitareasytapping"       "gh5_tapping_note"        8   (binSingle word32be)       (gb_tapping gh_guitareasy)
  makeEntry "guitarmediumstarpower"   "gh5_star_note"           6   (binSingle word16be)       (gb_starpower gh_guitarmedium)
  makeEntry "basseasyinstrument"      "gh5_instrument_note"     8   bin                        (gb_instrument gh_basseasy)
  makeEntry "guitarexpertinstrument"  "gh5_instrument_note"     8   bin                        (gb_instrument gh_guitarexpert)
  makeEntry "guitarexpertstarpower"   "gh5_star_note"           6   (binSingle word16be)       (gb_starpower gh_guitarexpert)
  makeDrums "drumsexpertinstrument"                                                            (drums_instrument gh_drumsexpert)
  makeEntry "bassmediumtapping"       "gh5_tapping_note"        8   (binSingle word32be)       (gb_tapping gh_bassmedium)
  makeEntry "vocalfreeform"           "gh5_vocal_freeform_note" 10  (byteString 10)            gh_vocalfreeform
  makeEntry "basseasystarpower"       "gh5_star_note"           6   (binSingle word16be)       (gb_starpower gh_basseasy)
  makeDrums "drumsmediuminstrument"                                                            (drums_instrument gh_drumsmedium)
  makeEntry "basshardinstrument"      "gh5_instrument_note"     8   bin                        (gb_instrument gh_basshard)
  makeEntry "vocallyrics"             "gh5_vocal_lyric"         68  (binSingle $ binLyric 64)  gh_vocallyrics
  makeEntry "timesig"                 "gh5_timesig_note"        6   bin                        gh_timesig
  makeEntry "basseasytapping"         "gh5_tapping_note"        8   (binSingle word32be)       (gb_tapping gh_basseasy)
  makeEntry "drumseasystarpower"      "gh5_star_note"           6   (binSingle word16be)       (drums_starpower gh_drumseasy)
  makeEntry "bassmediuminstrument"    "gh5_instrument_note"     8   bin                        (gb_instrument gh_bassmedium)
  makeEntry "guitarmediuminstrument"  "gh5_instrument_note"     8   bin                        (gb_instrument gh_guitarmedium)
  makeEntry "bassexperttapping"       "gh5_tapping_note"        8   (binSingle word32be)       (gb_tapping gh_bassexpert)
  makeEntry "drumsmediumstarpower"    "gh5_star_note"           6   (binSingle word16be)       (drums_starpower gh_drumsmedium)
  makeEntry "easydrumfill"            "gh5_drumfill_note"       8   (binSingle word32be)       (drums_drumfill gh_drumseasy)
  makeEntry "bandmoment"              "gh5_band_moment_note"    8   (binSingle word32be)       gh_bandmoment
  makeEntry "bassexpertinstrument"    "gh5_instrument_note"     8   bin                        (gb_instrument gh_bassexpert)
  makeDrums "drumshardinstrument"                                                              (drums_instrument gh_drumshard)
  makeEntry "expertdrumfill"          "gh5_drumfill_note"       8   (binSingle word32be)       (drums_drumfill gh_drumsexpert)
  makeEntry "basshardtapping"         "gh5_tapping_note"        8   (binSingle word32be)       (gb_tapping gh_basshard)
  makeEntry "bassexpertstarpower"     "gh5_star_note"           6   (binSingle word16be)       (gb_starpower gh_bassexpert)
  makeEntry "guitareasystarpower"     "gh5_star_note"           6   (binSingle word16be)       (gb_starpower gh_guitareasy)
  makeEntry "guitareasyinstrument"    "gh5_instrument_note"     8   bin                        (gb_instrument gh_guitareasy)
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
  (\tap -> singleTimeOffset tap <= w && w < singleTimeOffset tap + singleValue tap)
  taps

ghToMidi :: HM.HashMap Word32 T.Text -> GHNoteFile -> F.Song (F.FixedFile U.Beats)
ghToMidi bank pak = let
  toSeconds :: Word32 -> U.Seconds
  toSeconds = (/ 1000) . fromIntegral
  sigMap = Map.fromList [ (tsTimestamp ts, (tsNumerator ts, tsDenominator ts)) | ts <- gh_timesig pak ]
  barSigs = [ (t, maybe 4 (snd . snd) $ Map.lookupLE t sigMap) | t <- gh_fretbar pak ]
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
  getGB gb = emit5' $ fromPairs $ let
    isTap = tappingToFunction $ gb_tapping gb
    in gb_instrument gb >>= \note -> do
      fret <- if noteBits note `testBit` 5
        then [Nothing] -- open note
        else concat
          [ [Just Five.Green  | noteBits note `testBit` 0]
          , [Just Five.Red    | noteBits note `testBit` 1]
          , [Just Five.Yellow | noteBits note `testBit` 2]
          , [Just Five.Blue   | noteBits note `testBit` 3]
          , [Just Five.Orange | noteBits note `testBit` 4]
          ]
      let pos = toBeats $ noteTimeOffset note
          sht | isTap $ noteTimeOffset note = Tap
              | noteBits note `testBit` 6   = HOPO
              | otherwise                   = Strum
          len = toBeats (noteTimeOffset note + fromIntegral (noteDuration note)) - pos
      return (pos, ((fret, sht), Just len))
  getDrums drum = mempty
    { D.drumGems = let
      expert = either (map $ \note -> NoteExpertDrums note 0) id $ drums_instrument drum
      in fromPairs $ expert >>= \note -> let
        noteBit gem b = do
          guard $ noteBits (xdNote note) `testBit` b
          let vel | noteAccent (xdNote note) `testBit` b = D.VelocityAccent
                  | xdGhost note `testBit` b = D.VelocityGhost
                  | otherwise = D.VelocityNormal
          return (toBeats $ noteTimeOffset $ xdNote note, (gem, vel))
        in concat
          [ noteBit (D.Pro D.Green ())  0
          , noteBit D.Red               1
          , noteBit (D.Pro D.Yellow ()) 2
          , noteBit (D.Pro D.Blue ())   3
          , noteBit D.Orange            4
          , noteBit D.Kick              5
          ]
    }
  getKick2x drum = fromPairs $ either id (map xdNote) (drums_instrument drum) >>= \note -> do
    guard $ (noteBits note `testBit` 6) && not (noteBits note `testBit` 5)
    -- the not-bit-5 is because some songs like Tom Sawyer have both set for the Expert kicks
    -- TODO detect this and import as separate kick track, to fix songs like The Shortest Straw
    return (toBeats $ noteTimeOffset note, ())
  getOD = RTB.fromAbsoluteEventList . ATB.fromPairList . sort . concatMap
    (\(Single t len) -> [(toBeats t, True), (toBeats $ t + fromIntegral len, False)])
  getVocal vox lyrics phrases sp = let
    -- GH uses = before 2nd syllable, instead of - after 1st syllable like RB
    fixDash = \case
      (t1, lyric1) : (t2, T.uncons -> Just ('=', lyric2)) : rest
        -> fixDash $ (t1, lyric1 <> "-") : (t2, lyric2) : rest
      x : xs -> x : fixDash xs
      [] -> []
    -- Slides use a note connecting the two flat notes with special pitch 2,
    -- so we just remove those and put a + lyric at the end of each one
    (slideNotes, notSlides) = partition (\vn -> vnPitch vn == 2) vox
    -- TODO problem on Fight Fire With Fire (Metallica):
    -- several "fire" in choruses have a slide between "fi" and "re",
    -- so the "re" gets a "+" and messes things up.
    -- probably we should just ignore the slide (remove "+")
    pluses = flip map slideNotes $ \vn ->
      ( toBeats $ vnTimeOffset vn + fromIntegral (vnDuration vn)
      , "+"
      )
    -- Talkies have a special pitch of 26, so we add the # to those
    talkyPositions = Set.fromList $ map (toBeats . vnTimeOffset) $ filter (\vn -> vnPitch vn == 26) vox
    addTalkies = map $ \pair@(t, lyric) -> if Set.member t talkyPositions
      then (t, lyric <> "#")
      else pair
    -- Each phrase will be drawn between two adjacent points in the phrase points list,
    -- if there are vocal notes in it. Then determine SP phrases so they match
    drawnPhrases = flip mapMaybe (zip phrases $ drop 1 phrases) $ \(start, end) -> let
      notesInPhrase = filter (\vn -> start <= vnTimeOffset vn && vnTimeOffset vn < end) vox
      isStarPower = case notesInPhrase of
        [] -> False
        vn : _ -> any (\(Single t len) -> t <= vnTimeOffset vn && vnTimeOffset vn < t + fromIntegral len) sp
      in do
        guard $ not $ null notesInPhrase
        return (start, end, isStarPower)
    in mempty
      { V.vocalLyrics = fromPairs $ (pluses <>) $ addTalkies $ fixDash $ flip map lyrics $ \single ->
        (toBeats $ singleTimeOffset single, singleValue single)
      , V.vocalPhrase1 = fromPairs $ drawnPhrases >>= \(start, end, _) ->
        [(toBeats start, True), (toBeats end, False)]
      , V.vocalOverdrive = fromPairs $ drawnPhrases >>= \(start, end, isSP) -> do
        guard isSP
        [(toBeats start, True), (toBeats end, False)]
      , V.vocalNotes = fmap (\case EdgeOn () p -> (p, True); EdgeOff p -> (p, False))
        $ splitEdgesSimple $ fromPairs $ flip map notSlides $ \vn -> let
          pos = toBeats $ vnTimeOffset vn
          len = toBeats (vnTimeOffset vn + fromIntegral (vnDuration vn)) - pos
          pitch
            -- TODO GH might use a different octave reference? or maybe it's flexible
            | vnPitch vn < 36 = minBound
            | vnPitch vn > 84 = maxBound
            | otherwise       = toEnum $ fromIntegral (vnPitch vn) - 36
          in (pos, ((), pitch, len))
      }
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
    $ gh_markers pak
  fixed = mempty
    { F.fixedPartGuitar = mempty
      { Five.fiveDifficulties = Map.fromList
        [ (Expert, getGB $ gh_guitarexpert pak)
        , (Hard  , getGB $ gh_guitarhard   pak)
        , (Medium, getGB $ gh_guitarmedium pak)
        , (Easy  , getGB $ gh_guitareasy   pak)
        ]
      , Five.fiveOverdrive = getOD $ gb_starpower $ gh_guitarexpert pak
      }
    , F.fixedPartBass = mempty
      { Five.fiveDifficulties = Map.fromList
        [ (Expert, getGB $ gh_bassexpert pak)
        , (Hard  , getGB $ gh_basshard   pak)
        , (Medium, getGB $ gh_bassmedium pak)
        , (Easy  , getGB $ gh_basseasy   pak)
        ]
      , Five.fiveOverdrive = getOD $ gb_starpower $ gh_bassexpert pak
      }
    , F.fixedPartDrums = mempty
      { D.drumDifficulties = Map.fromList
        [ (Expert, getDrums $ gh_drumsexpert pak)
        , (Hard  , getDrums $ gh_drumshard   pak)
        , (Medium, getDrums $ gh_drumsmedium pak)
        , (Easy  , getDrums $ gh_drumseasy   pak)
        ]
      , D.drumOverdrive = getOD $ drums_starpower $ gh_drumsexpert pak
      , D.drumKick2x = getKick2x $ gh_drumsexpert pak
      }
    , F.fixedPartVocals = getVocal
      (gh_vocals         pak)
      (gh_vocallyrics    pak)
      (gh_vocalphrase    pak)
      (gh_vocalstarpower pak)
    , F.fixedHarm2      = fromMaybe mempty $ getVocal
      <$> gh_backup_vocals         pak
      <*> gh_backup_vocallyrics    pak
      <*> gh_backup_vocalphrase    pak
      <*> gh_backup_vocalstarpower pak
    , F.fixedEvents = mempty
      { eventsSections = RTB.mapMaybe (\case Right sect -> Just sect; _ -> Nothing) markers
      , eventsEnd      = RTB.mapMaybe (\case Left  ()   -> Just ()  ; _ -> Nothing) markers
      }
    }
  in F.Song
    { F.s_tempos = tempos
    , F.s_signatures = U.measureMapFromTimeSigs U.Truncate $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ do
      ts <- gh_timesig pak
      let unit = 4 / fromIntegral (tsDenominator ts)
          len = fromIntegral (tsNumerator ts) * unit
      return (toBeats $ tsTimestamp ts, U.TimeSig len unit)
    , F.s_tracks = fixed
    }
