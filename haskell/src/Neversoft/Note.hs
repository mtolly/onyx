{- |
Built from:
https://github.com/AerialX/rawksd
https://github.com/Nanook/Queen-Bee
expertarraytochart.bms by GHFear
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Neversoft.Note where

import           Control.Monad                    (forM, forM_, guard,
                                                   replicateM, void)
import           Control.Monad.Trans.Writer       (execWriter, tell)
import           Data.Binary.Codec.Class
import           Data.Bits
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (sort)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Guitars                          (emit5')
import           Neversoft.Checksum               (qbKeyCRC)
import           Neversoft.Pak
import qualified RockBand.Codec.Drums             as D
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as F
import qualified RockBand.Codec.Vocal             as V
import           RockBand.Common                  (Difficulty (..), Edge (..),
                                                   StrumHOPOTap (..),
                                                   splitEdgesSimple)
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
  0x40C001A3 <- getWord32be
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
  -- always 31 on That's What You Get X guitar
  -- usually 0 on That's What You Get X drums
  -- 31 (0b11111) on drums means accent (all non-kick gems?)
  -- 29 (0b11101) on drums means yellow accent, but not snare accent
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
  , drums_drumfill   :: [Single Word32]
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

  -- unknown entryIdentifier strings
  , gh_markers               :: [Single Word32] -- these match IDs in .qs.en
  , gh_vocalmarkers          :: [Single T.Text]
  , gh_backup_vocalmarkers   :: Maybe [Single T.Text]

  } deriving (Show)

-- Load from an uncompressed _song.pak.xen
loadSongPak :: (MonadFail m) => BL.ByteString -> m GHNoteFile
loadSongPak bs = do
  let nodes = splitPakNodes bs
      findNodeKey = findNodeCRC . qbKeyCRC
      findNodeCRC crc = listToMaybe $ filter (\(n, _) -> nodeFileType n == crc) nodes
  case findNodeKey ".note" of
    Nothing                -> fail ".note not found"
    Just (_node, noteData) -> loadNoteFile noteData

loadNoteFile :: (MonadFail m) => BL.ByteString -> m GHNoteFile
loadNoteFile noteData = do
  let (_dlcKey, entries) = runGet getNote noteData
      findEntryKey = findEntryCRC . qbKeyCRC
      findEntryCRC crc = listToMaybe $ filter ((== crc) . entryIdentifier) entries
      getEntryKey k = case findEntryKey k of
        Nothing    -> fail $ ".note entry not found: " <> show k
        Just entry -> return entry
      getEntryCRC n = case findEntryCRC n of
        Nothing    -> fail $ ".note entry not found: " <> show n
        Just entry -> return entry
      readEntry cdc entry = map (runGet (codecIn cdc) . BL.fromStrict) $ entryContents entry
      interpret options entry = let
        isMatch (aType, len, reader) = do
          guard $ entryType entry == qbKeyCRC aType
          guard $ entryElementSize entry == len
          return reader
        in case mapMaybe isMatch options of
          []         -> fail $ "Couldn't match one of these types/sizes: " <> show [(aType, len) | (aType, len, _) <- options]
          reader : _ -> return $ reader entry
      getGB prefix = do
        gb_instrument <- getEntryKey (prefix <> "instrument") >>= interpret [("gh5_instrument_note", 8, readEntry bin)]
        gb_tapping <- getEntryKey (prefix <> "tapping") >>= interpret [("gh5_tapping_note", 8, readEntry $ binSingle word32be)]
        gb_starpower <- getEntryKey (prefix <> "starpower") >>= interpret [("gh5_star_note", 6, readEntry $ binSingle word16be)]
        return GuitarBass{..}
      getDrums diff = do
        drums_instrument <- getEntryKey ("drums" <> diff <> "instrument") >>= interpret
          [ ("gh5_instrument_note", 8, Left <$> readEntry bin)
          , ("gh6_expert_drum_note", 9, Right <$> readEntry bin)
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
  gh_vocallyrics <- getEntryKey "vocallyrics" >>= interpret [("gh5_vocal_lyric", 68, readEntry $ binSingle $ binLyric 64)]
  gh_vocalphrase <- getEntryKey "vocalphrase" >>= interpret [("gh5_vocal_phrase", 4, readEntry word32be)]
  gh_vocalfreeform <- getEntryKey "vocalfreeform" >>= interpret [("gh5_vocal_freeform_note", 10, readEntry $ byteString 10)]
  gh_bandmoment <- getEntryKey "bandmoment" >>= interpret [("gh5_band_moment_note", 8, readEntry $ binSingle word32be)]
  gh_markers <- getEntryCRC 0x92511d84 >>= interpret [("gh5_marker_note", 8, readEntry $ binSingle word32be)]
  gh_vocalmarkers <- getEntryCRC 0x032292a7 >>= interpret [("gh5_vocal_marker_note", 260, readEntry $ binSingle $ binLyric 256)]
  gh_backup_vocalmarkers <- forM (findEntryCRC 1140412083) $ interpret [("gh5_vocal_marker_note", 260, readEntry $ binSingle $ binLyric 256)]
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
  makeEntryCRC 0x032292a7             "gh5_vocal_marker_note"   260 (binSingle $ binLyric 256) gh_vocalmarkers
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
  makeEntryCRC 0x92511d84             "gh5_marker_note"         8   (binSingle word32be)       gh_markers
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
  mapM_ (makeEntry "backup_vocalmarkers"   "gh5_vocal_marker_note"   260 (binSingle $ binLyric 256)) gh_backup_vocalmarkers

tappingToFunction :: [Single Word32] -> (Word32 -> Bool)
-- TODO make a map or something to optimize this
tappingToFunction taps w = any
  (\tap -> singleTimeOffset tap <= w && w < singleTimeOffset tap + singleValue tap)
  taps

ghToMidi :: GHNoteFile -> RBFile.Song (RBFile.FixedFile U.Beats)
ghToMidi pak = let
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
          [ [Just F.Green  | noteBits note `testBit` 0]
          , [Just F.Red    | noteBits note `testBit` 1]
          , [Just F.Yellow | noteBits note `testBit` 2]
          , [Just F.Blue   | noteBits note `testBit` 3]
          , [Just F.Orange | noteBits note `testBit` 4]
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
          [ noteBit (D.Pro D.Green ()) 0
          , noteBit D.Red 1
          , noteBit (D.Pro D.Yellow ()) 2
          , noteBit (D.Pro D.Blue ()) 3
          , noteBit D.Orange 4
          , noteBit D.Kick 5
          ]
    }
  getKick2x drum = fromPairs $ either id (map xdNote) (drums_instrument drum) >>= \note -> do
    guard $ (noteBits note `testBit` 6) && not (noteBits note `testBit` 5)
    -- the not-bit-5 is because some songs like Tom Sawyer have both set for the Expert kicks
    return (toBeats $ noteTimeOffset note, ())
  getOD = RTB.fromAbsoluteEventList . ATB.fromPairList . sort . concatMap
    (\(Single t len) -> [(toBeats t, True), (toBeats $ t + fromIntegral len, False)])
  -- TODO fix slides (convert from pitch 2 format)
  -- and multisyllable word format (move = on 2nd syllable to - on 1st)
  getVocal vox lyrics _phrases sp = mempty
    { V.vocalLyrics = fromPairs $ flip map lyrics $ \single ->
      (toBeats $ singleTimeOffset single, singleValue single)
    , V.vocalPhrase1 = RTB.empty -- TODO
    , V.vocalOverdrive = getOD sp
    , V.vocalNotes = fmap (\case EdgeOn () p -> (p, True); EdgeOff p -> (p, False))
      $ splitEdgesSimple $ fromPairs $ flip map vox $ \vn -> let
        pos = toBeats $ vnTimeOffset vn
        len = toBeats (vnTimeOffset vn + fromIntegral (vnDuration vn)) - pos
        pitch
          -- TODO GH might use a different octave reference? or maybe it's flexible
          | vnPitch vn < 36 = minBound
          | vnPitch vn > 84 = maxBound
          | otherwise       = toEnum $ fromIntegral (vnPitch vn) - 36
        in (pos, ((), pitch, len))
    }
  fixed = mempty
    { RBFile.fixedPartGuitar = mempty
      { F.fiveDifficulties = Map.fromList
        [ (Expert, getGB $ gh_guitarexpert pak)
        , (Hard  , getGB $ gh_guitarhard   pak)
        , (Medium, getGB $ gh_guitarmedium pak)
        , (Easy  , getGB $ gh_guitareasy   pak)
        ]
      , F.fiveOverdrive = getOD $ gb_starpower $ gh_guitarexpert pak
      }
    , RBFile.fixedPartBass = mempty
      { F.fiveDifficulties = Map.fromList
        [ (Expert, getGB $ gh_bassexpert pak)
        , (Hard  , getGB $ gh_basshard   pak)
        , (Medium, getGB $ gh_bassmedium pak)
        , (Easy  , getGB $ gh_basseasy   pak)
        ]
      , F.fiveOverdrive = getOD $ gb_starpower $ gh_bassexpert pak
      }
    , RBFile.fixedPartDrums = mempty
      { D.drumDifficulties = Map.fromList
        [ (Expert, getDrums $ gh_drumsexpert pak)
        , (Hard  , getDrums $ gh_drumshard   pak)
        , (Medium, getDrums $ gh_drumsmedium pak)
        , (Easy  , getDrums $ gh_drumseasy   pak)
        ]
      , D.drumOverdrive = getOD $ drums_starpower $ gh_drumsexpert pak
      , D.drumKick2x = getKick2x $ gh_drumsexpert pak
      }
    , RBFile.fixedPartVocals = getVocal
      (gh_vocals         pak)
      (gh_vocallyrics    pak)
      (gh_vocalphrase    pak)
      (gh_vocalstarpower pak)
    , RBFile.fixedHarm2      = fromMaybe mempty $ getVocal
      <$> gh_backup_vocals         pak
      <*> gh_backup_vocallyrics    pak
      <*> gh_backup_vocalphrase    pak
      <*> gh_backup_vocalstarpower pak
    -- TODO practice sections
    }
  in RBFile.Song
    { RBFile.s_tempos = tempos
    , RBFile.s_signatures = U.measureMapFromTimeSigs U.Truncate $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ do
      ts <- gh_timesig pak
      let unit = 4 / fromIntegral (tsDenominator ts)
          len = fromIntegral (tsNumerator ts) * unit
      return (toBeats $ tsTimestamp ts, U.TimeSig len unit)
    , RBFile.s_tracks = fixed
    }
