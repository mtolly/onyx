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

import           Control.Monad                    (replicateM)
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (sort)
import qualified Data.Map                         as Map
import           Data.Maybe                       (listToMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Word
import           Guitars                          (emit5')
import           Neversoft.Checksum               (qbKeyCRC)
import           Neversoft.Pak
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as F
import           RockBand.Common                  (Difficulty (..),
                                                   StrumHOPOTap (..))
import qualified Sound.MIDI.Util                  as U

data NoteEntry = NoteEntry
  { entryIdentifier  :: Word32
  , entryCount       :: Word32
  , entryType        :: Word32
  , entryElementSize :: Word32
  , entryContents    :: [B.ByteString]
  } deriving (Show)

readNote :: Get (B.ByteString, [NoteEntry])
readNote = do
  header <- getByteString 0x1C
  entries <- let
    getEntries = isEmpty >>= \case
      True -> return []
      False -> do
        entryIdentifier <- getWord32be
        entryCount <- getWord32be
        entryType <- getWord32be
        entryElementSize <- getWord32be
        entryContents <- replicateM (fromIntegral entryCount)
          $ getByteString $ fromIntegral entryElementSize
        (NoteEntry{..} :) <$> getEntries
    in getEntries
  return (header, entries)

data TimeSig = TimeSig
  { tsTimestamp   :: Word32
  , tsNumerator   :: Word8
  , tsDenominator :: Word8
  } deriving (Show)

getTimeSig :: Get TimeSig
getTimeSig = do
  tsTimestamp <- getWord32be
  tsNumerator <- getWord8
  tsDenominator <- getWord8
  return TimeSig{..}

data Note = Note
  { noteTimeOffset :: Word32
  , noteDuration   :: Word16
  , noteBits       :: Word8
  , noteUnknown    :: Word8
  , noteUnknown2   :: Maybe Word8 -- extra byte seen in some drum tracks, maybe X+?
  } deriving (Show)

getNote :: Get Note
getNote = do
  noteTimeOffset <- getWord32be
  noteDuration <- getWord16be
  noteBits <- getWord8
  noteUnknown <- getWord8
  noteUnknown2 <- isEmpty >>= \case
    True -> return Nothing
    False -> Just <$> getWord8
  return Note{..}

data Single a = Single
  { singleTimeOffset :: Word32
  , singleValue      :: a
  } deriving (Show)

getSingle :: Get a -> Get (Single a)
getSingle val = do
  singleTimeOffset <- getWord32be
  singleValue      <- val
  return Single{..}

data GuitarBass = GuitarBass
  { gb_instrument :: [Note]
  , gb_tapping    :: [Single Word32]
  , gb_starpower  :: [Single Word16]
  } deriving (Show)

data Drums = Drums
  { drums_instrument :: [Note]
  , drums_starpower  :: [Single Word16]
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

getVocalNote :: Get VocalNote
getVocalNote = do
  vnTimeOffset <- getWord32be
  vnDuration <- getWord16be
  vnPitch <- getWord8
  return VocalNote{..}

data GHSongPak = GHSongPak

  { gh_guitareasy     :: GuitarBass
  , gh_guitarmedium   :: GuitarBass
  , gh_guitarhard     :: GuitarBass
  , gh_guitarexpert   :: GuitarBass

  , gh_basseasy       :: GuitarBass
  , gh_bassmedium     :: GuitarBass
  , gh_basshard       :: GuitarBass
  , gh_bassexpert     :: GuitarBass

  , gh_drumseasy      :: Drums
  , gh_drumsmedium    :: Drums
  , gh_drumshard      :: Drums
  , gh_drumsexpert    :: Drums

  , gh_vocals         :: [VocalNote]
  , gh_vocallyrics    :: [Single T.Text]
  , gh_vocalstarpower :: [Single Word16]

  , gh_sections       :: [T.Text]
  , gh_fretbar        :: [Word32]
  , gh_timesig        :: [TimeSig]

  } deriving (Show)

-- Load from a _song.pak.xen
loadSongPak :: (MonadFail m) => BL.ByteString -> m GHSongPak
loadSongPak bs = do
  let nodes = splitPakNodes bs
      findNodeKey = findNodeCRC . qbKeyCRC
      findNodeCRC crc = listToMaybe $ filter (\(n, _) -> nodeFileType n == crc) nodes
      stringBank = qsBank nodes
  case findNodeKey ".note" of
    Nothing               -> fail ".note not found"
    Just (node, noteData) -> do
      let (header, entries) = runGet readNote noteData
          findEntryKey = findEntryCRC . qbKeyCRC
          findEntryCRC crc = listToMaybe $ filter ((== crc) . entryIdentifier) entries
          getEntryKey k = case findEntryKey k of
            Nothing    -> fail $ ".note entry not found: " <> show k
            Just entry -> return entry
          interpret sizes getter entry = if elem (entryElementSize entry) sizes
            then return $ map (runGet getter . BL.fromStrict) $ entryContents entry
            else fail $ "Invalid size of note entry elements: " <> show (entryElementSize entry) <> " but expected " <> show sizes
          getGB prefix = do
            gb_instrument <- getEntryKey (prefix <> "instrument") >>= interpret [8] getNote
            gb_tapping <- getEntryKey (prefix <> "tapping") >>= interpret [8] (getSingle getWord32be)
            gb_starpower <- getEntryKey (prefix <> "starpower") >>= interpret [6] (getSingle getWord16be)
            return GuitarBass{..}
          getDrums prefix = do
            drums_instrument <- getEntryKey (prefix <> "instrument") >>= interpret [8, 9] getNote
            drums_starpower <- getEntryKey (prefix <> "starpower") >>= interpret [6] (getSingle getWord16be)
            return Drums{..}
      gh_fretbar <- getEntryKey "fretbar" >>= interpret [4] getWord32be
      gh_timesig <- getEntryKey "timesig" >>= interpret [6] getTimeSig
      gh_guitareasy <- getGB "guitareasy"
      gh_guitarmedium <- getGB "guitarmedium"
      gh_guitarhard <- getGB "guitarhard"
      gh_guitarexpert <- getGB "guitarexpert"
      gh_basseasy <- getGB "basseasy"
      gh_bassmedium <- getGB "bassmedium"
      gh_basshard <- getGB "basshard"
      gh_bassexpert <- getGB "bassexpert"
      gh_drumseasy <- getDrums "drumseasy"
      gh_drumsmedium <- getDrums "drumsmedium"
      gh_drumshard <- getDrums "drumshard"
      gh_drumsexpert <- getDrums "drumsexpert"
      gh_vocals <- getEntryKey "vocals" >>= interpret [7] getVocalNote
      gh_vocalstarpower <- getEntryKey "vocalstarpower" >>= interpret [6] (getSingle getWord16be)
      let getLyric = T.takeWhile (/= '\0') . TE.decodeUtf16BE . BL.toStrict <$> getRemainingLazyByteString
      gh_vocallyrics <- getEntryKey "vocallyrics" >>= interpret [68] (getSingle getLyric)
      gh_sections <- return [] -- TODO
      return GHSongPak{..}

ghToMidi :: GHSongPak -> RBFile.Song (RBFile.FixedFile U.Beats)
ghToMidi pak = let
  toSeconds :: Word32 -> U.Seconds
  toSeconds = (/ 1000) . fromIntegral
  fretbarSecs = map toSeconds $ gh_fretbar pak
  tempos = U.tempoMapFromBPS $ let
    makeTempo t1 t2 = U.makeTempo 1 (realToFrac $ t2 - t1)
    in RTB.fromPairList
      $ zip (0 : repeat 1)
      $ zipWith makeTempo fretbarSecs (drop 1 fretbarSecs)
  toBeats :: Word32 -> U.Beats
  toBeats = U.unapplyTempoMap tempos . toSeconds
  getGB gb = emit5' $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort $ let
    isTap _ = False -- TODO
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
          sht = if noteBits note `testBit` 6
            then HOPO
            else Strum
          len = toBeats (noteTimeOffset note + fromIntegral (noteDuration note)) - pos
      return (pos, ((fret, sht), Just len))
  getOD = RTB.fromAbsoluteEventList . ATB.fromPairList . sort . concatMap
    (\(Single t len) -> [(toBeats t, True), (toBeats $ t + fromIntegral len, False)])
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
    }
  in RBFile.Song
    { RBFile.s_tempos = tempos
    , RBFile.s_signatures = U.measureMapFromLengths U.Truncate RTB.empty -- TODO
    , RBFile.s_tracks = fixed
    }
