{- |
Built from:
https://github.com/AerialX/rawksd
https://github.com/Nanook/Queen-Bee
expertarraytochart.bms by GHFear
-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GuitarHero5 where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (forM, guard, replicateM,
                                                   unless, when)
import           Data.Bifunctor
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isSpace)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (sort)
import qualified Data.Map                         as Map
import           Data.Maybe                       (listToMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Word
import           GuitarHero5.Audio                (qbKeyCRC)
import           Guitars                          (emit5')
import           Numeric                          (showHex)
import           Numeric                          (readHex)
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as F
import           RockBand.Common                  (Difficulty (..),
                                                   StrumHOPOTap (..))
import qualified Sound.MIDI.Util                  as U

data Node = Node
  { nodeFileType       :: Word32
  , nodeOffset         :: Word32
  , nodeSize           :: Word32
  , nodeFilenamePakKey :: Word32
  , nodeFilenameKey    :: Word32
  , nodeFilenameCRC    :: Word32
  , nodeUnknown        :: Word32
  , nodeFlags          :: Word32
  } deriving (Show)

splitNodes :: BL.ByteString -> [(Node, BL.ByteString)]
splitNodes bs = let
  end = qbKeyCRC "last"
  end2 = qbKeyCRC ".last"
  getNodes = do
    posn <- fromIntegral <$> bytesRead
    nodeFileType <- getWord32be
    if elem nodeFileType [end, end2]
      then return []
      else do
        nodeOffset         <- (+ posn) <$> getWord32be
        nodeSize           <- getWord32be
        nodeFilenamePakKey <- getWord32be
        nodeFilenameKey    <- getWord32be
        nodeFilenameCRC    <- getWord32be
        nodeUnknown        <- getWord32be
        nodeFlags          <- getWord32be
        (Node{..} :) <$> getNodes
  attachData node = let
    goToData
      = BL.take (fromIntegral $ nodeSize node)
      . BL.drop (fromIntegral $ nodeOffset node)
    in (node, goToData bs)
  in map attachData $ runGet getNodes bs

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

data GH5SongPak = GH5SongPak

  { gh5_guitareasy     :: GuitarBass
  , gh5_guitarmedium   :: GuitarBass
  , gh5_guitarhard     :: GuitarBass
  , gh5_guitarexpert   :: GuitarBass

  , gh5_basseasy       :: GuitarBass
  , gh5_bassmedium     :: GuitarBass
  , gh5_basshard       :: GuitarBass
  , gh5_bassexpert     :: GuitarBass

  , gh5_drumseasy      :: Drums
  , gh5_drumsmedium    :: Drums
  , gh5_drumshard      :: Drums
  , gh5_drumsexpert    :: Drums

  , gh5_vocals         :: [VocalNote]
  , gh5_vocallyrics    :: [Single T.Text]
  , gh5_vocalstarpower :: [Single Word16]

  , gh5_sections       :: [T.Text]
  , gh5_fretbar        :: [Word32]
  , gh5_timesig        :: [TimeSig]

  } deriving (Show)

qsBank :: [(Node, BL.ByteString)] -> Map.Map Word32 T.Text
qsBank nodes = Map.fromList $ do
  (node, nodeData) <- nodes
  guard $ nodeFileType node == qbKeyCRC ".qs.en"
  -- first 2 chars are "\xFF\xFE"
  line <- T.lines $ TE.decodeUtf16LE $ BL.toStrict $ BL.drop 2 nodeData
  guard $ T.length line > 8
  (x, s) <- readHex $ T.unpack $ T.take 8 line
  guard $ all isSpace s
  return (x, T.drop 9 line) -- TODO strip quotes and weird escapes

-- Load a _song.pak.xen
testChart :: FilePath -> IO GH5SongPak
testChart f = do
  nodes <- splitNodes <$> BL.readFile f
  let findNodeKey = findNodeCRC . qbKeyCRC
      findNodeCRC crc = listToMaybe $ filter (\(n, _) -> nodeFileType n == crc) nodes
      stringBank = qsBank nodes
  -- mapM_ print stringBank
  case findNodeKey ".note" of
    Nothing               -> error ".note not found"
    Just (node, noteData) -> do
      let (header, entries) = runGet readNote noteData
          findEntryKey = findEntryCRC . qbKeyCRC
          findEntryCRC crc = listToMaybe $ filter ((== crc) . entryIdentifier) entries
          getEntryKey k = case findEntryKey k of
            Nothing    -> error $ ".note entry not found: " <> show k
            Just entry -> return entry
          interpret sizes getter entry = if elem (entryElementSize entry) sizes
            then return $ map (runGet getter . BL.fromStrict) $ entryContents entry
            else error $ "Invalid size of note entry elements: " <> show (entryElementSize entry) <> " but expected " <> show sizes
          getGB prefix = do
            gb_instrument <- getEntryKey (prefix <> "instrument") >>= interpret [8] getNote
            gb_tapping <- getEntryKey (prefix <> "tapping") >>= interpret [8] (getSingle getWord32be)
            gb_starpower <- getEntryKey (prefix <> "starpower") >>= interpret [6] (getSingle getWord16be)
            return GuitarBass{..}
          getDrums prefix = do
            drums_instrument <- getEntryKey (prefix <> "instrument") >>= interpret [8, 9] getNote
            drums_starpower <- getEntryKey (prefix <> "starpower") >>= interpret [6] (getSingle getWord16be)
            return Drums{..}
      gh5_fretbar <- getEntryKey "fretbar" >>= interpret [4] getWord32be
      gh5_timesig <- getEntryKey "timesig" >>= interpret [6] getTimeSig
      gh5_guitareasy <- getGB "guitareasy"
      gh5_guitarmedium <- getGB "guitarmedium"
      gh5_guitarhard <- getGB "guitarhard"
      gh5_guitarexpert <- getGB "guitarexpert"
      gh5_basseasy <- getGB "basseasy"
      gh5_bassmedium <- getGB "bassmedium"
      gh5_basshard <- getGB "basshard"
      gh5_bassexpert <- getGB "bassexpert"
      gh5_drumseasy <- getDrums "drumseasy"
      gh5_drumsmedium <- getDrums "drumsmedium"
      gh5_drumshard <- getDrums "drumshard"
      gh5_drumsexpert <- getDrums "drumsexpert"
      gh5_vocals <- getEntryKey "vocals" >>= interpret [7] getVocalNote
      gh5_vocalstarpower <- getEntryKey "vocalstarpower" >>= interpret [6] (getSingle getWord16be)
      let getLyric = T.takeWhile (/= '\0') . TE.decodeUtf16BE . BL.toStrict <$> getRemainingLazyByteString
      gh5_vocallyrics <- getEntryKey "vocallyrics" >>= interpret [68] (getSingle getLyric)
      gh5_sections <- return [] -- TODO
      return GH5SongPak{..}

gh5ToMidi :: GH5SongPak -> RBFile.Song (RBFile.FixedFile U.Beats)
gh5ToMidi pak = let
  toSeconds :: Word32 -> U.Seconds
  toSeconds = (/ 1000) . fromIntegral
  fretbarSecs = map toSeconds $ gh5_fretbar pak
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
        [ (Expert, getGB $ gh5_guitarexpert pak)
        , (Hard  , getGB $ gh5_guitarhard   pak)
        , (Medium, getGB $ gh5_guitarmedium pak)
        , (Easy  , getGB $ gh5_guitareasy   pak)
        ]
      , F.fiveOverdrive = getOD $ gb_starpower $ gh5_guitarexpert pak
      }
    , RBFile.fixedPartBass = mempty
      { F.fiveDifficulties = Map.fromList
        [ (Expert, getGB $ gh5_bassexpert pak)
        , (Hard  , getGB $ gh5_basshard   pak)
        , (Medium, getGB $ gh5_bassmedium pak)
        , (Easy  , getGB $ gh5_basseasy   pak)
        ]
      , F.fiveOverdrive = getOD $ gb_starpower $ gh5_bassexpert pak
      }
    }
  in RBFile.Song
    { RBFile.s_tempos = tempos
    , RBFile.s_signatures = U.measureMapFromLengths U.Truncate RTB.empty -- TODO
    , RBFile.s_tracks = fixed
    }

data QBSection qs k
  = QBSectionArray k k (QBArray qs k)
  | QBSectionStruct k k [QBStructItem qs k]
  deriving (Show, Functor)

data QBArray qs k
  = QBArrayOfQbKey [k]
  | QBArrayOfInteger [Word32]
  | QBArrayOfStruct [[QBStructItem qs k]]
  deriving (Show, Functor)

data QBStructItem qs k
  = QBStructHeader -- empty
  | QBStructItemStruct k [QBStructItem qs k]
  | QBStructItemQbKey k k
  | QBStructItemString k B.ByteString
  | QBStructItemQbKeyString k k
  | QBStructItemQbKeyStringQs k qs
  | QBStructItemInteger k Word32
  | QBStructItemFloat k Float
  | QBStructItemArray k (QBArray qs k)
  deriving (Show, Functor)

instance Bifunctor QBSection where
  first f = \case
    QBSectionArray x y arr -> QBSectionArray x y $ first f arr
    QBSectionStruct x y items -> QBSectionStruct x y $ map (first f) items
  second = fmap

instance Bifunctor QBArray where
  first f = \case
    QBArrayOfQbKey ks -> QBArrayOfQbKey ks
    QBArrayOfInteger ns -> QBArrayOfInteger ns
    QBArrayOfStruct structs -> QBArrayOfStruct $ map (map $ first f) structs
  second = fmap

instance Bifunctor QBStructItem where
  first f = \case
    QBStructHeader -> QBStructHeader
    QBStructItemStruct x items -> QBStructItemStruct x $ map (first f) items
    QBStructItemQbKey x y -> QBStructItemQbKey x y
    QBStructItemString x y -> QBStructItemString x y
    QBStructItemQbKeyString x y -> QBStructItemQbKeyString x y
    QBStructItemQbKeyStringQs x qs -> QBStructItemQbKeyStringQs x $ f qs
    QBStructItemInteger x y -> QBStructItemInteger x y
    QBStructItemFloat x y -> QBStructItemFloat x y
    QBStructItemArray x arr -> QBStructItemArray x $ first f arr
  second = fmap

shouldBeAt :: Word32 -> Get ()
shouldBeAt w = do
  p <- fromIntegral <$> bytesRead
  unless (p == w) $ fail $ unwords
    [ "QB parser position expected to be"
    , "0x" <> showHex w ""
    , "but we're at"
    , "0x" <> showHex p ""
    ]

parseQBArray :: Get (QBArray Word32 Word32, Word32)
parseQBArray = do
  p1 <- getWord32be
  p2 <- getWord32be
  shouldBeAt p1
  arrayType <- getWord32be
  array <- case arrayType of
    0x00010D00 -> do
      len <- fromIntegral <$> getWord32be
      -- TODO figure out why this pointer is sometimes absent
      hasPointer <- lookAhead $ do
        p3 <- getWord32be
        (shouldBeAt p3 >> return True) <|> return False
      when hasPointer $ skip 4
      QBArrayOfQbKey <$> replicateM len getWord32be
    0x00010100 -> do
      len <- fromIntegral <$> getWord32be
      p3 <- getWord32be
      shouldBeAt p3
      QBArrayOfInteger <$> replicateM len getWord32be
    0x00010A00 -> do
      len <- fromIntegral <$> getWord32be
      p3 <- getWord32be
      shouldBeAt p3
      structStarts <- replicateM len getWord32be
      fmap QBArrayOfStruct $ forM structStarts $ \p4 -> do
        shouldBeAt p4
        parseQBStruct
    _ -> fail $ "Unrecognized array type: 0x" <> showHex arrayType ""
  return (array, p2)

parseQBStruct :: Get [QBStructItem Word32 Word32]
parseQBStruct = do
  itemType <- getWord32be
  (item, nextPosition) <- case itemType of
    0x00000100 -> do
      p <- getWord32be
      -- assuming these are always empty?
      return (QBStructHeader, p)
    0x00010D00 -> do
      x <- getWord32be
      y <- getWord32be
      p <- getWord32be
      return (QBStructItemQbKey x y, p)
    0x00011C00 -> do
      x <- getWord32be
      y <- getWord32be
      p <- getWord32be
      return (QBStructItemQbKeyStringQs x y, p)
    0x00011A00 -> do
      x <- getWord32be
      y <- getWord32be
      p <- getWord32be
      return (QBStructItemQbKeyString x y, p)
    0x00010100 -> do
      x <- getWord32be
      y <- getWord32be
      p <- getWord32be
      return (QBStructItemInteger x y, p)
    0x00010300 -> do
      x <- getWord32be
      start <- getWord32be
      end <- getWord32be
      shouldBeAt start
      -- TODO what if a string is the last element in a struct?
      -- shouldn't end be 0 in that case?
      b <- getByteString $ fromIntegral $ end - start
      let nullTerm = B.takeWhile (/= 0) b
      return (QBStructItemString x nullTerm, end)
    0x00010200 -> do
      x <- getWord32be
      f <- getFloatbe
      p <- getWord32be
      return (QBStructItemFloat x f, p)
    0x00010C00 -> do
      x <- getWord32be
      (array, p) <- parseQBArray
      return (QBStructItemArray x array, p)
    0x00010A00 -> do
      x <- getWord32be
      p1 <- getWord32be
      p2 <- getWord32be
      shouldBeAt p1
      items <- parseQBStruct
      return (QBStructItemStruct x items, p2)
    _ -> fail $ "Unrecognized struct item type: 0x" <> showHex itemType ""
  case nextPosition of
    0 -> return [item] -- this is the last item
    _ -> do
      shouldBeAt nextPosition
      (item :) <$> parseQBStruct

parseQBSection :: Get (QBSection Word32 Word32)
parseQBSection = do
  sectionType <- getWord32be
  case sectionType of
    0x00200C00 {- SectionArray -} -> do
      itemQbKeyCrc <- getWord32be
      fileId <- getWord32be
      (array, _) <- parseQBArray
      -- the snd above should be 0, I think
      return $ QBSectionArray itemQbKeyCrc fileId array
    0x00200A00 {- SectionStruct -} -> do
      itemQbKeyCrc <- getWord32be
      fileId <- getWord32be
      p1 <- getWord32be
      _reserved <- getWord32be
      shouldBeAt p1
      QBSectionStruct itemQbKeyCrc fileId <$> parseQBStruct
    _ -> fail $ "Unrecognized section type: 0x" <> showHex sectionType ""

parseQB :: Get [QBSection Word32 Word32]
parseQB = do
  _magic <- getWord32be
  fileSize <- getWord32be
  _unknown <- getByteString 20
  let parseSections = do
        pos <- fromIntegral <$> bytesRead
        if pos >= fileSize
          then return []
          else (:) <$> parseQBSection <*> parseSections
  parseSections

type QsResult = Either Word32 T.Text

loadTextPakXen :: FilePath -> IO [QBSection QsResult Word32]
loadTextPakXen f = do
  nodes <- splitNodes <$> BL.readFile f
  (_, qbFile) <- case filter (\(n, _) -> nodeFileType n == qbKeyCRC ".qb") nodes of
    [qb] -> return qb
    ns   -> fail $ show (length ns) <> " .qb files found in " <> f
  let qs = qsBank nodes
      qb = runGet parseQB qbFile
      lookupN mapping n = case Map.lookup n mapping of
        Nothing -> Left n
        Just s  -> Right s
  return $ map (first $ lookupN qs) qb

data SongInfo = SongInfo
  { songName       :: B.ByteString -- this is an id like "dlc747"
  , songTitle      :: T.Text
  , songArtist     :: T.Text
  , songYear       :: Int
  , songAlbumTitle :: T.Text
  , songDoubleKick :: Bool
  } deriving (Show)

parseSongInfo :: [QBSection QsResult Word32] -> Either String [SongInfo]
parseSongInfo sections = case sections of
  [_, QBSectionStruct _ _ items] -> let
    itemToInfos = \case
      QBStructItemStruct _ songEntries -> do
        songName <- case [ s | QBStructItemString k s <- songEntries, k == qbKeyCRC "name" ] of
          s : _ -> Right s
          []    -> Left "parseSongInfo: couldn't get song internal name"
        songTitle <- case [ s | QBStructItemQbKeyStringQs k (Right s) <- songEntries, k == qbKeyCRC "title" ] of
          s : _ -> Right s
          []    -> Left "parseSongInfo: couldn't get song title"
        songArtist <- case [ s | QBStructItemQbKeyStringQs k (Right s) <- songEntries, k == qbKeyCRC "artist" ] of
          s : _ -> Right s
          []    -> Left "parseSongInfo: couldn't get song artist"
        songYear <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "year" ] of
          n : _ -> Right $ fromIntegral n
          []    -> Left "parseSongInfo: couldn't get song year"
        songAlbumTitle <- case [ s | QBStructItemQbKeyStringQs k (Right s) <- songEntries, k == qbKeyCRC "album_title" ] of
          s : _ -> Right s
          []    -> Left "parseSongInfo: couldn't get song album_title"
        songDoubleKick <- case [ n | QBStructItemInteger k n <- songEntries, k == qbKeyCRC "double_kick" ] of
          0 : _ -> Right False
          1 : _ -> Right True
          [] -> Right False
          _ -> Left "parseSongInfo: couldn't understand double_kick field"
        Right [SongInfo{..}]
      QBStructHeader -> Right []
      _ -> Left "parseSongInfo: entry in song list that isn't a struct or header"
    in concat <$> mapM itemToInfos items
  _ -> Left "parseSongInfo: unexpected sections"
