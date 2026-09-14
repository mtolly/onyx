{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Nintendo.DonkeyKonga where

import           Control.Monad                         (forM, forM_, guard)
import           Data.Binary.Get
import           Data.Bits                             (shiftL, (.|.))
import qualified Data.ByteString                       as B
import qualified Data.EventList.Relative.TimeBody      as RTB
import           Data.Functor                          ((<&>))
import           Data.List                             (maximumBy, sort)
import           Data.Maybe
import           Data.Ord                              (comparing)
import           Data.Scientific                       (Scientific)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as TE
import           Data.Word
import           GHC.Float                             (castWord32ToFloat)
import qualified Numeric.NonNegative.Class             as NNC
import           Onyx.MIDI.Common
import qualified Sound.MIDI.File.Event                 as E
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.Util                       as U
import           Text.Read                             (readMaybe)

data DKDrum
  = DKYellow
  | DKRed
  | DKPurple
  | DKClap
  deriving (Eq, Ord)

data DKEvent t
  = DKNote DKDrum
  | DKRoll DKDrum t
  deriving (Eq, Ord)

-- Donkey Konga 1 chart format

data SheetBin = SheetBin
  { date :: Word32 -- "20 03 07 30", guessing this is a date
  , bars :: [SheetBar]
  } deriving (Show)

data SheetBar = SheetBar
  { unk1      :: Word16 -- offset used for bar start
  , unk2      :: Word8 -- always 1
  , unk3      :: Word8 -- determines tempo (length of bar)
  -- (see details in binToMidi)
  , divisions :: B.ByteString
  } deriving (Show)

readSheetBin :: Get SheetBin
readSheetBin = do
  date <- getWord32be
  let readBars cur = do
        unk1 <- getWord16be
        unk2 <- getWord8
        unk3 <- getWord8
        case (unk1, unk2, unk3) of
          (0xFFFF, 0xFF, 0xFE) -> return $ reverse cur
          _                    -> do
            divisions <- getByteString 0x30
            readBars $ SheetBar{..} : cur
  bars <- readBars []
  return SheetBin{..}

binToMidi :: SheetBin -> (U.TempoMap, RTB.T U.Seconds Word8)
binToMidi bin = let
  {-
    think this algorithm is more or less right, seems to give correct results.
    each bar has a start time and a length, and is basically pasted into the
    gameplay track based on those. the end of one bar does not need to exactly
    line up with, or even be close to, the start of the next one (see
    Hungarian Dance slowdowns where there are significant gaps).
    for now we just stretch each bar out which can give odd (though
    time-correct) note positions. if we wanted, we could instead insert new
    bars or beats in the gaps.
  -}
  barResults = cutoffBars $ bin.bars <&> \bar -> let
    -- magic numbers found by guesswork, may need further adjustment
    barLength = fromIntegral bar.unk3 / 60 -- U.Seconds
    barStart = fromIntegral bar.unk1 / 98.44 + barLength
    divisionLength = barLength / 48
    in (barStart, RTB.fromPairList $ zip
      (barStart : repeat divisionLength)
      (B.unpack bar.divisions))
  -- this is required to fix cases where bars overlap (with empty space)
  -- like some Hungarian Dance parts. hopefully there are no actual overlaps
  -- with notes!
  cutoffBars = \case
    (start1, bar1) : rest@((start2, _) : _)
      -> (start1, U.trackTake start2 bar1) : cutoffBars rest
    bars -> bars
  inSeconds = foldr RTB.merge RTB.empty $ map snd barResults
  makeTempos = \case
    bar1 : next@(bar2 : _) -> let
      barLength = bar2 - bar1
      in Wait 0 (U.makeTempo 4 barLength) $ RTB.delay 4 $ makeTempos next
    _ -> RNil
  tempos = U.tempoMapFromBPS $ makeTempos $ 0 : map fst barResults
  in (tempos, inSeconds)

-- testPrint :: FilePath -> IO ()
-- testPrint f = BL.readFile f >>= runGetM readSheetBin >>= \bin -> mapM_ print bin.bars

-- testConvert :: FilePath -> IO ()
-- testConvert f = BL.readFile f >>= runGetM readSheetBin >>= \bin ->
--   F.saveMIDIUtf8 (f <> ".mid") let
--     (tempos, track) = binToMidi bin
--     in F.Song
--       { F.tempos = tempos
--       , F.timesigs = U.measureMapFromLengths U.Error $ RTB.singleton 0 4
--       , F.tracks = F.RawFile
--         [ fmap (E.MetaEvent . Meta.TextEvent . (`showHex` "")) $ RTB.filter (/= 0) track
--         ]
--       }

interpretDK1 :: (NNC.C t) => RTB.T t Word8 -> RTB.T t (DKEvent t)
interpretDK1 dk = let

  normalNotes = fmap DKNote $ flip RTB.mapMaybe dk \case
    -- not sure if the duplicates have anything different about them
    0x01 -> Just DKYellow
    0x02 -> Just DKYellow
    0x03 -> Just DKYellow
    0x04 -> Just DKRed
    0x05 -> Just DKRed
    0x06 -> Just DKRed
    0x0D -> Just DKClap
    0x0E -> Just DKClap
    0x0F -> Just DKClap
    0x10 -> Just DKPurple
    _    -> Nothing

  lanes = findLanes dk
  findLanes = \case
    Wait dt x rest -> case x of
      0x12 -> makeLane dt 0x12 DKYellow rest
      0x13 -> makeLane dt 0x13 DKRed    rest
      0x16 -> makeLane dt 0x16 DKClap   rest
      0x17 -> makeLane dt 0x17 DKPurple rest
      _    -> RTB.delay dt $ findLanes rest
    RNil -> RNil
  makeLane dt byte drum rest = case RTB.span (== byte) rest of
    (lane, after) -> let
      laneLength = mconcat $ RTB.getTimes lane
      in if laneLength == NNC.zero
        then RTB.delay dt $ findLanes rest
        else Wait dt (DKRoll drum laneLength) $ RTB.delay laneLength $ findLanes after

  in RTB.merge normalNotes lanes

-- Donkey Konga 2/3 chart format (MIDI)

readTrackDK2 :: (NNC.C t) => RTB.T t (E.T s) -> RTB.T t (DKEvent t)
readTrackDK2 mid = let
  notes = joinEdgesSimple $ flip RTB.mapMaybe mid \e ->
    isNoteEdge e <&> \(p, b) ->
      (if b then EdgeOn () else EdgeOff) p
  noPurple = flip RTB.mapMaybe notes \case
    ((), 39, len) -> Just $ noteOrRoll DKClap   len
    ((), 63, len) -> Just $ noteOrRoll DKYellow len
    ((), 64, len) -> Just $ noteOrRoll DKRed    len
    _             -> Nothing
  noteOrRoll drum len = if len == NNC.zero
    then DKNote drum
    else DKRoll drum len
  makePurple instant = case sort instant of
    [DKNote DKYellow     , DKNote DKRed     ] -> [DKNote DKPurple]
    -- real purple roll form seen
    [DKNote DKYellow     , DKRoll DKRed len ] -> [DKRoll DKPurple len]
    -- this isn't actually used I think
    [DKRoll DKYellow len1, DKRoll DKRed len2] -> [DKRoll DKPurple $ min len1 len2]
    xs                                        -> xs
  in RTB.flatten $ fmap makePurple $ RTB.collectCoincident noPurple

{-
DK2/DK3 midis have a sysex event (in the tempo track) giving chart/audio sync:
  F0 7D 44 4B 05 hi lo id id xx F7  (DK2 U/E, DK3)
  F0 7D 44 4B 04 hi lo id id F7     (DK2 J, tutorials)
where 44 4B is "DK". (hi << 7) | lo is the time in milliseconds in the audio
where the chart's second measure starts; the first measure is a lead-in of any
length. The id bytes vary per chart file, and the last byte of the 05 form
varies by difficulty. A couple of songs have slightly different times in
different difficulty charts, so read it from the chart actually being used.
-}
findDKAudioStart :: [RTB.T t (E.T s)] -> Maybe Int
findDKAudioStart tracks = listToMaybe do
  trk <- tracks
  E.SystemExclusive (SysEx.Regular (0x7D : 0x44 : 0x4B : _ : hi : lo : _)) <- RTB.getBodies trk
  return $ fromIntegral hi * 128 + fromIntegral lo

-- SongInfo.res found in DK2 (U/E but not J) and DK3 (J)
-- (in other games, metadata is hardcoded in the executable)

data SongInfo = SongInfo
  { info_index    :: Int
  , info_SONGNAME :: T.Text
  , info_FILENAME :: B.ByteString
  , info_OFFSET   :: Maybe Scientific
  , info_LENGTH   :: Maybe Int
  , info_PRICE    :: Maybe Int
  , info_1E       :: Maybe Int
  , info_1H       :: Maybe Int
  , info_1X       :: Maybe Int
  , info_2E       :: Maybe Int
  , info_2H       :: Maybe Int
  , info_2X       :: Maybe Int
  , info_4        :: Maybe Int
  , info_B        :: Maybe Int
  , info_C        :: Maybe Int
  , info_RUBY     :: Maybe T.Text
  , info_COMMENT  :: Maybe T.Text
  , info_GENRE    :: Maybe T.Text
  } deriving (Show)

data SongInfoLine
  = SongInfoStart Int
  | SongInfoKV T.Text T.Text

-- Takes a decoder: Shift-JIS for Japanese games, Latin-1 for U/E (e.g. "Pokémon")
readSongInfo :: (B.ByteString -> T.Text) -> B.ByteString -> [SongInfo]
readSongInfo decode bs = let
  lns = do
    ln <- T.lines $ T.filter (/= '\r') $ decode bs
    case T.stripPrefix "#" ln >>= readMaybe . T.unpack of
      Just n  -> return $ SongInfoStart n
      Nothing -> case T.breakOn "=" ln of
        (x, y) | "=" `T.isPrefixOf` y -> return $ SongInfoKV x $ T.drop 1 y
        _                             -> []
  breakStart = break \case SongInfoStart{} -> True; _ -> False
  splitSongs xs = case breakStart xs of
    (_, SongInfoStart n : rest) -> case breakStart rest of
      (ys, zs) -> makeSongInfo n ys : splitSongs zs
    _ -> []
  makeSongInfo n xs = let
    findInfo needle = listToMaybe [ v | SongInfoKV k v <- xs, k == needle ]
    findNum needle = findInfo needle >>= readMaybe . T.unpack
    in SongInfo
      { info_index    = n
      , info_SONGNAME = fromMaybe "" $ findInfo "SONGNAME"
      , info_FILENAME = maybe "" TE.encodeUtf8 $ findInfo "FILENAME"
      , info_OFFSET   = findNum "OFFSET"
      , info_LENGTH   = findNum "LENGTH"
      , info_PRICE    = findNum "PRICE"
      , info_1E       = findNum "1E"
      , info_1H       = findNum "1H"
      , info_1X       = findNum "1X"
      , info_2E       = findNum "2E"
      , info_2H       = findNum "2H"
      , info_2X       = findNum "2X"
      , info_4        = findNum "4"
      , info_B        = findNum "B"
      , info_C        = findNum "C"
      , info_RUBY     = findInfo "RUBY"
      , info_COMMENT  = findInfo "COMMENT"
      , info_GENRE    = findInfo "GENRE"
      }
  in splitSongs lns

-- Metadata tables in main.dol, for the games without SongInfo.res
-- (DK1 J/U/E and DK2 J). Tables are located by scanning for records whose
-- pointers lead to valid strings, confirmed against the chart files on disc,
-- rather than using fixed addresses.

data DOL = DOL
  { bytes    :: B.ByteString
  , sections :: [(Int, Word32, Word32)] -- (file offset, load address, size)
  }

readDOL :: B.ByteString -> DOL
readDOL bytes = DOL
  { bytes = bytes
  , sections = do
    i <- [0 .. 17] -- 7 text sections, then 11 data sections
    let field base = fromMaybe 0 $ word32At bytes $ base + i * 4
        size = field 0x90
    guard $ size /= 0
    return (fromIntegral $ field 0, field 0x48, size)
  }

word32At :: B.ByteString -> Int -> Maybe Word32
word32At bs i = if i < 0 || i + 4 > B.length bs
  then Nothing
  else Just $ foldl (\acc j -> shiftL acc 8 .|. fromIntegral (B.index bs $ i + j)) 0 [0 .. 3]

dolAddress :: DOL -> Word32 -> Maybe Int
dolAddress dol addr = listToMaybe
  [ off + fromIntegral (addr - start)
  | (off, start, size) <- dol.sections
  , start <= addr && addr < start + size
  ]

-- null-terminated string at a load address
dolString :: DOL -> Word32 -> Maybe B.ByteString
dolString dol addr = do
  i <- dolAddress dol addr
  let rest = B.drop i dol.bytes
      str = B.takeWhile (/= 0) rest
  guard $ B.length str < 0x100 && B.length str < B.length rest
  return str

-- word at a file offset, used as a pointer to a non-empty string
dolStringPtr :: DOL -> Int -> Maybe B.ByteString
dolStringPtr dol i = do
  str <- word32At dol.bytes i >>= dolString dol
  guard $ not $ B.null str
  return str

dolPositions :: DOL -> [Int]
dolPositions dol = do
  (off, _, size) <- dol.sections
  [off, off + 4 .. off + fromIntegral size - 4]

data DK1Song = DK1Song
  { title       :: B.ByteString -- Shift-JIS in Japanese version, Latin-1 otherwise
  , streamIndex :: Word32
  , chart       :: B.ByteString -- solo chart name, e.g. "01sing" for "01sing_h.bin"
  , stream      :: B.ByteString -- e.g. "score/01sing" for "stream/score/01sing.dsp"
  } deriving (Show)

{-
DK1 song table: records of 0x18 bytes
  0x00 title pointer
  0x04 index into the stream name table
  0x08 solo, duet, quartet, battle chart name pointers
The stream name table is an array of string pointers starting with
"main/select"; song streams ("score/...") come after the 9 menu streams.
The predicate should check that a solo chart name exists on disc.
-}
findDK1Songs :: (B.ByteString -> Bool) -> DOL -> [DK1Song]
findDK1Songs isChart dol = let
  readRecord i = do
    title <- dolStringPtr dol i
    streamIndex <- word32At dol.bytes $ i + 4
    guard $ streamIndex < 0x100
    chart <- dolStringPtr dol $ i + 8
    guard $ isChart chart
    forM_ [12, 16, 20] $ dolStringPtr dol . (i +)
    return (title, streamIndex, chart)
  records = mapMaybe readRecord $ dolPositions dol
  lookupStream table n = do
    stream <- dolStringPtr dol $ table + 4 * fromIntegral n
    guard $ "score/" `B.isPrefixOf` stream
    return stream
  validTables = do
    table <- dolPositions dol
    guard $ dolStringPtr dol table == Just "main/select"
    songs <- maybeToList $ forM records \(title, streamIndex, chart) -> do
      stream <- lookupStream table streamIndex
      return DK1Song{..}
    return songs
  in if null records then [] else fromMaybe [] $ listToMaybe validTables

data DK2JSong = DK2JSong
  { number  :: Int -- 1-based, matches "stream/score/NN.dsp"
  , chart   :: B.ByteString -- e.g. "01ainota" for "01ainota_1x.mid"
  , title   :: Maybe B.ByteString -- Shift-JIS
  , comment :: Maybe B.ByteString -- Shift-JIS, e.g. which anime a song is from
  , ratings :: [Word32] -- difficulty stars: 1E 1H 1X 2E 2H 2X 4 B C
  , genre   :: Maybe Word32 -- index into findDK2JGenres
  -- same as OFFSET in SongInfo.res. Close to (but not) the chart/audio sync,
  -- which comes from the midi instead (see findDKAudioStart)
  , offset  :: Maybe Float
  } deriving (Show)

{-
DK2 (J) has three tables:
- chart name pointers, in song number order (includes hidden songs + tutorial)
- song records of 0x18 bytes, in menu order:
    0x00 genre
    0x04 song index (0-based into chart names)
    0x08 title pointer
    0x0C reading (furigana) pointer or 0
    0x10 comment pointer or 0
    0x14 pointer to 9 u32 difficulty ratings
- float offsets (seconds), indexed by song index, directly before the records
The predicate should check that a chart name exists on disc.
-}
findDK2JSongs :: (B.ByteString -> Bool) -> DOL -> [DK2JSong]
findDK2JSongs isChart dol = let
  chartAt i = dolStringPtr dol i >>= \s -> s <$ guard (isChart s)
  chartArrays = do
    i <- dolPositions dol
    guard $ isJust (chartAt i) && isNothing (chartAt $ i - 4)
    return $ catMaybes $ takeWhile isJust $ map chartAt [i, i + 4 ..]
  charts = if null chartArrays then [] else maximumBy (comparing length) chartArrays
  optString i = word32At dol.bytes i >>= \case
    0   -> Just Nothing
    ptr -> Just <$> dolString dol ptr
  readRecord i = do
    genre <- word32At dol.bytes i
    index <- fromIntegral <$> word32At dol.bytes (i + 4)
    guard $ genre < 0x100 && index < length charts
    title <- dolStringPtr dol $ i + 8
    _ruby <- optString $ i + 12
    comment <- optString $ i + 16
    ratingsPos <- word32At dol.bytes (i + 20) >>= dolAddress dol
    ratings <- forM [0 .. 8] \k -> word32At dol.bytes $ ratingsPos + 4 * k
    guard $ all (<= 10) ratings
    return (i, index :: Int, (title, comment, ratings, genre))
  records = mapMaybe readRecord $ dolPositions dol
  offsets = case records of
    [] -> []
    _  -> let
      tableStart = minimum [ i | (i, _, _) <- records ]
      count = maximum [ index | (_, index, _) <- records ] + 1
      floats = forM [0 .. count - 1] \k ->
        castWord32ToFloat <$> word32At dol.bytes (tableStart - 4 * (count - k))
      in case floats of
        Just fs | all (\x -> 0 <= x && x < 10) fs -> fs
        _                                         -> []
  in zip [0 ..] charts <&> \(index, chart) -> let
    record = listToMaybe [ r | (_, i, r) <- records, i == index ]
    in DK2JSong
      { number  = index + 1
      , chart   = chart
      , title   = record <&> \(t, _, _, _) -> t
      , comment = record >>= \(_, c, _, _) -> c >>= \c' -> c' <$ guard (not $ B.null c')
      , ratings = maybe [] (\(_, _, r, _) -> r) record
      , genre   = record <&> \(_, _, _, g) -> g
      , offset  = listToMaybe $ drop index offsets
      }

-- DK2 (J) genre names: an array of string pointers indexed by the record genre
-- number, starting with "ゲーム" and "Ｊ－ＰＯＰ" (Shift-JIS)
findDK2JGenres :: DOL -> [B.ByteString]
findDK2JGenres dol = fromMaybe [] $ listToMaybe do
  i <- dolPositions dol
  guard $ dolStringPtr dol i == Just "\x83\x51\x81\x5B\x83\x80"
  guard $ dolStringPtr dol (i + 4) == Just "\x82\x69\x81\x7C\x82\x6F\x82\x6E\x82\x6F"
  return $ catMaybes $ takeWhile isJust $ map (dolStringPtr dol) [i, i + 4 ..]

-- "GENRE_NAME_nn=..." lines, found in SongData.res (DK3) and GenreName_xx.res (DK2 E)
readGenreNames :: (B.ByteString -> T.Text) -> B.ByteString -> [(Int, T.Text)]
readGenreNames decode bs = do
  ln <- T.lines $ T.filter (/= '\r') $ decode bs
  (k, v) <- maybeToList $ T.breakOn "=" <$> T.stripPrefix "GENRE_NAME_" ln
  n <- maybeToList $ readMaybe $ T.unpack k
  name <- maybeToList $ T.strip <$> T.stripPrefix "=" v
  guard $ not $ T.null name
  return (n, name)
