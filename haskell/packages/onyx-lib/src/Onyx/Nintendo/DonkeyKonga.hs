{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Nintendo.DonkeyKonga where

import           Data.Binary.Get
import qualified Data.ByteString                  as B
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (sort)
import           Data.Maybe
import           Data.Scientific                  (Scientific)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Word
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.MIDI.Common
import           Onyx.Util.ShiftJIS               (decodeShiftJIS)
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

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
  barResults = cutoffBars $ flip map bin.bars $ \bar -> let
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
--   F.saveMIDI (f <> ".mid") $ let
--     (tempos, track) = binToMidi bin
--     in F.Song
--       { F.s_tempos = tempos
--       , F.s_signatures = U.measureMapFromLengths U.Error $ RTB.singleton 0 4
--       , F.s_tracks = F.RawFile
--         [ fmap (E.MetaEvent . Meta.TextEvent . (`showHex` "")) $ RTB.filter (/= 0) track
--         ]
--       }

interpretDK1 :: (NNC.C t) => RTB.T t Word8 -> RTB.T t (DKEvent t)
interpretDK1 dk = let

  normalNotes = fmap DKNote $ flip RTB.mapMaybe dk $ \case
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

readTrackDK2 :: (NNC.C t) => RTB.T t E.T -> RTB.T t (DKEvent t)
readTrackDK2 mid = let
  notes = joinEdgesSimple $ flip RTB.mapMaybe mid $ \e ->
    flip fmap (isNoteEdge e) $ \(p, b) ->
      (if b then EdgeOn () else EdgeOff) p
  noPurple = flip RTB.mapMaybe notes $ \case
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

readSongInfo :: B.ByteString -> [SongInfo]
readSongInfo bs = let
  lns = do
    ln <- T.lines $ T.pack $ filter (/= '\r') $ decodeShiftJIS bs
    case T.stripPrefix "#" ln >>= readMaybe . T.unpack of
      Just n  -> return $ SongInfoStart n
      Nothing -> case T.breakOn "=" ln of
        (x, y) | "=" `T.isPrefixOf` y -> return $ SongInfoKV x $ T.drop 1 y
        _                             -> []
  breakStart = break $ \case SongInfoStart{} -> True; _ -> False
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
