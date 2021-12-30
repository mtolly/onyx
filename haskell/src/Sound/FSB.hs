{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Sound.FSB where

import           Control.Monad
import           Control.Monad.Trans.Resource   (MonadResource)
import           Control.Monad.Trans.StackTrace
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Conduit.Audio             as CA
import           Data.Hashable                  (Hashable, hash)
import qualified Data.Map as Map
import           Data.Int
import           Data.SimpleHandle
import           Data.Word
import           FFMPEG                         (ffSource)
import           GHC.Generics                   (Generic)
import           GHC.IO.Handle                  (HandlePosn (..))
import           STFS.Package                   (runGetM)
import           System.FilePath                ((-<.>), (</>), dropExtension)
import qualified System.IO                      as IO
import System.IO.Temp (withSystemTempDirectory)

data FSBSong = FSBSong
  { fsbSongHeaderSize  :: Word16
  , fsbSongName        :: B.ByteString
  , fsbSongSamples     :: Word32
  , fsbSongDataSize    :: Word32
  , fsbSongLoopStart   :: Word32
  , fsbSongLoopEnd     :: Word32
  , fsbSongMode        :: Word32
  , fsbSongSampleRate  :: Word32
  , fsbSongDefVol      :: Word16
  , fsbSongDefPan      :: Word16
  , fsbSongDefPri      :: Word16
  , fsbSongChannels    :: Word16
  -- rest is less certain, see http://wiki.xentax.com/index.php/FSB_FSB4
  , fsbSongMinDistance :: Word32 -- 00 00 80 3F (big endian?) in WT
  , fsbSongMaxDistance :: Word32 -- 00 40 1C 46 (big endian?) in WT
  , fsbExtra           :: Either FSBExtraXMA FSBExtraMP3
  } deriving (Show)

data FSBExtraXMA = FSBExtraXMA
  { fsbXMAUnknown1 :: Word32 -- in GH3 dlc82 and WT dlc3, 0. WoR dlc721, same as fsbSongHeaderSize?
  , fsbXMAUnknown2 :: Word32 -- in GH3 dlc82, WT dlc3, and WoR dlc721, 0
  , fsbXMAUnknown3 :: Word32 -- in GH3 dlc82, 20 00 00 00. WT dlc3 and WoR dlc721, 0
  , fsbXMAUnknown4 :: Word32 -- in GH3 dlc82, 71 49 69 03. WT dlc3 and WoR dlc721, 0
  , fsbXMAUnknown5 :: Word32 -- in GH3 dlc82, 33 00 00 00. WT dlc3 and WoR dlc721, 0
  , fsbXMAUnknown6 :: Word32 -- think this is size of rest of song struct (unk7, unk8, seek table)
  , fsbXMAUnknown7 :: Word32 -- think this is number of XMA streams (channels / 2, rounded up)
  , fsbXMAUnknown8 :: Word32 -- think this is number of seek entries + 1?
  , fsbXMASeek     :: [Word32] -- LE (they are BE in .xma files)
  } deriving (Show)

data FSBExtraMP3 = FSBExtraMP3
  -- no idea what these are
  { fsbMP3Unknown1 :: Word32 -- 50 00 00 00
  , fsbMP3Unknown2 :: Word32 -- 00 00 00 00
  } deriving (Show)

getFSBSong :: Get FSBSong
getFSBSong = do
  pos1              <- bytesRead
  fsbSongHeaderSize <- getWord16le
  fsbSongName       <- B.takeWhile (/= 0) <$> getByteString 30
  fsbSongSamples    <- getWord32le
  fsbSongDataSize   <- getWord32le
  fsbSongLoopStart  <- getWord32le
  fsbSongLoopEnd    <- getWord32le
  fsbSongMode       <- getWord32le
  fsbSongSampleRate <- getWord32le
  fsbSongDefVol     <- getWord16le
  fsbSongDefPan     <- getWord16le
  fsbSongDefPri     <- getWord16le
  fsbSongChannels   <- getWord16le

  fsbSongMinDistance <- getWord32be
  fsbSongMaxDistance <- getWord32be

  -- fsbext fsb.h says: #define FSOUND_XMA 0x01000000
  fsbExtra <- if fsbSongMode .&. 0x01000000 /= 0
    then Left <$> do

      fsbXMAUnknown1 <- getWord32le
      fsbXMAUnknown2 <- getWord32le
      fsbXMAUnknown3 <- getWord32le
      fsbXMAUnknown4 <- getWord32le
      fsbXMAUnknown5 <- getWord32le
      fsbXMAUnknown6 <- getWord32le
      fsbXMAUnknown7 <- getWord32le
      fsbXMAUnknown8 <- getWord32le

      pos2 <- bytesRead

      let bytesLeft = fromIntegral fsbSongHeaderSize - fromIntegral (pos2 - pos1)
      numEntries <- case quotRem bytesLeft 4 of
        (q, 0) -> return q -- sometimes this is fsbXMAUnknown8, sometimes fsbXMAUnknown8 - 1?
        _      -> fail $ unwords
          [ "Incorrect size left for XMA seek table;"
          , show bytesLeft
          , "not divisible by 4. Header value claims"
          , show fsbXMAUnknown8
          , "entries"
          ]
      fsbXMASeek <- replicateM numEntries getWord32le

      return FSBExtraXMA{..}

    else Right <$> do
      fsbMP3Unknown1 <- getWord32le
      fsbMP3Unknown2 <- getWord32le
      return FSBExtraMP3{..}

  return FSBSong{..}

putFSBSong :: FSBSong -> Put
putFSBSong FSBSong{..} = do
  putWord16le fsbSongHeaderSize
  putByteString $ B.take 30 $ fsbSongName <> B.replicate 30 0
  putWord32le fsbSongSamples
  putWord32le fsbSongDataSize
  putWord32le fsbSongLoopStart
  putWord32le fsbSongLoopEnd
  putWord32le fsbSongMode
  putWord32le fsbSongSampleRate
  putWord16le fsbSongDefVol
  putWord16le fsbSongDefPan
  putWord16le fsbSongDefPri
  putWord16le fsbSongChannels

  putWord32be fsbSongMinDistance
  putWord32be fsbSongMaxDistance

  case fsbExtra of
    Left FSBExtraXMA{..} -> do
      putWord32le fsbXMAUnknown1
      putWord32le fsbXMAUnknown2
      putWord32le fsbXMAUnknown3
      putWord32le fsbXMAUnknown4
      putWord32le fsbXMAUnknown5
      putWord32le fsbXMAUnknown6
      putWord32le fsbXMAUnknown7
      putWord32le fsbXMAUnknown8
      mapM_ putWord32le fsbXMASeek
    Right FSBExtraMP3{..} -> do
      putWord32le fsbMP3Unknown1
      putWord32le fsbMP3Unknown2

data FSB3Header = FSB3Header
  { fsb3SongCount   :: Word32
  , fsb3HeadersSize :: Word32
  , fsb3DataSize    :: Word32
  , fsb3Version     :: Word32 -- 0x30000 (3.0) or 0x30001 (3.1)
  , fsb3Flags       :: Word32
  , fsb3Songs       :: [FSBSong]
  } deriving (Show)

getFSB3Header :: Get FSB3Header
getFSB3Header = do
  magic           <- getByteString 4
  guard $ magic == "FSB3"
  fsb3SongCount   <- getWord32le
  fsb3HeadersSize <- getWord32le
  fsb3DataSize    <- getWord32le
  fsb3Version     <- getWord32le
  fsb3Flags       <- getWord32le
  fsb3Songs       <- replicateM (fromIntegral fsb3SongCount) getFSBSong
  return FSB3Header{..}

putFSB3Header :: FSB3Header -> Put
putFSB3Header FSB3Header{..} = do
  putByteString "FSB3"
  putWord32le fsb3SongCount
  putWord32le fsb3HeadersSize
  putWord32le fsb3DataSize
  putWord32le fsb3Version
  putWord32le fsb3Flags
  mapM_ putFSBSong fsb3Songs

data FSB4Header = FSB4Header
  { fsb4SongCount   :: Word32
  , fsb4HeadersSize :: Word32
  , fsb4DataSize    :: Word32
  , fsb4Version     :: Word32 -- 0x40000 (4.0)
  , fsb4Flags       :: Word32
  , fsb4Hash        :: B.ByteString -- 8 bytes
  , fsb4GUID        :: B.ByteString -- 16 bytes
  , fsb4Songs       :: [FSBSong]
  } deriving (Show)

getFSB4Header :: Get FSB4Header
getFSB4Header = do
  magic           <- getByteString 4
  guard $ magic == "FSB4"
  fsb4SongCount   <- getWord32le
  fsb4HeadersSize <- getWord32le
  fsb4DataSize    <- getWord32le
  fsb4Version     <- getWord32le
  fsb4Flags       <- getWord32le
  fsb4Hash        <- getByteString 8
  fsb4GUID        <- getByteString 16
  fsb4Songs       <- replicateM (fromIntegral fsb4SongCount) getFSBSong
  return FSB4Header{..}

putFSB4Header :: FSB4Header -> Put
putFSB4Header FSB4Header{..} = do
  putByteString "FSB4"
  putWord32le fsb4SongCount
  putWord32le fsb4HeadersSize
  putWord32le fsb4DataSize
  putWord32le fsb4Version
  putWord32le fsb4Flags
  putByteString fsb4Hash
  putByteString fsb4GUID
  mapM_ putFSBSong fsb4Songs

songHeaderSize :: FSBSong -> Word16
songHeaderSize song = 64 + 8 + case fsbExtra song of
  Left  xma  -> 32 + fromIntegral (length $ fsbXMASeek xma) * 4
  Right _mp3 -> 8

fixFSB3 :: FSB3Header -> FSB3Header
fixFSB3 FSB3Header{..} = let
  songs = flip map fsb3Songs $ \song -> song
    { fsbSongHeaderSize = songHeaderSize song
    }
  in FSB3Header
    { fsb3SongCount   = fromIntegral $ length fsb3Songs
    , fsb3HeadersSize = sum $ map (fromIntegral . fsbSongHeaderSize) songs
    , fsb3DataSize    = sum $ map fsbSongDataSize songs
    , fsb3Version     = fsb3Version
    , fsb3Flags       = fsb3Flags
    , fsb3Songs       = songs
    }

data FSB = FSB
  { fsbHeader   :: Either FSB3Header FSB4Header
  , fsbSongData :: [BL.ByteString]
  } deriving (Show)

parseFSB :: (MonadFail m) => BL.ByteString -> m FSB
parseFSB = runGetM $ do
  magic <- lookAhead $ getByteString 4
  fsbHeader <- case magic of
    "FSB3" -> Left  <$> getFSB3Header
    "FSB4" -> Right <$> getFSB4Header
    _      -> fail $ "Unrecognized FSB magic: " <> show magic
  -- see GHWT aDLC3_1.fsb; this should skip zeroes to get to 0x10-alignment
  let dataStart = case fsbHeader of
        Left  fsb3 -> 0x18 + fsb3HeadersSize fsb3
        Right fsb4 -> 0x30 + fsb4HeadersSize fsb4
  bytesRead >>= \n -> skip $ fromIntegral dataStart - fromIntegral n
  fsbSongData <- forM (either fsb3Songs fsb4Songs fsbHeader) $ \song -> do
    getLazyByteString $ fromIntegral $ fsbSongDataSize song
  return FSB{..}

fixFSB :: FSB -> FSB
fixFSB fsb = let
  fixSongs songs = flip map (zip songs $ fsbSongData fsb) $ \(song, sdata) -> song
    { fsbSongHeaderSize = songHeaderSize song
    , fsbSongDataSize   = fromIntegral $ BL.length sdata
    }
  header = case fsbHeader fsb of
    Left header3 -> let
      songs = fixSongs $ fsb3Songs header3
      in Left header3
        { fsb3SongCount   = fromIntegral $ length songs
        -- TODO pad to 0x10
        , fsb3HeadersSize = sum $ map (fromIntegral . fsbSongHeaderSize) songs
        , fsb3DataSize    = sum $ map fsbSongDataSize songs
        , fsb3Songs       = songs
        }
    Right header4 -> let
      songs = fixSongs $ fsb4Songs header4
      in Right header4
        { fsb4SongCount   = fromIntegral $ length songs
        -- TODO pad to 0x10
        , fsb4HeadersSize = sum $ map (fromIntegral . fsbSongHeaderSize) songs
        , fsb4DataSize    = sum $ map fsbSongDataSize songs
        , fsb4Songs       = songs
        }
  in fsb { fsbHeader = header }

emitFSB :: FSB -> BL.ByteString
emitFSB fsb = runPut $ do
  let fsb' = fixFSB fsb
  either putFSB3Header putFSB4Header $ fsbHeader fsb'
  -- TODO pad to 0x10 to match fsbXHeadersSize
  mapM_ putLazyByteString $ fsbSongData fsb'

readGH3FSB3 :: (MonadResource m) => BL.ByteString -> IO [(B.ByteString, CA.AudioSource m Int16)]
readGH3FSB3 bs = do
  fsb <- parseFSB bs
  let songs = either fsb3Songs fsb4Songs $ fsbHeader fsb
  forM (zip songs $ fsbSongData fsb) $ \(song, sdata) -> do
    let bsnew = emitFSB FSB
          { fsbHeader = Right FSB4Header
            { fsb4SongCount   = 1
            , fsb4HeadersSize = 0
            , fsb4DataSize    = 0
            , fsb4Version     = 0x40000
            , fsb4Flags       = 0
            , fsb4Hash        = B.replicate 8 0
            , fsb4GUID        = B.replicate 16 0
            , fsb4Songs       = [song]
            }
          , fsbSongData = [sdata]
          }
        readable = makeHandle "decoded FSB audio" $ byteStringSimpleHandle bsnew
    src <- ffSource $ Left readable
    return (fsbSongName song, src)

data XMA2Packet = XMA2Packet
  { xma2FrameCount        :: Word32
  , xma2FrameOffsetInBits :: Word32
  , xma2PacketMetadata    :: Word32
  , xma2PacketSkipCount   :: Word32
  , xma2Data              :: B.ByteString
  }

splitXMA2Packets :: (MonadFail m) => BL.ByteString -> m [XMA2Packet]
splitXMA2Packets = runGetM go where
  go = isEmpty >>= \case
    True  -> return []
    False -> do
      packetInfo <- getWord32be
      xma2Data <- getByteString $ 2048 - 4
      let xma2FrameCount        = (packetInfo `shiftR` 26) .&. 0x3F
          xma2FrameOffsetInBits = (packetInfo `shiftR` 11) .&. 0x7FFF
          xma2PacketMetadata    = (packetInfo `shiftR` 8)  .&. 0x7
          xma2PacketSkipCount   = packetInfo               .&. 0xFF
      (XMA2Packet{..} :) <$> go

writeXMA2Packets :: [XMA2Packet] -> BL.ByteString
writeXMA2Packets pkts = runPut $ forM_ pkts $ \pkt -> do
  let packetInfo
        =   (xma2FrameCount        pkt `shiftL` 26)
        .|. (xma2FrameOffsetInBits pkt `shiftL` 11)
        .|. (xma2PacketMetadata    pkt `shiftL` 8 )
        .|. (xma2PacketSkipCount   pkt            )
  putWord32be packetInfo
  putByteString $ xma2Data pkt

fsb4sToGH3FSB3 :: (Monad m) => [(B.ByteString, BL.ByteString)] -> StackTraceT m BL.ByteString
fsb4sToGH3FSB3 inputs = do
  parsed <- forM inputs $ \(name, fsb4) -> flip runGetM fsb4 $ do
    header <- getFSB4Header
    song <- case fsb4Songs header of
      [song] -> return song
        { fsbSongName = name
        , fsbSongLoopEnd = fsbSongSamples song - 1 -- this is set correctly by makefsb4, but just -1 in GHWT files
        , fsbSongMode = 0x1100040
        , fsbSongDefVol = 255
        , fsbSongDefPan = 128
        , fsbSongDefPri = 255
        }
      xs     -> fail $ show (length xs) <> " subfiles found in input FSB4"
    songData <- getRemainingLazyByteString
    return (song, BL.take (fromIntegral $ fsbSongDataSize song) songData)
  return $ runPut $ do
    putFSB3Header $ fixFSB3 FSB3Header
      { fsb3SongCount = 0
      , fsb3HeadersSize = 0
      , fsb3DataSize = 0
      , fsb3Version = 0x30001
      , fsb3Flags = 0
      , fsb3Songs = map fst parsed
      }
    mapM_ putLazyByteString $ map snd parsed

fsbToXMAs :: FilePath -> FilePath -> IO ()
fsbToXMAs f dir = do
  fsb <- BL.readFile f >>= parseFSB
  forM_ (zip (either fsb3Songs fsb4Songs $ fsbHeader fsb) (fsbSongData fsb)) $ \(song, sdata) -> do
    let (stereoCount, monoCount) = quotRem (fromIntegral $ fsbSongChannels song) 2
    case fsbExtra song of
      Left _xma -> do
        writeXMA (dir </> B8.unpack (fsbSongName song) -<.> "xma") XMAContents
          { xmaChannels = fromIntegral $ fsbSongChannels song
          , xmaRate     = fromIntegral $ fsbSongSampleRate song
          , xmaSamples  = fromIntegral $ fsbSongSamples song
          , xmaData     = sdata
          }
        marked <- markXMAPacketStreams <$> splitXMA2Packets sdata
        forM_ [0 .. stereoCount + monoCount - 1] $ \i -> do
          writeXMA (dir </> dropExtension (B8.unpack $ fsbSongName song) <> "_" <> show i <> ".xma") XMAContents
            { xmaChannels = if i == stereoCount then 1 else 2
            , xmaRate     = fromIntegral $ fsbSongSampleRate song
            , xmaSamples  = fromIntegral $ fsbSongSamples song
            , xmaData     = writeXMA2Packets $ extractXMAStream i marked
            }
      Right _mp3 -> do
        BL.writeFile (dir </> B8.unpack (fsbSongName song) -<.> "mp3") sdata
        let mp3s = splitInterleavedMP3 (fromIntegral $ stereoCount + monoCount) sdata
        forM_ (zip [0..] mp3s) $ \(i, mp3) -> do
          BL.writeFile (dir </> dropExtension (B8.unpack $ fsbSongName song) <> "_" <> show (i :: Int) <> ".mp3") mp3

-- Splits a GH-style FSB into stereo XMA (360) or MP3 (PS3).
splitMultitrackFSB :: BL.ByteString -> IO [BL.ByteString]
splitMultitrackFSB bs = do
  fsb <- parseFSB bs
  (song, sdata) <- case zip (either fsb3Songs fsb4Songs $ fsbHeader fsb) (fsbSongData fsb) of
    [pair] -> return pair
    _      -> fail "Not exactly 1 item found in .fsb"
  let (stereoCount, monoCount) = quotRem (fromIntegral $ fsbSongChannels song) 2
  case fsbExtra song of
    Left _xma -> withSystemTempDirectory "onyx-split-fsb" $ \tmp -> do
      let tmpFile = tmp </> "out.xma"
      marked <- markXMAPacketStreams <$> splitXMA2Packets sdata
      forM [0 .. stereoCount + monoCount - 1] $ \i -> do
        writeXMA tmpFile XMAContents
          { xmaChannels = if i == stereoCount then 1 else 2
          , xmaRate     = fromIntegral $ fsbSongSampleRate song
          , xmaSamples  = fromIntegral $ fsbSongSamples song
          , xmaData     = writeXMA2Packets $ extractXMAStream i marked
          }
        BL.fromStrict <$> B.readFile tmpFile
    Right _mp3 -> return $ splitInterleavedMP3 (fromIntegral $ stereoCount + monoCount) sdata

splitInterleavedMP3 :: Int64 -> BL.ByteString -> [BL.ByteString]
splitInterleavedMP3 1 bs = [bs]
splitInterleavedMP3 n bs = let
  frameSize = 384 -- TODO actually figure this out, might vary.
  -- see http://www.datavoyage.com/mpgscript/mpeghdr.htm "How to calculate frame length"
  getPart i = BL.concat $ takeWhile (not . BL.null) $ do
    j <- [i * frameSize, (i + n) * frameSize ..]
    return $ BL.take frameSize $ BL.drop j bs
  in map getPart [0 .. n - 1]

markXMAPacketStreams :: [XMA2Packet] -> [(Int, XMA2Packet)]
markXMAPacketStreams = go 0 Map.empty where
  go nextNewStream skips (pkt : pkts) = case Map.toList $ Map.filter (== 0) skips of
    (i, _) : _ -> let
      skips' = Map.insert i (xma2PacketSkipCount pkt) $ Map.map (subtract 1) skips
      in (i, pkt) : go nextNewStream skips' pkts
    [] -> let
      skips' = Map.insert nextNewStream (xma2PacketSkipCount pkt) $ Map.map (subtract 1) skips
      in (nextNewStream, pkt) : go (nextNewStream + 1) skips' pkts
  go _ _ [] = []

-- Regroups packets to fit more of them into the 16-packet blocks.
extractXMAStream :: Int -> [(Int, XMA2Packet)] -> [XMA2Packet]
extractXMAStream i = go . splitBlocks where
  dummyBlock = XMA2Packet
    { xma2FrameCount = 0
    , xma2FrameOffsetInBits = 0
    , xma2PacketMetadata = 0
    , xma2PacketSkipCount = 0
    , xma2Data = B.replicate 2044 0
    }
  complete :: Int -> [XMA2Packet] -> [XMA2Packet]
  complete n [] = replicate n dummyBlock
  complete n (pkt : pkts) = let
    pkt' = pkt
      { xma2PacketSkipCount = case pkts of
        []    -> fromIntegral n - 1
        _ : _ -> 0
      }
    in pkt' : complete (n - 1) pkts
  splitBlocks :: [(Int, XMA2Packet)] -> [[XMA2Packet]]
  splitBlocks pkts = case splitAt 16 pkts of
    ([] , _   ) -> []
    (blk, rest) -> map snd (filter ((== i) . fst) blk) : splitBlocks rest
  go :: [[XMA2Packet]] -> [XMA2Packet]
  go [] = []
  go (blk : blks) = case blks of
    []          -> complete 16 blk
    next : rest -> if length blk + length next <= 16
      then go $ (blk <> next) : rest
      else complete 16 blk <> go blks

-- This assumes the block size is 32 KB (16 packets) which appears to be what FSB uses
makeXMASeekTable :: (MonadFail m) => BL.ByteString -> m [Word32]
makeXMASeekTable bs = do
  pkts <- splitXMA2Packets bs
  let stream0Counts = map
        (\(stream, pkt) -> if stream == 0 then xma2FrameCount pkt else 0)
        (markXMAPacketStreams pkts)
  return $ do
    -- Packets are 2048 bytes in all XMA. Each packet is 512 samples.
    -- FSB appear to use 16-packet blocks. (Maybe this is stored somewhere?)
    -- Each entry in the seek table is "how many samples would you have decoded by block N"
    packetIndex <- [0, 16 .. length stream0Counts]
    return $ sum (take packetIndex stream0Counts) * 512

data XMAContents = XMAContents
  { xmaChannels :: Int
  , xmaRate     :: Int
  , xmaSamples  :: Int
  , xmaData     :: BL.ByteString
  } deriving (Generic, Hashable)

parseXMA :: (MonadFail m) => BL.ByteString -> m XMAContents
parseXMA b = let
  findChunk tag bytes = if BL.null bytes
    then fail $ "Couldn't find RIFF chunk: " <> show tag
    else let
      thisTag = BL.take 4 bytes
      len = fmap fromIntegral $ runGetM getWord32le $ BL.drop 4 bytes
      in if tag == thisTag
        then len >>= \l -> return $ BL.take l $ BL.drop 8 bytes
        else len >>= \l -> findChunk tag $ BL.drop (8 + l) bytes
  in do
    riff <- BL.drop 4 <$> findChunk "RIFF" b
    fmt <- findChunk "fmt " riff
    -- ignoring "seek", we'll make our own seek table
    data_ <- findChunk "data" riff
    flip runGetM fmt $ do
      -- WAVEFORMATEX
      0x166    <- getWord16le -- wFormatTag;      // Audio format type; always WAVE_FORMAT_XMA2
      channels <- getWord16le -- nChannels;       // Channel count of the decoded audio
      rate     <- getWord32le -- nSamplesPerSec;  // Sample rate of the decoded audio
      _        <- getWord32le -- nAvgBytesPerSec; // Used internally by the XMA encoder
      _        <- getWord16le -- nBlockAlign;     // Decoded sample size; channels * wBitsPerSample / 8
      0x10     <- getWord16le -- wBitsPerSample;  // Bits per decoded mono sample; always 16 for XMA
      0x22     <- getWord16le -- cbSize;          // Size in bytes of the rest of this structure (34)
      -- rest
      _        <- getWord16le -- NumStreams;     // Number of audio streams (1 or 2 channels each)
      _        <- getWord32le -- ChannelMask;    // Spatial positions of the channels in this file
      len      <- getWord32le -- SamplesEncoded; // Total number of PCM samples the file decodes to
      _        <- getWord32le -- BytesPerBlock;  // XMA block size (but the last one may be shorter)
      _        <- getWord32le -- PlayBegin;      // First valid sample in the decoded audio
      _        <- getWord32le -- PlayLength;     // Length of the valid part of the decoded audio
      _        <- getWord32le -- LoopBegin;      // Beginning of the loop region in decoded sample terms
      _        <- getWord32le -- LoopLength;     // Length of the loop region in decoded sample terms
      _        <- getWord8    -- LoopCount;      // Number of loop repetitions; 255 = infinite
      _        <- getWord8    -- EncoderVersion; // Version of XMA encoder that generated the file
      _        <- getWord16le -- BlockCount;     // XMA blocks in file (and entries in its seek table)
      return XMAContents
        { xmaChannels = fromIntegral channels
        , xmaRate     = fromIntegral rate
        , xmaSamples  = fromIntegral len
        , xmaData     = data_
        }

writeXMA :: FilePath -> XMAContents -> IO ()
writeXMA fp (XMAContents c r len xmaData) = IO.withBinaryFile fp IO.WriteMode $ \h -> do
  let chunk ctype f = do
        let getPosn = IO.hGetPosn h
        B.hPut h ctype
        lenPosn <- getPosn
        B.hPut h $ B.pack [0xDE, 0xAD, 0xBE, 0xEF] -- filled in later
        HandlePosn _ start <- getPosn
        x <- f
        endPosn@(HandlePosn _ end) <- getPosn
        IO.hSetPosn lenPosn
        write $ putWord32le $ fromIntegral $ end - start
        IO.hSetPosn endPosn
        return x
      write = BL.hPut h . runPut
  xmaSeek <- makeXMASeekTable xmaData
  chunk "RIFF" $ do
    B.hPut h "WAVE"
    chunk "fmt " $ do

      -- WAVEFORMATEX wfx;
      write $ putWord16le 0x166                -- wFormatTag;      // Audio format type; always WAVE_FORMAT_XMA2
      write $ putWord16le $ fromIntegral c     -- nChannels;       // Channel count of the decoded audio
      write $ putWord32le $ fromIntegral r     -- nSamplesPerSec;  // Sample rate of the decoded audio
      write $ putWord32le 0 -- TODO            -- nAvgBytesPerSec; // Used internally by the XMA encoder
      write $ putWord16le $ fromIntegral c * 2 -- nBlockAlign;     // Decoded sample size; channels * wBitsPerSample / 8
      write $ putWord16le 0x10                 -- wBitsPerSample;  // Bits per decoded mono sample; always 16 for XMA
      write $ putWord16le 0x22                 -- cbSize;          // Size in bytes of the rest of this structure (34)

      write $ putWord16le $ (fromIntegral c + 1) `quot` 2 -- NumStreams;     // Number of audio streams (1 or 2 channels each)
      write $ putWord32le 0                               -- ChannelMask;    // Spatial positions of the channels in this file
      write $ putWord32le $ fromIntegral len              -- SamplesEncoded; // Total number of PCM samples the file decodes to
      write $ putWord32le 0x8000                          -- BytesPerBlock;  // XMA block size (but the last one may be shorter)
      -- think this is correct, 0x8000 is 16 * 2048 (FSB uses 16-packet blocks)
      write $ putWord32le 0                               -- PlayBegin;      // First valid sample in the decoded audio
      write $ putWord32le $ fromIntegral len              -- PlayLength;     // Length of the valid part of the decoded audio
      write $ putWord32le 0                               -- LoopBegin;      // Beginning of the loop region in decoded sample terms
      write $ putWord32le 0                               -- LoopLength;     // Length of the loop region in decoded sample terms
      write $ putWord8 0                                  -- LoopCount;      // Number of loop repetitions; 255 = infinite
      write $ putWord8 4                                  -- EncoderVersion; // Version of XMA encoder that generated the file
      write $ putWord16le $ fromIntegral $ length xmaSeek -- BlockCount;     // XMA blocks in file (and entries in its seek table)
      -- TODO should that be "length xmaSeek - 1"? We might be missing a final entry in our table

    chunk "seek" $ do
      -- FSB's version of the seek table starts from block 0, and is little-endian.
      -- But here it starts from block 1, and is big-endian
      forM_ (drop 1 xmaSeek) $ write . putWord32be
    chunk "data" $ do
      BL.hPut h xmaData

xmasToFSB :: (MonadFail m) => [(B.ByteString, XMAContents)] -> m FSB
xmasToFSB xmas = do

  songs <- forM xmas $ \(name, xma) -> do
    -- FSB should use 32 KB blocks (16-packet)
    seekTable <- makeXMASeekTable $ xmaData xma
    let lenSeekTable = fromIntegral $ length seekTable
        song = FSBSong
          { fsbSongHeaderSize = 0
          , fsbSongName       = name -- does not appear to matter. for gh3 even, looks like only position matters
          , fsbSongSamples    = fromIntegral $ xmaSamples xma
          , fsbSongDataSize   = 0
          , fsbSongLoopStart  = 0
          , fsbSongLoopEnd    = maxBound -- should be -1
          , fsbSongMode       = 0x5000000
          , fsbSongSampleRate = fromIntegral $ xmaRate xma
          , fsbSongDefVol     = 255
          , fsbSongDefPan     = 128
          , fsbSongDefPri     = 128
          , fsbSongChannels   = fromIntegral $ xmaChannels xma

          , fsbSongMinDistance = 0x803F
          , fsbSongMaxDistance = 0x401C46

          , fsbExtra = Left FSBExtraXMA
            { fsbXMAUnknown1 = 0
            , fsbXMAUnknown2 = 0
            , fsbXMAUnknown3 = 0
            , fsbXMAUnknown4 = 0
            , fsbXMAUnknown5 = 0
            , fsbXMAUnknown6 = 8 + 4 * lenSeekTable
            , fsbXMAUnknown7 = fromIntegral (xmaChannels xma + 1) `quot` 2
            , fsbXMAUnknown8 = lenSeekTable + 1
            , fsbXMASeek = seekTable
            }

          }
    return (song, xmaData xma)

  return $ fixFSB $ FSB
    { fsbHeader = Right $ FSB4Header
      { fsb4SongCount   = fromIntegral $ length songs
      , fsb4HeadersSize = 0
      , fsb4DataSize    = 0
      , fsb4Version     = 0x40000
      , fsb4Flags       = 32
      , fsb4Hash        = B.replicate 8 0
      , fsb4GUID        = B.take 16 $ "onyx" <> B8.pack (show $ hash xmas) <> "................"
      , fsb4Songs       = map fst songs
      }
    , fsbSongData = map snd songs
    }

-- Files for GHWT/5/WoR
ghBandFSB :: (MonadFail m) => XMAContents -> m FSB
ghBandFSB xma = xmasToFSB [("multichannel sound", xma)]

toGH3FSB :: FSB -> FSB
toGH3FSB fsb = case fsbHeader fsb of
  Left _ -> fsb
  Right fsb4 -> fsb
    { fsbHeader = Left FSB3Header
      { fsb3SongCount   = fsb4SongCount   fsb4
      , fsb3HeadersSize = fsb4HeadersSize fsb4
      , fsb3DataSize    = fsb4DataSize    fsb4
      , fsb3Version     = fsb4Version     fsb4
      , fsb3Flags       = fsb4Flags       fsb4
      , fsb3Songs       = fsb4Songs       fsb4
      }
    }

splitXMA2Frames :: B.ByteString -> [[Bool]]
splitXMA2Frames = go . concatMap toBits . B.unpack where
  toBits byte = map (byte `testBit`) [7, 6 .. 0]
  go [] = [] -- does this happen?
  go stream = let
    len = foldr (.|.) 0 $ do
      (i, b) <- zip [14, 13 .. 0] $ take 15 stream
      return $ if b then bit i else 0
    in case len of
      0x7FFF -> []
      _      -> case splitAt len stream of
        (x, y) -> x : go y
