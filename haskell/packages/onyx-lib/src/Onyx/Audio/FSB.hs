{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Audio.FSB where

import           Control.Concurrent.Async     (forConcurrently)
import           Control.Exception            (evaluate)
import           Control.Monad
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.ST
import           Control.Monad.Trans.Resource (MonadResource, ResourceT)
import           Data.Array.ST
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit.Audio           (Duration (..), secondsToFrames)
import qualified Data.Conduit.Audio           as CA
import           Data.Functor.Identity        (Identity (..), runIdentity)
import           Data.Hashable                (Hashable, hash)
import           Data.Int
import           Data.List.Extra              (transpose)
import           Data.List.Split              (chunksOf)
import qualified Data.Map                     as Map
import qualified Data.Text                    as T
import           Data.Word
import           GHC.Generics                 (Generic)
import           GHC.IO.Handle                (HandlePosn (..))
import           Numeric                      (showHex)
import           Onyx.FFMPEG                  (ffSource)
import           Onyx.Util.Binary             (runGetM)
import           Onyx.Util.Handle
import           System.FilePath              (dropExtension, (<.>), (</>))
import qualified System.IO                    as IO
import           System.IO.Temp               (withSystemTempDirectory)

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

  -- TODO figure out gh3 wii, some different codec probably.
  -- mode is 0x20400040 but fsb.h defines don't make sense...?

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

data XMA1Packet = XMA1Packet
  { xma1SequenceNumber    :: Word32
  , xma1Unk               :: Word32
  , xma1FrameOffsetInBits :: Word32
  , xma1PacketSkipCount   :: Word32
  , xma1PacketData        :: B.ByteString
  } deriving (Show)

splitXMA1Packets :: (MonadFail m) => BL.ByteString -> m [XMA1Packet]
splitXMA1Packets = runGetM go where
  go = isEmpty >>= \case
    True  -> return []
    False -> do
      packetInfo <- getWord32be
      xma1PacketData <- getByteString $ 2048 - 4
      let xma1SequenceNumber    = (packetInfo `shiftR` 28) .&. 0xF
          xma1Unk               = (packetInfo `shiftR` 26) .&. 0x3
          xma1FrameOffsetInBits = (packetInfo `shiftR` 11) .&. 0x7FFF
          xma1PacketSkipCount   = packetInfo               .&. 0x7FF
      (XMA1Packet{..} :) <$> go

writeXMA1Packets :: [XMA1Packet] -> BL.ByteString
writeXMA1Packets pkts = runPut $ forM_ pkts $ \pkt -> do
  let packetInfo
        =   (xma1SequenceNumber    pkt `shiftL` 28)
        .|. (xma1Unk               pkt `shiftL` 26)
        .|. (xma1FrameOffsetInBits pkt `shiftL` 11)
        .|. (xma1PacketSkipCount   pkt            )
  putWord32be packetInfo
  putByteString $ xma1PacketData pkt

data XMA2Packet = XMA2Packet
  { xma2FrameCount        :: Word32
  , xma2FrameOffsetInBits :: Word32
  , xma2PacketMetadata    :: Word32
  , xma2PacketSkipCount   :: Word32
  , xma2PacketData        :: B.ByteString
  } deriving (Show)

splitXMA2Packets :: (MonadFail m) => BL.ByteString -> m [XMA2Packet]
splitXMA2Packets = runGetM go where
  go = isEmpty >>= \case
    True  -> return []
    False -> do
      packetInfo <- getWord32be
      xma2PacketData <- getByteString $ 2048 - 4
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
  putByteString $ xma2PacketData pkt

splitFSBStreamsToDir :: FilePath -> FilePath -> IO ()
splitFSBStreamsToDir f dir = do
  fsb <- BL.readFile f >>= parseFSB
  streams <- splitFSBStreams fsb
  streamNames <- case fsbHeader fsb of
    Left  fsb3 -> return $ do
      (i, name) <- zip [0..] $ map fsbSongName $ fsb3Songs fsb3
      return $ show (i :: Int) <> "_" <> dropExtension (B8.unpack name)
    Right fsb4 -> case fsb4Songs fsb4 of
      [song] -> return [ dropExtension (B8.unpack $ fsbSongName song) <> "_" <> show (i :: Int) | i <- [0..] ]
      _      -> fail "Not exactly 1 stream in FSB4" -- shouldn't happen, caught by splitFSBStreams
  forM_ (zip streams streamNames) $ \(stream, streamName) -> do
    case stream of
      FSB_MP3  bs   -> BL.writeFile (dir </> streamName <.> "mp3") bs
      FSB_XMA1 xma1 -> writeXMA1    (dir </> streamName <.> "xma") xma1
      FSB_XMA2 xma2 -> writeXMA2    (dir </> streamName <.> "xma") xma2

data FSBStream
  = FSB_MP3  BL.ByteString
  | FSB_XMA1 XMA1Contents
  | FSB_XMA2 XMA2Contents

getFSBStreamBytes :: FSBStream -> IO (BL.ByteString, T.Text)
getFSBStreamBytes = \case
  FSB_MP3 bs -> return (bs, "mp3")
  FSB_XMA1 xma1 -> do
    bs <- makeXMA1 xma1
    return (bs, "xma")
  FSB_XMA2 xma2 -> do
    bs <- makeXMA2 xma2
    return (bs, "xma")

-- Splits an FSB3 or FSB4 file into streams, each of which is mono or stereo.
splitFSBStreams :: FSB -> IO [FSBStream]
splitFSBStreams fsb = do
  case fsbHeader fsb of
    Left fsb3 -> forM (zip (fsb3Songs fsb3) (fsbSongData fsb)) $ \(song, sdata) -> do
      unless (fsbSongChannels song == 2) $ fail $
        "Expected stereo audio but found " <> show (fsbSongChannels song) <> " channels"
      case fsbExtra song of
        Left xma -> do
          return $ FSB_XMA1 XMA1Contents
            { xma1Channels  = fromIntegral $ fsbSongChannels song
            , xma1Rate      = fromIntegral $ fsbSongSampleRate song
            , xma1SeekTable = fsbXMASeek xma -- both fsb3 and .xma xma1 start with 0 entry (.xma xma2 does not)
            , xma1Data      = sdata
            }
        Right _mp3 -> return $ FSB_MP3 sdata
    -- FSB4 in Neversoft GH and (I think?) Power Gig is a single multi-channel "song".
    -- FSB4 in PS3 Rock Revolution is more like FSB3, with separate songs.
    Right fsb4 -> fmap concat $ forM (zip (fsb4Songs fsb4) (fsbSongData fsb)) $ \(song, sdata) -> do
      let (stereoCount, monoCount) = quotRem (fromIntegral $ fsbSongChannels song) 2
      case fsbExtra song of
        Left _xma -> do
          marked <- markXMA2PacketStreams <$> splitXMA2Packets sdata
          return $ flip map [0 .. stereoCount + monoCount - 1] $ \i -> FSB_XMA2 $ XMA2Contents
            { xma2Channels        = if i == stereoCount then 1 else 2
            , xma2Rate            = fromIntegral $ fsbSongSampleRate song
            , xma2Samples         = fromIntegral $ fsbSongSamples song
            , xma2PacketsPerBlock = 16
            , xma2SeekTable       = Nothing
            , xma2Data            = writeXMA2Packets $ extractXMAStream 16 i marked
            }
        Right _mp3 -> map FSB_MP3 <$> splitInterleavedMP3 (fromIntegral $ stereoCount + monoCount) sdata

-- Currently FFMPEG does not read XMA1 correctly.
-- So for project import purposes, convert input XMA1 to XMA2.
splitFSBStreams' :: FSB -> IO [FSBStream]
splitFSBStreams' fsb = do
  streams <- splitFSBStreams fsb
  forConcurrently streams $ \case
    FSB_XMA1 xma1 -> do
      xma2 <- xma1To2 xma1
      _ <- evaluate $ BL.length $ xma2Data xma2
      return $ FSB_XMA2 xma2
    stream        -> return stream

-- Only works on a CBR MP3 with no ID3 tags
mp3CBRFrameSize :: BL.ByteString -> Maybe Int64
mp3CBRFrameSize bs = do
  -- replace with BL.indexMaybe after we upgrade to bytestring 0.11.0.0
  guard $ BL.length bs >= 3
  let byte1 = BL.index bs 1
      byte2 = BL.index bs 2
  mpegVersion <- case (byte1 .&. 0x18) `shiftR` 3 of
    2 -> Just 2
    3 -> Just 1
    _ -> Nothing
  layerVersion <- case (byte1 .&. 0x6) `shiftR` 1 of
    1 -> Just 3
    2 -> Just 2
    3 -> Just 1
    _ -> Nothing
  samplesPerFrame <- case (mpegVersion, layerVersion) of
    (_, 1) -> Just 384
    (_, 2) -> Just 1152
    (1, 3) -> Just 1152
    (2, 3) -> Just 576
    _      -> Nothing
  bitRateLookup <- case (mpegVersion :: Int, layerVersion :: Int) of
    (1, 1) -> Just [0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 0]
    (1, 2) -> Just [0, 32, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384, 0]
    (1, 3) -> Just [0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 0]
    (2, 1) -> Just [0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0]
    (2, _) -> Just [0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0]
    _      -> Nothing
  bitRate <- case drop (fromIntegral ((byte2 .&. 0xF0) `shiftR` 4)) bitRateLookup of
    kbps : _ -> guard (kbps /= 0) >> Just ((kbps :: Rational) * 1000)
    []       -> Nothing
  sampleRate <- case (mpegVersion, (byte2 .&. 0xC) `shiftR` 2) of
    (1, 0) -> Just 44100
    (1, 1) -> Just 48000
    (1, 2) -> Just 32000
    (2, 0) -> Just 22050
    (2, 1) -> Just 24000
    (2, 2) -> Just 16000
    _      -> Nothing
  let padding = fromIntegral $ (byte2 .&. 0x2) `shiftR` 1
  -- Previously this used an algorithm from http://www.datavoyage.com/mpgscript/mpeghdr.htm
  -- But it gave wrong results at low bit/sample rates, so now this is from
  -- https://www.codeproject.com/articles/8295/mpeg-audio-frame-header
  -- Also previously used round, but floor is necessary for importing Power Gig PS3
  return $ floor $ (samplesPerFrame / 8 * bitRate) / sampleRate + padding

trimMP3Frame :: BL.ByteString -> BL.ByteString
trimMP3Frame bs = case mp3CBRFrameSize bs of
  Nothing   -> bs -- dunno
  Just size -> BL.take size bs

splitInterleavedMP3 :: (MonadFail m) => Int64 -> BL.ByteString -> m [BL.ByteString]
splitInterleavedMP3 1 bs = return [bs]
splitInterleavedMP3 n bs = do
  frameSize <- case mp3CBRFrameSize bs of
    Nothing -> fail "Couldn't parse frame size of interleaved MP3 to split apart"
    Just size -> return size
  -- need to round up to 0x10, as seen in Power Gig PS3 (44100 Hz, 192 kbps)
  let roundedFrameSize = case quotRem frameSize 0x10 of
        (_, 0) -> frameSize
        (x, _) -> (x + 1) * 0x10
      getPart i = BL.concat $ takeWhile (not . BL.null) $ do
        j <- [i * roundedFrameSize, (i + n) * roundedFrameSize ..]
        -- actual frame size differs in Power Gig PS3, so we need to trim each one to its specific size
        return $ trimMP3Frame $ BL.take roundedFrameSize $ BL.drop j bs
  return $ map getPart [0 .. n - 1]

interleaveMP3 :: (MonadFail m) => [BL.ByteString] -> m BL.ByteString
interleaveMP3 [x]  = return x
interleaveMP3 mp3s = do
  -- lame is sometimes inconsistent with padding of same-sized audio streams
  -- (in our case the backing and silent tracks)
  -- so instead of failing on inconsistent file size,
  -- we now just use the shortest file size and drop any extra frames
  frameSize <- case mapM mp3CBRFrameSize mp3s of
    Nothing -> fail "Unable to calculate frame size of input MP3"
    Just [] -> fail "No MP3s given to interleave"
    Just (size : sizes) -> if all (== size) sizes
      then return size
      else fail "Inconsistent frame size in MP3s to interleave"
  let minLength = minimum $ map BL.length mp3s
  eachFrameCount <- case quotRem minLength frameSize of
    (count, 0) -> return $ fromIntegral count
    _          -> fail "MP3 to interleave has a length not divisible by frame size"
  return $ BL.concat $ concat $ transpose $ flip map mp3s $ \mp3 -> do
    posn <- take eachFrameCount $ [0, frameSize ..]
    return $ BL.take frameSize $ BL.drop posn mp3

markXMA2PacketStreams :: [XMA2Packet] -> [(Int, XMA2Packet)]
markXMA2PacketStreams = go 0 Map.empty where
  go nextNewStream skips (pkt : pkts) = case Map.toList $ Map.filter (== 0) skips of
    (i, _) : _ -> let
      skips' = Map.insert i (xma2PacketSkipCount pkt) $ Map.map (subtract 1) skips
      in (i, pkt) : go nextNewStream skips' pkts
    [] -> let
      skips' = Map.insert nextNewStream (xma2PacketSkipCount pkt) $ Map.map (subtract 1) skips
      in (nextNewStream, pkt) : go (nextNewStream + 1) skips' pkts
  go _ _ [] = []

-- Regroups packets to fit more of them into the 16-packet blocks.
extractXMAStream :: Int -> Int -> [(Int, XMA2Packet)] -> [XMA2Packet]
extractXMAStream packetsPerBlock i = go . splitBlocks where
  dummyBlock = XMA2Packet
    { xma2FrameCount = 0
    , xma2FrameOffsetInBits = 0
    , xma2PacketMetadata = 0
    , xma2PacketSkipCount = 0
    , xma2PacketData = B.replicate 2044 0
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
  splitBlocks pkts = case splitAt packetsPerBlock pkts of
    ([] , _   ) -> []
    (blk, rest) -> map snd (filter ((== i) . fst) blk) : splitBlocks rest
  go :: [[XMA2Packet]] -> [XMA2Packet]
  go [] = []
  go (blk : blks) = case blks of
    []          -> complete packetsPerBlock blk
    next : rest -> if length blk + length next <= packetsPerBlock
      then go $ (blk <> next) : rest
      else complete packetsPerBlock blk <> go blks

-- when we get to makeXMA1SeekTable:
-- * entries are per packet
-- * do they include a frame in progress at packet start or not? need to check
-- * they are little endian! (big endian in xma 2)

-- Note, returns a 0 in front (.fsb format, not .xma)
makeXMA2SeekTable :: (MonadFail m) => XMA2Contents -> m [Word32]
makeXMA2SeekTable xma = do
  pkts <- splitXMA2Packets $ xma2Data xma
  let stream0Counts = map
        (\(stream, pkt) -> if stream == 0 then xma2FrameCount pkt else 0)
        (markXMA2PacketStreams pkts)
  return $ do
    -- Packets are 2048 bytes in all XMA. Each frame is 512 samples.
    -- FSB appear to use 16-packet blocks. (Maybe this is stored somewhere?)
    -- Each entry in the seek table is "how many samples would you have decoded by block N"
    packetIndex <- [0, xma2PacketsPerBlock xma .. length stream0Counts]
    return $ sum (take packetIndex stream0Counts) * 512

data XMA1Contents = XMA1Contents
  { xma1Channels  :: Int
  , xma1Rate      :: Int
  , xma1SeekTable :: [Word32] -- this is per-packet (not per-block like XMA2)
  , xma1Data      :: BL.ByteString
  } deriving (Eq, Generic, Hashable)

data XMA2Contents = XMA2Contents
  { xma2Channels        :: Int
  , xma2Rate            :: Int
  , xma2Samples         :: Int
  , xma2PacketsPerBlock :: Int -- 16 for FSB4 (GH), 32 for PowerGig .xma
  , xma2SeekTable       :: Maybe [Word32] -- should start from block 1 (not 0) like .xma (not .fsb)
  , xma2Data            :: BL.ByteString
  } deriving (Eq, Generic, Hashable)

parseXMA2 :: (MonadFail m) => BL.ByteString -> m XMA2Contents
parseXMA2 b = parseXMA b >>= \case
  Left  _    -> fail "Expected to parse XMA2 but found XMA1"
  Right xma2 -> return xma2

parseXMA :: (MonadFail m) => BL.ByteString -> m (Either XMA1Contents XMA2Contents)
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
    seek <- findChunk "seek" riff
    data_ <- findChunk "data" riff
    flip runGetM fmt $ do
      fmtTag   <- getWord16le -- wFormatTag;      // Audio format type; always WAVE_FORMAT_XMA2
      case fmtTag of
        0x165 -> do -- XMA1
          -- rest of XMAWAVEFORMAT (includes format tag)
          _          <- getWord16le -- BitsPerSample;      // Bit depth (currently required to be 16)
          _          <- getWord16le -- EncodeOptions;      // Options for XMA encoder/decoder
          _          <- getWord16le -- LargestSkip;        // Largest skip used in interleaving streams
          numStreams <- getWord16le -- NumStreams;         // Number of interleaved audio streams
          _          <- getWord8    -- LoopCount;          // Number of loop repetitions; 255 = infinite
          _          <- getWord8    -- Version;            // XMA encoder version that generated the file.
                                    --                     // Always 3 or higher for XMA2 files.
          -- XMASTREAMFORMAT XmaStreams[1]; // Per-stream format information; the actual array length is in the NumStreams field.
          -- [However, Onyx will only support single-stream XMA1 files.]
          streams <- replicateM (fromIntegral numStreams) $ do
            _        <- getWord32le -- PsuedoBytesPerSec; // Used by the XMA encoder (typo preserved for legacy reasons)
            rate     <- getWord32le -- SampleRate;        // The stream's decoded sample rate (in XMA2 files,
                                    --                    // this is the same for all streams in the file).
            _        <- getWord32le -- LoopStart;         // Bit offset of the frame containing the loop start
                                    --                    // point, relative to the beginning of the stream.
            _        <- getWord32le -- LoopEnd;           // Bit offset of the frame containing the loop end.
            _        <- getWord8    -- SubframeData;      // Two 4-bit numbers specifying the exact location of
                                    --                    // the loop points within the frames that contain them.
                                    --                    //   SubframeEnd: Subframe of the loop end frame where
                                    --                    //                the loop ends.  Ranges from 0 to 3.
                                    --                    //   SubframeSkip: Subframes to skip in the start frame to
                                    --                    //                 reach the loop.  Ranges from 0 to 4.
            channels <- getWord8    -- Channels;          // Number of channels in the stream (1 or 2)
            _        <- getWord16le -- ChannelMask;       // Spatial positions of the channels in the stream
            return (channels, rate)
          (channels, rate) <- case streams of
            [pair] -> return pair
            _      -> fail $ "We only support 1 stream in XMA1 files, but found " <> show (length streams)
          seekTable <- flip runGetM seek $ do
            1     <- getWord32le -- always?
            count <- getWord32le
            replicateM (fromIntegral count) getWord32le -- note, little endian! (xma2 is big)
          return $ Left XMA1Contents
            { xma1Channels  = fromIntegral channels
            , xma1Rate      = fromIntegral rate
            , xma1SeekTable = seekTable
            , xma1Data      = data_
            }
        0x166 -> do -- XMA2
          -- WAVEFORMATEX (includes format tag)
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
          blkSize  <- getWord32le -- BytesPerBlock;  // XMA block size (but the last one may be shorter)
          _        <- getWord32le -- PlayBegin;      // First valid sample in the decoded audio
          _        <- getWord32le -- PlayLength;     // Length of the valid part of the decoded audio
          _        <- getWord32le -- LoopBegin;      // Beginning of the loop region in decoded sample terms
          _        <- getWord32le -- LoopLength;     // Length of the loop region in decoded sample terms
          _        <- getWord8    -- LoopCount;      // Number of loop repetitions; 255 = infinite
          _        <- getWord8    -- EncoderVersion; // Version of XMA encoder that generated the file
          _        <- getWord16le -- BlockCount;     // XMA blocks in file (and entries in its seek table)
          packetsPerBlock <- case quotRem blkSize 2048 of
            (ppblk, 0) -> return ppblk
            _          -> fail "Expected a block size divisible by 2048"
          let getSeekTable = isEmpty >>= \case
                True  -> return []
                False -> liftM2 (:) getWord32be getSeekTable
              -- TODO maybe use BlockCount from above instead of going to end?
          seekTable <- runGetM getSeekTable seek
          return $ Right XMA2Contents
            { xma2Channels        = fromIntegral channels
            , xma2Rate            = fromIntegral rate
            , xma2Samples         = fromIntegral len
            , xma2PacketsPerBlock = fromIntegral packetsPerBlock
            , xma2SeekTable       = Just seekTable
            , xma2Data            = data_
            }
        _ -> fail $ "Unrecognized RIFF format tag in .xma fmt chunk: 0x" <> showHex fmtTag ""

-- | Returns a new XMA with initial blocks/packets removed, and a remaining frame count to skip manually.
seekXMA :: (MonadIO m, MonadFail m) => BL.ByteString -> Duration -> m (Readable, Int)
seekXMA bs pos = parseXMA bs >>= \case
  Left  xma1 -> do
    let frames = case pos of
          Seconds s -> secondsToFrames s $ fromIntegral $ xma1Rate xma1
          Frames  f -> f
        seekTable = xma1SeekTable xma1
    let go !currentPackets !currentFrames [] = (currentPackets, currentFrames, [])
        go !currentPackets !currentFrames (nextFrames : rest) = if fromIntegral frames >= nextFrames
          then go (currentPackets + 1) nextFrames rest
          else (currentPackets, currentFrames, rest)
        (dropPackets, dropPos, newSeekTable) = go 0 0 seekTable
    newXMA <- liftIO $ makeXMA1 xma1
      { xma1SeekTable = newSeekTable
      , xma1Data = BL.drop (dropPackets * 2048) $ xma1Data xma1
      }
    return (makeHandle "temp.xma" $ byteStringSimpleHandle newXMA, frames - fromIntegral dropPos)
  Right xma2 -> do
    let frames = case pos of
          Seconds s -> secondsToFrames s $ fromIntegral $ xma2Rate xma2
          Frames  f -> f
    seekTable <- maybe (drop 1 <$> makeXMA2SeekTable xma2) return $ xma2SeekTable xma2
    let go !currentBlocks !currentFrames [] = (currentBlocks, currentFrames, [])
        go !currentBlocks !currentFrames (nextFrames : rest) = if fromIntegral frames >= nextFrames
          then go (currentBlocks + 1) nextFrames rest
          else (currentBlocks, currentFrames, rest)
        (dropBlocks, dropPos, newSeekTable) = go 0 0 seekTable
    [newXMA] <- liftIO $ makeXMA2s $ return xma2
      { xma2Samples = xma2Samples xma2 - fromIntegral dropPos
      , xma2SeekTable = Just newSeekTable
      , xma2Data = BL.drop (dropBlocks * fromIntegral (xma2PacketsPerBlock xma2) * 2048) $ xma2Data xma2
      }
    return (makeHandle "temp.xma" $ byteStringSimpleHandle newXMA, frames - fromIntegral dropPos)

riffChunk :: IO.Handle -> B.ByteString -> IO a -> IO a
riffChunk h magic f = do
  let getPosn = IO.hGetPosn h
  B.hPut h magic
  lenPosn <- getPosn
  B.hPut h $ B.pack [0xDE, 0xAD, 0xBE, 0xEF] -- filled in later
  HandlePosn _ start <- getPosn
  x <- f
  endPosn@(HandlePosn _ end) <- getPosn
  IO.hSetPosn lenPosn
  writePut h $ putWord32le $ fromIntegral $ end - start
  IO.hSetPosn endPosn
  return x

writePut :: IO.Handle -> Put -> IO ()
writePut h = BL.hPut h . runPut

writeXMA1 :: FilePath -> XMA1Contents -> IO ()
writeXMA1 fp xma = IO.withBinaryFile fp IO.WriteMode $ \h -> do
  let xmaSeek = xma1SeekTable xma
  riffChunk h "RIFF" $ do
    B.hPut h "WAVE"
    riffChunk h "fmt " $ do
      writePut h $ putWord16le 0x165 -- FormatTag;          // Audio format type (always WAVE_FORMAT_XMA)
      writePut h $ putWord16le 16    -- BitsPerSample;      // Bit depth (currently required to be 16)
      writePut h $ putWord16le 0     -- EncodeOptions;      // Options for XMA encoder/decoder [does this matter? 0x1D6 in test file]
      writePut h $ putWord16le 0     -- LargestSkip;        // Largest skip used in interleaving streams
      writePut h $ putWord16le 1     -- NumStreams;         // Number of interleaved audio streams
      writePut h $ putWord8    0     -- LoopCount;          // Number of loop repetitions; 255 = infinite
      writePut h $ putWord8    2     -- Version;            // XMA encoder version that generated the file.
                                     --                     // Always 3 or higher for XMA2 files.
      -- XMASTREAMFORMAT XmaStreams[1]; // Per-stream format information; the actual
      --                                // array length is in the NumStreams field.
      writePut h $ putWord32le 0                              -- PsuedoBytesPerSec; // Used by the XMA encoder (typo preserved for legacy reasons) [does this matter?]
      writePut h $ putWord32le $ fromIntegral $ xma1Rate xma  -- SampleRate;        // The stream's decoded sample rate (in XMA2 files,
                                                              --                    // this is the same for all streams in the file).
      writePut h $ putWord32le 0                              -- LoopStart;         // Bit offset of the frame containing the loop start
                                                              --                    // point, relative to the beginning of the stream.
      writePut h $ putWord32le 0                              -- LoopEnd;           // Bit offset of the frame containing the loop end.
      writePut h $ putWord8 0                                 -- SubframeData;      // Two 4-bit numbers specifying the exact location of
                                                              --                    // the loop points within the frames that contain them.
                                                              --                    //   SubframeEnd: Subframe of the loop end frame where
                                                              --                    //                the loop ends.  Ranges from 0 to 3.
                                                              --                    //   SubframeSkip: Subframes to skip in the start frame to
                                                              --                    //                 reach the loop.  Ranges from 0 to 4.
      writePut h $ putWord8 $ fromIntegral $ xma1Channels xma -- Channels;          // Number of channels in the stream (1 or 2)
      channelMask <- case xma1Channels xma of
        1 -> return 0x0001
        2 -> return 0x0201
        -- shouldn't happen (all streams are mono or stereo)
        _ -> fail $ "Unsupported channel count for writing XMA1 stream: " <> show (xma1Channels xma)
      writePut h $ putWord16le channelMask                    -- ChannelMask;       // Spatial positions of the channels in the stream

    -- Probably doesn't matter but xmaencode.exe puts "data" before "seek"
    riffChunk h "data" $ do
      BL.hPut h $ xma1Data xma
    riffChunk h "seek" $ do
      writePut h $ putWord32le 1
      writePut h $ putWord32le $ fromIntegral $ length xmaSeek
      forM_ xmaSeek $ writePut h . putWord32le

writeXMA2 :: FilePath -> XMA2Contents -> IO ()
writeXMA2 fp xma = IO.withBinaryFile fp IO.WriteMode $ \h -> do
  xmaSeek <- makeXMA2SeekTable xma -- TODO use existing one if it's there
  riffChunk h "RIFF" $ do
    B.hPut h "WAVE"
    riffChunk h "fmt " $ do
      let c = xma2Channels xma
          r = xma2Rate xma
          len = xma2Samples xma
          blockSize = fromIntegral (xma2PacketsPerBlock xma) * 2048

      -- WAVEFORMATEX wfx;
      writePut h $ putWord16le 0x166                -- wFormatTag;      // Audio format type; always WAVE_FORMAT_XMA2
      writePut h $ putWord16le $ fromIntegral c     -- nChannels;       // Channel count of the decoded audio
      writePut h $ putWord32le $ fromIntegral r     -- nSamplesPerSec;  // Sample rate of the decoded audio
      writePut h $ putWord32le 0 -- TODO            -- nAvgBytesPerSec; // Used internally by the XMA encoder
      writePut h $ putWord16le $ fromIntegral c * 2 -- nBlockAlign;     // Decoded sample size; channels * wBitsPerSample / 8
      writePut h $ putWord16le 0x10                 -- wBitsPerSample;  // Bits per decoded mono sample; always 16 for XMA
      writePut h $ putWord16le 0x22                 -- cbSize;          // Size in bytes of the rest of this structure (34)

      writePut h $ putWord16le $ (fromIntegral c + 1) `quot` 2 -- NumStreams;     // Number of audio streams (1 or 2 channels each)
      writePut h $ putWord32le 0                               -- ChannelMask;    // Spatial positions of the channels in this file
      writePut h $ putWord32le $ fromIntegral len              -- SamplesEncoded; // Total number of PCM samples the file decodes to
      writePut h $ putWord32le blockSize                       -- BytesPerBlock;  // XMA block size (but the last one may be shorter)
      writePut h $ putWord32le 0                               -- PlayBegin;      // First valid sample in the decoded audio
      writePut h $ putWord32le $ fromIntegral len              -- PlayLength;     // Length of the valid part of the decoded audio
      writePut h $ putWord32le 0                               -- LoopBegin;      // Beginning of the loop region in decoded sample terms
      writePut h $ putWord32le 0                               -- LoopLength;     // Length of the loop region in decoded sample terms
      writePut h $ putWord8 0                                  -- LoopCount;      // Number of loop repetitions; 255 = infinite
      writePut h $ putWord8 4                                  -- EncoderVersion; // Version of XMA encoder that generated the file
      writePut h $ putWord16le $ fromIntegral $ length xmaSeek -- BlockCount;     // XMA blocks in file (and entries in its seek table)
      -- TODO should that be "length xmaSeek - 1"? We might be missing a final entry in our table

    riffChunk h "seek" $ do
      -- FSB's version of the seek table starts from block 0, and is little-endian.
      -- But here it starts from block 1, and is big-endian
      forM_ (drop 1 xmaSeek) $ writePut h . putWord32be
    riffChunk h "data" $ do
      BL.hPut h $ xma2Data xma

makeXMA2s :: (Traversable f) => f XMA2Contents -> IO (f BL.ByteString)
makeXMA2s xmas = withSystemTempDirectory "onyx-xma" $ \tmp -> do
  let tmpFile = tmp </> "out.xma"
  forM xmas $ \xma -> do
    writeXMA2 tmpFile xma
    BL.fromStrict <$> B.readFile tmpFile

makeXMA2 :: XMA2Contents -> IO BL.ByteString
makeXMA2 = fmap runIdentity . makeXMA2s . Identity

makeXMA1 :: XMA1Contents -> IO BL.ByteString
makeXMA1 xma = withSystemTempDirectory "onyx-xma" $ \tmp -> do
  let tmpFile = tmp </> "out.xma"
  writeXMA1 tmpFile xma
  BL.fromStrict <$> B.readFile tmpFile

mp3sToFSB3 :: (MonadIO m, MonadFail m) => [(B.ByteString, BL.ByteString)] -> m FSB
mp3sToFSB3 mp3s = do

  songs <- forM mp3s $ \(name, mp3) -> do
    src <- liftIO $ ffSource $ Left $ makeHandle "mp3" $ byteStringSimpleHandle mp3
    let _ = src :: CA.AudioSource (ResourceT IO) Int16
        numFrames = fromIntegral $ CA.frames src
        song = FSBSong
          { fsbSongHeaderSize = 0
          , fsbSongName       = name
          , fsbSongSamples    = numFrames
          -- interestingly, if you get fsbSongSamples wrong, gh3 practice mode seeks to the wrong place.
          -- I think it seeks to roughly "(desiredSamples / fsbSongSamples) * fsbSongDataSize"
          , fsbSongDataSize   = 0
          , fsbSongLoopStart  = 0
          , fsbSongLoopEnd    = numFrames - 1
          , fsbSongMode       = 0x240 -- this is from SanicStudios' mp3 fsbs
          , fsbSongSampleRate = round $ CA.rate src
          , fsbSongDefVol     = 255
          , fsbSongDefPan     = 128
          , fsbSongDefPri     = 255
          , fsbSongChannels   = fromIntegral $ CA.channels src
          , fsbSongMinDistance = 0x803F
          , fsbSongMaxDistance = 0x401C46
          , fsbExtra = Right FSBExtraMP3
            -- copied from official gh3 files
            { fsbMP3Unknown1 = 0
            , fsbMP3Unknown2 = 0
            }
          }
    return (song, mp3)

  return $ fixFSB $ FSB
    { fsbHeader = Left $ FSB3Header
      { fsb3SongCount   = fromIntegral $ length songs
      , fsb3HeadersSize = 0
      , fsb3DataSize    = 0
      , fsb3Version     = 0x30001
      , fsb3Flags       = 0
      , fsb3Songs       = map fst songs
      }
    , fsbSongData = map snd songs
    }

xmasToFSB3 :: (MonadFail m) => [(B.ByteString, (XMA1Contents, Word32))] -> m FSB
xmasToFSB3 xmas = do

  songs <- forM xmas $ \(name, (xma, numFrames)) -> do
    let seekTable = xma1SeekTable xma
        lenSeekTable = fromIntegral $ length seekTable
        song = FSBSong
          { fsbSongHeaderSize = 0
          , fsbSongName       = name
          , fsbSongSamples    = numFrames * 512
          , fsbSongDataSize   = 0
          , fsbSongLoopStart  = 0
          , fsbSongLoopEnd    = numFrames * 512 - 1
          , fsbSongMode       = 0x1002040
          , fsbSongSampleRate = fromIntegral $ xma1Rate xma -- RR uses 48000 but plays 44100 ok. GH3 uses 44100
          , fsbSongDefVol     = 255 -- 1 in RR (maybe not used) but GH3 does apply it
          , fsbSongDefPan     = 128 -- 0 in RR (maybe not used) but GH3 probably does apply it
          , fsbSongDefPri     = 255 -- 128 in RR (dunno what this is)
          , fsbSongChannels   = fromIntegral $ xma1Channels xma

          , fsbSongMinDistance = 0x803F
          , fsbSongMaxDistance = 0x401C46

          , fsbExtra = Left FSBExtraXMA
            { fsbXMAUnknown1 = 0
            , fsbXMAUnknown2 = 0
            , fsbXMAUnknown3 = 32
            , fsbXMAUnknown4 = 0 -- RR 0x5ac6c22 but not consistent
            , fsbXMAUnknown5 = 19
            , fsbXMAUnknown6 = 8 + 4 * lenSeekTable
            , fsbXMAUnknown7 = fromIntegral (xma1Channels xma + 1) `quot` 2
            , fsbXMAUnknown8 = lenSeekTable + 1
            , fsbXMASeek = seekTable
            }

          }
    return (song, xma1Data xma)

  return $ fixFSB $ FSB
    { fsbHeader = Left $ FSB3Header
      { fsb3SongCount   = fromIntegral $ length songs
      , fsb3HeadersSize = 0
      , fsb3DataSize    = 0
      , fsb3Version     = 0x30001
      , fsb3Flags       = 0
      , fsb3Songs       = map fst songs
      }
    , fsbSongData = map snd songs
    }

xmasToFSB4 :: (MonadFail m) => [(B.ByteString, XMA2Contents)] -> m FSB
xmasToFSB4 xmas = do

  songs <- forM xmas $ \(name, xma) -> do
    -- FSB should use 32 KB blocks (16-packet)
    unless (xma2PacketsPerBlock xma == 16) $ fail "XMA data doesn't have FSB packets-per-block of 16"
    seekTable <- makeXMA2SeekTable xma
    let lenSeekTable = fromIntegral $ length seekTable
        song = FSBSong
          { fsbSongHeaderSize = 0
          , fsbSongName       = name -- does not appear to matter. for gh3 even, looks like only position matters
          , fsbSongSamples    = fromIntegral $ xma2Samples xma
          , fsbSongDataSize   = 0
          , fsbSongLoopStart  = 0
          , fsbSongLoopEnd    = maxBound -- should be -1. for RR, fsbSongSamples - 1
          , fsbSongMode       = 0x5000000 -- for RR, 0x1002040
          , fsbSongSampleRate = fromIntegral $ xma2Rate xma -- 48000 in RR
          , fsbSongDefVol     = 255 -- 1 in RR
          , fsbSongDefPan     = 128 -- 0 in RR
          , fsbSongDefPri     = 128
          , fsbSongChannels   = fromIntegral $ xma2Channels xma

          , fsbSongMinDistance = 0x803F
          , fsbSongMaxDistance = 0x401C46

          , fsbExtra = Left FSBExtraXMA
            { fsbXMAUnknown1 = 0
            , fsbXMAUnknown2 = 0
            , fsbXMAUnknown3 = 0 -- RR 32
            , fsbXMAUnknown4 = 0 -- RR 0x5ac6c22 but not consistent
            , fsbXMAUnknown5 = 0 -- RR 19
            , fsbXMAUnknown6 = 8 + 4 * lenSeekTable
            , fsbXMAUnknown7 = fromIntegral (xma2Channels xma + 1) `quot` 2
            , fsbXMAUnknown8 = lenSeekTable + 1
            , fsbXMASeek = seekTable
            }

          }
    return (song, xma2Data xma)

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
ghBandXMAtoFSB4 :: (MonadFail m) => XMA2Contents -> m FSB
ghBandXMAtoFSB4 xma = xmasToFSB4 [("multichannel sound", xma)]

ghBandMP3sToFSB4 :: (MonadFail m, MonadIO m) => [BL.ByteString] -> m FSB
ghBandMP3sToFSB4 mp3s = do

  srcs <- liftIO $ mapM (ffSource . Left . makeHandle "mp3" . byteStringSimpleHandle) mp3s
  let _ = srcs :: [CA.AudioSource (ResourceT IO) Int16]

  let song = FSBSong
        { fsbSongHeaderSize = 0
        , fsbSongName       = "multichannel sound"
        , fsbSongSamples    = fromIntegral $ CA.frames $ head srcs
        , fsbSongDataSize   = 0
        , fsbSongLoopStart  = 0
        , fsbSongLoopEnd    = maxBound -- should be -1
        , fsbSongMode       = 0x14000200
        , fsbSongSampleRate = round $ CA.rate $ head srcs
        , fsbSongDefVol     = 255
        , fsbSongDefPan     = 128
        , fsbSongDefPri     = 128
        , fsbSongChannels   = fromIntegral $ sum $ map CA.channels srcs
        , fsbSongMinDistance = 0x803F
        , fsbSongMaxDistance = 0x401C46
        , fsbExtra = Right FSBExtraMP3
          -- no idea what these are
          { fsbMP3Unknown1 = 0x50
          , fsbMP3Unknown2 = 0
          }
        }

  interleaved <- interleaveMP3 mp3s

  return $ fixFSB $ FSB
    { fsbHeader = Right $ FSB4Header
      { fsb4SongCount   = 1
      , fsb4HeadersSize = 0
      , fsb4DataSize    = 0
      , fsb4Version     = 0x40000
      , fsb4Flags       = 32
      , fsb4Hash        = B.replicate 8 0
      , fsb4GUID        = B.take 16 $ "onyx" <> B8.pack (show $ hash mp3s) <> "................"
      , fsb4Songs       = [song]
      }
    , fsbSongData = [interleaved]
    }

data XMAFrame = XMAFrame
  { xmaFrameBitLength :: !Int
  -- bits are stored from high to low in each byte
  -- content also includes the length bits
  , xmaFrameContent   :: !B.ByteString
  -- note, when writing, the last bit of each frame should be changed to be 1
  -- if there are more frames in this packet, and 0 otherwise
  } deriving (Show)

getStringBit :: B.ByteString -> Int -> Bool
getStringBit packetData b = case quotRem b 8 of
  -- when we upgrade bytesring, use indexMaybe
  (x, y) -> B.index packetData x `testBit` (7 - y)

xmaFrameBits :: XMAFrame -> [Bool]
xmaFrameBits frame = map
  (getStringBit $ xmaFrameContent frame)
  [0 .. xmaFrameBitLength frame - 1]

packBitsBE :: (Bits a) => [Bool] -> a
packBitsBE = foldr (\(i, b) acc -> if b then setBit acc i else acc) zeroBits . zip [0..] . reverse

packBitsFrame :: [Bool] -> XMAFrame
packBitsFrame bits = XMAFrame
  { xmaFrameBitLength = length bits
  , xmaFrameContent
    = B.pack
    $ map (\bools -> packBitsBE $ take 8 $ bools <> repeat False)
    $ chunksOf 8 bits
  }

-- Ignores packet skip values (use markXMA2PacketStreams to filter first).
splitXMAFrames :: [Either XMA1Packet XMA2Packet] -> [XMAFrame]
splitXMAFrames = freshPacket where
  bitsPerPacket :: Int
  bitsPerPacket = 2044 * 8
  freshPacket = \case
    [] -> []
    pkt : pkts -> case either xma1FrameOffsetInBits xma2FrameOffsetInBits pkt of
      0x7FFF -> freshPacket pkts -- no frames, skip this packet
      bitOffset -> let
        packetData = either xma1PacketData xma2PacketData pkt
        in newFrame (fromIntegral bitOffset) packetData pkts
  newFrame :: Int -> B.ByteString -> [Either XMA1Packet XMA2Packet] -> [XMAFrame]
  newFrame !bitOffset !packetData pkts = let
    -- the length bits may extend into the next packet
    extendedPacketData = case pkts of
      pkt : _ -> packetData <> either xma1PacketData xma2PacketData pkt
      []      -> packetData
    len = packBitsBE $ map (getStringBit extendedPacketData) $ take 15 [bitOffset ..]
    bitsLeft = bitsPerPacket - bitOffset
    in if quot bitOffset 8 >= B.length extendedPacketData
      then [] -- not sure here. was getting index error at the end of Kool Thing preview audio
      else if bitsLeft >= len
        then let
          frame = packBitsFrame $ map (getStringBit packetData) $ take len [bitOffset ..]
          in frame : continuePacket frame (bitOffset + len) packetData pkts
        else continueFrame (map (getStringBit packetData) [bitOffset .. bitsPerPacket - 1]) (len - bitsLeft) pkts
  continuePacket lastFrame bitOffset packetData pkts = let
    trailerBit = getStringBit (xmaFrameContent lastFrame) (xmaFrameBitLength lastFrame - 1)
    in if trailerBit
      then newFrame bitOffset packetData pkts
      else freshPacket pkts
  continueFrame partialBits needBits = \case
    [] -> [] -- maybe warn?
    allPackets@(pkt : pkts) -> let
      packetData = either xma1PacketData xma2PacketData pkt
      in if needBits <= bitsPerPacket
        then let
          frame = packBitsFrame $ partialBits <> map (getStringBit packetData) [0 .. needBits - 1]
          in frame : freshPacket allPackets
        else continueFrame (partialBits <> map (getStringBit packetData) [0 .. bitsPerPacket - 1]) (needBits - bitsPerPacket) pkts

buildXMA2Stream :: Int -> [XMAFrame] -> [XMA2Packet]
buildXMA2Stream packetsPerBlock topFrames = runST (newBlock topFrames) where
  newPacket = XMA2Packet
    { xma2FrameCount        = 0
    , xma2FrameOffsetInBits = 0x7FFF
    , xma2PacketMetadata    = 1
    , xma2PacketSkipCount   = 0
    , xma2PacketData        = mempty
    }
  emptyPacket = newPacket
    { xma2PacketData = B.replicate 2044 0xFF
    }
  maxPacketBits = (2048 - 4) * 8
  newPacketData :: ST s (STUArray s Int Word8)
  newPacketData = newArray (0, 2043) 0xFF
  newBlock :: [XMAFrame] -> ST s [XMA2Packet]
  newBlock fms = do
    packetData <- newPacketData
    fillBlock 0 newPacket 0 packetData 0 fms
  fillBlock !prevPackets !curPacket !curPacketUsedBits !curPacketData !frameBitProgress restFrames = case restFrames of
    [] -> (:[]) <$> finalizePacket curPacket curPacketData
    frame : frames -> let
      -- number of bits left to store in this block
      remainingBlockBits = maxPacketBits - curPacketUsedBits
        + remainingBlockBitsAfterThisPacket
      remainingBlockBitsAfterThisPacket = maxPacketBits * (packetsPerBlock - prevPackets - 1)
      remainingPacketBits = maxPacketBits - curPacketUsedBits
      remainingFrameBits = xmaFrameBitLength frame - frameBitProgress
      -- can we start this frame in this packet?
      canStartThisPacket
        = xma2FrameCount curPacket < 0x3F -- not at the max limit of frames starting in one packet
        && curPacketUsedBits < maxPacketBits -- packet isn't full
        && remainingBlockBits >= remainingFrameBits -- we have enough bits in the block
      in if canStartThisPacket
        then let
          -- can we fit the whole frame into this packet?
          canFinishThisPacket = remainingPacketBits >= remainingFrameBits
          curPacket' = if frameBitProgress /= 0 then curPacket else curPacket
            { xma2FrameCount = xma2FrameCount curPacket + 1
            , xma2FrameOffsetInBits = case xma2FrameOffsetInBits curPacket of
              0x7FFF -> fromIntegral curPacketUsedBits
              offset -> offset
            }
          in do
            -- set previous trailer to 1
            when (curPacketUsedBits > 0) $ writeBit (curPacketUsedBits - 1) True curPacketData
            if canFinishThisPacket
              then do
                copyBits remainingFrameBits
                  (frameBitProgress, xmaFrameContent frame)
                  (curPacketUsedBits, curPacketData)
                -- set this trailer to 0 (may be changed to 1 later)
                writeBit (curPacketUsedBits + remainingFrameBits - 1) False curPacketData
                fillBlock prevPackets curPacket'
                  (curPacketUsedBits + remainingFrameBits)
                  curPacketData 0 frames
              else do
                copyBits remainingPacketBits
                  (frameBitProgress, xmaFrameContent frame)
                  (curPacketUsedBits, curPacketData)
                packet <- finalizePacket curPacket' curPacketData
                (packet :) <$> do
                  packetData <- newPacketData
                  fillBlock (prevPackets + 1) newPacket 0 packetData
                    (frameBitProgress + remainingPacketBits)
                    restFrames
        else let
          -- can we start this frame in the next packet, but in the same block?
          canStartNextPacket = remainingBlockBitsAfterThisPacket >= xmaFrameBitLength frame
          in if canStartNextPacket
            -- start a new packet in the same block
            then do
              packet <- finalizePacket curPacket curPacketData
              (packet :) <$> do
                packetData <- newPacketData
                fillBlock (prevPackets + 1) newPacket 0 packetData frameBitProgress restFrames
            -- start a new block
            else let
              emptyPackets = replicate (packetsPerBlock - prevPackets - 1) emptyPacket
              curPacket' = curPacket { xma2PacketSkipCount = fromIntegral $ length emptyPackets }
              in do
                packet <- finalizePacket curPacket' curPacketData
                ((packet : emptyPackets) <>) <$> newBlock restFrames
  copyBits :: Int -> (Int, B.ByteString) -> (Int, STUArray s Int Word8) -> ST s ()
  copyBits numBits (srcPos, src) (destPos, dest) = do
    forM_ [0 .. numBits - 1] $ \i -> do
      writeBit (destPos + i) (getStringBit src $ srcPos + i) dest
  writeBit :: Int -> Bool -> STUArray s Int Word8 -> ST s ()
  writeBit i b arr = case quotRem i 8 of
    (x, y) -> do
      byte <- readArray arr x
      let byte' = if b then byte `setBit` (7 - y) else byte `clearBit` (7 - y)
      writeArray arr x byte'
  finalizePacket :: XMA2Packet -> STUArray s Int Word8 -> ST s XMA2Packet
  finalizePacket packet packetData = do
    bs <- B.pack <$> getElems packetData
    bs `seq` return packet
      { xma2PacketData = bs
      }

xma1To2 :: (MonadFail m) => XMA1Contents -> m XMA2Contents
xma1To2 xma1 = do
  packets <- splitXMA1Packets $ xma1Data xma1
  let frames = splitXMAFrames $ map Left packets
      newPackets = buildXMA2Stream 16 frames
  return XMA2Contents
    { xma2Channels = xma1Channels xma1
    , xma2Rate     = xma1Rate xma1
    , xma2Samples  = length frames * 512
    , xma2PacketsPerBlock = 16
    , xma2SeekTable = Nothing
    , xma2Data     = writeXMA2Packets newPackets
    }

xma2To1 :: (MonadFail m) => XMA2Contents -> m (XMA1Contents, Word32)
xma2To1 xma2 = do
  packets <- splitXMA2Packets $ xma2Data xma2
  let newPackets = zipWith
        (\i pkt -> XMA1Packet
          { xma1SequenceNumber    = i
          , xma1Unk               = 2
          , xma1FrameOffsetInBits = xma2FrameOffsetInBits pkt
          , xma1PacketSkipCount   = xma2PacketSkipCount   pkt
          , xma1PacketData        = xma2PacketData        pkt
          }
        )
        (cycle [0..15])
        packets
      makeSeekTable _    []           = []
      makeSeekTable !cur (pkt : pkts) = (cur * 512) : makeSeekTable (cur + xma2FrameCount pkt) pkts
  return (XMA1Contents
    { xma1Channels  = xma2Channels xma2
    , xma1Rate      = xma2Rate xma2
    , xma1SeekTable = makeSeekTable 0 packets
    , xma1Data      = writeXMA1Packets newPackets
    }, sum $ map xma2FrameCount packets)

{-

newtype XMAFrame = XMAFrame
  -- includes the length bits
  { xmaFrameContent :: VU.Vector Bool -- this uses 1 byte per bit I think. could improve
  } deriving (Show)

-- TODO these need to look at last bit of a frame to determine whether to continue in packet.
-- (0 or 0x7FFF frame length doesn't matter I think)
-- bit 1 = more frames in packet, 0 = skip to next packet

splitXMA1Frames :: [XMA1Packet] -> [(Int, XMAFrame)]
splitXMA1Frames = go 0 . concatMap (concatMap toBits . B.unpack . xma1PacketData) where
  toBits byte = map (byte `testBit`) [7, 6 .. 0]
  go _    []     = []
  go posn stream = let
    len = foldr (.|.) 0 $ do
      (i, b) <- zip [14, 13 .. 0] $ take 15 stream
      return $ if b then bit i else 0
    in case len of
      0 -> let
        jump = 2044 * 8 - rem posn (2044 * 8)
        in go (posn + jump) $ drop jump stream
      _ -> let
        (x, y) = splitAt len stream
        x' = XMAFrame (VU.fromList x)
        in seq x' $ (posn, x') : go (posn + len) y

splitXMA2Frames :: [XMA2Packet] -> [(Int, XMAFrame)]
splitXMA2Frames = go 0 . concatMap (concatMap toBits . B.unpack . xma2PacketData) where
  toBits byte = map (byte `testBit`) [7, 6 .. 0]
  go _    []     = []
  go posn stream = let
    len = foldr (.|.) 0 $ do
      (i, b) <- zip [14, 13 .. 0] $ take 15 stream
      return $ if b then bit i else 0
    in case len of
      0x7FFF -> let
        jump = 2044 * 8 - rem posn (2044 * 8)
        in go (posn + jump) $ drop jump stream
      _ -> let
        (x, y) = splitAt len stream
        x' = XMAFrame (VU.fromList x)
        in seq x' $ (posn, x') : go (posn + len) y

bitLength :: XMAFrame -> Int
bitLength frame = VU.length (xmaFrameContent frame)

debugPrintXMA1Stream :: Int -> BL.ByteString -> IO ()
debugPrintXMA1Stream byteOffset stream = do
  packets <- splitXMA1Packets stream
  let printPosition bytes bits = do
        putStrLn $ "Position 0x" <> showHex (bytes :: Int) "" <> " and " <> show (bits :: Int) <> " bits"
      frames = splitXMA1Frames packets
      printPacket (packetNumber, packet) = do
        let packetLocation = byteOffset + packetNumber * 2048
            packetBitStart = packetNumber * 2044 * 8
            nextPacketBitStart = packetBitStart + 2044 * 8
            framesStartingHere = takeWhile ((< nextPacketBitStart) . fst) $ dropWhile ((< packetBitStart) . fst) frames
        printPosition packetLocation 0
        putStrLn $ "Packet " <> show (packetNumber :: Int)
        print packet { xma1PacketData = mempty }
        forM_ framesStartingHere $ \(frameBitOffset, frame) -> do
          let frameLocationBits = (packetLocation + 4) * 8 + (frameBitOffset - packetBitStart)
              (bytes, bits) = quotRem frameLocationBits 8
          printPosition bytes bits
          putStrLn $ "Frame of length " <> show (bitLength frame)
            <> " (" <> show (nextPacketBitStart - frameBitOffset) <> " bits left in packet)"
          VU.forM_ (xmaFrameContent frame) $ \x -> putStr $ if x then "1" else "0"
          putStr "\n"
  mapM_ printPacket $ zip [0..] packets

debugPrintXMA2Stream :: Int -> Int -> BL.ByteString -> IO ()
debugPrintXMA2Stream byteOffset packetsPerBlock stream = do
  packets <- markXMA2PacketStreams <$> splitXMA2Packets stream
  let blocks = zip [0..] $ chunksOf packetsPerBlock packets
      printPosition bytes bits = do
        putStrLn $ "Position 0x" <> showHex (bytes :: Int) "" <> " and " <> show (bits :: Int) <> " bits"
      printBlock (blockNumber, blockPackets) = do
        putStrLn $ "Start Block " <> show (blockNumber :: Int)
        let streamData = do
              streamIndex <- nubOrd $ map fst blockPackets
              let blocksData = map snd $ filter ((== streamIndex) . fst) blockPackets
                  blocksFrames = splitXMA2Frames blocksData
              return (streamIndex, blocksFrames)
            firstPacketIndex = blockNumber * packetsPerBlock
        forM_ (zip [firstPacketIndex ..] blockPackets) $ \(packetNumber, (streamIndex, packet)) -> do
          let packetLocation = byteOffset + packetNumber * 2048
              thisStreamPacketNumbers = map fst $ filter ((== streamIndex) . fst . snd) $ zip [firstPacketIndex..] blockPackets
              -- the number of packets for this stream in this block before this packet
              streamPacketIndex = length $ filter (< packetNumber) thisStreamPacketNumbers
              allFrames = fromMaybe [] $ lookup streamIndex streamData
              packetBitStart = streamPacketIndex * 2044 * 8
              nextPacketBitStart = packetBitStart + 2044 * 8
              framesStartingHere = takeWhile ((< nextPacketBitStart) . fst) $ dropWhile ((< packetBitStart) . fst) allFrames
          printPosition packetLocation 0
          putStrLn $ "Packet " <> show packetNumber <> " for stream " <> show streamIndex
          print packet { xma2PacketData = mempty }
          -- only show stream 0's packets for now
          when (streamIndex == 0) $ forM_ framesStartingHere $ \(frameBitOffsetInBlock, frame) -> do
            let frameLocationBits = (packetLocation + 4) * 8 + (frameBitOffsetInBlock - packetBitStart)
                (bytes, bits) = quotRem frameLocationBits 8
            printPosition bytes bits
            putStrLn $ "Frame of length " <> show (bitLength frame)
              <> " (" <> show (nextPacketBitStart - frameBitOffsetInBlock) <> " bits left in packet)"
            VU.forM_ (xmaFrameContent frame) $ \x -> putStr $ if x then "1" else "0"
            putStr "\n"
  mapM_ printBlock blocks

-}
