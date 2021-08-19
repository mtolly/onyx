{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Sound.FSB where

import           Control.Monad                  (forM, forM_, guard, replicateM)
import           Control.Monad.Trans.Resource   (MonadResource)
import           Control.Monad.Trans.StackTrace
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Conduit.Audio             as CA
import           Data.Int
import           Data.SimpleHandle
import           Data.Word
import           FFMPEG                         (ffSource)
import           GHC.IO.Handle                  (HandlePosn (..))
import           STFS.Package                   (runGetM)
import           System.FilePath                ((-<.>), (</>))
import qualified System.IO                      as IO

data FSBSong = FSBSong
  { fsbSongHeaderSize :: Word16
  , fsbSongName       :: B.ByteString
  , fsbSongSamples    :: Word32
  , fsbSongDataSize   :: Word32
  , fsbSongLoopStart  :: Word32
  , fsbSongLoopEnd    :: Word32
  , fsbSongMode       :: Word32
  , fsbSongSampleRate :: Word32
  , fsbSongDefVol     :: Word16
  , fsbSongDefPan     :: Word16
  , fsbSongDefPri     :: Word16
  , fsbSongChannels   :: Word16
  , fsbSongExtraData  :: B.ByteString
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
  pos2              <- bytesRead
  fsbSongExtraData  <- getByteString $
    fromIntegral fsbSongHeaderSize - fromIntegral (pos2 - pos1)
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
  putByteString fsbSongExtraData

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
songHeaderSize song = 64 + fromIntegral (B.length $ fsbSongExtraData song)

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
        , fsb3HeadersSize = sum $ map (fromIntegral . fsbSongHeaderSize) songs
        , fsb3DataSize    = sum $ map fsbSongDataSize songs
        , fsb3Songs       = songs
        }
    Right header4 -> let
      songs = fixSongs $ fsb4Songs header4
      in Right header4
        { fsb4SongCount   = fromIntegral $ length songs
        , fsb4HeadersSize = sum $ map (fromIntegral . fsbSongHeaderSize) songs
        , fsb4DataSize    = sum $ map fsbSongDataSize songs
        , fsb4Songs       = songs
        }
  in fsb { fsbHeader = header }

emitFSB :: FSB -> BL.ByteString
emitFSB fsb = runPut $ do
  let fsb' = fixFSB fsb
  either putFSB3Header putFSB4Header $ fsbHeader fsb'
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

splitXMA2Packets :: (MonadFail m) => BL.ByteString -> m [(Word32, Word32, Word32, Word32, B.ByteString)]
splitXMA2Packets = runGetM go where
  go = isEmpty >>= \case
    True  -> return []
    False -> do
      packetInfo <- getWord32be
      xmaData <- getByteString $ 2048 - 4
      let frameCount        = (packetInfo `shiftR` 26) .&. 0x3F
          frameOffsetInBits = (packetInfo `shiftR` 11) .&. 0x7FFF
          packetMetaData    = (packetInfo `shiftR` 8)  .&. 0x7
          packetSkipCount   = packetInfo               .&. 0xFF
          pkt = (frameCount, frameOffsetInBits, packetMetaData, packetSkipCount, xmaData)
      (pkt :) <$> go

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

buildXMAFile :: FilePath -> Int -> Int -> Int -> BL.ByteString -> BL.ByteString -> IO ()
buildXMAFile fp r c len xmaSeek xmaData = IO.withBinaryFile fp IO.WriteMode $ \h -> do
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

      write $ putWord16le $ (fromIntegral c + 1) `quot` 2             -- NumStreams;     // Number of audio streams (1 or 2 channels each)
      write $ putWord32le 0                                           -- ChannelMask;    // Spatial positions of the channels in this file
      write $ putWord32le $ fromIntegral len                          -- SamplesEncoded; // Total number of PCM samples the file decodes to
      write $ putWord32le 0x10000                                     -- BytesPerBlock;  // XMA block size (but the last one may be shorter)
      write $ putWord32le 0                                           -- PlayBegin;      // First valid sample in the decoded audio
      write $ putWord32le $ fromIntegral len                          -- PlayLength;     // Length of the valid part of the decoded audio
      write $ putWord32le 0                                           -- LoopBegin;      // Beginning of the loop region in decoded sample terms
      write $ putWord32le 0                                           -- LoopLength;     // Length of the loop region in decoded sample terms
      write $ putWord8 0                                              -- LoopCount;      // Number of loop repetitions; 255 = infinite
      write $ putWord8 4                                              -- EncoderVersion; // Version of XMA encoder that generated the file
      write $ putWord16le $ fromIntegral (BL.length xmaSeek) `quot` 4 -- BlockCount;     // XMA blocks in file (and entries in its seek table)

    chunk "seek" $ do
      BL.hPut h xmaSeek
    chunk "data" $ do
      BL.hPut h xmaData

fsbToXMAs :: FilePath -> FilePath -> IO ()
fsbToXMAs f dir = do
  fsb <- BL.readFile f >>= parseFSB
  forM_ (zip (either fsb3Songs fsb4Songs $ fsbHeader fsb) (fsbSongData fsb)) $ \(song, sdata) -> do
    buildXMAFile
      (dir </> B8.unpack (fsbSongName song) -<.> "xma")
      (fromIntegral $ fsbSongSampleRate song)
      (fromIntegral $ fsbSongChannels song)
      (fromIntegral $ fsbSongSamples song)
      (BL.fromStrict $ fsbSongExtraData song)
      sdata
