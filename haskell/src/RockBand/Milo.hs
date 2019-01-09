{-
Thanks to PyMilo, LibForge, and MiloMod for information on these structures.
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RockBand.Milo where

import qualified Codec.Compression.GZip           as GZ
import qualified Codec.Compression.Zlib.Internal  as Z
import           Control.Monad                    (forM, replicateM)
import           Control.Monad.ST.Lazy
import           Data.Binary.Get
import           Data.Binary.IEEE754              (getFloat32be)
import           Data.Bits
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Word
import qualified RockBand.Codec.File              as RBFile
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U

data MiloCompression
  = MILO_A
  | MILO_B
  | MILO_C
  | MILO_D
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- decompresses zlib stream, but ignores "input ended prematurely" error
zlibTruncate :: BL.ByteString -> BL.ByteString
zlibTruncate bs = runST $ let
  go input = \case
    Z.DecompressInputRequired f              -> case input of
      []     -> f B.empty >>= go []
      x : xs -> f x       >>= go xs
    Z.DecompressOutputAvailable out getNext  -> do
      next <- getNext
      (BL.fromStrict out <>) <$> go input next
    Z.DecompressStreamEnd _unread            -> return BL.empty
    Z.DecompressStreamError Z.TruncatedInput -> return BL.empty
    Z.DecompressStreamError err              ->
      error $ "Milo Zlib decompression error: " <> show err
  in go (BL.toChunks bs) $ Z.decompressST Z.zlibFormat Z.defaultDecompressParams

decompressBlock :: MiloCompression -> BL.ByteString -> BL.ByteString
decompressBlock comp bs = case comp of
  MILO_A -> bs
  MILO_B -> zlibTruncate $ zlib_info <> bs
  MILO_C -> GZ.decompress bs
  MILO_D -> zlibTruncate $ zlib_info <> BL.drop 4 (BL.take (BL.length bs - 1) bs)
  where zlib_info = BL.pack [0x78, 0x9C]

decompressMilo :: Get BL.ByteString
decompressMilo = do
  startingOffset <- bytesRead
  comp <- getWord32le >>= \case
    0xCABEDEAF -> return MILO_A
    0xCBBEDEAF -> return MILO_B
    0xCCBEDEAF -> return MILO_C
    0xCDBEDEAF -> return MILO_D
    n          -> fail $ "Unrecognized .milo compression: " <> show n
  offset <- getWord32le
  blockCount <- getWord32le
  _largestBlock <- getWord32le -- max uncompressed size
  let maxSize = 1 `shiftL` 24
  blockInfo <- replicateM (fromIntegral blockCount) $ do
    size <- getWord32le
    let (compressed, size') = case comp of
          MILO_A -> (False, size)
          MILO_D ->
            ( size .&. maxSize == 0
            , size .&. complement maxSize
            )
          _      -> (True, size)
    return (size', compressed)
  posn <- bytesRead
  skip $ fromIntegral offset - fromIntegral (posn - startingOffset)
  fmap BL.concat $ forM blockInfo $ \(size, compressed) -> do
    bs <- getLazyByteString $ fromIntegral size
    return $ if compressed then decompressBlock comp bs else bs

data Lipsync = Lipsync
  { lipsyncVersion    :: Word32
  , lipsyncSubversion :: Word32
  , lipsyncDTAImport  :: B.ByteString
  , lipsyncVisemes    :: [B.ByteString]
  , lipsyncKeyframes  :: [Keyframe]
  } deriving (Eq, Show)

newtype Keyframe = Keyframe
  { keyframeEvents :: [VisemeEvent]
  } deriving (Eq, Show)

data VisemeEvent = VisemeEvent
  { visemeIndex  :: Int
  , visemeWeight :: Word8
  } deriving (Eq, Show)

stringBE :: Get B.ByteString
stringBE = do
  len <- getWord32be
  getByteString $ fromIntegral len

parseLipsync :: Get Lipsync
parseLipsync = do
  lipsyncVersion <- getWord32be
  lipsyncSubversion <- getWord32be
  lipsyncDTAImport <- stringBE
  dtb <- getWord8
  case dtb of
    0 -> return ()
    _ -> fail "Parsing of Lipsync files with embedded DTB is not currently supported"
  skip 4 -- skips zeroes
  visemeCount <- getWord32be
  lipsyncVisemes <- replicateM (fromIntegral visemeCount) stringBE
  keyframeCount <- getWord32be
  _followingSize <- getWord32be
  lipsyncKeyframes <- replicateM (fromIntegral keyframeCount) $ do
    eventCount <- getWord8
    keyframeEvents <- replicateM (fromIntegral eventCount) $ do
      visemeIndex <- fromIntegral <$> getWord8
      visemeWeight <- getWord8
      return VisemeEvent{..}
    return Keyframe{..}
  return Lipsync{..}

lipsyncToMIDI :: U.TempoMap -> U.MeasureMap -> Lipsync -> RBFile.Song (RBFile.RawFile U.Beats)
lipsyncToMIDI tmap mmap lip = RBFile.Song tmap mmap $ RBFile.RawFile $ (:[])
  $ U.setTrackName "LIPSYNC"
  $ U.unapplyTempoTrack tmap
  $ RTB.flatten
  $ RTB.fromPairList
  $ do
    (dt, key) <- zip (0 : repeat (1/30 :: U.Seconds)) $ lipsyncKeyframes lip
    let evts = do
          vis <- keyframeEvents key
          let str = "[viseme " <> B8.unpack (lipsyncVisemes lip !! visemeIndex vis) <> " " <> show (visemeWeight vis) <> "]"
          return $ E.MetaEvent $ Meta.TextEvent str
    return (dt, evts)

data Venue = Venue
  { venueVersion    :: Word32
  , venueSubversion :: Word32
  , venueDTAImport  :: B.ByteString
  , venueMystery    :: B.ByteString
  , venueTracks     :: [Track]
  } deriving (Eq, Show)

data Track = Track
  { trackVersion    :: Word32
  , trackSubversion :: Word32
  , trackDomain     :: B.ByteString
  , trackMystery    :: B.ByteString
  , trackName       :: B.ByteString
  , trackMystery2   :: Word32
  , trackName2      :: B.ByteString
  , trackMystery3   :: B.ByteString
  , trackEvents     :: ATB.T U.Seconds B.ByteString
  } deriving (Eq, Show)

data VenueEvent = VenueEvent
  { venueEvent :: B.ByteString
  , venueTime  :: U.Seconds
  } deriving (Eq, Show)

parseVenue :: Get Venue
parseVenue = do
  venueVersion <- getWord32be -- 0xD
  venueSubversion <- getWord32be -- 0x2
  venueDTAImport <- stringBE -- "song_anim"
  venueMystery <- getByteString 17
    {-
      00
      00 00 00 00
      00 00 00 04
      46 6D F5 79 -- probably end timestamp
      00 00 00 01
    -}
  trackCount <- getWord32be
  venueTracks <- replicateM (fromIntegral trackCount) $ do
    trackVersion <- getWord32be -- usually 6, 2 in postproc track
    trackSubversion <- getWord32be -- usually 6, 2 in postproc track
    trackDomain <- stringBE -- "BandDirector"
    trackMystery <- getByteString 11 -- 01 00 01 00 00 00 00 00 00 00 05
    trackName <- stringBE -- like "bass_intensity"
    trackMystery2 <- getWord32be
    trackName2 <- stringBE -- like "lightpreset_interp" but usually ""
    trackMystery3 <- getByteString 5
    eventCount <- getWord32be
    trackEvents <- fmap ATB.fromPairList $ replicateM (fromIntegral eventCount) $ do
      event <- stringBE
      -- see "postproc" track where each event has 4 extra bytes of 0
      event' <- if B.null event then stringBE else return event
      frames <- getFloat32be
      return (realToFrac $ frames / 30, event')
    return Track{..}
  return Venue{..}

venueToMIDI :: U.TempoMap -> U.MeasureMap -> Venue -> RBFile.Song (RBFile.RawFile U.Beats)
venueToMIDI tmap mmap venue = RBFile.Song tmap mmap $ RBFile.RawFile $ do
  trk <- venueTracks venue
  return
    $ U.setTrackName (B8.unpack $ trackName trk)
    $ U.unapplyTempoTrack tmap
    $ RTB.fromAbsoluteEventList
    $ fmap (E.MetaEvent . Meta.TextEvent . B8.unpack)
    $ trackEvents trk

{-
testConvert :: FilePath -> FilePath -> FilePath -> IO ()
testConvert fmid fven fout = do
  res <- logStdout $ stackIO (Load.fromFile fmid) >>= RBFile.readMIDIFile'
  mid <- case res of
    Left err -> error $ show err
    Right mid -> return mid
  ven <- fmap (runGet parseVenue) $ BL.readFile fven
  let raw = venueToMIDI (RBFile.s_tempos mid) (RBFile.s_signatures mid) ven `asTypeOf` mid
  Save.toFile fout $ RBFile.showMIDIFile' raw
-}
