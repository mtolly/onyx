{-
PS2 ISOs are standard ISO 9660 format, no extensions required.
(There is also UDF info in DVD images but we don't need to parse it.)
But, also supports dual layer ISOs (RB1/RB2, probably later GHs).

Mostly implemented using the ECMA-119 version of the spec:
https://www.ecma-international.org/wp-content/uploads/ECMA-119_4th_edition_june_2019.pdf

Also supports .bin/.cue images (PS1 and PS2 CDs). These contain the same
ISO 9660 filesystem, but each 2048-byte logical sector is stored inside a raw
sector (2352 bytes for MODE1/2352 and MODE2/2352) along with sync, header,
subheader and error correction data. Files marked in their CD-XA attributes
as Form 2 or interleaved (XA audio, STR video) can't be represented as 2048
bytes per sector, so those are extracted as 2336 bytes per sector
(subheader + data + EDC/ECC), the same as dumpsxiso/jPSXdec expect.
-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.ISO where

import           Control.Monad            (forM, guard, unless, when)
import           Data.Bifunctor           (bimap)
import           Data.Binary.Get
import           Data.Bits                (shiftL, testBit, (.&.), (.|.))
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (isSpace, toLower, toUpper)
import           Data.Functor             ((<&>))
import           Data.IORef
import           Data.List                (find)
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Word
import           Onyx.Util.Binary         (runGetM)
import           Onyx.Util.Handle         (Folder (..), Readable (..),
                                           SimpleHandle (..), fileReadable,
                                           fromFiles, handleLabel,
                                           openSimpleHandle, subHandle,
                                           useHandle)
import qualified System.Directory         as Dir
import           System.FilePath          (takeDirectory, takeFileName, (</>))
import           System.IO                (Handle, SeekMode (..), hClose,
                                           hFileSize, hSeek)

data PrimaryVolumeDescriptor = PrimaryVolumeDescriptor
  -- byte 1, string "CD001"
  { version                        :: Word8 -- 1
  -- byte 0
  , systemIdentifier               :: B.ByteString -- "PLAYSTATION"
  , volumeIdentifier               :: B.ByteString -- "BAND_S" for RB2
  -- 8 bytes of 0
  , volumeSpaceSize                :: Word32
  -- 32 bytes of 0
  , volumeSetSize                  :: Word16
  , volumeSequenceNumber           :: Word16
  , logicalBlockSize               :: Word16 -- 0x800; logical sectors and logical blocks appear to be the same for us
  , pathTableSize                  :: Word32
  , locationTypeLPathTable         :: Word32
  , locationOptionalTypeLPathTable :: Word32
  , locationTypeMPathTable         :: Word32
  , locationOptionalTypeMPathTable :: Word32
  , rootDirectoryRecord            :: DirectoryRecord
  , volumeSetIdentifier            :: B.ByteString
  , publisherIdentifier            :: B.ByteString
  , dataPreparerIdentifier         :: B.ByteString
  , applicationIdentifier          :: B.ByteString
  , copyrightFileIdentifier        :: B.ByteString
  , abstractFileIdentifier         :: B.ByteString
  , bibliographicFileIdentifier    :: B.ByteString
  , volumeCreationDateTime         :: B.ByteString
  , volumeModificationDateTime     :: B.ByteString
  , volumeExpirationDateTime       :: B.ByteString
  , volumeEffectiveDateTime        :: B.ByteString
  , fileStructureVersion           :: Word8
  -- rest is reserved or application use
  } deriving (Show)

data DirectoryRecord = DirectoryRecord
  { extendedAttributeRecordLength :: Word8
  , locationOfExtent              :: Word32
  , dataLength                    :: Word32
  , recordingDateTime             :: B.ByteString -- 7 bytes
  , fileFlags                     :: Word8
  , fileUnitSize                  :: Word8
  , interleaveGapSize             :: Word8
  , volumeSequenceNumber          :: Word16
  , fileIdentifier                :: B.ByteString
  , systemUse                     :: B.ByteString -- CD-XA info on PlayStation discs
  } deriving (Show)

data PathTableRecord = PathTableRecord
  { extendedAttributeRecordLength :: Word8
  , locationOfExtent              :: Word32
  , parentDirectoryNumber         :: Word16
  , directoryIdentifier           :: B.ByteString
  } deriving (Show)

mirrored :: (Eq a, Show a) => Get a -> Get a -> Get a
mirrored getLE getBE = do
  x <- getLE
  y <- getBE
  unless (x == y) $ fail $ "Both-byte order values didn't match: " <> show x <> " != " <> show y
  return x

spacePadded :: B.ByteString -> B.ByteString
spacePadded = B.dropWhileEnd (== 0x20)

folderISO :: Readable -> IO (Folder B.ByteString Readable)
folderISO r = isISO r >>= \case
  False -> return mempty
  True  -> do
    (layer, nextLayer) <- folderISOLayer (const Nothing) r
    rest <- folderISO $ subHandle id nextLayer Nothing r
    return $ layer <> rest

isISO :: Readable -> IO Bool
isISO r = useHandle r \h -> do
  len <- hFileSize h
  if len < 0x8006
    then return False
    else do
      hSeek h AbsoluteSeek 0x8001
      bs <- B.hGet h 5
      return $ bs == "CD001"

-- | The first argument can supply a different handle for a file,
-- instead of the default of reading its extent from the given handle.
folderISOLayer :: (DirectoryRecord -> Maybe Readable) -> Readable -> IO (Folder B.ByteString Readable, Integer)
folderISOLayer special r = do
  (pvd, recs) <- useHandle r \h -> do
    hSeek h AbsoluteSeek 0x8000
    pvd <- B.hGet h 0x800 >>= runGetM getPrimaryVolumeDescriptor . BL.fromStrict
    hSeek h AbsoluteSeek $ fromIntegral pvd.locationTypeMPathTable * 0x800
    ptrs <- B.hGet h (fromIntegral pvd.pathTableSize) >>= runGetM (getMany getPathTableRecordBE) . BL.fromStrict
    recs <- fromFiles <$> getFileTree h ptrs
    return (pvd, recs)
  return (bimap
    (\name -> fromMaybe name $ B.stripSuffix ";1" name)
    (\dr -> fromMaybe
      (subHandle
        id -- TODO add label
        (fromIntegral dr.locationOfExtent * 0x800)
        (Just $ fromIntegral dr.dataLength)
        r
      )
      (special dr)
    )
    recs, fromIntegral pvd.volumeSpaceSize * 0x800 - 0x8000)

getFileTree :: Handle -> [PathTableRecord] -> IO [(NE.NonEmpty B.ByteString, DirectoryRecord)]
getFileTree h ptrs = do
  -- RB2 PS2 2nd layer appears to have root and GEN folder with same extent number.
  -- Map.fromList keeps the last entry for each extent number
  let realPTRs = Map.elems $ Map.fromList [ (ptr.locationOfExtent, ptr) | ptr <- ptrs ]
      ptrToParents ptr = case ptr.directoryIdentifier of
        "\0"  -> []
        dirID -> dirID : ptrToParents (ptrs !! fromIntegral (ptr.parentDirectoryNumber - 1))
  fmap concat $ forM realPTRs \ptr -> do
    let parents = ptrToParents ptr
    recs <- getFolderFiles h ptr
    return $ recs <&> \rec -> let
      path = NE.reverse $ rec.fileIdentifier :| parents
      in (path, rec)

getFolderFiles :: Handle -> PathTableRecord -> IO [DirectoryRecord]
getFolderFiles h ptr = do
  hSeek h AbsoluteSeek $ fromIntegral ptr.locationOfExtent * 0x800
  firstSector <- B.hGet h 0x800
  -- the first record (".") tells us the size of the whole directory
  self <- runGetM getDirectoryRecord $ BL.fromStrict firstSector
  let sectorCount = maybe 1 (\dr -> max 1 $ (dr.dataLength + 0x7FF) `quot` 0x800) self
  moreSectors <- B.hGet h $ fromIntegral (sectorCount - 1) * 0x800
  -- records don't cross sector boundaries; each sector is padded with zeroes
  let splitSectors bs = if B.null bs then [] else B.take 0x800 bs : splitSectors (B.drop 0x800 bs)
  recs <- fmap concat $ forM (firstSector : splitSectors moreSectors) $
    runGetM (getMany getDirectoryRecord) . BL.fromStrict
  -- bit 1 is true if directory (including special . and .. entries)
  return $ filter (\rec -> not $ testBit rec.fileFlags 1) recs

getPrimaryVolumeDescriptor :: Get PrimaryVolumeDescriptor
getPrimaryVolumeDescriptor = do
  1                              <- getWord8
  "CD001"                        <- getByteString 5
  version                        <- getWord8
  skip 1
  systemIdentifier               <- spacePadded <$> getByteString 0x20
  volumeIdentifier               <- spacePadded <$> getByteString 0x20
  skip 8
  volumeSpaceSize                <- mirrored getWord32le getWord32be
  skip 32
  volumeSetSize                  <- mirrored getWord16le getWord16be
  volumeSequenceNumber           <- mirrored getWord16le getWord16be
  logicalBlockSize               <- mirrored getWord16le getWord16be
  pathTableSize                  <- mirrored getWord32le getWord32be
  locationTypeLPathTable         <- getWord32le
  locationOptionalTypeLPathTable <- getWord32le
  locationTypeMPathTable         <- getWord32be
  locationOptionalTypeMPathTable <- getWord32be
  Just rootDirectoryRecord       <- getDirectoryRecord
  volumeSetIdentifier            <- spacePadded <$> getByteString 0x80
  publisherIdentifier            <- spacePadded <$> getByteString 0x80
  dataPreparerIdentifier         <- spacePadded <$> getByteString 0x80
  applicationIdentifier          <- spacePadded <$> getByteString 0x80
  copyrightFileIdentifier        <- spacePadded <$> getByteString 37
  abstractFileIdentifier         <- spacePadded <$> getByteString 37
  bibliographicFileIdentifier    <- spacePadded <$> getByteString 37
  volumeCreationDateTime         <- getByteString 17
  volumeModificationDateTime     <- getByteString 17
  volumeExpirationDateTime       <- getByteString 17
  volumeEffectiveDateTime        <- getByteString 17
  fileStructureVersion           <- getWord8
  return PrimaryVolumeDescriptor{..}

getMany :: Get (Maybe a) -> Get [a]
getMany g = isEmpty >>= \case
  True  -> return []
  False -> g >>= \case
    Just x  -> (x :) <$> getMany g
    Nothing -> return []

getDirectoryRecord :: Get (Maybe DirectoryRecord)
getDirectoryRecord = lookAhead getWord8 >>= \case
  0   -> return Nothing
  len -> Just <$> do
    posn1                         <- bytesRead
    skip 1 -- len
    extendedAttributeRecordLength <- getWord8
    locationOfExtent              <- mirrored getWord32le getWord32be
    dataLength                    <- mirrored getWord32le getWord32be
    recordingDateTime             <- getByteString 7
    fileFlags                     <- getWord8
    fileUnitSize                  <- getWord8
    interleaveGapSize             <- getWord8
    volumeSequenceNumber          <- mirrored getWord16le getWord16be
    lenFileIdentifier             <- getWord8
    fileIdentifier                <- getByteString $ fromIntegral lenFileIdentifier
    when (even lenFileIdentifier) $ skip 1
    posn2                         <- bytesRead
    systemUse                     <- getByteString $ max 0 $ fromIntegral len - fromIntegral (posn2 - posn1)
    return DirectoryRecord{..}

getPathTableRecordBE :: Get (Maybe PathTableRecord)
getPathTableRecordBE = lookAhead getWord8 >>= \case
  0 -> return Nothing
  lenDI -> Just <$> do
    skip 1 -- lenDI
    extendedAttributeRecordLength <- getWord8
    locationOfExtent              <- getWord32be
    parentDirectoryNumber         <- getWord16be
    directoryIdentifier           <- getByteString $ fromIntegral lenDI
    when (odd lenDI) $ skip 1
    return PathTableRecord{..}

-- .bin/.cue support

data CueTrack = CueTrack
  { file   :: FilePath
  , number :: Int
  , mode   :: String -- uppercased, e.g. "MODE2/2352" or "AUDIO"
  } deriving (Show)

parseCue :: T.Text -> [CueTrack]
parseCue = go Nothing . map (cueWords . T.unpack) . T.lines where
  go _ [] = []
  go curFile (line : rest) = case line of
    cmd : name : _ | map toUpper cmd == "FILE" -> go (Just name) rest
    cmd : num : trackMode : _ | map toUpper cmd == "TRACK", Just f <- curFile, [(n, "")] <- reads num
      -> CueTrack { file = f, number = n, mode = map toUpper trackMode } : go curFile rest
    _ -> go curFile rest
  cueWords s = case dropWhile isSpace s of
    ""         -> []
    '"' : str  -> let (w, after) = break (== '"') str in w : cueWords (drop 1 after)
    str        -> let (w, after) = break isSpace str in w : cueWords after

-- | Returns (raw sector size, offset of 2048 bytes of user data within sector)
cueSectorLayout :: (MonadFail m) => String -> m (Integer, Integer)
cueSectorLayout = \case
  "MODE1/2048" -> return (2048, 0)
  "MODE1/2352" -> return (2352, 16)
  "MODE2/2352" -> return (2352, 24)
  "MODE2/2336" -> return (2336, 8)
  m            -> fail $ "Unsupported cue track mode: " <> m

-- | Given raw sector size, and the offset and size of a slice of each sector,
-- presents those slices as one contiguous stream.
-- With a 2048-byte slice of user data, this can be read like a normal .iso.
sectorSlices :: Integer -> Integer -> Integer -> Readable -> Readable
sectorSlices sectorSize 0           sliceSize r | sectorSize == sliceSize = r
sectorSlices sectorSize sliceOffset sliceSize r = Readable
  { rFilePath = Nothing
  , rOpen = do
    h <- rOpen r
    rawSize <- hFileSize h
    posn <- newIORef 0
    let size = (rawSize `quot` sectorSize) * sliceSize
    openSimpleHandle (handleLabel h <> " | sector slices") SimpleHandle
      { shSize  = size
      , shSeek  = writeIORef posn
      , shTell  = readIORef posn
      , shClose = hClose h
      , shRead  = \n -> do
        p <- readIORef posn
        let n' = max 0 $ min n $ size - p
        if n' == 0
          then return B.empty
          else do
            let firstSector = p `quot` sliceSize
                lastSector = (p + n' - 1) `quot` sliceSize
                sectorCount = lastSector - firstSector + 1
            hSeek h AbsoluteSeek $ firstSector * sectorSize
            raw <- B.hGet h $ fromIntegral $ sectorCount * sectorSize
            let slices = B.concat
                  [ B.take (fromIntegral sliceSize) $ B.drop (fromIntegral $ i * sectorSize + sliceOffset) raw
                  | i <- [0 .. sectorCount - 1]
                  ]
                result = B.take (fromIntegral n') $ B.drop (fromIntegral $ p `rem` sliceSize) slices
            writeIORef posn $ p + fromIntegral (B.length result)
            return result
      }
  }

-- | CD-XA attributes from the system use area of a directory record, if present.
xaAttributes :: DirectoryRecord -> Maybe Word16
xaAttributes dr = do
  let su = dr.systemUse
  guard $ B.length su >= 8 && B.take 2 (B.drop 6 su) == "XA"
  return $ (fromIntegral (B.index su 4) `shiftL` 8) .|. fromIntegral (B.index su 5)

folderCueBin :: FilePath -> IO (Folder B.ByteString Readable)
folderCueBin cue = do
  tracks <- parseCue . TE.decodeUtf8With lenientDecode . dropBOM <$> B.readFile cue
  track <- case filter (\t -> t.mode /= "AUDIO") tracks of
    t : _ -> return t
    []    -> fail $ "No data track found in cue sheet: " <> cue
  (sectorSize, dataOffset) <- cueSectorLayout track.mode
  bin <- findCueFile (takeDirectory cue) track.file
  let binReadable = fileReadable bin
      cooked = sectorSlices sectorSize dataOffset 0x800 binReadable
      -- subheader (8 bytes) through end of sector
      mode2Raw = sectorSlices sectorSize (dataOffset - 8) 2336 binReadable
      -- 0x1000 = Form 2, 0x2000 = interleaved (mixed Form 1 and 2 sectors)
      special dr = do
        guard $ dataOffset >= 8
        attrs <- xaAttributes dr
        guard $ attrs .&. 0x3000 /= 0
        let sectors = (fromIntegral dr.dataLength + 0x7FF) `quot` 0x800
        return $ subHandle id
          (fromIntegral dr.locationOfExtent * 2336)
          (Just $ sectors * 2336)
          mode2Raw
  isISO cooked >>= \case
    False -> fail $ "Data track does not contain an ISO 9660 filesystem: " <> bin
    True  -> fst <$> folderISOLayer special cooked
  where dropBOM bs = fromMaybe bs $ B.stripPrefix "\xEF\xBB\xBF" bs

-- | Finds a file referenced by a cue sheet, falling back to a case-insensitive search.
findCueFile :: FilePath -> FilePath -> IO FilePath
findCueFile dir name = do
  let path = dir </> name
  Dir.doesFileExist path >>= \case
    True  -> return path
    False -> do
      let base = takeFileName name
      ents <- Dir.listDirectory dir
      case find (\ent -> map toLower ent == map toLower base) ents of
        Just ent -> return $ dir </> ent
        Nothing  -> fail $ "Couldn't find file referenced by cue sheet: " <> path
