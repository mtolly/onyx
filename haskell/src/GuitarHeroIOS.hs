{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module GuitarHeroIOS where

import qualified Codec.Compression.Zlib.Raw as Raw
import           Control.Monad              (forM, replicateM)
import           Data.Binary.Get
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as BL
import           Data.Word
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            (dropExtension, takeDirectory,
                                             (-<.>), (</>))

{-
Most of the .iga format is the same as described in http://wiki.xentax.com/index.php/Madagascar:_Escape_2_Africa
The one difference is where it says "Padding (-1)", in GH files that's only -1 for the .track files.
All other files are compressed via raw DEFLATE format, and they have a zero or positive number in that location.

To decompress a file, jump to its location and perform the following:

1. read 2 bytes, big endian
2. read that many bytes, decompress into data via DEFLATE
3. if you need more bytes to get to the stated uncompressed size,
  jump to the next 0x800-divisible location (after the data you read), and repeat

Other sources for later versions of the format (supporting LZMA instead of DEFLATE):
https://github.com/KillzXGaming/Switch-Toolbox/blob/e5bb9fa/File_Format_Library/FileFormats/CrashBandicoot/IGA_PAK.cs
http://aluigi.altervista.org/bms/marvel_ultimate_alliance_2.bms
-}

data IGAHeader = IGAHeader
  { igaMagic                   :: Word32 -- 49 47 41 1A, but little endian
  , igaVersion                 :: Word32 -- xentax says 2 but GH uses 4
  , igaDirectoryLength         :: Word32
  , igaFileCount               :: Word32
  , igaUnk1                    :: Word32
  , igaUnk2                    :: Word32
  , igaFilenameDirectoryOffset :: Word32
  , igaFilenameDirectoryLength :: Word32
  , igaUnk3                    :: B.ByteString -- 16 bytes, most 0
  , igaHashes                  :: [Word32]
  , igaFiles                   :: [(Word32, Word32, Word32)] -- offset, length, unknown
  } deriving (Show)

getIGAHeader :: Get IGAHeader
getIGAHeader = do
  igaMagic <- getWord32le
  igaVersion <- getWord32le
  igaDirectoryLength <- getWord32le
  igaFileCount <- getWord32le
  igaUnk1 <- getWord32le
  igaUnk2 <- getWord32le
  igaFilenameDirectoryOffset <- getWord32le
  igaFilenameDirectoryLength <- getWord32le
  igaUnk3 <- getByteString 16
  igaHashes <- replicateM (fromIntegral igaFileCount) getWord32le
  igaFiles <- replicateM (fromIntegral igaFileCount) $ do
    fileOffset <- getWord32le
    fileLength <- getWord32le
    fileUnk <- getWord32le
    return (fileOffset, fileLength, fileUnk)
  return IGAHeader{..}

loadIGA :: FilePath -> IO (IGAHeader, [(B.ByteString, BL.ByteString)])
loadIGA f = do
  bs <- B.readFile f
  -- TODO handle Get errors
  let header = runGet getIGAHeader $ BL.fromStrict bs
      namesSection
        = B.take (fromIntegral $ igaFilenameDirectoryLength header)
        $ B.drop (fromIntegral $ igaFilenameDirectoryOffset header) bs
      nameOffsets = runGet (replicateM (fromIntegral $ igaFileCount header) getWord32le)
        $ BL.fromStrict namesSection
      names = flip map nameOffsets $ \offset ->
        B.takeWhile (/= 0) $ B.drop (fromIntegral offset) namesSection
      sliceFile name (offset, len, compression) = if compression == -1
        then return (name, BL.fromStrict $ B.take (fromIntegral len) $ B.drop (fromIntegral offset) bs)
        else let
          decodeFrom offset' len' = print (name, offset', len', compression) >> let
            jumpToData = BL.fromStrict $ B.drop (fromIntegral offset') bs
            compressedLength = runGet getWord16be jumpToData
            compressed = runGet (skip 2 >> getLazyByteString (fromIntegral compressedLength)) jumpToData
            uncompressed = Raw.decompress compressed
            gotFromThisBlock = fromIntegral $ BL.length uncompressed
            in case compare gotFromThisBlock (fromIntegral len') of
              EQ -> do
                putStrLn "File complete!"
                return uncompressed
              LT -> do
                -- we jump to the next 0x800 boundary after the data we uncompressed
                let newOffset = offset' + case 2 + fromIntegral compressedLength of
                      0x800 -> 0x800
                      x     -> (quot x 0x800 + 1) * 0x800
                (uncompressed <>) <$> decodeFrom newOffset (len' - gotFromThisBlock)
              GT -> do
                let totalDecoded = len + (gotFromThisBlock - len')
                putStrLn $ unwords
                  [ "Size mismatch! Expected"
                  , show len
                  , "bytes but uncompressed"
                  , show totalDecoded
                  , "for file"
                  , show name
                  ]
                return uncompressed
          in (name,) <$> decodeFrom offset len
  contents <- sequence $ zipWith sliceFile names $ igaFiles header
  return (header, contents)

extractIGA :: FilePath -> IO [FilePath]
extractIGA f = do
  (header, files) <- loadIGA f
  let headerTxt = f -<.> "txt"
  writeFile headerTxt $ unlines
    [ show header
    , show $ zip3 (map fst files) (igaHashes header) (igaFiles header)
    ]
  extracted <- forM files $ \(name, contents) -> do
    let out = dropExtension f </> B8.unpack name
    createDirectoryIfMissing True $ takeDirectory out
    BL.writeFile out contents
    return out
  return $ headerTxt : extracted
