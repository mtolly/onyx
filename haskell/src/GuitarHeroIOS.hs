{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
          jumpToData = BL.fromStrict $ B.drop (fromIntegral offset) bs
          compressed = runGet (getWord16be >>= getLazyByteString . fromIntegral) jumpToData
          uncompressed = Raw.decompress compressed
          in do
            -- print (name, BL.length uncompressed, len, compression)
            name' <- if BL.length uncompressed /= fromIntegral len
              then do
                -- putStrLn "Size mismatch!"
                return $ name <> ".truncated"
              else return name
            return (name', uncompressed)
  contents <- sequence $ zipWith sliceFile names $ igaFiles header
  return (header, contents)

{-
-- decompresses raw deflate stream, but ignores "input ended prematurely" error
rawTruncate :: BL.ByteString -> BL.ByteString
rawTruncate bs = runST $ let
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
      error $ ".iga deflate decompression error: " <> show err
  in go (BL.toChunks bs) $ Z.decompressST Z.rawFormat Z.defaultDecompressParams
-}

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
