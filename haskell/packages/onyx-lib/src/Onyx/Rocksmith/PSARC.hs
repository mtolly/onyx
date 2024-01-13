{-

Mostly ported from
https://github.com/0x0L/rocksmith/blob/master/rocksmith/psarc.py

-}

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Onyx.Rocksmith.PSARC where

import qualified Codec.Compression.Zlib.Internal as Z
import           Control.Applicative             (liftA2)
import           Control.Monad                   (replicateM)
import           Control.Monad.ST.Lazy           (runST)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BL
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           Onyx.Codec.Binary
import           Onyx.Rocksmith.Crypt
import           Onyx.Util.Binary                (runGetM)
import qualified Onyx.Util.Handle                as SH
import           System.IO                       (SeekMode (..), hSeek)

data Header = Header
  { hdr_magic        :: B.ByteString
  , hdr_versionMajor :: Word16
  , hdr_versionMinor :: Word16
  , hdr_compression  :: B.ByteString
  , hdr_tocLength    :: Word32
  , hdr_tocEntrySize :: Word32
  , hdr_tocEntries   :: Word32
  , hdr_blockSize    :: Word32
  , hdr_archiveFlags :: Word32
  } deriving (Show)

instance Bin Header where
  bin = do
    hdr_magic        <- hdr_magic        =. byteString 4
    hdr_versionMajor <- hdr_versionMajor =. word16be
    hdr_versionMinor <- hdr_versionMinor =. word16be
    hdr_compression  <- hdr_compression  =. byteString 4
    hdr_tocLength    <- hdr_tocLength    =. word32be
    hdr_tocEntrySize <- hdr_tocEntrySize =. word32be
    hdr_tocEntries   <- hdr_tocEntries   =. word32be
    hdr_blockSize    <- hdr_blockSize    =. word32be
    hdr_archiveFlags <- hdr_archiveFlags =. word32be
    return Header{..}

data TOCEntry = TOCEntry
  { toc_MD5    :: B.ByteString
  , toc_zIndex :: Word32
  , toc_length :: Word64 -- actually 40 bits
  , toc_offset :: Word64 -- actually 40 bits
  } deriving (Show)

instance Bin TOCEntry where
  bin = do
    toc_MD5    <- toc_MD5    =. byteString 16
    toc_zIndex <- toc_zIndex =. word32be
    toc_length <- toc_length =. word40be
    toc_offset <- toc_offset =. word40be
    return TOCEntry{..}

word40be :: BinaryCodec Word64
word40be = Codec
  { codecOut = undefined -- TODO
  , codecIn = do
    a <- getWord8
    b <- getWord32be
    return $ fromIntegral a * 0x100000000 + fromIntegral b
  }

extractPSARC :: FilePath -> FilePath -> IO ()
extractPSARC fin dout = do
  folder <- readPSARCFolder $ SH.fileReadable fin
  SH.saveHandleFolder folder dout

readPSARCFolder :: SH.Readable -> IO (SH.Folder T.Text SH.Readable)
readPSARCFolder readable = do
  (hdr, tocBytes, origLabel) <- SH.useHandle readable $ \h -> do
    hdr <- BL.hGet h 32 >>= runGetM (codecIn (bin :: BinaryCodec Header))
    tocEnc <- B.hGet h $ fromIntegral (hdr_tocLength hdr) - 32
    toc <- decryptPSARCTable' tocEnc
    return (hdr, toc, SH.handleLabel h)
  (toc, zlengths) <- return $ flip runGet (BL.fromStrict tocBytes) $ do
    toc <- replicateM (fromIntegral $ hdr_tocEntries hdr) $ do
      codecIn (bin :: BinaryCodec TOCEntry)
    let getZLengths = isEmpty >>= \case
          True  -> return []
          False -> liftA2 (:) getWord16be getZLengths
    zlengths <- getZLengths
    return (toc, zlengths)
  let entryReaders = flip map toc $ \entry -> SH.useHandle readable $ \h -> do
        hSeek h AbsoluteSeek $ fromIntegral $ toc_offset entry
        let readEntry' []       _   = return BL.empty
            readEntry' (z : zs) len = if len == toc_length entry
              then return BL.empty
              else do
                let defaultBlockSize = 0x10000
                chunk <- BL.hGet h $ if z == 0 then defaultBlockSize else z
                let chunk' = fromMaybe chunk $ zlibMaybe chunk
                (chunk' <>) <$> readEntry' zs (len + fromIntegral (BL.length chunk'))
            lens = map fromIntegral $ drop (fromIntegral $ toc_zIndex entry) zlengths
        readEntry' lens 0
  (listingReader, fileReaders) <- case entryReaders of
    []     -> fail "No file listing found in PSARC"
    x : xs -> return (x, xs)
  listing <- listingReader
  let filenames = T.lines $ TE.decodeLatin1 $ BL.toStrict listing
      folder = SH.fromFiles $ do
        (path, fileReader) <- zip filenames fileReaders
        Just spath <- [SH.splitPath path] -- error if Nothing?
        let newLabel = origLabel <> " | " <> T.unpack path
        return (spath, SH.makeHandle newLabel $ fileReader >>= SH.byteStringSimpleHandle)
  return folder

zlibMaybe :: BL.ByteString -> Maybe BL.ByteString
zlibMaybe bs = runST $ let
  go input = \case
    Z.DecompressInputRequired f              -> case input of
      []     -> f B.empty >>= go []
      x : xs -> f x       >>= go xs
    Z.DecompressOutputAvailable out getNext  -> do
      next <- getNext
      fmap (BL.fromStrict out <>) <$> go input next
    Z.DecompressStreamEnd _unread            -> return $ Just BL.empty
    Z.DecompressStreamError _err              -> return Nothing
  in go (BL.toChunks bs) $ Z.decompressST Z.zlibFormat Z.defaultDecompressParams
