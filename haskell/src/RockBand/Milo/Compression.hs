{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module RockBand.Milo.Compression where

import qualified Codec.Compression.GZip          as GZ
import qualified Codec.Compression.Zlib.Internal as Z
import           Control.Monad                   (forM, replicateM)
import           Control.Monad.ST.Lazy
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BL
import           Data.List                       (foldl')
import           Data.List.Split                 (keepDelimsR, onSublist, split)
import           Data.Word                       (Word8)

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

magicBarrier :: [Word8]
magicBarrier = [0xAD, 0xDE, 0xAD, 0xDE]

addMiloHeader :: BL.ByteString -> BL.ByteString
addMiloHeader bs = let
  headerSize = 0x810
  chunks = map (fromIntegral . length) $ filter (not . null) $
    case split (keepDelimsR $ onSublist magicBarrier) $ BL.unpack bs of
      []           -> []
      [c]          -> [c]
      c1 : c2 : cs -> (c1 ++ c2) : cs
  header = runPut $ do
    putWord32le 0xCABEDEAF
    putWord32le headerSize
    putWord32le $ fromIntegral $ length chunks
    putWord32le $ foldl' max 0 chunks
    mapM_ putWord32le chunks
  in BL.concat
    [ header
    , BL.replicate (fromIntegral headerSize - BL.length header) 0
    , bs
    ]
