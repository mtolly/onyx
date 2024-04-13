{-
PARAM.SFO format
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.PlayStation.SFO where

import           Control.Monad            (replicateM, unless)
import           Data.Binary.Get
import qualified Data.ByteString          as B
import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString.Lazy     as BL
import           Data.List                (sortOn)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Word
import           Onyx.Util.Binary         (runGetM)

data SFOEntry
  = SFO_UTF8_S Word32 T.Text -- Word32 is max allocated size
  | SFO_UTF8   Word32 T.Text -- Word32 is max allocated size
  | SFO_Word32 Word32
  deriving (Show)

data SFO = SFO
  { version :: Word32
  , entries :: [(T.Text, SFOEntry)]
  } deriving (Show)

readSFO :: (MonadFail m) => BL.ByteString -> m SFO
readSFO bs = do
  (version, keysStart, dataStart, numEntries) <- flip runGetM bs $ do
    magic <- getByteString 4
    unless (magic == "\0PSF") $ fail $ "Unrecognized magic identifier in .SFO: " <> show magic
    (,,,) <$> getWord32le <*> getWord32le <*> getWord32le <*> getWord32le
  let toText = TE.decodeUtf8With lenientDecode . BL.toStrict
  entries <- flip runGetM (BL.drop 0x14 bs) $ do
    replicateM (fromIntegral numEntries) $ do
      keyOffset  <- getWord16le
      dataFormat <- getWord16be
      usedBytes  <- getWord32le
      totalBytes <- getWord32le
      dataOffset <- getWord32le
      let key = toText $ BL.takeWhile (/= 0) keyBytes
          keyBytes  = BL.drop (fromIntegral keysStart + fromIntegral keyOffset ) bs
          dataBytes = BL.drop (fromIntegral dataStart + fromIntegral dataOffset) bs
      dat <- case dataFormat of
        0x0400 -> return $ SFO_UTF8_S totalBytes $ toText $ BL.take (fromIntegral usedBytes) dataBytes
        0x0402 -> return $ SFO_UTF8 totalBytes $ toText $ BL.takeWhile (/= 0) $ BL.take (fromIntegral usedBytes) dataBytes
        0x0404 -> SFO_Word32 <$> runGetM getWord32le dataBytes
        _ -> fail $ "Unrecognized entry type in .SFO: " <> show dataFormat
      return (key, dat)
  return SFO
    { version = version
    , entries = entries
    }

makeSFO :: SFO -> BL.ByteString
makeSFO sfo = let
  -- sort by key alphabetically
  entries = sortOn fst sfo.entries
  padTo4 bs = case rem (BL.length bs) 4 of
    0 -> bs
    r -> bs <> BL.replicate (4 - r) 0
  withLength allocated bs = let
    size = fromIntegral $ B.length bs
    in case compare size allocated of
      LT -> (size, BB.byteString bs <> BB.byteString (B.replicate (fromIntegral $ allocated - size) 0))
      EQ -> (size, BB.byteString bs)
      GT -> (allocated, BB.byteString $ B.take (fromIntegral allocated) bs)
  go indexTable keyTable keyPosn dataTable dataPosn ((k, v) : rest) = let
    (dataFormat, usedBytes, totalBytes, dat) = case v of
      SFO_UTF8_S allocated t -> let
        (used, datBuilder) = withLength allocated $ TE.encodeUtf8 t
        in (0x0400, used, allocated, datBuilder)
      SFO_UTF8 allocated t -> let
        (used, datBuilder) = withLength (allocated - 1) $ TE.encodeUtf8 t
        in (0x0402, used + 1, allocated, datBuilder <> BB.word8 0)
      SFO_Word32 n -> (0x0404, 4, 4, BB.word32LE n)
    indexTable' = mconcat
      [ indexTable
      , BB.word16LE keyPosn
      , BB.word16BE dataFormat
      , BB.word32LE usedBytes
      , BB.word32LE totalBytes
      , BB.word32LE dataPosn
      ]
    key = TE.encodeUtf8 k <> B.singleton 0
    keyTable' = keyTable <> BB.byteString key
    keyPosn' = keyPosn + fromIntegral (B.length key)
    dataTable' = dataTable <> dat
    dataPosn' = dataPosn + totalBytes
    in go indexTable' keyTable' keyPosn' dataTable' dataPosn' rest
  go indexTable keyTable _ dataTable _ [] = let
    indexTableBytes = BB.toLazyByteString indexTable
    keyTableBytes = padTo4 $ BB.toLazyByteString keyTable
    keysStart = 20 + BL.length indexTableBytes
    dataStart = keysStart + BL.length keyTableBytes
    in BB.toLazyByteString $ mconcat
      -- header
      [ BB.byteString "\0PSF"
      , BB.word32LE sfo.version
      , BB.word32LE $ fromIntegral keysStart
      , BB.word32LE $ fromIntegral dataStart
      , BB.word32LE $ fromIntegral $ length entries
      -- tables
      , BB.lazyByteString indexTableBytes
      , BB.lazyByteString keyTableBytes
      , dataTable
      ]
  in go mempty mempty 0 mempty 0 entries
