{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-
MT: mostly unmodified Claude output

Checks and fixes the end-of-stream flag in Ogg files. Onyx in 20261011/20261016
didn't set EOS on the last page, which causes RB on PS3 to loop the end of the
audio. Only page flags and CRCs are changed, so page offsets and sizes stay the
same.
-}
module Onyx.Audio.OggEOS
( fixOggEOS
, fixOggEOSBytes
) where

import           Control.Monad           (unless, when)
import           Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL
import           Data.List               (foldl')
import           Data.Maybe              (fromMaybe)
import qualified Data.Vector.Unboxed     as VU
import           Data.Word               (Word32, Word8)
import           Onyx.Util.Handle        (Readable (..), byteStringSimpleHandle,
                                          handleLabel, openSimpleHandle,
                                          useHandle)
import           System.IO               (hFileSize)
import           Witch                   (from)

-- | Checks an Ogg file's EOS flags. Returns 'Nothing' if they're already
-- correct (only the last page has EOS), or a fixed version if not.
-- The fixed version redoes the work each time it's opened.
fixOggEOS :: Readable -> IO (Maybe Readable)
fixOggEOS r = do
  (label, input) <- readAll r
  case fixOggEOSBytes input of
    Left  err     -> fail $ "fixOggEOS (" <> label <> "): " <> err
    Right Nothing -> return Nothing
    Right (Just _) -> return $ Just Readable
      { rFilePath = Nothing
      , rOpen = do
        (label', input') <- readAll r
        case fixOggEOSBytes input' of
          Left  err -> fail $ "fixOggEOS (" <> label' <> "): " <> err
          Right out -> byteStringSimpleHandle (fromMaybe (BL.fromStrict input') out)
            >>= openSimpleHandle (label' <> " | EOS fixed")
      }

readAll :: Readable -> IO (String, B.ByteString)
readAll r = useHandle r \h -> do
  bs <- hFileSize h >>= B.hGet h . fromIntegral
  return (handleLabel h, bs)

-- | Pure version of 'fixOggEOS' operating on the whole file.
fixOggEOSBytes :: B.ByteString -> Either String (Maybe BL.ByteString)
fixOggEOSBytes input = do
  pages <- scanPages input
  firstPage <- case pages of
    p : _ -> Right p
    []    -> Left "No Ogg pages found"
  unless (all (\pg -> pg.serial == firstPage.serial) pages) $
    Left "Multiplexed or chained Ogg streams are not supported"
  let lastIndex = length pages - 1
      withWanted = flip map (zip [0 ..] pages) \(i, pg) -> let
        wanted = if i == lastIndex
          then pg.flags .|. eosFlag
          else pg.flags .&. complement eosFlag
        in (pg, wanted)
  Right $ if all (\(pg, wanted) -> pg.flags == wanted) withWanted
    then Nothing
    else Just $ BL.fromChunks $ flip map withWanted \(pg, wanted) -> let
      bytes = B.take pg.size $ B.drop pg.offset input
      in if pg.flags == wanted then bytes else setPageFlags wanted bytes

eosFlag :: Word8
eosFlag = 4

data PageInfo = PageInfo
  { offset :: Int
  , size   :: Int
  , flags  :: Word8
  , serial :: Word32
  }

readLE :: (Integral a, Bits a) => B.ByteString -> Int -> Int -> a
readLE bs off n = foldr
  (\i acc -> fromIntegral (B.index bs $ off + i) .|. shiftL acc 8)
  0
  [0 .. n - 1]

scanPages :: B.ByteString -> Either String [PageInfo]
scanPages input = go 0 where
  go off = if off >= B.length input then Right [] else do
    let rest = B.drop off input
        bad msg = Left $ msg <> " at offset " <> show off
    when (B.length rest < 27 || B.take 4 rest /= "OggS") $ bad "Invalid Ogg page"
    when (B.index rest 4 /= 0) $ bad "Unsupported Ogg version"
    let nsegs = from $ B.index rest 26
    when (B.length rest < 27 + nsegs) $ bad "Truncated Ogg page"
    let total = 27 + nsegs + sum (map from $ B.unpack $ B.take nsegs $ B.drop 27 rest)
    when (B.length rest < total) $ bad "Truncated Ogg page"
    let page = PageInfo
          { offset = off
          , size   = total
          , flags  = B.index rest 5
          , serial = readLE rest 14 4
          }
    (page :) <$> go (off + total)

-- | Replaces the header flags of a complete page and recomputes its CRC.
setPageFlags :: Word8 -> B.ByteString -> B.ByteString
setPageFlags newFlags page = let
  noCRC = B.concat
    [ B.take 5 page
    , B.singleton newFlags
    , B.take 16 $ B.drop 6 page
    , B.replicate 4 0
    , B.drop 26 page
    ]
  crc = BL.toStrict $ BB.toLazyByteString $ BB.word32LE $ oggCRC noCRC
  in B.take 22 noCRC <> crc <> B.drop 26 noCRC

crcTable :: VU.Vector Word32
crcTable = VU.generate 256 \i -> foldl'
  (\r _ -> if testBit r 31 then shiftL r 1 `xor` 0x04C11DB7 else shiftL r 1)
  (shiftL (fromIntegral i) 24)
  [1 .. 8 :: Int]

oggCRC :: B.ByteString -> Word32
oggCRC = B.foldl'
  (\crc b -> shiftL crc 8 `xor` (crcTable VU.! fromIntegral (shiftR crc 24 `xor` from b)))
  0
