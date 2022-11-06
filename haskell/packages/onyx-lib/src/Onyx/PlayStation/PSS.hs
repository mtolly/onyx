{-
MPEG structures decoded with the help of
https://unix4lyfe.org/mpeg1/
http://dvd.sourceforge.net/dvdinfo/mpeghdrs.html
http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.PlayStation.PSS where

import           Control.Monad        (forM)
import           Data.Binary.Get
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           Numeric
import           System.IO

data Packet a
  = PackStart B.ByteString
  | Packet Word8 a
  deriving (Show)

scanPackets :: Handle -> IO [(Int, Packet (Int, Int))]
scanPackets h = go [] 0 where
  go packets !posn = hIsEOF h >>= \case
    True -> return $ reverse packets -- seems to happen in VOB, but RB PSS should use B9 explicit end
    False -> do
      pktID <- B.hGet h 4
      let unknownID = fail $ "Unrecognized packet ID " <> show pktID <> " at position " <> showHex posn ""
      case B.unpack <$> B.stripPrefix "\x00\x00\x01" pktID of
        Just [byte] -> case byte of
          0xB9 -> return $ reverse packets -- end indicator
          0xBA -> do
            bytes <- B.hGet h 10
            go ((posn, PackStart bytes) : packets) (posn + 14)
          _ -> do
            dataLen <- fromIntegral . runGet getWord16be <$> BL.hGet h 2
            let dataPosn = posn + 6
            hSeek h RelativeSeek $ fromIntegral dataLen
            go ((posn, Packet byte (dataPosn, dataLen)) : packets) (posn + 6 + dataLen)
        _ -> unknownID

extractVideoStream :: Word8 -> [(Int, Packet (Int, Int))] -> Handle -> IO BL.ByteString
extractVideoStream videoID pkts h = do
  let matchPackets = [(pos, len) | (_, Packet pktID (pos, len)) <- pkts, videoID == pktID]
  fmap BL.concat $ forM matchPackets $ \(pos, len) -> do
    -- Packet starts with extension described at http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
    hSeek h AbsoluteSeek $ fromIntegral pos + 2
    remainingExtLength <- B.head <$> B.hGet h 1
    hSeek h RelativeSeek $ fromIntegral remainingExtLength
    BL.hGet h $ len - 3 - fromIntegral remainingExtLength

extractVGSStream :: Word8 -> [(Int, Packet (Int, Int))] -> Handle -> IO BL.ByteString
extractVGSStream vgsID pkts h = do
  let matchPackets = [(pos, len) | (_, Packet pktID (pos, len)) <- pkts, vgsID == pktID]
  fmap BL.concat $ forM matchPackets $ \(pos, len) -> do
    -- Header is unclear, does not appear to be correct PES extension length but always 17 bytes total
    hSeek h AbsoluteSeek $ fromIntegral pos + 17
    BL.hGet h $ len - 17
