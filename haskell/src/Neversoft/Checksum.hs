{-# LANGUAGE BangPatterns #-}
module Neversoft.Checksum where

import           Data.Bits
import qualified Data.ByteString     as B
import qualified Data.Vector.Unboxed as VU
import           Data.Word

crcTable :: VU.Vector Word32
crcTable = VU.fromList $ flip map [0..255] $ let
  poly = 0xEDB88320 :: Word32
  crcStep crc = if crc `testBit` 0
    then (crc `shiftR` 1) `xor` poly
    else crc `shiftR` 1
  in foldr (.) id $ replicate 8 crcStep

qbKeyCRC :: B.ByteString -> Word32
qbKeyCRC = go 0xFFFFFFFF where
  go !crc bs = case B.uncons bs of
    Nothing -> crc
    Just (b, bs') -> let
      index = fromIntegral crc `xor` b
      entry = crcTable VU.! fromIntegral index
      crc' = (crc `shiftR` 8) `xor` entry
      in go crc' bs'

crc32 :: B.ByteString -> Word32
crc32 = (`xor` 0xFFFFFFFF) . qbKeyCRC
