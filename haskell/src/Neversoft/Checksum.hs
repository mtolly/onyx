{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Neversoft.Checksum where

import           Data.Bits
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char             (toLower)
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Vector.Unboxed   as VU
import           Data.Word
import           Resources
import           System.IO.Unsafe      (unsafePerformIO)

crcTable :: VU.Vector Word32
crcTable = VU.fromList $ flip map [0..255] $ let
  poly = 0xEDB88320 :: Word32
  crcStep crc = if crc `testBit` 0
    then (crc `shiftR` 1) `xor` poly
    else crc `shiftR` 1
  in foldr (.) id $ replicate 8 crcStep

qbKeyCRC :: B.ByteString -> Word32
qbKeyCRC = go 0xFFFFFFFF . B8.map toLower where
  go !crc bs = case B.uncons bs of
    Nothing -> crc
    Just (b, bs') -> let
      index = fromIntegral crc `xor` b
      entry = crcTable VU.! fromIntegral index
      crc' = (crc `shiftR` 8) `xor` entry
      in go crc' bs'

crc32 :: B.ByteString -> Word32
crc32 = (`xor` 0xFFFFFFFF) . qbKeyCRC

{-# NOINLINE knownKeys #-}
knownKeys :: HM.HashMap Word32 B.ByteString
knownKeys = unsafePerformIO $ do
  keys <- getResourcesPath "ghwor-wii-qb-keys.txt" >>= fmap B8.lines . B.readFile
  let allKeys = keys
        <> ["dlc" <> B8.pack (show (n :: Int)) | n <- [0..999]]
        <> ["album_title", "metal", "grunge"]
  return $ HM.fromList $ map (\k -> (qbKeyCRC k, k)) allKeys

-- This appears to be how most .qs / .qs.(lang) keys are calculated, but some are different?
-- Escape sequences are given to this as backslash + char. So "\LSong Title" is ['\\', 'L', 'S', ...]
qsKey :: T.Text -> Word32
qsKey = qbKeyCRC . TE.encodeUtf16LE

huntKeys :: Int -> [B.ByteString] -> Word32 -> Maybe B.ByteString
huntKeys maxLevel parts k = go (1 :: Int) where
  combos 1 = parts
  combos n = (<>) <$> parts <*> combos (n - 1)
  go n
    | n >= maxLevel = Nothing
    | otherwise = case lookup k [ (qbKeyCRC str, str) | str <- combos n ] of
      Nothing  -> go $ n + 1
      Just str -> Just str
