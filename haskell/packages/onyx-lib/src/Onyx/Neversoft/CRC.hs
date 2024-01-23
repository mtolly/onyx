{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Onyx.Neversoft.CRC where

import           Data.Bifunctor        (first)
import           Data.Binary.Get       (Get, getWord32be)
import           Data.Binary.Put       (Put, putWord32be)
import           Data.Bits
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char             (toLower, toUpper)
import           Data.Hashable         (Hashable)
import qualified Data.HashMap.Strict   as HM
import           Data.Profunctor
import           Data.String           (IsString (..))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Vector.Unboxed   as VU
import           Data.Word
import           Numeric               (showHex)
import           Onyx.Codec.Binary     (BinEndian (..))
import           Onyx.Resources
import           System.Directory      (doesFileExist)
import           System.IO.Unsafe      (unsafePerformIO)

newtype QBKey = QBKey { fromQBKey :: Word32 }
  deriving (Eq, Ord, Hashable, Num)

qb8Hex :: QBKey -> String
qb8Hex (QBKey k) = map toUpper $ reverse $ take 8 $ reverse (showHex k "") <> repeat '0'

instance Show QBKey where
  show k = "0x" <> qb8Hex k

instance Read QBKey where
  readsPrec p s = map (first QBKey) $ (readsPrec p :: ReadS Word32) s

instance IsString QBKey where
  fromString = qbKeyCRC . B8.pack

getQBKeyBE :: Get QBKey
getQBKeyBE = QBKey <$> getWord32be

putQBKeyBE :: QBKey -> Put
putQBKeyBE = putWord32be . fromQBKey

instance BinEndian QBKey where
  binEndian = dimap fromQBKey QBKey binEndian

crcTable :: VU.Vector Word32
crcTable = VU.fromList $ flip map [0..255] $ let
  poly = 0xEDB88320 :: Word32
  crcStep crc = if crc `testBit` 0
    then (crc `shiftR` 1) `xor` poly
    else crc `shiftR` 1
  in foldr (.) id $ replicate 8 crcStep

qbKeyCRC :: B.ByteString -> QBKey
qbKeyCRC = QBKey . qbKeyCRCNoLowercase . B8.map toLower

crc32 :: B.ByteString -> Word32
crc32 = (`xor` 0xFFFFFFFF) . fromQBKey . qbKeyCRC

qbKeyCRCNoLowercase :: B.ByteString -> Word32
qbKeyCRCNoLowercase = go 0xFFFFFFFF where
  go !crc bs = case B.uncons bs of
    Nothing -> crc
    Just (b, bs') -> let
      index = fromIntegral crc `xor` b
      entry = crcTable VU.! fromIntegral index
      crc' = (crc `shiftR` 8) `xor` entry
      in go crc' bs'

{-# NOINLINE knownKeys #-}
knownKeys :: HM.HashMap QBKey B.ByteString
knownKeys = unsafePerformIO $ do
  path <- getResourcesPath "gh-debug.txt"
  doesFileExist path >>= \case
    False -> return HM.empty
    True -> do
      lns <- B8.lines <$> B.readFile path
      return $ HM.fromList $ map
        (\s -> (QBKey $ read $ B8.unpack $ B.take 10 s, B.init $ B.drop 12 s))
        lns

-- This appears to be how most .qs / .qs.(lang) keys are calculated, but some are different?
-- Escape sequences are given to this as backslash + char. So "\LSong Title" is ['\\', 'L', 'S', ...]
qsKey :: T.Text -> Word32
qsKey = qbKeyCRCNoLowercase . TE.encodeUtf16LE

huntKeys :: Int -> [B.ByteString] -> QBKey -> Maybe B.ByteString
huntKeys maxLevel parts k = go (1 :: Int) where
  combos 1 = parts
  combos n = (<>) <$> parts <*> combos (n - 1)
  go n
    | n >= maxLevel = Nothing
    | otherwise = case lookup k [ (qbKeyCRC str, str) | str <- combos n ] of
      Nothing  -> go $ n + 1
      Just str -> Just str
