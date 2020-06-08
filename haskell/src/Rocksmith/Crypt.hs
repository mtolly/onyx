module Rocksmith.Crypt where

import           Codec.Compression.Zlib (decompress)
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Binary.Get
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL

data GamePlatform
  = PC
  | Mac
  | Xbox360 -- is Xbox One same?
  | PS3 -- is PS4 same?
  deriving (Eq, Ord, Show, Enum, Bounded)

unpackSNG :: GamePlatform -> FilePath -> IO BL.ByteString
unpackSNG plat fp = do
  bs <- B.readFile fp
  bs' <- case plat of
    PC  -> decryptSNGData bs sngKeyPC
    Mac -> decryptSNGData bs sngKeyMac
    _   -> return $ BL.fromStrict $ B.drop 8 bs
  return $ decompress $ BL.drop 4 bs'

sngKeyMac, sngKeyPC, arcKey, arcIV :: B.ByteString
sngKeyMac = B.pack
  [ 0x98, 0x21, 0x33, 0x0E, 0x34, 0xB9, 0x1F, 0x70
  , 0xD0, 0xA4, 0x8C, 0xBD, 0x62, 0x59, 0x93, 0x12
  , 0x69, 0x70, 0xCE, 0xA0, 0x91, 0x92, 0xC0, 0xE6
  , 0xCD, 0xA6, 0x76, 0xCC, 0x98, 0x38, 0x28, 0x9D
  ]
sngKeyPC = B.pack
  [ 0xCB, 0x64, 0x8D, 0xF3, 0xD1, 0x2A, 0x16, 0xBF
  , 0x71, 0x70, 0x14, 0x14, 0xE6, 0x96, 0x19, 0xEC
  , 0x17, 0x1C, 0xCA, 0x5D, 0x2A, 0x14, 0x2E, 0x3E
  , 0x59, 0xDE, 0x7A, 0xDD, 0xA1, 0x8A, 0x3A, 0x30
  ]
arcKey = B.pack
  [ 0xC5, 0x3D, 0xB2, 0x38, 0x70, 0xA1, 0xA2, 0xF7
  , 0x1C, 0xAE, 0x64, 0x06, 0x1F, 0xDD, 0x0E, 0x11
  , 0x57, 0x30, 0x9D, 0xC8, 0x52, 0x04, 0xD4, 0xC5
  , 0xBF, 0xDF, 0x25, 0x09, 0x0D, 0xF2, 0x57, 0x2C
  ]
arcIV = B.pack
  [ 0xE9, 0x15, 0xAA, 0x01, 0x8F, 0xEF, 0x71, 0xFC
  , 0x50, 0x81, 0x32, 0xE4, 0xBB, 0x4C, 0xEB, 0x42
  ]

decryptSNGData :: (MonadFail m) => B.ByteString -> B.ByteString -> m BL.ByteString
decryptSNGData input key = do
  Just initIV <- flip runGet (BL.fromStrict input) $ do
    _4A <- getWord32be -- 0x4A either little or big endian
    _platform <- getWord32be
    iv <- getByteString 16
    return $ return $ makeIV iv
  CryptoPassed cipher <- return $ cipherInit key
  let go iv rest = if B.null rest
        then []
        else let
          chunk = cfbDecrypt (cipher :: AES256) iv $ B.take 16 rest
          in chunk : go (ivAdd iv 1) (B.drop 16 rest)
  return $ BL.fromChunks $ go initIV $ B.drop 24 input

decryptPSARCTable :: (MonadFail m) => B.ByteString -> m B.ByteString
decryptPSARCTable input = do
  cipher <- case cipherInit arcKey of
    CryptoPassed cipher -> return cipher
    CryptoFailed err    -> fail $ show err
  Just iv <- return $ makeIV arcIV
  return $ cfbDecrypt (cipher :: AES256) iv input

decryptPSARCTable' :: (MonadFail m) => B.ByteString -> m B.ByteString
decryptPSARCTable' input = if B.all (== 0) $ B.take 16 input
  then return input -- Xbox .psarc don't have encrypted TOC?
  else decryptPSARCTable input
