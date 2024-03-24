{-# LANGUAGE OverloadedStrings #-}
module Onyx.PlayStation.NPData
( packNPData
, NPDataConfig(..)
, rb2CustomMidEdatConfig
, rb3CustomMidEdatConfig
, ghworCustomMidEdatConfig
, gh3CustomMidEdatConfig
, rockRevolutionEdatConfig
, ghworKLIC
, rockBandKLIC
, rs2014KLIC
, decryptNPData
, NPDecryptConfig(..)
) where

import           Control.Monad          (forM_, when)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Digest.Pure.MD5   as MD5
import           Foreign
import           Foreign.C

foreign import ccall "pack_data_from_paths"
  c_pack_data_from_paths
    :: CString   -- ^ input_path
    -> CString   -- ^ output_path
    -> CString   -- ^ hash_file_name
    -> Ptr Word8 -- ^ content_id
    -> Ptr Word8 -- ^ devklic
    -> Ptr Word8 -- ^ rifkey
    -> CInt      -- ^ version
    -> CInt      -- ^ license
    -> CInt      -- ^ type
    -> CInt      -- ^ block
    -> Bool      -- ^ useCompression
    -> Bool      -- ^ isEDAT
    -> Bool      -- ^ isFinalized
    -> Bool      -- ^ verbose
    -> IO Bool

foreign import ccall "extract_data_from_paths"
  c_extract_data_from_paths
    :: CString   -- ^ input_path
    -> CString   -- ^ output_path
    -> CString   -- ^ hash_file_name
    -> Ptr Word8 -- ^ devklic
    -> Ptr Word8 -- ^ rifkey
    -> Bool      -- ^ verbose
    -> IO Bool

foreign import ccall "get_rif_key"
  c_get_rif_key
    :: Ptr Word8
    -> Ptr Word8
    -> IO ()

data NPDataConfig = NPDataConfig
  { npdContentID :: B.ByteString
  , npdKLIC      :: B.ByteString
  , npdRAP       :: Maybe B.ByteString
  , npdVersion   :: Int
  , npdLicense   :: Int
  , npdType      :: Int
  , npdBlock     :: Int
  , npdEDAT      :: Bool
  }

-- ^ .mid.edat encryption for customs using NTSC Still Alive free DLC license
rb2MidEdatConfig :: NPDataConfig
rb2MidEdatConfig = NPDataConfig
  { npdContentID = "UP0006-BLUS30050_00-RBSTILLALCCF005D"
  -- klic is MD5("Ih38rtW1ng3rCCF005D10025250")
  , npdKLIC      = B.pack        [0x22, 0x19, 0x54, 0xA1, 0x0A, 0x89, 0xCE, 0xC6, 0x9E, 0xE0, 0x99, 0x41, 0x23, 0x39, 0x81, 0x74]
  , npdRAP       = Just $ B.pack [0x44, 0xD3, 0x9A, 0xCD, 0x73, 0x40, 0x2B, 0xE0, 0x94, 0x56, 0xF9, 0xA0, 0xD7, 0x30, 0xB4, 0xB5]
  , npdVersion   = 2
  , npdLicense   = 3
  , npdType      = 0
  , npdBlock     = 16
  , npdEDAT      = True
  }

-- ^ .mid.edat encryption for customs using NTSC Rock Band Free Pack 01 free DLC license
rb3NTSCMidEdatConfig :: NPDataConfig
rb3NTSCMidEdatConfig = NPDataConfig
  { npdContentID = "UP8802-BLUS30463_00-RBHMXBANDCCFF0D6"
  -- klic is MD5("Ih38rtW1ng3rHMX075610025250") where HMX0756 is the free pack ID
  , npdKLIC      = B.pack [0x0B, 0x72, 0xB6, 0x2D, 0xAB, 0xA8, 0xCA, 0xFD, 0xA3, 0x35, 0x2F, 0xF9, 0x79, 0xC6, 0xD5, 0xC2]
  , npdRAP       = Just $ B.pack [0xCF, 0x1E, 0xA0, 0xDA, 0x36, 0xEB, 0x82, 0xD9, 0x6D, 0xC2, 0x68, 0x6C, 0xD9, 0x17, 0x44, 0xEA]
  , npdVersion   = 2
  , npdLicense   = 3
  , npdType      = 0
  , npdBlock     = 16
  , npdEDAT      = True
  }

{-

PAL free pack (from c3, like ntsc version)
content id: EP0006-BLES00986_00-RBHMXBANDCCFF0D6
klic: 0B72B62DABA8CAFDA3352FF979C6D5C2
rap: 59 de 7e 02 16 a6 7b fb 0c ff 90 c2 6b f5 52 4b

-}

rb2CustomMidEdatConfig :: B.ByteString -> NPDataConfig
rb2CustomMidEdatConfig dir = NPDataConfig
  { npdContentID = "UP0006-BLUS30050_00-" <> dir
  , npdKLIC      = rockBandKLIC dir
  , npdRAP       = Nothing
  , npdVersion   = 2
  , npdLicense   = 3
  , npdType      = 0
  , npdBlock     = 16
  , npdEDAT      = True
  }

rb3CustomMidEdatConfig :: B.ByteString -> NPDataConfig
rb3CustomMidEdatConfig dir = NPDataConfig
  { npdContentID = "UP8802-BLUS30463_00-" <> dir -- actually this probably doesn't have to be the same
  , npdKLIC      = rockBandKLIC dir
  , npdRAP       = Nothing
  , npdVersion   = 2
  , npdLicense   = 3
  , npdType      = 0
  , npdBlock     = 16
  , npdEDAT      = True
  }

ghworCustomMidEdatConfig :: B.ByteString -> NPDataConfig
ghworCustomMidEdatConfig pkgLabel = NPDataConfig
  { npdContentID = "UP0002-BLUS30487_00-" <> pkgLabel
  , npdKLIC      = ghworKLIC
  , npdRAP       = Nothing
  , npdVersion   = 2
  , npdLicense   = 3
  , npdType      = 0
  , npdBlock     = 16
  , npdEDAT      = True
  }

gh3CustomMidEdatConfig :: B.ByteString -> NPDataConfig
gh3CustomMidEdatConfig pkgLabel = NPDataConfig
  { npdContentID = "UP0002-BLUS30074_00-" <> pkgLabel
  , npdKLIC      = ghworKLIC
  , npdRAP       = Nothing
  , npdVersion   = 2
  , npdLicense   = 3
  , npdType      = 0
  , npdBlock     = 16
  , npdEDAT      = True
  }

-- RR PS3 appears to not like rapless edat... annoying
rockRevolutionEdatConfig :: B.ByteString -> NPDataConfig
rockRevolutionEdatConfig pkgLabel = NPDataConfig
  { npdContentID = "UP0101-BLUS30212_00-" <> pkgLabel
  , npdKLIC      = B.replicate 16 0
  , npdRAP       = Just $ B.replicate 16 0
  , npdVersion   = 2
  , npdLicense   = 2
  , npdType      = 0
  , npdBlock     = 16
  , npdEDAT      = True
  }

packNPData :: NPDataConfig -> FilePath -> FilePath -> B.ByteString -> IO ()
packNPData cfg fin fout hashName = do
  withCString fin $ \pin -> do
    withCString fout $ \pout -> do
      -- must be null-terminated
      B.useAsCString hashName $ \pname -> do
        -- content ID must be padded to 0x30 bytes with zeroes at end
        BU.unsafeUseAsCString (npdContentID cfg <> B.replicate 0x30 0) $ \pcid -> do
          -- klic, RAP, and RIF are all 0x10 bytes
          BU.unsafeUseAsCString (npdKLIC cfg <> B.replicate 0x10 0) $ \pklic -> do
            withArray (replicate 0x10 0) $ \prif -> do
              forM_ (npdRAP cfg) $ \rap -> do
                BU.unsafeUseAsCString (rap <> B.replicate 0x10 0) $ \prap -> do
                  c_get_rif_key (castPtr prap) (castPtr prif)
              err <- c_pack_data_from_paths
                pin
                pout
                pname
                (castPtr pcid)
                (castPtr pklic)
                prif
                (fromIntegral $ npdVersion cfg)
                (fromIntegral $ npdLicense cfg)
                (fromIntegral $ npdType cfg)
                (fromIntegral $ npdBlock cfg)
                False
                (npdEDAT cfg)
                True
                True
              when err $ ioError $ userError "Error in PS3 file encryption"

data NPDecryptConfig = NPDecryptConfig
  { decKLIC :: B.ByteString
  , decRAP  :: Maybe B.ByteString
  }

decryptNPData :: NPDecryptConfig -> FilePath -> FilePath -> B.ByteString -> IO ()
decryptNPData cfg fin fout hashName = do
  withCString fin $ \pin -> do
    withCString fout $ \pout -> do
      -- must be null-terminated
      B.useAsCString hashName $ \pname -> do
        -- klic, RAP, and RIF are all 0x10 bytes
        BU.unsafeUseAsCString (decKLIC cfg <> B.replicate 0x10 0) $ \pklic -> do
          withArray (replicate 0x10 0) $ \prif -> do
            forM_ (decRAP cfg) $ \rap -> do
              BU.unsafeUseAsCString (rap <> B.replicate 0x10 0) $ \prap -> do
                c_get_rif_key (castPtr prap) (castPtr prif)
            err <- c_extract_data_from_paths
              pin
              pout
              pname
              (castPtr pklic)
              prif
              True
            when err $ ioError $ userError "Error in PS3 file decryption"

-- Appears to be a constant dev klic for WoR DLC, and GH3? + probably everything in between
ghworKLIC :: B.ByteString
ghworKLIC = B.pack [0xD7,0xF3,0xF9,0x0A,0x1F,0x01,0x2D,0x84,0x4C,0xA5,0x57,0xE0,0x8E,0xE4,0x23,0x91]

rockBandKLIC :: B.ByteString -> B.ByteString
rockBandKLIC dir = MD5.md5DigestBytes $ MD5.md5 $ "Ih38rtW1ng3r" <> BL.fromStrict dir <> "10025250"

-- Thanks to https://github.com/rscustom/rocksmith-custom-song-toolkit/blob/a60453fa5cb6948085f5bafda9685b500c320a7d/RocksmithToolkitLib/DLCPackage/RijndaelEncryptor.cs#L325
rs2014KLIC :: B.ByteString
rs2014KLIC = B.pack [0xCB,0x4A,0x06,0xE8,0x53,0x78,0xCE,0xD3,0x07,0xE6,0x3E,0xFD,0x10,0x84,0xC1,0x9D]
