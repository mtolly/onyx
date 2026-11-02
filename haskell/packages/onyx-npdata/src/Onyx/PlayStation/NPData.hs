{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
module Onyx.PlayStation.NPData
( packNPData
, NPDataConfig(..)
, rb2CustomMidEdatConfig
, rb3CustomMidEdatConfig
, ghworCustomMidEdatConfig
, gh3CustomMidEdatConfig
, ghaCustomMidEdatConfig
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
  { contentID :: B.ByteString
  , klic      :: B.ByteString
  , rap       :: Maybe B.ByteString
  , version   :: Int
  , license   :: Int
  , type_     :: Int
  , block     :: Int
  , edat      :: Bool
  }

-- ^ .mid.edat encryption for customs using NTSC Still Alive free DLC license
_rb2MidEdatConfig :: NPDataConfig
_rb2MidEdatConfig = NPDataConfig
  { contentID = "UP0006-BLUS30050_00-RBSTILLALCCF005D"
  -- klic is MD5("Ih38rtW1ng3rCCF005D10025250")
  , klic      = B.pack        [0x22, 0x19, 0x54, 0xA1, 0x0A, 0x89, 0xCE, 0xC6, 0x9E, 0xE0, 0x99, 0x41, 0x23, 0x39, 0x81, 0x74]
  , rap       = Just $ B.pack [0x44, 0xD3, 0x9A, 0xCD, 0x73, 0x40, 0x2B, 0xE0, 0x94, 0x56, 0xF9, 0xA0, 0xD7, 0x30, 0xB4, 0xB5]
  , version   = 2
  , license   = 3
  , type_     = 0
  , block     = 16
  , edat      = True
  }

-- ^ .mid.edat encryption for customs using NTSC Rock Band Free Pack 01 free DLC license
_rb3NTSCMidEdatConfig :: NPDataConfig
_rb3NTSCMidEdatConfig = NPDataConfig
  { contentID = "UP8802-BLUS30463_00-RBHMXBANDCCFF0D6"
  -- klic is MD5("Ih38rtW1ng3rHMX075610025250") where HMX0756 is the free pack ID
  , klic      = B.pack [0x0B, 0x72, 0xB6, 0x2D, 0xAB, 0xA8, 0xCA, 0xFD, 0xA3, 0x35, 0x2F, 0xF9, 0x79, 0xC6, 0xD5, 0xC2]
  , rap       = Just $ B.pack [0xCF, 0x1E, 0xA0, 0xDA, 0x36, 0xEB, 0x82, 0xD9, 0x6D, 0xC2, 0x68, 0x6C, 0xD9, 0x17, 0x44, 0xEA]
  , version   = 2
  , license   = 3
  , type_     = 0
  , block     = 16
  , edat      = True
  }

{-

PAL free pack (from c3, like ntsc version)
content id: EP0006-BLES00986_00-RBHMXBANDCCFF0D6
klic: 0B72B62DABA8CAFDA3352FF979C6D5C2
rap: 59 de 7e 02 16 a6 7b fb 0c ff 90 c2 6b f5 52 4b

-}

rb2CustomMidEdatConfig :: B.ByteString -> NPDataConfig
rb2CustomMidEdatConfig dir = NPDataConfig
  { contentID = "UP0006-BLUS30050_00-" <> dir
  , klic      = rockBandKLIC dir
  , rap       = Nothing
  , version   = 2
  , license   = 3
  , type_     = 0
  , block     = 16
  , edat      = True
  }

rb3CustomMidEdatConfig :: B.ByteString -> NPDataConfig
rb3CustomMidEdatConfig dir = NPDataConfig
  { contentID = "UP8802-BLUS30463_00-" <> dir -- actually this probably doesn't have to be the same
  , klic      = rockBandKLIC dir
  , rap       = Nothing
  , version   = 2
  , license   = 3
  , type_     = 0
  , block     = 16
  , edat      = True
  }

ghworCustomMidEdatConfig :: B.ByteString -> NPDataConfig
ghworCustomMidEdatConfig pkgLabel = NPDataConfig
  { contentID = "UP0002-BLUS30487_00-" <> pkgLabel
  , klic      = ghworKLIC
  , rap       = Nothing
  , version   = 2
  , license   = 3
  , type_     = 0
  , block     = 16
  , edat      = True
  }

gh3CustomMidEdatConfig :: B.ByteString -> NPDataConfig
gh3CustomMidEdatConfig pkgLabel = NPDataConfig
  { contentID = "UP0002-BLUS30074_00-" <> pkgLabel
  , klic      = ghworKLIC
  , rap       = Nothing
  , version   = 2
  , license   = 3
  , type_     = 0
  , block     = 16
  , edat      = True
  }

ghaCustomMidEdatConfig :: B.ByteString -> NPDataConfig
ghaCustomMidEdatConfig pkgLabel = NPDataConfig
  { contentID = "UP0002-BLUS30133_00-" <> pkgLabel
  , klic      = ghworKLIC
  , rap       = Nothing
  , version   = 2
  , license   = 3
  , type_     = 0
  , block     = 16
  , edat      = True
  }

-- RR PS3 appears to not like rapless edat... annoying
rockRevolutionEdatConfig :: B.ByteString -> NPDataConfig
rockRevolutionEdatConfig pkgLabel = NPDataConfig
  { contentID = "UP0101-BLUS30212_00-" <> pkgLabel
  , klic      = B.replicate 16 0
  , rap       = Just $ B.replicate 16 0
  , version   = 2
  , license   = 2
  , type_     = 0
  , block     = 16
  , edat      = True
  }

packNPData :: NPDataConfig -> FilePath -> FilePath -> B.ByteString -> IO ()
packNPData cfg fin fout hashName = do
  withCString fin \pin -> do
    withCString fout \pout -> do
      -- must be null-terminated
      B.useAsCString hashName \pname -> do
        -- content ID must be padded to 0x30 bytes with zeroes at end
        BU.unsafeUseAsCString (cfg.contentID <> B.replicate 0x30 0) \pcid -> do
          -- klic, RAP, and RIF are all 0x10 bytes
          BU.unsafeUseAsCString (cfg.klic <> B.replicate 0x10 0) \pklic -> do
            withArray (replicate 0x10 0) \prif -> do
              forM_ cfg.rap \rap -> do
                BU.unsafeUseAsCString (rap <> B.replicate 0x10 0) \prap -> do
                  c_get_rif_key (castPtr prap) (castPtr prif)
              err <- c_pack_data_from_paths
                pin
                pout
                pname
                (castPtr pcid)
                (castPtr pklic)
                prif
                (fromIntegral cfg.version)
                (fromIntegral cfg.license)
                (fromIntegral cfg.type_)
                (fromIntegral cfg.block)
                False
                cfg.edat
                True
                True
              when err $ ioError $ userError "Error in PS3 file encryption"

data NPDecryptConfig = NPDecryptConfig
  { decKLIC :: B.ByteString
  , decRAP  :: Maybe B.ByteString
  }

decryptNPData :: NPDecryptConfig -> FilePath -> FilePath -> B.ByteString -> IO ()
decryptNPData cfg fin fout hashName = do
  withCString fin \pin -> do
    withCString fout \pout -> do
      -- must be null-terminated
      B.useAsCString hashName \pname -> do
        -- klic, RAP, and RIF are all 0x10 bytes
        BU.unsafeUseAsCString (cfg.decKLIC <> B.replicate 0x10 0) \pklic -> do
          withArray (replicate 0x10 0) \prif -> do
            forM_ cfg.decRAP \rap -> do
              BU.unsafeUseAsCString (rap <> B.replicate 0x10 0) \prap -> do
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
