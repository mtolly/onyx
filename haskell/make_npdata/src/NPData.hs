{-# LANGUAGE OverloadedStrings #-}
module NPData (packNPData, NPDataConfig(..), rb2MidEdatConfig) where

import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as BU
import           Foreign
import           Foreign.C
import           Control.Monad          (forM_, when)

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
    -> Bool      -- ^ isSDAT
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
  , npdSDAT      :: Bool
  }

-- ^ .mid.edat encryption for customs using NTSC Still Alive free DLC license
rb2MidEdatConfig :: NPDataConfig
rb2MidEdatConfig = NPDataConfig
  { npdContentID = "UP0006-BLUS30050_00-RBSTILLALCCF005D"
  , npdKLIC      = B.pack        [0x22, 0x19, 0x54, 0xA1, 0x0A, 0x89, 0xCE, 0xC6, 0x9E, 0xE0, 0x99, 0x41, 0x23, 0x39, 0x81, 0x74]
  , npdRAP       = Just $ B.pack [0x44, 0xD3, 0x9A, 0xCD, 0x73, 0x40, 0x2B, 0xE0, 0x94, 0x56, 0xF9, 0xA0, 0xD7, 0x30, 0xB4, 0xB5]
  , npdVersion   = 2
  , npdLicense   = 3
  , npdType      = 0
  , npdBlock     = 16
  , npdSDAT      = False
  }

packNPData :: NPDataConfig -> FilePath -> FilePath -> B.ByteString -> IO ()
packNPData cfg fin fout hashName = do
  withCString fin $ \pin -> do
    withCString fout $ \pout -> do
      BU.unsafeUseAsCString hashName $ \pname -> do
        BU.unsafeUseAsCString (npdContentID cfg <> B.replicate 0x30 0) $ \pcid -> do
          BU.unsafeUseAsCString (npdKLIC cfg) $ \pklic -> do
            withArray (replicate 0x10 0) $ \prif -> do
              forM_ (npdRAP cfg) $ \rap -> do
                BU.unsafeUseAsCString rap $ \prap -> do
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
                (npdSDAT cfg)
                False
              when err $ ioError $ userError "Error in PS3 file encryption"
