{-
Ported from
https://github.com/LagoLunatic/wwrando/blob/46f14efd873c6ebde1782576070a87bc22ca5b69/wwlib/gcm.py
-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Nintendo.GCM where

import           Control.Monad
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty   as NE
import qualified Data.Vector          as V
import           Data.Word
import           Onyx.Util.Binary     (runGetM)
import           Onyx.Util.Handle
import           System.IO

data FileEntry = DirEntry Dir | FileEntry File
  deriving (Show)

data Dir = Dir
  { file_index       :: Word32
  , name             :: B.ByteString
  , parent_fst_index :: Word32
  , next_fst_index   :: Word32
  } deriving (Show)

data File = File
  { file_index       :: Word32
  , name             :: B.ByteString
  , file_data_offset :: Word32
  , file_size        :: Word32
  } deriving (Show)

loadGCM :: Readable -> IO (Folder B.ByteString Readable)
loadGCM gcm = useHandle gcm $ \h -> do

  let read_u32 off = do
        hSeek h AbsoluteSeek off
        BL.hGet h 4 >>= runGetM getWord32be
      read_str_until_null_character off = do
        hSeek h AbsoluteSeek off
        -- does hGetChar on a withBinaryFile handle work sensibly?
        -- don't care about encoding as long as it's 1 byte per get
        let loop !n = hGetChar h >>= \case
              '\0' -> return n
              _    -> loop $ n + 1
        len <- loop 0
        hSeek h AbsoluteSeek off
        B.hGet h len

  fst_offset <- read_u32 0x424
  fst_size <- read_u32 0x428

  -- read_filesystem
  num_file_entries <- read_u32 $ fromIntegral fst_offset + 8
  let fnt_offset = fst_offset + num_file_entries * 0xC
  entries <- forM [0 .. num_file_entries - 1] $ \file_index -> do
    let file_entry_offset = fst_offset + file_index * 0xC
    -- FileEntry.read
    is_dir_and_name_offset <- read_u32 $ fromIntegral file_entry_offset
    file_data_offset_or_parent_fst_index <- read_u32 $ fromIntegral file_entry_offset + 4
    file_size_or_next_fst_index <- read_u32 $ fromIntegral file_entry_offset + 8

    let is_dir = (is_dir_and_name_offset .&. 0xFF000000) /= 0
        name_offset = is_dir_and_name_offset .&. 0x00FFFFFF

    name <- case file_index of
      0 -> return ""
      _ -> read_str_until_null_character $ fromIntegral $ fnt_offset + name_offset

    return $ if is_dir
      then let
        parent_fst_index = file_data_offset_or_parent_fst_index
        next_fst_index = file_size_or_next_fst_index
        in DirEntry Dir{..}
      else let
        file_data_offset = file_data_offset_or_parent_fst_index
        file_size = file_size_or_next_fst_index
        in FileEntry File{..}

  let entryArray = V.fromList entries
      makeFile path off size = (path, subHandle (<> (" : " <> show path)) off (Just size) gcm)
      -- TODO for files, there may be another indicator as to which directory they're in
      -- (not just last directory seen). order seems to be weird in .nkit.iso files
      makeFiles currentParents = \case
        [] -> []
        DirEntry dir : rest -> let
          getNewParents d = case d.file_index of
            0 -> []
            _ -> case entryArray V.!? fromIntegral d.parent_fst_index of
              Just (DirEntry parent) -> getNewParents parent <> [d.name]
              _                      -> ["broken-path"]
          in makeFiles (getNewParents dir) rest
        FileEntry file : rest -> let
          newFile = makeFile
            (NE.prependList currentParents $ pure file.name)
            (fromIntegral file.file_data_offset)
            (fromIntegral file.file_size)
          in newFile : makeFiles currentParents rest
      normalFiles = makeFiles [] entries

  -- read_system_data

  let boot_bin = makeFile ("sys" :| ["boot.bin"]) 0 0x440
      bi2_bin = makeFile ("sys" :| ["bi2.bin"]) 0x440 0x2000

  let apploader_header_size = 0x20
  apploader_size <- read_u32 $ 0x2440 + 0x14
  apploader_trailer_size <- read_u32 $ 0x2440 + 0x18
  let apploader_full_size = apploader_header_size + apploader_size + apploader_trailer_size
      apploader_img = makeFile ("sys" :| ["apploader.img"]) 0x2440 (fromIntegral apploader_full_size)

  dol_offset <- read_u32 0x420
  dol_sizes_text <- forM [0..6] $ \i -> do -- Text sections
    section_offset <- read_u32 $ fromIntegral dol_offset + 0x00 + i*4
    section_size <- read_u32 $ fromIntegral dol_offset + 0x90 + i*4
    let section_end_offset = section_offset + section_size
    return section_end_offset
  dol_sizes_data <- forM [0..10] $ \i -> do -- Data sections
    section_offset <- read_u32 $ fromIntegral dol_offset + 0x1C + i*4
    section_size <- read_u32 $ fromIntegral dol_offset + 0xAC + i*4
    let section_end_offset = section_offset + section_size
    return section_end_offset
  let main_dol_size = foldr max 0 $ dol_sizes_text <> dol_sizes_data
      main_dol = makeFile ("sys" :| ["main.dol"]) (fromIntegral dol_offset) (fromIntegral main_dol_size)

  let fst_bin = makeFile ("sys" :| ["fst.bin"]) (fromIntegral fst_offset) (fromIntegral fst_size)

  return $ fromFiles $
    [ boot_bin
    , bi2_bin
    , apploader_img
    , main_dol
    , fst_bin
    ] <> normalFiles
