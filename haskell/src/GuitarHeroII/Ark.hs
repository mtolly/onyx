{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module GuitarHeroII.Ark (replaceSong, GameGH(..), detectGameGH, readFileEntries, extractArk, createHdrArk) where

import           ArkTool
import           Control.Monad   (forM_, replicateM, forM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.DTA        as D
import qualified System.IO       as IO
import           System.IO.Temp  (withSystemTempFile)
import Amplitude.PS2.Ark (FileEntry(..), extractArk, FoundFile(..), traverseFolder, makeStringBank)
import Data.Binary.Get (runGet, getInt32le, getWord32le)
import qualified Data.HashMap.Strict as HM
import Data.Binary.Put (runPut, putWord32le, putInt32le)
import Data.Foldable (toList)
import Data.List.Extra (nubOrd)
import Codec.Compression.GZip (decompress)

-- Haskell extract/repack stuff

readFileEntries :: FilePath -> IO [FileEntry]
readFileEntries hdr = IO.withBinaryFile hdr IO.ReadMode $ \h -> do
  let w32 = runGet getWord32le . BL.fromStrict <$> B.hGet h 4
  3 <- w32
  arkCount <- w32
  arkCount2 <- w32
  -- arkCount should equal arkCount2
  arkSizes <- replicateM (fromIntegral arkCount) w32
  stringSize <- w32
  stringBytes <- B.hGet h $ fromIntegral stringSize
  offsetCount <- w32
  offsets <- replicateM (fromIntegral offsetCount) w32
  entryCount <- w32
  entryBytes <- B.hGet h $ fromIntegral entryCount * 20
  let findString i = B.takeWhile (/= 0) $ B.drop (fromIntegral $ offsets !! fromIntegral i) stringBytes
  return $ flip runGet (BL.fromStrict entryBytes) $ replicateM (fromIntegral entryCount) $ do
    fe_offset <- getWord32le
    fe_name <- findString <$> getInt32le
    fe_folder <- (\i -> if i == -1 then Nothing else Just $ findString i) <$> getInt32le
    fe_size <- getWord32le
    fe_inflate <- getWord32le
    return FileEntry{..}

createHdrArk :: FilePath -> FilePath -> FilePath -> IO ()
createHdrArk dout hdr ark = do
  files <- traverseFolder dout
  let strings = nubOrd $ files >>= \ff -> ff_arkName ff : toList (ff_arkParent ff)
      (stringBank, offsets) = makeStringBank strings
      stringToIndex = HM.fromList $ zip strings [0..]
      getStringIndex str = case HM.lookup str stringToIndex of
        Nothing -> fail "panic! couldn't find string offset in bank"
        Just i  -> return i
      fileCount = length files
      stringCount = HM.size stringToIndex
  (entries, arkSize) <- IO.withBinaryFile ark IO.WriteMode $ \h -> do
    entries <- forM files $ \ff -> do
      posn <- IO.hTell h
      contents <- BL.fromStrict <$> B.readFile (ff_onDisk ff)
      BL.hPut h contents
      return FileEntry
        { fe_offset = fromIntegral posn
        , fe_name = ff_arkName ff
        , fe_folder = ff_arkParent ff
        , fe_size = fromIntegral $ BL.length contents
        , fe_inflate = if ff_gzip ff -- should always be false in GH files
          then fromIntegral $ BL.length $ decompress contents
          else 0
        }
    arkSize <- IO.hTell h
    return (entries, arkSize)
  IO.withBinaryFile hdr IO.WriteMode $ \h -> do
    let w32 = BL.hPut h . runPut . putWord32le
        i32 = BL.hPut h . runPut . putInt32le
    w32 3
    w32 1
    w32 1
    w32 $ fromIntegral arkSize
    w32 $ fromIntegral $ BL.length stringBank
    BL.hPut h stringBank
    w32 $ fromIntegral stringCount
    mapM_ w32 offsets
    w32 $ fromIntegral fileCount
    forM_ entries $ \entry -> do
      w32 $ fe_offset entry
      getStringIndex (fe_name entry) >>= i32
      maybe (return (-1)) getStringIndex (fe_folder entry) >>= i32
      w32 $ fe_size entry
      w32 $ fe_inflate entry

-- ArkTool stuff

data GameGH = GameGH1 | GameGH2

-- TODO: find better way to do this.
-- for comparison, Guitar Wizard looked for existence of
-- "songs/aceofspades/aceofspades.mid", etc., which seems less resilient
detectGameGH
  :: FilePath
  -> IO (Maybe GameGH)
detectGameGH gen = withArk gen $ \ark -> do
  searchFiles ark "songs/*/*.mid" >>= let
    go []           = return Nothing
    go (ent : rest) = do
      midInArk <- ark_Arkname ent
      withSystemTempFile "ghtest.mid" $ \fmid hdl -> do
        IO.hClose hdl
        ark_GetFile' ark fmid midInArk True
        -- could parse whole midi but this is fine
        bs <- B.readFile fmid
        if
          | "T1 GEMS"     `B.isInfixOf` bs -> return $ Just GameGH1
          | "PART GUITAR" `B.isInfixOf` bs -> return $ Just GameGH2
          | otherwise                      -> go rest
    in go

-- | Replaces a song in Guitar Hero II (U).
replaceSong
  :: FilePath -- ^ the @GEN@ folder containing ark and hdr files
  -> B.ByteString -- ^ the folder name (and DTB symbol) of the song to replace
  -> [D.Chunk B.ByteString] -- ^ the DTB snippet to insert into @config\/gen\/songs.dtb@
  -> [(B.ByteString, FilePath)] -- ^ the files to copy into the song folder, e.g. @("yyz.mid", "some/dir/notes.mid")@
  -> IO ()
replaceSong gen sym snippet files = withArk gen $ \ark -> do
  withSystemTempFile "songs.dtb" $ \fdtb hdl -> do
    IO.hClose hdl
    ark_GetFile' ark fdtb "config/gen/songs.dtb" True
    D.DTA z (D.Tree _ chunks) <- D.readFileDTB fdtb
    let adjust chunk = case chunk of
          D.Parens (D.Tree _ (D.Sym k : _)) | k == sym ->
            D.Parens $ D.Tree 0 $ D.Sym k : map adjustSnippet snippet
          _ -> chunk
        adjustSnippet = \case
          D.String s -> D.String $ adjustString s
          D.Parens t -> D.Parens $ adjustTree t
          D.Braces t -> D.Braces $ adjustTree t
          D.Brackets t -> D.Brackets $ adjustTree t
          chunk -> chunk
        adjustTree (D.Tree tid cks) = D.Tree tid $ map adjustSnippet cks
        adjustString str = case B.breakSubstring "$SONGKEY" str of
          (h, t) | not $ B.null t -> adjustString $ h <> sym <> B.drop 8 t
          _                       -> str
    D.writeFileDTB fdtb $ D.renumberFrom 1 $ D.DTA z $ D.Tree 0 $ map adjust chunks
    ark_ReplaceAFile' ark fdtb "config/gen/songs.dtb" True
    entries <- searchFiles ark $ "songs/" <> sym <> "/*"
    forM_ entries $ \entry -> do
      name <- ark_Arkname entry
      ark_RemoveFile' ark name
    forM_ files $ \(arkName, localPath) -> do
      let arkPath = "songs/" <> sym <> "/" <> arkName
      ark_AddFile' ark localPath arkPath True -- encryption doesn't matter
    ark_Save' ark
