{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module GuitarHeroII.Ark (replaceSong, GameGH(..), detectGameGH) where

import           ArkTool
import           Control.Monad   (forM_)
import qualified Data.ByteString as B
import qualified Data.DTA        as D
import           Data.Monoid     ((<>))
import qualified System.IO       as IO
import           System.IO.Temp  (withSystemTempFile)

data GameGH = GameGH1 | GameGH2

-- TODO: find better way to do this.
-- for comparison, Guitar Wizard looked for existence of
-- "songs/aceofspades/aceofspades.mid", etc., which seems less resilient
detectGameGH
  :: FilePath
  -> IO (Maybe GameGH)
detectGameGH gen = withArk gen $ \ark -> do
  searchFiles ark "songs/*/*.mid" >>= \case
    []      -> return Nothing
    ent : _ -> do
      midInArk <- ark_Arkname ent
      withSystemTempFile "ghtest.mid" $ \fmid hdl -> do
        IO.hClose hdl
        ark_GetFile' ark fmid midInArk True
        -- could parse whole midi but this is fine
        bs <- B.readFile fmid
        return $ if
          | "T1 GEMS"     `B.isInfixOf` bs -> Just GameGH1
          | "PART GUITAR" `B.isInfixOf` bs -> Just GameGH2
          | otherwise                      -> Nothing

-- | Replaces a song in Guitar Hero II (U).
replaceSong
  :: FilePath -- ^ the @GEN@ folder containing ark and hdr files
  -> B.ByteString -- ^ the folder name (and DTB key) of the song to replace
  -> [D.Chunk B.ByteString] -- ^ the DTB snippet to insert into @config\/gen\/songs.dtb@
  -> [(B.ByteString, FilePath)] -- ^ the files to copy into the song folder, e.g. @("yyz.mid", "some/dir/notes.mid")@
  -> IO ()
replaceSong gen key snippet files = withArk gen $ \ark -> do
  withSystemTempFile "songs.dtb" $ \fdtb hdl -> do
    IO.hClose hdl
    ark_GetFile' ark fdtb "config/gen/songs.dtb" True
    D.DTA z (D.Tree _ chunks) <- D.readFileDTB fdtb
    let adjust chunk = case chunk of
          D.Parens (D.Tree _ (D.Key k : _)) | k == key ->
            D.Parens $ D.Tree 0 $ D.Key k : map adjustSnippet snippet
          _ -> chunk
        adjustSnippet = \case
          D.String s -> D.String $ adjustString s
          D.Parens t -> D.Parens $ adjustTree t
          D.Braces t -> D.Braces $ adjustTree t
          D.Brackets t -> D.Brackets $ adjustTree t
          chunk -> chunk
        adjustTree (D.Tree tid cks) = D.Tree tid $ map adjustSnippet cks
        adjustString str = case B.breakSubstring "$SONGKEY" str of
          (h, t) | not $ B.null t -> adjustString $ h <> key <> B.drop 8 t
          _                       -> str
    D.writeFileDTB fdtb $ D.renumberFrom 1 $ D.DTA z $ D.Tree 0 $ map adjust chunks
    ark_ReplaceAFile' ark fdtb "config/gen/songs.dtb" True
    entries <- searchFiles ark $ "songs/" <> key <> "/*"
    forM_ entries $ \entry -> do
      name <- ark_Arkname entry
      ark_RemoveFile' ark name
    forM_ files $ \(arkName, localPath) -> do
      let arkPath = "songs/" <> key <> "/" <> arkName
      ark_AddFile' ark localPath arkPath True -- encryption doesn't matter
    ark_Save' ark
