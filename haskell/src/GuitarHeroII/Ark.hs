{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module GuitarHeroII.Ark (replaceSong) where

import           ArkTool
import           Control.Exception (bracket, bracket_)
import           Control.Monad     (forM_, unless)
import qualified Data.ByteString   as B
import qualified Data.DTA          as D
import           Data.Monoid       ((<>))
import qualified System.IO         as IO
import           System.IO.Temp    (withSystemTempFile)

-- | Replaces a song in Guitar Hero II (U).
replaceSong
  :: FilePath -- ^ the @GEN@ folder containing ark and hdr files
  -> B.ByteString -- ^ the folder name (and DTB key) of the song to replace
  -> [D.Chunk B.ByteString] -- ^ the DTB snippet to insert into @config/gen/songs.dtb@
  -> [(B.ByteString, FilePath)] -- ^ the files to copy into the song folder, e.g. @("yyz.mid", "some/dir/notes.mid")@
  -> IO ()
replaceSong gen key snippet files = do
  let wrap msg f = f >>= \b -> unless b $ fail msg
  bracket ark_new ark_delete $ \ark -> do
    let open = wrap "Couldn't open the ARK file" $ ark_Open ark gen
    bracket_ open (ark_Close ark) $ do
      withSystemTempFile "songs.dtb" $ \fdtb hdl -> do
        IO.hClose hdl
        wrap "Couldn't find songs.dtb in the ARK." $
          ark_GetFile ark fdtb "config/gen/songs.dtb" True
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
              _ -> str
        D.writeFileDTB fdtb $ D.renumberFrom 1 $ D.DTA z $ D.Tree 0 $ map adjust chunks
        wrap "Couldn't update songs.dtb in the ARK." $
          ark_ReplaceAFile ark fdtb "config/gen/songs.dtb" True
        entries <- searchFiles ark $ "songs/" <> key <> "/*"
        forM_ entries $ \entry -> do
          name <- ark_Arkname entry
          wrap ("Couldn't remove file from ARK: " <> show name) $
            ark_RemoveFile ark name
        forM_ files $ \(arkName, localPath) -> do
          let arkPath = "songs/" <> key <> "/" <> arkName
          wrap ("Couldn't add file " <> show localPath <> " to ARK as " <> show arkPath) $
            ark_AddFile ark localPath arkPath True -- encryption doesn't matter
        wrap "Couldn't save the updated ARK file." $ ark_Save ark
