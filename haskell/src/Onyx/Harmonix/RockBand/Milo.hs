{-
Thanks to PyMilo, LibForge, and MiloMod for information on these structures.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Harmonix.RockBand.Milo
( module Onyx.Harmonix.RockBand.Milo.Compression
, module Onyx.Harmonix.RockBand.Milo.Dir
, module Onyx.Harmonix.RockBand.Milo.Lipsync
, module Onyx.Harmonix.RockBand.Milo.SongPref
, module Onyx.Harmonix.RockBand.Milo.Venue
, unpackMilo
, packMilo
, loadMilo
, miloToFolder
) where

import           Control.Monad                           (forM_)
import           Control.Monad.IO.Class
import           Data.Bifunctor                          (bimap)
import           Data.Binary.Get                         (runGetOrFail)
import qualified Data.ByteString                         as B
import qualified Data.ByteString.Char8                   as B8
import qualified Data.ByteString.Lazy                    as BL
import qualified Data.Text.Encoding                      as TE
import           Onyx.Harmonix.RockBand.Milo.Compression
import           Onyx.Harmonix.RockBand.Milo.Dir
import           Onyx.Harmonix.RockBand.Milo.Lipsync
import           Onyx.Harmonix.RockBand.Milo.SongPref
import           Onyx.Harmonix.RockBand.Milo.Venue
import           Onyx.StackTrace
import           Onyx.Util.Handle                        (Folder (..),
                                                          byteStringSimpleHandle,
                                                          makeHandle,
                                                          saveHandleFolder)
import           Onyx.Xbox.STFS                          (runGetM)
import qualified System.Directory                        as Dir
import           System.FilePath                         ((<.>), (</>))

pieceName :: Int -> String
pieceName n = let
  num = case show n of
    [c]      -> ['0', '0', c]
    [c1, c2] -> ['0', c1, c2]
    s        -> s
  in "piece" <> num <.> "bin"

loadMilo :: (MonadIO m) => FilePath -> StackTraceT m MiloDir
loadMilo fin = do
  bs <- stackIO $ BL.readFile fin
  dec <- inside ("Decompressing milo file: " <> fin) $ runGetM decompressMilo bs
  inside ("Parsing milo file: " <> fin) $ runGetM parseMiloFile dec

miloToFolder :: MiloDir -> (B.ByteString, Folder B.ByteString BL.ByteString)
miloToFolder dir = (miloName dir, Folder
  { folderFiles      = zip (map snd $ miloEntryNames dir) (miloFiles dir)
  , folderSubfolders = map miloToFolder $ miloSubdirs dir
  })

-- | Decompresses a milo and tries to parse the directory structure inside.
-- Extracts either the parsed structure and its files, or the raw split pieces.
unpackMilo :: (MonadIO m, SendMessage m) => FilePath -> FilePath -> StackTraceT m ()
unpackMilo fin dout = do
  bs <- stackIO $ BL.readFile fin
  dec <- inside ("Decompressing milo file: " <> fin) $ runGetM decompressMilo bs
  stackIO $ Dir.createDirectoryIfMissing False dout
  stackIO $ BL.writeFile (dout </> "decompressed.bin") dec
  case runGetOrFail parseMiloFile dec of
    Left (_, pos, err) -> do
      stackIO $ forM_ (zip [0..] $ breakMilo $ BL.toStrict dec) $ \(i, piece) -> do
        B.writeFile (dout </> pieceName i) piece
      inside "Parsing .milo structure" $ inside ("position " <> show pos) $ warn err
      lg "Couldn't parse milo structure; split into simple pieces."
    Right (_, _, topdir) -> do
      let (topname, folder) = miloToFolder topdir
          folder' = bimap TE.decodeLatin1 (makeHandle "" . byteStringSimpleHandle) folder
      stackIO $ saveHandleFolder folder' $ dout </> B8.unpack topname
      let removeFiles dir = dir
            { miloSubdirs = map removeFiles $ miloSubdirs dir
            , miloFiles = map (const "(file contents)") $ miloFiles dir
            }
      stackIO $ writeFile (dout </> "parsed.txt") $ show $ removeFiles topdir
      lg "Recognized and extracted milo contents."

-- | Repacks a folder previously unpacked by 'unpackMilo'.
packMilo :: (MonadIO m, SendMessage m) => FilePath -> FilePath -> StackTraceT m ()
packMilo din fout = do
  bs <- stackIO (Dir.doesFileExist $ din </> "piece000.bin") >>= \case
    True -> stackIO $ do
      -- glue raw pieces together
      let findPieces n = Dir.doesFileExist (din </> pieceName n) >>= \case
            True  -> findPieces $ n + 1
            False -> return n
      count <- findPieces 0
      pieces <- mapM (BL.readFile . (din </>) . pieceName) [0 .. count - 1]
      return $ BL.intercalate (BL.pack magicBarrier) pieces
    False -> do
      dec <- stackIO $ BL.readFile $ din </> "decompressed.bin"
      case runGetOrFail parseMiloFile dec of
        Left (_, pos, err) -> inside "Re-parsing milo decompressed.bin"
          $ inside ("position " <> show pos) $ fatal err
        Right (_, _, topdir) -> do
          let reload parent milodir = do
                let thisout = parent </> B8.unpack (miloName milodir)
                newFiles <- mapM (BL.readFile . (thisout </>) . B8.unpack . snd)
                  $ miloEntryNames milodir
                newDirs <- mapM (reload thisout) $ miloSubdirs milodir
                return milodir
                  { miloFiles = newFiles
                  , miloSubdirs = newDirs
                  }
          topdir' <- stackIO $ reload din topdir
          return $ makeMiloFile topdir'
  stackIO $ BL.writeFile fout $ addMiloHeader bs
