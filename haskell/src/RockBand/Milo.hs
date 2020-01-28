{-
Thanks to PyMilo, LibForge, and MiloMod for information on these structures.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module RockBand.Milo
( module RockBand.Milo.Compression
, module RockBand.Milo.Dir
, module RockBand.Milo.Lipsync
, module RockBand.Milo.SongPref
, module RockBand.Milo.Venue
, unpackMilo
, packMilo
) where

import           Control.Monad                  (forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.StackTrace
import           Data.Binary.Get                (runGetOrFail)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import           RockBand.Milo.Compression
import           RockBand.Milo.Dir
import           RockBand.Milo.Lipsync
import           RockBand.Milo.SongPref
import           RockBand.Milo.Venue
import qualified System.Directory               as Dir
import           System.FilePath                ((<.>), (</>))

pieceName :: Int -> String
pieceName n = let
  num = case show n of
    [c]      -> ['0', '0', c]
    [c1, c2] -> ['0', c1, c2]
    s        -> s
  in "piece" <> num <.> "bin"

-- | Decompresses a milo and tries to parse the directory structure inside.
-- Extracts either the parsed structure and its files, or the raw split pieces.
unpackMilo :: (MonadIO m, SendMessage m) => FilePath -> FilePath -> StackTraceT m ()
unpackMilo fin dout = do
  bs <- stackIO $ BL.readFile fin
  dec <- case runGetOrFail decompressMilo bs of
    Left (_, pos, err) -> fatal $ "Failed to decompress the milo. Error at position " <> show pos <> ": " <> err
    Right (_, _, x)    -> return x
  stackIO $ Dir.createDirectoryIfMissing False dout
  stackIO $ BL.writeFile (dout </> "decompressed.bin") dec
  case runGetOrFail parseMiloFile dec of
    Left (_, pos, err) -> do
      stackIO $ forM_ (zip [0..] $ breakMilo $ BL.toStrict dec) $ \(i, piece) -> do
        B.writeFile (dout </> pieceName i) piece
      inside "Parsing .milo structure" $ inside ("position " <> show pos) $ warn err
      lg "Couldn't parse milo structure; split into simple pieces."
    Right (_, _, topdir) -> do
      let go parent milodir = do
            let thisout = parent </> B8.unpack (miloName milodir)
            Dir.createDirectoryIfMissing False thisout
            forM_ (zip (miloEntryNames milodir) (miloFiles milodir)) $ \((_typ, name), fileBytes) -> do
              BL.writeFile (thisout </> B8.unpack name) fileBytes
            mapM_ (go thisout) $ miloSubdirs milodir
          removeFiles dir = dir
            { miloSubdirs = map removeFiles $ miloSubdirs dir
            , miloFiles = map (const "(file contents)") $ miloFiles dir
            }
      stackIO $ go dout topdir
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
