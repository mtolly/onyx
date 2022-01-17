{-
Process for extracting and repacking already-valid Rock Band songs without a whole import/export.
- Create packs from songs
- Extract packs into songs
- Change platform: 360 <-> PS3, eventually also Wii
- Certain changes to MIDI: black venue, remove OD, remove lanes, etc.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module QuickConvert where

import Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.StackTrace
import           Data.Bifunctor                 (first)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toLower)
import           Data.DTA                       (readDTASections)
import qualified Data.DTA.Serialize.RB3         as D
import           Data.Foldable                  (toList)
import           Data.SimpleHandle
import qualified Data.Text                      as T
import Data.Hashable (hash)
import qualified Data.Text.Encoding             as TE
import           Image                          (swapPNGXboxPS3)
import           MoggDecrypt                    (encryptRB1)
import Resources (getResourcesPath)
import           NPData
import           PlayStation.PKG
import           PrettyDTA                      (DTASingle (..), readDTASingle)
import           STFS.Package
import           System.FilePath                (dropExtension, splitExtension,
                                                 takeExtension, (</>))
import           System.IO                      (hFileSize)

data QuickSong = QuickSong
  { quickSongDTA    :: B.ByteString
  , quickSongFolder :: T.Text
  , quickSongFiles  :: Folder T.Text (QuickFile, Readable)
  }

data QuickFile
  = QFEncryptedMOGG
  | QFUnencryptedMOGG
  | QFMilo
  | QFPNGXbox
  | QFPNGPS3
  | QFEncryptedMIDI B.ByteString -- folder for decryption
  | QFUnencryptedMIDI
  | QFOther
  deriving (Eq, Show)

-- Returns [(song's dta section, folder under "songs/" that has the song)]
findSongs :: (SendMessage m, MonadIO m) => Readable -> StackTraceT m [(B.ByteString, T.Text)]
findSongs r = do
  bs <- fmap BL.toStrict $ stackIO $ useHandle r handleToByteString
  sections <- readDTASections bs
  forM sections $ \pair@(bytes, _chunk) -> do
    (single, _) <- readDTASingle pair
    let base = D.songName $ D.song $ dtaSongPackage single
    case T.splitOn "/" base of
      ["songs", x, _] -> return (bytes, x)
      _               -> fatal $
        "Unrecognized format of name parameter in songs.dta, expected songs/x/y but got: " <> show base

detectMOGG :: (MonadIO m) => Readable -> StackTraceT m (QuickFile, Readable)
detectMOGG r = do
  moggType <- stackIO $ useHandle r $ \h -> B.hGet h 1
  let qfile = if B.unpack moggType == [0xA] then QFUnencryptedMOGG else QFEncryptedMOGG
  return (qfile, r)

detectMIDI :: (MonadIO m) => B.ByteString -> Readable -> StackTraceT m (QuickFile, Readable)
detectMIDI packageFolder r = do
  magic <- stackIO $ useHandle r $ \h -> B.hGet h 1
  let qfile = if magic == "MThd" then QFUnencryptedMIDI else QFEncryptedMIDI packageFolder
  return (qfile, r)

mapMFilesWithName :: (Monad m) => ((T.Text, a) -> m (T.Text, b)) -> Folder T.Text a -> m (Folder T.Text b)
mapMFilesWithName f dir = do
  newFiles <- mapM f $ folderFiles dir
  newSubs  <- forM (folderSubfolders dir) $ \(subName, sub) -> do
    sub' <- mapMFilesWithName f sub
    return (subName, sub')
  return Folder { folderFiles = newFiles, folderSubfolders = newSubs }

loadInput :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m [QuickSong]
loadInput fin = case map toLower $ takeExtension fin of
  ".pkg" -> do
    pkg <- stackIO $ loadPKG fin
    usr <- case findFolder ["USRDIR"] $ pkgFolder pkg of
      Just dir -> return dir
      Nothing  -> fatal "No USRDIR found inside .pkg"
    fmap concat $ forM (folderSubfolders usr) $ \(packageName, packageFolder) -> do
      let textFolder = first TE.decodeLatin1 packageFolder
      songsFolder <- case findFolder ["songs"] textFolder of
        Just dir -> return dir
        Nothing -> fatal $ "No songs folder found inside .pkg for: " <> show packageName
      dta <- case findFile (pure "songs.dta") songsFolder of
        Just r -> return r
        Nothing -> fatal $ "No songs.dta found inside .pkg for: " <> show packageName
      songRefs <- findSongs dta
      forM songRefs $ \(dtaPart, subName) -> do
        songFolder <- case findFolder [subName] songsFolder of
          Just dir -> return dir
          Nothing -> fatal $ "Song folder not found in .pkg for " <> show (packageName, subName)
        let identifyFile (name, r) = case map toLower $ takeExtension $ T.unpack name of
              ".mogg"     -> (name,) <$> detectMOGG r
              ".milo_ps3" -> return (name, (QFMilo, r))
              ".png_ps3"  -> return (name, (QFPNGPS3, r))
              ".edat"     -> (name,) <$> detectMIDI packageName r
              _           -> return (name, (QFOther, r))
        identified <- mapMFilesWithName identifyFile songFolder
        return QuickSong
          { quickSongDTA    = dtaPart
          , quickSongFolder = subName
          , quickSongFiles  = identified
          }
  _ -> do
    conFolder <- stackIO $ getSTFSFolder fin
    songsFolder <- case findFolder ["songs"] conFolder of
      Just dir -> return dir
      Nothing  -> fatal "No songs folder found inside CON/LIVE"
    dta <- case findFile (pure "songs.dta") songsFolder of
      Just r  -> return r
      Nothing -> fatal "No songs.dta found inside CON/LIVE"
    songRefs <- findSongs dta
    forM songRefs $ \(dtaPart, subName) -> do
      songFolder <- case findFolder [subName] songsFolder of
        Just dir -> return dir
        Nothing -> fatal $ "Song folder not found in CON/LIVE: " <> show subName
      let identifyFile (name, r) = case map toLower $ takeExtension $ T.unpack name of
            ".mogg"      -> (name,) <$> detectMOGG r
            ".milo_xbox" -> return (name, (QFMilo, r))
            ".png_xbox"  -> return (name, (QFPNGXbox, r))
            ".mid"       -> return (name, (QFUnencryptedMIDI, r))
            _            -> return (name, (QFOther, r))
      identified <- mapMFilesWithName identifyFile songFolder
      return QuickSong
        { quickSongDTA    = dtaPart
        , quickSongFolder = subName
        , quickSongFiles  = identified
        }

decryptEDAT :: (MonadIO m, SendMessage m) => B.ByteString -> B.ByteString -> Readable -> StackTraceT m Readable
decryptEDAT crypt name r = tryDecryptEDAT crypt name r >>= \case
  Just r' -> return r'
  Nothing -> fatal $ "Couldn't decrypt " <> show name

encryptEDAT :: (MonadResource m) => B.ByteString -> B.ByteString -> Bool -> Readable -> StackTraceT m Readable
encryptEDAT crypt name rb3 r = tempDir "onyxquickconv" $ \tmp -> stackIO $ do
  let cfg = if rb3
        then rb3CustomMidEdatConfig crypt
        else rb2CustomMidEdatConfig crypt
      tmpIn  = tmp </> "in.mid"
      tmpOut = tmp </> "out.mid.edat"
  saveReadable r tmpIn
  packNPData cfg tmpIn tmpOut name
  makeHandle (B8.unpack name) . byteStringSimpleHandle . BL.fromStrict <$> B.readFile tmpOut

getXboxFile :: (MonadIO m, SendMessage m) => (T.Text, (QuickFile, Readable)) -> StackTraceT m (T.Text, Readable)
getXboxFile (name, (qfile, r)) = case qfile of
  QFEncryptedMOGG -> return ok
  QFUnencryptedMOGG -> return ok
  QFMilo -> let
    name' = T.pack $ dropExtension (T.unpack name) <> ".milo_xbox"
    in return (name', r)
  QFPNGXbox -> return ok
  QFPNGPS3 -> do
    let name' = T.pack $ dropExtension (T.unpack name) <> ".png_xbox"
    bs <- stackIO $ useHandle r handleToByteString
    let r' = makeHandle (T.unpack name') $ byteStringSimpleHandle $ swapPNGXboxPS3 bs
    return (name', r')
  QFEncryptedMIDI crypt -> do
    let name' = case splitExtension $ T.unpack name of
          (base, ".edat") -> T.pack base
          _               -> name
    r' <- decryptEDAT crypt (TE.encodeUtf8 name) r
    return (name', r')
  QFUnencryptedMIDI -> return ok
  QFOther -> return ok
  where ok = (name, r)

getPS3File :: (MonadResource m, SendMessage m) => Maybe (B.ByteString, Bool) -> (T.Text, (QuickFile, Readable)) -> StackTraceT m (T.Text, Readable)
getPS3File mcrypt (name, (qfile, r)) = case qfile of
  QFEncryptedMOGG -> return ok
  QFUnencryptedMOGG -> tempDir "onyxquickconv" $ \tmp -> stackIO $ do
    let tmpIn  = tmp </> "in.mogg"
        tmpOut = tmp </> "out.mogg"
    saveReadable r tmpIn
    encryptRB1 tmpIn tmpOut
    r' <- makeHandle (T.unpack name) . byteStringSimpleHandle . BL.fromStrict <$> B.readFile tmpOut
    return (name, r')
  QFMilo -> let
    name' = T.pack $ dropExtension (T.unpack name) <> ".milo_ps3"
    in return (name', r)
  QFPNGXbox -> stackIO $ do
    let name' = T.pack $ dropExtension (T.unpack name) <> ".png_ps3"
    bs <- useHandle r handleToByteString
    let r' = makeHandle (T.unpack name') $ byteStringSimpleHandle $ swapPNGXboxPS3 bs
    return (name', r')
  QFPNGPS3 -> return ok
  QFEncryptedMIDI crypt -> do
    r' <- case mcrypt of
      Nothing              -> decryptEDAT crypt (TE.encodeUtf8 name) r
      Just (newCrypt, rb3) -> if crypt == newCrypt
        then return r -- fine as is
        else decryptEDAT crypt (TE.encodeUtf8 name) r >>= encryptEDAT newCrypt (TE.encodeUtf8 name) rb3
    return (name, r')
  QFUnencryptedMIDI -> do
    let name' = case takeExtension $ T.unpack name of
          ".edat" -> name
          _       -> name <> ".edat"
    r' <- case mcrypt of
      Nothing           -> return r
      Just (crypt, rb3) -> encryptEDAT crypt (TE.encodeUtf8 name') rb3 r
    return (name', r')
  QFOther -> return ok
  where ok = (name, r)

makePS3DTA :: BL.ByteString -> BL.ByteString
makePS3DTA = id -- TODO set rating 4 -> 2, and maybe numeric song_id

saveQuickSongsSTFS :: (MonadIO m, SendMessage m) => [QuickSong] -> CreateOptions -> FilePath -> StackTraceT m ()
saveQuickSongsSTFS qsongs opts fout = do
  xboxFolders <- forM qsongs $ \qsong -> do
    xboxFolder <- mapMFilesWithName getXboxFile $ quickSongFiles qsong
    return (quickSongFolder qsong, xboxFolder)
  let songsFolder = Folder
        { folderSubfolders = xboxFolders
        , folderFiles = [("songs.dta", songsDTA)]
        }
      topFolder = Folder
        { folderSubfolders = [("songs", songsFolder)]
        , folderFiles = []
        }
      songsDTA = makeHandle "songs.dta" $ byteStringSimpleHandle
        $ BL.intercalate "\n" $ map (BL.fromStrict . quickSongDTA) qsongs
  stackIO $ makeCONReadable opts topFolder fout

saveQuickSongsPKG :: (MonadResource m, SendMessage m) => [QuickSong] -> Bool -> Bool -> FilePath -> StackTraceT m ()
saveQuickSongsPKG qsongs rb3 encryptMid fout = do
  let packageFolder = T.pack $ take 0x1B $ "OQC" <> show (hash $ map quickSongDTA qsongs)
      crypt         = TE.encodeUtf8 packageFolder
      contentID     = npdContentID
        $ (if rb3 then rb3CustomMidEdatConfig else rb2CustomMidEdatConfig) crypt
  ps3Folders <- forM qsongs $ \qsong -> do
    let mcrypt = guard encryptMid >> Just (crypt, rb3)
    ps3Folder <- mapMFilesWithName (getPS3File mcrypt) $ quickSongFiles qsong
    return (quickSongFolder qsong, ps3Folder)
  let rootFolder = container "USRDIR" $ container packageFolder $ container "songs" Folder
        { folderSubfolders = ps3Folders
        , folderFiles = [("songs.dta", songsDTA)]
        }
      container name inner = Folder { folderSubfolders = [(name, inner)], folderFiles = [] }
      songsDTA = makeHandle "songs.dta" $ byteStringSimpleHandle
        $ BL.intercalate "\n" $ map (makePS3DTA . BL.fromStrict . quickSongDTA) qsongs
  stackIO $ do
    extraPath <- getResourcesPath $ if rb3 then "pkg-contents/rb3" else "pkg-contents/rb2"
    extra <- crawlFolder extraPath
    makePKG contentID (first TE.encodeUtf8 $ rootFolder <> extra) >>= BL.writeFile fout

contentsSize :: QuickSong -> IO Integer
contentsSize qsong = do
  fileSizes <- mapM (\(_, r) -> useHandle r hFileSize) $ toList $ quickSongFiles qsong
  return $ sum fileSizes

-- Organizes based on combined size of song files.
-- Should add some buffer to account for package overhead and platform conversion.
organizePacks :: Integer -> [QuickSong] -> IO [[QuickSong]]
organizePacks maxSize qsongs = do
  songSizes <- mapM contentsSize qsongs
  let go :: Integer -> [QuickSong] -> [(Integer, QuickSong)] -> [[QuickSong]]
      go curSize curSongs incoming = case incoming of
        [] -> case curSongs of
          []    -> []
          _ : _ -> [curSongs]
        (nextSize, nextSong) : rest -> let
          newSize = curSize + nextSize
          in if newSize > maxSize
            then curSongs : go 0 [] incoming
            else go newSize (nextSong : curSongs) rest
  return $ go 0 [] $ zip songSizes qsongs

saveQuickPacksSTFS :: (MonadIO m, SendMessage m) => Integer -> [QuickSong] -> (Int -> (CreateOptions, FilePath)) -> StackTraceT m ()
saveQuickPacksSTFS maxSize qsongs getTitleFile = do
  packs <- stackIO $ organizePacks maxSize qsongs
  forM_ (zip [1..] packs) $ \(i, pack) -> do
    let (opts, fout) = getTitleFile i
    saveQuickSongsSTFS pack opts fout

{-

MIDI edits: Black Venue, No Overdrive, No Lanes

-----------------

Transform in place (each input is replaced with a file, package type preserved)
  for pkg, don't change encryption folders
  2nd row: enc/unenc midi select
  3rd row: go button
One to one (each input file gets one new output file, of a single package type)
  for pkg->pkg, don't change encryption folders
  2nd row: con/pkg/live, if pkg then (enc/unenc midi select, separate/combined songs.dta select)
  3rd row: template box with default as %dir%/%base%-convert(.pkg), go button
Make packs (songs are combined into packs, up to a maximum size)
  2nd row: con/pkg/live, if pkg then (enc/unenc midi select, separate/combined songs.dta select)
  3rd row: max size, go button (opens a save-as dialog)
Make songs (each song gets one new output file)
  2nd row: con/pkg/live, if pkg then (enc/unenc midi select)
  3rd row: go button (opens pick folder dialog, folders get auto names inside)

-}
