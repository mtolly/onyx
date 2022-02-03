{-
Process for extracting and repacking already-valid Rock Band songs without a whole import/export.
- Create packs from songs
- Extract packs into songs
- Change platform: 360 <-> PS3, eventually also Wii
- Certain changes to MIDI: black venue, remove OD, remove lanes, etc.
-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module QuickConvert where

import           Control.Monad
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Resource   (MonadResource)
import           Control.Monad.Trans.StackTrace
import           Data.Bifunctor                 (first)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toLower)
import           Data.DTA                       (Chunk (..), DTA (..),
                                                 Tree (..), readDTASections,
                                                 showDTA)
import           Data.Foldable                  (toList)
import           Data.Hashable                  (hash)
import qualified Data.Map                       as Map
import           Data.Maybe                     (catMaybes)
import           Data.SimpleHandle
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Image                          (swapPNGXboxPS3)
import           MoggDecrypt                    (encryptRB1)
import           NPData
import           PlayStation.PKG
import           Resources                      (getResourcesPath)
import qualified Sound.MIDI.File                as F
import qualified Sound.MIDI.File.Save           as Save
import           STFS.Package
import           System.FilePath                (dropExtension, splitExtension,
                                                 takeExtension, (</>))
import           System.IO                      (hFileSize)

data QuickSong = QuickSong
  { quickSongDTA       :: QuickDTA
  , quickSongFiles     :: Folder T.Text (QuickFile Readable) -- contents of 'foo'
  , quickSongPS3Folder :: Maybe B.ByteString                 -- if came from ps3, this is the USRDIR subfolder (used for .mid.edat encryption)
  }

data QuickDTA = QuickDTA
  { qdtaRaw    :: B.ByteString
  , qdtaParsed :: Chunk B.ByteString
  , qdtaFolder :: T.Text -- in 'songs/foo/bar.mid', this is 'foo'
  , qdtaRB3    :: Bool
  }

data QuickConvertFormat
  = QCFormatCON
  | QCFormatLIVE
  | QCFormatPKG

data QuickInput = QuickInput
  { quickInputPath   :: FilePath
  , quickInputFormat :: QuickConvertFormat
  , quickInputXbox   :: Maybe Metadata
  , quickInputSongs  :: [QuickSong]
  }

data QuickFile a
  = QFEncryptedMOGG a
  | QFUnencryptedMOGG a
  | QFMilo a
  | QFPNGXbox a
  | QFPNGPS3 a
  | QFEncryptedMIDI B.ByteString a -- folder for decryption
  | QFUnencryptedMIDI a
  | QFParsedMIDI F.T
  | QFOther a
  deriving (Eq, Show, Foldable)

pattern Key :: s -> [Chunk s] -> Chunk s
pattern Key k v <- Parens (Tree _ (Sym k : v))

findSongs :: (SendMessage m, MonadIO m) => Readable -> StackTraceT m [QuickDTA]
findSongs r = do
  bs <- fmap BL.toStrict $ stackIO $ useHandle r handleToByteString
  sections <- readDTASections bs
  fmap catMaybes $ forM sections $ \(bytes, chunk) -> do
    case chunk of
      Parens (Tree _ data1) -> errorToWarning $ do
        data2 <- case [x | Key "song" x <- data1] of
          []    -> fatal "No song chunk found"
          x : _ -> return x
        base <- case concat [x | Key "name" x <- data2] of
          String x : _ -> return x
          Sym    x : _ -> return x
          _            -> fatal "Couldn't get path parameter (songs/foo/bar) for song"
        folder <- case T.splitOn "/" $ TE.decodeLatin1 base of
          ["songs", x, _] -> return x
          _               -> fatal $
            "Unrecognized format of name parameter in songs.dta, expected songs/x/y but got: " <> show base
        rb3 <- case concat [x | Key "format" x <- data1] of
          Int n : _ -> return $ n >= 10
          _         -> fatal "Couldn't get format number of song"
        return QuickDTA
          { qdtaRaw    = bytes
          , qdtaParsed = chunk
          , qdtaFolder = folder
          , qdtaRB3    = rb3
          }
      _ -> return Nothing

detectMOGG :: (MonadIO m) => Readable -> StackTraceT m (QuickFile Readable)
detectMOGG r = do
  moggType <- stackIO $ useHandle r $ \h -> B.hGet h 1
  let qfile = if B.unpack moggType == [0xA] then QFUnencryptedMOGG else QFEncryptedMOGG
  return $ qfile r

detectMIDI :: (MonadIO m) => B.ByteString -> Readable -> StackTraceT m (QuickFile Readable)
detectMIDI packageFolder r = do
  magic <- stackIO $ useHandle r $ \h -> B.hGet h 1
  let qfile = if magic == "MThd" then QFUnencryptedMIDI else QFEncryptedMIDI packageFolder
  return $ qfile r

mapMFilesWithName :: (Monad m) => ((T.Text, a) -> m (T.Text, b)) -> Folder T.Text a -> m (Folder T.Text b)
mapMFilesWithName f dir = do
  newFiles <- mapM f $ folderFiles dir
  newSubs  <- forM (folderSubfolders dir) $ \(subName, sub) -> do
    sub' <- mapMFilesWithName f sub
    return (subName, sub')
  return Folder { folderFiles = newFiles, folderSubfolders = newSubs }

loadInput :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m QuickInput
loadInput fin = case map toLower $ takeExtension fin of
  ".pkg" -> do
    pkg <- stackIO $ loadPKG fin
    usr <- case findFolder ["USRDIR"] $ pkgFolder pkg of
      Just dir -> return dir
      Nothing  -> fatal "No USRDIR found inside .pkg"
    songs <- fmap concat $ forM (folderSubfolders usr) $ \(packageName, packageFolder) -> do
      let textFolder = first TE.decodeLatin1 packageFolder
      songsFolder <- case findFolder ["songs"] textFolder of
        Just dir -> return dir
        Nothing -> fatal $ "No songs folder found inside .pkg for: " <> show packageName
      dta <- case findFile (pure "songs.dta") songsFolder of
        Just r -> return r
        Nothing -> fatal $ "No songs.dta found inside .pkg for: " <> show packageName
      songRefs <- findSongs dta
      forM songRefs $ \qdta -> do
        songFolder <- case findFolder [qdtaFolder qdta] songsFolder of
          Just dir -> return dir
          Nothing -> fatal $ "Song folder not found in .pkg for " <> show (packageName, qdtaFolder qdta)
        let identifyFile (name, r) = case map toLower $ takeExtension $ T.unpack name of
              ".mogg"     -> (name,) <$> detectMOGG r
              ".milo_ps3" -> return (name, QFMilo r)
              ".png_ps3"  -> return (name, QFPNGPS3 r)
              ".edat"     -> (name,) <$> detectMIDI packageName r
              _           -> return (name, QFOther r)
        identified <- mapMFilesWithName identifyFile songFolder
        return QuickSong
          { quickSongDTA       = qdta
          , quickSongFiles     = identified
          , quickSongPS3Folder = Just packageName
          }
    return QuickInput
      { quickInputPath   = fin
      , quickInputFormat = QCFormatPKG
      , quickInputSongs  = songs
      , quickInputXbox   = Nothing
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
    songs <- forM songRefs $ \qdta -> do
      songFolder <- case findFolder [qdtaFolder qdta] songsFolder of
        Just dir -> return dir
        Nothing -> fatal $ "Song folder not found in CON/LIVE: " <> show (qdtaFolder qdta)
      let identifyFile (name, r) = case map toLower $ takeExtension $ T.unpack name of
            ".mogg"      -> (name,) <$> detectMOGG r
            ".milo_xbox" -> return (name, QFMilo r)
            ".png_xbox"  -> return (name, QFPNGXbox r)
            ".mid"       -> return (name, QFUnencryptedMIDI r)
            _            -> return (name, QFOther r)
      identified <- mapMFilesWithName identifyFile songFolder
      return QuickSong
        { quickSongDTA       = qdta
        , quickSongFiles     = identified
        , quickSongPS3Folder = Nothing
        }
    (header, meta) <- stackIO $ withSTFSPackage fin $ \stfs -> return (stfsHeader stfs, stfsMetadata stfs)
    return QuickInput
      { quickInputPath   = fin
      , quickInputFormat = case header of CON{} -> QCFormatCON; _ -> QCFormatLIVE
      , quickInputSongs  = songs
      , quickInputXbox   = Just meta
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

getXboxFile :: (MonadIO m, SendMessage m) => (T.Text, QuickFile Readable) -> StackTraceT m (T.Text, Readable)
getXboxFile (name, qfile) = case qfile of
  QFEncryptedMOGG r -> return (name, r)
  QFUnencryptedMOGG r -> return (name, r)
  QFMilo r -> let
    name' = T.pack $ dropExtension (T.unpack name) <> ".milo_xbox"
    in return (name', r)
  QFPNGXbox r -> return (name, r)
  QFPNGPS3 r -> do
    let name' = T.pack $ dropExtension (T.unpack name) <> ".png_xbox"
    bs <- stackIO $ useHandle r handleToByteString
    let r' = makeHandle (T.unpack name') $ byteStringSimpleHandle $ swapPNGXboxPS3 bs
    return (name', r')
  QFEncryptedMIDI crypt r -> do
    let name' = case splitExtension $ T.unpack name of
          (base, ".edat") -> T.pack base
          _               -> name
    r' <- decryptEDAT crypt (TE.encodeUtf8 name) r
    return (name', r')
  QFUnencryptedMIDI r -> return (name, r)
  QFParsedMIDI mid -> getXboxFile (name, QFUnencryptedMIDI $ makeHandle "" $ byteStringSimpleHandle $ Save.toByteString mid)
  QFOther r -> return (name, r)

getPS3File :: (MonadResource m, SendMessage m) => Maybe (B.ByteString, Bool) -> (T.Text, QuickFile Readable) -> StackTraceT m (T.Text, Readable)
getPS3File mcrypt (name, qfile) = case qfile of
  QFEncryptedMOGG r -> return (name, r)
  QFUnencryptedMOGG r -> tempDir "onyxquickconv" $ \tmp -> stackIO $ do
    let tmpIn  = tmp </> "in.mogg"
        tmpOut = tmp </> "out.mogg"
    saveReadable r tmpIn
    encryptRB1 tmpIn tmpOut
    r' <- makeHandle (T.unpack name) . byteStringSimpleHandle . BL.fromStrict <$> B.readFile tmpOut
    return (name, r')
  QFMilo r -> let
    name' = T.pack $ dropExtension (T.unpack name) <> ".milo_ps3"
    in return (name', r)
  QFPNGXbox r -> stackIO $ do
    let name' = T.pack $ dropExtension (T.unpack name) <> ".png_ps3"
    bs <- useHandle r handleToByteString
    let r' = makeHandle (T.unpack name') $ byteStringSimpleHandle $ swapPNGXboxPS3 bs
    return (name', r')
  QFPNGPS3 r -> return (name, r)
  QFEncryptedMIDI crypt r -> do
    r' <- case mcrypt of
      Nothing              -> decryptEDAT crypt (TE.encodeUtf8 name) r
      Just (newCrypt, rb3) -> if crypt == newCrypt
        then return r -- fine as is
        else decryptEDAT crypt (TE.encodeUtf8 name) r >>= encryptEDAT newCrypt (TE.encodeUtf8 name) rb3
    return (name, r')
  QFUnencryptedMIDI r -> do
    let name' = case takeExtension $ T.unpack name of
          ".edat" -> name
          _       -> name <> ".edat"
    r' <- case mcrypt of
      Nothing           -> return r
      Just (crypt, rb3) -> encryptEDAT crypt (TE.encodeUtf8 name') rb3 r
    return (name', r')
  QFParsedMIDI mid -> getPS3File mcrypt (name, QFUnencryptedMIDI $ makeHandle "" $ byteStringSimpleHandle $ Save.toByteString mid)
  QFOther r -> return (name, r)

makePS3DTA :: Chunk B.ByteString -> Chunk B.ByteString
makePS3DTA = id -- TODO set rating 4 -> 2, and maybe numeric song_id

saveQuickSongsSTFS :: (MonadIO m, SendMessage m) => [QuickSong] -> CreateOptions -> FilePath -> StackTraceT m ()
saveQuickSongsSTFS qsongs opts fout = do
  xboxFolders <- forM qsongs $ \qsong -> do
    xboxFolder <- mapMFilesWithName getXboxFile $ quickSongFiles qsong
    return (qdtaFolder $ quickSongDTA qsong, xboxFolder)
  let songsFolder = Folder
        { folderSubfolders = xboxFolders
        , folderFiles = [("songs.dta", songsDTA)]
        }
      topFolder = Folder
        { folderSubfolders = [("songs", songsFolder)]
        , folderFiles = []
        }
      songsDTA = makeHandle "songs.dta" $ byteStringSimpleHandle
        $ BL.intercalate "\n" $ map (BL.fromStrict . qdtaRaw . quickSongDTA) qsongs
  stackIO $ makeCONReadable opts topFolder fout

data QuickPS3Folder
  = QCOneFolder
  | QCSeparateFolders

data QuickPS3Settings = QuickPS3Settings
  { qcPS3Folder  :: Maybe QuickPS3Folder
  , qcPS3Encrypt :: Bool
  , qcPS3RB3     :: Bool
  }

saveQuickSongsPKG :: (MonadResource m, SendMessage m) => [QuickSong] -> QuickPS3Settings -> FilePath -> StackTraceT m ()
saveQuickSongsPKG qsongs settings fout = do
  let oneFolder = B8.pack $ take 0x1B $ "OQC" <> show (hash $ map (qdtaRaw . quickSongDTA) qsongs)
      contentID = npdContentID
        $ (if qcPS3RB3 settings then rb3CustomMidEdatConfig else rb2CustomMidEdatConfig) oneFolder
  qsongMapping <- fmap (Map.unionsWith (<>)) $ forM qsongs $ \qsong -> do
    let separateFolder = B8.pack $ take 0x1B $ "OQC" <> show (hash $ qdtaRaw $ quickSongDTA qsong)
        chosenFolder = case qcPS3Folder settings of
          Nothing -> case quickSongPS3Folder qsong of
            Just dir -> dir -- came from ps3, keep the same subfolder
            Nothing  -> separateFolder -- came from xbox, assign new random folder
          Just QCOneFolder -> oneFolder -- one folder for whole pkg
          Just QCSeparateFolders -> separateFolder -- separate folder for each song
        mcrypt = guard (qcPS3Encrypt settings) >> Just (chosenFolder, qcPS3RB3 settings)
    ps3Folder <- mapMFilesWithName (getPS3File mcrypt) $ quickSongFiles qsong
    return $ Map.singleton chosenFolder [(qsong, ps3Folder)]
  let rootFolder = container "USRDIR" $ mconcat $ do
        (usrdirSub, songs) <- Map.toList qsongMapping
        let songsDTA = makeHandle "songs.dta" $ byteStringSimpleHandle
              $ BL.intercalate "\n"
              $ map (chunkLatin1 . makePS3DTA . qdtaParsed . quickSongDTA . fst) songs
            chunkLatin1 :: Chunk B.ByteString -> BL.ByteString
            chunkLatin1 chunk = BL.fromStrict $ B8.pack $ T.unpack $ showDTA
              $ DTA 0 $ Tree 0 [TE.decodeLatin1 <$> chunk]
        return $ container (TE.decodeLatin1 usrdirSub) $ container "songs" Folder
          { folderFiles = [("songs.dta", songsDTA)]
          , folderSubfolders = map (first $ qdtaFolder . quickSongDTA) songs
          }
      container name inner = Folder { folderSubfolders = [(name, inner)], folderFiles = [] }
  stackIO $ do
    extraPath <- getResourcesPath $ if qcPS3RB3 settings then "pkg-contents/rb3" else "pkg-contents/rb2"
    extra <- crawlFolder extraPath
    makePKG contentID (first TE.encodeUtf8 $ rootFolder <> extra) fout

contentsSize :: QuickSong -> IO Integer
contentsSize qsong = do
  -- TODO add parsed midi size somehow
  fileSizes <- mapM (\r -> useHandle r hFileSize) $ toList (quickSongFiles qsong) >>= toList
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
  3rd row: max size, go button (opens a save-as dialog, used as pack name or template for multiple)
Make songs (each song gets one new output file)
  2nd row: con/pkg/live, if pkg then (enc/unenc midi select)
  3rd row: go button (opens pick folder dialog, songs get auto names inside)

-}
