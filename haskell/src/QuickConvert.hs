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
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.StackTrace
import           Data.Bifunctor                   (first)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (toLower)
import           Data.DTA                         (Chunk (..), DTA (..),
                                                   Tree (..), readDTASections,
                                                   showDTA)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Hashable                    (hash)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe)
import           Data.SimpleHandle
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Text.Encoding.Error         (lenientDecode)
import           Image                            (swapPNGXboxPS3)
import           MoggDecrypt                      (encryptRB1)
import           NPData
import qualified Numeric.NonNegative.Class        as NNC
import           PlayStation.PKG
import           Resources                        (getResourcesPath)
import           RockBand.Common                  (pattern RNil, pattern Wait,
                                                   isNoteEdge, makeEdgeCPV)
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import           Sound.MIDI.File.FastParse        (getMIDI)
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           STFS.Package
import System.Directory (makeAbsolute)
import           System.FilePath                  (dropExtension,
                                                   splitExtension, takeDirectory,
                                                   takeExtension, (</>), takeFileName)
import           System.IO                        (hFileSize)

data QuickSong = QuickSong
  { quickSongDTA       :: QuickDTA
  , quickSongFiles     :: Folder T.Text QuickFileSized -- contents of 'foo'
  , quickSongPS3Folder :: Maybe B.ByteString      -- if came from ps3, this is the USRDIR subfolder (used for .mid.edat encryption)
  }

data QuickDTA = QuickDTA
  { qdtaRaw    :: B.ByteString
  , qdtaParsed :: Chunk B.ByteString
  , qdtaFolder :: T.Text -- in 'songs/foo/bar.mid', this is 'foo'
  , qdtaRB3    :: Bool
  , qdtaTitle  :: T.Text
  , qdtaArtist :: Maybe T.Text
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

type QuickFileSized = (Integer, QuickFile Readable)

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

splitSongsDTA :: (SendMessage m, MonadIO m) => Readable -> StackTraceT m [QuickDTA]
splitSongsDTA r = do
  bs <- fmap BL.toStrict $ stackIO $ useHandle r handleToByteString
  let noBOM = fromMaybe bs $ B.stripPrefix "\xEF\xBB\xBF" bs
  sections <- readDTASections noBOM
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
          _         -> do
            -- old RB1 songs are missing format, gets added through missing_song_data.dta
            warn "Couldn't get format number of song"
            return False
        title <- case concat [x | Key "name" x <- data1] of
          String s : _ -> return s
          _            -> fatal "Couldn't get name of song"
        let artist = case concat [x | Key "artist" x <- data1] of
              String s : _ -> Just s
              _            -> Nothing
            isUTF8 = case concat [x | Key "encoding" x <- data1] of
              String "utf8" : _ -> True
              _                 -> False
            readString = if isUTF8 then TE.decodeUtf8With lenientDecode else TE.decodeLatin1
        return QuickDTA
          { qdtaRaw    = bytes
          , qdtaParsed = chunk
          , qdtaFolder = folder
          , qdtaRB3    = rb3
          , qdtaTitle  = readString title
          , qdtaArtist = readString <$> artist
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

loadQuickSongsPS3 :: (MonadIO m, SendMessage m) => B.ByteString -> Folder T.Text Readable -> StackTraceT m [QuickSong]
loadQuickSongsPS3 packageName packageFolder = do
  songsFolder <- case findFolder ["songs"] packageFolder of
    Just dir -> return dir
    Nothing -> fatal $ "No songs folder found inside .pkg for: " <> show packageName
  dta <- case findFile (pure "songs.dta") songsFolder of
    Just r -> return r
    Nothing -> fatal $ "No songs.dta found inside .pkg for: " <> show packageName
  songRefs <- splitSongsDTA dta
  forM songRefs $ \qdta -> do
    songFolder <- case findFolder [qdtaFolder qdta] songsFolder of
      Just dir -> return dir
      Nothing -> fatal $ "Song folder not found in .pkg for " <> show (packageName, qdtaFolder qdta)
    let identifyFile (name, r) = do
          size <- stackIO $ useHandle r hFileSize
          fmap (addSize size) $ case map toLower $ takeExtension $ T.unpack name of
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

loadQuickSongsXbox :: (MonadIO m, SendMessage m) => Folder T.Text Readable -> StackTraceT m [QuickSong]
loadQuickSongsXbox folder = do
  songsFolder <- case findFolder ["songs"] folder of
    Just dir -> return dir
    Nothing  -> fatal "No songs folder found inside CON/LIVE"
  dta <- case findFile (pure "songs.dta") songsFolder of
    Just r  -> return r
    Nothing -> fatal "No songs.dta found inside CON/LIVE"
  songRefs <- splitSongsDTA dta
  forM songRefs $ \qdta -> do
    songFolder <- case findFolder [qdtaFolder qdta] songsFolder of
      Just dir -> return dir
      Nothing -> fatal $ "Song folder not found in CON/LIVE: " <> show (qdtaFolder qdta)
    let identifyFile (name, r) = do
          size <- stackIO $ useHandle r hFileSize
          fmap (addSize size) $ case map toLower $ takeExtension $ T.unpack name of
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

loadQuickInput :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m (Maybe QuickInput)
loadQuickInput fin = inside ("Loading: " <> fin) $ case map toLower $ takeExtension fin of
  ".pkg" -> errorToWarning $ do
    pkg <- stackIO $ loadPKG fin
    usr <- case findFolder ["USRDIR"] $ pkgFolder pkg of
      Just dir -> return dir
      Nothing  -> fatal "No USRDIR found inside .pkg"
    songs <- fmap concat $ forM (folderSubfolders usr) $ \(packageName, folder) -> do
      loadQuickSongsPS3 packageName $ first TE.decodeLatin1 folder
    return QuickInput
      { quickInputPath   = fin
      , quickInputFormat = QCFormatPKG
      , quickInputSongs  = songs
      , quickInputXbox   = Nothing
      }
  _ -> errorToEither (stackIO $ getSTFSFolder fin) >>= \case
    Right folder -> errorToWarning $ do
      songs <- loadQuickSongsXbox folder
      (header, meta) <- stackIO $ withSTFSPackage fin $ \stfs -> return (stfsHeader stfs, stfsMetadata stfs)
      return QuickInput
        { quickInputPath   = fin
        , quickInputFormat = case header of CON{} -> QCFormatCON; _ -> QCFormatLIVE
        , quickInputSongs  = songs
        , quickInputXbox   = Just meta
        }
    Left _ -> case takeFileName fin of
      "songs.dta" -> errorToWarning $ do
        finAbs <- stackIO $ makeAbsolute fin
        let packageRoot = takeDirectory $ takeDirectory finAbs -- should be the folder that has "songs" subfolder
        folder <- stackIO $ crawlFolder packageRoot
        if folderHasEDAT folder
          then do
            -- this assumes there's a proper parent folder to use for decryption key
            songs <- loadQuickSongsPS3 (B8.pack $ takeFileName packageRoot) folder
            return QuickInput
              { quickInputPath   = packageRoot <> ".pkg"
              , quickInputFormat = QCFormatPKG -- TODO make a real format type for raw folders
              , quickInputSongs  = songs
              , quickInputXbox   = Nothing
              }
          else do
            songs <- loadQuickSongsXbox folder
            return QuickInput
              { quickInputPath   = packageRoot <> "_con"
              , quickInputFormat = QCFormatCON -- TODO make a real format type for raw folders
              , quickInputSongs  = songs
              , quickInputXbox   = Nothing
              }
      _ -> return Nothing

folderHasEDAT :: Folder T.Text a -> Bool
folderHasEDAT folder = any isEDAT (map fst $ folderFiles folder) || any folderHasEDAT (map snd $ folderSubfolders folder)
  where isEDAT s = ".edat" `T.isSuffixOf` T.toLower s

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
    xboxFolder <- mapMFilesWithName (getXboxFile . discardSize) $ quickSongFiles qsong
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

addSize :: size -> (T.Text, a) -> (T.Text, (size, a))
addSize n (t, x) = (t, (n, x))

discardSize :: (T.Text, (size, a)) -> (T.Text, a)
discardSize (t, (_, x)) = (t, x)

saveQuickSongsPKG :: (MonadResource m, SendMessage m) => [QuickSong] -> QuickPS3Settings -> FilePath -> StackTraceT m ()
saveQuickSongsPKG qsongs settings fout = do
  -- TODO put some of song title/artist in the folders
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
    ps3Folder <- mapMFilesWithName (getPS3File mcrypt . discardSize) $ quickSongFiles qsong
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

contentsSize :: QuickSong -> Integer
contentsSize qsong = sum $ map fst $ toList $ quickSongFiles qsong

-- Organizes based on combined size of song files.
-- Should add some buffer to account for package overhead and platform conversion.
organizePacks :: Integer -> [QuickSong] -> [[QuickSong]]
organizePacks maxSize = let
  go :: Integer -> [QuickSong] -> [(Integer, QuickSong)] -> [[QuickSong]]
  go curSize curSongs incoming = case incoming of
    [] -> case curSongs of
      []    -> []
      _ : _ -> [curSongs]
    (nextSize, nextSong) : rest -> let
      newSize = curSize + nextSize
      in if newSize > maxSize && not (null curSongs) -- always add at least one song
        then curSongs : go 0 [] incoming
        else go newSize (nextSong : curSongs) rest
  in go 0 [] . map (\qsong -> (contentsSize qsong, qsong))

blackVenue :: Bool -> F.T -> F.T
blackVenue isRB3 (F.Cons typ dvn trks) = let
  black = U.setTrackName "VENUE" $ if isRB3
    then RTB.fromPairList $ map (\s -> (0, E.MetaEvent $ Meta.TextEvent s))
      ["[lighting (blackout_fast)]", "[film_b+w.pp]", "[coop_all_far]"]
    else Wait 0 (E.MetaEvent $ Meta.TextEvent "[verse]")
      $ Wait 0 (E.MetaEvent $ Meta.TextEvent "[lighting (blackout_fast)]")
      $ Wait 0 (makeEdgeCPV 0 60 $ Just 96) -- camera cut
      $ Wait 0 (makeEdgeCPV 0 61 $ Just 96) -- focus bass
      $ Wait 0 (makeEdgeCPV 0 62 $ Just 96) -- focus drums
      $ Wait 0 (makeEdgeCPV 0 63 $ Just 96) -- focus guitar
      $ Wait 0 (makeEdgeCPV 0 64 $ Just 96) -- focus vocal
      $ Wait 0 (makeEdgeCPV 0 71 $ Just 96) -- only far
      $ Wait 0 (makeEdgeCPV 0 108 $ Just 96) -- video_bw
      $ Wait 120 (makeEdgeCPV 0 60 Nothing)
      $ Wait 0 (makeEdgeCPV 0 61 Nothing)
      $ Wait 0 (makeEdgeCPV 0 62 Nothing)
      $ Wait 0 (makeEdgeCPV 0 63 Nothing)
      $ Wait 0 (makeEdgeCPV 0 64 Nothing)
      $ Wait 0 (makeEdgeCPV 0 71 Nothing)
      $ Wait 0 (makeEdgeCPV 0 108 Nothing) RNil
  isVenue = (== Just "VENUE") . U.trackName
  in F.Cons typ dvn $ filter (not . isVenue) trks ++ [black]

removePitch :: (NNC.C t) => Int -> RTB.T t E.T -> RTB.T t E.T
removePitch n = RTB.filter $ maybe True (\(p, _) -> p /= n) . isNoteEdge

noOverdrive :: F.T -> F.T
noOverdrive (F.Cons typ dvn trks) = F.Cons typ dvn $ let
  instrumentTracks =
    [ "PART GUITAR"
    , "PART BASS"
    , "PART DRUMS"
    , "PART KEYS"
    , "PART VOCALS"
    , "HARM1"
    , "HARM2"
    , "HARM3"
    , "PART REAL_KEYS_E"
    , "PART REAL_KEYS_M"
    , "PART REAL_KEYS_H"
    , "PART REAL_KEYS_X"
    , "PART REAL_GUITAR"
    , "PART REAL_GUITAR_22"
    , "PART REAL_BASS"
    , "PART REAL_BASS_22"
    ]
  in flip map trks $ \trk -> if maybe False (`elem` instrumentTracks) $ U.trackName trk
    then removePitch 116 trk
    else trk

noLanesGBK :: F.T -> F.T
noLanesGBK (F.Cons typ dvn trks) = F.Cons typ dvn $ let
  noLaneTracks =
    [ "PART GUITAR"
    , "PART BASS"
    , "PART KEYS"
    , "PART REAL_GUITAR"
    , "PART REAL_GUITAR_22"
    , "PART REAL_BASS"
    , "PART REAL_BASS_22"
    ]
  noTrillTracks =
    [ "PART REAL_KEYS_E"
    , "PART REAL_KEYS_M"
    , "PART REAL_KEYS_H"
    , "PART REAL_KEYS_X"
    ]
  in flip map trks $ \trk -> let
    name = U.trackName trk
    in case (maybe False (`elem` noLaneTracks) name, maybe False (`elem` noTrillTracks) name) of
      (True, _   ) -> removePitch 116 $ removePitch 117 trk
      (_   , True) -> removePitch 117 trk
      _            -> trk

noLanesDrums :: F.T -> F.T
noLanesDrums (F.Cons typ dvn trks) = F.Cons typ dvn $ flip map trks $ \trk ->
  case U.trackName trk of
    Just "PART DRUMS" -> removePitch 116 $ removePitch 117 trk
    _                 -> trk

applyToMIDI :: (SendMessage m, MonadIO m) => (F.T -> F.T) -> Folder T.Text QuickFileSized -> StackTraceT m (Folder T.Text QuickFileSized)
applyToMIDI midFunction = mapMFilesWithName $ let
  withParsed name mid = return (name, QFParsedMIDI $ midFunction mid)
  withUnenc name r = do
    bs <- stackIO $ useHandle r handleToByteString
    (mid, warns) <- runGetM getMIDI bs
    mapM_ warn warns
    withParsed name mid
  withEnc name crypt r = do
    let name' = case splitExtension $ T.unpack name of
          (base, ".edat") -> T.pack base
          _               -> name
    r' <- decryptEDAT crypt (TE.encodeUtf8 name) r
    withUnenc name' r'
  in \(name, (size, qfile)) -> do
    (name', qfile') <- case qfile of
      QFEncryptedMIDI crypt r   -> withEnc    name crypt r
      QFUnencryptedMIDI     r   -> withUnenc  name       r
      QFParsedMIDI          mid -> withParsed name       mid
      _                         -> return (name, qfile)
    return (name', (size, qfile'))
