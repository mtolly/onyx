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
module Onyx.QuickConvert.RockBand where

import           Codec.Picture                        (convertRGB8, decodeImage,
                                                       pixelMap)
import           Codec.Picture.Types                  (dropTransparency)
import           Control.Exception                    (evaluate)
import           Control.Monad
import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.Trans.Resource         (MonadResource, ResourceT,
                                                       runResourceT)
import           Data.Bifunctor                       (first, second)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as BL
import           Data.Char                            (isAlphaNum, isAscii,
                                                       isLower, isUpper,
                                                       toLower)
import qualified Data.Conduit.Audio                   as CA
import           Data.Conduit.Audio.Sndfile           (sinkSnd, sourceSndFrom)
import qualified Data.Digest.Pure.MD5                 as MD5
import qualified Data.EventList.Absolute.TimeBody     as ATB
import qualified Data.EventList.Relative.TimeBody     as RTB
import           Data.Foldable                        (toList)
import           Data.Hashable                        (hash)
import           Data.Int                             (Int16, Int32)
import qualified Data.IntSet                          as IntSet
import           Data.List                            (partition)
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       listToMaybe, mapMaybe)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import           Data.Text.Encoding.Error             (lenientDecode)
import           Data.Tuple.Extra                     (fst3, snd3, thd3)
import qualified Numeric.NonNegative.Class            as NNC
import           Onyx.Audio                           (applyPansVols, fadeEnd,
                                                       fadeStart)
import           Onyx.Harmonix.DTA                    (Chunk (..), DTA (..),
                                                       Tree (..), readDTABytes,
                                                       readDTASections,
                                                       readDTA_latin1, showDTA)
import           Onyx.Harmonix.DTA.C3                 (C3DTAComments (..),
                                                       DTASingle (..),
                                                       insertCommentsBytes,
                                                       readDTASingles,
                                                       writeDTASingle)
import qualified Onyx.Harmonix.DTA.Serialize.RockBand as D
import           Onyx.Harmonix.Magma                  (rbaContents)
import           Onyx.Harmonix.MOGG                   (encryptMOGGToByteString,
                                                       fixOldC3Mogg, moggToOgg,
                                                       oggToMogg)
import           Onyx.Harmonix.RockBand.Milo          (addMiloHeader,
                                                       decompressMilo)
import qualified Onyx.Image.DXT                       as I
import           Onyx.MIDI.Common                     (isNoteEdge, isNoteEdge',
                                                       joinEdgesSimple,
                                                       makeEdge', makeEdgeCPV,
                                                       pattern RNil,
                                                       pattern Wait,
                                                       splitEdgesSimple)
import           Onyx.MIDI.Parse                      (getMIDI)
import           Onyx.Nintendo.U8                     (packU8Folder)
import           Onyx.PlayStation.NPData
import           Onyx.PlayStation.PKG
import           Onyx.Resources                       (getResourcesPath)
import           Onyx.StackTrace
import           Onyx.Util.Binary                     (runGetM)
import           Onyx.Util.Handle
import           Onyx.Xbox.STFS
import qualified Sound.File.Sndfile                   as Snd
import qualified Sound.MIDI.File                      as F
import qualified Sound.MIDI.File.Event                as E
import qualified Sound.MIDI.File.Event.Meta           as Meta
import qualified Sound.MIDI.File.Save                 as Save
import qualified Sound.MIDI.Util                      as U
import           System.FilePath                      (dropExtension,
                                                       splitExtension,
                                                       takeDirectory,
                                                       takeExtension,
                                                       takeFileName, (</>))
import           System.IO                            (hFileSize)

data QuickSong = QuickSong
  { quickSongDTA       :: QuickDTA
  , quickSongFiles     :: Folder T.Text QuickFileSized -- contents of 'foo'
  , quickSongPS3Folder :: Maybe B.ByteString      -- if came from ps3, this is the USRDIR subfolder (used for .mid.edat encryption)
  }

data QuickDTA = QuickDTA
  { qdtaOriginal :: B.ByteString -- not used for output anymore, may get out of sync with parsed/comments
  , qdtaParsed   :: Chunk B.ByteString
  , qdtaComments :: [B.ByteString] -- c3 style extra comments
  , qdtaFolder   :: T.Text -- in 'songs/foo/bar.mid', this is 'foo'
  , qdtaRB3      :: Bool
  , qdtaTitle    :: T.Text
  , qdtaArtist   :: Maybe T.Text
  , qdtaPreview  :: (Int32, Int32)
  , qdtaPans     :: [Float]
  , qdtaVols     :: [Float]
  }

data QuickConvertFormat
  = QCFormatCON
  | QCFormatLIVE
  | QCFormatPKG
  deriving (Show)

data QuickConvertFormatInput
  = QCInputTwoWay QuickConvertFormat
  | QCInputLoose
  | QCInputRBA
  deriving (Show)

data QuickInput = QuickInput
  { quickInputPath   :: FilePath
  , quickInputFormat :: QuickConvertFormatInput
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
  | QFParsedMIDI (F.T B.ByteString)
  | QFOther a
  deriving (Eq, Show, Foldable)

pattern Key :: s -> [Chunk s] -> Chunk s
pattern Key k v <- Parens (Tree _ (Sym k : v)) where
  Key k v = Parens $ Tree 0 $ Sym k : v

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
        preview <- case concat [x | Key "preview" x <- data1] of
          [Int start, Int end] -> return (start, end)
          _                    -> fatal "Couldn't get preview time of song"
        let getFloats = mapM $ \case
              Int   i -> return $ realToFrac i
              Float f -> return f
              x       -> fatal $ "Expected a number but found: " <> show x
        pans <- case concat [x | Key "pans" x <- data2] of
          Parens (Tree _ xs) : _ -> getFloats xs
          _                      -> fatal "Couldn't get pans list"
        vols <- case concat [x | Key "vols" x <- data2] of
          Parens (Tree _ xs) : _ -> getFloats xs
          _                      -> fatal "Couldn't get vols list"
        let comments = filter (\s -> B.take 1 s == ";") $ map B8.strip $ B8.lines bytes
        return QuickDTA
          { qdtaOriginal = bytes
          , qdtaParsed   = chunk
          , qdtaComments = comments
          , qdtaFolder   = folder
          , qdtaRB3      = rb3
          , qdtaTitle    = readString title
          , qdtaArtist   = readString <$> artist
          , qdtaPreview  = preview
          , qdtaPans     = pans
          , qdtaVols     = vols
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

mapMaybeFolder :: (a -> Maybe b) -> Folder t a -> Folder t b
mapMaybeFolder f dir = Folder
  { folderSubfolders = map (second $ mapMaybeFolder f) $ folderSubfolders dir
  , folderFiles = flip mapMaybe (folderFiles dir) $ \(name, file) -> case f file of
    Nothing    -> Nothing
    Just file' -> Just (name, file')
  }

loadQuickSongsLoose :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m [QuickSong]
loadQuickSongsLoose pathDTA = do
  songsFolder <- stackIO $ crawlFolder $ takeDirectory pathDTA
  let maybePackageName = B8.pack $ takeFileName $ takeDirectory $ takeDirectory pathDTA
  songRefs <- splitSongsDTA $ fileReadable pathDTA
  forM songRefs $ \qdta -> do
    songFolder <- case findFolderCI [qdtaFolder qdta] songsFolder of
      Just dir -> return dir
      Nothing -> fatal $ "Song folder not found in directory structure: " <> show (qdtaFolder qdta)
    let identifyFile (name, r) = do
          size <- stackIO $ useHandle r hFileSize
          fmap (addSize size) $ case map toLower $ takeExtension $ T.unpack name of
            ".mogg"      -> (name,) <$> detectMOGG r
            ".milo_xbox" -> return (name, QFMilo r)
            ".milo_ps3"  -> return (name, QFMilo r)
            ".png_xbox"  -> return (name, QFPNGXbox r)
            ".png_ps3"   -> return (name, QFPNGPS3 r)
            ".mid"       -> return (name, QFUnencryptedMIDI r)
            ".edat"      -> (name,) <$> detectMIDI maybePackageName r
            _            -> return (name, QFOther r)
    identified <- mapMFilesWithName identifyFile songFolder
    return QuickSong
      { quickSongDTA       = qdta
      , quickSongFiles     = identified
      , quickSongPS3Folder = Nothing
      }

loadQuickSongsPS3 :: (MonadIO m, SendMessage m) => B.ByteString -> Folder T.Text Readable -> StackTraceT m [QuickSong]
loadQuickSongsPS3 packageName packageFolder = do
  songsFolder <- case findFolderCI ["songs"] packageFolder of
    Just dir -> return dir
    Nothing -> fatal $ "No songs folder found inside .pkg for: " <> show packageName
  dta <- case findFileCI (pure "songs.dta") songsFolder of
    Just r -> return r
    Nothing -> fatal $ "No songs.dta found inside .pkg for: " <> show packageName
  songRefs <- splitSongsDTA dta
  forM songRefs $ \qdta -> do
    songFolder <- case findFolderCI [qdtaFolder qdta] songsFolder of
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
  songsFolder <- case findFolderCI ["songs"] folder of
    Just dir -> return dir
    Nothing  -> fatal "No songs folder found inside CON/LIVE"
  dta <- case findFileCI (pure "songs.dta") songsFolder of
    Just r  -> return r
    Nothing -> fatal "No songs.dta found inside CON/LIVE"
  songRefs <- splitSongsDTA dta
  forM songRefs $ \qdta -> do
    songFolder <- case findFolderCI [qdtaFolder qdta] songsFolder of
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

loadQuickSongsRBA :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m [QuickSong]
loadQuickSongsRBA fin = do
  let contents = rbaContents fin
      need i = case lookup i contents of
        Just r  -> return r
        Nothing -> fatal $ "Required RBA subfile " <> show i <> " not found"
  dtaBS <- need 0 >>= \r -> stackIO $ useHandle r handleToByteString
  md5 <- stackIO $ evaluate $ MD5.md5 dtaBS
  let shortName = T.pack $ "onyx" ++ take 10 (show md5)
  packSongs <- readDTASingles $ BL.toStrict dtaBS
  (DTASingle _top pkg _comments, isUTF8) <- case packSongs of
    [song] -> return song
    _      -> fatal $ "Expected 1 song in RBA, found " <> show (length packSongs)
  midi <- need 1
  mogg <- need 2
  milo <- need 3
  bmp <- need 4 >>= \r -> stackIO $ useHandle r handleToByteString
  extraBS <- need 6 >>= \r -> stackIO $ useHandle r handleToByteString
  extra <- fmap (if isUTF8 then TE.decodeUtf8With lenientDecode else TE.decodeLatin1)
    <$> readDTABytes (BL.toStrict extraBS)
  pngXbox <- case decodeImage $ BL.toStrict bmp of
    Left  err -> fatal err
    Right dyn -> return $ I.toDXT1File I.PNGXbox $ convertRGB8 dyn
  let newDTA
        = BL.fromStrict
        $ (if isUTF8 then TE.encodeUtf8 else B8.pack . T.unpack)
        $ writeDTASingle DTASingle
        { dtaTopKey = shortName
        , dtaSongPackage = pkg
          { D.song = (D.song pkg)
            { D.songName = T.intercalate "/" ["songs", shortName, shortName]
            }
          , D.songId = Just $ Right shortName
          }
        , dtaC3Comments = C3DTAComments
          { c3dtaCreatedUsing = Nothing
          , c3dtaAuthoredBy   = case extra of
            DTA _ (Tree _ [Parens (Tree _
              ( String "backend"
              : Parens (Tree _ [Sym "author", String s])
              : _
              ))])
              -> Just s
            _ -> Nothing
          , c3dtaSong         = Nothing
          , c3dtaLanguages    = Nothing -- TODO
          , c3dtaKaraoke      = Nothing
          , c3dtaMultitrack   = Nothing
          , c3dtaConvert      = Nothing
          , c3dta2xBass       = Nothing
          , c3dtaRhythmKeys   = Nothing
          , c3dtaRhythmBass   = Nothing
          , c3dtaCATemh       = Nothing
          , c3dtaExpertOnly   = Nothing
          }
        }
  loadQuickSongsXbox $ fromFiles
    [ ("songs" :| ["songs.dta"], makeHandle "songs.dta" $ byteStringSimpleHandle newDTA)
    , ("songs" :| [shortName, shortName <> ".mid"], midi)
    , ("songs" :| [shortName, shortName <> ".mogg"], mogg)
    , ("songs" :| [shortName, "gen", shortName <> ".milo_xbox"], milo)
    , ("songs" :| [shortName, "gen", shortName <> "_keep.png_xbox"], makeHandle "image.png_xbox" $ byteStringSimpleHandle pngXbox)
    ]

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
      , quickInputFormat = QCInputTwoWay QCFormatPKG
      , quickInputSongs  = songs
      , quickInputXbox   = Nothing
      }
  ".rba" -> errorToWarning $ do
    songs <- loadQuickSongsRBA fin
    return QuickInput
      { quickInputPath   = fin
      , quickInputFormat = QCInputRBA
      , quickInputSongs  = songs
      , quickInputXbox   = Nothing
      }
  _ -> errorToEither (stackIO $ getSTFSFolder fin) >>= \case
    Right folder -> errorToWarning $ do
      songs <- loadQuickSongsXbox folder
      (header, meta) <- stackIO $ withSTFSPackage fin $ \stfs -> return (stfsHeader stfs, stfsMetadata stfs)
      return QuickInput
        { quickInputPath   = fin
        , quickInputFormat = QCInputTwoWay $ case header of CON{} -> QCFormatCON; _ -> QCFormatLIVE
        , quickInputSongs  = songs
        , quickInputXbox   = Just meta
        }
    Left _ -> case takeFileName fin of
      "songs.dta" -> errorToWarning $ do
        songs <- loadQuickSongsLoose fin
        return QuickInput
          { quickInputPath   = takeDirectory fin -- not sure best way to handle this
          , quickInputFormat = QCInputLoose
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
  Nothing -> (if crypt /= "HMX0756" then tryDecryptEDAT "HMX0756" name r else return Nothing) >>= \case
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

verifyCryptEDAT :: (MonadResource m, SendMessage m) => B.ByteString -> B.ByteString -> Bool -> Readable -> StackTraceT m Readable
verifyCryptEDAT crypt name rb3 r = tryDecryptEDAT crypt name r >>= \case
  Just r' -> return r'
  Nothing -> (if crypt /= "HMX0756" then tryDecryptEDAT "HMX0756" name r else return Nothing) >>= \case
    Just r' -> encryptEDAT crypt name rb3 r'
    Nothing -> fatal $ "Couldn't decrypt " <> show name

getXboxFile :: (MonadIO m, SendMessage m) => (T.Text, QuickFile Readable) -> StackTraceT m (T.Text, Readable)
getXboxFile (name, qfile) = case qfile of
  QFEncryptedMOGG r -> return (name, fixOldC3Mogg r)
  QFUnencryptedMOGG r -> return (name, r)
  QFMilo r -> let
    name' = T.pack $ dropExtension (T.unpack name) <> ".milo_xbox"
    in return (name', r)
  QFPNGXbox r -> return (name, r)
  QFPNGPS3 r -> do
    let name' = T.pack $ dropExtension (T.unpack name) <> ".png_xbox"
    bs <- stackIO $ useHandle r handleToByteString
    let r' = makeHandle (T.unpack name') $ byteStringSimpleHandle $ I.swapPNGXboxPS3 bs
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
  QFEncryptedMOGG r -> return (name, fixOldC3Mogg r)
  QFUnencryptedMOGG r -> let
    enc = makeHandle "encrypted mogg for ps3" $ encryptMOGGToByteString r >>= byteStringSimpleHandle
    in return (name, enc)
  QFMilo r -> let
    name' = T.pack $ dropExtension (T.unpack name) <> ".milo_ps3"
    in return (name', r)
  QFPNGXbox r -> stackIO $ do
    let name' = T.pack $ dropExtension (T.unpack name) <> ".png_ps3"
    bs <- useHandle r handleToByteString
    let r' = makeHandle (T.unpack name') $ byteStringSimpleHandle $ I.swapPNGXboxPS3 bs
    return (name', r')
  QFPNGPS3 r -> return (name, r)
  QFEncryptedMIDI crypt r -> do
    let nameBytes = TE.encodeUtf8 name
    r' <- case mcrypt of
      Nothing              -> decryptEDAT crypt (TE.encodeUtf8 name) r
      Just (newCrypt, rb3) -> if crypt == newCrypt
        then verifyCryptEDAT crypt nameBytes rb3 r
        else decryptEDAT crypt nameBytes r >>= encryptEDAT newCrypt nameBytes rb3
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
makePS3DTA = adjustChunk where
  adjustTree (Tree nid chunks) = Tree nid $ adjustChunks chunks
  adjustChunks = \case
    [Sym "rating" , Int 4] -> [Sym "rating" , Int 2]
    [Sym "song_id", Sym n] -> [Sym "song_id", Int $ quickSongID n]
    xs                     -> map adjustChunk xs
  adjustChunk = \case
    Parens   tree -> Parens   $ adjustTree tree
    Braces   tree -> Braces   $ adjustTree tree
    Brackets tree -> Brackets $ adjustTree tree
    x             -> x
  quickSongID hashed = let
    -- want these to be higher than real DLC, but lower than C3 IDs
    n = hash hashed `mod` 1000000000
    minID = 10000000
    in fromIntegral $ if n < minID then n + minID else n

makeWiiDTA :: Int32 -> Chunk B.ByteString -> Chunk B.ByteString
makeWiiDTA previewLength = adjustChunk where
  adjustTree (Tree nid chunks) = Tree nid $ adjustChunks chunks
  adjustChunks = \case
    [Sym "preview", Int start, Int _end]
      -> [Sym "preview", Int start, Int $ start + previewLength]
    [Sym "name", String template]
      | "songs/" `B.isPrefixOf` template
      -> [Sym "name", String $ "dlc/sZAE/001/content/" <> template]
    xs -> map adjustChunk xs
  adjustChunk = \case
    Parens   tree -> Parens   $ adjustTree tree
    Braces   tree -> Braces   $ adjustTree tree
    Brackets tree -> Brackets $ adjustTree tree
    x             -> x

data WiiDestination = WiiMeta | WiiSong

getWiiFile :: (MonadIO m, SendMessage m) => (T.Text, QuickFile Readable) -> StackTraceT m (T.Text, (WiiDestination, Readable))
getWiiFile (name, qfile) = case qfile of
  QFEncryptedMOGG r -> return (name, (WiiSong, fixOldC3Mogg r))
  QFUnencryptedMOGG r -> return (name, (WiiSong, r))
  QFMilo r -> let
    name' = T.pack $ dropExtension (T.unpack name) <> ".milo_wii"
    in return (name', (WiiSong, r))
  QFPNGXbox r -> stackIO $ do
    let name' = T.pack $ dropExtension (T.unpack name) <> ".png_wii"
    bs <- useHandle r handleToByteString
    let bs' = I.toDXT1File I.PNGWii $ pixelMap dropTransparency $ I.readRBImage False bs
        r' = makeHandle (T.unpack name') $ byteStringSimpleHandle bs'
    return (name', (WiiMeta, r'))
  QFPNGPS3 r -> stackIO $ do
    let name' = T.pack $ dropExtension (T.unpack name) <> ".png_wii"
    bs <- useHandle r handleToByteString
    let bs' = I.toDXT1File I.PNGWii $ pixelMap dropTransparency $ I.readRBImage True bs
        r' = makeHandle (T.unpack name') $ byteStringSimpleHandle bs'
    return (name', (WiiMeta, r'))
  QFEncryptedMIDI crypt r -> do
    let name' = case splitExtension $ T.unpack name of
          (base, ".edat") -> T.pack base
          _               -> name
    r' <- decryptEDAT crypt (TE.encodeUtf8 name) r
    return (name', (WiiSong, r'))
  QFUnencryptedMIDI r -> return (name, (WiiSong, r))
  QFParsedMIDI mid -> getWiiFile (name, QFUnencryptedMIDI $ makeHandle "" $ byteStringSimpleHandle $ Save.toByteString mid)
  QFOther r -> return (name, (WiiSong, r)) -- should warn maybe?

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
      songsDTA = makeHandle "songs.dta" $ byteStringSimpleHandle $ glueDTA $ do
        qsong <- qsongs
        return (qdtaParsed $ quickSongDTA qsong, qdtaComments $ quickSongDTA qsong)
  stackIO $ makeCONReadable opts topFolder fout

data QuickPS3Folder
  = QCOneFolder
  | QCSeparateFolders
  | QCCustomFolder B.ByteString

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
  let oneFolder = B8.pack $ take 0x1B $ filter (\c -> isAlphaNum c && isAscii c)
        $ "O" <> take 6 (show $ hash $ map (qdtaOriginal . quickSongDTA) qsongs) <> case qsongs of
          []        -> "" -- shouldn't happen
          qsong : _ -> T.unpack (qdtaTitle $ quickSongDTA qsong)
      contentID = npdContentID
        $ (if qcPS3RB3 settings then rb3CustomMidEdatConfig else rb2CustomMidEdatConfig) oneFolder
  qsongMapping <- fmap (Map.unionsWith (<>)) $ forM qsongs $ \qsong -> do
    let separateFolder = B8.pack $ take 0x1B $ filter (\c -> isAlphaNum c && isAscii c)
          $ "O" <> take 6 (show $ hash $ qdtaOriginal $ quickSongDTA qsong) <> T.unpack (qdtaTitle $ quickSongDTA qsong)
        chosenFolder = case qcPS3Folder settings of
          Nothing -> case quickSongPS3Folder qsong of
            Just dir -> dir -- came from ps3, keep the same subfolder
            Nothing  -> separateFolder -- came from xbox, assign new random folder
          Just QCOneFolder -> oneFolder -- one folder for whole pkg
          Just QCSeparateFolders -> separateFolder -- separate folder for each song
          Just (QCCustomFolder bs) -> bs
        mcrypt = guard (qcPS3Encrypt settings) >> Just (chosenFolder, qcPS3RB3 settings)
    ps3Folder <- mapMFilesWithName (getPS3File mcrypt . discardSize) $ quickSongFiles qsong
    return $ Map.singleton chosenFolder [(qsong, ps3Folder)]
  let rootFolder = container "USRDIR" $ mconcat $ do
        (usrdirSub, songs) <- Map.toList qsongMapping
        let songsDTA = makeHandle "songs.dta" $ byteStringSimpleHandle $ glueDTA $ do
              (qsong, _) <- songs
              return (qdtaParsed $ quickSongDTA qsong, qdtaComments $ quickSongDTA qsong)
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

data QuickDolphinSettings = QuickDolphinSettings
  { qcDolphinPreview :: Bool -- if True, try to make preview audio
  }

saveQuickSongsDolphin :: (MonadResource m, SendMessage m) => [QuickSong] -> QuickDolphinSettings -> FilePath -> StackTraceT m [FilePath]
saveQuickSongsDolphin qsongs settings dout = tempDir "onyx-dolphin" $ \temp -> do
  let container name inner = Folder { folderSubfolders = [(name, inner)], folderFiles = [] }
  triples <- forM qsongs $ \qsong -> do
    lg $ T.unpack $ T.intercalate " - " $ concat
      [ toList $ qdtaArtist $ quickSongDTA qsong
      , [qdtaTitle $ quickSongDTA qsong]
      ]
    wiiFiles <- mapMFilesWithName (getWiiFile . discardSize) $ quickSongFiles qsong
    let (prevStart, prevEnd) = qdtaPreview $ quickSongDTA qsong
        prevLength = min 15000 $ prevEnd - prevStart
        dta = makeWiiDTA prevLength $ qdtaParsed $ quickSongDTA qsong
        fullOgg = temp </> "full.ogg"
        prevOgg = temp </> "preview.ogg"
        prevMogg = temp </> "preview.mogg"
        silentPreview = stackIO
          $ runResourceT
          $ sinkSnd prevOgg (Snd.Format Snd.HeaderFormatOgg Snd.SampleFormatVorbis Snd.EndianFile)
          $ (CA.silent (CA.Seconds $ realToFrac prevLength / 1000) 44100 2 :: CA.AudioSource (ResourceT IO) Int16)
    if qcDolphinPreview settings
      then case [r | (_, QFUnencryptedMOGG r) <- toList $ quickSongFiles qsong] of
        [] -> do
          lg "No unencrypted .mogg found, making silent preview"
          silentPreview
        r : _ -> do
          -- TODO wanted to use ffSourceFrom (with Readable), but it crashes! no idea why
          errorToEither (stackIO $ saveReadable (moggToOgg r) fullOgg) >>= \case
            -- TODO the above error catch is untested since moving to moggcrypt
            Right () -> do
              src <- stackIO $ sourceSndFrom (CA.Seconds $ realToFrac prevStart / 1000) fullOgg
              stackIO
                $ runResourceT
                $ sinkSnd prevOgg (Snd.Format Snd.HeaderFormatOgg Snd.SampleFormatVorbis Snd.EndianFile)
                $ fadeStart (CA.Seconds 0.75)
                $ fadeEnd (CA.Seconds 0.75)
                $ CA.takeStart (CA.Seconds $ realToFrac prevLength / 1000)
                $ applyPansVols (qdtaPans $ quickSongDTA qsong) (qdtaVols $ quickSongDTA qsong)
                $ src
            Left _err -> do
              lg "Couldn't decrypt audio, making silent preview"
              silentPreview
      else silentPreview
    oggToMogg prevOgg prevMogg
    previewReadable <- stackIO $ makeHandle "preview mogg" . byteStringSimpleHandle . BL.fromStrict <$> B.readFile prevMogg
    let metaContents = container "songs" $ container (qdtaFolder $ quickSongDTA qsong) $ flip mapMaybeFolder wiiFiles $ \case
          (WiiMeta, r) -> Just r
          _            -> Nothing
        previewContents = container "songs" $ container (qdtaFolder $ quickSongDTA qsong) Folder
          { folderSubfolders = []
          , folderFiles = [(qdtaFolder (quickSongDTA qsong) <> "_prev.mogg", previewReadable)]
          }
        songContents = container "songs" $ container (qdtaFolder $ quickSongDTA qsong) $ flip mapMaybeFolder wiiFiles $ \case
          (WiiSong, r) -> Just r
          _            -> Nothing
    return ((dta, qdtaComments $ quickSongDTA qsong), metaContents <> previewContents, songContents)
  let dta = makeHandle "songs.dta" $ byteStringSimpleHandle $ glueDTA $ map fst3 triples
      dtaFolder = container "songs" Folder
        { folderSubfolders = []
        , folderFiles = [("songs.dta", dta)]
        }
      meta = container "content" $ first TE.encodeUtf8 (mconcat $ map snd3 triples) <> dtaFolder
      song = container "content" $ first TE.encodeUtf8 (mconcat $ map thd3 triples)
      out1 = dout </> "00000001.app"
      out2 = dout </> "00000002.app"
  stackIO $ packU8Folder meta out1
  stackIO $ packU8Folder song out2
  return [out1, out2]

glueDTA :: [(Chunk B.ByteString, [B.ByteString])] -> BL.ByteString
glueDTA = let
  chunkLatin1 (chunk, comments) = insertCommentsBytes (DTA 0 $ Tree 0 [chunk]) comments
  in BL.intercalate "\n" . map chunkLatin1

-- Useful processors for modifying songs

blackVenue :: Bool -> F.T B.ByteString -> F.T B.ByteString
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

removePitch :: (NNC.C t) => Int -> RTB.T t (E.T B.ByteString) -> RTB.T t (E.T B.ByteString)
removePitch n = RTB.filter $ maybe True (\(p, _) -> p /= n) . isNoteEdge

noOverdrive :: F.T B.ByteString -> F.T B.ByteString
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

noLanesGBK :: F.T B.ByteString -> F.T B.ByteString
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

noLanesDrums :: F.T B.ByteString -> F.T B.ByteString
noLanesDrums (F.Cons typ dvn trks) = F.Cons typ dvn $ flip map trks $ \trk ->
  case U.trackName trk of
    Just "PART DRUMS" -> removePitch 116 $ removePitch 117 trk
    _                 -> trk

noDrumFills :: F.T B.ByteString -> F.T B.ByteString
noDrumFills (F.Cons typ dvn trks) = F.Cons typ dvn $ let
   -- remove drum fills, except starting at [coda]
  isCoda = \case
    E.MetaEvent (Meta.TextEvent "[coda]") -> True
    _                                     -> False
  maybeCoda = listToMaybe $ do
    trk <- trks
    guard $ U.trackName trk == Just "EVENTS"
    case RTB.filter isCoda trk of
      Wait t _ _ -> [t]
      _          -> []
  keepDrumEvent (t, evt) = case isNoteEdge evt of
    Just (pitch, on) | 120 <= pitch && pitch <= 124 -> if on
      then maybe False (t >=) maybeCoda -- keep note-on if equal since it's the BRE start
      else maybe False (t > ) maybeCoda
    _ -> True
  in flip map trks $ \trk -> case U.trackName trk of
    Just "PART DRUMS" -> RTB.fromAbsoluteEventList $ ATB.fromPairList
      $ filter keepDrumEvent $ ATB.toPairList $ RTB.toAbsoluteEventList 0 trk
    _ -> trk

mustang22 :: F.T B.ByteString -> F.T B.ByteString
mustang22 (F.Cons typ dvn trksOrig) = F.Cons typ dvn $ let
  -- for pg/gb, if both 17 and 22 tracks, remove 17, rename 22
  process name17 name22 trks = let
    (trk17, notTrk17 ) = partition (\trk -> U.trackName trk == Just name17) trks
    (trk22, notProtar) = partition (\trk -> U.trackName trk == Just name22) notTrk17
    in if not (null trk17) && not (null trk22)
      then notProtar <> map (U.setTrackName name17) trk22
      else trks
  in process "PART REAL_GUITAR" "PART REAL_GUITAR_22"
    $ process "PART REAL_BASS" "PART REAL_BASS_22" trksOrig

unmuteOver22 :: F.T B.ByteString -> F.T B.ByteString
unmuteOver22 (F.Cons typ dvn trks) = F.Cons typ dvn $ let
   -- for pg/pb, match note on/off, then unmute (ch 3 -> 0) if velocity 123 or above and pitch in pg note range
  processProtar trk = let
    (notes, notNotes) = RTB.partitionMaybe isNoteEdge' trk
    newNotes = splitEdgesSimple $ fmap processNote $ joinEdgesSimple notes
    protarPitches = IntSet.fromList $ [24, 48, 72, 96] >>= \base -> take 6 [base ..]
    processNote note@(v, (c, p), len) = if v >= 123 && c == 3 && IntSet.member p protarPitches
      then (v, (0, p), len)
      else note
    in RTB.merge (fmap makeEdge' newNotes) notNotes
  protarTracks =
    [ "PART REAL_GUITAR"
    , "PART REAL_GUITAR_22"
    , "PART REAL_BASS"
    , "PART REAL_BASS_22"
    ]
  in flip map trks $ \trk -> if maybe False (`elem` protarTracks) $ U.trackName trk
    then processProtar trk
    else trk

applyToMIDI
  :: (SendMessage m, MonadIO m)
  => (F.T B.ByteString -> F.T B.ByteString)
  -> Folder T.Text QuickFileSized
  -> StackTraceT m (Folder T.Text QuickFileSized)
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

decompressMilos :: (SendMessage m, MonadIO m) => Folder T.Text QuickFileSized -> StackTraceT m (Folder T.Text QuickFileSized)
decompressMilos = mapMFilesWithName $ \(name, (size, qfile)) -> do
  newFileSized <- case qfile of
    QFMilo r -> do
      bs <- stackIO $ useHandle r handleToByteString
      if BL.take 4 bs == "\xAF\xDE\xBE\xCA"
        then return (size, qfile) -- already uncompressed
        else errorToWarning (runGetM decompressMilo bs) >>= \case
          Nothing -> do
            warn "Couldn't decompress milo, leaving as is"
            return (size, qfile)
          Just decomp -> do
            let newMilo = addMiloHeader decomp
            return (fromIntegral $ BL.length newMilo, QFMilo $ makeHandle (T.unpack name) $ byteStringSimpleHandle newMilo)
    _ -> return (size, qfile)
  return (name, newFileSized)

-- Fills in the Deluxe author DTA tag from the C3 comment
transferAuthorToTag :: QuickSong -> QuickSong
transferAuthorToTag qsong = let
  qdta = quickSongDTA qsong
  isAuthorTag = \case
    Key "author" _ -> True
    _              -> False
  in case mapMaybe (B8.stripPrefix ";Song authored by ") $ qdtaComments qdta of
    []         -> qsong
    author : _ -> case qdtaParsed qdta of
      Parens (Tree w children@(_ : rest)) -> if any isAuthorTag rest
        then qsong -- existing author tag
        else qsong
          { quickSongDTA = qdta
            { qdtaParsed = Parens $ Tree w $ children <> [Key "author" [String author]]
            }
          }
      _ -> qsong -- warn?

-- Strips 2x Bass Pedal labels and BirdmanExe author tags like (B) (X) (Z) (O) (Rh) (I) etc.
stripTitleTags :: QuickSong -> QuickSong
stripTitleTags qsong = let
  qdta = quickSongDTA qsong
  strippableTag s = or
    [ elem (B8.map toLower s) ["2x bass pedal", "2x", "x+"]
    , case B8.unpack s of
      [x]    | isUpper x              -> True -- single letter author tags
      [x, y] | isUpper x && isLower y -> True -- double letter like (Rh)
      _                               -> False
    ]
  stripper title1 = case B.stripSuffix ")" title1 of
    Just title2 -> case B8.breakEnd (== '(') title2 of
      (title3, inParens) -> case B.stripSuffix " (" title3 of
        Just title4 -> if strippableTag inParens
          then stripper title4
          else title1
        Nothing -> title1
    Nothing -> title1
  eachChild = \case
    Key "name" [String s] -> Key "name" [String $ stripper s]
    x                     -> x
  in case qdtaParsed qdta of
    Parens (Tree w (k : rest)) -> qsong
      { quickSongDTA = qdta
        { qdtaParsed = Parens $ Tree w $ k : map eachChild rest
        -- could also strip from qdtaTitle but doesn't really matter
        }
      }
    _ -> qsong -- warn?

------------------------

-- Old direct con to pkg, works with non-songs like pro upgrades
conToPkg :: (SendMessage m, MonadResource m) => Bool -> FilePath -> FilePath -> StackTraceT m ()
conToPkg isRB3 fin fout = tempDir "onyx-con2pkg" $ \tmp -> do
  conFolder <- stackIO $ getSTFSFolder fin
  title <- stackIO $ withSTFSPackage fin $ return . T.concat . take 1 . md_DisplayName . stfsMetadata
  let rbps3Folder = T.toUpper $ T.pack $ take 0x1B $ filter (\c -> isAlphaNum c && isAscii c)
        $ "O" <> take 6 (show $ hash $ show $ void conFolder) <> T.unpack title
      rbps3EDATConfig = if isRB3
        then rb3CustomMidEdatConfig $ TE.encodeUtf8 rbps3Folder
        else rb2CustomMidEdatConfig $ TE.encodeUtf8 rbps3Folder
      rbps3ContentID = npdContentID rbps3EDATConfig
      container name inner = Folder { folderSubfolders = [(name, inner)], folderFiles = [] }
      adjustFiles folder = do
        newFiles <- forM (folderFiles folder) $ \(name, r) -> case map toLower $ takeExtension $ T.unpack name of
          ".mogg" -> stackIO $ do
            moggType <- useHandle r $ \h -> B.hGet h 1
            case B.unpack moggType of
              [0xA] -> let
                enc = makeHandle "encrypted mogg for ps3" $ encryptMOGGToByteString r >>= byteStringSimpleHandle
                in return (name, enc)
              _     -> return (name, r)
          ".mid" -> stackIO $ do
            let tmpIn  = tmp </> "in.mid"
                tmpOut = tmp </> "out.mid.edat"
                name' = name <> ".edat"
            saveReadable r tmpIn
            packNPData rbps3EDATConfig tmpIn tmpOut $ TE.encodeUtf8 name'
            r' <- makeHandle "" . byteStringSimpleHandle . BL.fromStrict <$> B.readFile tmpOut
            return (name', r')
          ".dta" -> do
            -- TODO get rid of BOM!
            b <- stackIO $ useHandle r handleToByteString
            mdta <- errorToWarning $ readDTA_latin1 $ BL.toStrict b
            case mdta of
              Just dta -> do
                let adjustDTA (DTA z tree) = DTA z $ adjustDTATree tree
                    adjustDTATree (Tree nid chunks) = Tree nid $ adjustDTAChunks chunks
                    adjustDTAChunks = \case
                      [Sym "rating", Int 4] -> [Sym "rating", Int 2]
                      xs                    -> map adjustDTAChunk xs
                    adjustDTAChunk = \case
                      Parens tree   -> Parens $ adjustDTATree tree
                      Braces tree   -> Braces $ adjustDTATree tree
                      Brackets tree -> Brackets $ adjustDTATree tree
                      x             -> x
                    b' = BL.fromStrict $ B8.pack $ T.unpack $ showDTA $ adjustDTA dta
                    r' = makeHandle "" $ byteStringSimpleHandle b'
                return (name, r')
              Nothing -> do
                warn "Couldn't parse songs.dta; passing through unmodified. (For official non-RBN DLC, this should be fine)"
                return (name, r)
          ".milo_xbox" -> do
            let name' = T.pack $ dropExtension (T.unpack name) <> ".milo_ps3"
            return (name', r)
          ".png_xbox" -> stackIO $ do
            b <- useHandle r handleToByteString
            let name' = T.pack $ dropExtension (T.unpack name) <> ".png_ps3"
                b' = BL.take 0x20 b <> BL.pack (swapBytes $ BL.unpack $ BL.drop 0x20 b)
                swapBytes (x : y : xs) = y : x : swapBytes xs
                swapBytes xs           = xs
                r' = makeHandle "" $ byteStringSimpleHandle b'
            return (name', r')
          _ -> return (name, r)
        newSubs <- forM (folderSubfolders folder) $ \(name, sub) -> do
          sub' <- adjustFiles sub
          return (name, sub')
        return Folder { folderSubfolders = newSubs, folderFiles = newFiles }
  main <- container "USRDIR" . container rbps3Folder <$> adjustFiles conFolder
  stackIO $ do
    extraPath <- getResourcesPath $ if isRB3 then "pkg-contents/rb3" else "pkg-contents/rb2"
    extra <- crawlFolder extraPath
    makePKG rbps3ContentID (first TE.encodeUtf8 $ main <> extra) fout
