{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Onyx.Neversoft.GH3.Metadata where

import           Control.Monad.Extra          (forM, forM_, guard, replicateM)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Bifunctor               (first)
import           Data.Binary.Get
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import           Data.Char                    (toLower)
import           Data.Either                  (lefts, rights)
import           Data.List.Extra              (nubOrdOn)
import           Data.Maybe                   (catMaybes, fromMaybe,
                                               listToMaybe, mapMaybe)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Word
import           GHC.ByteOrder
import           Onyx.Harmonix.Ark.GH2        (SongSort (..), sortSongs)
import           Onyx.Neversoft.CRC
import           Onyx.Neversoft.Pak
import           Onyx.Neversoft.QB
import           Onyx.PlayStation.NPData      (gh3CustomMidEdatConfig,
                                               npdContentID, packNPData)
import           Onyx.PlayStation.PKG         (getDecryptedUSRDIR, loadPKG,
                                               makePKG, pkgFolder)
import           Onyx.Preferences             (Preferences (prefArtistSort),
                                               readPreferences)
import           Onyx.Resources               (getResourcesPath, gh3Thumbnail)
import           Onyx.StackTrace
import           Onyx.Util.Files              (shortWindowsPath)
import           Onyx.Util.Handle             (Folder (..), Readable,
                                               byteStringSimpleHandle,
                                               crawlFolder, handleToByteString,
                                               makeHandle, useHandle)
import           Onyx.Xbox.STFS               (CreateOptions (..),
                                               LicenseEntry (..), getSTFSFolder,
                                               makeCONMemory, runGetM)
import           System.FilePath              (dropExtension, takeExtension,
                                               (</>))

-- Metadata in _text.pak.qb for GH3

data GH3TextPakQB = GH3TextPakQB
  { gh3TextPakSongStructs :: [(Word32, [QBStructItem QSResult Word32])]
  , gh3OtherNodes         :: [(Node, BL.ByteString)] -- used to get section names later
  } deriving (Show)

readGH3TextPakQBDisc :: (MonadFail m, ?endian :: ByteOrder) => BL.ByteString -> BL.ByteString -> m GH3TextPakQB
readGH3TextPakQBDisc pak pab = do
  nodes <- splitPakNodes (pakFormatGH3 ?endian) pak $ Just pab
  readGH3TextPakQB nodes

readGH3TextPakQBDLC :: (MonadFail m) => BL.ByteString -> m GH3TextPakQB
readGH3TextPakQBDLC pak = do
  let ?endian = BigEndian
  nodes <- splitPakNodes (pakFormatGH3 ?endian) pak Nothing
  readGH3TextPakQB nodes

data GH3Language
  = GH3English
  | GH3French
  | GH3German
  | GH3Italian
  | GH3Spanish
  deriving (Eq)

readGH3TextSetDLC :: (MonadFail m, MonadIO m) => Folder T.Text Readable -> m [(GH3Language, GH3TextPakQB)]
readGH3TextSetDLC folder = let
  -- dropExtension drops either .xen or .ps3
  stripLang name = case T.pack $ dropExtension $ T.unpack $ T.toLower name of
    (T.stripSuffix "_text.pak"   -> Just dlName) -> Just (GH3English, dlName)
    (T.stripSuffix "_text_f.pak" -> Just dlName) -> Just (GH3French , dlName)
    (T.stripSuffix "_text_g.pak" -> Just dlName) -> Just (GH3German , dlName)
    (T.stripSuffix "_text_i.pak" -> Just dlName) -> Just (GH3Italian, dlName)
    (T.stripSuffix "_text_s.pak" -> Just dlName) -> Just (GH3Spanish, dlName)
    _                                            -> Nothing
  in fmap catMaybes $ forM (folderFiles folder) $ \(name, r) -> case stripLang name of
    -- require "dl" in front to ignore GHWT files in Death Magnetic
    Just (lang, dlName) | "dl" `T.isPrefixOf` dlName -> do
      bs <- liftIO $ useHandle r handleToByteString
      text <- readGH3TextPakQBDLC bs
      return $ Just (lang, text)
    _ -> return Nothing

loadGH3TextSetDLC :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m [(GH3Language, GH3TextPakQB)]
loadGH3TextSetDLC f = case map toLower $ takeExtension f of
  ".pkg" -> do
    usrdirs <- stackIO (pkgFolder <$> loadPKG f) >>= getDecryptedUSRDIR
    fmap concat $ forM usrdirs $ \(_, usrdir) -> do
      readGH3TextSetDLC $ first TE.decodeLatin1 usrdir
  _      -> stackIO (getSTFSFolder f) >>= readGH3TextSetDLC

buildGH3TextSet :: Preferences -> B.ByteString -> GH3Language -> [GH3TextPakQB] -> (BL.ByteString, [T.Text])
buildGH3TextSet prefs dlName lang paks = let
  otherNodes = nubOrdOn (nodeFilenameKey . fst) $ paks >>= gh3OtherNodes
  unk1 = qbKeyCRC $ "1o99lm\\" <> dlName <> ".qb"
  unk2 = qbKeyCRC $ "7buqvk" <> dlName
  sortAlgo = if prefArtistSort prefs
    then SongSortArtistTitle
    else SongSortTitleArtist
  allSongData
    = sortSongs sortAlgo (\(k, items) -> case parseSongInfoGH3 items of
      Left  _    -> (T.pack $ show k, "") -- hopefully shouldn't happen?
      Right song -> (gh3Title song, gh3Artist song)
      )
    $ nubOrdOn fst
    $ paks >>= gh3TextPakSongStructs
  allSongIDs = map fst allSongData
  metadataQB =
    ( Node
      { nodeFileType       = qbKeyCRC ".qb"
      , nodeOffset         = 0
      , nodeSize           = 0
      , nodeFilenamePakKey = 0
      , nodeFilenameKey    = unk1
      , nodeFilenameCRC    = unk2
      , nodeUnknown        = 0
      , nodeFlags          = 0
      , nodeName           = Nothing
      }
    , putQB $ discardQS
      [ QBSectionStruct 1293449288 unk1
        [ QBStructHeader
        , QBStructItemString830000 (qbKeyCRC "prefix") "download"
        , QBStructItemInteger810000 2923132203 1
        , QBStructItemStruct8A0000 1902830817
          [ QBStructHeader
          , QBStructItemStringW (qbKeyCRC "title") $ case lang of
            GH3English -> "Downloaded songs"
            GH3French  -> "Chansons téléchargées"
            GH3German  -> "Heruntergel. Songs"
            GH3Italian -> "Canzoni scaricate"
            GH3Spanish -> "Temas descargados"
          , QBStructItemArray8C0000 (qbKeyCRC "songs") $ QBArrayOfQbKey allSongIDs
          , QBStructItemQbKey8D0000 0 637243660
          , QBStructItemQbKey8D0000 (qbKeyCRC "level") 1568516040
          ]
        ]
      , QBSectionArray (qbKeyCRC "download_songlist") unk1 $ QBArrayOfQbKey allSongIDs
      , QBSectionStruct (qbKeyCRC "download_songlist_props") unk1
        $ QBStructHeader
        : [ QBStructItemStruct8A0000 x y | (x, y) <- allSongData ]
      ]
    )
  end =
    ( Node
      { nodeFileType = qbKeyCRC ".last"
      , nodeOffset = 1
      , nodeSize = 0
      , nodeFilenamePakKey = 0
      , nodeFilenameKey = qbKeyCRC "chunk.last"
      , nodeFilenameCRC = qbKeyCRC "chunk"
      , nodeUnknown = 0
      , nodeFlags = 0
      , nodeName = Nothing
      }
    , BL.replicate 4 0xAB
    )
  songsLog = flip map allSongData $ \(k, items) -> case parseSongInfoGH3 items of
    Left  _    -> "[" <> T.pack (show k) <> "] (couldn't recognize metadata keys)"
    Right song -> T.pack (show $ gh3Title song) <> " (" <> gh3Artist song <> ")"
  in (buildPak $ otherNodes <> [metadataQB, end], songsLog)

combineGH3SongCache360 :: (SendMessage m, MonadIO m) => [FilePath] -> FilePath -> StackTraceT m ()
combineGH3SongCache360 ins out = do
  sets <- fmap concat $ mapM loadGH3TextSetDLC ins
  let dlText = "dl2000000000"
      dlBytes = B8.pack $ T.unpack dlText
  mystery <- gh3MysteryScript dlBytes
  prefs <- readPreferences
  let folder = Folder
        { folderSubfolders = []
        , folderFiles =
          [ (dlText <> ".pak.xen"       , mystery                     )
          , (dlText <> "_text.pak.xen"  , eng                         )
          , (dlText <> "_text_f.pak.xen", fst $ getLanguage GH3French )
          , (dlText <> "_text_g.pak.xen", fst $ getLanguage GH3German )
          , (dlText <> "_text_i.pak.xen", fst $ getLanguage GH3Italian)
          , (dlText <> "_text_s.pak.xen", fst $ getLanguage GH3Spanish)
          ]
        }
      (eng, engLog) = getLanguage GH3English
      getLanguage lang = buildGH3TextSet prefs dlBytes lang
        [ pak | (lang', pak) <- sets, lang == lang' ]
  lg $ "Found " <> show (length engLog) <> " songs"
  forM_ engLog $ \line -> lg $ "- " <> T.unpack line
  thumb <- liftIO $ gh3Thumbnail >>= B.readFile
  liftIO $ makeCONMemory CreateOptions
    { createNames = replicate 6 "GH3 Customs Database"
    , createDescriptions = []
    , createTitleID = 0x415607F7
    , createTitleName = "Guitar Hero 3"
    , createThumb = thumb
    , createTitleThumb = thumb
    , createLicenses = [LicenseEntry (-1) 1 1, LicenseEntry (-1) 1 0]
    , createMediaID       = 0
    , createVersion       = 0
    , createBaseVersion   = 0
    , createTransferFlags = 0xC0
    , createLIVE = True
    } folder out

combineGH3SongCachePS3 :: (SendMessage m, MonadIO m, MonadResource m) => [FilePath] -> FilePath -> StackTraceT m ()
combineGH3SongCachePS3 ins out = do
  sets <- fmap concat $ mapM loadGH3TextSetDLC ins
  let dlText = "dl2000000000"
      dlBytes = B8.pack $ T.unpack dlText
      folderLabel = "CUSTOMS_DATABASE"
      edatConfig = gh3CustomMidEdatConfig folderLabel
  mystery <- gh3MysteryScript dlBytes
  prefs <- readPreferences
  let files =
        [ (dlText <> ".pak.ps3"       , mystery                     )
        , (dlText <> "_text.pak.ps3"  , eng                         )
        , (dlText <> "_text_f.pak.ps3", fst $ getLanguage GH3French )
        , (dlText <> "_text_g.pak.ps3", fst $ getLanguage GH3German )
        , (dlText <> "_text_i.pak.ps3", fst $ getLanguage GH3Italian)
        , (dlText <> "_text_s.pak.ps3", fst $ getLanguage GH3Spanish)
        ]
      (eng, engLog) = getLanguage GH3English
      getLanguage lang = buildGH3TextSet prefs dlBytes lang
        [ pak | (lang', pak) <- sets, lang == lang' ]
  lg $ "Found " <> show (length engLog) <> " songs"
  forM_ engLog $ \line -> lg $ "- " <> T.unpack line
  edats <- tempDir "onyx-gh3-ps3" $ \tmp -> do
    let fin  = tmp </> "tmp.pak"
        fout = tmp </> "tmp.edat"
    fout' <- shortWindowsPath True fout
    forM files $ \(name, bs) -> do
      let nameText  = T.toUpper name <> ".EDAT"
          nameBytes = TE.encodeUtf8 nameText
      stackIO $ BL.writeFile fin bs
      fin' <- shortWindowsPath False fin
      stackIO $ packNPData edatConfig fin' fout' nameBytes
      bs' <- stackIO $ BL.fromStrict <$> B.readFile fout'
      return (nameBytes, makeHandle (T.unpack nameText) $ byteStringSimpleHandle bs')
  let main = Folder
        { folderSubfolders = return $ (,) "USRDIR" Folder
          { folderSubfolders = return $ (,) folderLabel Folder
            { folderSubfolders = []
            , folderFiles = edats
            }
          , folderFiles = []
          }
        , folderFiles = []
        }
  extra <- stackIO $ getResourcesPath "pkg-contents/gh3" >>= fmap (first TE.encodeUtf8) . crawlFolder
  stackIO $ makePKG (npdContentID edatConfig) (main <> extra) out

readGH3TextPakQB :: (MonadFail m, ?endian :: ByteOrder) => [(Node, BL.ByteString)] -> m GH3TextPakQB
readGH3TextPakQB nodes = do
  let mappingQS = qsBank nodes -- could also filter by matching nodeFilenameCRC
      getSonglistProps qb = do
        QBSectionStruct structID _fileID (QBStructHeader : songs) <- qb
        guard $ elem structID
          [ qbKeyCRC "permanent_songlist_props" -- disc
          , qbKeyCRC "download_songlist_props" -- dlc
          ]
        songs
  sortedNodes <- forM nodes $ \pair@(node, bs) -> if nodeFileType node == qbKeyCRC ".qb"
    then do
      -- this will just be an empty list if we can't parse the qb,
      -- such as some parts of gh3 disc .pak we don't care about
      let qb = fromMaybe [] $ map (lookupQS mappingQS) <$> runGetM parseQB bs
      return $ case getSonglistProps qb of
        []    -> Left  pair
        songs -> Right songs
    else return $ Left pair
  structs <- forM (concat $ rights sortedNodes) $ \case
    QBStructItemStruct8A0000 k struct -> return (k, struct)
    item -> fail $ "Unexpected item in _text.pak instead of song struct: " <> show item
  return $ GH3TextPakQB
    { gh3TextPakSongStructs = structs
    , gh3OtherNodes
      = filter (\(node, _) -> nodeFileType node /= qbKeyCRC ".last")
      $ lefts sortedNodes
    }

data GH3Singer
  = GH3SingerFemale
  | GH3SingerMale
  | GH3SingerBret
  deriving (Show)

data SongInfoGH3 = SongInfoGH3
  { gh3Name                 :: B.ByteString -- this is an id like "dlc3"
  , gh3Title                :: T.Text
  , gh3Artist               :: T.Text
  , gh3Year                 :: Maybe T.Text
  , gh3Singer               :: Maybe GH3Singer
  , gh3Keyboard             :: Bool
  , gh3BandPlaybackVolume   :: Float
  , gh3GuitarPlaybackVolume :: Float
  , gh3RhythmTrack          :: Bool
  , gh3UseCoopNotetracks    :: Bool
  , gh3HammerOnMeasureScale :: Maybe Float
  -- lots of other fields ignored
  } deriving (Show)

parseSongInfoGH3 :: [QBStructItem QSResult Word32] -> Either String SongInfoGH3
parseSongInfoGH3 songEntries = do
  gh3Name <- case [ s | QBStructItemString830000 k s <- songEntries, k == qbKeyCRC "name" ] of
    s : _ -> Right s
    []    -> Left "parseSongInfoGH3: couldn't get song internal name"
  let getString key = let
        crc = qbKeyCRC key
        in listToMaybe $ songEntries >>= \case
          QBStructItemStringW       k s | k == crc -> [s]                 -- 360
          QBStructItemString830000  k s | k == crc -> [TE.decodeLatin1 s] -- PS2
          _                                        -> []
  gh3Title <- maybe (Left $ "parseSongInfoGH3: couldn't get song title") Right $ getString "title"
  gh3Artist <- maybe (Left $ "parseSongInfoGH3: couldn't get song artist") Right $ getString "artist"
  -- tutorial songs don't have a year
  let gh3Year = getString "year"
  gh3Singer <- case [ s | QBStructItemQbKey8D0000 k s <- songEntries, k == qbKeyCRC "singer" ] of
    s : _
      | s == qbKeyCRC "female" -> Right $ Just GH3SingerFemale
      | s == qbKeyCRC "male"   -> Right $ Just GH3SingerMale
      | s == qbKeyCRC "bret"   -> Right $ Just GH3SingerBret
      | s == qbKeyCRC "none"   -> Right Nothing
      | otherwise              -> Left $ "parseSongInfoGH3: unrecognized key for singer (song " <> show gh3Name <> ", key " <> show s <> ")" -- should probably just be warning
    []    -> Right Nothing
  gh3Keyboard <- case [ s | QBStructItemQbKey8D0000 k s <- songEntries, k == qbKeyCRC "keyboard" ] of
    s : _
      | s == qbKeyCRC "true"  -> Right True
      | s == qbKeyCRC "false" -> Right False
      | otherwise             -> Left $ "parseSongInfoGH3: unrecognized key for keyboard (song " <> show gh3Name <> ", key " <> show s <> ")" -- should probably just be warning
    []    -> Right False
  let findFloat key = let
        crc = qbKeyCRC key
        in listToMaybe $ flip mapMaybe songEntries $ \case
          QBStructItemInteger810000 k x | k == crc -> Just $ fromIntegral x
          QBStructItemFloat820000   k x | k == crc -> Just x
          _                                        -> Nothing
      gh3BandPlaybackVolume   = fromMaybe 0 $ findFloat "band_playback_volume"
      gh3GuitarPlaybackVolume = fromMaybe 0 $ findFloat "guitar_playback_volume"
      gh3UseCoopNotetracks    = not $ null
        [ () | QBStructItemQbKey8D0000 0 k <- songEntries, k == qbKeyCRC "use_coop_notetracks" ]
      gh3HammerOnMeasureScale = listToMaybe
        [ n | QBStructItemFloat820000 k n <- songEntries, k == qbKeyCRC "hammer_on_measure_scale" ]
  gh3RhythmTrack <- case [ n | QBStructItemInteger810000 k n <- songEntries, k == qbKeyCRC "rhythm_track" ] of
    0 : _ -> Right False
    1 : _ -> Right True
    _ : _ -> Left "parseSongInfoGH3: unrecognized value for coop-is-rhythm key"
    []    -> Right False
  Right SongInfoGH3{..}

-- Unknown contents of "dl<num>.pak.xen"
gh3MysteryScript :: (MonadIO m) => B.ByteString -> m BL.ByteString
gh3MysteryScript dlName = do
  -- copying hopefully-complete script from SanicStudios (DLC added bits on the end over time)
  mysteryScript <- liftIO $ getResourcesPath "gh3-mystery-script.qb" >>= fmap BL.fromStrict . B.readFile
  let nodes =
        [ ( Node {nodeFileType = qbKeyCRC ".qb", nodeOffset = 0, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = unk1, nodeFilenameCRC = qbKeyCRC dlName, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
          , mysteryScript
          )
        , ( Node {nodeFileType = qbKeyCRC ".last", nodeOffset = 1, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = qbKeyCRC "chunk.last", nodeFilenameCRC = qbKeyCRC "chunk", nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
          , BL.replicate 4 0xAB
          )
        ]
      -- don't know the actual prefix string but this works
      unk1 = qbKeyCRC $ "1ni76fm\\" <> dlName -- in dl15.pak it's 3159505916, in dl26.pak it's 3913805506
  return $ buildPak nodes

data GH3Dat = GH3Dat
  { gh3DatLength :: Word32 -- length in bytes of .fsb.xen
  , gh3DatAudio  :: [(Word32, Word32)] -- (qb key of e.g. "songname_guitar", fsb stream index)
  }

getGH3Dat :: Get GH3Dat
getGH3Dat = do
  count <- getWord32be
  gh3DatLength <- getWord32be
  gh3DatAudio <- replicateM (fromIntegral count) $ do
    nameKey <- getWord32be
    streamIndex <- getWord32be
    -- are these always zero?
    0 <- getWord32be
    0 <- getWord32be
    0 <- getWord32be
    return (nameKey, streamIndex)
  return GH3Dat{..}

{-
wii notes:
*.wad.ngc are several single track FSB3 glued together into one file. dunno codec yet
*.dat.ngc are different from 360: each track is
  qb key (like 360)
  start bytes in wad
  end bytes in wad (or length? didn't check)
  8 bytes 0
-}
