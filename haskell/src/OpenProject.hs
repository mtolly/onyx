{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module OpenProject where

import           Build
import           Config
import           Control.Applicative            ((<|>))
import           Control.Monad                  (forM, forM_, guard)
import qualified Control.Monad.Catch            as MC
import           Control.Monad.Codec.Onyx.JSON  (loadYaml)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import           Data.Bifunctor                 (first)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (isAlpha, toLower)
import           Data.DTA                       (Chunk (..), DTA (..),
                                                 Tree (..), readFileDTA)
import           Data.Foldable                  (toList)
import           Data.Functor                   (void)
import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.HashMap.Strict            as Map
import           Data.Int                       (Int32)
import           Data.List.Extra                (stripSuffix)
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Data.SimpleHandle              (Folder (..), crawlFolder,
                                                 fileReadable, findFile)
import qualified Data.Text                      as T
import           Data.Text.Encoding             (encodeUtf8)
import qualified Data.Text.Encoding             as TE
import           GuitarHeroII.Ark               (GH2Installation (..),
                                                 GameGH (..), addBonusSongGH1,
                                                 addBonusSongGH2, detectGameGH)
import           GuitarHeroII.Convert           (adjustSongText)
import           GuitarPro                      (parseGP, parseGPX)
import           Import.Amplitude2016           (importAmplitude)
import           Import.Base                    (ImportLevel (..), saveImport)
import           Import.BMS                     (importBMS)
import           Import.DTXMania                (importDTX, importSet)
import           Import.FretsOnFire             (importFoF)
import qualified Import.GuitarHero1             as GH1
import qualified Import.GuitarHero2             as GH2
import           Import.GuitarPro               (importGPIF)
import           Import.Magma                   (importMagma)
import           Import.Neversoft               (importGH3Disc, importGH5WoR,
                                                 importNeversoftGH)
import           Import.PowerGig                (importPowerGig)
import           Import.RockBand                (importRB4, importRBA,
                                                 importSTFSFolder)
import           Import.Rocksmith               as RS
import           PlayStation.PKG                (getDecryptedUSRDIR, loadPKG,
                                                 pkgFolder, tryDecryptEDAT)
import           Preferences
import qualified Sound.Jammit.Base              as J
import           STFS.Package                   (getSTFSFolder)
import qualified System.Directory               as Dir
import           System.FilePath                (dropExtension,
                                                 dropTrailingPathSeparator,
                                                 takeDirectory, takeExtension,
                                                 takeFileName, (-<.>), (</>))
import qualified System.IO                      as IO
import qualified System.IO.Temp                 as Temp
import           System.Random                  (randomRIO)

data Project = Project
  { projectLocation :: FilePath -- path to song.yml
  , projectSongYaml :: SongYaml FilePath
  , projectRelease  :: Maybe ReleaseKey -- delete the temp import dir early if you want
  , projectSource   :: FilePath -- absolute path to the source STFS file, PS dir, etc.
  , projectTemplate :: FilePath -- string you can append "_whatever" to for generated files
  }

resourceTempDir :: (MonadResource m) => m (ReleaseKey, FilePath)
resourceTempDir = do
  tmp <- liftIO Temp.getCanonicalTemporaryDirectory
  let ignoringIOErrors ioe = ioe `MC.catch` (\e -> const (return ()) (e :: IOError))
  allocate
    (Temp.createTempDirectory tmp "onyx")
    (ignoringIOErrors . Dir.removeDirectoryRecursive)

data Importable m = Importable
  { impTitle   :: Maybe T.Text
  , impArtist  :: Maybe T.Text
  , impAuthor  :: Maybe T.Text
  , impFormat  :: T.Text
  , impPath    :: FilePath
  , impIndex   :: Maybe Int
  , impProject :: StackTraceT m Project
  , imp2x      :: Bool -- True if only 2x bass pedal
  }

findAllSongs :: (SendMessage m, MonadResource m)
  => FilePath -> StackTraceT m [Importable m]
findAllSongs fp' = do
  (subs, imps) <- findSongs fp'
  concat . (imps :) <$> mapM findAllSongs subs

findSongs :: (SendMessage m, MonadResource m)
  => FilePath -> StackTraceT m ([FilePath], [Importable m])
findSongs fp' = inside ("searching: " <> fp') $ fmap (fromMaybe ([], [])) $ errorToWarning $ do
  fp <- stackIO $ Dir.makeAbsolute fp'
  let found imp = foundMany [imp]
      foundMany imps = return ([], imps)
      withYaml index loc isDir key fyml = loadYaml fyml >>= \yml -> return Project
        { projectLocation = fyml
        , projectSongYaml = yml
        , projectRelease = key
        , projectSource = loc
        , projectTemplate = fromMaybe
          ( (if isDir then id else dropExtension)
          $ dropTrailingPathSeparator loc
          ) (stripSuffix "_rb3con" loc <|> stripSuffix "_rb2con" loc)
          <> maybe "" (\i -> "_" <> show (i :: Int)) index
        }
      importFrom index loc isDir fn = do
        (key, tmp) <- resourceTempDir
        () <- fn tmp
        withYaml index loc isDir (Just key) (tmp </> "song.yml")
      foundFoF loc = do
        -- loc can be a .ini or .chart
        let dir = takeDirectory loc
        foundImport "Frets on Fire/Phase Shift/Clone Hero" dir $ importFoF dir
      foundGH loc = do
        let dir = takeDirectory loc
        stackIO (detectGameGH dir) >>= \case
          Nothing -> do
            warn $ "Couldn't detect GH game version for: " <> dir
            return ([], [])
          Just GameGH2DX2 -> GH2.importGH2 dir >>= foundImports "Guitar Hero II (DX)" dir
          Just GameGH2 -> GH2.importGH2 dir >>= foundImports "Guitar Hero II" dir
          Just GameGH1 -> GH1.importGH1 dir >>= foundImports "Guitar Hero (1)" dir
      foundDTXSet loc = importSet loc >>= foundImports "DTXMania (set.def)" (takeDirectory loc)
      foundDTX loc = foundImport "DTXMania" loc $ importDTX loc
      foundBME loc = foundImport "Be-Music Source" loc $ importBMS loc
      foundYaml loc = do
        let dir = takeDirectory loc
        yml <- loadYaml loc
        let _ = yml :: SongYaml FilePath
        found Importable
          { impTitle = _title $ _metadata yml
          , impArtist = _artist $ _metadata yml
          , impAuthor = _author $ _metadata yml
          , impFormat = "Onyx"
          , impPath = dir
          , impIndex = Nothing
          , impProject = withYaml Nothing dir True Nothing loc
          , imp2x = any (maybe False ((== Kicks2x) . drumsKicks) . partDrums) $ _parts yml
          }
      foundRBProj loc = foundImport "Magma Project" loc $ importMagma loc
      foundAmplitude loc = do
        let dir = takeDirectory loc
        foundImport "Amplitude (2016)" dir $ importAmplitude loc
      foundSTFS loc = do
        folder <- stackIO $ getSTFSFolder loc
        mconcat <$> sequence
          [ case findFile ("songs" :| ["songs.dta"]) folder of
            Just _ -> do
              imps <- importSTFSFolder loc folder
              foundImports "Rock Band (Xbox 360 CON/LIVE)" loc imps
            Nothing -> return ([], [])
          , case findFile ("config" :| ["songs.dta"]) folder of
            Just _ -> do
              imps <- GH2.importGH2DLC loc folder
              foundImports "Guitar Hero II (Xbox 360 DLC)" loc imps
            Nothing -> return ([], [])
          , if any (\(name, _) -> ".xen" `T.isSuffixOf` name) $ folderFiles folder
            then do
              imps <- importNeversoftGH loc folder
              foundImports "Guitar Hero (Neversoft) (360)" loc imps
            else return ([], [])
          , importRSXbox folder >>= foundImports "Rocksmith" loc
          , case mapMaybe (\(name, _) -> T.stripSuffix ".hdr.e.2" name) $ folderFiles folder of
            []    -> return ([], [])
            bases -> do
              imps <- concat <$> mapM (importPowerGig folder) bases
              foundImports "Power Gig (Xbox 360 DLC)" loc imps
          ]
      foundRS psarc = importRS (fileReadable psarc) >>= foundImports "Rocksmith" psarc
      foundRSPS3 edat = do
        mpsarc <- tryDecryptEDAT "" (B8.pack $ takeFileName edat) $ fileReadable edat
        case mpsarc of
          Nothing    -> fatal "Couldn't decrypt .psarc.edat"
          Just psarc -> importRS psarc >>= foundImports "Rocksmith" edat
      foundPS3 loc = do
        usrdirs <- stackIO (pkgFolder <$> loadPKG loc) >>= getDecryptedUSRDIR
        fmap mconcat $ forM usrdirs $ \(_, folderBS) -> let
          folder = first TE.decodeLatin1 folderBS
          in mconcat <$> sequence
            [ case findFile ("songs" :| ["songs.dta"]) folder of
              Just _ -> do
                imps <- importSTFSFolder loc folder
                foundImports "Rock Band (PS3 .pkg)" loc imps
              Nothing -> return ([], [])
            , if any (\(name, _) -> ".PS3" `T.isSuffixOf` name) $ folderFiles folder
              then do
                imps <- importGH5WoR loc folder
                foundImports "Guitar Hero (Neversoft) (PS3)" loc imps
              else return ([], [])
            ]
      foundGP loc = do
        let imp level = parseGP loc >>= \gpif -> importGPIF gpif level
        foundImports "Guitar Pro (.gp)" loc [imp]
      foundGPX loc = do
        let imp level = parseGPX loc >>= \gpif -> importGPIF gpif level
        foundImports "Guitar Pro (.gpx)" loc [imp]
      foundPowerGig loc = do
        dir <- stackIO $ crawlFolder $ takeDirectory loc
        let base = T.takeWhile (/= '.') $ T.pack $ takeFileName loc
        imps <- importPowerGig dir base
        foundImports "Power Gig (Xbox 360)" loc imps
      found360Game xex = do
        let loc = takeDirectory xex
        dir <- stackIO $ crawlFolder loc
        if any (\(name, _) -> T.toUpper name == "DATA") $ folderSubfolders dir
          then do
            imps <- importGH3Disc loc dir
            foundImports "Guitar Hero III (360)" loc imps
          else return ([], [])
      foundImports fmt path imports = do
        isDir <- stackIO $ Dir.doesDirectoryExist path
        let single = null $ drop 1 imports
        fmap ([],) $ forM (zip [0..] imports) $ \(i, imp) -> do
          quick <- imp ImportQuick
          let index = guard (not single) >> Just i
          return Importable
            { impTitle = _title $ _metadata quick
            , impArtist = _artist $ _metadata quick
            , impAuthor = _author $ _metadata quick
            , impFormat = fmt
            , impPath = path
            , impIndex = index
            , impProject = importFrom index path isDir $ \dout ->
              void $ imp ImportFull >>= stackIO . saveImport dout
            , imp2x = any (maybe False ((== Kicks2x) . drumsKicks) . partDrums) $ _parts quick
            }
      foundImport fmt path imp = foundImports fmt path [imp]
  isDir <- stackIO $ Dir.doesDirectoryExist fp
  if isDir
    then do
      ents <- stackIO $ Dir.listDirectory fp
      let lookFor [] = stackIO (Dir.doesFileExist $ fp </> "songs/songs.dta") >>= \case
            True  -> stackIO (crawlFolder fp)
              >>= importSTFSFolder fp
              >>= foundImports "Rock Band Extracted" fp
            False -> return (map (fp </>) ents, [])
          lookFor ((file, use) : rest) = case filter ((== file) . map toLower) ents of
            match : _ -> use $ fp </> match
            []        -> lookFor rest
      lookFor
        [ ("song.yml", foundYaml)
        , ("song.ini", foundFoF)
        , ("notes.chart", foundFoF)
        , ("set.def", foundDTXSet)
        , ("main.hdr", foundGH)
        ]
    else do
      case map toLower $ takeExtension fp of
        ".yml" -> foundYaml fp
        ".yaml" -> foundYaml fp
        ".rbproj" -> foundRBProj fp
        ".songdta_ps4" -> foundImport "Rock Band 4" (takeDirectory fp) $ importRB4 fp
        ".moggsong" -> stackIO (Dir.doesFileExist $ fp -<.> "songdta_ps4") >>= \case
          True  -> return ([], []) -- rb4, ignore this and import .songdta_ps4 instead
          False -> foundAmplitude fp
        ".chart" -> foundFoF fp
        ".dtx" -> foundDTX fp
        ".gda" -> foundDTX fp
        ".bms" -> foundBME fp
        ".bme" -> foundBME fp
        ".bml" -> foundBME fp
        ".psarc" -> foundRS fp
        ".edat" | map toLower (takeExtension $ dropExtension fp) == ".psarc"
          -> foundRSPS3 fp
        ".pkg" -> foundPS3 fp
        ".gp" -> foundGP fp
        ".gpx" -> foundGPX fp
        ".2" -> foundPowerGig fp -- assuming this is Data.hdr.e.2
        _ -> case map toLower $ takeFileName fp of
          "song.ini" -> foundFoF fp
          "set.def" -> foundDTXSet fp
          "main.hdr" -> foundGH fp
          "default.xex" -> found360Game fp
          _ -> do
            magic <- stackIO $ IO.withBinaryFile fp IO.ReadMode $ \h -> BL.hGet h 4
            case magic of
              "RBSF" -> foundImport "Magma RBA" fp $ importRBA fp
              "CON " -> foundSTFS fp
              "LIVE" -> foundSTFS fp
              _      -> return ([], [])

openProject :: (SendMessage m, MonadResource m) =>
  Maybe Int -> FilePath -> StackTraceT m Project
openProject i fp = do
  (_, imps) <- findSongs fp
  case i of
    Nothing -> case imps of
      [imp] -> impProject imp
      []    -> fatal $ "Couldn't find a song at location: " <> fp
      _     -> fatal $ unlines
        $ ("Found more than 1 song at location: " <> fp) : do
          (j, imp) <- zip [0 :: Int ..] imps
          let f = maybe "?" T.unpack
          return $ show j <> ": " <> f (impTitle imp) <> " (" <> f (impArtist imp) <> ")"
    Just j  -> case drop j imps of
      imp : _ -> impProject imp
      []      -> fatal $ "Couldn't find song #" <> show j <> " at location: " <> fp

withProject :: (SendMessage m, MonadResource m) =>
  Maybe Int -> FilePath -> (Project -> StackTraceT m a) -> StackTraceT m a
withProject i fp fn = do
  proj <- openProject i fp
  x <- fn proj
  stackIO $ mapM_ release $ projectRelease proj
  return x

getAudioDirs :: (SendMessage m, MonadIO m) => Project -> StackTraceT m [FilePath]
getAudioDirs proj = do
  jmt <- stackIO J.findJammitDir
  let addons = maybe id (:) jmt [takeDirectory $ projectLocation proj]
  dirs <- prefAudioDirs <$> readPreferences
  mapM (stackIO . Dir.canonicalizePath) $ addons ++ dirs

shakeBuild1 :: (MonadIO m) =>
  Project -> [(T.Text, Target FilePath)] -> FilePath -> StackTraceT (QueueLog m) FilePath
shakeBuild1 proj extraTargets = fmap runIdentity . shakeBuildMany proj extraTargets . Identity

shakeBuildMany :: (MonadIO m, Functor f, Foldable f) =>
  Project -> [(T.Text, Target FilePath)] -> f FilePath -> StackTraceT (QueueLog m) (f FilePath)
shakeBuildMany proj extraTargets buildables = do
  audioDirs <- getAudioDirs proj
  shakeBuild audioDirs (projectLocation proj) extraTargets $ toList buildables
  return $ fmap (takeDirectory (projectLocation proj) </>) buildables

buildCommon :: (MonadIO m) => Target FilePath -> (String -> FilePath) -> Project -> StackTraceT (QueueLog m) FilePath
buildCommon target getBuildable proj = do
  let targetHash = show $ hash target `mod` 100000000
      buildable = getBuildable targetHash
  shakeBuild1 proj [(T.pack targetHash, target)] buildable

randomRBSongID :: (MonadIO m) => m Int32
randomRBSongID = liftIO $ randomRIO (10000000, 1000000000)

buildRB3CON :: (MonadIO m) => TargetRB3 FilePath -> Project -> StackTraceT (QueueLog m) FilePath
buildRB3CON rb3 proj = do
  songID <- randomRBSongID
  let rb3' = rb3 { rb3_SongID = SongIDInt songID }
  buildCommon (RB3 rb3') (\targetHash -> "gen/target" </> targetHash </> "rb3con") proj

buildRB3PKG :: (MonadIO m) => TargetRB3 FilePath -> Project -> StackTraceT (QueueLog m) FilePath
buildRB3PKG rb3 proj = do
  songID <- randomRBSongID
  let rb3' = rb3 { rb3_SongID = SongIDInt songID }
  buildCommon (RB3 rb3') (\targetHash -> "gen/target" </> targetHash </> "rb3-ps3.pkg") proj

buildRB2CON :: (MonadIO m) => TargetRB2 -> Project -> StackTraceT (QueueLog m) FilePath
buildRB2CON rb2 proj = do
  songID <- randomRBSongID
  let rb2' = rb2 { rb2_SongID = SongIDInt songID }
  buildCommon (RB2 rb2') (\targetHash -> "gen/target" </> targetHash </> "rb2con") proj

buildRB2PKG :: (MonadIO m) => TargetRB2 -> Project -> StackTraceT (QueueLog m) FilePath
buildRB2PKG rb2 proj = do
  songID <- randomRBSongID
  let rb2' = rb2 { rb2_SongID = SongIDInt songID }
  buildCommon (RB2 rb2') (\targetHash -> "gen/target" </> targetHash </> "rb2-ps3.pkg") proj

buildMagmaV2 :: (MonadIO m) => TargetRB3 FilePath -> Project -> StackTraceT (QueueLog m) FilePath
buildMagmaV2 rb3 = buildCommon (RB3 rb3) $ \targetHash -> "gen/target" </> targetHash </> "magma"

buildPSDir :: (MonadIO m) => TargetPS -> Project -> StackTraceT (QueueLog m) FilePath
buildPSDir ps = buildCommon (PS ps) $ \targetHash -> "gen/target" </> targetHash </> "ps"

buildPSZip :: (MonadIO m) => TargetPS -> Project -> StackTraceT (QueueLog m) FilePath
buildPSZip ps = buildCommon (PS ps) $ \targetHash -> "gen/target" </> targetHash </> "ps.zip"

buildGH1Dir :: (MonadIO m) => TargetGH1 -> Project -> StackTraceT (QueueLog m) FilePath
buildGH1Dir gh1 = buildCommon (GH1 gh1) $ \targetHash -> "gen/target" </> targetHash </> "gh1"

buildGH2Dir :: (MonadIO m) => TargetGH2 -> Project -> StackTraceT (QueueLog m) FilePath
buildGH2Dir gh2 = buildCommon (GH2 gh2) $ \targetHash -> "gen/target" </> targetHash </> "gh2"

buildGH2LIVE :: (MonadIO m) => TargetGH2 -> Project -> StackTraceT (QueueLog m) FilePath
buildGH2LIVE gh2 = buildCommon (GH2 gh2) $ \targetHash -> "gen/target" </> targetHash </> "gh2live"

buildGHWORLIVE :: (MonadIO m) => TargetGH5 -> Project -> StackTraceT (QueueLog m) FilePath
buildGHWORLIVE gh5 = buildCommon (GH5 gh5) $ \targetHash -> "gen/target" </> targetHash </> "ghworlive"

buildGHWORPKG :: (MonadIO m) => TargetGH5 -> Project -> StackTraceT (QueueLog m) FilePath
buildGHWORPKG gh5 = buildCommon (GH5 gh5) $ \targetHash -> "gen/target" </> targetHash </> "ps3.pkg"

installGH1 :: (MonadIO m) => TargetGH1 -> Project -> FilePath -> StackTraceT (QueueLog m) ()
installGH1 gh1 proj gen = do
  stackIO (detectGameGH gen) >>= \case
    Nothing         -> fatal "Couldn't detect what game this ARK is for."
    Just GameGH1    -> return ()
    Just GameGH2    -> fatal "This appears to be a Guitar Hero II or 80's ARK!"
    Just GameGH2DX2 -> fatal "This appears to be a Guitar Hero II Deluxe ARK!"
  dir <- buildGH1Dir gh1 proj
  files <- stackIO $ Dir.listDirectory dir
  dta <- stackIO $ readFileDTA $ dir </> "songs-inner.dta"
  sym <- stackIO $ B.readFile $ dir </> "symbol"
  let chunks = treeChunks $ topTree $ fmap (B8.pack . T.unpack) dta
      filePairs = flip mapMaybe files $ \f -> do
        guard $ elem (takeExtension f) [".mid", ".vgs", ".voc"]
        return (sym <> B8.pack (dropWhile isAlpha f), dir </> f)
  let toBytes = B8.pack . T.unpack
  sortBonus <- prefSortGH2 <$> readPreferences
  stackIO $ addBonusSongGH1 GH2Installation
    { gh2i_GEN              = gen
    , gh2i_symbol           = sym
    , gh2i_song             = chunks
    , gh2i_coop_max_scores  = [] -- not used
    , gh2i_shop_title       = Just $ toBytes $ targetTitle (projectSongYaml proj) $ GH1 gh1 -- not used
    , gh2i_shop_description = Just $ toBytes $ T.unlines
      [ "Artist: " <> getArtist (_metadata $ projectSongYaml proj)
      , "Album: "  <> getAlbum  (_metadata $ projectSongYaml proj)
      , "Author: " <> getAuthor (_metadata $ projectSongYaml proj)
      ]
    , gh2i_author           = toBytes . adjustSongText <$> _author (_metadata $ projectSongYaml proj)
    , gh2i_album_art        = Nothing -- not used
    , gh2i_files            = filePairs
    , gh2i_sort             = sortBonus
    , gh2i_loading_phrase   = toBytes <$> gh1_LoadingPhrase gh1
    }

installGH2 :: (MonadIO m) => TargetGH2 -> Project -> FilePath -> StackTraceT (QueueLog m) ()
installGH2 gh2 proj gen = do
  isDX2 <- stackIO (detectGameGH gen) >>= \case
    Nothing         -> fatal "Couldn't detect what game this ARK is for."
    Just GameGH1    -> fatal "This appears to be a Guitar Hero (1) ARK!"
    Just GameGH2    -> do
      lg "ARK detected as GH2, no drums support."
      return False
    Just GameGH2DX2 -> do
      lg "ARK detected as GH2 Deluxe with drums support."
      return True
  dir <- buildGH2Dir gh2 proj
  files <- stackIO $ Dir.listDirectory dir
  dta <- stackIO $ readFileDTA $ if isDX2
    then dir </> "songs-inner-dx2.dta"
    else dir </> "songs-inner.dta"
  sym <- stackIO $ B.readFile $ dir </> "symbol"
  coop <- stackIO $ readFileDTA $ dir </> "coop_max_scores.dta"
  let chunks = treeChunks $ topTree $ fmap (B8.pack . T.unpack) dta
      filePairs = flip mapMaybe files $ \f -> do
        guard $ elem (takeExtension f) [".mid", ".vgs", ".voc"]
        return (sym <> B8.pack (dropWhile isAlpha f), dir </> f)
  coopNums <- case coop of
    DTA 0 (Tree _ [Parens (Tree _ [Sym _, Parens (Tree _ nums)])]) -> forM nums $ \case
      Int n -> return $ fromIntegral n
      _     -> fatal "unexcepted non-int in coop scores list"
    _ -> fatal "Couldn't read coop scores list"
  let toBytes = B8.pack . T.unpack
  sortBonus <- prefSortGH2 <$> readPreferences
  stackIO $ addBonusSongGH2 GH2Installation
    { gh2i_GEN              = gen
    , gh2i_symbol           = sym
    , gh2i_song             = chunks
    , gh2i_coop_max_scores  = coopNums
    , gh2i_shop_title       = Just $ toBytes $ targetTitle (projectSongYaml proj) $ GH2 gh2
    , gh2i_shop_description = Just $ toBytes $ T.unlines
      [ "Artist: " <> getArtist (_metadata $ projectSongYaml proj)
      , "Album: "  <> getAlbum  (_metadata $ projectSongYaml proj)
      , "Author: " <> getAuthor (_metadata $ projectSongYaml proj)
      ]
    , gh2i_author           = toBytes . adjustSongText <$> _author (_metadata $ projectSongYaml proj)
    , gh2i_album_art        = Just $ dir </> "cover.png_ps2"
    , gh2i_files            = filePairs
    , gh2i_sort             = sortBonus
    , gh2i_loading_phrase   = toBytes <$> gh2_LoadingPhrase gh2
    }

makeGH1DIY :: (MonadIO m) => TargetGH1 -> Project -> FilePath -> StackTraceT (QueueLog m) ()
makeGH1DIY gh1 proj dout = do
  dir <- buildGH1Dir gh1 proj
  stackIO $ Dir.createDirectoryIfMissing False dout
  files <- stackIO $ Dir.listDirectory dir
  sym <- stackIO $ fmap B8.unpack $ B.readFile $ dir </> "symbol"
  let filePairs = flip mapMaybe files $ \f -> let ext = takeExtension f in if
        | elem ext [".vgs", ".mid"]               -> Just (sym <> dropWhile isAlpha f, dir </> f)
        | ext == ".dta" && f /= "songs-inner.dta" -> Just (f, dir </> f)
        | otherwise                               -> Nothing
  stackIO $ forM_ filePairs $ \(dest, src) -> Dir.copyFile src $ dout </> dest
  let s = T.pack sym
  stackIO $ B.writeFile (dout </> "README.txt") $ encodeUtf8 $ T.intercalate "\r\n"
    [ "Instructions for GH1 song installation"
    , ""
    , "You must have a tool that can edit .ARK files such as arkhelper,"
    , "and a tool that can edit .dtb files such as dtab (which arkhelper can use automatically)."
    , ""
    , "1. Add the contents of songs.dta to: config/gen/songs.dtb"
    , "2. Make a new folder: songs/" <> s <> "/ and copy the .mid and .vgs files into it"
    , "3. Edit either config/gen/campaign.dtb or config/gen/store.dtb to add your song as a career or bonus song respectively"
    , "4. If added as a bonus song, edit ghui/eng/gen/locale.dtb with the key '" <> s <> "_shop_desc' if you want a description in the shop"
    -- TODO also mention loading tip in locale.dtb?
    ]

makeGH2DIY :: (MonadIO m) => TargetGH2 -> Project -> FilePath -> StackTraceT (QueueLog m) ()
makeGH2DIY gh2 proj dout = do
  dir <- buildGH2Dir gh2 proj
  stackIO $ Dir.createDirectoryIfMissing False dout
  files <- stackIO $ Dir.listDirectory dir
  sym <- stackIO $ fmap B8.unpack $ B.readFile $ dir </> "symbol"
  let filePairs = flip mapMaybe files $ \f -> let ext = takeExtension f in if
        | elem ext [".voc", ".vgs", ".mid"]       -> Just (sym <> dropWhile isAlpha f          , dir </> f)
        | ext == ".dta" && f /= "songs-inner.dta" && f /= "songs-inner-dx2.dta"
          -> Just (f, dir </> f)
        | ext == ".png_ps2"                       -> Just ("us_logo_" <> sym <> "_keep.png_ps2", dir </> f)
        | otherwise                               -> Nothing
  stackIO $ forM_ filePairs $ \(dest, src) -> Dir.copyFile src $ dout </> dest
  let s = T.pack sym
  stackIO $ B.writeFile (dout </> "README.txt") $ encodeUtf8 $ T.intercalate "\r\n"
    [ "Instructions for GH2 song installation"
    , ""
    , "You must have a tool that can edit .ARK files such as arkhelper,"
    , "and a tool that can edit .dtb files such as dtab (which arkhelper can use automatically)."
    , ""
    , "1. Add the contents of songs.dta to: config/gen/songs.dtb"
    , "  (For GH2 Deluxe 2.0, use songs-dx2.dta instead)"
    , "2. (possibly optional) Add the contents of coop_max_scores.dta to: config/gen/coop_max_scores.dtb"
    , "3. Make a new folder: songs/" <> s <> "/ and copy all .mid, .vgs, and .voc files into it"
    , "4. Edit either config/gen/campaign.dtb or config/gen/store.dtb to add your song as a career or bonus song respectively"
    , "5. If added as a bonus song, copy the .png_ps2 to ui/image/og/gen/us_logo_" <> s <> "_keep.png_ps2 if you want album art in the shop"
    , "6. If added as a bonus song, edit ui/eng/gen/locale.dtb with keys '" <> s <> "' and '" <> s <> "_shop_desc' if you want a title/description in the shop"
    -- TODO also mention loading tip in locale.dtb?
    ]

buildPlayer :: (MonadIO m) => Maybe T.Text -> Project -> StackTraceT (QueueLog m) FilePath
buildPlayer mplan proj = do
  planName <- case mplan of
    Just planName -> return planName
    Nothing       -> case Map.toList $ _plans $ projectSongYaml proj of
      [(planName, _)] -> return planName
      []              -> fatal "Project has no audio plans"
      _ : _ : _       -> fatal "Project has more than 1 audio plan"
  shakeBuild1 proj [] $ "gen/plan" </> T.unpack planName </> "web"

choosePlan :: (Monad m) => Maybe T.Text -> Project -> StackTraceT m T.Text
choosePlan (Just plan) _    = return plan
choosePlan Nothing     proj = case Map.keys $ _plans $ projectSongYaml proj of
  [p]   -> return p
  plans -> fatal $ "No plan selected, and the project doesn't have exactly 1 plan: " <> show plans

proKeysHanging :: (MonadIO m) => Maybe T.Text -> Project -> StackTraceT (QueueLog m) ()
proKeysHanging mplan proj = do
  plan <- choosePlan mplan proj
  void $ shakeBuild1 proj [] $ "gen/plan" </> T.unpack plan </> "hanging"
