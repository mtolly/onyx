{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenProject where

import           Beatmania.BMS                  (BMS (..), readBMSLines)
import           Beatmania.Import               (importBMS)
import           Build
import           Config
import           Control.Applicative            ((<|>))
import           Control.Monad                  (guard)
import qualified Control.Monad.Catch            as MC
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import           Data.Aeson                     ((.:))
import qualified Data.Aeson.Types               as A
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (isAlpha, toLower)
import           Data.DTA                       (DTA (..), Tree (..),
                                                 readFileDTA)
import qualified Data.DTA.Serialize             as DTA
import qualified Data.DTA.Serialize.Amplitude   as Amp
import qualified Data.DTA.Serialize.GH1         as GH1
import qualified Data.DTA.Serialize.GH2         as GH2
import qualified Data.DTA.Serialize.Magma       as RBProj
import qualified Data.DTA.Serialize.RB3         as D
import           Data.Foldable                  (toList)
import           Data.Functor                   (void)
import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.HashMap.Strict            as Map
import           Data.List.Extra                (stripSuffix)
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import qualified Data.Yaml                      as Y
import           DTXMania.DTX
import           DTXMania.Import
import           DTXMania.Set
import qualified FeedBack.Load                  as FB
import qualified FretsOnFire                    as FoF
import qualified GuitarHeroI.Import             as GH1
import           GuitarHeroII.Ark               (GameGH (..), detectGameGH,
                                                 replaceSong)
import qualified GuitarHeroII.Import            as GH2
import           Import
import           Magma                          (getRBAFileBS)
import           PrettyDTA                      (C3DTAComments (..),
                                                 DTASingle (..), readDTASingles)
import qualified Sound.Jammit.Base              as J
import           STFS.Package                   (STFSContents (..), withSTFS)
import qualified System.Directory               as Dir
import           System.FilePath                (dropExtension,
                                                 dropTrailingPathSeparator,
                                                 takeDirectory, takeExtension,
                                                 takeFileName, (</>))
import qualified System.IO                      as IO
import qualified System.IO.Temp                 as Temp

data Project = Project
  { projectLocation :: FilePath -- path to song.yml
  , projectSongYaml :: SongYaml
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
  }

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
      foundChart loc = do
        let dir = takeDirectory loc
        ini <- fmap FB.chartToIni $ FB.loadChartFile loc
        found Importable
          { impTitle = FoF.name ini
          , impArtist = FoF.artist ini
          , impAuthor = FoF.charter ini
          , impFormat = "Clone Hero"
          , impPath = dir
          , impIndex = Nothing
          , impProject = importFrom Nothing dir True $ void . importFoF dir
          }
      foundIni loc = do
        let dir = takeDirectory loc
        ini <- FoF.loadSong loc
        found Importable
          { impTitle = FoF.name ini
          , impArtist = FoF.artist ini
          , impAuthor = FoF.charter ini
          , impFormat = "Frets on Fire/Phase Shift/Clone Hero"
          , impPath = dir
          , impIndex = Nothing
          , impProject = importFrom Nothing dir True $ void . importFoF dir
          }
      foundGH loc = do
        let dir = takeDirectory loc
        stackIO (detectGameGH dir) >>= \case
          Nothing -> do
            warn $ "Couldn't detect GH game version for: " <> dir
            return ([], [])
          Just GameGH2 -> do
            songs <- GH2.getImports <$> GH2.getSongList dir
            let eachSong i (_, (mode, pkg)) = let
                  index = Just i
                  in Importable
                    { impTitle = Just $ GH2.name pkg <> case mode of
                      GH2.ImportSolo -> ""
                      GH2.ImportCoop -> " (Co-op)"
                    , impArtist = Just $ GH2.artist pkg
                    , impAuthor = Nothing
                    , impFormat = "Guitar Hero II"
                    , impPath = dir
                    , impIndex = index
                    , impProject = importFrom index dir True $ GH2.importGH2 mode pkg dir
                    }
            foundMany $ zipWith eachSong [0..] songs
          Just GameGH1 -> do
            songs <- GH1.getSongList dir
            let eachSong i (_, pkg) = let
                  index = Just i
                  in Importable
                    { impTitle = Just $ GH1.name pkg
                    , impArtist = Just $ GH1.artist pkg
                    , impAuthor = Nothing
                    , impFormat = "Guitar Hero (1)"
                    , impPath = dir
                    , impIndex = index
                    , impProject = importFrom index dir True $ GH1.importGH1 pkg dir
                    }
            foundMany $ zipWith eachSong [0..] songs
      foundDTXSet loc = do
        let dir = takeDirectory loc
        songs <- stackIO $ loadSet loc
        -- TODO get the artist from the top found difficulty
        let eachSong i song = let
              index = guard (not $ null $ drop 1 songs) >> Just i
              in Importable
                { impTitle = Just $ setTitle song
                , impArtist = Nothing
                , impAuthor = Nothing
                , impFormat = "DTXMania (set.def)"
                , impPath = dir
                , impIndex = index
                , impProject = importFrom index dir True $ importSet i loc
                }
        foundMany $ zipWith eachSong [0..] songs
      foundDTX fmt loc = do
        dtx <- stackIO $ readDTXLines fmt <$> loadDTXLines loc
        found Importable
          { impTitle = dtx_TITLE dtx
          , impArtist = dtx_ARTIST dtx
          , impAuthor = Nothing
          , impFormat = "DTXMania"
          , impPath = loc
          , impIndex = Nothing
          , impProject = importFrom Nothing loc False $ importDTX loc
          }
      foundBME loc = do
        bms <- stackIO $ readBMSLines <$> loadDTXLines loc
        found Importable
          { impTitle = bms_TITLE bms
          , impArtist = bms_ARTIST bms
          , impAuthor = Nothing
          , impFormat = "Be-Music Source"
          , impPath = loc
          , impIndex = Nothing
          , impProject = importFrom Nothing loc False $ importBMS loc
          }
      foundYaml loc = do
        let dir = takeDirectory loc
        yml <- loadYaml loc
        found Importable
          { impTitle = _title $ _metadata yml
          , impArtist = _artist $ _metadata yml
          , impAuthor = _author $ _metadata yml
          , impFormat = "Onyx"
          , impPath = dir
          , impIndex = Nothing
          , impProject = withYaml Nothing dir True Nothing loc
          }
      foundRBProj loc = do
        RBProj.RBProj proj <- stackIO (readFileDTA loc) >>= DTA.unserialize DTA.stackChunks
        found Importable
          { impTitle = Just $ RBProj.songName $ RBProj.metadata proj
          , impArtist = Just $ RBProj.artistName $ RBProj.metadata proj
          , impAuthor = Just $ RBProj.author $ RBProj.metadata proj
          , impFormat = "Magma Project"
          , impPath = loc
          , impIndex = Nothing
          , impProject = importFrom Nothing loc False $ void . importMagma loc
          }
      foundAmplitude loc = do
        let dir = takeDirectory loc
        song <- stackIO (readFileDTA loc) >>= DTA.unserialize DTA.stackChunks
        found Importable
          { impTitle = Just $ Amp.title song
          , impArtist = Just $ Amp.artist song
          , impAuthor = Nothing
          , impFormat = "Amplitude (2016)"
          , impPath = dir
          , impIndex = Nothing
          , impProject = importFrom Nothing dir True $ importAmplitude loc
          }
      foundDTA bs fmt loc isDir imp = do
        singles <- map fst <$> readDTASingles bs
        let eachSong i single = let
              index = guard (not $ null $ drop 1 singles) >> Just i
              in Importable
                { impTitle = Just $ D.name $ dtaSongPackage single
                , impArtist = Just $ D.artist $ dtaSongPackage single
                , impAuthor = c3dtaAuthoredBy $ dtaC3Comments single
                , impFormat = fmt
                , impPath = loc
                , impIndex = index
                , impProject = importFrom index loc isDir $ imp i
                }
        foundMany $ zipWith eachSong [0..] singles
      foundSTFS loc = do
        dta <- stackIO $ withSTFS loc $ \stfs ->
          sequence $ lookup ("songs" </> "songs.dta") $ stfsFiles stfs
        case dta of
          Just bs -> foundDTA (BL.toStrict bs) "Xbox 360 STFS (CON/LIVE)" loc False $ \i -> void . importSTFS i loc Nothing
          Nothing -> return ([], [])
  isDir <- stackIO $ Dir.doesDirectoryExist fp
  if isDir
    then do
      ents <- stackIO $ Dir.listDirectory fp
      let lookFor [] = stackIO (Dir.doesFileExist $ fp </> "songs/songs.dta") >>= \case
            True  -> do
              bs <- stackIO $ B.readFile $ fp </> "songs/songs.dta"
              foundDTA bs "Rock Band Extracted" fp True $ \i -> void . importSTFSDir i fp Nothing
            False -> return (map (fp </>) ents, [])
          lookFor ((file, use) : rest) = case filter ((== file) . map toLower) ents of
            match : _ -> use $ fp </> match
            []        -> lookFor rest
      lookFor
        [ ("song.yml", foundYaml)
        , ("song.ini", foundIni)
        , ("notes.chart", foundChart)
        , ("set.def", foundDTXSet)
        , ("main.hdr", foundGH)
        ]
    else do
      case map toLower $ takeExtension fp of
        ".yml" -> foundYaml fp
        ".yaml" -> foundYaml fp
        ".rbproj" -> foundRBProj fp
        ".moggsong" -> foundAmplitude fp
        ".chart" -> foundChart fp
        ".dtx" -> foundDTX FormatDTX fp
        ".gda" -> foundDTX FormatGDA fp
        ".bms" -> foundBME fp
        ".bme" -> foundBME fp
        ".bml" -> foundBME fp
        _ -> case map toLower $ takeFileName fp of
          "song.ini" -> foundIni fp
          "set.def" -> foundDTXSet fp
          "main.hdr" -> foundGH fp
          _ -> do
            magic <- stackIO $ IO.withBinaryFile fp IO.ReadMode $ \h -> BL.hGet h 4
            case magic of
              "RBSF" -> do
                bs <- getRBAFileBS 0 fp
                foundDTA (BL.toStrict bs) "Magma RBA" fp False $ \_ -> void . importRBA fp Nothing
              "CON " -> foundSTFS fp
              "LIVE" -> foundSTFS fp
              _ -> return ([], [])

openProject :: (SendMessage m, MonadResource m) =>
  Maybe Int -> FilePath -> StackTraceT m Project
openProject i fp = do
  (_, imps) <- findSongs fp
  case i of
    Nothing -> case imps of
      [imp] -> impProject imp
      []    -> fatal $ "Couldn't find a song at location: " <> fp
      _     -> fatal $ "Found more than 1 song at location: " <> fp
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

readConfig :: (MonadIO m) => StackTraceT m (Map.HashMap T.Text Y.Value)
readConfig = do
  cfg <- stackIO $ Dir.getXdgDirectory Dir.XdgConfig "onyx.yml"
  stackIO (Dir.doesFileExist cfg) >>= \case
    False -> return Map.empty
    True  -> stackIO (Y.decodeFileEither cfg) >>= \case
      Left err -> fatal $ show err
      Right x  -> return x

getAudioDirs :: (MonadIO m) => Project -> StackTraceT m [FilePath]
getAudioDirs proj = do
  config <- readConfig
  jmt <- stackIO J.findJammitDir
  let addons = maybe id (:) jmt [takeDirectory $ projectLocation proj]
  dirs <- case A.parseEither (.: "audio-dirs") config of
    Left _    -> return []
    Right obj -> either fatal return $ A.parseEither A.parseJSON obj
  mapM (stackIO . Dir.canonicalizePath) $ addons ++ dirs

shakeBuild1 :: (MonadIO m) =>
  Project -> [(T.Text, Target)] -> FilePath -> StackTraceT (QueueLog m) FilePath
shakeBuild1 proj extraTargets = fmap runIdentity . shakeBuildMany proj extraTargets . Identity

shakeBuildMany :: (MonadIO m, Functor f, Foldable f) =>
  Project -> [(T.Text, Target)] -> f FilePath -> StackTraceT (QueueLog m) (f FilePath)
shakeBuildMany proj extraTargets buildables = do
  audioDirs <- getAudioDirs proj
  shakeBuild audioDirs (projectLocation proj) extraTargets $ toList buildables
  return $ fmap (takeDirectory (projectLocation proj) </>) buildables

buildCommon :: (MonadIO m) => Target -> (String -> FilePath) -> Project -> StackTraceT (QueueLog m) FilePath
buildCommon target getBuildable proj = do
  let targetHash = show $ hash target `mod` 100000000
      buildable = getBuildable targetHash
  shakeBuild1 proj [(T.pack targetHash, target)] buildable

buildRB3CON :: (MonadIO m) => TargetRB3 -> Project -> StackTraceT (QueueLog m) FilePath
buildRB3CON rb3 = buildCommon (RB3 rb3) $ \targetHash -> "gen/target" </> targetHash </> "rb3con"

buildRB2CON :: (MonadIO m) => TargetRB2 -> Project -> StackTraceT (QueueLog m) FilePath
buildRB2CON rb2 = buildCommon (RB2 rb2) $ \targetHash -> "gen/target" </> targetHash </> "rb2con"

buildMagmaV2 :: (MonadIO m) => TargetRB3 -> Project -> StackTraceT (QueueLog m) FilePath
buildMagmaV2 rb3 = buildCommon (RB3 rb3) $ \targetHash -> "gen/target" </> targetHash </> "magma"

buildPSDir :: (MonadIO m) => TargetPS -> Project -> StackTraceT (QueueLog m) FilePath
buildPSDir ps = buildCommon (PS ps) $ \targetHash -> "gen/target" </> targetHash </> "ps"

buildPSZip :: (MonadIO m) => TargetPS -> Project -> StackTraceT (QueueLog m) FilePath
buildPSZip ps = buildCommon (PS ps) $ \targetHash -> "gen/target" </> targetHash </> "ps.zip"

buildGH2Dir :: (MonadIO m) => TargetGH2 -> Project -> StackTraceT (QueueLog m) FilePath
buildGH2Dir gh2 = buildCommon (GH2 gh2) $ \targetHash -> "gen/target" </> targetHash </> "gh2"

installGH2 :: (MonadIO m) => TargetGH2 -> Project -> B.ByteString -> FilePath -> StackTraceT (QueueLog m) ()
installGH2 gh2 proj song gen = do
  dir <- buildGH2Dir gh2 proj
  files <- stackIO $ Dir.listDirectory dir
  dta <- stackIO $ readFileDTA $ dir </> "songs.dta"
  let chunks = treeChunks $ topTree $ fmap (B8.pack . T.unpack) dta
      filePairs = flip mapMaybe files $ \f -> do
        guard $ f /= "songs.dta"
        return (song <> B8.pack (dropWhile isAlpha f), dir </> f)
  stackIO $ replaceSong gen song chunks filePairs

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
