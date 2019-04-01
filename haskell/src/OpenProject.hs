{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenProject where

import           Build
import           Config
import           Control.Applicative            ((<|>))
import qualified Control.Monad.Catch            as MC
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import           Data.Aeson                     ((.:))
import qualified Data.Aeson.Types               as A
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toLower)
import           Data.Default.Class             (def)
import qualified Data.DTA.Serialize.RB3         as D
import           Data.Functor                   (void)
import           Data.Hashable
import qualified Data.HashMap.Strict            as Map
import           Data.List.Extra                (stripSuffix)
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import qualified Data.Yaml                      as Y
import qualified FeedBack.Load                  as FB
import qualified FretsOnFire                    as FoF
import           Import
import           JSONData                       (toJSON)
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
  , impProject :: StackTraceT m Project
  }

findSongs :: (SendMessage m, MonadResource m)
  => FilePath -> StackTraceT m ([FilePath], Maybe (Importable m))
findSongs fp' = do
  fp <- stackIO $ Dir.makeAbsolute fp'
  let found imp = return ([], Just imp)
      withYaml loc key fyml = loadYaml fyml >>= \yml -> return Project
        { projectLocation = fyml
        , projectSongYaml = yml
        , projectRelease = key
        , projectSource = loc
        , projectTemplate
          = dropExtension
          $ dropTrailingPathSeparator
          $ fromMaybe loc
          $ stripSuffix "_rb3con" loc <|> stripSuffix "_rb2con" loc
        }
      importFrom loc fn = do
        (key, tmp) <- resourceTempDir
        () <- fn tmp
        withYaml loc (Just key) (tmp </> "song.yml")
      foundChart loc = do
        let dir = takeDirectory loc
        ini <- fmap FB.chartToIni $ FB.loadChartFile loc
        found Importable
          { impTitle = FoF.name ini
          , impArtist = FoF.artist ini
          , impAuthor = FoF.charter ini
          , impFormat = "Clone Hero"
          , impPath = dir
          , impProject = importFrom dir $ void . importFoF True False dir
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
          , impProject = importFrom dir $ void . importFoF True False dir
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
          , impProject = withYaml dir Nothing loc
          }
      foundDTA bs fmt loc imp = do
        [(single, _)] <- readDTASingles bs
        found Importable
          { impTitle = Just $ D.name $ dtaSongPackage single
          , impArtist = Just $ D.artist $ dtaSongPackage single
          , impAuthor = c3dtaAuthoredBy $ dtaC3Comments single
          , impFormat = fmt
          , impPath = loc
          , impProject = importFrom loc imp
          }
      foundSTFS loc = do
        Just bs <- stackIO $ withSTFS loc $ \stfs ->
          sequence $ lookup ("songs" </> "songs.dta") $ stfsFiles stfs
        foundDTA (BL.toStrict bs) "Xbox 360 STFS (CON/LIVE)" loc $ void . importSTFS loc Nothing
  isDir <- stackIO $ Dir.doesDirectoryExist fp
  if isDir
    then do
      ents <- stackIO $ Dir.listDirectory fp
      if
        | elem "song.yml" ents -> foundYaml $ fp </> "song.yml"
        | elem "song.ini" ents -> foundIni $ fp </> "song.ini"
        | elem "notes.chart" ents -> foundChart $ fp </> "notes.chart"
        | [ent] <- filter (\ent -> takeExtension ent == ".rbproj") ents
          -> undefined ent
        | [ent] <- filter (\ent -> takeExtension ent == ".moggsong") ents
          -> undefined ent
        | otherwise -> stackIO (Dir.doesFileExist $ fp </> "songs/songs.dta") >>= \case
          True  -> do
            bs <- stackIO $ B.readFile $ fp </> "songs/songs.dta"
            foundDTA bs "Rock Band Extracted" fp $ void . importSTFSDir fp Nothing
          False -> return (map (fp </>) ents, Nothing)
    else do
      case map toLower $ takeExtension fp of
        ".yml" -> foundYaml fp
        ".yaml" -> foundYaml fp
        ".rbproj" -> undefined
        ".moggsong" -> undefined
        ".chart" -> foundChart fp
        _ -> case map toLower $ takeFileName fp of
          "song.ini" -> foundIni fp
          _ -> do
            magic <- stackIO $ IO.withBinaryFile fp IO.ReadMode $ \h -> BL.hGet h 4
            case magic of
              "RBSF" -> do
                bs <- getRBAFileBS 0 fp
                foundDTA (BL.toStrict bs) "Magma RBA" fp $ void . importRBA fp Nothing
              "CON " -> foundSTFS fp
              "LIVE" -> foundSTFS fp
              _ -> return ([], Nothing)

openProject :: (SendMessage m, MonadResource m) =>
  FilePath -> StackTraceT m Project
openProject fp = do
  isDir <- stackIO $ Dir.doesDirectoryExist fp
  absolute <- stackIO $ Dir.makeAbsolute fp
  let withYaml key fyml = loadYaml fyml >>= \yml -> return Project
        { projectLocation = fyml
        , projectSongYaml = yml
        , projectRelease = key
        , projectSource = absolute
        , projectTemplate = dropExtension $ if isDir
          then dropTrailingPathSeparator absolute
          else fromMaybe absolute $
            stripSuffix "_rb3con" absolute <|> stripSuffix "_rb2con" absolute
        }
      importFrom fn = do
        (key, tmp) <- resourceTempDir
        () <- fn tmp
        withYaml (Just key) (tmp </> "song.yml")
  if isDir
    then do
      ents <- stackIO $ Dir.listDirectory fp
      if
        | elem "song.yml" ents -> withYaml Nothing $ fp </> "song.yml"
        | elem "song.ini" ents -> importFrom $ void . importFoF True False fp
        | elem "notes.chart" ents -> importFrom $ void . importFoF True False fp
        | [ent] <- filter (\ent -> takeExtension ent == ".rbproj") ents
          -> importFrom $ void . importMagma (fp </> ent)
        | [ent] <- filter (\ent -> takeExtension ent == ".moggsong") ents
          -> importFrom $ importAmplitude (fp </> ent)
        | otherwise -> stackIO (Dir.doesFileExist $ fp </> "songs/songs.dta") >>= \case
          True  -> importFrom $ void . importSTFSDir fp Nothing
          False -> fatal "Unrecognized folder format"
    else case takeExtension fp of
      ".yml"      -> withYaml Nothing fp
      ".yaml"     -> withYaml Nothing fp
      ".rbproj"   -> importFrom $ void . importMagma fp
      ".moggsong" -> importFrom $ importAmplitude fp
      ".chart"    -> importFrom $ void . importFoF True False (takeDirectory fp)
      _           -> case takeFileName fp of
        "song.ini" -> importFrom $ void . importFoF True False (takeDirectory fp)
        _ -> do
          magic <- stackIO $ IO.withBinaryFile fp IO.ReadMode $ \h -> BL.hGet h 4
          case magic of
            "RBSF" -> importFrom $ void . importRBA fp Nothing
            "CON " -> importFrom $ void . importSTFS fp Nothing
            "LIVE" -> importFrom $ void . importSTFS fp Nothing
            "MThd" -> importFrom $ \dir -> do
              -- raw midi file, import as a "song"
              stackIO $ Dir.copyFile fp $ dir </> "notes.mid"
              stackIO $ Y.encodeFile (dir </> "song.yml") $ toJSON SongYaml
                { _metadata = def
                , _audio    = Map.empty
                , _jammit   = Map.empty
                , _plans    = Map.empty -- TODO empty plan
                , _targets  = Map.empty
                , _parts    = Parts Map.empty -- TODO identify midi tracks
                }
            _      -> fatal "Unrecognized song format"

withProject :: (SendMessage m, MonadResource m) =>
  FilePath -> (Project -> StackTraceT m a) -> StackTraceT m a
withProject fp fn = do
  proj <- openProject fp
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

shakeBuild1 :: (MonadIO m) => Project -> [(T.Text, Target)] -> FilePath -> StackTraceT (QueueLog m) FilePath
shakeBuild1 proj extraTargets buildable = do
  audioDirs <- getAudioDirs proj
  shakeBuild audioDirs (projectLocation proj) extraTargets [buildable]
  return $ takeDirectory (projectLocation proj) </> buildable

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

buildPlayer :: (MonadIO m) => Project -> StackTraceT (QueueLog m) FilePath
buildPlayer proj = case Map.toList $ _plans $ projectSongYaml proj of
  [(planName, _)] -> shakeBuild1 proj [] $ "gen/plan" </> T.unpack planName </> "web"
  []              -> fatal "Project has no audio plans"
  _ : _ : _       -> fatal "Project has more than 1 audio plan"

choosePlan :: (Monad m) => Maybe T.Text -> Project -> StackTraceT m T.Text
choosePlan (Just plan) _    = return plan
choosePlan Nothing     proj = case Map.keys $ _plans $ projectSongYaml proj of
  [p]   -> return p
  plans -> fatal $ "No plan selected, and the project doesn't have exactly 1 plan: " <> show plans

proKeysHanging :: (MonadIO m) => Maybe T.Text -> Project -> StackTraceT (QueueLog m) ()
proKeysHanging mplan proj = do
  plan <- choosePlan mplan proj
  void $ shakeBuild1 proj [] $ "gen/plan" </> T.unpack plan </> "hanging"
