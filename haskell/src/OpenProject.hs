{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenProject where

import           Build
import           Config
import qualified Control.Monad.Catch            as MC
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import           Data.Aeson                     ((.:))
import qualified Data.Aeson.Types               as A
import qualified Data.ByteString.Lazy           as BL
import           Data.Functor                   (void)
import           Data.Hashable
import qualified Data.HashMap.Strict            as Map
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import qualified Data.Yaml                      as Y
import           Import
import qualified Sound.Jammit.Base              as J
import qualified System.Directory               as Dir
import           System.FilePath                (takeDirectory, takeExtension,
                                                 takeFileName, (</>))
import qualified System.IO                      as IO
import qualified System.IO.Temp                 as Temp

data Project = Project
  { projectLocation :: FilePath
  , projectSongYaml :: SongYaml
  , projectRelease  :: Maybe ReleaseKey
  }

resourceTempDir :: (MonadResource m) => m (ReleaseKey, FilePath)
resourceTempDir = do
  tmp <- liftIO Temp.getCanonicalTemporaryDirectory
  let ignoringIOErrors ioe = ioe `MC.catch` (\e -> const (return ()) (e :: IOError))
  allocate
    (Temp.createTempDirectory tmp "onyx")
    (ignoringIOErrors . Dir.removeDirectoryRecursive)

openProject :: (SendMessage m, MonadResource m, MonadUnliftIO m) =>
  FilePath -> StackTraceT m Project
openProject fp = do
  let withYaml key fyml = loadYaml fyml >>= \yml -> return $ Project fyml yml key
      importFrom fn = do
        (key, tmp) <- resourceTempDir
        () <- fn tmp
        withYaml (Just key) (tmp </> "song.yml")
  isDir <- stackIO $ Dir.doesDirectoryExist fp
  if isDir
    then stackIO (Dir.doesFileExist $ fp </> "song.yml") >>= \case
      True  -> withYaml Nothing $ fp </> "song.yml"
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
            _      -> fatal "Unrecognized song format"

withProject :: (SendMessage m, MonadUnliftIO m) =>
  FilePath -> (Project -> StackTraceT m a) -> StackTraceT m a
withProject fp fn = mapStackTraceT runResourceT $ openProject fp >>= mapStackTraceT lift . fn

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

buildPSZip :: (MonadIO m) => TargetPS -> Project -> StackTraceT (QueueLog m) FilePath
buildPSZip ps = buildCommon (PS ps) $ \targetHash -> "gen/target" </> targetHash </> "ps.zip"

choosePlan :: (Monad m) => Maybe T.Text -> Project -> StackTraceT m T.Text
choosePlan (Just plan) _    = return plan
choosePlan Nothing     proj = case Map.keys $ _plans $ projectSongYaml proj of
  [p]   -> return p
  plans -> fatal $ "No plan selected, and the project doesn't have exactly 1 plan: " <> show plans

proKeysHanging :: (MonadIO m) => Maybe T.Text -> Project -> StackTraceT (QueueLog m) ()
proKeysHanging mplan proj = do
  plan <- choosePlan mplan proj
  void $ shakeBuild1 proj [] $ "gen/plan" </> T.unpack plan </> "hanging"
