module OpenProject where

import           Build
import           Config
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.StackTrace
import           Data.Hashable
import qualified Data.Text                      as T
import           System.Directory               (doesDirectoryExist)
import           System.FilePath                (takeDirectory, (</>))

data Project = Project
  { projectLocation :: FilePath
  , projectSongYaml :: SongYaml
  } deriving (Eq, Show)

withProject :: (SendMessage m, MonadIO m) =>
  FilePath -> (Project -> StackTraceT m a) -> StackTraceT m a
withProject fp fn = do
  isDir <- stackIO $ doesDirectoryExist fp
  let fyml = if isDir
        then fp </> "song.yml"
        else fp
  loadYaml fyml >>= fn . Project fyml

buildCommon :: (MonadIO m) => Target -> (String -> FilePath) -> Project -> StackTraceT (QueueLog m) FilePath
buildCommon target getBuildable proj = do
  let targetHash = show $ hash target `mod` 100000000
      buildable = getBuildable targetHash
      yamlPath = projectLocation proj
      yamlDir = takeDirectory yamlPath
  shakeBuild [yamlDir] yamlPath [(T.pack targetHash, target)] [buildable]
  return $ yamlDir </> buildable

buildRB3CON :: (MonadIO m) => TargetRB3 -> Project -> StackTraceT (QueueLog m) FilePath
buildRB3CON rb3 = buildCommon (RB3 rb3) $ \targetHash -> "gen/target" </> targetHash </> "rb3con"

buildRB2CON :: (MonadIO m) => TargetRB2 -> Project -> StackTraceT (QueueLog m) FilePath
buildRB2CON rb2 = buildCommon (RB2 rb2) $ \targetHash -> "gen/target" </> targetHash </> "rb2con"

buildMagmaV2 :: (MonadIO m) => TargetRB3 -> Project -> StackTraceT (QueueLog m) FilePath
buildMagmaV2 rb3 = buildCommon (RB3 rb3) $ \targetHash -> "gen/target" </> targetHash </> "magma"

buildPSZip :: (MonadIO m) => TargetPS -> Project -> StackTraceT (QueueLog m) FilePath
buildPSZip ps = buildCommon (PS ps) $ \targetHash -> "gen/target" </> targetHash </> "ps.zip"
