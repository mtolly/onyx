{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module AudioSearch where

import Config
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Conduit.Audio
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.StackTrace
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad.Trans.Resource
import Path
import Path.IO

newtype AudioLibrary = AudioLibrary (MVar AudioState)

data AudioState = AudioState
  { audioFiles :: HM.HashMap T.Text (Path Abs File) -- ^ keys are MD5 of audio content (i.e. FLAC fingerprint)
  , audioMOGGs :: HM.HashMap T.Text (Path Abs File) -- ^ keys are MD5 of whole file
  , audioJammit :: HM.HashMap (T.Text, T.Text, Instrument) (Path Abs Dir) -- ^ keys are (title, artist, instrument)
  , audioQueueDirs :: [Path Abs Dir]
  , audioQueueFiles :: [Path Abs File]
  , audioScannedDirs :: HS.HashSet (Path Abs Dir)
  , audioScannedFiles :: HS.HashSet (Path Abs File)
  }

newAudioLibrary :: (MonadIO m) => m AudioLibrary
newAudioLibrary = fmap AudioLibrary $ liftIO $ newMVar $ AudioState
  { audioFiles = HM.empty
  , audioMOGGs = HM.empty
  , audioJammit = HM.empty
  , audioQueueDirs = []
  , audioQueueFiles = []
  , audioScannedDirs = HS.empty
  , audioScannedFiles = HS.empty
  }

addAudioDir :: (MonadIO m) => AudioLibrary -> Path Abs Dir -> m ()
addAudioDir (AudioLibrary var) dir = liftIO $ modifyMVar_ var $ return . queueDir dir

queueDir :: Path Abs Dir -> AudioState -> AudioState
queueDir dir ast = if HS.member dir $ audioScannedDirs ast
  then ast
  else ast { audioQueueDirs = dir : audioQueueDirs ast }

queueFile :: Path Abs File -> AudioState -> AudioState
queueFile f ast = if HS.member f $ audioScannedFiles ast
  then ast
  else ast { audioQueueFiles = f : audioQueueFiles ast }

addDirectoryContents :: (MonadIO m) => AudioState -> Path Abs Dir -> StackTraceT m AudioState
addDirectoryContents ast dir = do
  (dirs, files) <- stackIO $ listDir dir
  return $ foldr ($) ast $ map queueDir dirs ++ map queueFile files

verifyFile :: AudioInfo -> Path Abs File -> StackTraceT m ()
verifyFile AudioInfo{..} f = undefined

audioSearch :: (MonadIO m, MonadResource r) =>
  AudioLibrary -> AudioInfo -> StackTraceT m (AudioSource r Float)
audioSearch = undefined
