{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Preferences where

import           Build                          (loadYaml)
import           Config                         (MagmaSetting (..))
import           Control.Monad.Codec            ((=.))
import           Control.Monad.Codec.Onyx
import           Control.Monad.Codec.Onyx.JSON
import           Control.Monad.IO.Class
import           Control.Monad.Trans.StackTrace
import           Data.Default.Class
import qualified System.Directory               as Dir

data Preferences = Preferences
  { prefMagma      :: MagmaSetting
  , prefBlackVenue :: Bool
  , prefMSAA       :: Maybe Int
  , prefFXAA       :: Bool
  , prefDirRB      :: Maybe FilePath
  , prefDirCH      :: Maybe FilePath
  , prefDirWii     :: Maybe FilePath
  , prefDirPreview :: Maybe FilePath
  , prefAudioDirs  :: [FilePath]
  }

instance StackJSON Preferences where
  stackJSON = asObject "Preferences" $ do
    prefMagma      <- prefMagma      =. fill MagmaRequire "magma"       stackJSON
    prefBlackVenue <- prefBlackVenue =. fill False        "black-venue" stackJSON
    prefMSAA       <- prefMSAA       =. fill (Just 4)     "msaa"        stackJSON
    prefFXAA       <- prefFXAA       =. fill True         "fxaa"        stackJSON
    prefDirRB      <- prefDirRB      =. opt  Nothing      "dir-rb"      stackJSON
    prefDirCH      <- prefDirCH      =. opt  Nothing      "dir-ch"      stackJSON
    prefDirWii     <- prefDirWii     =. opt  Nothing      "dir-wii"     stackJSON
    prefDirPreview <- prefDirPreview =. opt  Nothing      "dir-preview" stackJSON
    prefAudioDirs  <- prefAudioDirs  =. opt  []           "audio-dirs"  stackJSON
    return Preferences{..}

instance Default Preferences where
  def = fromEmptyObject

readPreferences :: (SendMessage m, MonadIO m) => StackTraceT m Preferences
readPreferences = do
  cfg <- stackIO $ Dir.getXdgDirectory Dir.XdgConfig "onyx.yml"
  stackIO (Dir.doesFileExist cfg) >>= \case
    False -> return def
    True  -> loadYaml cfg

savePreferences :: (MonadIO m) => Preferences -> m ()
savePreferences prefs = liftIO $ do
  cfg <- Dir.getXdgDirectory Dir.XdgConfig "onyx.yml"
  yamlEncodeFile cfg $ toJSON prefs
