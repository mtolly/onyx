{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Preferences where

import           Control.Monad.Codec            ((=.))
import           Control.Monad.Codec.Onyx
import           Control.Monad.Codec.Onyx.JSON
import           Control.Monad.IO.Class
import           Control.Monad.Trans.StackTrace
import qualified Data.Aeson                     as A
import           Data.Default.Class
import           Data.Hashable                  (Hashable (..))
import           GHC.Generics                   (Generic (..))
import qualified System.Directory               as Dir

data Preferences = Preferences
  { prefMagma         :: MagmaSetting
  , prefBlackVenue    :: Bool
  , prefLabel2x       :: Bool
  , prefTrimXbox      :: Bool
  , prefRBNumberID    :: Bool
  , prefSortGH2       :: Bool
  , prefPS3Encrypt    :: Bool
  , prefMSAA          :: Maybe Int
  , prefFXAA          :: Bool
  , prefDirRB         :: Maybe FilePath
  , prefDirCH         :: Maybe FilePath
  , prefDirWii        :: Maybe FilePath
  , prefDirPreview    :: Maybe FilePath
  , prefAudioDirs     :: [FilePath]
  , prefOGGQuality    :: Double
  , prefGH2Offset     :: Double -- in seconds
  , prefWarnedXboxGH2 :: Bool
  , prefWarnedXboxWoR :: Bool
  , prefGH4Lane       :: Bool
  , prefDecryptSilent :: Bool
  }

instance StackJSON Preferences where
  stackJSON = asObject "Preferences" $ do
    prefMagma         <- prefMagma         =. fill MagmaRequire "magma"           stackJSON
    prefBlackVenue    <- prefBlackVenue    =. fill False        "black-venue"     stackJSON
    prefLabel2x       <- prefLabel2x       =. fill True         "label-2x"        stackJSON
    prefTrimXbox      <- prefTrimXbox      =. fill False        "trim-xbox"       stackJSON
    prefSortGH2       <- prefSortGH2       =. fill True         "sort-gh2"        stackJSON
    prefPS3Encrypt    <- prefPS3Encrypt    =. fill True         "ps3-encrypt"     stackJSON
    prefRBNumberID    <- prefRBNumberID    =. fill False        "rb-number-id"    stackJSON
    prefMSAA          <- prefMSAA          =. fill (Just 4)     "msaa"            stackJSON
    prefFXAA          <- prefFXAA          =. fill True         "fxaa"            stackJSON
    prefDirRB         <- prefDirRB         =. opt  Nothing      "dir-rb"          stackJSON
    prefDirCH         <- prefDirCH         =. opt  Nothing      "dir-ch"          stackJSON
    prefDirWii        <- prefDirWii        =. opt  Nothing      "dir-wii"         stackJSON
    prefDirPreview    <- prefDirPreview    =. opt  Nothing      "dir-preview"     stackJSON
    prefAudioDirs     <- prefAudioDirs     =. opt  []           "audio-dirs"      stackJSON
    prefOGGQuality    <- prefOGGQuality    =. fill 0.5          "ogg-quality"     stackJSON
    prefGH2Offset     <- prefGH2Offset     =. fill 0            "gh2-offset"      stackJSON
    prefWarnedXboxGH2 <- prefWarnedXboxGH2 =. opt  False        "warned-xbox"     stackJSON
    prefWarnedXboxWoR <- prefWarnedXboxWoR =. opt  False        "warned-xbox-wor" stackJSON
    prefGH4Lane       <- prefGH4Lane       =. opt  False        "gh-4-lane"       stackJSON
    prefDecryptSilent <- prefDecryptSilent =. opt  False        "decrypt-silent"  stackJSON
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

data MagmaSetting
  = MagmaRequire
  | MagmaTry
  | MagmaDisable
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable)

instance StackJSON MagmaSetting where
  stackJSON = enumCodecFull "a Magma setting from {require, try, disable}" $ \case
    MagmaRequire -> is "require" |?> is (A.Bool True)
    MagmaTry     -> is "try"
    MagmaDisable -> is "disable" |?> is (A.Bool False)
