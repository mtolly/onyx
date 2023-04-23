{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Preferences where

import           Control.Monad          (unless)
import           Control.Monad.Codec    ((=.))
import           Control.Monad.IO.Class
import qualified Data.Aeson             as A
import           Data.Default.Class
import           Data.Hashable          (Hashable (..))
import           GHC.Conc               (getNumProcessors, setNumCapabilities)
import           GHC.Generics           (Generic (..))
import           Onyx.Codec.Common
import           Onyx.Codec.JSON
import           Onyx.StackTrace
import qualified System.Directory       as Dir
import           System.FilePath        (takeDirectory)

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
  , prefArtistSort    :: Bool -- in gh2/gh3, sort by artist then title
  , prefThreads       :: Maybe Int
  , prefDetectMuted   :: Bool
  , prefLegalTempos   :: Bool -- for RB3/RB2 export, try to modfiy tempos to be in Magma-legal range
  , prefTrueLayout    :: [TrueDrumLayoutHint]
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
    prefArtistSort    <- prefArtistSort    =. opt  False        "artist-sort"     stackJSON
    prefThreads       <- prefThreads       =. opt  Nothing      "threads"         stackJSON
    prefDetectMuted   <- prefDetectMuted   =. opt  True         "detect-muted"    stackJSON
    prefLegalTempos   <- prefLegalTempos   =. opt  True         "legal-tempos"    stackJSON
    prefTrueLayout    <- prefTrueLayout    =. opt  []           "true-layout"     stackJSON
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
  let configFolder = takeDirectory cfg
  exists <- Dir.doesDirectoryExist configFolder
  unless exists $ do
    -- From getXdgDirectory docs, https://github.com/haskell/directory/blob/f6b288bd96/System/Directory.hs#L1209
    --   Note: The directory may not actually exist, in which case you would need
    --   to create it with file mode @700@ (i.e. only accessible by the owner).
    -- (I don't see an easy way to change the non-owner permissions without unix-only code + cpp hackery)
    Dir.createDirectoryIfMissing True configFolder
    perms <- Dir.getPermissions configFolder
    Dir.setPermissions configFolder
      $ Dir.setOwnerReadable   True
      $ Dir.setOwnerWritable   True
      $ Dir.setOwnerExecutable True
      $ Dir.setOwnerSearchable True perms
  yamlEncodeFile cfg $ toJSON prefs
  applyThreads prefs

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

applyThreads :: Preferences -> IO ()
applyThreads prefs = do
  procs <- getNumProcessors
  setNumCapabilities $ case prefThreads prefs of
    Nothing -> procs
    Just n  -> min n procs

data TrueDrumLayoutHint
  = TDLeftCrossHand  -- L to R: snare, hihat, left crash
  | TDLeftOpenHand   -- L to R: left crash, hihat, snare
  | TDRightFarCrash  -- L to R: ride, right crash
  | TDRightNearCrash -- L to R: right crash, ride
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON TrueDrumLayoutHint where
  stackJSON = enumCodec "a true drums layout hint" $ \case
    TDLeftCrossHand  -> "cross-hand"
    TDLeftOpenHand   -> "open-hand"
    TDRightFarCrash  -> "far-crash"
    TDRightNearCrash -> "near-crash"
