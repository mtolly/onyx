{-# LANGUAGE TemplateHaskell #-}
module Config where

import qualified Data.Aeson as A
import Data.Aeson.TH
import Text.Read (readEither)
import Data.Char (isUpper, toLower)

import Audio

data Config = Config
  { _title :: String
  , _artist :: String
  , _album :: String
  , _albumArt :: FilePath
  , _trackNumber :: Int
  , _package :: String
  , _jammitTitle :: Maybe String
  , _jammitArtist :: Maybe String
  , _jammitAudio :: Maybe (Audio Double ())
  , _albumAudio :: Maybe (Audio Double ())
  } deriving (Eq, Ord, Show, Read)

sampleConfig :: Config
sampleConfig = Config
  { _title = "My Song"
  , _artist = "Some Artist"
  , _album = "An Album"
  , _albumArt = "../../cover/album.png"
  , _trackNumber = 3
  , _package = "mysong"
  , _jammitTitle = Just $ "Jammit Song"
  , _jammitArtist = Nothing
  , _jammitAudio = Nothing
  , _albumAudio = Just $ Silence 2 8
  }

sampleValue :: A.Value
sampleValue = A.toJSON sampleConfig

instance (Read t, Read a) => A.FromJSON (Audio t a) where
  parseJSON v = A.parseJSON v >>= either fail return . readEither

instance (Show t, Show a) => A.ToJSON (Audio t a) where
  toJSON = A.toJSON . show

$(deriveJSON
  defaultOptions
    { fieldLabelModifier = let
      camelToHyphens = concatMap $ \c -> if isUpper c
        then ['-', toLower c]
        else [c]
      in camelToHyphens. drop 1
    , omitNothingFields = True
    }
  ''Config
  )