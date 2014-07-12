{-# LANGUAGE TemplateHaskell #-}
module Config where

import Data.Aeson.TH
import Data.Char (isUpper, toLower)

import Audio

data Song = Song
  { _title :: String
  , _artist :: String
  , _album :: String
  , _genre :: String
  , _subgenre :: String
  , _vocalGender :: Gender
  , _fileAlbumArt :: FilePath
  , _trackNumber :: Int
  , _package :: String
  , _jammitTitle :: Maybe String
  , _jammitArtist :: Maybe String
  , _jammitAudio :: Maybe (Audio Double ())
  , _albumAudio :: Maybe (Audio Double ())
  , _config :: Config
  } deriving (Eq, Ord, Show, Read)

data Config
  = Drums
  | DrumsBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Gender = Male | Female
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

$(deriveJSON
  defaultOptions
    { fieldLabelModifier = let
      camelToHyphens = concatMap $ \c -> if isUpper c
        then ['-', toLower c]
        else [c]
      in camelToHyphens . drop 1
    , omitNothingFields = True
    }
  ''Song
  )

$(deriveJSON
  defaultOptions
    { allNullaryToStringTag = True
    , constructorTagModifier = let
      camelToHyphens = concatMap $ \c -> if isUpper c
        then ['-', toLower c]
        else [c]
      in drop 1 . camelToHyphens
    }
  ''Config
  )

$(deriveJSON
  defaultOptions
    { allNullaryToStringTag = True
    , constructorTagModifier = let
      camelToHyphens = concatMap $ \c -> if isUpper c
        then ['-', toLower c]
        else [c]
      in drop 1 . camelToHyphens
    }
  ''Gender
  )
