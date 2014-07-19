{-# LANGUAGE TemplateHaskell #-}
module Config where

import Data.Aeson.TH

import Audio
import TH

data Song = Song
  { _title :: String
  , _artist :: String
  , _album :: String
  , _genre :: String
  , _subgenre :: String
  , _year :: Int
  , _vocalGender :: Gender
  , _fileAlbumArt :: FilePath
  , _trackNumber :: Int
  , _package :: String
  , _jammitTitle :: Maybe String
  , _jammitArtist :: Maybe String
  , _jammitAudio :: Maybe (Audio Double ())
  , _albumAudio :: Maybe (Audio Double ())
  , _config :: [Instrument]
  } deriving (Eq, Ord, Show, Read)

data Instrument = Drums | Bass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Gender = Male | Female
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

$(deriveJSON
  defaultOptions
    { fieldLabelModifier = camelToHyphens . drop 1
    , omitNothingFields = True
    }
  ''Song
  )

$(deriveJSON
  defaultOptions
    { allNullaryToStringTag = True
    , constructorTagModifier = drop 1 . camelToHyphens
    }
  ''Instrument
  )

$(deriveJSON
  defaultOptions
    { allNullaryToStringTag = True
    , constructorTagModifier = drop 1 . camelToHyphens
    }
  ''Gender
  )
