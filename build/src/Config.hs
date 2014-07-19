{-# LANGUAGE TemplateHaskell #-}
module Config where

import Data.Aeson.Types
import Data.Aeson.TH

import Audio
import TH
import qualified Data.HashMap.Strict as Map
import Control.Applicative ((<|>))

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
  , _audio :: Map.HashMap String (AudioConfig Double)
  , _config :: [Instrument]
  } deriving (Eq, Show, Read)

data Instrument = Drums | Bass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Gender = Male | Female
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data AudioConfig t
  = AudioSimple (Audio t ())
  | AudioStems (Map.HashMap String (Audio t FilePath))
  deriving (Eq, Show, Read)

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

instance (Show t) => ToJSON (AudioConfig t) where
  toJSON (AudioSimple x) = toJSON x
  toJSON (AudioStems x) = toJSON x

instance (Read t) => FromJSON (AudioConfig t) where
  parseJSON v = fmap AudioSimple (parseJSON v) <|> fmap AudioStems (parseJSON v)
