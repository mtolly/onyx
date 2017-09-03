{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.DTA.Serialize.GH2 where

import           Control.Monad.Codec    ((=.))
import           Data.DTA
import           Data.DTA.Serialize
import           Data.DTA.Serialize.RB3 (AnimTempo, channelList)
import           Data.Hashable          (Hashable (..))
import qualified Data.HashMap.Strict    as Map
import qualified Data.Text              as T
import           GHC.Generics           (Generic (..))
import           JSONData               (opt, req)

data Song = Song
  { songName :: T.Text
  , tracks   :: Map.HashMap T.Text [Integer]
  , pans     :: [Float]
  , vols     :: [Float]
  , cores    :: [Integer]
  , midiFile :: T.Text
  } deriving (Eq, Show, Read)

instance StackChunks Song where
  stackChunks = asStrictAssoc "Song" $ do
    songName <- songName =. req "name"      (single chunkStringOrKey)
    tracks   <- tracks   =. req "tracks"    (chunksParens $ chunksDict chunkKey channelList)
    pans     <- pans     =. req "pans"      (chunksParens stackChunks)
    vols     <- vols     =. req "vols"      (chunksParens stackChunks)
    cores    <- cores    =. req "cores"     (chunksParens stackChunks)
    midiFile <- midiFile =. req "midi_file" stackChunks
    return Song{..}

data CharacterOutfit
  = Funk1
  | Goth2
  | Classic
  | Punk1
  | Glam1
  | Alterna1
  | Metal1
  | Rock2
  | Deathmetal1
  | Rockabill1
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Hashable)

instance StackChunk CharacterOutfit where
  stackChunk = dtaEnum "CharacterOutfit" $ Key . T.toLower . T.pack . show
instance StackChunks CharacterOutfit

data Guitar
  = Flying_V
  | LesPaul
  | SG
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Hashable)

instance StackChunk Guitar where
  stackChunk = dtaEnum "Guitar" $ Key . T.toLower . T.pack . show
instance StackChunks Guitar

data Venue
  = Arena
  | Battle
  | Big
  | Fest
  | Small1
  | Small2
  | Theatre
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Hashable)

instance StackChunk Venue where
  stackChunk = dtaEnum "Venue" $ Key . T.toLower . T.pack . show
instance StackChunks Venue

data Quickplay = Quickplay
  { characterOutfit :: CharacterOutfit
  , guitar          :: Guitar
  , venue           :: Venue
  } deriving (Eq, Ord, Show, Read, Generic, Hashable)

instance StackChunks Quickplay where
  stackChunks = asStrictAssoc "Quickplay" $ do
    characterOutfit <- characterOutfit =. req "character_outfit" stackChunks
    guitar          <- guitar          =. req "guitar"           stackChunks
    venue           <- venue           =. req "venue"            stackChunks
    return Quickplay{..}

data BandMember
  = MetalBass
  | MetalDrummer
  | MetalKeyboard
  | FemaleSinger
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Hashable)

instance StackChunk BandMember where
  stackChunk = dtaEnum "BandMember" $ Key . \case
    MetalBass     -> "metal_bass"
    MetalDrummer  -> "metal_drummer"
    MetalKeyboard -> "metal_keyboard"
    FemaleSinger  -> "female_singer"
instance StackChunks BandMember

data SongPackage = SongPackage
  { name           :: T.Text
  , artist         :: T.Text
  , song           :: Song
  , animTempo      :: AnimTempo
  , preview        :: (Integer, Integer)
  , quickplay      :: Quickplay
  , practiceSpeeds :: [Integer]
  , songCoop       :: Maybe Song
  , songPractice1  :: Song
  , songPractice2  :: Song
  , songPractice3  :: Song
  , band           :: Maybe [BandMember]
  } deriving (Eq, Show, Read)

instance StackChunks SongPackage where
  stackChunks = asStrictAssoc "SongPackage" $ do
    name           <- name           =. req         "name"            (single chunkString)
    artist         <- artist         =. req         "artist"          (single chunkString)
    song           <- song           =. req         "song"            stackChunks
    animTempo      <- animTempo      =. req         "anim_tempo"      stackChunks
    preview        <- preview        =. req         "preview"         stackChunks
    quickplay      <- quickplay      =. req         "quickplay"       stackChunks
    practiceSpeeds <- practiceSpeeds =. req         "practice_speeds" (chunksParens stackChunks)
    songCoop       <- songCoop       =. opt Nothing "song_coop"       stackChunks
    songPractice1  <- songPractice1  =. req         "song_practice1"  stackChunks
    songPractice2  <- songPractice2  =. req         "song_practice2"  stackChunks
    songPractice3  <- songPractice3  =. req         "song_practice3"  stackChunks
    band           <- band           =. opt Nothing "band"            stackChunks
    return SongPackage{..}
