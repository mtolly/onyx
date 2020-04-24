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
import           JSONData               (enumCodec, opt, req)
import           RockBand.Codec         (reprPrefix)

data Song = Song
  { songName      :: T.Text
  , tracks        :: Map.HashMap T.Text [Integer]
  , pans          :: [Float]
  , vols          :: [Float]
  , cores         :: [Integer]
  , midiFile      :: T.Text
  , hopoThreshold :: Maybe Integer
  } deriving (Eq, Show)

instance StackChunks Song where
  stackChunks = asWarnAssoc "Song" $ do
    songName      <- songName      =. req         "name"           (single chunkString)
    tracks        <- tracks        =. req         "tracks"         (chunksParens $ chunksDict chunkKey channelList)
    pans          <- pans          =. req         "pans"           (chunksParens stackChunks)
    vols          <- vols          =. req         "vols"           (chunksParens stackChunks)
    cores         <- cores         =. req         "cores"          (chunksParens stackChunks)
    midiFile      <- midiFile      =. req         "midi_file"      stackChunks
    hopoThreshold <- hopoThreshold =. opt Nothing "hopo_threshold" stackChunks
    return Song{..}

data CharacterOutfit
  = Char_funk1
  | Char_goth2
  | Char_classic
  | Char_punk1
  | Char_glam1
  | Char_alterna1
  | Char_metal1
  | Char_rock2
  | Char_deathmetal1
  | Char_rockabill1
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable)

instance StackChunk CharacterOutfit where
  stackChunk = enumCodec "CharacterOutfit" $ Key . reprPrefix "Char_"
instance StackChunks CharacterOutfit

data Guitar
  = Guitar_flying_v
  | Guitar_lespaul
  | Guitar_sg
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable)

instance StackChunk Guitar where
  stackChunk = enumCodec "Guitar" $ Key . reprPrefix "Guitar_"
instance StackChunks Guitar

data Venue
  = Venue_arena
  | Venue_battle
  | Venue_big
  | Venue_fest
  | Venue_small1
  | Venue_small2
  | Venue_theatre
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable)

instance StackChunk Venue where
  stackChunk = enumCodec "Venue" $ Key . reprPrefix "Venue_"
instance StackChunks Venue

data Quickplay = Quickplay
  { characterOutfit :: Either CharacterOutfit T.Text
  , guitar          :: Either Guitar T.Text
  , venue           :: Either Venue T.Text
  } deriving (Eq, Ord, Show, Generic, Hashable)

instance StackChunks Quickplay where
  stackChunks = asWarnAssoc "Quickplay" $ do
    characterOutfit <- characterOutfit =. req "character_outfit" stackChunks
    guitar          <- guitar          =. req "guitar"           stackChunks
    venue           <- venue           =. req "venue"            stackChunks
    return Quickplay{..}

data BandMember
  = MetalBass
  | MetalDrummer
  | MetalKeyboard
  | FemaleSinger
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable)

instance StackChunk BandMember where
  stackChunk = enumCodec "BandMember" $ Key . \case
    MetalBass     -> "metal_bass"
    MetalDrummer  -> "metal_drummer"
    MetalKeyboard -> "metal_keyboard"
    FemaleSinger  -> "female_singer"
instance StackChunks BandMember

data SongPackage = SongPackage
  { name           :: T.Text
  , artist         :: T.Text
  , caption        :: Maybe T.Text
  , song           :: Song
  , animTempo      :: AnimTempo
  , preview        :: (Integer, Integer)
  , quickplay      :: Quickplay
  , practiceSpeeds :: Maybe [Integer]
  , songCoop       :: Maybe Song
  , songPractice1  :: Maybe Song
  , songPractice2  :: Maybe Song
  , songPractice3  :: Maybe Song
  , band           :: Maybe [Either BandMember T.Text]
  } deriving (Eq, Show)

instance StackChunks SongPackage where
  stackChunks = asWarnAssoc "SongPackage" $ do
    name           <- name           =. req         "name"            (single chunkString)
    artist         <- artist         =. req         "artist"          (single chunkString)
    caption        <- caption        =. opt Nothing "caption"         (chunksMaybe $ single chunkKey)
    song           <- song           =. req         "song"            stackChunks
    animTempo      <- animTempo      =. req         "anim_tempo"      stackChunks
    preview        <- preview        =. req         "preview"         stackChunks
    quickplay      <- quickplay      =. req         "quickplay"       stackChunks
    practiceSpeeds <- practiceSpeeds =. opt Nothing "practice_speeds" (chunksParens stackChunks)
    songCoop       <- songCoop       =. opt Nothing "song_coop"       stackChunks
    songPractice1  <- songPractice1  =. opt Nothing "song_practice_1" stackChunks
    songPractice2  <- songPractice2  =. opt Nothing "song_practice_2" stackChunks
    songPractice3  <- songPractice3  =. opt Nothing "song_practice_3" stackChunks
    band           <- band           =. opt Nothing "band"            stackChunks
    return SongPackage{..}
