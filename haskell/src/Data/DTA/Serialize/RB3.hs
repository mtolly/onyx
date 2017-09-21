{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.DTA.Serialize.RB3 where

import           Control.Applicative      ((<|>))
import           Control.Monad.Codec      ((=.))
import           Data.DTA
import           Data.DTA.Serialize
import           Data.DTA.Serialize.Magma (Gender (..))
import qualified Data.HashMap.Strict      as Map
import qualified Data.Text                as T
import           JSONData                 (StackCodec (..), eitherCodec,
                                           expected, fill, opt, req)

data Pitch
  = C | CSharp
  | D | DSharp
  | E
  | F | FSharp
  | G | GSharp
  | A | ASharp
  | B
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackChunk Pitch where
  stackChunk = dtaEnum "Pitch" $ Int . fromIntegral . fromEnum
instance StackChunks Pitch

data Tonality = Major | Minor
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackChunk Tonality where
  stackChunk = dtaEnum "Tonality" $ Int . fromIntegral . fromEnum
instance StackChunks Tonality

data AnimTempo = KTempoSlow | KTempoMedium | KTempoFast
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackChunk AnimTempo where
  stackChunk = dtaEnum "AnimTempo" $ \case
    KTempoSlow   -> Key "kTempoSlow"
    KTempoMedium -> Key "kTempoMedium"
    KTempoFast   -> Key "kTempoFast"
instance StackChunks AnimTempo

newtype DrumSounds = DrumSounds
  { seqs :: [T.Text]
  } deriving (Eq, Ord, Show, Read)

instance StackChunks DrumSounds where
  stackChunks = asStrictAssoc "DrumSounds" $ do
    seqs <- seqs =. req "seqs" (chunksParens $ chunksList chunkKey)
    return DrumSounds{..}

channelList :: ChunksCodec [Integer]
channelList = StackCodec
  { stackShow = stackShow fmt
  , stackParse = stackParse fmt <|> fmap (: []) (stackParse fmt')
    <|> expected "a number or a list of numbers"
  } where fmt  = chunksParens (stackChunks :: ChunksCodec [Integer])
          fmt' = stackChunks :: ChunksCodec Integer

data Song = Song
  { songName         :: T.Text
  , tracksCount      :: Maybe [Integer]
  , tracks           :: Map.HashMap T.Text [Integer]
  , vocalParts       :: Maybe Integer
  , pans             :: [Float]
  , vols             :: [Float]
  , cores            :: [Integer]
  , drumSolo         :: DrumSounds
  , drumFreestyle    :: DrumSounds
  , crowdChannels    :: Maybe [Integer]
  , hopoThreshold    :: Maybe Integer
  , muteVolume       :: Maybe Integer
  , muteVolumeVocals :: Maybe Integer
  -- seen in magma v1 / rb2:
  , midiFile         :: Maybe T.Text
  } deriving (Eq, Show, Read)

instance StackChunks Song where
  stackChunks = asStrictAssoc "Song" $ do
    songName         <- songName         =. req         "name"               (single chunkStringOrKey)
    tracksCount      <- tracksCount      =. opt Nothing "tracks_count"       (chunksMaybe $ chunksParens stackChunks)
    tracks           <- tracks           =. req         "tracks"             (chunksParens $ chunksDict chunkKey channelList)
    vocalParts       <- vocalParts       =. opt Nothing "vocal_parts"        stackChunks
    pans             <- pans             =. req         "pans"               (chunksParens stackChunks)
    vols             <- vols             =. req         "vols"               (chunksParens stackChunks)
    cores            <- cores            =. req         "cores"              (chunksParens stackChunks)
    drumSolo         <- drumSolo         =. req         "drum_solo"          stackChunks
    drumFreestyle    <- drumFreestyle    =. req         "drum_freestyle"     stackChunks
    crowdChannels    <- crowdChannels    =. opt Nothing "crowd_channels"     stackChunks
    hopoThreshold    <- hopoThreshold    =. opt Nothing "hopo_threshold"     stackChunks
    muteVolume       <- muteVolume       =. opt Nothing "mute_volume"        stackChunks
    muteVolumeVocals <- muteVolumeVocals =. opt Nothing "mute_volume_vocals" stackChunks
    midiFile         <- midiFile         =. opt Nothing "midi_file"          stackChunks
    return Song{..}

data SongPackage = SongPackage
  { name              :: T.Text
  , artist            :: T.Text
  , master            :: Bool
  , songId            :: Maybe (Either Integer T.Text)
  , song              :: Song
  , bank              :: Maybe T.Text
  , drumBank          :: Maybe T.Text
  , animTempo         :: Either AnimTempo Integer
  , bandFailCue       :: Maybe T.Text
  , songScrollSpeed   :: Integer
  , preview           :: (Integer, Integer)
  , songLength        :: Maybe Integer
  , rank              :: Map.HashMap T.Text Integer
  , solo              :: Maybe [T.Text]
  , songFormat        :: Integer
  , version           :: Integer
  , fake              :: Maybe Bool
  , gameOrigin        :: Maybe T.Text
  , ugc               :: Maybe Bool
  , rating            :: Integer
  , genre             :: T.Text
  , subGenre          :: Maybe T.Text
  , vocalGender       :: Maybe Gender
  , shortVersion      :: Maybe Integer
  , yearReleased      :: Integer
  , albumArt          :: Maybe Bool
  , albumName         :: Maybe T.Text
  , albumTrackNumber  :: Maybe Integer
  , packName          :: Maybe T.Text
  , vocalTonicNote    :: Maybe Pitch
  , songTonality      :: Maybe Tonality
  , songKey           :: Maybe Pitch
  , tuningOffsetCents :: Maybe Float
  , realGuitarTuning  :: Maybe [Integer]
  , realBassTuning    :: Maybe [Integer]
  , guidePitchVolume  :: Maybe Float
  , encoding          :: Maybe T.Text
  , extraAuthoring    :: Maybe [T.Text]
  , alternatePath     :: Maybe Bool
  -- seen in magma v1 / rb2:
  , context           :: Maybe Integer
  , decade            :: Maybe T.Text
  , downloaded        :: Maybe Bool
  , basePoints        :: Maybe Integer
  } deriving (Eq, Show, Read)

instance StackChunks SongPackage where
  stackChunks = asStrictAssoc "SongPackage" $ do
    name              <- name              =. req         "name"                (single chunkString)
    artist            <- artist            =. req         "artist"              (single chunkString)
    master            <- master            =. fill False  "master"              stackChunks
    songId            <- songId            =. opt Nothing "song_id"             (chunksMaybe $ eitherCodec stackChunks $ single chunkKey)
    song              <- song              =. req         "song"                stackChunks
    bank              <- bank              =. opt Nothing "bank"                stackChunks
    drumBank          <- drumBank          =. opt Nothing "drum_bank"           stackChunks
    animTempo         <- animTempo         =. req         "anim_tempo"          stackChunks
    bandFailCue       <- bandFailCue       =. opt Nothing "band_fail_cue"       stackChunks
    songScrollSpeed   <- songScrollSpeed   =. req         "song_scroll_speed"   stackChunks
    preview           <- preview           =. req         "preview"             stackChunks
    songLength        <- songLength        =. opt Nothing "song_length"         stackChunks
    rank              <- rank              =. req         "rank"                stackChunks
    solo              <- solo              =. opt Nothing "solo"                (chunksMaybe $ chunksParens $ chunksList chunkKey)
    songFormat        <- songFormat        =. req         "format"              stackChunks
    version           <- version           =. req         "version"             stackChunks
    fake              <- fake              =. opt Nothing "fake"                stackChunks
    gameOrigin        <- gameOrigin        =. opt Nothing "game_origin"         (chunksMaybe $ single chunkKey)
    ugc               <- ugc               =. opt Nothing "ugc"                 stackChunks
    rating            <- rating            =. fill 4      "rating"              stackChunks -- 4 is Unrated
    genre             <- genre             =. req         "genre"               (single chunkKey)
    subGenre          <- subGenre          =. opt Nothing "sub_genre"           (chunksMaybe $ single chunkKey)
    vocalGender       <- vocalGender       =. opt Nothing "vocal_gender"        stackChunks
    shortVersion      <- shortVersion      =. opt Nothing "short_version"       stackChunks
    yearReleased      <- yearReleased      =. req         "year_released"       stackChunks
    albumArt          <- albumArt          =. opt Nothing "album_art"           stackChunks
    albumName         <- albumName         =. opt Nothing "album_name"          (chunksMaybe $ single chunkString)
    albumTrackNumber  <- albumTrackNumber  =. opt Nothing "album_track_number"  stackChunks
    packName          <- packName          =. opt Nothing "pack_name"           (chunksMaybe $ single chunkString)
    vocalTonicNote    <- vocalTonicNote    =. opt Nothing "vocal_tonic_note"    stackChunks
    songTonality      <- songTonality      =. opt Nothing "song_tonality"       stackChunks
    songKey           <- songKey           =. opt Nothing "song_key"            stackChunks
    tuningOffsetCents <- tuningOffsetCents =. opt Nothing "tuning_offset_cents" stackChunks
    realGuitarTuning  <- realGuitarTuning  =. opt Nothing "real_guitar_tuning"  (chunksMaybe $ chunksParens stackChunks)
    realBassTuning    <- realBassTuning    =. opt Nothing "real_bass_tuning"    (chunksMaybe $ chunksParens stackChunks)
    guidePitchVolume  <- guidePitchVolume  =. opt Nothing "guide_pitch_volume"  stackChunks
    encoding          <- encoding          =. opt Nothing "encoding"            (chunksMaybe $ single chunkKey)
    extraAuthoring    <- extraAuthoring    =. opt Nothing "extra_authoring"     stackChunks
    alternatePath     <- alternatePath     =. opt Nothing "alternate_path"      stackChunks
    context           <- context           =. opt Nothing "context"             stackChunks
    decade            <- decade            =. opt Nothing "decade"              (chunksMaybe $ single chunkKey)
    downloaded        <- downloaded        =. opt Nothing "downloaded"          stackChunks
    basePoints        <- basePoints        =. opt Nothing "base_points"         stackChunks
    return SongPackage{..}
