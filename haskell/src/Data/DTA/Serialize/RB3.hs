{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.DTA.Serialize.RB3 where

import           Control.Applicative            ((<|>))
import           Control.Monad.Codec            (CodecFor (..), (=.))
import           Control.Monad.Trans.StackTrace (SendMessage)
import           Data.DTA
import           Data.DTA.Serialize
import           Data.DTA.Serialize.Magma       (Gender (..))
import qualified Data.HashMap.Strict            as Map
import qualified Data.Text                      as T
import           JSONData                       (eitherCodec, expected, fill,
                                                 opt, req)
import           RockBand.Common                (Key (..), Tonality (..))

chunkTonicNote :: (SendMessage m) => ChunkCodec m Key
chunkTonicNote = dtaEnum "Key" $ Int . fromIntegral . fromEnum

chunkTonality :: (SendMessage m) => ChunkCodec m Tonality
chunkTonality = dtaEnum "Tonality" $ Int . fromIntegral . fromEnum

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

channelList :: (SendMessage m) => ChunksCodec m [Integer]
channelList = Codec
  { codecOut = codecOut fmt
  , codecIn = codecIn fmt <|> fmap (: []) (codecIn fmt')
    <|> expected "a number or a list of numbers"
  } where fmt  = chunksParens (stackChunks :: (SendMessage m) => ChunksCodec m [Integer])
          fmt' = stackChunks :: (SendMessage m) => ChunksCodec m Integer

data Song = Song
  -- rbn2 keys in c3 magma order:
  { songName         :: T.Text
  , tracksCount      :: Maybe [Integer]
  , tracks           :: DictList T.Text [Integer]
  , pans             :: [Float]
  , vols             :: [Float]
  , cores            :: [Integer]
  , crowdChannels    :: Maybe [Integer]
  , vocalParts       :: Maybe Integer
  , drumSolo         :: DrumSounds
  , drumFreestyle    :: DrumSounds
  , muteVolume       :: Maybe Integer
  , muteVolumeVocals :: Maybe Integer
  , hopoThreshold    :: Maybe Integer
  -- magma v1 / rb2:
  , midiFile         :: Maybe T.Text
  } deriving (Eq, Show, Read)

instance StackChunks Song where
  stackChunks = asStrictAssoc "Song" $ do
    songName         <- songName         =. req         "name"               (single chunkString)
    tracksCount      <- tracksCount      =. opt Nothing "tracks_count"       (chunksMaybe $ chunksParens stackChunks)
    tracks           <- tracks           =. req         "tracks"             (chunksParens $ chunksDictList chunkKey channelList)
    pans             <- pans             =. req         "pans"               (chunksParens stackChunks)
    vols             <- vols             =. req         "vols"               (chunksParens stackChunks)
    cores            <- cores            =. req         "cores"              (chunksParens stackChunks)
    crowdChannels    <- crowdChannels    =. opt Nothing "crowd_channels"     stackChunks
    vocalParts       <- vocalParts       =. opt Nothing "vocal_parts"        stackChunks
    drumSolo         <- drumSolo         =. req         "drum_solo"          stackChunks
    drumFreestyle    <- drumFreestyle    =. req         "drum_freestyle"     stackChunks
    muteVolume       <- muteVolume       =. opt Nothing "mute_volume"        stackChunks
    muteVolumeVocals <- muteVolumeVocals =. opt Nothing "mute_volume_vocals" stackChunks
    hopoThreshold    <- hopoThreshold    =. opt Nothing "hopo_threshold"     stackChunks
    midiFile         <- midiFile         =. opt Nothing "midi_file"          stackChunks
    return Song{..}

data SongPackage = SongPackage
  -- rbn2 keys in c3 magma order:
  { name              :: T.Text
  , artist            :: T.Text
  , master            :: Bool
  , song              :: Song
  , songScrollSpeed   :: Integer
  , bank              :: Maybe T.Text
  , drumBank          :: Maybe T.Text
  , animTempo         :: Either AnimTempo Integer
  , songLength        :: Maybe Integer
  , preview           :: (Integer, Integer)
  , rank              :: Map.HashMap T.Text Integer
  , genre             :: T.Text
  , vocalGender       :: Maybe Gender
  , version           :: Integer
  , songFormat        :: Integer
  , albumArt          :: Maybe Bool
  , yearReleased      :: Integer
  , rating            :: Integer
  , subGenre          :: Maybe T.Text
  , songId            :: Maybe (Either Integer T.Text)
  , solo              :: Maybe [T.Text]
  , tuningOffsetCents :: Maybe Float -- can this really be float, or only int? should double check
  , guidePitchVolume  :: Maybe Float
  , gameOrigin        :: Maybe T.Text
  , encoding          :: Maybe T.Text
  , albumName         :: Maybe T.Text
  , albumTrackNumber  :: Maybe Integer
  , vocalTonicNote    :: Maybe Key
  , songTonality      :: Maybe Tonality
  , realGuitarTuning  :: Maybe [Integer]
  , realBassTuning    :: Maybe [Integer]
  -- other keys:
  , bandFailCue       :: Maybe T.Text
  , fake              :: Maybe Bool
  , ugc               :: Maybe Bool
  , shortVersion      :: Maybe Integer
  , yearRecorded      :: Maybe Integer
  , packName          :: Maybe T.Text
  , songKey           :: Maybe Key -- shows in pro gtr/keys trainer I think
  , extraAuthoring    :: Maybe [T.Text] -- added by rb3 update snippets
  , context           :: Maybe Integer
  , decade            :: Maybe T.Text
  , downloaded        :: Maybe Bool
  , basePoints        :: Maybe Integer
  , alternatePath     :: Maybe Bool
  , videoVenues       :: Maybe [T.Text] -- lego
  } deriving (Eq, Show, Read)

instance StackChunks SongPackage where
  stackChunks = asStrictAssoc "SongPackage" $ do
    name              <- name              =. req         "name"                (single chunkString)
    artist            <- artist            =. req         "artist"              (single chunkString)
    master            <- master            =. fill False  "master"              stackChunks
    song              <- song              =. req         "song"                stackChunks
    songScrollSpeed   <- songScrollSpeed   =. req         "song_scroll_speed"   stackChunks
    bank              <- bank              =. opt Nothing "bank"                stackChunks
    drumBank          <- drumBank          =. opt Nothing "drum_bank"           (chunksMaybe $ single chunkKey) -- this has to be output as a key for C3
    animTempo         <- animTempo         =. req         "anim_tempo"          stackChunks
    songLength        <- songLength        =. opt Nothing "song_length"         stackChunks
    preview           <- preview           =. req         "preview"             stackChunks
    rank              <- rank              =. req         "rank"                (chunksDict chunkKey stackChunks)
    genre             <- genre             =. req         "genre"               (single chunkKey)
    vocalGender       <- vocalGender       =. opt Nothing "vocal_gender"        stackChunks
    version           <- version           =. req         "version"             stackChunks
    songFormat        <- songFormat        =. req         "format"              stackChunks
    albumArt          <- albumArt          =. opt Nothing "album_art"           stackChunks
    yearReleased      <- yearReleased      =. req         "year_released"       stackChunks
    rating            <- rating            =. fill 4      "rating"              stackChunks -- 4 is Unrated
    subGenre          <- subGenre          =. opt Nothing "sub_genre"           (chunksMaybe $ single chunkKey)
    songId            <- songId            =. opt Nothing "song_id"             (chunksMaybe $ eitherCodec stackChunks $ single chunkKey)
    solo              <- solo              =. opt Nothing "solo"                (chunksMaybe $ chunksParens $ chunksList chunkKey)
    tuningOffsetCents <- tuningOffsetCents =. opt Nothing "tuning_offset_cents" stackChunks
    guidePitchVolume  <- guidePitchVolume  =. opt Nothing "guide_pitch_volume"  stackChunks
    gameOrigin        <- gameOrigin        =. opt Nothing "game_origin"         (chunksMaybe $ single chunkKey)
    encoding          <- encoding          =. opt Nothing "encoding"            (chunksMaybe $ single chunkKey)
    albumName         <- albumName         =. opt Nothing "album_name"          (chunksMaybe $ single chunkString)
    albumTrackNumber  <- albumTrackNumber  =. opt Nothing "album_track_number"  stackChunks
    vocalTonicNote    <- vocalTonicNote    =. opt Nothing "vocal_tonic_note"    (chunksMaybe $ single chunkTonicNote)
    songTonality      <- songTonality      =. opt Nothing "song_tonality"       (chunksMaybe $ single chunkTonality)
    realGuitarTuning  <- realGuitarTuning  =. opt Nothing "real_guitar_tuning"  (chunksMaybe $ chunksParens stackChunks)
    realBassTuning    <- realBassTuning    =. opt Nothing "real_bass_tuning"    (chunksMaybe $ chunksParens stackChunks)
    bandFailCue       <- bandFailCue       =. opt Nothing "band_fail_cue"       stackChunks
    fake              <- fake              =. opt Nothing "fake"                stackChunks
    ugc               <- ugc               =. opt Nothing "ugc"                 stackChunks
    shortVersion      <- shortVersion      =. opt Nothing "short_version"       stackChunks
    yearRecorded      <- yearRecorded      =. opt Nothing "year_recorded"       stackChunks
    packName          <- packName          =. opt Nothing "pack_name"           (chunksMaybe $ single chunkString)
    songKey           <- songKey           =. opt Nothing "song_key"            (chunksMaybe $ single chunkTonicNote)
    extraAuthoring    <- extraAuthoring    =. opt Nothing "extra_authoring"     stackChunks
    context           <- context           =. opt Nothing "context"             stackChunks
    decade            <- decade            =. opt Nothing "decade"              (chunksMaybe $ single chunkKey)
    downloaded        <- downloaded        =. opt Nothing "downloaded"          stackChunks
    basePoints        <- basePoints        =. opt Nothing "base_points"         stackChunks
    alternatePath     <- alternatePath     =. opt Nothing "alternate_path"      stackChunks
    videoVenues       <- videoVenues       =. opt Nothing "video_venues"        (chunksMaybe $ chunksParens $ chunksList chunkKey)
    return SongPackage{..}
