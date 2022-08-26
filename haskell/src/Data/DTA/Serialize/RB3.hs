{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.DTA.Serialize.RB3 where

import           Control.Applicative            ((<|>))
import           Control.Monad.Codec            (CodecFor (..), (=.))
import           Control.Monad.Codec.Onyx       (eitherCodec, enumCodec,
                                                 expected, fill, opt, req)
import           Control.Monad.Trans.StackTrace (SendMessage)
import           Data.DTA
import           Data.DTA.Serialize
import           Data.DTA.Serialize.Magma       (Gender (..))
import qualified Data.HashMap.Strict            as Map
import qualified Data.Text                      as T
import           RockBand.Common                (Key (..), Tonality (..))

chunkTonicNote :: (SendMessage m) => ChunkCodec m Key
chunkTonicNote = enumCodec "Key" $ Int . fromIntegral . fromEnum

chunkTonality :: (SendMessage m) => ChunkCodec m Tonality
chunkTonality = enumCodec "Tonality" $ Int . fromIntegral . fromEnum

data AnimTempo = KTempoSlow | KTempoMedium | KTempoFast
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackChunk AnimTempo where
  stackChunk = enumCodec "AnimTempo" $ \case
    KTempoSlow   -> Sym "kTempoSlow"
    KTempoMedium -> Sym "kTempoMedium"
    KTempoFast   -> Sym "kTempoFast"
instance StackChunks AnimTempo

newtype DrumSounds = DrumSounds
  { seqs :: [T.Text]
  } deriving (Eq, Ord, Show)

instance StackChunks DrumSounds where
  stackChunks = asWarnAssoc "DrumSounds" $ do
    seqs <- seqs =. req "seqs" (chunksParens $ chunksList chunkSym)
    return DrumSounds{..}

channelList :: (SendMessage m) => ChunksCodec m [Integer]
channelList = Codec
  { codecOut = codecOut fmt1
  , codecIn = codecIn fmt1 <|> fmap (: []) (codecIn fmt2) <|> codecIn fmt3
    <|> expected "a number or a list of numbers"
  } where fmt1 = chunksParens (stackChunks :: (SendMessage m) => ChunksCodec m [Integer])
          fmt2 = stackChunks :: (SendMessage m) => ChunksCodec m Integer
          -- this format (multiple numbers with no parens) seen in GH80s 2007-05-18 prototype
          fmt3 = stackChunks :: (SendMessage m) => ChunksCodec m [Integer]

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
  , muteVolume       :: Maybe Float
  , muteVolumeVocals :: Maybe Float
  , hopoThreshold    :: Maybe Integer
  -- magma v1 / rb2:
  , midiFile         :: Maybe T.Text
  } deriving (Eq, Show)

instance StackChunks Song where
  stackChunks = asWarnAssoc "Song" $ do
    songName         <- songName         =. req         "name"               (single chunkString)
    tracksCount      <- tracksCount      =. opt Nothing "tracks_count"       (chunksMaybe $ chunksParens stackChunks)
    tracks           <- tracks           =. req         "tracks"             (chunksParens $ chunksDictList chunkSym channelList)
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
  , artist            :: Maybe T.Text -- absent in beatles
  , master            :: Bool
  , song              :: Song
  , songScrollSpeed   :: Integer
  , bank              :: Maybe T.Text
  , drumBank          :: Maybe T.Text
  , animTempo         :: Either AnimTempo Integer
  , songLength        :: Maybe Integer
  , preview           :: (Integer, Integer)
  , rank              :: Map.HashMap T.Text Integer
  , genre             :: Maybe T.Text -- absent in beatles
  , vocalGender       :: Maybe Gender
  , version           :: Integer
  , songFormat        :: Integer
  , albumArt          :: Maybe Bool
  , yearReleased      :: Maybe Integer -- absent in beatles (has date_released)
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
  , yearRecorded      :: Maybe Integer -- beatles uses date_recorded
  , packName          :: Maybe T.Text
  , songKey           :: Maybe Key -- shows in pro gtr/keys trainer I think
  , extraAuthoring    :: Maybe [T.Text] -- added by rb3 update snippets
  , context           :: Maybe Integer
  , decade            :: Maybe T.Text
  , downloaded        :: Maybe Bool
  , basePoints        :: Maybe Integer
  , alternatePath     :: Maybe Bool
  , videoVenues       :: Maybe [T.Text] -- lego
  , dateReleased      :: Maybe T.Text -- beatles, "YYYY-MM-DD" format
  , dateRecorded      :: Maybe T.Text -- beatles, "YYYY-MM-DD" format
  , video             :: Bool -- rb2 ps2, 1 if bg video should get an extra 2 seconds (5 instead of 3) leadin for music video countdown
  -- rb2dx:
  , author            :: Maybe T.Text
  } deriving (Eq, Show)

instance StackChunks SongPackage where
  stackChunks = asWarnAssoc "SongPackage" $ do
    name              <- name              =. req         "name"                (single chunkString)
    artist            <- artist            =. opt Nothing "artist"              (chunksMaybe $ single chunkString)
    master            <- master            =. fill False  "master"              stackChunks
    song              <- song              =. req         "song"                stackChunks
    songScrollSpeed   <- songScrollSpeed   =. req         "song_scroll_speed"   stackChunks
    bank              <- bank              =. opt Nothing "bank"                stackChunks
    drumBank          <- drumBank          =. opt Nothing "drum_bank"           (chunksMaybe $ single chunkSym) -- this has to be output as a symbol for C3
    animTempo         <- animTempo         =. fill (Left KTempoMedium) "anim_tempo" stackChunks
    songLength        <- songLength        =. opt Nothing "song_length"         stackChunks
    preview           <- preview           =. req         "preview"             stackChunks
    rank              <- rank              =. req         "rank"                (chunksDict chunkSym stackChunks)
    genre             <- genre             =. opt Nothing "genre"               (chunksMaybe $ single chunkSym)
    vocalGender       <- vocalGender       =. opt Nothing "vocal_gender"        stackChunks
    version           <- version           =. req         "version"             stackChunks
    songFormat        <- songFormat        =. req         "format"              stackChunks
    albumArt          <- albumArt          =. opt Nothing "album_art"           stackChunks
    yearReleased      <- yearReleased      =. opt Nothing "year_released"       stackChunks
    rating            <- rating            =. fill 4      "rating"              stackChunks -- 4 is Unrated
    subGenre          <- subGenre          =. opt Nothing "sub_genre"           (chunksMaybe $ single chunkSym)
    songId            <- songId            =. opt Nothing "song_id"             (chunksMaybe $ eitherCodec stackChunks $ single chunkSym)
    solo              <- solo              =. opt Nothing "solo"                (chunksMaybe $ chunksParens $ chunksList chunkSym)
    tuningOffsetCents <- tuningOffsetCents =. opt Nothing "tuning_offset_cents" stackChunks
    guidePitchVolume  <- guidePitchVolume  =. opt Nothing "guide_pitch_volume"  stackChunks
    gameOrigin        <- gameOrigin        =. opt Nothing "game_origin"         (chunksMaybe $ single chunkSym)
    encoding          <- encoding          =. opt Nothing "encoding"            (chunksMaybe $ single chunkSym)
    albumName         <- albumName         =. opt Nothing "album_name"          (chunksMaybe $ single chunkString)
    albumTrackNumber  <- albumTrackNumber  =. opt Nothing "album_track_number"  stackChunks
    vocalTonicNote    <- vocalTonicNote    =. opt Nothing "vocal_tonic_note"    (chunksMaybe $ single chunkTonicNote)
    -- older songs have vocal_note_tonic, but these have vocal_tonic_note
    -- added by missing_song_data.dta
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
    decade            <- decade            =. opt Nothing "decade"              (chunksMaybe $ single chunkSym)
    downloaded        <- downloaded        =. opt Nothing "downloaded"          stackChunks
    basePoints        <- basePoints        =. opt Nothing "base_points"         stackChunks
    alternatePath     <- alternatePath     =. opt Nothing "alternate_path"      stackChunks
    videoVenues       <- videoVenues       =. opt Nothing "video_venues"        (chunksMaybe $ chunksParens $ chunksList chunkSym)
    dateReleased      <- dateReleased      =. opt Nothing "date_released"       (chunksMaybe $ single chunkString)
    dateRecorded      <- dateRecorded      =. opt Nothing "date_recorded"       (chunksMaybe $ single chunkString)
    author            <- author            =. opt Nothing "author"              (chunksMaybe $ single chunkString)
    video             <- video             =. opt False   "video"               stackChunks
    return SongPackage{..}
