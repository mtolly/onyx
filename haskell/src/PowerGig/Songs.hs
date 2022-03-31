{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module PowerGig.Songs where

import           Control.Monad.Codec
import           Control.Monad.Codec.Onyx.XML
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Lazy           as BL
import           Data.Fixed                     (Milli)
import           Data.Foldable                  (toList)
import           Data.List.NonEmpty             (NonEmpty (..))
import           Data.SimpleHandle
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Vector                    as V
import           Rocksmith.CST                  (cstSpaceW3)
import           Text.XML.Light                 (parseXMLDoc)

findSongKeys :: Folder T.Text Readable -> IO [T.Text]
findSongKeys dir = case findFile ("Scripting" :| ["Songs.lua"]) dir of
  Nothing -> return []
  Just r -> do
    bs <- useHandle r handleToByteString
    return $ do
      ln <- T.lines $ TE.decodeLatin1 $ BL.toStrict bs
      toList $ T.stripPrefix "song:SetKey(" (T.strip ln)
        >>= T.stripSuffix ")"
        >>= T.stripPrefix "\"" . T.strip
        >>= T.stripSuffix "\""

loadSongXML :: (MonadIO m, SendMessage m) => T.Text -> Folder T.Text Readable -> StackTraceT m Song
loadSongXML k dir = inside ("Loading PowerGig song XML for: " <> show k) $ do
  -- need case-insensitive because some are different
  case findFileCI ("Audio" :| ["songs", k, k <> ".xml"]) dir of
    Nothing -> fatal "Couldn't find XML file"
    Just r -> do
      bs <- stackIO $ useHandle r handleToByteString
      elt <- maybe (fatal "Couldn't parse XML") return $ parseXMLDoc $ TE.decodeUtf8 $ BL.toStrict bs
      mapStackTraceT (`runReaderT` elt) $ codecIn $ isTag "song" $ parseInside' insideCodec

-- song.xsd

data Song = Song
  { song_info   :: Info
  , song_audio  :: Audio
  , song_source :: T.Text -- "disc" or "dlc"
  } deriving (Show)

instance IsInside Song where
  insideCodec = do
    useNamespace (Just "xsi") cstSpaceW3
    song_info   <- song_info   =. childTag "info"   (parseInside' insideCodec)
    song_audio  <- song_audio  =. childTag "audio"  (parseInside' insideCodec)
    song_source <- song_source =. childTag "source" (parseInside' childText)
    ignoreAttr $ inSpace cstSpaceW3 "noNamespaceSchemaLocation"
    return Song{..}

data Info = Info
  { info_title            :: T.Text
  , info_title_short      :: Maybe T.Text
  , info_title_shorter    :: Maybe T.Text
  , info_artist           :: T.Text
  , info_artist_short     :: Maybe T.Text
  , info_year             :: Int -- 1900 to 2020 inclusive
  , info_album            :: T.Text
  , info_composer         :: T.Text
  , info_lyricist         :: T.Text
  , info_genre            :: T.Text -- actually enum in xsd
  , info_cover_artwork    :: CoverArtwork
  , info_url              :: T.Text
  , info_length           :: Length
  , info_singer_gender    :: Maybe T.Text -- "male" "female" or "both"
  , info_guitar_intensity :: Maybe Int -- 1 to 3 inclusive
  , info_drums_intensity  :: Maybe Int
  , info_vocals_intensity :: Maybe Int
  } deriving (Show)

instance IsInside Info where
  insideCodec = do
    info_title            <- info_title            =. childTag    "title"            (parseInside' childText)
    info_title_short      <- info_title_short      =. childTagOpt "title_short"      (parseInside' childText)
    info_title_shorter    <- info_title_shorter    =. childTagOpt "title_shorter"    (parseInside' childText)
    info_artist           <- info_artist           =. childTag    "artist"           (parseInside' childText)
    info_artist_short     <- info_artist_short     =. childTagOpt "artist_short"     (parseInside' childText)
    info_year             <- info_year             =. childTag    "year"             (parseInside' $ intText childText)
    info_album            <- info_album            =. childTag    "album"            (parseInside' childText)
    info_composer         <- info_composer         =. childTag    "composer"         (parseInside' childText)
    info_lyricist         <- info_lyricist         =. childTag    "lyricist"         (parseInside' childText)
    info_genre            <- info_genre            =. childTag    "genre"            (parseInside' childText)
    info_cover_artwork    <- info_cover_artwork    =. childTag    "cover_artwork"    (parseInside' insideCodec)
    info_url              <- info_url              =. childTag    "url"              (parseInside' childText)
    info_length           <- info_length           =. childTag    "length"           (parseInside' insideCodec)
    info_singer_gender    <- info_singer_gender    =. childTagOpt "singer_gender"    (parseInside' childText)
    info_guitar_intensity <- info_guitar_intensity =. childTagOpt "guitar_intensity" (parseInside' $ intText childText)
    info_drums_intensity  <- info_drums_intensity  =. childTagOpt "drums_intensity"  (parseInside' $ intText childText)
    info_vocals_intensity <- info_vocals_intensity =. childTagOpt "vocals_intensity" (parseInside' $ intText childText)
    return Info{..}

data Audio = Audio
  { audio_combined_audio :: Maybe CombinedAudio
  , audio_count_off      :: Maybe CountOff
  , audio_preview        :: V.Vector Preview
  , audio_midi           :: T.Text
  , audio_cue            :: T.Text
  , audio_tempo          :: T.Text
  , audio_key_signature  :: KeySignature
  , audio_bpm            :: Int -- 0 to 300 inclusive
  , audio_chords         :: V.Vector T.Text
  , audio_compressor     :: Maybe Compressor
  , audio_backing_track  :: InstrumentType
  , audio_guitar         :: InstrumentType
  , audio_drums          :: InstrumentType
  , audio_vocals         :: InstrumentType
  } deriving (Show)

instance IsInside Audio where
  insideCodec = do
    audio_combined_audio <- audio_combined_audio =. childTagOpt "combined_audio"  (parseInside' insideCodec)
    audio_count_off      <- audio_count_off      =. childTagOpt "count_off"       (parseInside' insideCodec)
    audio_preview        <- audio_preview        =. bareListWithTagName "preview" (parseInside' insideCodec)
    audio_midi           <- audio_midi           =. childTag    "midi"            (parseInside' childText)
    audio_cue            <- audio_cue            =. childTag    "cue"             (parseInside' childText)
    audio_tempo          <- audio_tempo          =. childTag    "tempo"           (parseInside' childText)
    audio_key_signature  <- audio_key_signature  =. childTag    "key_signature"   (parseInside' insideCodec)
    audio_bpm            <- audio_bpm            =. childTag    "bpm"             (parseInside' $ intText childText)
    audio_chords         <- audio_chords         =. childTag    "chords"          (parseInside' $ bareList $ isTag "chord" $ parseInside' childText)
    audio_compressor     <- audio_compressor     =. childTagOpt "compressor"      (parseInside' insideCodec)
    audio_backing_track  <- audio_backing_track  =. childTag    "backing_track"   (parseInside' insideCodec)
    audio_guitar         <- audio_guitar         =. childTag    "guitar"          (parseInside' insideCodec)
    audio_drums          <- audio_drums          =. childTag    "drums"           (parseInside' insideCodec)
    audio_vocals         <- audio_vocals         =. childTag    "vocals"          (parseInside' insideCodec)
    return Audio{..}

data CombinedAudio = CombinedAudio
  { ca_file         :: T.Text
  , ca_xbox360_file :: Maybe T.Text
  , ca_ps3_file     :: Maybe T.Text
  , ca_num_channels :: Int -- 1 to 16 inclusive
  } deriving (Show)

instance IsInside CombinedAudio where
  insideCodec = do
    ca_file         <- ca_file         =. reqAttr "file"
    ca_xbox360_file <- ca_xbox360_file =. optAttr "xbox360_file"
    ca_ps3_file     <- ca_ps3_file     =. optAttr "ps3_file"
    ca_num_channels <- ca_num_channels =. intText (reqAttr "num_channels")
    return CombinedAudio{..}

data CountOff = CountOff
  { count_off_bpm :: Maybe Milli
  } deriving (Show)

instance IsInside CountOff where
  insideCodec = do
    count_off_bpm <- count_off_bpm =. zoomValue (maybeValue milliValue) (optAttr "bpm")
    return CountOff{..}

data CoverArtwork = CoverArtwork
  { art_file_name :: T.Text
  } deriving (Show)

instance IsInside CoverArtwork where
  insideCodec = do
    art_file_name <- art_file_name =. reqAttr "file_name"
    return CoverArtwork{..}

data Length = Length
  { length_minutes :: Int -- 0 to 59 inclusive
  , length_seconds :: Int -- 0 to 59 inclusive
  } deriving (Show)

instance IsInside Length where
  insideCodec = do
    length_minutes <- length_minutes =. childTag "minutes" (parseInside' $ intText childText)
    length_seconds <- length_seconds =. childTag "seconds" (parseInside' $ intText childText)
    return Length{..}

data KeySignature = KeySignature
  { ks_note  :: T.Text -- A B C D E F G
  , ks_pitch :: T.Text -- sharp flat natural
  , ks_key   :: T.Text -- major minor mixolydian
  } deriving (Show)

instance IsInside KeySignature where
  insideCodec = do
    ks_note  <- ks_note  =. childTag "note"  (parseInside' childText)
    ks_pitch <- ks_pitch =. childTag "pitch" (parseInside' childText)
    ks_key   <- ks_key   =. childTag "key"   (parseInside' childText)
    return KeySignature{..}

data Preview = Preview
  { preview_start_position   :: Maybe Milli
  , preview_attack_time      :: Maybe Milli
  , preview_release_position :: Maybe Milli
  , preview_release_time     :: Maybe Milli
  , preview_file             :: Maybe T.Text
  , preview_xbox360_file     :: Maybe T.Text
  , preview_ps3_file         :: Maybe T.Text
  } deriving (Show)

instance IsInside Preview where
  insideCodec = do
    preview_start_position   <- preview_start_position    =. zoomValue (maybeValue milliValue) (optAttr "start_position"  )
    preview_attack_time      <- preview_attack_time       =. zoomValue (maybeValue milliValue) (optAttr "attack_time"     )
    preview_release_position <- preview_release_position  =. zoomValue (maybeValue milliValue) (optAttr "release_position")
    preview_release_time     <- preview_release_time      =. zoomValue (maybeValue milliValue) (optAttr "release_time"    )
    preview_file             <- preview_file              =. optAttr "file"
    preview_xbox360_file     <- preview_xbox360_file      =. optAttr "xbox360_file"
    preview_ps3_file         <- preview_ps3_file          =. optAttr "ps3_file"
    return Preview{..}

-- Leaving undone, doesn't appear to actually exist in any of the xml files
data Compressor = Compressor deriving (Show)
instance IsInside Compressor where insideCodec = return Compressor

data InstrumentType = InstrumentType
  { inst_chord_resolution  :: Maybe (PerDifficulty Int)
  , inst_sustain_threshold :: Maybe (PerDifficulty Int)
  , inst_fly_time          :: Maybe (PerDifficulty Milli)
  , inst_tolerance         :: Maybe (PerDifficulty Milli)
  , inst_wav_file          :: Maybe T.Text
  , inst_xbox360_file      :: Maybe T.Text
  , inst_volume            :: Milli
  , inst_fill_mute_level   :: Maybe Milli
  , inst_ramp_times        :: Maybe RampTimes
  , inst_mode              :: Mode
  , inst_stereo_width      :: Maybe StereoWidth
  , inst_samples           :: Maybe (V.Vector SampleKit)
  , inst_display_range     :: Maybe DisplayRange
  } deriving (Show)

instance IsInside InstrumentType where
  insideCodec = do
    inst_chord_resolution  <- inst_chord_resolution  =. childTagOpt "chord_resolution"  (parseInside' $ parsePerDifficulty intValue)
    inst_sustain_threshold <- inst_sustain_threshold =. childTagOpt "sustain_threshold" (parseInside' $ parsePerDifficulty intValue)
    inst_fly_time          <- inst_fly_time          =. childTagOpt "fly_time"          (parseInside' $ parsePerDifficulty milliValue)
    inst_tolerance         <- inst_tolerance         =. childTagOpt "tolerance"         (parseInside' $ parsePerDifficulty milliValue)
    inst_wav_file          <- inst_wav_file          =. childTagOpt "wav_file"          (parseInside' childText)
    inst_xbox360_file      <- inst_xbox360_file      =. childTagOpt "xbox360_file"      (parseInside' childText)
    inst_volume            <- inst_volume            =. childTag    "volume"            (parseInside' $ milliText childText)
    inst_fill_mute_level   <- inst_fill_mute_level   =. childTagOpt "fill_mute_level"   (parseInside' $ milliText childText)
    inst_ramp_times        <- inst_ramp_times        =. childTagOpt "ramp_times"        (parseInside' insideCodec)
    inst_mode              <- inst_mode              =. childTag    "mode"              (parseInside' insideCodec)
    inst_stereo_width      <- inst_stereo_width      =. childTagOpt "stereo_width"      (parseInside' insideCodec)
    inst_samples           <- inst_samples           =. childTagOpt "samples"           (parseInside' $ bareList $ isTag "sample_kit" $ parseInside' insideCodec)
    inst_display_range     <- inst_display_range     =. childTagOpt "display_range"     (parseInside' insideCodec)
    return InstrumentType{..}

data PerDifficulty a = PerDifficulty
  { pd_beginner :: Maybe a
  , pd_easy     :: Maybe a
  , pd_medium   :: Maybe a
  , pd_hard     :: Maybe a
  , pd_expert   :: Maybe a
  } deriving (Show)

parsePerDifficulty :: (Monad m) => ValueCodec' m T.Text a -> InsideCodec m (PerDifficulty a)
parsePerDifficulty c = do
  pd_beginner <- pd_beginner =. zoomValue (maybeValue c) (optAttr "beginner")
  pd_easy     <- pd_easy     =. zoomValue (maybeValue c) (optAttr "easy"    )
  pd_medium   <- pd_medium   =. zoomValue (maybeValue c) (optAttr "medium"  )
  pd_hard     <- pd_hard     =. zoomValue (maybeValue c) (optAttr "hard"    )
  pd_expert   <- pd_expert   =. zoomValue (maybeValue c) (optAttr "expert"  )
  return PerDifficulty{..}

data RampTimes = RampTimes
  { ramp_mute_down      :: Maybe Milli
  , ramp_mute_up        :: Maybe Milli
  , ramp_fill_mute_down :: Maybe Milli
  , ramp_fill_mute_up   :: Maybe Milli
  } deriving (Show)

instance IsInside RampTimes where
  insideCodec = do
    ramp_mute_down      <- ramp_mute_down      =. zoomValue (maybeValue milliValue) (optAttr "mute_down")
    ramp_mute_up        <- ramp_mute_up        =. zoomValue (maybeValue milliValue) (optAttr "mute_up")
    ramp_fill_mute_down <- ramp_fill_mute_down =. zoomValue (maybeValue milliValue) (optAttr "fill_mute_down")
    ramp_fill_mute_up   <- ramp_fill_mute_up   =. zoomValue (maybeValue milliValue) (optAttr "fill_mute_up")
    return RampTimes{..}

data Mode = Mode
  -- actually only one of these should be present. but this is easier to make the parser for
  { mode_2d       :: Maybe Milli -- not actually present but in comments
  , mode_3d       :: Maybe Mode3D -- not actually present
  , mode_speakers :: Maybe ModeSpeakers
  } deriving (Show)

instance IsInside Mode where
  insideCodec = do
    mode_2d       <- mode_2d       =. childTagOpt "mode_2d"       (parseInside' $ milliText childText)
    mode_3d       <- mode_3d       =. childTagOpt "mode_3d"       (parseInside' insideCodec)
    mode_speakers <- mode_speakers =. childTagOpt "mode_speakers" (parseInside' insideCodec)
    return Mode{..}

data Mode3D = Mode3D
  { mode3d_x :: Milli
  , mode3d_y :: Milli
  , mode3d_z :: Milli
  } deriving (Show)

instance IsInside Mode3D where
  insideCodec = do
    mode3d_x <- mode3d_x =. milliText (reqAttr "x")
    mode3d_y <- mode3d_y =. milliText (reqAttr "y")
    mode3d_z <- mode3d_z =. milliText (reqAttr "z")
    return Mode3D{..}

data ModeSpeakers = ModeSpeakers
  { mode_speakers_speaker      :: V.Vector Speaker
  , mode_speakers_mute_levels  :: Maybe MuteLevels
  , mode_speakers_kit          :: V.Vector Kit
  , mode_speakers_unmute_pad   :: Maybe (PerDifficulty T.Text) -- each text: "same" or "any"
  , mode_speakers_num_speakers :: Maybe Int -- 1 to 8 inclusive
  , mode_speakers_num_channels :: Maybe Int -- 1 to 16 inclusive
  , mode_speakers_base_channel :: Maybe Int -- 1 to 16 inclusive
  } deriving (Show)

instance IsInside ModeSpeakers where
  insideCodec = do
    mode_speakers_speaker      <- mode_speakers_speaker      =. bareListWithTagName "speaker" (parseInside' insideCodec)
    mode_speakers_mute_levels  <- mode_speakers_mute_levels  =. childTagOpt "mute_levels" (parseInside' insideCodec)
    mode_speakers_kit          <- mode_speakers_kit          =. bareListWithTagName "kit" (parseInside' insideCodec)
    mode_speakers_unmute_pad   <- mode_speakers_unmute_pad   =. childTagOpt "unmute_pad"  (parseInside' $ parsePerDifficulty identityCodec')
    mode_speakers_num_speakers <- mode_speakers_num_speakers =. zoomValue (maybeValue intValue) (optAttr "num_speakers")
    mode_speakers_num_channels <- mode_speakers_num_channels =. zoomValue (maybeValue intValue) (optAttr "num_channels")
    mode_speakers_base_channel <- mode_speakers_base_channel =. zoomValue (maybeValue intValue) (optAttr "base_channel")
    return ModeSpeakers{..}

data Speaker = Speaker
  { speaker_output        :: T.Text -- front_left front_right front_center low_frequency back_left back_right side_left side_right
  , speaker_front_left    :: Maybe Milli
  , speaker_front_right   :: Maybe Milli
  , speaker_front_center  :: Maybe Milli
  , speaker_low_frequency :: Maybe Milli
  , speaker_back_left     :: Maybe Milli
  , speaker_back_right    :: Maybe Milli
  , speaker_side_left     :: Maybe Milli
  , speaker_side_right    :: Maybe Milli
  } deriving (Show)

instance IsInside Speaker where
  insideCodec = do
    speaker_output        <- speaker_output        =. reqAttr "output"
    speaker_front_left    <- speaker_front_left    =. zoomValue (maybeValue milliValue) (optAttr "front_left"   )
    speaker_front_right   <- speaker_front_right   =. zoomValue (maybeValue milliValue) (optAttr "front_right"  )
    speaker_front_center  <- speaker_front_center  =. zoomValue (maybeValue milliValue) (optAttr "front_center" )
    speaker_low_frequency <- speaker_low_frequency =. zoomValue (maybeValue milliValue) (optAttr "low_frequency")
    speaker_back_left     <- speaker_back_left     =. zoomValue (maybeValue milliValue) (optAttr "back_left"    )
    speaker_back_right    <- speaker_back_right    =. zoomValue (maybeValue milliValue) (optAttr "back_right"   )
    speaker_side_left     <- speaker_side_left     =. zoomValue (maybeValue milliValue) (optAttr "side_left"    )
    speaker_side_right    <- speaker_side_right    =. zoomValue (maybeValue milliValue) (optAttr "side_right"   )
    return Speaker{..}

data MuteLevels = MuteLevels
  { mute_levels_front_left    :: Maybe Milli
  , mute_levels_front_right   :: Maybe Milli
  , mute_levels_front_center  :: Maybe Milli
  , mute_levels_low_frequency :: Maybe Milli
  , mute_levels_back_left     :: Maybe Milli
  , mute_levels_back_right    :: Maybe Milli
  , mute_levels_side_left     :: Maybe Milli
  , mute_levels_side_right    :: Maybe Milli
  } deriving (Show)

instance IsInside MuteLevels where
  insideCodec = do
    mute_levels_front_left    <- mute_levels_front_left    =. zoomValue (maybeValue milliValue) (optAttr "front_left"   )
    mute_levels_front_right   <- mute_levels_front_right   =. zoomValue (maybeValue milliValue) (optAttr "front_right"  )
    mute_levels_front_center  <- mute_levels_front_center  =. zoomValue (maybeValue milliValue) (optAttr "front_center" )
    mute_levels_low_frequency <- mute_levels_low_frequency =. zoomValue (maybeValue milliValue) (optAttr "low_frequency")
    mute_levels_back_left     <- mute_levels_back_left     =. zoomValue (maybeValue milliValue) (optAttr "back_left"    )
    mute_levels_back_right    <- mute_levels_back_right    =. zoomValue (maybeValue milliValue) (optAttr "back_right"   )
    mute_levels_side_left     <- mute_levels_side_left     =. zoomValue (maybeValue milliValue) (optAttr "side_left"    )
    mute_levels_side_right    <- mute_levels_side_right    =. zoomValue (maybeValue milliValue) (optAttr "side_right"   )
    return MuteLevels{..}

data Kit = Kit
  { kit_kit_number    :: Int
  , kit_front_left    :: Maybe Int
  , kit_front_right   :: Maybe Int
  , kit_front_center  :: Maybe Int
  , kit_low_frequency :: Maybe Int
  , kit_back_left     :: Maybe Int
  , kit_back_right    :: Maybe Int
  , kit_side_left     :: Maybe Int
  , kit_side_right    :: Maybe Int
  } deriving (Show)

instance IsInside Kit where
  insideCodec = do
    kit_kit_number    <- kit_kit_number    =. intText (reqAttr "kit_number")
    kit_front_left    <- kit_front_left    =. zoomValue (maybeValue intValue) (optAttr "front_left"   )
    kit_front_right   <- kit_front_right   =. zoomValue (maybeValue intValue) (optAttr "front_right"  )
    kit_front_center  <- kit_front_center  =. zoomValue (maybeValue intValue) (optAttr "front_center" )
    kit_low_frequency <- kit_low_frequency =. zoomValue (maybeValue intValue) (optAttr "low_frequency")
    kit_back_left     <- kit_back_left     =. zoomValue (maybeValue intValue) (optAttr "back_left"    )
    kit_back_right    <- kit_back_right    =. zoomValue (maybeValue intValue) (optAttr "back_right"   )
    kit_side_left     <- kit_side_left     =. zoomValue (maybeValue intValue) (optAttr "side_left"    )
    kit_side_right    <- kit_side_right    =. zoomValue (maybeValue intValue) (optAttr "side_right"   )
    return Kit{..}

data StereoWidth = StereoWidth
  { sw_wide   :: Milli
  , sw_narrow :: Milli
  } deriving (Show)

instance IsInside StereoWidth where
  insideCodec = do
    sw_wide   <- sw_wide   =. milliText (reqAttr "wide"  )
    sw_narrow <- sw_narrow =. milliText (reqAttr "narrow")
    return StereoWidth{..}

data SampleKit = SampleKit
  { sample_kit_sound  :: V.Vector Sound
  , sample_kit_number :: Milli
  } deriving (Show)

instance IsInside SampleKit where
  insideCodec = do
    sample_kit_sound  <- sample_kit_sound  =. bareList (isTag "sound" $ parseInside' insideCodec)
    sample_kit_number <- sample_kit_number =. milliText (reqAttr "number")
    return SampleKit{..}

data Sound = Sound
  { sound_pad          :: T.Text -- one of: green red yellow blue orange white
  , sound_file         :: T.Text
  , sound_xbox360_file :: Maybe T.Text
  , sound_volume       :: Maybe Milli
  , sound_releasetime  :: Maybe Milli
  , sound_rvbroom      :: Maybe Int
  } deriving (Show)

instance IsInside Sound where
  insideCodec = do
    sound_pad          <- sound_pad          =. reqAttr "pad"
    sound_file         <- sound_file         =. reqAttr "file"
    sound_xbox360_file <- sound_xbox360_file =. optAttr "xbox360_file"
    sound_volume       <- sound_volume       =. zoomValue (maybeValue milliValue) (optAttr "volume")
    sound_releasetime  <- sound_releasetime  =. zoomValue (maybeValue milliValue) (optAttr "releasetime")
    sound_rvbroom      <- sound_rvbroom      =. zoomValue (maybeValue intValue  ) (optAttr "rvbroom")
    return Sound{..}

data DisplayRange = DisplayRange
  { dr_height :: Int -- 0 to 127 inclusive
  } deriving (Show)

instance IsInside DisplayRange where
  insideCodec = do
    dr_height <- dr_height =. intText (reqAttr "height")
    return DisplayRange{..}
