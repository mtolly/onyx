{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.DTA.Serialize.RB3 where

import           Control.Applicative      ((<|>))
import           Data.DTA
import           Data.DTA.Serialize.Magma (Gender (..))
import           Data.DTA.Serialize2
import qualified Data.HashMap.Strict      as Map
import qualified Data.Text                as T
import           JSONData                 (StackCodec (..), eitherCodec)

data Pitch
  = C
  | CSharp
  | D
  | DSharp
  | E
  | F
  | FSharp
  | G
  | GSharp
  | A
  | ASharp
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

dtaRecord "DrumSounds" eosr $ do
  req "seqs" "seqs" [t| [T.Text] |] [e| chunksParens $ chunksList chunkKey |]

channelList :: ChunksCodec [Integer]
channelList = StackCodec
  { stackShow = stackShow fmt
  , stackParse = stackParse fmt <|> fmap (: []) (stackParse fmt')
    <|> expected "a number or a list of numbers"
  } where fmt  = chunksParens (stackChunks :: ChunksCodec [Integer])
          fmt' = stackChunks :: ChunksCodec Integer

dtaRecord "Song" esr $ do
  req "songName" "name" [t| T.Text |] [e| single chunkStringOrKey |]
  opt "tracksCount" "tracks_count" [t| Maybe [Integer] |] [e| Nothing |] [e| chunksMaybe $ chunksParens stackChunks |]
  req "tracks" "tracks" [t| Map.HashMap T.Text [Integer] |] [e| chunksParens $ chunksDict chunkKey channelList |]
  opt "vocalParts" "vocal_parts" [t| Maybe Integer |] [e| Nothing |] [e| stackChunks |]
  req "pans" "pans" [t| [Float] |] [e| chunksParens stackChunks |]
  req "vols" "vols" [t| [Float] |] [e| chunksParens stackChunks |]
  req "cores" "cores" [t| [Integer] |] [e| chunksParens stackChunks |]
  req "drumSolo" "drum_solo" [t| DrumSounds |] [e| stackChunks |]
  req "drumFreestyle" "drum_freestyle" [t| DrumSounds |] [e| stackChunks |]
  opt "crowdChannels" "crowd_channels" [t| Maybe [Integer] |] [e| Nothing |] [e| stackChunks |]
  opt "hopoThreshold" "hopo_threshold" [t| Maybe Integer |] [e| Nothing |] [e| stackChunks |]
  opt "muteVolume" "mute_volume" [t| Maybe Integer |] [e| Nothing |] [e| stackChunks |]
  opt "muteVolumeVocals" "mute_volume_vocals" [t| Maybe Integer |] [e| Nothing |] [e| stackChunks |]
  -- seen in magma v1 / rb2:
  opt "midiFile" "midi_file" [t| Maybe T.Text |] [e| Nothing |] [e| stackChunks |]

dtaRecord "SongPackage" esr $ do
  req "name" "name" [t| T.Text |] [e| single chunkString |]
  req "artist" "artist" [t| T.Text |] [e| single chunkString |]
  req "master" "master" [t| Bool |] [e| stackChunks |]
  req "songId" "song_id" [t| Either Integer T.Text |] [e| eitherCodec stackChunks $ single chunkKey |]
  req "song" "song" [t| Song |] [e| stackChunks |]
  opt "bank" "bank" [t| Maybe T.Text |] [e| Nothing |] [e| stackChunks |]
  opt "drumBank" "drum_bank" [t| Maybe T.Text |] [e| Nothing |] [e| stackChunks |]
  req "animTempo" "anim_tempo" [t| Either AnimTempo Integer |] [e| stackChunks |]
  opt "bandFailCue" "band_fail_cue" [t| Maybe T.Text |] [e| Nothing |] [e| stackChunks |]
  req "songScrollSpeed" "song_scroll_speed" [t| Integer |] [e| stackChunks |]
  req "preview" "preview" [t| (Integer, Integer) |] [e| stackChunks |]
  opt "songLength" "song_length" [t| Maybe Integer |] [e| Nothing |] [e| stackChunks |]
  req "rank" "rank" [t| Map.HashMap T.Text Integer |] [e| stackChunks |]
  opt "solo" "solo" [t| Maybe [T.Text] |] [e| Nothing |] [e| chunksMaybe $ chunksParens $ chunksList chunkKey |]
  req "songFormat" "format" [t| Integer |] [e| stackChunks |]
  req "version" "version" [t| Integer |] [e| stackChunks |]
  req "gameOrigin" "game_origin" [t| T.Text |] [e| single chunkKey |]
  req "rating" "rating" [t| Integer |] [e| stackChunks |]
  req "genre" "genre" [t| T.Text |] [e| single chunkKey |]
  opt "subGenre" "sub_genre" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe $ single chunkKey |]
  req "vocalGender" "vocal_gender" [t| Gender |] [e| stackChunks |]
  opt "shortVersion" "short_version" [t| Maybe Integer |] [e| Nothing |] [e| stackChunks |]
  req "yearReleased" "year_released" [t| Integer |] [e| stackChunks |]
  opt "albumArt" "album_art" [t| Maybe Bool |] [e| Nothing |] [e| stackChunks |]
  opt "albumName" "album_name" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe $ single chunkString |]
  opt "albumTrackNumber" "album_track_number" [t| Maybe Integer |] [e| Nothing |] [e| stackChunks |]
  opt "vocalTonicNote" "vocal_tonic_note" [t| Maybe Pitch |] [e| Nothing |] [e| stackChunks |]
  opt "songTonality" "song_tonality" [t| Maybe Tonality |] [e| Nothing |] [e| stackChunks |]
  opt "songKey" "song_key" [t| Maybe Pitch |] [e| Nothing |] [e| stackChunks |]
  opt "tuningOffsetCents" "tuning_offset_cents" [t| Maybe Float |] [e| Nothing |] [e| stackChunks |]
  opt "realGuitarTuning" "real_guitar_tuning" [t| Maybe [Integer] |] [e| Nothing |] [e| chunksMaybe $ chunksParens stackChunks |]
  opt "realBassTuning" "real_bass_tuning" [t| Maybe [Integer] |] [e| Nothing |] [e| chunksMaybe $ chunksParens stackChunks |]
  opt "guidePitchVolume" "guide_pitch_volume" [t| Maybe Float |] [e| Nothing |] [e| stackChunks |]
  opt "encoding" "encoding" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe $ single chunkKey |]
  -- seen in magma v1 / rb2:
  opt "context" "context" [t| Maybe Integer |] [e| Nothing |] [e| stackChunks |]
  opt "decade" "decade" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe $ single chunkKey |]
  opt "downloaded" "downloaded" [t| Maybe Bool |] [e| Nothing |] [e| stackChunks |]
  opt "basePoints" "base_points" [t| Maybe Integer |] [e| Nothing |] [e| stackChunks |]
