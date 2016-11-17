{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.DTA.Serialize.RB3 where

import           Control.Applicative      ((<|>))
import           Data.DTA
import           Data.DTA.Serialize.Magma (Gender (..))
import           Data.DTA.Serialize2
import qualified Data.Text                as T

dtaEnum "Pitch" eosreb $ do
  val "C"      [e| [Int 0 ] |]
  val "CSharp" [e| [Int 1 ] |]
  val "D"      [e| [Int 2 ] |]
  val "DSharp" [e| [Int 3 ] |]
  val "E"      [e| [Int 4 ] |]
  val "F"      [e| [Int 5 ] |]
  val "FSharp" [e| [Int 6 ] |]
  val "G"      [e| [Int 7 ] |]
  val "GSharp" [e| [Int 8 ] |]
  val "A"      [e| [Int 9 ] |]
  val "ASharp" [e| [Int 10] |]
  val "B"      [e| [Int 11] |]

dtaEnum "Tonality" eosreb $ do
  val "Major" [e| [Int 0] |]
  val "Minor" [e| [Int 1] |]

dtaEnum "AnimTempo" eosreb $ do
  val "KTempoSlow"   [e| [Key "kTempoSlow"  ] |]
  val "KTempoMedium" [e| [Key "kTempoMedium"] |]
  val "KTempoFast"   [e| [Key "kTempoFast"  ] |]

dtaRecord "DrumSounds" eosr $ do
  req "seqs" "seqs" [t| [T.Text] |] [e| chunksParens $ chunksList chunksKey |]

channelList :: ChunkFormat [Integer]
channelList = ChunkFormat
  { toChunks = toChunks fmt
  , fromChunks = fromChunks fmt <|> fmap (: []) (fromChunks fmt')
    <|> expected "a number or a list of numbers"
  } where fmt  = chunksParens (format :: ChunkFormat [Integer])
          fmt' = format :: ChunkFormat Integer

dtaRecord "Song" eosr $ do
  req "songName" "name" [t| T.Text |] [e| chunksString |]
  opt "tracksCount" "tracks_count" [t| Maybe [Integer] |] [e| Nothing |] [e| chunksMaybe $ chunksParens format |]
  req "tracks" "tracks" [t| Dict [Integer] |] [e| chunksParens $ chunksDict channelList |]
  opt "vocalParts" "vocal_parts" [t| Maybe Integer |] [e| Nothing |] [e| format |]
  req "pans" "pans" [t| [Float] |] [e| chunksParens format |]
  req "vols" "vols" [t| [Float] |] [e| chunksParens format |]
  req "cores" "cores" [t| [Integer] |] [e| chunksParens format |]
  req "drumSolo" "drum_solo" [t| DrumSounds |] [e| format |]
  req "drumFreestyle" "drum_freestyle" [t| DrumSounds |] [e| format |]
  opt "crowdChannels" "crowd_channels" [t| Maybe [Integer] |] [e| Nothing |] [e| format |]
  opt "hopoThreshold" "hopo_threshold" [t| Maybe Integer |] [e| Nothing |] [e| format |]
  opt "muteVolume" "mute_volume" [t| Maybe Integer |] [e| Nothing |] [e| format |]
  opt "muteVolumeVocals" "mute_volume_vocals" [t| Maybe Integer |] [e| Nothing |] [e| format |]
  -- seen in magma v1 / rb2:
  opt "midiFile" "midi_file" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe chunksString |]

chunksStringOrKey :: ChunkFormat T.Text
chunksStringOrKey = ChunkFormat
  { toChunks   = toChunks chunksString
  , fromChunks = fromChunks chunksString <|> fromChunks chunksKey <|> expected "a string or keyword"
  }

dtaRecord "SongPackage" eosr $ do
  req "name" "name" [t| T.Text |] [e| chunksString |]
  req "artist" "artist" [t| T.Text |] [e| chunksString |]
  req "master" "master" [t| Bool |] [e| format |]
  req "songId" "song_id" [t| Either Integer T.Text |] [e| chunksEither format chunksKey |]
  req "song" "song" [t| Song |] [e| format |]
  opt "bank" "bank" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe chunksStringOrKey |]
  opt "drumBank" "drum_bank" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe chunksStringOrKey |]
  req "animTempo" "anim_tempo" [t| Either AnimTempo Integer |] [e| format |]
  opt "bandFailCue" "band_fail_cue" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe chunksStringOrKey |]
  req "songScrollSpeed" "song_scroll_speed" [t| Integer |] [e| format |]
  req "preview" "preview" [t| (Integer, Integer) |] [e| format |]
  req "songLength" "song_length" [t| Integer |] [e| format |]
  req "rank" "rank" [t| Dict Integer |] [e| format |]
  opt "solo" "solo" [t| Maybe [T.Text] |] [e| Nothing |] [e| chunksMaybe $ chunksParens $ chunksList chunksKey |]
  req "songFormat" "format" [t| Integer |] [e| format |]
  req "version" "version" [t| Integer |] [e| format |]
  req "gameOrigin" "game_origin" [t| T.Text |] [e| chunksKey |]
  req "rating" "rating" [t| Integer |] [e| format |]
  req "genre" "genre" [t| T.Text |] [e| chunksKey |]
  opt "subGenre" "sub_genre" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe chunksKey |]
  req "vocalGender" "vocal_gender" [t| Gender |] [e| format |]
  opt "shortVersion" "short_version" [t| Maybe Integer |] [e| Nothing |] [e| format |]
  req "yearReleased" "year_released" [t| Integer |] [e| format |]
  opt "albumArt" "album_art" [t| Maybe Bool |] [e| Nothing |] [e| format |]
  opt "albumName" "album_name" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe chunksString |]
  opt "albumTrackNumber" "album_track_number" [t| Maybe Integer |] [e| Nothing |] [e| format |]
  opt "vocalTonicNote" "vocal_tonic_note" [t| Maybe Pitch |] [e| Nothing |] [e| format |]
  opt "songTonality" "song_tonality" [t| Maybe Tonality |] [e| Nothing |] [e| format |]
  opt "songKey" "song_key" [t| Maybe Pitch |] [e| Nothing |] [e| format |]
  opt "tuningOffsetCents" "tuning_offset_cents" [t| Maybe Float |] [e| Nothing |] [e| format |]
  opt "realGuitarTuning" "real_guitar_tuning" [t| Maybe [Integer] |] [e| Nothing |] [e| chunksMaybe $ chunksParens format |]
  opt "realBassTuning" "real_bass_tuning" [t| Maybe [Integer] |] [e| Nothing |] [e| chunksMaybe $ chunksParens format |]
  opt "guidePitchVolume" "guide_pitch_volume" [t| Maybe Float |] [e| Nothing |] [e| format |]
  opt "encoding" "encoding" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe chunksKey |]
  -- seen in magma v1 / rb2:
  opt "context" "context" [t| Maybe Integer |] [e| Nothing |] [e| format |]
  opt "decade" "decade" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe chunksKey |]
  opt "downloaded" "downloaded" [t| Maybe Bool |] [e| Nothing |] [e| format |]
  opt "basePoints" "base_points" [t| Maybe Integer |] [e| Nothing |] [e| format |]
