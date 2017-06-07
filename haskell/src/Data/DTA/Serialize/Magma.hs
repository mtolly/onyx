{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.DTA.Serialize.Magma where

import           Data.DTA
import           Data.DTA.Serialize2
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import           Language.Haskell.TH (stringE)
import JSONData (eitherCodec)

dtaRecord "Metadata" eosr $ do
  req "songName" "song_name" [t| T.Text |] [e| chunksString |]
  req "artistName" "artist_name" [t| T.Text |] [e| chunksString |]
  req "genre" "genre" [t| T.Text |] [e| chunksKey |]
  req "subGenre" "sub_genre" [t| T.Text |] [e| chunksKey |]
  req "yearReleased" "year_released" [t| Integer |] [e| format |]
  req "albumName" "album_name" [t| T.Text |] [e| chunksString |]
  req "author" "author" [t| T.Text |] [e| chunksString |]
  req "releaseLabel" "release_label" [t| T.Text |] [e| chunksString |]
  req "country" "country" [t| T.Text |] [e| chunksKey |]
  req "price" "price" [t| Integer |] [e| format |]
  req "trackNumber" "track_number" [t| Integer |] [e| format |]
  req "hasAlbum" "has_album" [t| Bool |] [e| format |]

dtaRecord "AudioFile" eosr $ do
  req "audioEnabled" "enabled" [t| Bool |] [e| format |]
  req "channels" "channels" [t| Integer |] [e| format |]
  req "pan" "pan" [t| [Float] |] [e| format |]
  req "vol" "vol" [t| [Float] |] [e| format |]
  req "audioFile" "file" [t| T.Text |] [e| chunksString |]

dtaRecord "DryVoxPart" eosr $ do
  req "dryVoxFile" "file" [t| T.Text |] [e| chunksString |]
  req "dryVoxEnabled" "enabled" [t| Bool |] [e| format |]

dtaRecord "DryVox" eosr $ do
  opt "dryVoxFileRB2" "file" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe chunksString |]
  req "part0" "part0" [t| DryVoxPart |] [e| format |]
  req "part1" "part1" [t| DryVoxPart |] [e| format |]
  req "part2" "part2" [t| DryVoxPart |] [e| format |]
  req "tuningOffsetCents" "tuning_offset_cents" [t| Float |] [e| format |]

dtaEnum "AutogenTheme" eosreb $ do
  val "DefaultTheme" [e| [String ""] |]
  let theme s = val s [e| [String ($(stringE s) <> ".rbtheme")] |]
  theme "AggressiveMetal"
  theme "ArenaRock"
  theme "DarkHeavyRock"
  theme "DustyVintage"
  theme "EdgyProgRock"
  theme "FeelGoodPopRock"
  theme "GaragePunkRock"
  theme "PsychJamRock"
  theme "SlowJam"
  theme "SynthPop"

dtaRecord "Midi" eosr $ do
  req "midiFile" "file" [t| T.Text |] [e| chunksString |]
  req "autogenTheme" "autogen_theme" [t| Either AutogenTheme T.Text |] [e| eitherCodec format chunksString |]

dtaEnum "DrumLayout" eosreb $ do
  val "Kit"          [e| [Key "drum_layout_kit"             ] |]
  val "KitSnare"     [e| [Key "drum_layout_kit_snare"       ] |]
  val "KitKick"      [e| [Key "drum_layout_kit_kick"        ] |]
  val "KitKickSnare" [e| [Key "drum_layout_kit_kick_snare"  ] |]

dtaRecord "Tracks" eosr $ do
  req "drumLayout" "drum_layout" [t| DrumLayout |] [e| format |]
  req "drumKit" "drum_kit" [t| AudioFile |] [e| format |]
  req "drumKick" "drum_kick" [t| AudioFile |] [e| format |]
  req "drumSnare" "drum_snare" [t| AudioFile |] [e| format |]
  req "bass" "bass" [t| AudioFile |] [e| format |]
  req "guitar" "guitar" [t| AudioFile |] [e| format |]
  req "vocals" "vocals" [t| AudioFile |] [e| format |]
  req "keys" "keys" [t| AudioFile |] [e| format |]
  req "backing" "backing" [t| AudioFile |] [e| format |]

dtaEnum "Percussion" eosreb $ do
  val "Tambourine" [e| [Key "tambourine"] |]
  val "Cowbell"    [e| [Key "cowbell"   ] |]
  val "Handclap"   [e| [Key "handclap"  ] |]

dtaEnum "Gender" eosreb $ do
  val "Male"   [e| [Key "male"  ] |]
  val "Female" [e| [Key "female"] |]

dtaRecord "Languages" eosr $ do
  opt "english" "english" [t| Maybe Bool |] [e| Nothing |] [e| format |]
  opt "french" "french" [t| Maybe Bool |] [e| Nothing |] [e| format |]
  opt "italian" "italian" [t| Maybe Bool |] [e| Nothing |] [e| format |]
  opt "spanish" "spanish" [t| Maybe Bool |] [e| Nothing |] [e| format |]
  opt "german" "german" [t| Maybe Bool |] [e| Nothing |] [e| format |]
  opt "japanese" "japanese" [t| Maybe Bool |] [e| Nothing |] [e| format |]

dtaRecord "AlbumArt" eosr $ do
  req "albumArtFile" "file" [t| T.Text |] [e| chunksString |]

dtaRecord "Gamedata" eosr $ do
  req "previewStartMs" "preview_start_ms" [t| Integer |] [e| format |]
  -- ranks: 1 is no dots, 7 is devils
  req "rankGuitar" "rank_guitar" [t| Integer |] [e| format |]
  req "rankBass" "rank_bass" [t| Integer |] [e| format |]
  req "rankDrum" "rank_drum" [t| Integer |] [e| format |]
  req "rankVocals" "rank_vocals" [t| Integer |] [e| format |]
  req "rankKeys" "rank_keys" [t| Integer |] [e| format |]
  req "rankProKeys" "rank_pro_keys" [t| Integer |] [e| format |]
  req "rankBand" "rank_band" [t| Integer |] [e| format |]
  -- scroll speed: normal = 2300, fast = 2000
  req "vocalScrollSpeed" "vocal_scroll_speed" [t| Integer |] [e| format |]
  -- Slow (under 100bpm) = 16. Medium (100-160bpm) = 32. Fast (over 160bpm) = 64.
  req "animTempo" "anim_tempo" [t| Integer |] [e| format |]
  req "vocalGender" "vocal_gender" [t| Gender |] [e| format |]
  req "vocalPercussion" "vocal_percussion" [t| Percussion |] [e| format |]
  req "vocalParts" "vocal_parts" [t| Integer |] [e| format |]
  req "guidePitchVolume" "guide_pitch_volume" [t| Float |] [e| format |]

dtaRecord "Project" eosr $ do
  req "toolVersion" "tool_version" [t| T.Text |] [e| chunksString |]
  req "projectVersion" "project_version" [t| Integer |] [e| format |]
  req "metadata" "metadata" [t| Metadata |] [e| format |]
  req "gamedata" "gamedata" [t| Gamedata |] [e| format |]
  req "languages" "languages" [t| Languages |] [e| format |]
  req "destinationFile" "destination_file" [t| T.Text |] [e| chunksString |]
  req "midi" "midi" [t| Midi |] [e| format |]
  req "dryVox" "dry_vox" [t| DryVox |] [e| format |]
  req "albumArt" "album_art" [t| AlbumArt |] [e| format |]
  req "tracks" "tracks" [t| Tracks |] [e| format |]

dtaRecord "RBProj" eosr $ do
  req "project" "project" [t| Project |] [e| format |]
