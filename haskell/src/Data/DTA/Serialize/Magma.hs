{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.DTA.Serialize.Magma where

import           Data.DTA
import           Data.DTA.Serialize2
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import           JSONData            (eitherCodec)

dtaRecord "Metadata" eosr $ do
  req "songName" "song_name" [t| T.Text |] [e| single chunkString |]
  req "artistName" "artist_name" [t| T.Text |] [e| single chunkString |]
  req "genre" "genre" [t| T.Text |] [e| single chunkKey |]
  req "subGenre" "sub_genre" [t| T.Text |] [e| single chunkKey |]
  req "yearReleased" "year_released" [t| Integer |] [e| stackChunks |]
  req "albumName" "album_name" [t| T.Text |] [e| single chunkString |]
  req "author" "author" [t| T.Text |] [e| single chunkString |]
  req "releaseLabel" "release_label" [t| T.Text |] [e| single chunkString |]
  req "country" "country" [t| T.Text |] [e| single chunkKey |]
  req "price" "price" [t| Integer |] [e| stackChunks |]
  req "trackNumber" "track_number" [t| Integer |] [e| stackChunks |]
  req "hasAlbum" "has_album" [t| Bool |] [e| stackChunks |]

dtaRecord "AudioFile" eosr $ do
  req "audioEnabled" "enabled" [t| Bool |] [e| stackChunks |]
  req "channels" "channels" [t| Integer |] [e| stackChunks |]
  req "pan" "pan" [t| [Float] |] [e| stackChunks |]
  req "vol" "vol" [t| [Float] |] [e| stackChunks |]
  req "audioFile" "file" [t| T.Text |] [e| single chunkString |]

dtaRecord "DryVoxPart" eosr $ do
  req "dryVoxFile" "file" [t| T.Text |] [e| single chunkString |]
  req "dryVoxEnabled" "enabled" [t| Bool |] [e| stackChunks |]

dtaRecord "DryVox" eosr $ do
  opt "dryVoxFileRB2" "file" [t| Maybe T.Text |] [e| Nothing |] [e| chunksMaybe $ single chunkString |]
  req "part0" "part0" [t| DryVoxPart |] [e| stackChunks |]
  req "part1" "part1" [t| DryVoxPart |] [e| stackChunks |]
  req "part2" "part2" [t| DryVoxPart |] [e| stackChunks |]
  req "tuningOffsetCents" "tuning_offset_cents" [t| Float |] [e| stackChunks |]

data AutogenTheme
  = DefaultTheme
  | AggressiveMetal
  | ArenaRock
  | DarkHeavyRock
  | DustyVintage
  | EdgyProgRock
  | FeelGoodPopRock
  | GaragePunkRock
  | PsychJamRock
  | SlowJam
  | SynthPop
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackChunk AutogenTheme where
  stackChunk = dtaEnum "AutogenTheme" $ \case
    DefaultTheme -> String ""
    theme        -> String $ T.pack (show theme) <> ".rbtheme"
instance StackChunks AutogenTheme

dtaRecord "Midi" eosr $ do
  req "midiFile" "file" [t| T.Text |] [e| single chunkString |]
  req "autogenTheme" "autogen_theme" [t| Either AutogenTheme T.Text |] [e| eitherCodec stackChunks $ single chunkString |]

data DrumLayout
  = Kit
  | KitSnare
  | KitKick
  | KitKickSnare
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackChunk DrumLayout where
  stackChunk = dtaEnum "DrumLayout" $ \case
    Kit          -> Key "drum_layout_kit"
    KitSnare     -> Key "drum_layout_kit_snare"
    KitKick      -> Key "drum_layout_kit_kick"
    KitKickSnare -> Key "drum_layout_kit_kick_snare"
instance StackChunks DrumLayout

dtaRecord "Tracks" eosr $ do
  req "drumLayout" "drum_layout" [t| DrumLayout |] [e| stackChunks |]
  req "drumKit" "drum_kit" [t| AudioFile |] [e| stackChunks |]
  req "drumKick" "drum_kick" [t| AudioFile |] [e| stackChunks |]
  req "drumSnare" "drum_snare" [t| AudioFile |] [e| stackChunks |]
  req "bass" "bass" [t| AudioFile |] [e| stackChunks |]
  req "guitar" "guitar" [t| AudioFile |] [e| stackChunks |]
  req "vocals" "vocals" [t| AudioFile |] [e| stackChunks |]
  req "keys" "keys" [t| AudioFile |] [e| stackChunks |]
  req "backing" "backing" [t| AudioFile |] [e| stackChunks |]

data Percussion
  = Tambourine
  | Cowbell
  | Handclap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackChunk Percussion where
  stackChunk = dtaEnum "Percussion" $ \case
    Tambourine -> Key "tambourine"
    Cowbell    -> Key "cowbell"
    Handclap   -> Key "handclap"
instance StackChunks Percussion

data Gender = Male | Female
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackChunk Gender where
  stackChunk = dtaEnum "Gender" $ \case
    Male   -> Key "male"
    Female -> Key "female"
instance StackChunks Gender

dtaRecord "Languages" eosr $ do
  opt "english" "english" [t| Maybe Bool |] [e| Nothing |] [e| stackChunks |]
  opt "french" "french" [t| Maybe Bool |] [e| Nothing |] [e| stackChunks |]
  opt "italian" "italian" [t| Maybe Bool |] [e| Nothing |] [e| stackChunks |]
  opt "spanish" "spanish" [t| Maybe Bool |] [e| Nothing |] [e| stackChunks |]
  opt "german" "german" [t| Maybe Bool |] [e| Nothing |] [e| stackChunks |]
  opt "japanese" "japanese" [t| Maybe Bool |] [e| Nothing |] [e| stackChunks |]

dtaRecord "AlbumArt" eosr $ do
  req "albumArtFile" "file" [t| T.Text |] [e| single chunkString |]

dtaRecord "Gamedata" eosr $ do
  req "previewStartMs" "preview_start_ms" [t| Integer |] [e| stackChunks |]
  -- ranks: 1 is no dots, 7 is devils
  req "rankGuitar" "rank_guitar" [t| Integer |] [e| stackChunks |]
  req "rankBass" "rank_bass" [t| Integer |] [e| stackChunks |]
  req "rankDrum" "rank_drum" [t| Integer |] [e| stackChunks |]
  req "rankVocals" "rank_vocals" [t| Integer |] [e| stackChunks |]
  req "rankKeys" "rank_keys" [t| Integer |] [e| stackChunks |]
  req "rankProKeys" "rank_pro_keys" [t| Integer |] [e| stackChunks |]
  req "rankBand" "rank_band" [t| Integer |] [e| stackChunks |]
  -- scroll speed: normal = 2300, fast = 2000
  req "vocalScrollSpeed" "vocal_scroll_speed" [t| Integer |] [e| stackChunks |]
  -- Slow (under 100bpm) = 16. Medium (100-160bpm) = 32. Fast (over 160bpm) = 64.
  req "animTempo" "anim_tempo" [t| Integer |] [e| stackChunks |]
  req "vocalGender" "vocal_gender" [t| Gender |] [e| stackChunks |]
  req "vocalPercussion" "vocal_percussion" [t| Percussion |] [e| stackChunks |]
  req "vocalParts" "vocal_parts" [t| Integer |] [e| stackChunks |]
  req "guidePitchVolume" "guide_pitch_volume" [t| Float |] [e| stackChunks |]

dtaRecord "Project" eosr $ do
  req "toolVersion" "tool_version" [t| T.Text |] [e| single chunkString |]
  req "projectVersion" "project_version" [t| Integer |] [e| stackChunks |]
  req "metadata" "metadata" [t| Metadata |] [e| stackChunks |]
  req "gamedata" "gamedata" [t| Gamedata |] [e| stackChunks |]
  req "languages" "languages" [t| Languages |] [e| stackChunks |]
  req "destinationFile" "destination_file" [t| T.Text |] [e| single chunkString |]
  req "midi" "midi" [t| Midi |] [e| stackChunks |]
  req "dryVox" "dry_vox" [t| DryVox |] [e| stackChunks |]
  req "albumArt" "album_art" [t| AlbumArt |] [e| stackChunks |]
  req "tracks" "tracks" [t| Tracks |] [e| stackChunks |]

dtaRecord "RBProj" eosr $ do
  req "project" "project" [t| Project |] [e| stackChunks |]
