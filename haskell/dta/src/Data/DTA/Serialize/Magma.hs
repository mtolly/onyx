{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Data.DTA.Serialize.Magma where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad ((>=>))

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

import Data.DTA
import Data.DTA.Serialize



data RBProj = RBProj { project :: Project } deriving (Eq, Ord, Read, Show)

instance ToChunks RBProj where { toChunks x = makeDict $ Dict $ Map.fromList $ [("project", toChunks $ project x)] }

instance FromChunks RBProj where { fromChunks = getDict >=> \d -> RBProj <$> (dictLookup "project" d >>= fromChunks) }

data Project = Project { toolVersion :: String, projectVersion :: Integer, metadata :: Metadata, gamedata :: Gamedata, languages :: Languages, destinationFile :: String, midi :: Midi, dryVox :: DryVox, albumArt :: AlbumArt, tracks :: Tracks } deriving (Eq, Ord, Read, Show)

instance ToChunks Project where { toChunks x = makeDict $ Dict $ Map.fromList $ [("tool_version", toChunks $ toolVersion x)] ++ [("project_version", toChunks $ projectVersion x)] ++ [("metadata", toChunks $ metadata x)] ++ [("gamedata", toChunks $ gamedata x)] ++ [("languages", toChunks $ languages x)] ++ [("destination_file", toChunks $ destinationFile x)] ++ [("midi", toChunks $ midi x)] ++ [("dry_vox", toChunks $ dryVox x)] ++ [("album_art", toChunks $ albumArt x)] ++ [("tracks", toChunks $ tracks x)] }

instance FromChunks Project where { fromChunks = getDict >=> \d -> Project <$> (dictLookup "tool_version" d >>= fromChunks) <*> (dictLookup "project_version" d >>= fromChunks) <*> (dictLookup "metadata" d >>= fromChunks) <*> (dictLookup "gamedata" d >>= fromChunks) <*> (dictLookup "languages" d >>= fromChunks) <*> (dictLookup "destination_file" d >>= fromChunks) <*> (dictLookup "midi" d >>= fromChunks) <*> (dictLookup "dry_vox" d >>= fromChunks) <*> (dictLookup "album_art" d >>= fromChunks) <*> (dictLookup "tracks" d >>= fromChunks) }

data AlbumArt = AlbumArt { albumArtFile :: String } deriving (Eq, Ord, Read, Show)

instance ToChunks AlbumArt where { toChunks x = makeDict $ Dict $ Map.fromList $ [("file", toChunks $ albumArtFile x)] }

instance FromChunks AlbumArt where { fromChunks = getDict >=> \d -> AlbumArt <$> (dictLookup "file" d >>= fromChunks) }

data Languages = Languages { english :: Bool, french :: Bool, italian :: Bool, spanish :: Bool, german :: Bool, japanese :: Bool } deriving (Eq, Ord, Read, Show)

instance ToChunks Languages where { toChunks x = makeDict $ Dict $ Map.fromList $ [("english", toChunks $ english x)] ++ [("french", toChunks $ french x)] ++ [("italian", toChunks $ italian x)] ++ [("spanish", toChunks $ spanish x)] ++ [("german", toChunks $ german x)] ++ [("japanese", toChunks $ japanese x)] }

instance FromChunks Languages where { fromChunks = getDict >=> \d -> Languages <$> (dictLookup "english" d >>= fromChunks) <*> (dictLookup "french" d >>= fromChunks) <*> (dictLookup "italian" d >>= fromChunks) <*> (dictLookup "spanish" d >>= fromChunks) <*> (dictLookup "german" d >>= fromChunks) <*> (dictLookup "japanese" d >>= fromChunks) }

data Gamedata = Gamedata { previewStartMs :: Integer, rankGuitar :: Integer {- ^ 1 is no dots, 7 is devils. -}, rankBass :: Integer, rankDrum :: Integer, rankVocals :: Integer, rankKeys :: Integer, rankProKeys :: Integer, rankBand :: Integer, vocalScrollSpeed :: Integer {- ^ Normal = 2300. Fast = 2000. -}, animTempo :: Integer {- ^ Slow (under 100bpm) = 16. Medium (100-160bpm) = 32. Fast (over 160bpm) = 64. -}, vocalGender :: Gender, vocalPercussion :: Percussion, vocalParts :: Integer, guidePitchVolume :: Float } deriving (Eq, Ord, Read, Show)

instance ToChunks Gamedata where { toChunks x = makeDict $ Dict $ Map.fromList $ [("preview_start_ms", toChunks $ previewStartMs x)] ++ [("rank_guitar", toChunks $ rankGuitar x)] ++ [("rank_bass", toChunks $ rankBass x)] ++ [("rank_drum", toChunks $ rankDrum x)] ++ [("rank_vocals", toChunks $ rankVocals x)] ++ [("rank_keys", toChunks $ rankKeys x)] ++ [("rank_pro_keys", toChunks $ rankProKeys x)] ++ [("rank_band", toChunks $ rankBand x)] ++ [("vocal_scroll_speed", toChunks $ vocalScrollSpeed x)] ++ [("anim_tempo", toChunks $ animTempo x)] ++ [("vocal_gender", toChunks $ vocalGender x)] ++ [("vocal_percussion", toChunks $ vocalPercussion x)] ++ [("vocal_parts", toChunks $ vocalParts x)] ++ [("guide_pitch_volume", toChunks $ guidePitchVolume x)] }

instance FromChunks Gamedata where { fromChunks = getDict >=> \d -> Gamedata <$> (dictLookup "preview_start_ms" d >>= fromChunks) <*> (dictLookup "rank_guitar" d >>= fromChunks) <*> (dictLookup "rank_bass" d >>= fromChunks) <*> (dictLookup "rank_drum" d >>= fromChunks) <*> (dictLookup "rank_vocals" d >>= fromChunks) <*> (dictLookup "rank_keys" d >>= fromChunks) <*> (dictLookup "rank_pro_keys" d >>= fromChunks) <*> (dictLookup "rank_band" d >>= fromChunks) <*> (dictLookup "vocal_scroll_speed" d >>= fromChunks) <*> (dictLookup "anim_tempo" d >>= fromChunks) <*> (dictLookup "vocal_gender" d >>= fromChunks) <*> (dictLookup "vocal_percussion" d >>= fromChunks) <*> (dictLookup "vocal_parts" d >>= fromChunks) <*> (dictLookup "guide_pitch_volume" d >>= fromChunks) }

data Gender = Male | Female deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToChunks Gender where { toChunks Male = [Key "male"]; toChunks Female = [Key "female"] }

instance FromChunks Gender where { fromChunks [Key "male"] = Right Male; fromChunks [Key "female"] = Right Female; fromChunks cs = Left $ "Couldn't read as Gender: " ++ show cs }

data Percussion = Tambourine | Cowbell | Handclap deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToChunks Percussion where { toChunks Tambourine = [Key "tambourine"]; toChunks Cowbell = [Key "cowbell"]; toChunks Handclap = [Key "handclap"] }

instance FromChunks Percussion where { fromChunks [Key "tambourine"] = Right Tambourine; fromChunks [Key "cowbell"] = Right Cowbell; fromChunks [Key "handclap"] = Right Handclap; fromChunks cs = Left $ "Couldn't read as Percussion: " ++ show cs }

data Metadata = Metadata { songName :: String, artistName :: String, genre :: Keyword, subGenre :: Keyword, yearReleased :: Integer, albumName :: String, author :: String, releaseLabel :: String, country :: Keyword, price :: Integer, trackNumber :: Integer, hasAlbum :: Bool } deriving (Eq, Ord, Read, Show)

instance ToChunks Metadata where { toChunks x = makeDict $ Dict $ Map.fromList $ [("song_name", toChunks $ songName x)] ++ [("artist_name", toChunks $ artistName x)] ++ [("genre", toChunks $ genre x)] ++ [("sub_genre", toChunks $ subGenre x)] ++ [("year_released", toChunks $ yearReleased x)] ++ [("album_name", toChunks $ albumName x)] ++ [("author", toChunks $ author x)] ++ [("release_label", toChunks $ releaseLabel x)] ++ [("country", toChunks $ country x)] ++ [("price", toChunks $ price x)] ++ [("track_number", toChunks $ trackNumber x)] ++ [("has_album", toChunks $ hasAlbum x)] }

instance FromChunks Metadata where { fromChunks = getDict >=> \d -> Metadata <$> (dictLookup "song_name" d >>= fromChunks) <*> (dictLookup "artist_name" d >>= fromChunks) <*> (dictLookup "genre" d >>= fromChunks) <*> (dictLookup "sub_genre" d >>= fromChunks) <*> (dictLookup "year_released" d >>= fromChunks) <*> (dictLookup "album_name" d >>= fromChunks) <*> (dictLookup "author" d >>= fromChunks) <*> (dictLookup "release_label" d >>= fromChunks) <*> (dictLookup "country" d >>= fromChunks) <*> (dictLookup "price" d >>= fromChunks) <*> (dictLookup "track_number" d >>= fromChunks) <*> (dictLookup "has_album" d >>= fromChunks) }

data Midi = Midi { midiFile :: String, autogenTheme :: Either AutogenTheme String } deriving (Eq, Ord, Read, Show)

instance ToChunks Midi where { toChunks x = makeDict $ Dict $ Map.fromList $ [("file", toChunks $ midiFile x)] ++ [("autogen_theme", toChunks $ autogenTheme x)] }

instance FromChunks Midi where { fromChunks = getDict >=> \d -> Midi <$> (dictLookup "file" d >>= fromChunks) <*> (dictLookup "autogen_theme" d >>= fromChunks) }

data AutogenTheme = DefaultTheme | AggressiveMetal | ArenaRock | DarkHeavyRock | DustyVintage | EdgyProgRock | FeelGoodPopRock | GaragePunkRock | PsychJamRock | SlowJam | SynthPop deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToChunks AutogenTheme where { toChunks DefaultTheme = [String ""]; toChunks AggressiveMetal = [String "AggressiveMetal.rbtheme"]; toChunks ArenaRock = [String "ArenaRock.rbtheme"]; toChunks DarkHeavyRock = [String "DarkHeavyRock.rbtheme"]; toChunks DustyVintage = [String "DustyVintage.rbtheme"]; toChunks EdgyProgRock = [String "EdgyProgRock.rbtheme"]; toChunks FeelGoodPopRock = [String "FeelGoodPopRock.rbtheme"]; toChunks GaragePunkRock = [String "GaragePunkRock.rbtheme"]; toChunks PsychJamRock = [String "PsychJamRock.rbtheme"]; toChunks SlowJam = [String "SlowJam.rbtheme"]; toChunks SynthPop = [String "SynthPop.rbtheme"] }

instance FromChunks AutogenTheme where { fromChunks [String ""] = Right DefaultTheme; fromChunks [String "AggressiveMetal.rbtheme"] = Right AggressiveMetal; fromChunks [String "ArenaRock.rbtheme"] = Right ArenaRock; fromChunks [String "DarkHeavyRock.rbtheme"] = Right DarkHeavyRock; fromChunks [String "DustyVintage.rbtheme"] = Right DustyVintage; fromChunks [String "EdgyProgRock.rbtheme"] = Right EdgyProgRock; fromChunks [String "FeelGoodPopRock.rbtheme"] = Right FeelGoodPopRock; fromChunks [String "GaragePunkRock.rbtheme"] = Right GaragePunkRock; fromChunks [String "PsychJamRock.rbtheme"] = Right PsychJamRock; fromChunks [String "SlowJam.rbtheme"] = Right SlowJam; fromChunks [String "SynthPop.rbtheme"] = Right SynthPop; fromChunks cs = Left $ "Couldn't read as AutogenTheme: " ++ show cs }

data DryVox = DryVox { part0 :: DryVoxPart, part1 :: DryVoxPart, part2 :: DryVoxPart, tuningOffsetCents :: Float } deriving (Eq, Ord, Read, Show)

instance ToChunks DryVox where { toChunks x = makeDict $ Dict $ Map.fromList $ [("part0", toChunks $ part0 x)] ++ [("part1", toChunks $ part1 x)] ++ [("part2", toChunks $ part2 x)] ++ [("tuning_offset_cents", toChunks $ tuningOffsetCents x)] }

instance FromChunks DryVox where { fromChunks = getDict >=> \d -> DryVox <$> (dictLookup "part0" d >>= fromChunks) <*> (dictLookup "part1" d >>= fromChunks) <*> (dictLookup "part2" d >>= fromChunks) <*> (dictLookup "tuning_offset_cents" d >>= fromChunks) }

data DryVoxPart = DryVoxPart { dryVoxFile :: String, dryVoxEnabled :: Bool } deriving (Eq, Ord, Read, Show)

instance ToChunks DryVoxPart where { toChunks x = makeDict $ Dict $ Map.fromList $ [("file", toChunks $ dryVoxFile x)] ++ [("enabled", toChunks $ dryVoxEnabled x)] }

instance FromChunks DryVoxPart where { fromChunks = getDict >=> \d -> DryVoxPart <$> (dictLookup "file" d >>= fromChunks) <*> (dictLookup "enabled" d >>= fromChunks) }

data Tracks = Tracks { drumLayout :: DrumLayout, drumKit :: AudioFile, drumKick :: AudioFile, drumSnare :: AudioFile, bass :: AudioFile, guitar :: AudioFile, vocals :: AudioFile, keys :: AudioFile, backing :: AudioFile } deriving (Eq, Ord, Read, Show)

instance ToChunks Tracks where { toChunks x = makeDict $ Dict $ Map.fromList $ [("drum_layout", toChunks $ drumLayout x)] ++ [("drum_kit", toChunks $ drumKit x)] ++ [("drum_kick", toChunks $ drumKick x)] ++ [("drum_snare", toChunks $ drumSnare x)] ++ [("bass", toChunks $ bass x)] ++ [("guitar", toChunks $ guitar x)] ++ [("vocals", toChunks $ vocals x)] ++ [("keys", toChunks $ keys x)] ++ [("backing", toChunks $ backing x)] }

instance FromChunks Tracks where { fromChunks = getDict >=> \d -> Tracks <$> (dictLookup "drum_layout" d >>= fromChunks) <*> (dictLookup "drum_kit" d >>= fromChunks) <*> (dictLookup "drum_kick" d >>= fromChunks) <*> (dictLookup "drum_snare" d >>= fromChunks) <*> (dictLookup "bass" d >>= fromChunks) <*> (dictLookup "guitar" d >>= fromChunks) <*> (dictLookup "vocals" d >>= fromChunks) <*> (dictLookup "keys" d >>= fromChunks) <*> (dictLookup "backing" d >>= fromChunks) }

data DrumLayout = Kit | KitSnare | KitKick | KitKickSnare deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToChunks DrumLayout where { toChunks Kit = [Key "drum_layout_kit"]; toChunks KitSnare = [Key "drum_layout_kit_snare"]; toChunks KitKick = [Key "drum_layout_kit_kick"]; toChunks KitKickSnare = [Key "drum_layout_kit_kick_snare"] }

instance FromChunks DrumLayout where { fromChunks [Key "drum_layout_kit"] = Right Kit; fromChunks [Key "drum_layout_kit_snare"] = Right KitSnare; fromChunks [Key "drum_layout_kit_kick"] = Right KitKick; fromChunks [Key "drum_layout_kit_kick_snare"] = Right KitKickSnare; fromChunks cs = Left $ "Couldn't read as DrumLayout: " ++ show cs }

data AudioFile = AudioFile { audioEnabled :: Bool, channels :: Integer, pan :: [Float], vol :: [Float], audioFile :: String } deriving (Eq, Ord, Read, Show)

instance ToChunks AudioFile where { toChunks x = makeDict $ Dict $ Map.fromList $ [("enabled", toChunks $ audioEnabled x)] ++ [("channels", toChunks $ channels x)] ++ [("pan", toChunks $ pan x)] ++ [("vol", toChunks $ vol x)] ++ [("file", toChunks $ audioFile x)] }

instance FromChunks AudioFile where { fromChunks = getDict >=> \d -> AudioFile <$> (dictLookup "enabled" d >>= fromChunks) <*> (dictLookup "channels" d >>= fromChunks) <*> (dictLookup "pan" d >>= fromChunks) <*> (dictLookup "vol" d >>= fromChunks) <*> (dictLookup "file" d >>= fromChunks) }
