{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.DTA.Serialize.Magma where

import           Control.Monad.Codec      ((=.))
import           Control.Monad.Codec.Onyx
import           Data.DTA
import           Data.DTA.Serialize
import qualified Data.Text                as T

data Metadata = Metadata
  { songName     :: T.Text
  , artistName   :: T.Text
  , genre        :: T.Text
  , subGenre     :: T.Text
  , yearReleased :: Integer
  , albumName    :: T.Text
  , author       :: T.Text
  , releaseLabel :: T.Text
  , country      :: T.Text
  , price        :: Integer
  , trackNumber  :: Integer
  , hasAlbum     :: Bool
  } deriving (Eq, Ord, Show)

instance StackChunks Metadata where
  stackChunks = asStrictAssoc "Metadata" $ do
    songName     <- songName     =. req "song_name"     (single chunkString)
    artistName   <- artistName   =. req "artist_name"   (single chunkString)
    genre        <- genre        =. req "genre"         (single chunkSym)
    subGenre     <- subGenre     =. req "sub_genre"     (single chunkSym)
    yearReleased <- yearReleased =. req "year_released" stackChunks
    albumName    <- albumName    =. req "album_name"    (single chunkString)
    author       <- author       =. req "author"        (single chunkString)
    releaseLabel <- releaseLabel =. req "release_label" (single chunkString)
    country      <- country      =. req "country"       (single chunkSym)
    price        <- price        =. req "price"         stackChunks
    trackNumber  <- trackNumber  =. req "track_number"  stackChunks
    hasAlbum     <- hasAlbum     =. req "has_album"     stackChunks
    return Metadata{..}

data AudioFile = AudioFile
  { audioEnabled :: Bool
  , channels     :: Integer
  , pan          :: [Float]
  , vol          :: [Float]
  , audioFile    :: T.Text
  } deriving (Eq, Ord, Show)

instance StackChunks AudioFile where
  stackChunks = asStrictAssoc "AudioFile" $ do
    audioEnabled <- audioEnabled =. req "enabled"  stackChunks
    channels     <- channels     =. req "channels" stackChunks
    pan          <- pan          =. req "pan"      stackChunks
    vol          <- vol          =. req "vol"      stackChunks
    audioFile    <- audioFile    =. req "file"     (single chunkString)
    return AudioFile{..}

data DryVoxPart = DryVoxPart
  { dryVoxFile    :: T.Text
  , dryVoxEnabled :: Bool
  } deriving (Eq, Ord, Show)

instance StackChunks DryVoxPart where
  stackChunks = asStrictAssoc "DryVoxPart" $ do
    dryVoxFile    <- dryVoxFile    =. req "file"    (single chunkString)
    dryVoxEnabled <- dryVoxEnabled =. req "enabled" stackChunks
    return DryVoxPart{..}

data DryVox = DryVox
  { dryVoxFileRB2     :: Maybe T.Text
  , part0             :: DryVoxPart
  , part1             :: DryVoxPart
  , part2             :: DryVoxPart
  , tuningOffsetCents :: Float
  } deriving (Eq, Ord, Show)

instance StackChunks DryVox where
  stackChunks = asStrictAssoc "DryVox" $ do
    dryVoxFileRB2     <- dryVoxFileRB2     =. opt Nothing "file"                (chunksMaybe $ single chunkString)
    part0             <- part0             =. req         "part0"               stackChunks
    part1             <- part1             =. req         "part1"               stackChunks
    part2             <- part2             =. req         "part2"               stackChunks
    tuningOffsetCents <- tuningOffsetCents =. req         "tuning_offset_cents" stackChunks
    return DryVox{..}

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
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackChunk AutogenTheme where
  stackChunk = enumCodecFull "AutogenTheme" $ \case
    DefaultTheme -> is (String "") |?> is (String "Default.rbtheme")
    theme        -> is $ String $ T.pack (show theme) <> ".rbtheme"
instance StackChunks AutogenTheme

data Midi = Midi
  { midiFile     :: T.Text
  , autogenTheme :: Either AutogenTheme T.Text
  } deriving (Eq, Ord, Show)

instance StackChunks Midi where
  stackChunks = asStrictAssoc "Midi" $ do
    midiFile     <- midiFile     =. req "file"          (single chunkString)
    autogenTheme <- autogenTheme =. req "autogen_theme" (eitherCodec stackChunks $ single chunkString)
    return Midi{..}

data DrumLayout
  = Kit
  | KitSnare
  | KitKick
  | KitKickSnare
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackChunk DrumLayout where
  stackChunk = enumCodec "DrumLayout" $ \case
    Kit          -> Sym "drum_layout_kit"
    KitSnare     -> Sym "drum_layout_kit_snare"
    KitKick      -> Sym "drum_layout_kit_kick"
    KitKickSnare -> Sym "drum_layout_kit_kick_snare"
instance StackChunks DrumLayout

data Tracks = Tracks
  { drumLayout :: DrumLayout
  , drumKit    :: AudioFile
  , drumKick   :: AudioFile
  , drumSnare  :: AudioFile
  , bass       :: AudioFile
  , guitar     :: AudioFile
  , vocals     :: AudioFile
  , keys       :: AudioFile
  , backing    :: AudioFile
  } deriving (Eq, Ord, Show)

instance StackChunks Tracks where
  stackChunks = asStrictAssoc "Tracks" $ do
    drumLayout <- drumLayout =. req "drum_layout" stackChunks
    drumKit    <- drumKit    =. req "drum_kit"    stackChunks
    drumKick   <- drumKick   =. req "drum_kick"   stackChunks
    drumSnare  <- drumSnare  =. req "drum_snare"  stackChunks
    bass       <- bass       =. req "bass"        stackChunks
    guitar     <- guitar     =. req "guitar"      stackChunks
    vocals     <- vocals     =. req "vocals"      stackChunks
    keys       <- keys       =. req "keys"        stackChunks
    backing    <- backing    =. req "backing"     stackChunks
    return Tracks{..}

data Percussion
  = Tambourine
  | Cowbell
  | Handclap
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackChunk Percussion where
  stackChunk = enumCodec "Percussion" $ \case
    Tambourine -> Sym "tambourine"
    Cowbell    -> Sym "cowbell"
    Handclap   -> Sym "handclap"
instance StackChunks Percussion

data Gender = Male | Female
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackChunk Gender where
  stackChunk = enumCodec "Gender" $ \case
    Male   -> Sym "male"
    Female -> Sym "female"
instance StackChunks Gender

data Languages = Languages
  { english  :: Maybe Bool
  , french   :: Maybe Bool
  , italian  :: Maybe Bool
  , spanish  :: Maybe Bool
  , german   :: Maybe Bool
  , japanese :: Maybe Bool
  } deriving (Eq, Ord, Show)

instance StackChunks Languages where
  stackChunks = asStrictAssoc "Languages" $ do
    english  <- english  =. opt Nothing "english"  stackChunks
    french   <- french   =. opt Nothing "french"   stackChunks
    italian  <- italian  =. opt Nothing "italian"  stackChunks
    spanish  <- spanish  =. opt Nothing "spanish"  stackChunks
    german   <- german   =. opt Nothing "german"   stackChunks
    japanese <- japanese =. opt Nothing "japanese" stackChunks
    return Languages{..}

newtype AlbumArt = AlbumArt
  { albumArtFile :: T.Text
  } deriving (Eq, Ord, Show)

instance StackChunks AlbumArt where
  stackChunks = asStrictAssoc "AlbumArt" $ do
    albumArtFile <- albumArtFile =. req "file" (single chunkString)
    return AlbumArt{..}

data Gamedata = Gamedata
  { previewStartMs   :: Integer
  -- | tiers (not really ranks): 1 is no dots, 7 is devils
  , rankGuitar       :: Integer
  , rankBass         :: Integer
  , rankDrum         :: Integer
  , rankVocals       :: Integer
  , rankKeys         :: Integer
  , rankProKeys      :: Integer
  , rankBand         :: Integer
  -- | scroll speed: normal = 2300, fast = 2000
  , vocalScrollSpeed :: Integer
  -- | Slow (under 100bpm) = 16. Medium (100-160bpm) = 32. Fast (over 160bpm) = 64.
  , animTempo        :: Integer
  , vocalGender      :: Gender
  , vocalPercussion  :: Percussion
  , vocalParts       :: Integer
  , guidePitchVolume :: Float
  } deriving (Eq, Ord, Show)

instance StackChunks Gamedata where
  stackChunks = asStrictAssoc "Gamedata" $ do
    previewStartMs   <- previewStartMs   =. req "preview_start_ms"   stackChunks
    rankGuitar       <- rankGuitar       =. req "rank_guitar"        stackChunks
    rankBass         <- rankBass         =. req "rank_bass"          stackChunks
    rankDrum         <- rankDrum         =. req "rank_drum"          stackChunks
    rankVocals       <- rankVocals       =. req "rank_vocals"        stackChunks
    rankKeys         <- rankKeys         =. req "rank_keys"          stackChunks
    rankProKeys      <- rankProKeys      =. req "rank_pro_keys"      stackChunks
    rankBand         <- rankBand         =. req "rank_band"          stackChunks
    vocalScrollSpeed <- vocalScrollSpeed =. req "vocal_scroll_speed" stackChunks
    animTempo        <- animTempo        =. req "anim_tempo"         stackChunks
    vocalGender      <- vocalGender      =. req "vocal_gender"       stackChunks
    vocalPercussion  <- vocalPercussion  =. req "vocal_percussion"   stackChunks
    vocalParts       <- vocalParts       =. req "vocal_parts"        stackChunks
    guidePitchVolume <- guidePitchVolume =. req "guide_pitch_volume" stackChunks
    return Gamedata{..}

data Project = Project
  { toolVersion     :: T.Text
  , projectVersion  :: Integer
  , metadata        :: Metadata
  , gamedata        :: Gamedata
  , languages       :: Languages
  , destinationFile :: T.Text
  , midi            :: Midi
  , dryVox          :: DryVox
  , albumArt        :: AlbumArt
  , tracks          :: Tracks
  } deriving (Eq, Ord, Show)

instance StackChunks Project where
  stackChunks = asStrictAssoc "Project" $ do
    toolVersion     <- toolVersion     =. req "tool_version"     (single chunkString)
    projectVersion  <- projectVersion  =. req "project_version"  stackChunks
    metadata        <- metadata        =. req "metadata"         stackChunks
    gamedata        <- gamedata        =. req "gamedata"         stackChunks
    languages       <- languages       =. req "languages"        stackChunks
    destinationFile <- destinationFile =. req "destination_file" (single chunkString)
    midi            <- midi            =. req "midi"             stackChunks
    dryVox          <- dryVox          =. req "dry_vox"          stackChunks
    albumArt        <- albumArt        =. req "album_art"        stackChunks
    tracks          <- tracks          =. req "tracks"           stackChunks
    return Project{..}

newtype RBProj = RBProj
  { project :: Project
  } deriving (Eq, Ord, Show)

instance StackChunks RBProj where
  stackChunks = asStrictAssoc "RBProj" $ do
    project <- project =. req "project" stackChunks
    return RBProj{..}
