{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.DTA.Serialize.GH1 where

import           Control.Monad.Codec    ((=.))
import           Data.DTA
import           Data.DTA.Serialize
import           Data.DTA.Serialize.RB3 (AnimTempo)
import qualified Data.Text              as T
import           JSONData               (enumCodec, opt, req)
import           RockBand.Codec         (reprPrefix)

data Song = Song
  { songName    :: T.Text
  , tracks      :: Integer
  , slip_tracks :: [[Integer]]
  , pans        :: [Float]
  , vols        :: [Float]
  , cores       :: [Integer]
  -- TODO (solo (riffs [standard|symph|more]))
  } deriving (Eq, Show)

instance StackChunks Song where
  stackChunks = asWarnAssoc "Song" $ do
    songName      <- songName      =. req "name"        (single chunkString)
    tracks        <- tracks        =. req "tracks"      stackChunks
    slip_tracks   <- slip_tracks   =. req "slip_tracks" (chunksParens $ chunksList $ chunkParens $ chunksList chunkInt)
    pans          <- pans          =. req "pans"        (chunksParens stackChunks)
    vols          <- vols          =. req "vols"        (chunksParens stackChunks)
    cores         <- cores         =. req "cores"       (chunksParens stackChunks)
    return Song{..}

data Character
  = Char_nu_metal
  | Char_metal
  | Char_hiphop
  | Char_classic
  | Char_punk
  | Char_alterna
  deriving (Eq, Show, Enum, Bounded)

instance StackChunk Character where
  stackChunk = enumCodec "Character" $ Sym . reprPrefix "Char_"
instance StackChunks Character

data Guitar
  = Guitar_gibson_flying_v
  | Guitar_gibson_lespaul
  | Guitar_gibson_sg
  deriving (Eq, Show, Enum, Bounded)

instance StackChunk Guitar where
  stackChunk = enumCodec "Guitar" $ Sym . reprPrefix "Guitar_"
instance StackChunks Guitar

data Venue
  = Venue_fest
  | Venue_arena
  | Venue_theatre
  | Venue_big_club
  | Venue_basement
  | Venue_small_club
  deriving (Eq, Show, Enum, Bounded)

instance StackChunk Venue where
  stackChunk = enumCodec "Venue" $ Sym . reprPrefix "Venue_"
instance StackChunks Venue

data Quickplay = Quickplay
  { character :: Either Character T.Text
  , guitar    :: Either Guitar T.Text
  , venue     :: Either Venue T.Text
  } deriving (Eq, Show)

instance StackChunks Quickplay where
  stackChunks = asWarnAssoc "Quickplay" $ do
    character <- character =. req "character" stackChunks
    guitar    <- guitar    =. req "guitar"    stackChunks
    venue     <- venue     =. req "venue"     stackChunks
    return Quickplay{..}

data BandMember
  = KEYBOARD_METAL
  | BASS_METAL
  | DRUMMER_METAL
  | SINGER_FEMALE_METAL
  deriving (Eq, Show, Enum, Bounded)

instance StackChunk BandMember where
  stackChunk = enumCodec "BandMember" $ Sym . T.pack . show
instance StackChunks BandMember

data SongPackage = SongPackage
  { name      :: T.Text
  , artist    :: T.Text
  , song      :: Song
  , band      :: Maybe [Either BandMember T.Text]
  , bank      :: T.Text
  , bpm       :: Integer
  , animTempo :: AnimTempo
  , preview   :: (Integer, Integer)
  , midiFile  :: T.Text
  , quickplay :: Quickplay
  } deriving (Eq, Show)

instance StackChunks SongPackage where
  stackChunks = asWarnAssoc "SongPackage" $ do
    name      <- name      =. req         "name"       (single chunkString)
    artist    <- artist    =. req         "artist"     (single chunkString)
    song      <- song      =. req         "song"       stackChunks
    band      <- band      =. opt Nothing "band"       stackChunks
    bank      <- bank      =. req         "bank"       stackChunks
    bpm       <- bpm       =. req         "bpm"        stackChunks
    animTempo <- animTempo =. req         "anim_tempo" stackChunks
    preview   <- preview   =. req         "preview"    stackChunks
    midiFile  <- midiFile  =. req         "midi_file"  stackChunks
    quickplay <- quickplay =. req         "quickplay"  stackChunks
    return SongPackage{..}
