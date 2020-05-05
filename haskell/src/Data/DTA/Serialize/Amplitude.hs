-- | .moggsong files found in Amplitude (PS3 + PS4)
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.DTA.Serialize.Amplitude where

import           Control.Monad.Codec            (CodecFor (..), fmapArg, (=.))
import           Control.Monad.Trans.Reader     (local)
import           Control.Monad.Trans.StackTrace (SendMessage, mapStackTraceT)
import           Control.Monad.Trans.Writer     (tell)
import           Data.DTA.Base
import           Data.DTA.Serialize
import           Data.Fixed                     (divMod')
import           Data.List.Split                (splitOn)
import           Data.Profunctor                (dimap)
import qualified Data.Text                      as T
import           JSONData                       (expected, opt, req)
import qualified Sound.MIDI.Util                as U
import           Text.Read                      (readMaybe)

-- | Top level
data Song = Song
  { mogg_path          :: T.Text
  , midi_path          :: T.Text
  , song_info          :: SongInfo
  , tracks             :: [(T.Text, ([Integer], T.Text))]
  , pans               :: [Float]
  , vols               :: [Float]
  , active_track_db    :: [Float]
  , arena_path         :: T.Text
  , score_goal         :: [[Integer]]
  , tunnel_scale       :: Float
  , enable_order       :: [Integer]
  , section_start_bars :: [Integer]
  , title              :: T.Text
  , title_short        :: Maybe T.Text
  , artist             :: T.Text
  , artist_short       :: T.Text
  , unlock_requirement :: T.Text
  , desc               :: T.Text
  , bpm                :: Integer
  , preview_start_ms   :: Integer
  , preview_length_ms  :: Integer
  , boss_level         :: Maybe Integer
  } deriving (Eq, Show)

data SongInfo = SongInfo
  { length' :: U.Beats
  -- ^ given in measures:beats:ticks format internally.
  -- we assume 4-beat bars and 480-tick beats.
  -- also the dtas say "SHOULD BE 5 LESS THAN ACTUAL" which is indeed true
  , countin :: Integer -- ^ number of bars before gems start
  } deriving (Eq, Show)

chunkTracks :: (SendMessage m) => ChunkCodec m [(T.Text, ([Integer], T.Text))]
chunkTracks = dimap DictList fromDictList $ chunkParens $ chunksDictList chunkSym $ chunksPair (chunkParens stackChunks) chunkSym

-- | Drops children that aren't parentheses.
-- (This is a hack because in Crystal, HMX forgot to comment out the line
-- with instrument labels above the pans/vols.)
-- (Also because in All the Time, they used : instead of ;)
cleanAssoc :: (Monad m) => ChunksCodec m a -> ChunksCodec m a
cleanAssoc cdc = cdc
  { codecIn = let
    f = filter $ \case Parens{} -> True; _ -> False
    in mapStackTraceT (local f) $ codecIn cdc
  }

instance StackChunks Song where
  stackChunks = cleanAssoc $ asStrictAssoc "Song" $ do
    mogg_path          <- mogg_path          =. req "mogg_path"           (single chunkSym)
    midi_path          <- midi_path          =. req "midi_path"           (single chunkSym)
    song_info          <- song_info          =. req "song_info"           stackChunks
    tracks             <- tracks             =. req "tracks"              (single chunkTracks)
    pans               <- pans               =. req "pans"                (chunksParens stackChunks)
    vols               <- vols               =. req "vols"                (chunksParens stackChunks)
    active_track_db    <- active_track_db    =. req "active_track_db"     stackChunks
    arena_path         <- arena_path         =. req "arena_path"          (single chunkSym)
    score_goal         <- score_goal         =. req "score_goal"          (chunksList $ chunkParens stackChunks)
    tunnel_scale       <- tunnel_scale       =. opt 1 "tunnel_scale"      stackChunks
    enable_order       <- enable_order       =. req "enable_order"        (chunksParens stackChunks)
    section_start_bars <- section_start_bars =. req "section_start_bars"  stackChunks
    title              <- title              =. req "title"               stackChunks
    title_short        <- title_short        =. opt Nothing "title_short" stackChunks
    artist             <- artist             =. req "artist"              stackChunks
    artist_short       <- artist_short       =. req "artist_short"        stackChunks
    unlock_requirement <- unlock_requirement =. req "unlock_requirement"  (single chunkSym)
    desc               <- desc               =. req "desc"                (single chunkSym)
    bpm                <- bpm                =. req "bpm"                 stackChunks
    preview_start_ms   <- preview_start_ms   =. req "preview_start_ms"    stackChunks
    preview_length_ms  <- preview_length_ms  =. req "preview_length_ms"   stackChunks
    boss_level         <- boss_level         =. opt Nothing "boss_level"  stackChunks
    return Song{..}

chunkBarBeatTick :: (SendMessage m) => ChunkCodec m U.Beats
chunkBarBeatTick = Codec
  { codecIn = do
    k <- codecIn chunkSym
    case splitOn ":" $ T.unpack k of
      [bars, beats, ticks] -> do
        let read' = fmap (fromInteger :: Integer -> U.Beats)
                  . maybe (expected "a number") return . readMaybe
        bars' <- read' bars
        beats' <- read' beats
        ticks' <- read' ticks
        return $ bars' * 4 + beats' + ticks' / 480
      _ -> expected "a length in the form bars:beats:ticks"
  , codecOut = fmapArg $ \t -> let
    bars, beats', ticks :: Integer
    (bars, beats) = divMod' t 4
    (beats', part) = properFraction beats
    ticks = floor $ part * 480
    in tell $ Sym $ T.pack $ show bars <> ":" <> show beats' <> ":" <> show ticks
  }

instance StackChunks SongInfo where
  stackChunks = cleanAssoc $ asStrictAssoc "SongInfo" $ do
    length' <- length' =. req "length" (single chunkBarBeatTick)
    countin <- countin =. req "countin" stackChunks
    return SongInfo{..}
