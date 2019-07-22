{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FretsOnFire where

import           Control.Applicative            ((<|>))
import           Control.Monad                  ((>=>))
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer
import qualified Data.ByteString                as B
import           Data.Default.Class             (Default (..))
import qualified Data.HashMap.Strict            as HM
import           Data.Ini
import           Data.List                      (stripPrefix)
import           Data.Maybe                     (mapMaybe)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           DecodeText                     (decodeGeneral)
import           Text.Read                      (readMaybe)

data Song = Song
  { name             :: Maybe T.Text
  , artist           :: Maybe T.Text
  , album            :: Maybe T.Text
  , charter          :: Maybe T.Text -- ^ can be @frets@ or @charter@
  , year             :: Maybe Int
  , genre            :: Maybe T.Text
  , proDrums         :: Maybe Bool
  , songLength       :: Maybe Int
  , previewStartTime :: Maybe Int
  , diffBand         :: Maybe Int
  , diffGuitar       :: Maybe Int
  , diffGuitarGHL    :: Maybe Int
  , diffBass         :: Maybe Int
  , diffBassGHL      :: Maybe Int
  , diffDrums        :: Maybe Int
  , diffDrumsReal    :: Maybe Int
  , diffKeys         :: Maybe Int
  , diffKeysReal     :: Maybe Int
  , diffVocals       :: Maybe Int
  , diffVocalsHarm   :: Maybe Int
  , diffDance        :: Maybe Int
  , diffBassReal     :: Maybe Int
  , diffGuitarReal   :: Maybe Int
  , diffBassReal22   :: Maybe Int
  , diffGuitarReal22 :: Maybe Int
  , diffGuitarCoop   :: Maybe Int
  , diffRhythm       :: Maybe Int
  , diffDrumsRealPS  :: Maybe Int
  , diffKeysRealPS   :: Maybe Int
  , delay            :: Maybe Int
  , starPowerNote    :: Maybe Int -- ^ can be @star_power_note@ or @multiplier_note@
  , eighthNoteHOPO   :: Maybe Bool
  , hopoFrequency    :: Maybe Int
  , track            :: Maybe Int
  , sysexSlider      :: Maybe Bool
  , sysexOpenBass    :: Maybe Bool
  , video            :: Maybe FilePath
  , fiveLaneDrums    :: Maybe Bool
  , drumFallbackBlue :: Maybe Bool
  {- TODO:
  video_start_time
  video_end_time
  video_loop
  loading_phrase
  banner_link_a
  link_name_a
  banner_link_b
  link_name_b
  last_play
  kit_type
  keys_type
  guitar_type
  bass_type
  dance_type
  background
  rating
  count
  real_keys_lane_count_right
  real_keys_lane_count_left
  real_guitar_tuning
  real_bass_tuning
  sysex_high_hat_ctrl
  sysex_rimshot
  icon
  -}
  } deriving (Eq, Ord, Show, Read)

instance Default Song where
  def = Song
    def def def def def def def def def def
    def def def def def def def def def def
    def def def def def def def def def def
    def def def def def def def def def

-- | Strips <b>bold</b>, <i>italic</i>, and <color=red>colored</color>
-- which are supported by CH in metadata, lyrics, and sections.
stripTags :: T.Text -> T.Text
stripTags = let
  simple = ["<b>", "</b>", "<i>", "</i>", "</color>"]
  go "" = ""
  go s@(c:cs) = case mapMaybe (`stripPrefix` s) simple of
    [] -> case stripPrefix "<color=" s of
      Nothing -> c : go cs
      Just after -> case break (== '>') after of
        (_, '>' : after') -> go after'
        _                 -> c : go cs
    after : _ -> go after
  in T.pack . go . T.unpack

loadSong :: (MonadIO m) => FilePath -> StackTraceT m Song
loadSong fp = do
  let readIniUTF8
        = fmap (parseIni . decodeGeneral) . B.readFile
  ini <- inside fp $ liftIO (readIniUTF8 fp) >>= either fatal return
  -- TODO make all keys lowercase before lookup

  let str :: T.Text -> Maybe T.Text
      str k = either (const Nothing) Just $ lookupValue "song" k ini <|> lookupValue "Song" k ini
      int :: T.Text -> Maybe Int
      int = str >=> readMaybe . T.unpack
      bool :: T.Text -> Maybe Bool
      bool = str >=> \s -> case T.strip s of
        "True"  -> Just True
        "False" -> Just False
        "1"     -> Just True
        "0"     -> Just False
        _       -> Nothing

      name = stripTags <$> str "name"
      artist = stripTags <$> str "artist"
      album = stripTags <$> str "album"
      charter = stripTags <$> (str "charter" <|> str "frets")
      year = int "year"
      genre = stripTags <$> str "genre"
      proDrums = bool "pro_drums"
      songLength = int "song_length"
      previewStartTime = int "preview_start_time"
      diffBand = int "diff_band"
      diffGuitar = int "diff_guitar"
      diffGuitarGHL = int "diff_guitarghl"
      diffBass = int "diff_bass"
      diffBassGHL = int "diff_bassghl"
      diffDrums = int "diff_drums"
      diffDrumsReal = int "diff_drums_real"
      diffKeys = int "diff_keys"
      diffKeysReal = int "diff_keys_real"
      diffVocals = int "diff_vocals"
      diffVocalsHarm = int "diff_vocals_harm"
      diffDance = int "diff_dance"
      diffBassReal = int "diff_bass_real"
      diffGuitarReal = int "diff_guitar_real"
      diffBassReal22 = int "diff_bass_real_22"
      diffGuitarReal22 = int "diff_guitar_real_22"
      diffGuitarCoop = int "diff_guitar_coop"
      diffRhythm = int "diff_rhythm"
      diffDrumsRealPS = int "diff_drums_real_ps"
      diffKeysRealPS = int "diff_keys_real_ps"
      delay = int "delay"
      starPowerNote = int "star_power_note" <|> int "multiplier_note"
      eighthNoteHOPO = bool "eighthnote_hopo"
      hopoFrequency = int "hopo_frequency"
      track = int "track"
      sysexSlider = bool "sysex_slider"
      sysexOpenBass = bool "sysex_open_bass"
      video = fmap T.unpack $ str "video"
      fiveLaneDrums = bool "five_lane_drums"
      drumFallbackBlue = bool "drum_fallback_blue"

  return Song{..}

saveSong :: (MonadIO m) => FilePath -> Song -> m ()
saveSong fp Song{..} = writePSIni fp $ flip Ini []
  $ HM.singleton "song" $ execWriter $ do
    let str k = maybe (return ()) $ \v -> tell [(k, v)]
        shown k = str k . fmap (T.pack . show)
    str "name" name
    str "artist" artist
    str "album" album
    str "charter" charter
    str "frets" charter
    shown "year" year
    str "genre" genre
    shown "pro_drums" proDrums
    shown "song_length" songLength
    shown "preview_start_time" previewStartTime
    shown "diff_band" diffBand
    shown "diff_guitar" diffGuitar
    shown "diff_guitarghl" diffGuitarGHL
    shown "diff_bass" diffBass
    shown "diff_bassghl" diffBassGHL
    shown "diff_drums" diffDrums
    shown "diff_drums_real" diffDrumsReal
    shown "diff_keys" diffKeys
    shown "diff_keys_real" diffKeysReal
    shown "diff_vocals" diffVocals
    shown "diff_vocals_harm" diffVocalsHarm
    shown "diff_dance" diffDance
    shown "diff_bass_real" diffBassReal
    shown "diff_guitar_real" diffGuitarReal
    shown "diff_bass_real_22" diffBassReal22
    shown "diff_guitar_real_22" diffGuitarReal22
    shown "diff_guitar_coop" diffGuitarCoop
    shown "diff_rhythm" diffRhythm
    shown "diff_drums_real_ps" diffDrumsRealPS
    shown "diff_keys_real_ps" diffKeysRealPS
    shown "delay" delay
    shown "star_power_note" starPowerNote
    shown "multiplier_note" starPowerNote
    shown "eighthnote_hopo" eighthNoteHOPO
    shown "track" track
    shown "sysex_slider" sysexSlider
    shown "sysex_open_bass" sysexOpenBass
    str "video" $ fmap T.pack video
    shown "five_lane_drums" fiveLaneDrums
    shown "drum_fallback_blue" drumFallbackBlue

writePSIni :: (MonadIO m) => FilePath -> Ini -> m ()
writePSIni fp (Ini hmap _) = let
  txt = T.intercalate "\r\n" $ map section $ HM.toList hmap
  section (title, pairs) = T.intercalate "\r\n" $
    T.concat ["[", title, "]"] : map line pairs
  line (k, v) = T.concat [k, " = ", v]
  in liftIO $ B.writeFile fp $ TE.encodeUtf8 $ T.append txt "\r\n"
