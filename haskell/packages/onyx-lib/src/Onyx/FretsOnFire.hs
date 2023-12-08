{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Onyx.FretsOnFire where

import           Control.Applicative        ((<|>))
import           Control.Monad              (when, (>=>))
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Writer
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (isSpace)
import           Data.Default.Class         (Default (..))
import           Data.Fixed                 (Milli)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (stripPrefix)
import           Data.Maybe                 (mapMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Onyx.StackTrace
import           Onyx.Util.Handle           (Readable, handleToByteString,
                                             useHandle)
import           Onyx.Util.Text.Decode      (decodeGeneral)
import           Text.Read                  (readMaybe)

data Song = Song
  { name             :: Maybe T.Text
  , artist           :: Maybe T.Text
  , album            :: Maybe T.Text
  , charter          :: Maybe T.Text -- ^ can be @frets@ or @charter@
  , year             :: Maybe Int -- TODO probably shouldn't be restricted to int, I've seen "year = 2008-2009" which CH shows as-is but sorts under 2008
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
  , track            :: Maybe Int -- ^ either @track@ or @album_track@
  , sysexSlider      :: Maybe Bool
  , sysexOpenBass    :: Maybe Bool
  , fiveLaneDrums    :: Maybe Bool
  , drumFallbackBlue :: Maybe Bool
  , loadingPhrase    :: Maybe T.Text
  , video            :: Maybe FilePath -- ^ only used by PS, CH only accepts @video.*@
  , videoStartTime   :: Maybe Milli
  , videoEndTime     :: Maybe Milli
  , videoLoop        :: Maybe Bool
  , cassetteColor    :: Maybe T.Text -- ^ old FoF background color that label.png goes on top of
  , tags             :: Maybe T.Text
  , background       :: Maybe FilePath -- ^ probably only PS, CH finds @background.*@
  {- TODO:
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
  rating
  count
  real_keys_lane_count_right
  real_keys_lane_count_left
  real_guitar_tuning
  real_bass_tuning
  sysex_high_hat_ctrl
  sysex_rimshot
  icon
  playlist_track
  -}
  } deriving (Eq, Ord, Show)

instance Default Song where
  def = Song
    def def def def def def def def def def
    def def def def def def def def def def
    def def def def def def def def def def
    def def def def def def def def def def
    def def def def def def

-- | Strips <b>bold</b>, <i>italic</i>, and <color=red>colored</color>
-- which are supported by CH in metadata, lyrics, and sections.
-- Also replaces <br> with newline characters.
stripTags :: T.Text -> T.Text
stripTags = let
  -- TODO also remove <size=3></size>
  simple = ["<b>", "</b>", "<i>", "</i>", "</color>"]
  go "" = ""
  go s@(c:cs) = case mapMaybe (`stripPrefix` s) simple of
    [] -> case stripPrefix "<color=" s of
      Nothing -> case stripPrefix "<br>" s of
        Nothing    -> c : go cs
        Just after -> '\n' : go after
      Just after -> case break (== '>') after of
        (_, '>' : after') -> go after'
        _                 -> c : go cs
    after : _ -> go after
  in T.pack . go . T.unpack

loadSong :: (MonadIO m, SendMessage m) => Readable -> StackTraceT m Song
loadSong r = do

  Ini ini <- inside "Parsing song.ini" $ do
    bs <- liftIO $ decodeGeneral . BL.toStrict <$> useHandle r handleToByteString
    readPSIni bs

  let str :: T.Text -> Maybe T.Text
      str k = HM.lookup k ini
      int :: T.Text -> Maybe Int
      int = str >=> readMaybe . T.unpack
      milli :: T.Text -> Maybe Milli
      milli = str >=> fmap (/ 1000) . readMaybe . T.unpack
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
      track = int "track" <|> int "album_track"
      sysexSlider = bool "sysex_slider"
      sysexOpenBass = bool "sysex_open_bass"
      fiveLaneDrums = bool "five_lane_drums"
      drumFallbackBlue = bool "drum_fallback_blue"
      loadingPhrase = stripTags <$> str "loading_phrase"
      video = fmap T.unpack $ str "video"
      videoStartTime = milli "video_start_time"
      videoEndTime = milli "video_end_time"
      videoLoop = bool "video_loop"
      cassetteColor = str "cassettecolor"
      tags = str "tags"
      background = fmap T.unpack $ str "background"

  return Song{..}

saveSong :: (MonadIO m) => FilePath -> Song -> m ()
saveSong fp Song{..} = writePSIni fp $ execWriter $ do
  let str k = maybe (return ()) $ \v -> tell [(k, v)]
      shown k = str k . fmap (T.pack . show)
      milli k = shown k . fmap ((floor :: Milli -> Int) . (* 1000))
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
  shown "album_track" track
  shown "sysex_slider" sysexSlider
  shown "sysex_open_bass" sysexOpenBass
  shown "five_lane_drums" fiveLaneDrums
  shown "drum_fallback_blue" drumFallbackBlue
  str "loading_phrase" loadingPhrase
  str "video" $ fmap T.pack video
  milli "video_start_time" videoStartTime
  milli "video_end_time" videoEndTime
  shown "video_loop" videoLoop
  str "cassettecolor" cassetteColor
  str "tags" tags
  str "background" $ fmap T.pack background

-- simple, only stores [song] section
newtype Ini = Ini (HM.HashMap T.Text T.Text)

data IniLine
  = IniSection T.Text
  | IniKeyValue T.Text T.Text
  | IniContinue T.Text

readPSIni :: (SendMessage m) => T.Text -> StackTraceT m Ini
readPSIni = fmap Ini . go HM.empty False . mapMaybe interpretLine . zip [1..] . T.lines where
  interpretLine :: (Int, T.Text) -> Maybe (Int, IniLine)
  interpretLine (i, T.strip -> line) = if ";" `T.isPrefixOf` line || "//" `T.isPrefixOf` line
    then Nothing
    else case T.stripPrefix "[" line >>= T.stripSuffix "]" of
      Just newSection -> Just (i, IniSection $ T.strip newSection)
      Nothing         -> case T.break (== '=') line of
        (x, y) -> case T.stripPrefix "=" y of
          Just y' -> Just (i, IniKeyValue (T.strip x) (T.strip y'))
          Nothing -> Just (i, IniContinue line)
  go hm          _             []                 = return hm
  go hm inSongSection ((i, line) : rest) = case line of
    IniSection newSection -> if T.toCaseFold newSection == "song"
      then go hm True rest
      else do
        inside ("song.ini line " <> show i) $ warn $ "Unrecognized section (not [song]) in song.ini: " <> show newSection
        go hm False rest
    IniContinue x -> do
      when (T.any (not . isSpace) x) $ inside ("song.ini line " <> show i)
        $ warn $ "Unrecognized song.ini line: " <> T.unpack x
      go hm inSongSection rest
    IniKeyValue x y -> let
      (fullValue, rest') = pullContinues y rest
      hm' = if inSongSection
        then HM.insert (T.toCaseFold x) (T.strip fullValue) hm
        else hm
      in go hm' inSongSection rest'
  pullContinues value ((_, IniContinue next) : rest) = pullContinues (value <> "\n" <> next) rest
  pullContinues value rest                           = (value, rest)

writePSIni :: (MonadIO m) => FilePath -> [(T.Text, T.Text)] -> m ()
writePSIni fp pairs = let
  txt = T.intercalate "\r\n" $ "[song]" : map line pairs
  line (k, v) = T.concat [k, " = ", v]
  in liftIO $ B.writeFile fp $ TE.encodeUtf8 $ T.append txt "\r\n"
