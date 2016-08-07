{- |
Format a @songs.dta@ so that C3 CON Tools can read it.
-}
{-# LANGUAGE LambdaCase #-}
module PrettyDTA where

import           Config

import           Control.Monad.Trans.Writer
import qualified Data.ByteString            as B
import           Data.DTA.Serialize
import           Data.DTA.Serialize.Magma   (Gender (..))
import qualified Data.DTA.Serialize.RB3     as D
import           Data.Foldable              (forM_)
import           Data.List                  (sortOn)
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE

writeUtf8CRLF :: FilePath -> String -> IO ()
writeUtf8CRLF fp = B.writeFile fp . TE.encodeUtf8 . T.pack
  . concatMap (\case '\n' -> "\r\n"; c -> [c])

writeLatin1CRLF :: FilePath -> String -> IO ()
writeLatin1CRLF fp = B.writeFile fp . B.pack . map (fromIntegral . fromEnum) . T.unpack . T.pack
  . concatMap (\case '\n' -> "\r\n"; c -> [c])

stringLit :: String -> String
stringLit s = "\"" ++ (s >>= \case '"' -> "\\q"; c -> [c]) ++ "\""

prettyDTA :: String -> Metadata -> Plan -> Bool -> D.SongPackage -> String
prettyDTA name meta plan is2x pkg = unlines $ execWriter $ do
  ln "("
  indent $ do
    ln $ quote name
    two "name" $ stringLit $ D.name pkg
    two "artist" $ stringLit $ D.artist pkg
    inline "master" $ if D.master pkg then "1" else "0"
    parens $ do
      ln $ quote "song"
      two "name" $ stringLit $ D.songName $ D.song pkg
      forM_ (D.tracksCount $ D.song pkg) $ \(InParens ns) -> do
        two "tracks_count" $ "(" ++ unwords (map show ns) ++ ")"
      parens $ do
        ln $ quote "tracks"
        parens $ do
          let trackOrder (k, _) = case k of
                "drum"   -> Left (0 :: Int)
                "bass"   -> Left 1
                "guitar" -> Left 2
                "vocals" -> Left 3
                "keys"   -> Left 4
                _        -> Right k
          forM_ (sortOn trackOrder $ Map.toList $ fromDict $ fromInParens $ D.tracks $ D.song pkg) $ \(k, v) -> do
            two k $ case v of
              Left  n             -> "(" ++ show n                ++ ")"
              Right (InParens ns) -> "(" ++ unwords (map show ns) ++ ")"
      two "pans" $ "(" ++ unwords (map show $ fromInParens $ D.pans $ D.song pkg) ++ ")"
      two "vols" $ "(" ++ unwords (map show $ fromInParens $ D.vols $ D.song pkg) ++ ")"
      two "cores" $ "(" ++ unwords (map show $ fromInParens $ D.cores $ D.song pkg) ++ ")"
      forM_ (D.crowdChannels $ D.song pkg) $ inline "crowd_channels" . unwords . map show -- C3 unindents this for some reason but whatever
      forM_ (D.vocalParts $ D.song pkg) $ inline "vocal_parts" . show
      parens $ do
        ln $ quote "drum_solo"
        two "seqs" $ "(" ++ unwords (map (quote . fromKeyword) $ fromInParens $ D.seqs $ D.drumSolo $ D.song pkg) ++ ")"
      parens $ do
        ln $ quote "drum_freestyle"
        two "seqs" $ "(" ++ unwords (map (quote . fromKeyword) $ fromInParens $ D.seqs $ D.drumFreestyle $ D.song pkg) ++ ")"
      forM_ (D.muteVolume       $ D.song pkg) $ inlineRaw "mute_volume"        . show
      forM_ (D.muteVolumeVocals $ D.song pkg) $ inlineRaw "mute_volume_vocals" . show
      forM_ (D.hopoThreshold    $ D.song pkg) $ inlineRaw "hopo_threshold"     . show
      -- rb2
      forM_ (D.midiFile         $ D.song pkg) $ inline "midi_file" . show
    inline "song_scroll_speed" $ show $ D.songScrollSpeed pkg
    forM_ (D.bank pkg) $ two "bank" . stringLit . either id fromKeyword
    forM_ (D.drumBank pkg) $ inlineRaw "drum_bank" . either id fromKeyword
    inline "anim_tempo" $ case D.animTempo pkg of
      Left D.KTempoSlow -> "kTempoSlow"
      Left D.KTempoMedium -> "kTempoMedium"
      Left D.KTempoFast -> "kTempoFast"
      Right n -> show n
    inline "song_length" $ show $ D.songLength pkg
    inline "preview" $ case D.preview pkg of (start, end) -> show start ++ " " ++ show end
    parens $ do
      ln $ quote "rank"
      let rankOrder (k, _) = case k of
            "drum"      -> Left (0 :: Int)
            "guitar"    -> Left 1
            "bass"      -> Left 2
            "vocals"    -> Left 3
            "keys"      -> Left 4
            "real_keys" -> Left 5
            "band"      -> Left 6
            _           -> Right k
      forM_ (sortOn rankOrder $ Map.toList $ fromDict $ D.rank pkg) $ \(k, v) -> do
        inline k $ show v
    inline "genre" $ quote $ fromKeyword $ D.genre pkg
    inline "vocal_gender" $ quote $ case D.vocalGender pkg of
      Female -> "female"
      Male -> "male"
    inline "version" $ show $ D.version pkg
    inline "format" $ show $ D.format pkg
    forM_ (D.albumArt pkg) $ \b -> inline "album_art" $ if b then "1" else "0"
    inline "year_released" $ show $ D.yearReleased pkg
    inline "rating" $ show $ D.rating pkg
    forM_ (D.subGenre pkg) $ inline "sub_genre" . quote . fromKeyword
    inline "song_id" $ either show fromKeyword $ D.songId pkg
    forM_ (D.solo pkg) $ inlineRaw "solo" . parenthesize . unwords . map fromKeyword . fromInParens
    forM_ (D.tuningOffsetCents pkg) $ inline "tuning_offset_cents" . show -- TODO: should this be an int?
    forM_ (D.guidePitchVolume pkg) $ inline "guide_pitch_volume" . show
    inline "game_origin" $ quote $ fromKeyword $ D.gameOrigin pkg
    forM_ (D.encoding pkg) $ inline "encoding" . quote . fromKeyword
    forM_ (D.albumName pkg) $ two "album_name" . stringLit
    forM_ (D.albumTrackNumber pkg) $ inline "album_track_number" . show
    forM_ (D.vocalTonicNote pkg) $ inlineRaw "vocal_tonic_note" . show . fromEnum
    forM_ (D.songTonality pkg) $ inlineRaw "song_tonality" . \case
      D.Major -> "0"
      D.Minor -> "1"
    -- the following keys, I'm not sure if they need to go in a certain place
    forM_ (D.bandFailCue pkg) $ inline "band_fail_cue" . show . either id fromKeyword
    forM_ (D.shortVersion pkg) $ inline "short_version" . show
    forM_ (D.realGuitarTuning pkg) $ inline "real_guitar_tuning" . unwords . map show . fromInParens
    forM_ (D.realBassTuning pkg) $ inline "real_bass_tuning" . unwords . map show . fromInParens
    -- rb2 stuff
    forM_ (D.context pkg) $ inline "context" . show
    forM_ (D.decade pkg) $ inline "decade" . fromKeyword
    forM_ (D.downloaded pkg) $ inline "downloaded" . \case True -> "1"; False -> "0"
    forM_ (D.basePoints pkg) $ inline "base_points" . show
  -- C3 comments
  ln ";DO NOT EDIT THE FOLLOWING LINES MANUALLY"
  ln ";Created using Onyxite's Rock Band Tool"
  ln $ ";Song authored by " ++ T.unpack (getAuthor meta)
  ln $ ";Song=" ++ T.unpack (getTitle meta)
  ln $ ";Language(s)=" ++ concatMap (\t -> T.unpack t ++ ",") (_languages meta)
  ln $ ";Karaoke=" ++ if getKaraoke plan then "1" else "0"
  ln $ ";Multitrack=" ++ if getMultitrack plan then "1" else "0"
  ln $ ";Convert=" ++ if _convert meta then "1" else "0"
  ln $ ";2xBass=" ++ if is2x then "1" else "0"
  ln $ ";RhythmKeys=" ++ if _rhythmKeys meta then "1" else "0"
  ln $ ";RhythmBass=" ++ if _rhythmBass meta then "1" else "0"
  ln $ ";CATemh=" ++ if _catEMH meta then "1" else "0"
  ln $ ";ExpertOnly=" ++ if _expertOnly meta then "1" else "0"
  ln ")"
  where indent = mapWriter $ \(x, s) -> (x, map ("   " ++) s)
        parens act = do
          ln "("
          () <- indent act
          ln ")"
        two k v = parens $ do
          ln $ quote k
          ln v
        ln s = tell [s]
        quote s = "'" ++ s ++ "'"
        inline = inlineRaw . quote
        inlineRaw k v = ln $ parenthesize $ k ++ " " ++ v
        parenthesize s = "(" ++ s ++ ")"
