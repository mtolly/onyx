{- |
Format a @songs.dta@ so that C3 CON Tools can read it.
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module PrettyDTA where

import           Config

import           Control.Monad.Trans.Writer
import qualified Data.ByteString            as B
import qualified Data.DTA                   as D
import           Data.DTA.Serialize
import           Data.DTA.Serialize.Magma   (Gender (..))
import qualified Data.DTA.Serialize.RB3     as D
import           Data.Foldable              (forM_)
import           Data.List                  (sortOn, stripPrefix)
import           Data.List.Split            (splitOn)
import qualified Data.Map                   as Map
import           Data.Maybe                 (listToMaybe, mapMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           System.IO.Extra            (latin1, readFileEncoding', utf8)

writeUtf8CRLF :: FilePath -> String -> IO ()
writeUtf8CRLF fp = B.writeFile fp . TE.encodeUtf8 . T.pack
  . concatMap (\case '\n' -> "\r\n"; c -> [c])

writeLatin1CRLF :: FilePath -> String -> IO ()
writeLatin1CRLF fp = B.writeFile fp . B.pack . map (fromIntegral . fromEnum) . T.unpack . T.pack
  . concatMap (\case '\n' -> "\r\n"; c -> [c])

stringLit :: String -> String
stringLit s = "\"" ++ (s >>= \case '"' -> "\\q"; c -> [c]) ++ "\""

data C3DTAComments = C3DTAComments
  { c3dtaCreatedUsing :: Maybe T.Text
  , c3dtaAuthoredBy   :: Maybe T.Text
  , c3dtaSong         :: Maybe T.Text
  , c3dtaLanguages    :: Maybe [T.Text]
  , c3dtaKaraoke      :: Maybe Bool
  , c3dtaMultitrack   :: Maybe Bool
  , c3dtaConvert      :: Maybe Bool
  , c3dta2xBass       :: Maybe Bool
  , c3dtaRhythmKeys   :: Maybe Bool
  , c3dtaRhythmBass   :: Maybe Bool
  , c3dtaCATemh       :: Maybe Bool
  , c3dtaExpertOnly   :: Maybe Bool
  } deriving (Eq, Ord, Show, Read)

makeC3DTAComments :: Metadata -> Plan -> Bool -> C3DTAComments
makeC3DTAComments meta plan is2x = C3DTAComments
  { c3dtaCreatedUsing = Just $ T.pack "Onyxite's Rock Band Tool"
  , c3dtaAuthoredBy   = Just $ getAuthor meta
  , c3dtaSong         = Just $ getTitle meta
  , c3dtaLanguages    = Just $ _languages meta
  , c3dtaKaraoke      = Just $ getKaraoke plan
  , c3dtaMultitrack   = Just $ getMultitrack plan
  , c3dtaConvert      = Just $ _convert meta
  , c3dta2xBass       = Just is2x
  , c3dtaRhythmKeys   = Just $ _rhythmKeys meta
  , c3dtaRhythmBass   = Just $ _rhythmBass meta
  , c3dtaCATemh       = Just $ _catEMH meta
  , c3dtaExpertOnly   = Just $ _expertOnly meta
  }

data DTASingle = DTASingle
  { dtaTopKey      :: String
  , dtaSongPackage :: D.SongPackage
  , dtaC3Comments  :: C3DTAComments
  } deriving (Eq, Ord, Show, Read)

-- | CONs put out by C3 Magma sometimes bizarrely have the @tracks_count@ key
-- completely removed from @songs.dta@, but the list of track counts is still
-- there. So, we have to put it back before parsing @song@ as a key-value map.
fixTracksCount :: [D.Chunk String] -> [D.Chunk String]
fixTracksCount = map findSong where
  findSong = \case
    D.Parens (D.Tree w (D.Key "song" : rest)) ->
      D.Parens (D.Tree w (D.Key "song" : map findTracksCount rest))
    x -> x
  findTracksCount = \case
    D.Parens (D.Tree w [D.Parens (D.Tree w2 nums)]) ->
      D.Parens $ D.Tree w [D.Key "tracks_count", D.Parens $ D.Tree w2 nums]
    x -> x

-- | Returns @(short song name, DTA file contents, is UTF8)@
readRB3DTA :: FilePath -> IO (String, D.SongPackage, Bool)
readRB3DTA dtaPath = do
  -- Not sure what encoding it is, try both.
  let readSongWith :: (FilePath -> IO (D.DTA String)) -> IO (String, D.SongPackage)
      readSongWith rdr = do
        dta <- rdr dtaPath
        (k, chunks) <- case D.treeChunks $ D.topTree dta of
          [D.Parens (D.Tree _ (D.Key k : chunks))] -> return (k, chunks)
          _ -> error $ dtaPath ++ " is not a valid songs.dta with exactly one song"
        case fromChunks $ fixTracksCount chunks of
          Left e    -> error $ dtaPath ++ " couldn't be unserialized: " ++ e
          Right pkg -> return (k, pkg)
  (k_l1, l1) <- readSongWith D.readFileDTA_latin1
  case fromKeyword <$> D.encoding l1 of
    Just "utf8" -> (\(k, pkg) -> (k, pkg, True)) <$> readSongWith D.readFileDTA_utf8
    Just "latin1" -> return (k_l1, l1, False)
    Nothing -> return (k_l1, l1, False)
    Just enc -> error $ dtaPath ++ " specifies an unrecognized encoding: " ++ enc

readDTASingle :: FilePath -> IO DTASingle
readDTASingle file = do
  (topKey, pkg, isUTF8) <- readRB3DTA file
  -- C3 puts extra info in DTA comments
  dtaLines <- fmap (lines . filter (/= '\r')) $ readFileEncoding' (if isUTF8 then utf8 else latin1) file
  let findBool s
        | elem (";" ++ s ++ "=0") dtaLines = Just False
        | elem (";" ++ s ++ "=1") dtaLines = Just True
        | otherwise                        = Nothing
      comments = C3DTAComments
        { c3dtaCreatedUsing = Just $ T.pack "Onyxite's Rock Band Tool"
        , c3dtaAuthoredBy = fmap T.pack $ listToMaybe $ mapMaybe (stripPrefix ";Song authored by ") dtaLines
        , c3dtaSong = fmap T.pack $ listToMaybe $ mapMaybe (stripPrefix ";Song=") dtaLines
        , c3dtaLanguages
          = fmap (map T.pack .  filter (not . null) . splitOn ",")
          $ listToMaybe $ mapMaybe (stripPrefix ";Language(s)=") dtaLines
        , c3dtaKaraoke = findBool "Karaoke"
        , c3dtaMultitrack = findBool "Multitrack"
        , c3dtaConvert = findBool "Convert"
        , c3dta2xBass = findBool "2xBass"
        , c3dtaRhythmKeys = findBool "RhythmKeys"
        , c3dtaRhythmBass = findBool "RhythmBass"
        , c3dtaCATemh = findBool "CATemh"
        , c3dtaExpertOnly = findBool "ExpertOnly"
        }
  return $ DTASingle topKey pkg comments

writeDTASingle :: DTASingle -> String
writeDTASingle (DTASingle x y z) = prettyDTA x y z

prettyDTA :: String -> D.SongPackage -> C3DTAComments -> String
prettyDTA name pkg C3DTAComments{..} = unlines $ execWriter $ do
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
      Left D.KTempoSlow   -> "kTempoSlow"
      Left D.KTempoMedium -> "kTempoMedium"
      Left D.KTempoFast   -> "kTempoFast"
      Right n             -> show n
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
      Male   -> "male"
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
    forM_ (D.songKey pkg) $ inlineRaw "song_key" . show . fromEnum
    forM_ (D.bandFailCue pkg) $ inline "band_fail_cue" . show . either id fromKeyword
    forM_ (D.shortVersion pkg) $ inline "short_version" . show
    forM_ (D.realGuitarTuning pkg) $ inline "real_guitar_tuning" . parenthesize . unwords . map show . fromInParens
    forM_ (D.realBassTuning pkg) $ inline "real_bass_tuning" . parenthesize . unwords . map show . fromInParens
    -- rb2 stuff
    forM_ (D.context pkg) $ inline "context" . show
    forM_ (D.decade pkg) $ inline "decade" . fromKeyword
    forM_ (D.downloaded pkg) $ inline "downloaded" . \case True -> "1"; False -> "0"
    forM_ (D.basePoints pkg) $ inline "base_points" . show
  -- C3 comments
  ln ";DO NOT EDIT THE FOLLOWING LINES MANUALLY"
  forM_ c3dtaCreatedUsing $ \t -> ln $ ";Created using " ++ T.unpack t
  forM_ c3dtaAuthoredBy $ \t -> ln $ ";Song authored by " ++ T.unpack t
  forM_ c3dtaSong $ \t -> ln $ ";Song=" ++ T.unpack t
  forM_ c3dtaLanguages $ \ts -> ln $ concatMap (\t -> T.unpack t ++ ",") ts
  forM_ c3dtaKaraoke $ \b -> ln $ ";Karaoke=" ++ if b then "1" else "0"
  forM_ c3dtaMultitrack $ \b -> ln $ ";Multitrack=" ++ if b then "1" else "0"
  forM_ c3dtaConvert $ \b -> ln $ ";Convert=" ++ if b then "1" else "0"
  forM_ c3dta2xBass $ \b -> ln $ ";2xBass=" ++ if b then "1" else "0"
  forM_ c3dtaRhythmKeys $ \b -> ln $ ";RhythmKeys=" ++ if b then "1" else "0"
  forM_ c3dtaRhythmBass $ \b -> ln $ ";RhythmBass=" ++ if b then "1" else "0"
  forM_ c3dtaCATemh $ \b -> ln $ ";CATemh=" ++ if b then "1" else "0"
  forM_ c3dtaExpertOnly $ \b -> ln $ ";ExpertOnly=" ++ if b then "1" else "0"
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
