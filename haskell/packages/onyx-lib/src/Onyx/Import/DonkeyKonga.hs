{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
module Onyx.Import.DonkeyKonga where

import           Control.Monad                    (forM, guard, when)
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Conduit                     (yield)
import qualified Data.Conduit.Audio               as CA
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import qualified Data.HashMap.Strict              as HM
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   listToMaybe, mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import qualified Data.Vector.Storable             as V
import           Onyx.Audio
import           Onyx.Import.Base
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read                   (mapTrack)
import           Onyx.MIDI.Track.Drums
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.Nintendo.DonkeyKonga
import           Onyx.Nintendo.DSP
import           Onyx.Nintendo.GCM
import           Onyx.Project
import           Onyx.Resources                   (getResourcesPath)
import           Onyx.StackTrace
import           Onyx.Util.Binary                 (runGetM)
import           Onyx.Util.Handle
import           Onyx.Util.Text.ShiftJIS          (decodeShiftJIS)
import           Optics                           ((.~))
import qualified Sound.MIDI.Util                  as U
import           System.Directory                 (listDirectory)
import           System.FilePath                  ((<.>), (</>))
import           Text.Read                        (readMaybe)

supportedDKGames :: (SendMessage m, MonadIO m) => [(B.ByteString, Readable -> StackTraceT m [Import m])]
supportedDKGames =
  [ ("GKGJ01", importDK1  True ) -- dk1 japan
  , ("GKGE01", importDK1  False) -- dk1 us
  , ("GKGP01", importDK1  False) -- dk1 europe
  , ("GY2J01", importDK23 True ) -- dk2 japan
  , ("GY2E01", importDK23 False) -- dk2 us
  , ("GY2P01", importDK23 False) -- dk2 europe
  , ("GY3J01", importDK23 True ) -- dk3 japan
  ]

data DKSongInfo = DKSongInfo
  { title   :: T.Text
  , comment :: Maybe T.Text
  , genre   :: Maybe T.Text
  -- the game's difficulty numbers for 1-player charts, keyed by chart filename suffix
  , ratings :: [(B.ByteString, Integer)]
  }

loadMainDOL :: (MonadIO m) => Folder B.ByteString Readable -> StackTraceT m DOL
loadMainDOL tree = case findFile ("sys" :| ["main.dol"]) tree of
  Nothing -> fatal "Couldn't find main.dol"
  Just r  -> readDOL . BL.toStrict <$> stackIO (useHandle r handleToByteString)

-- Imports from Donkey Konga 1 (J/U/E) .iso (GCM)
importDK1 :: (SendMessage m, MonadIO m) => Bool -> Readable -> StackTraceT m [Import m]
importDK1 isJapanese gcm = do
  tree <- stackIO $ loadGCM gcm
  chartFolder <- case findFolder ["Game", "Sheet", "Solo"] tree of
    Nothing -> fatal "Couldn't find charts folder"
    Just d  -> return d
  audioFolder <- case findFolder ["stream", "score"] tree of
    Nothing -> fatal "Couldn't find audio folder"
    Just d  -> return d
  dol <- loadMainDOL tree
  let chartFiles = folderFiles chartFolder
      audioFiles = folderFiles audioFolder
      songs = findDK1Songs (\name -> isJust $ lookup (name <> "_h.bin") chartFiles) dol
      decodeTitle = T.strip . if isJapanese then decodeShiftJIS else TE.decodeLatin1
  when (null songs) $ fatal "Couldn't find song table in main.dol"
  fmap catMaybes $ forM songs \song -> do
    let audioName = fromMaybe song.stream (B.stripPrefix "score/" song.stream) <> ".dsp"
    case (lookup (song.chart <> "_h.bin") chartFiles, lookup audioName audioFiles) of
      (Just bin, Just dsp) -> return $ Just $ importDKSong DKSongInfo
        { title   = decodeTitle song.title
        , comment = Nothing
        , genre   = Nothing
        , ratings = []
        } Nothing False bin dsp
      _ -> do
        warn $ "Couldn't find chart or audio for song: " <> show song
        return Nothing

-- Imports from Donkey Konga 2 (J/U/E) or Donkey Konga 3 (J) .iso (GCM)
importDK23 :: (SendMessage m, MonadIO m) => Bool -> Readable -> StackTraceT m [Import m]
importDK23 isJapanese gcm = do
  tree <- stackIO $ loadGCM gcm
  chartFolder <- case findFolder ["Score"] tree of
    Nothing -> fatal "Couldn't find charts folder"
    Just d  -> return d
  audioFolder <- case findFolder ["stream", "score"] tree of
    Nothing -> fatal "Couldn't find audio folder"
    Just d  -> return d
  let chartFiles = folderFiles chartFolder
      audioFiles = folderFiles audioFolder
      -- most songs have a 1-player expert chart, but some bonus songs
      -- (and the tutorial) only have hard or easy
      -- returns (suffix, file)
      findChart name = listToMaybe $ mapMaybe
        (\suffix -> (\r -> (suffix, r)) <$> lookup (name <> suffix) chartFiles)
        ["_1x.mid", "_1h.mid", "_1e.mid"]
      nonEmpty t = t <$ guard (not $ T.null t)
      readResource name = forM (findFile ("Resource" :| [name]) tree) \r ->
        BL.toStrict <$> stackIO (useHandle r handleToByteString)
  -- (song number, chart name, info); song number gives the audio filename
  songs <- readResource "SongInfo.res" >>= \case
    Just songInfo -> do
      let decode = if isJapanese then decodeShiftJIS else TE.decodeLatin1
          info = readSongInfo decode songInfo
      -- Genre names are in SongData.res (DK3) or GenreName_uk.res (DK2 E).
      -- DK2 (U) has a genre name list left over from Japanese DK2, which doesn't
      -- match its songs' genre numbers, so we don't use it.
      genreNames <- readGenreNames decode . fromMaybe B.empty
        <$> readResource (if isJapanese then "SongData.res" else "GenreName_uk.res")
      return $ info <&> \s -> (s.info_index, s.info_FILENAME, DKSongInfo
        { title   = fromMaybe (TE.decodeLatin1 s.info_FILENAME) $ nonEmpty $ T.strip s.info_SONGNAME
        , comment = s.info_COMMENT >>= nonEmpty . T.strip
        , genre   = s.info_GENRE >>= readMaybe . T.unpack >>= (`lookup` genreNames)
        , ratings = catMaybes
          [ (\n -> (suffix, fromIntegral n)) <$> rating
          | (suffix, rating) <- [("_1e.mid", s.info_1E), ("_1h.mid", s.info_1H), ("_1x.mid", s.info_1X)]
          ]
        })
    Nothing -> do
      -- DK2 (J) has no SongInfo.res, metadata is in main.dol instead
      dol <- loadMainDOL tree
      let dkSongs = findDK2JSongs (isJust . findChart) dol
          genreNames = findDK2JGenres dol
      when (null dkSongs) $ fatal "Couldn't find song tables in main.dol"
      return $ dkSongs <&> \s -> (s.number, s.chart, DKSongInfo
        { title   = maybe (TE.decodeLatin1 s.chart) (T.strip . decodeShiftJIS) s.title
        , comment = decodeShiftJIS <$> s.comment
        , genre   = s.genre >>= \g -> T.strip . decodeShiftJIS <$> listToMaybe (drop (fromIntegral g) genreNames)
        , ratings = zip ["_1e.mid", "_1h.mid", "_1x.mid"] $ map fromIntegral s.ratings
        })
  fmap catMaybes $ forM songs \(number, chartName, info) -> let
    audioName = B8.pack $ (if number < 10 then ('0' :) else id) (show number) <> ".dsp"
    in case (findChart chartName, lookup audioName audioFiles) of
      (Just (suffix, mid), Just dsp) -> return $ Just $ importDKSong info (lookup suffix info.ratings) True mid dsp
      _ -> do
        warn $ "Couldn't find chart or audio for song: " <> show (number, chartName)
        return Nothing

-- rating is the game's difficulty number for the chart, imported as-is for now
importDKSong :: (SendMessage m, MonadIO m) => DKSongInfo -> Maybe Integer -> Bool -> Readable -> Readable -> Import m
importDKSong info rating isMidi chartFile dsp level = do

  (soundsHigh, soundsLow, soundsClap) <- case level of
    ImportQuick -> return ([], [], [])
    ImportFull  -> stackIO loadBongoSounds
  let panSounds pan vol = mapM \(name, path) -> do
        src <- buildSource' $ Input path
        return (name, applyPansVols [pan] [vol] src)
  soundsHigh' <- panSounds (-0.3) 4  soundsHigh
  soundsLow'  <- panSounds 0.3    4  soundsLow
  soundsClap' <- panSounds 0      12 soundsClap
  let sampleInfo = ("bongos", AudioSamples SamplesInfo
        { groupPolyphony = Nothing
        , groupCrossfade = 0
        })
      soundsMap = HM.fromList $ sampleInfo : do
        (name, src) <- soundsHigh' <> soundsLow' <> soundsClap'
        return (name, AudioFile AudioInfo
          { md5 = Nothing
          , frames = Nothing
          , commands = []
          , filePath = Just $ SoftFile ("sfx" </> T.unpack name <.> "wav") $ SoftAudio src
          , rate = Nothing
          , channels = 2
          })

  -- lag: audio time = chart time + lag
  let defaultSigs = U.measureMapFromLengths U.Error $ RTB.singleton 0 4
  (tempos, timesigs, chart, lag) <- case level of
    ImportQuick -> return (U.makeTempoMap RTB.empty, defaultSigs, RTB.empty, 0)
    ImportFull -> if isMidi
      then do
        raw <- F.loadRawMIDIReadable chartFile
        mid <- F.readMIDIFile' raw
        let _ = mid :: F.Song (F.RawFile U.Beats)
            dk = readTrackDK2 $ U.applyTempoTrack mid.tempos case mid.tracks.rawTracks of
              t : _ -> t
              []    -> RTB.empty
            measure2 = realToFrac $ U.applyTempoMap mid.tempos $ U.unapplyMeasureMap mid.timesigs (1, 0) :: Double
        -- the sync sysex is in the tempo track, which readMIDIFile doesn't keep
        lag <- case findDKAudioStart $ either id (const []) $ U.decodeFile raw of
          Just ms -> return $ fromIntegral ms / 1000 - measure2
          Nothing -> do
            warn "Couldn't find Donkey Konga sync info in chart; audio may be misaligned"
            return 0
        return (mid.tempos, mid.timesigs, dk, lag)
      else do
        b <- stackIO $ useHandle chartFile handleToByteString
        (tempos, events) <- fmap binToMidi $ runGetM readSheetBin b
        return (tempos, defaultSigs, interpretDK1 events, 0)
  -- Only ever add time: delay the chart by whole seconds and pad the audio
  -- with the remainder, or just pad the audio
  let (audioDelay, midiDelay) = if lag > 0
        then let
          secs = ceiling lag :: Int
          in (fromIntegral secs - lag, secs)
        else (negate lag, 0)
  let converted = mapTrack (U.unapplyTempoTrack tempos) $ convertDKDrums chart
      roundRobin = zip3 (cycle $ map fst soundsHigh) (cycle $ map fst soundsLow) (cycle $ map fst soundsClap)
      bongoSampleTrack = F.SamplesTrack
        { F.sampleTriggers
          = RTB.flatten
          $ RTB.fromPairList
          $ map (\((ahigh, alow, aclap), (dt, instant)) -> let
              drums = map fst instant
              in (dt, concat
                [ [F.simpleTrigger ahigh | elem (Pro Yellow ()) drums]
                , [F.simpleTrigger alow  | elem (Pro Blue   ()) drums]
                , [F.simpleTrigger aclap | elem Red drums || elem (Pro Green ()) drums]
                ])
            )
          $ zip roundRobin
          $ RTB.toPairList
          $ RTB.collectCoincident
          $ (.gems)
          $ fromMaybe mempty $ Map.lookup Expert converted.difficulties
        , F.sampleNotes = RTB.empty
        }

  audio <- case level of
    ImportQuick -> return Nothing
    ImportFull -> do
      (rate, chanL, chanR) <- stackIO $ useHandle dsp handleToByteString >>= decodeStereoCstr . BL.toStrict
      return $ Just $ CA.mapSamples CA.fractionalSample CA.AudioSource
        { CA.source = yield $ CA.interleave [chanL, chanR]
        , CA.rate = fromIntegral rate
        , CA.channels = 2
        , CA.frames = V.length chanL
        }

  return SongYaml
    { metadata = def'
      { title = Just info.title
      , comments = toList info.comment
      , genre = info.genre
      , fileAlbumArt = Nothing
      }
    , global = def'
      { backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      , fileMidi = SoftFile "notes.mid" $ SoftChart $ F.padAnyFile midiDelay $ F.Song
        { F.tempos = tempos
        , F.timesigs = timesigs
        , F.tracks = mempty
          { F.onyxParts = Map.singleton F.PartDrums mempty
            { F.onyxPartDrums = converted
            }
          , F.onyxSamples = Map.singleton "bongos" bongoSampleTrack
          }
        }
      , fileMidiOriginal = Nothing
      , fileSongAnim = Nothing
      }
    , audio = case audio of
      Nothing -> HM.empty
      Just src -> HM.insert "song" (AudioFile AudioInfo
        { md5 = Nothing
        , frames = Nothing
        , commands = []
        , filePath = Just $ SoftFile "song.wav" $ SoftAudio src
        , rate = Nothing
        , channels = 2
        }) soundsMap
    , jammit = HM.empty
    , plans = case audio of
      Nothing -> HM.empty
      Just _ -> HM.singleton "plan" $ StandardPlan StandardPlanInfo
        { song        = Just $ PansVols [-1, 1] [-2, -2]
          $ (if audioDelay > 0 then Pad Start $ CA.Seconds audioDelay else id)
          $ Input $ Named "song"
        , parts       = Parts $ HM.singleton F.PartDrums $ PartSingle $ Input $ Named "bongos"
        , crowd       = Nothing
        , comments    = []
        , tuningCents = 0
        , fileTempo   = Nothing
        }
    , targets = HM.empty
    , parts = Parts $ HM.singleton F.PartDrums
      $ (emptyPart :: Part SoftFile)
      & #drums .~ Just (emptyPartDrums Drums4 Kicks1x & #difficulty .~ maybe (Tier 1) Tier rating)
    }

convertDKDrums :: RTB.T U.Seconds (DKEvent U.Seconds) -> DrumTrack U.Seconds
convertDKDrums dk = let

  red    = (Red          , VelocityNormal)
  yellow = (Pro Yellow (), VelocityNormal)
  blue   = (Pro Blue   (), VelocityNormal)
  green  = (Pro Green  (), VelocityNormal)

  laneSpeed = 0.080 :: U.Seconds -- 80 ms for consistent combo
  -- make lanes into single notes if they're too short to have 2 notes
  dk' = dk <&> \case
    DKRoll drum t | t <= laneSpeed -> DKNote drum
    event                          -> event

  normalNotes = RTB.flatten $ dk' <&> \case
    DKNote drum -> case drum of
      DKYellow -> [yellow]
      DKRed    -> [blue]
      DKPurple -> [yellow, blue]
      DKClap   -> [red, green]
    DKRoll{} -> []

  makeLane len = Wait 0 (Just LaneExpert) $ Wait len Nothing RNil
  singleLanes = U.trackJoin $ dk' <&> \case
    DKRoll DKYellow len -> makeLane len
    DKRoll DKRed    len -> makeLane len
    _                   -> RNil
  doubleLanes = U.trackJoin $ dk' <&> \case
    DKRoll DKPurple len -> makeLane len
    DKRoll DKClap   len -> makeLane len
    _                   -> RNil

  laneNotes = U.trackJoin $ dk' <&> \case
    DKRoll drum len -> let
      pattern = case drum of
        DKYellow -> [yellow]
        DKRed    -> [blue]
        DKPurple -> [blue, yellow]
        DKClap   -> [green, red]
      in U.trackTake len $ RTB.fromPairList $ zip (0 : repeat laneSpeed) $ cycle pattern
    DKNote _ -> RNil

  in mempty
    { difficulties = Map.singleton Expert mempty
      { gems = RTB.merge normalNotes laneNotes
      }
    , singleRoll = singleLanes
    , doubleRoll = doubleLanes
    }

-- Returns (high bongo, low bongo, clap)
loadBongoSounds :: IO ([(T.Text, FilePath)], [(T.Text, FilePath)], [(T.Text, FilePath)])
loadBongoSounds = do
  dir <- getResourcesPath "sfx"
  let list category = do
        let path = dir </> category
        ents <- filter (\x -> take 1 x /= ".") <$> listDirectory path
        return $ zip
          (map (\i -> T.pack $ category <> "-" <> show (i :: Int)) [1..])
          (map (path </>) ents)
  high <- list "bongo-high"
  low  <- list "bongo-low"
  clap <- list "clap"
  return (high, low, clap)
