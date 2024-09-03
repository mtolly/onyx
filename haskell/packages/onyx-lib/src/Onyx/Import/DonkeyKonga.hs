{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.DonkeyKonga where

import           Control.Monad                    (forM)
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isDigit)
import           Data.Conduit                     (yield)
import qualified Data.Conduit.Audio               as CA
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
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
import qualified Sound.MIDI.Util                  as U
import           System.Directory                 (listDirectory)
import           System.FilePath                  ((<.>), (</>))

supportedDKGames :: (SendMessage m, MonadIO m) => [(B.ByteString, Readable -> StackTraceT m [Import m])]
supportedDKGames =
  [ ("GKGJ01", importDK1 ) -- dk1 japan
  , ("GKGE01", importDK1 ) -- dk1 us
  , ("GKGP01", importDK1 ) -- dk1 europe
  , ("GY2J01", importDK23) -- dk2 japan
  , ("GY2E01", importDK23) -- dk2 us
  , ("GY2P01", importDK23) -- dk2 europe
  , ("GY3J01", importDK23) -- dk3 japan
  ]

-- Imports from Donkey Konga 1 (J/U/E) .iso (GCM)
importDK1 :: (SendMessage m, MonadIO m) => Readable -> StackTraceT m [Import m]
importDK1 gcm = do
  tree <- stackIO $ loadGCM gcm
  chartFolder <- case findFolder ["Game", "Sheet", "Solo"] tree of
    Nothing -> fatal "Couldn't find charts folder"
    Just d  -> return d
  audioFolder <- case findFolder ["stream", "score"] tree of
    Nothing -> fatal "Couldn't find audio folder"
    Just d  -> return d
  -- not sure if my GCM code is wrong; in Japanese DK1 .nkit.iso, files alphabetically
  -- after "CVS" come after the CVS folder entry so they are inside?
  let chartFiles = folderFiles chartFolder <> maybe [] folderFiles (findFolder ["CVS"] chartFolder)
      audioFiles = folderFiles audioFolder <> maybe [] folderFiles (findFolder ["CVS"] audioFolder)
  fmap concat $ forM chartFiles $ \(name, bin) -> case B.stripSuffix "_h.bin" name of
    Nothing -> return []
    Just chartName -> let
      -- have to manually match up .bin and .dsp names since they aren't the same
      matchName = if B8.any isDigit $ B.take 1 chartName
        then let
          -- US version: all .bin/.dsp start with matching numbers, suffixes may differ
          number = B8.takeWhile isDigit chartName
          in \(audioName, _) -> number `B.isPrefixOf` audioName
        else let
          -- Japanese version: no numbers, most are same but some differ
          targetAudioName = case chartName of
            "asueno"   -> "asuheno.dsp"
            "clarifin" -> "clari.dsp"
            "tokei"    -> "furudokei.dsp"
            x          -> x <> ".dsp"
          in \(audioName, _) -> audioName == targetAudioName
      in case filter matchName audioFiles of
        [] -> do
          warn $ "Couldn't locate matching audio for chart: " <> show name
          return []
        (_, dsp) : _ -> return [importDKSong chartName False bin dsp]

-- Imports from Donkey Konga 2 (J/U/E) or Donkey Konga 3 (J) .iso (GCM)
importDK23 :: (SendMessage m, MonadIO m) => Readable -> StackTraceT m [Import m]
importDK23 gcm = do
  tree <- stackIO $ loadGCM gcm
  -- song info file not present in japanese dk2
  songInfo <- case findFile ("Resource" :| ["SongInfo.res"]) tree of
    Nothing -> return Nothing
    Just r  -> Just . readSongInfo . BL.toStrict <$> stackIO (useHandle r handleToByteString)
  chartFolder <- case findFolder ["Score"] tree of
    Nothing -> fatal "Couldn't find charts folder"
    Just d  -> return d
  audioFolder <- case findFolder ["stream", "score"] tree of
    Nothing -> fatal "Couldn't find audio folder"
    Just d  -> return d
  let chartFiles = folderFiles chartFolder <> maybe [] folderFiles (findFolder ["CVS"] chartFolder)
      audioFiles = folderFiles audioFolder <> maybe [] folderFiles (findFolder ["CVS"] audioFolder)
      allSongIDs = case songInfo of
        Nothing   -> mapMaybe (\(name, _) -> B.stripSuffix "_1x.mid" name) chartFiles
        Just info -> map (.info_FILENAME) $ filter (\s -> s.info_PRICE /= Just 0) info
  forM allSongIDs $ \songID -> do
    let chartFilename = songID <> "_1x.mid"
        audioFilename = B.take 2 songID <> ".dsp"
    mid <- case lookup chartFilename chartFiles of
      Nothing -> fatal $ "Couldn't find chart file: " <> show chartFilename
      Just r  -> return r
    dsp <- case lookup audioFilename audioFiles of
      Nothing -> fatal $ "Couldn't find audio file: " <> show audioFilename
      Just r  -> return r
    return $ importDKSong songID True mid dsp

-- got these from random webpages, should probably get exact disc info + add artists and other data
dk1TitleLookup :: [(B.ByteString, T.Text)]
dk1TitleLookup =

  -- Japan songs
  [ ("advance" , "アドバンス・アドベンチャー")
  , ("ashita"  , "明日があるさ")
  , ("asueno"  , "明日への扉")
  , ("clarifin", "I Broke My Clarinet")
  , ("colors"  , "Colors")
  , ("dance"   , "恋のダンスサイト")
  , ("desire"  , "DESIRE - 情熱")
  , ("dktheme" , "Donkey Konga")
  , ("fly"     , "Fly High (Fly High)")
  , ("ginga"   , "The Galaxy Express 999")
  , ("hamutaro", "ハム太郎とっとこうた")
  , ("hungary" , "Hungarian Dance No. 2")
  , ("hyoutan" , "ひょっこりひょうたん島")
  , ("janken"  , "ミニモニ。ジャンケンぴょん！")
  , ("kirby"   , "カービィ！")
  , ("labamba" , "La Bamba")
  , ("lalala"  , "風のららら")
  , ("mambo"   , "Mambo No. 5")
  , ("mario"   , "Super Mario Theme")
  , ("masque"  , "Mas Que Nada")
  , ("mataaeru", "またあえる日まで")
  , ("momoiro" , "♥ 桃色片思い ♥")
  , ("morikuma", "I Met a Bear")
  , ("okula"   , "Oklahoma Mixer")
  , ("pikmin"  , "愛の歌")
  , ("rap"     , "Monkey Rap")
  , ("shake"   , "Shake")
  , ("somebody", "Love Somebody")
  , ("stafy"   , "伝説のスタフィー")
  , ("tokei"   , "My Grandfather's Clock")
  , ("turkey"  , "Turkish March")
  , ("weare"   , "Ｗｅ　ａｒｅ　ｔｈｅ　ＯＮＥ～僕らはひとつ～")

  -- USA songs
  , ("01sing"          , "Sing, Sing, Sing (With a Swing)")
  , ("02bingo"         , "Bingo")
  , ("03medleya"       , "Diddy's Ditties")
  , ("04medleyb"       , "Campfire Medley")
  , ("05kirby"         , "Kirby:  Right Back At Ya!")
  , ("06pokemon"       , "Pokémon Theme")
  , ("07shining"       , "Shining Star")
  , ("08likewow"       , "Like Wow")
  , ("09locomo"        , "The Loco-Motion")
  , ("10louie"         , "Louie Louie")
  , ("11wild"          , "Wild Thing")
  , ("12hungary"       , "Hungarian Dance No. 5 in G Minor")
  , ("13on"            , "On the Road Again")
  , ("14busy"          , "Busy Child")
  , ("15rockyou"       , "We Will Rock You")
  , ("16oye"           , "Oye Como Va")
  , ("17dancin"        , "Dancing in the Street")
  , ("18youcant"       , "You Can't Hurry Love")
  , ("19lobster"       , "Rock Lobster")
  , ("20rockthis"      , "Rock This Town")
  , ("21right"         , "Right Here, Right Now")
  , ("22whatilike"     , "What I Like About You")
  , ("23whipit"        , "Whip It")
  , ("24paralos"       , "Para Los Rumberos")
  , ("25impression"    , "The Impression That I Get")
  , ("26dktheme"       , "Donkey Konga Theme")
  , ("27rap"           , "DK Rap")
  , ("28mario"         , "Mario Bros. Theme")
  , ("29zelda"         , "The Legend of Zelda Theme")
  , ("30stupid"        , "Stupid Cupid")
  , ("31allthesm"      , "All The Small Things")
  , ("32ithinkiloveyou", "I Think I Love You")
  , ("33turkey"        , "Turkish March")

  -- Europe songs (minus ones in US list)
  , ("02alright"  , "Alright")
  , ("03dont"     , "Don't Stop Me Now")
  , ("04lady"     , "Lady Marmalade")
  , ("05red"      , "99 Red Balloons")
  , ("06iwantu"   , "I Want You Back")
  , ("07tubthump" , "Tubthumping")
  , ("08back"     , "Back for Good")
  , ("10september", "September")
  , ("13rechard"  , "Richard III")
  , ("15louie"    , "Louie Louie")
  , ("19canned"   , "Canned Heat")
  , ("20cosmic"   , "Cosmic Girl")
  , ("21battlef"  , "Super Smash Bros. Melee Opening")
  , ("22dkcountry", "Donkey Kong Country Theme")
  , ("23rcruise"  , "Rainbow Cruise")
  , ("30turkey"   , "Turkish March")

  ]

-- TODO support passing SongInfo data
-- TODO figure out dk2/dk3 offset issue
importDKSong :: (SendMessage m, MonadIO m) => B.ByteString -> Bool -> Readable -> Readable -> Import m
importDKSong chartName isMidi chartFile dsp level = do

  (soundsHigh, soundsLow, soundsClap) <- case level of
    ImportQuick -> return ([], [], [])
    ImportFull  -> stackIO loadBongoSounds
  let panSounds pan vol = mapM $ \(name, path) -> do
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

  (tempos, chart) <- case level of
    ImportQuick -> return (U.makeTempoMap RTB.empty, RTB.empty)
    ImportFull -> if isMidi
      then do
        mid <- F.loadMIDIReadable chartFile
        let _ = mid :: F.Song (F.RawFile U.Beats)
            dk = readTrackDK2 $ U.applyTempoTrack mid.tempos $ case mid.tracks.rawTracks of
              t : _ -> t
              []    -> RTB.empty
        return (mid.tempos, dk)
      else do
        b <- stackIO $ useHandle chartFile handleToByteString
        (tempos, events) <- fmap binToMidi $ runGetM readSheetBin b
        return (tempos, interpretDK1 events)
  let converted = mapTrack (U.unapplyTempoTrack tempos) $ convertDKDrums chart
      roundRobin = zip3 (cycle $ map fst soundsHigh) (cycle $ map fst soundsLow) (cycle $ map fst soundsClap)
      bongoSampleTrack = F.SamplesTrack
        { F.sampleTriggers
          = RTB.flatten
          $ RTB.fromPairList
          $ map (\((ahigh, alow, aclap), (dt, instant)) -> let
              drums = map fst instant
              in (dt, concat
                [ [F.SampleTrigger "" ahigh | elem (Pro Yellow ()) drums]
                , [F.SampleTrigger "" alow  | elem (Pro Blue   ()) drums]
                , [F.SampleTrigger "" aclap | elem Red drums || elem (Pro Green ()) drums]
                ])
            )
          $ zip roundRobin
          $ RTB.toPairList
          $ RTB.collectCoincident
          $ (.drumGems)
          $ fromMaybe mempty $ Map.lookup Expert converted.drumDifficulties
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
      { title = Just $ fromMaybe (TE.decodeLatin1 chartName) $ lookup chartName dk1TitleLookup
      , fileAlbumArt = Nothing
      }
    , global = def'
      { backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      , fileMidi = SoftFile "notes.mid" $ SoftChart $ F.Song
        { F.tempos = tempos
        , F.timesigs = U.measureMapFromLengths U.Error $ RTB.singleton 0 4
        , F.tracks = mempty
          { F.onyxParts = Map.singleton F.FlexDrums mempty
            { F.onyxPartDrums = converted
            }
          , F.onyxSamples = Map.singleton "bongos" bongoSampleTrack
          }
        }
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
        { song        = Just $ PansVols [-1, 1] [-2, -2] $ Input $ Named "song"
        , parts       = Parts $ HM.singleton F.FlexDrums $ PartSingle $ Input $ Named "bongos"
        , crowd       = Nothing
        , comments    = []
        , tuningCents = 0
        , fileTempo   = Nothing
        }
    , targets = HM.empty
    , parts = Parts $ HM.singleton F.FlexDrums (emptyPart :: Part SoftFile)
      { drums = Just $ emptyPartDrums Drums4 Kicks1x
      }
    }

convertDKDrums :: RTB.T U.Seconds (DKEvent U.Seconds) -> DrumTrack U.Seconds
convertDKDrums dk = let

  red    = (Red          , VelocityNormal)
  yellow = (Pro Yellow (), VelocityNormal)
  blue   = (Pro Blue   (), VelocityNormal)
  green  = (Pro Green  (), VelocityNormal)

  laneSpeed = 0.080 :: U.Seconds -- 80 ms for consistent combo
  -- make lanes into single notes if they're too short to have 2 notes
  dk' = flip fmap dk $ \case
    DKRoll drum t | t <= laneSpeed -> DKNote drum
    event                          -> event

  normalNotes = RTB.flatten $ flip fmap dk' $ \case
    DKNote drum -> case drum of
      DKYellow -> [yellow]
      DKRed    -> [blue]
      DKPurple -> [yellow, blue]
      DKClap   -> [red, green]
    DKRoll{} -> []

  makeLane len = Wait 0 (Just LaneExpert) $ Wait len Nothing RNil
  singleLanes = U.trackJoin $ flip fmap dk' $ \case
    DKRoll DKYellow len -> makeLane len
    DKRoll DKRed    len -> makeLane len
    _                   -> RNil
  doubleLanes = U.trackJoin $ flip fmap dk' $ \case
    DKRoll DKPurple len -> makeLane len
    DKRoll DKClap   len -> makeLane len
    _                   -> RNil

  laneNotes = U.trackJoin $ flip fmap dk' $ \case
    DKRoll drum len -> let
      pattern = case drum of
        DKYellow -> [yellow]
        DKRed    -> [blue]
        DKPurple -> [blue, yellow]
        DKClap   -> [green, red]
      in U.trackTake len $ RTB.fromPairList $ zip (0 : repeat laneSpeed) $ cycle pattern
    DKNote _ -> RNil

  in mempty
    { drumDifficulties = Map.singleton Expert mempty
      { drumGems = RTB.merge normalNotes laneNotes
      }
    , drumSingleRoll = singleLanes
    , drumDoubleRoll = doubleLanes
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
