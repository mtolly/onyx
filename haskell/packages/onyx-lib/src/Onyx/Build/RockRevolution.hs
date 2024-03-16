{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Build.RockRevolution (rrRules) where

import           Control.Monad.Codec
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Strict (execState)
import           Data.Binary.Put                  (runPut)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import qualified Data.Conduit.Audio               as CA
import           Data.Conduit.Audio.LAME          (sinkMP3WithHandle)
import qualified Data.Conduit.Audio.LAME.Binding  as L
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Functor.Identity            (Identity)
import           Data.Int                         (Int16)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Version                     (showVersion)
import           Data.Word                        (Word32)
import           Development.Shake                hiding (phony, (%>))
import           Development.Shake.FilePath
import           Onyx.Audio                       (clampIfSilent)
import           Onyx.Audio.FSB
import           Onyx.Audio.FSB.FEV
import           Onyx.Build.Common
import           Onyx.FFMPEG                      (ffSource)
import           Onyx.Guitar                      (noExtendedSustains',
                                                   noOpenNotes,
                                                   standardBlipThreshold)
import           Onyx.Import.RockRevolution
import           Onyx.MIDI.Common                 (Difficulty (..),
                                                   StrumHOPOTap (..),
                                                   blipEdgesRB_, makeEdgeCPV,
                                                   pattern RNil, pattern Wait)
import           Onyx.MIDI.Read
import qualified Onyx.MIDI.Track.Drums            as D
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.Mode
import           Onyx.Project                     hiding (Difficulty)
import           Onyx.StackTrace
import           Onyx.Util.Text.Decode            (encodeLatin1)
import           Onyx.Xbox.STFS                   (rrpkg)
import           Paths_onyx_lib                   (version)
import qualified Sound.MIDI.File                  as File
import qualified Sound.MIDI.File.Event            as Event
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           System.IO                        (IOMode (..), hFileSize,
                                                   withBinaryFile)

rrRules :: BuildInfo -> FilePath -> TargetRR FilePath -> QueueLog Rules ()
rrRules buildInfo dir rr = do

  let songYaml = biSongYaml buildInfo
      _rel = biRelative buildInfo
      gen = biGen buildInfo
      metadata = getTargetMetadata songYaml $ RR rr
      key = fromMaybe (error "need key!") rr.songID
      str = T.unpack key
      bytes = TE.encodeUtf8 key

      title  = getTitle  metadata
      artist = getArtist metadata

  (planName, plan) <- case getPlan rr.common.plan songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show rr
    Just pair -> return pair
  let planDir = gen $ "plan" </> T.unpack planName

  let file f = dir </> "files/data" </> f
      makeStringsBin strs fout = stackIO $ B.writeFile fout
        $ B.concat $ map (\t -> TE.encodeUtf8 t <> B.singleton 0) strs

  let songBinEnglish = file $ "English_s" <> str <> "_Strings.bin"
      songBinFrench  = file $ "French_s"  <> str <> "_Strings.bin"
      songBinSpanish = file $ "Spanish_s" <> str <> "_Strings.bin"
  -- songBinEnglish written with midis below
  songBinFrench  %> shk . copyFile' songBinEnglish
  songBinSpanish %> shk . copyFile' songBinEnglish

  let sFevSeq = file $ "s" <> str <> ".fev.seq"
      sFsbSeq = file $ "s" <> str <> ".fsb.seq"
      sDrumsFsbSeq = file $ "s" <> str <> "_Drums.fsb.seq"
  forM_ [sFevSeq, sFsbSeq, sDrumsFsbSeq] $ \f -> do
    f %> \out -> stackIO $ B.writeFile out (B.pack [1, 0, 0, 0])

  let rrPartDrums  = getPart rr.drums  songYaml >>= anyDrums
      rrPartGuitar = getPart rr.guitar songYaml >>= anyFiveFret
      rrPartBass   = getPart rr.bass   songYaml >>= anyFiveFret

      bass01   = file $ "s" <> str <> "_bass_01.mid"
      bass02   = file $ "s" <> str <> "_bass_02.mid"
      bass03   = file $ "s" <> str <> "_bass_03.mid"
      bass04   = file $ "s" <> str <> "_bass_04.mid"
      bass05   = file $ "s" <> str <> "_bass_05.mid"
      drums01  = file $ "s" <> str <> "_drums_01.mid"
      drums02  = file $ "s" <> str <> "_drums_02.mid"
      drums03  = file $ "s" <> str <> "_drums_03.mid"
      drums04  = file $ "s" <> str <> "_drums_04.mid"
      drums05  = file $ "s" <> str <> "_drums_05.mid"
      guitar01 = file $ "s" <> str <> "_guitar_01.mid"
      guitar02 = file $ "s" <> str <> "_guitar_02.mid"
      guitar03 = file $ "s" <> str <> "_guitar_03.mid"
      guitar04 = file $ "s" <> str <> "_guitar_04.mid"
      guitar05 = file $ "s" <> str <> "_guitar_05.mid"
      control  = file $ "s" <> str <> "_control.mid"
      allMidis =
        [   bass01,   bass02,   bass03,   bass04,   bass05
        ,  drums01,  drums02,  drums03,  drums04,  drums05
        , guitar01, guitar02, guitar03, guitar04, guitar05
        , control
        ]

  let loadOnyxMidi :: Staction (F.Song (F.OnyxFile U.Beats))
      loadOnyxMidi = F.shakeMIDI $ planDir </> "processed.mid"

  (songBinEnglish : allMidis) %> \_ -> do
    mid <- applyTargetMIDI rr.common <$> loadOnyxMidi
    let input partName = ModeInput
          { tempo  = F.s_tempos mid
          , events = F.onyxEvents $ F.s_tracks mid
          , part   = F.getFlexPart partName $ F.s_tracks mid
          }
        guitar = (\builder -> builder FiveTypeGuitar $ input rr.guitar) <$> rrPartGuitar
        bass   = (\builder -> builder FiveTypeGuitar $ input rr.bass  ) <$> rrPartBass
        drums  = (\builder -> builder drumTarget     $ input rr.drums ) <$> rrPartDrums
        drumTarget = if rr.is2xBassPedal then DrumTargetRB1x else DrumTargetRB2x

        toTrackMidi :: (ParseTrack f) => f U.Beats -> F.Song (RTB.T U.Beats (Event.T B.ByteString))
        toTrackMidi trk = mid
          { F.s_tracks = fmap (fmap encodeLatin1) $ execState (codecOut (forcePure parseTrack) trk) RTB.empty
          }

        forcePure
          :: TrackCodec (PureLog Identity) U.Beats (trk U.Beats)
          -> TrackCodec (PureLog Identity) U.Beats (trk U.Beats)
        forcePure = id

        emptyMid = mid { F.s_tracks = RTB.empty }

        makeGB :: Difficulty -> Maybe FiveResult -> File.T B.ByteString
        makeGB _    Nothing       = F.showMixedMIDI emptyMid
        makeGB diff (Just result) = let
          gems = fromMaybe RTB.empty $ Map.lookup diff result.notes
          in F.showMixedMIDI $ toTrackMidi $ (makeRRFiveDifficulty result gems)
            { rrfSolo = result.other.fiveSolo
            }

        makeDrums :: Difficulty -> File.T B.ByteString
        makeDrums diff = let
          gems = fromMaybe RTB.empty $ drums >>= \res -> Map.lookup diff res.notes
          in F.showMixedMIDI $ toTrackMidi $ (makeRRDrumDifficulty4Lane gems)
            { rrdSolo = maybe RTB.empty (\res -> res.other.drumSolo) drums
            }

    makeStringsBin
      [ artist
      , title
      -- custom practice sections here
      ] songBinEnglish

    stackIO $ Save.toFile bass01 $ makeGB Easy   bass
    stackIO $ Save.toFile bass02 $ makeGB Easy   bass
    stackIO $ Save.toFile bass03 $ makeGB Medium bass
    stackIO $ Save.toFile bass04 $ makeGB Hard   bass
    stackIO $ Save.toFile bass05 $ makeGB Expert bass

    stackIO $ Save.toFile guitar01 $ makeGB Easy   guitar
    stackIO $ Save.toFile guitar02 $ makeGB Easy   guitar
    stackIO $ Save.toFile guitar03 $ makeGB Medium guitar
    stackIO $ Save.toFile guitar04 $ makeGB Hard   guitar
    stackIO $ Save.toFile guitar05 $ makeGB Expert guitar

    stackIO $ Save.toFile drums01 $ makeDrums Easy
    stackIO $ Save.toFile drums02 $ makeDrums Easy
    stackIO $ Save.toFile drums03 $ makeDrums Medium
    stackIO $ Save.toFile drums04 $ makeDrums Hard
    stackIO $ Save.toFile drums05 $ makeDrums Expert

    -- TODO more stuff
    stackIO $ Save.toFile control $ F.showMixedMIDI mid
      { F.s_tracks = U.trackJoin $ fmap
        (\() -> Wait 0 (makeEdgeCPV 14 126 $ Just 96) $ Wait (1/8) (makeEdgeCPV 14 126 Nothing) RNil )
        (eventsEnd $ F.onyxEvents $ F.s_tracks mid)
      }

  let songLua = file $ "s" <> str <> ".lua"
  songLua %> \out -> do

    mid <- applyTargetMIDI rr.common <$> loadOnyxMidi
    let (pstart, pend) = previewBounds metadata mid 0 False
        showTimeParts :: Int -> T.Text
        showTimeParts ms = let
          (minutes, afterMinutes) = quotRem ms 60000
          (seconds, milliseconds) = quotRem afterMinutes 1000
          in T.pack $ "{ " <> show minutes <> ", " <> show seconds <> ", " <> show milliseconds <> " }"
    endMS <- case eventsEnd $ F.onyxEvents $ F.s_tracks mid of
      Wait dt _ _ -> return $ floor $ U.applyTempoMap (F.s_tempos mid) dt * 1000
      RNil        -> fatal "panic! no [end] in processed midi"

    let lua = T.intercalate "\r\n"
          [ "Sex = Sex or {}"
          , "Playback = Playback or {}"
          , "QuantiseLevel = QuantiseLevel or {}"
          , ""
          , "-- standard information"
          , "Year = " <> T.pack (show $ getYear songYaml.metadata)
          , ""
          , "KitId = 1002" -- hopefully this works
          , "SoloGuitarId = 2200" -- ?
          , "DrumPlayback = Playback.kMidi" -- ?
          , "DisplayArtist = false" -- ?
          , ""
          -- for official songs this is different from the filename song id?
          , "-- leaderboard - this MUST be unique"
          , "LeaderboardId = " <> key
          , ""
          -- TODO looks like these can be 1 through 5. never actually different per level
          , "-- star ratings for each difficulty level:"
          , "DrumDifficulty = { 0, 5, 5, 5, 5, 5 }"
          , "GuitarDifficulty = { 0, 5, 5, 5, 5, 5 }"
          , "BassDifficulty = { 0, 4, 4, 4, 4, 4 }"
          , ""
          -- TODO
          , "-- top score for each difficulty level:"
          , "DrumMaxScore = { 0, 332900, 480000, 881175, 1104675, 1377450 }"
          , "GuitarMaxScore = { 0, 358650, 542850, 820650, 1304250, 1550250 }"
          , "BassMaxScore = { 0, 446250, 539850, 881250, 1162350, 1386750 }"
          , ""
          -- shuffle is used on the main menu, whole song is fine
          , "-- FE music times = { min, sec, ms }"
          , "ShuffleStartTime = { 0, 0, 0 }"
          , "ShuffleStopTime = " <> showTimeParts endMS
          , "PreviewStartTime = " <> showTimeParts pstart
          , "PreviewStopTime = " <> showTimeParts pend
          , ""
          , "-- character information"
          , "SingerSex = Sex.kMale" -- TODO
          -- hopefully no lipsync is ok
          , ""
          -- ?
          , "DefaultDrummer = 10"
          , "DefaultBassist = 15"
          , "DefaultGuitarist = 9"
          , "DefaultStandin = 8"
          , ""
          , "-- Audio Mastering information"
          , "s" <> key <> " ="
          , "{"
          , "  ParentMix = \"MusicMix001\","
          , "  MixType = 2,"
          , "  Volume = 0,"
          , "  DSPs = {}"
          , "}"
          , ""
          , "s" <> key <> "_FE ="
          , "{"
          , "  ParentMix = \"FrontMixMusic001\","
          , "  MixType = 2,"
          , "  Volume = -7,"
          , "  DSPs = { \"FEEQ01\", \"FEEQ02\", \"FEMusicVerb\" }"
          , "}"
          , ""
          , "Quantise = QuantiseLevel.kNone"
          , ""
          ]

    stackIO $ B.writeFile out $ TE.encodeUtf8 lua

  dir </> "pad.txt" %> \out -> stackIO $ B.writeFile out "0" -- TODO

  let pathGuitar  = dir </> "guitar.mp3"
      pathBass    = dir </> "bass.mp3"
      pathDrums   = dir </> "drums.mp3"
      pathSong    = dir </> "song.mp3"
      pathFE      = dir </> "fe.mp3"
      rrParts = [rr.guitar, rr.bass, rr.drums]
      setup lame = liftIO $ do
        L.check $ L.setBrate lame 128
        L.check $ L.setQuality lame 5
        L.check $ L.setOutSamplerate lame 44100
      pathPad = dir </> "pad.txt"
      readPad :: Staction Int
      readPad = shk $ read <$> readFile' pathPad

  pathGuitar %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo rrParts rr.common mid 0 planName plan
      [(rr.guitar, 1)]
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3WithHandle out setup $ padAudio pad $ clampIfSilent s
  pathBass %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo rrParts rr.common mid 0 planName plan
      [(rr.bass, 1)]
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3WithHandle out setup $ padAudio pad $ clampIfSilent s
  pathDrums %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo rrParts rr.common mid 0 planName plan
      [(rr.drums, 1)]
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3WithHandle out setup $ padAudio pad $ clampIfSilent s
  pathSong %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceBacking buildInfo rr.common mid 0 planName plan
      [ (rr.guitar, 1)
      , (rr.bass  , 1)
      , (rr.drums , 1)
      ]
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3WithHandle out setup $ padAudio pad $ clampIfSilent s
  pathFE %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceBacking buildInfo rr.common mid 0 planName plan []
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3WithHandle out setup $ padAudio pad $ clampIfSilent s

  let songFev = file $ "s" <> str <> ".fev" -- makeMainFEV
      songFsb = file $ "s" <> str <> ".fsb"
      songDrumsFsb = file $ "s" <> str <> "_Drums.fsb"
      songFEFev = file $ "s" <> str <> "_FE.fev" -- makeFrontEndFEV
      songFEFsb = file $ "s" <> str <> "_FE.fsb"

  let testSize f = stackIO $ (>= 5000) <$> withBinaryFile f ReadMode hFileSize
      getAudio path = testSize path >>= \case
        False -> return Nothing
        True  -> Just <$> do
          bs <- stackIO $ BL.readFile path
          src <- stackIO $ ffSource $ Right path
          let _ = src :: CA.AudioSource (ResourceT IO) Int16
              size = round $ CA.framesToSeconds (CA.frames src) (CA.rate src) * 1000
          return (bs, size)

  [songFev, songFsb, songDrumsFsb] %> \_ -> do
    shk $ need [pathGuitar, pathBass, pathDrums, pathSong]
    maybeGuitar  <- getAudio pathGuitar
    maybeBass    <- getAudio pathBass
    maybeDrums   <- getAudio pathDrums
    maybeBacking <- getAudio pathSong
    stackIO $ mp3sToFSB3 (catMaybes
      [ (\(bs, _) -> ("onyx_guitar.mp3" , bs)) <$> maybeGuitar
      , (\(bs, _) -> ("onyx_bass.mp3"   , bs)) <$> maybeBass
      , (\(bs, _) -> ("onyx_backing.mp3", bs)) <$> maybeBacking
      ]) >>= BL.writeFile songFsb . emitFSB
    stackIO $ mp3sToFSB3 (catMaybes
      [ (\(bs, _) -> ("onyx_drums.mp3", bs)) <$> maybeDrums
      ]) >>= BL.writeFile songDrumsFsb . emitFSB
    let fev = makeMainFEV bytes (snd <$> maybeGuitar) (snd <$> maybeBass) (snd <$> maybeDrums) (snd <$> maybeBacking)
    stackIO $ BL.writeFile songFev $ runPut $ void $ codecOut binFEV fev

  [songFEFev, songFEFsb] %> \_ -> do
    shk $ need [pathFE]
    (feBytes, feSize) <- getAudio pathFE >>= \case
      Nothing   -> fatal "panic! couldn't produce rock revolution FE audio"
      Just pair -> return pair
    stackIO $ mp3sToFSB3 [("onyx_fe.mp3", feBytes)] >>= BL.writeFile songFEFsb . emitFSB
    let fev = makeFrontEndFEV bytes feSize
    stackIO $ BL.writeFile songFEFev $ runPut $ void $ codecOut binFEV fev

  dir </> "rrlive" %> \out -> do
    let files = concat
          [ [songBinEnglish, songBinFrench, songBinSpanish]
          , [sFevSeq, sFsbSeq, sDrumsFsbSeq]
          , allMidis
          , [songLua]
          , [songFev, songFsb, songDrumsFsb, songFEFev, songFEFsb]
          ]
    shk $ need files
    lg "# Producing Rock Revolution LIVE file"
    mapStackTraceT (mapQueueLog $ liftIO . runResourceT) $ rrpkg
      (artist <> " - " <> title)
      (T.pack $ "Compiled by Onyx Music Game Toolkit version " <> showVersion version)
      (dir </> "files")
      out

makeRRFiveDifficulty :: FiveResult -> RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats) -> RRFiveDifficulty U.Beats
makeRRFiveDifficulty result gems = RRFiveDifficulty
  -- TODO do hopo chords work? do they break?
  { rrfStrums = blipEdgesRB_ $ flip RTB.mapMaybe processed $ \case
    ((color, Strum), len) -> Just (color, len)
    _                     -> Nothing
  , rrfHOPOs = blipEdgesRB_ $ flip RTB.mapMaybe processed $ \case
    ((_    , Strum), _  ) -> Nothing
    ((color, _    ), len) -> Just (color, len)
  , rrfSolo = RTB.empty -- added later
  } where
    processed = noOpenNotes result.settings.detectMutedOpens
      $ noExtendedSustains' standardBlipThreshold gap gems
    gap = fromIntegral result.settings.sustainGap / 480

makeRRDrumDifficulty4Lane :: RTB.T U.Beats (D.Gem D.ProType, D.DrumVelocity) -> RRDrumDifficulty U.Beats
makeRRDrumDifficulty4Lane gems = RRDrumDifficulty
  { rrdGems      = flip fmap gems $ \case
    (D.Kick                 , _) -> (RR_Kick     , RRC_Kick   )
    (D.Red                  , _) -> (RR_Snare    , RRC_Snare  )
    (D.Pro D.Yellow D.Cymbal, _) -> (RR_HihatOpen, RRC_Hihat  )
    (D.Pro D.Blue   D.Cymbal, _) -> (RR_Ride     , RRC_HighTom)
    (D.Pro D.Green  D.Cymbal, _) -> (RR_Crash1   , RRC_CrashR )
    (D.Pro D.Yellow D.Tom   , _) -> (RR_Tom3     , RRC_Hihat  )
    (D.Pro D.Blue   D.Tom   , _) -> (RR_Tom4     , RRC_HighTom)
    (D.Pro D.Green  D.Tom   , _) -> (RR_Tom5     , RRC_CrashR )
    (D.Orange               , _) -> (RR_China    , RRC_CrashR )
  , rrdHidden    = RTB.empty
  , rrdFreestyle = RTB.empty -- TODO
  , rrdSolo      = RTB.empty -- added later
  }

makeMainFEV :: B.ByteString -> Maybe Word32 -> Maybe Word32 -> Maybe Word32 -> Maybe Word32 -> FEV
makeMainFEV songID timeGuitar timeBass timeDrums timeBacking = FEV
  { version = FEVVersion26
  , projectName = "s" <> songID
  , waveBanks =
    [ WaveBank
      { bankType = 512
      , bankMaxStreams = 32
      , bankName = "s" <> songID
      }
    , WaveBank
      { bankType = 512
      , bankMaxStreams = 32
      , bankName = "s" <> songID <> "_Drums"
      }
    ]
  , topLevelEventCategory = EventCategory
    { name = "master"
    , volumeFieldRatio = 1.0
    , pitch = 0.0
    , maxPlaybacks = Nothing
    , maxPlaybackBehavior = Nothing
    , subcategories =
      [ EventCategory
        { name = "music"
        , volumeFieldRatio = 1.0
        , pitch = 0.0
        , maxPlaybacks = Nothing
        , maxPlaybackBehavior = Nothing
        , subcategories = []
        }
      , EventCategory
        { name = "s" <> songID
        , volumeFieldRatio = 1.0
        , pitch = 0.0
        , maxPlaybacks = Nothing
        , maxPlaybackBehavior = Nothing
        , subcategories = do
          name <- ["Guitar", "Bass", "Drums", "Backing"]
          return EventCategory
            { name = name
            , volumeFieldRatio = 1.0
            , pitch = 0.0
            , maxPlaybacks = Nothing
            , maxPlaybackBehavior = Nothing
            , subcategories = []
            }
        }
      ]
    }
  , topLevelEventGroups =
    [ EventGroup
      { name = "Stems"
      , userProperties = []
      , subgroups = []
      , events = let
        event eventName volName soundDefName categoryName = Event
          { name = eventName
          , unk1 = 1.0
          , unk2 = 0
          , unk3 = 0
          , unk4 = 0
          , unk5 = 0
          , unk6 = 1
          , unk7 = 8
          , unk8 = 24
          , unk9 = 1.0
          , unk10 = 10000.0
          , unk11 = 0
          , unk12 = 8
          , unk13 = [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 360.0, 360.0, 1.0]
          , maxPlaybackBehavior = 1
          , unk14 = [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0]
          , layers =
            [ EventLayer
              { name = Just eventName
              , magic = 2
              , priority = -1
              , controlParameter = Left volName
              , soundDefInstances =
                [ SoundDefInstance
                  { nameOrIndex = Left soundDefName
                  , soundStart = 0.0
                  , soundLength = 1.0
                  , unk1 = 0
                  , unk2 = 1
                  , unk3 = -1
                  , padding = 0
                  , loopCount = 0
                  , autopitchEnabled = 0
                  , autopitchReferencePoint = 0
                  , autopitchAtMin = 0
                  , fineTune = 0
                  , volume = 1.0
                  , crossfadeInLength = -1.0
                  , crossfadeOutLength = -1.0
                  , crossfadeInType = 2
                  , crossfadeOutType = 2
                  }
                ]
              , envelopes =
                [ Envelope
                  { envelopeID = Just "env000"
                  , parent = Left ""
                  , name = "Volume"
                  , unk1 = 0
                  , unk2 = 0
                  , points =
                    [ Point
                      { x = 0.0
                      , y = 0.0
                      , curveShapeType = 2
                      }
                    , Point
                      { x = 1.0
                      , y = 1.0
                      , curveShapeType = 2
                      }
                    ]
                  , unk3 = 0
                  , unk4 = 0
                  }
                ]
              }
            ]
          , parameters =
            [ Parameter
              { name = volName
              , velocity = 0.0
              , paramMin = 0.0
              , paramMax = 1.0
              , flagsType = 3
              , seekSpeed = 0.0
              , numEnvelopesControlled = 1
              , unk0 = 0
              }
            ]
          , unk15 = 0
          , parentEventCategoryNames = [ categoryName ]
          }
        in  [ event "Drums"   "Drums_Vol"   ("s" <> songID <> "_drums"   ) ("s" <> songID <> "/Drums"  )
            , event "Guitar"  "Guitar_Vol"  ("s" <> songID <> "_guitar"  ) ("s" <> songID <> "/Guitar" )
            , event "Bass"    "Bass_Vol"    ("s" <> songID <> "_bass"    ) ("s" <> songID <> "/Bass"   )
            , event "Backing" "Backing_Vol" ("s" <> songID <> "_MixMinus") ("s" <> songID <> "/Backing")
            ]
      }
    ]
  , soundDefs = let
    blankDef = SoundDef
      { name = ""
      , unk1 = 3
      , unk2 = 0
      , unk3 = 0
      , unk4 = 1
      , unk5 = 1.0
      , unk6 = 1
      , unk7 = 1.0
      , unk8 = 1.0
      , unk9 = 1.0
      , unk10 = [0, 1, 0, 0, 0]
      , waveforms = []
      }
    in catMaybes
      [ flip fmap timeGuitar $ \playtime -> blankDef
        { name = "s" <> songID <> "_guitar"
        , waveforms =
          [ Waveform
            { padding = 0
            , weight = 100
            , name = "../../stems/s" <> songID <> "/s" <> songID <> "_guitar.wav"
            , bankName = "s" <> songID
            , indexInBank = 0
            , playtime = playtime
            }
          ]
        }
      , flip fmap timeBass $ \playtime -> blankDef
        { name = "s" <> songID <> "_bass"
        , waveforms =
          [ Waveform
            { padding = 0
            , weight = 100
            , name = "../../stems/s" <> songID <> "/s" <> songID <> "_bass.wav"
            , bankName = "s" <> songID
            , indexInBank = fromIntegral $ length $ filter isJust [timeGuitar]
            , playtime = playtime
            }
          ]
        }
      , flip fmap timeDrums $ \playtime -> blankDef
        { name = "s" <> songID <> "_drums"
        , waveforms =
          [ Waveform
            { padding = 0
            , weight = 100
            , name = "../../stems/s" <> songID <> "/s" <> songID <> "_drums.wav"
            , bankName = "s" <> songID <> "_Drums"
            , indexInBank = 0
            , playtime = playtime
            }
          ]
        }
      , flip fmap timeBacking $ \playtime -> blankDef
        { name = "s" <> songID <> "_MixMinus"
        , waveforms =
          [ Waveform
            { padding = 0
            , weight = 100
            , name = "../../stems/s" <> songID <> "/s" <> songID <> "_MixMinus.wav"
            , bankName = "s" <> songID
            , indexInBank = fromIntegral $ length $ filter isJust [timeGuitar, timeBass]
            , playtime = playtime
            }
          ]
        }
      ]
  , reverbs = []
  }

makeFrontEndFEV :: B.ByteString -> Word32 -> FEV
makeFrontEndFEV songID playtime = FEV
  { version = FEVVersion26
  , projectName = "s" <> songID <> "_FE"
  , waveBanks =
    [ WaveBank
      { bankType = 128
      , bankMaxStreams = 32
      , bankName = "s" <> songID <> "_FE"
      }
    ]
  , topLevelEventCategory = EventCategory
    { name = "master"
    , volumeFieldRatio = 1.0
    , pitch = 0.0
    , maxPlaybacks = Nothing
    , maxPlaybackBehavior = Nothing
    , subcategories = do
      name <- ["music", "s" <> songID <> "_FE"]
      return EventCategory
        { name = name
        , volumeFieldRatio = 1.0
        , pitch = 0.0
        , maxPlaybacks = Nothing
        , maxPlaybackBehavior = Nothing
        , subcategories = []
        }
    }
  , topLevelEventGroups =
    [ EventGroup
      { name = "Song"
      , userProperties = []
      , subgroups = []
      , events =
        [ Event
          { name = "Song"
          , unk1 = 1.0
          , unk2 = 0
          , unk3 = 0
          , unk4 = 0
          , unk5 = 128
          , unk6 = 1
          , unk7 = 8
          , unk8 = 24
          , unk9 = 1.0
          , unk10 = 10000.0
          , unk11 = 0
          , unk12 = 8
          , unk13 = [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 360.0, 360.0, 1.0]
          , maxPlaybackBehavior = 1
          , unk14 = [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0]
          , layers =
            [ EventLayer
              { name = Just "layer00"
              , magic = 2
              , priority = -1
              , controlParameter = Left ""
              , soundDefInstances =
                [ SoundDefInstance
                  { nameOrIndex = Left $ "s" <> songID <> "_FE"
                  , soundStart = 0.0
                  , soundLength = 1.0
                  , unk1 = 0
                  , unk2 = 1
                  , unk3 = -1
                  , padding = 0
                  , loopCount = 0
                  , autopitchEnabled = 0
                  , autopitchReferencePoint = 0
                  , autopitchAtMin = 0
                  , fineTune = 0
                  , volume = 1.0
                  , crossfadeInLength = -1.0
                  , crossfadeOutLength = -1.0
                  , crossfadeInType = 2
                  , crossfadeOutType = 2
                  }
                ]
              , envelopes = []
              }
            ]
          , parameters = []
          , unk15 = 0
          , parentEventCategoryNames = [ "s" <> songID <> "_FE" ]
          }
        ]
      }
    ]
  , soundDefs =
    [ SoundDef
      { name = "s" <> songID <> "_FE"
      , unk1 = 3
      , unk2 = 0
      , unk3 = 0
      , unk4 = 1
      , unk5 = 1.0
      , unk6 = 1
      , unk7 = 1.0
      , unk8 = 1.0
      , unk9 = 1.0
      , unk10 = [0, 1, 0, 0, 0]
      , waveforms =
        [ Waveform
          { padding = 0
          , weight = 100
          , name = "../../stems/s" <> songID <> "/s" <> songID <> "_FE.wav"
          , bankName = "s" <> songID <> "_FE"
          , indexInBank = 0
          , playtime = playtime
          }
        ]
      }
    ]
  , reverbs = []
  }
