{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module Onyx.Import.DTXMania where

import           Control.Monad.Extra              (forM, guard, unless)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Char                        (toLower)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import           Onyx.Audio
import           Onyx.DTXMania.DTX
import           Onyx.DTXMania.Set
import           Onyx.Guitar                      (emit5')
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..),
                                                   StrumHOPOTap (..),
                                                   pattern RNil, pattern Wait)
import           Onyx.MIDI.Track.Drums            (DrumVelocity (..))
import qualified Onyx.MIDI.Track.Drums.Full       as FD
import           Onyx.MIDI.Track.File             (FlexPartName (..))
import qualified Onyx.MIDI.Track.File             as RBFile
import           Onyx.MIDI.Track.FiveFret
import           Onyx.Project                     hiding (TargetDTX (..))
import           Onyx.StackTrace
import           Onyx.Util.Handle                 (fileReadable)
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.FilePath                  (takeDirectory, takeExtension,
                                                   (<.>), (</>))

dtxConvertDrums, dtxConvertGuitar, dtxConvertBass
  :: DTX -> RBFile.Song (RBFile.OnyxFile U.Beats) -> RBFile.Song (RBFile.OnyxFile U.Beats)
dtxConvertDrums dtx (RBFile.Song tmap mmap onyx) = let
  importFullDrums notes = mempty
    { FD.fdDifficulties = Map.singleton Expert FD.FullDrumDifficulty
      { FD.fdGems = flip RTB.mapMaybe notes $ \case
        HihatClose -> Just (FD.Hihat    , FD.GemHihatClosed, VelocityNormal)
        Snare      -> Just (FD.Snare    , FD.GemNormal     , VelocityNormal)
        BassDrum   -> Just (FD.Kick     , FD.GemNormal     , VelocityNormal)
        HighTom    -> Just (FD.Tom1     , FD.GemNormal     , VelocityNormal)
        LowTom     -> Just (FD.Tom2     , FD.GemNormal     , VelocityNormal)
        Cymbal     -> Just (FD.CrashR   , FD.GemNormal     , VelocityNormal)
        FloorTom   -> Just (FD.Tom3     , FD.GemNormal     , VelocityNormal)
        HihatOpen  -> Just (FD.Hihat    , FD.GemHihatOpen  , VelocityNormal)
        RideCymbal -> Just (FD.Ride     , FD.GemNormal     , VelocityNormal)
        LeftCymbal -> Just (FD.CrashL   , FD.GemNormal     , VelocityNormal)
        LeftPedal  -> Just (FD.HihatFoot, FD.GemNormal     , VelocityNormal)
        LeftBass   -> Nothing
      , FD.fdFlam = RTB.empty
      }
    , FD.fdKick2 = RTB.mapMaybe (\case LeftBass -> Just (); _ -> Nothing) notes
    }
  in RBFile.Song tmap mmap $ RBFile.editOnyxPart FlexDrums
    (\opart -> opart { RBFile.onyxPartFullDrums = importFullDrums $ fmap fst $ dtx_Drums dtx })
    onyx
dtxConvertGuitar = dtxConvertGB dtx_Guitar $ \onyx five -> RBFile.editOnyxPart
  FlexGuitar
  (\opart -> opart { RBFile.onyxPartGuitar = five })
  onyx
dtxConvertBass   = dtxConvertGB dtx_Bass   $ \onyx five -> RBFile.editOnyxPart
  FlexBass
  (\opart -> opart { RBFile.onyxPartGuitar = five })
  onyx

dtxConvertGB
  :: (DTX -> RTB.T U.Beats ([Color], Chip))
  -> (f U.Beats -> FiveTrack U.Beats -> f U.Beats)
  -> DTX
  -> RBFile.Song (f U.Beats)
  -> RBFile.Song (f U.Beats)
dtxConvertGB getter setter dtx (RBFile.Song tmap mmap fixed) = let
  -- TODO we should be able to add sustains by looking at the length of audio chips
  guitarToFive notes = mempty
    { fiveDifficulties = Map.singleton Expert $ emit5' $ RTB.flatten $ flip fmap notes $ \case
      [] -> [((Nothing, Strum), Nothing)]
      xs -> [((Just x , Strum), Nothing) | x <- xs ]
    }
  in RBFile.Song tmap mmap $ setter fixed $ guitarToFive $ fmap fst $ getter dtx

dtxBaseMIDI :: DTX -> RBFile.Song (RBFile.OnyxFile U.Beats)
dtxBaseMIDI dtx = RBFile.Song (dtx_TempoMap dtx) (dtx_MeasureMap dtx) mempty

dtxAddChipAudio :: (SendMessage m, MonadIO m) => DTX -> FilePath -> StackTraceT m (SongYaml SoftFile -> SongYaml SoftFile)
dtxAddChipAudio dtx fin = do
  audio <- fmap catMaybes $ forM (HM.toList $ dtx_WAV dtx) $ \(chip, fp) -> do
    msrc <- dtxAudioSource $ takeDirectory fin </> fp -- already fixed backslashes and yens in readDTXLines
    return $ flip fmap msrc $ \src -> let
      -- could be smarter about this (apply volume later) but this works
      adjustVolume = case fromMaybe 100 $ HM.lookup chip $ dtx_VOLUME dtx of
        100 -> id
        vol -> CA.gain $ realToFrac vol / 100
      fixMono = case CA.channels src of
        1 -> applyPansVols
          [maybe 0 (\n -> realToFrac n / 100) $ HM.lookup chip $ dtx_PAN dtx]
          [0]
        _ -> id
      in (chip, AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just $ SoftFile ("samples" </> T.unpack chip <.> "wav") $ SoftAudio
          $ fixMono $ adjustVolume src
        , _rate = Nothing
        , _channels = 2
        })
  return $ \songYaml -> songYaml { _audio = HM.fromList audio }

dtxMakeAudioPlan :: DTX
  -> (SongYaml SoftFile, RBFile.Song (RBFile.OnyxFile U.Beats))
  -> (SongYaml SoftFile, RBFile.Song (RBFile.OnyxFile U.Beats))
dtxMakeAudioPlan dtx (songYaml, mid) = let
  foundChips = HM.keysSet $ _audio songYaml
  audioForChipsOverlap name chips = if RTB.null chips
    then (Nothing, Nothing)
    else let
      poly = SamplesInfo
        { _groupPolyphony = Nothing -- unlimited
        , _groupCrossfade = 0.002 -- doesn't matter
        }
      audios = AudioSamples poly
      track = RBFile.SamplesTrack
        $ fmap (\chip -> RBFile.SampleTrigger "" chip)
        -- don't include sample if we didn't find its audio
        $ RTB.filter (\chip -> HS.member chip foundChips) chips
      in (Just (name, audios), Just (name, track))
  audioForChipGroups name chips = if all (RTB.null . snd) chips
    then (Nothing, Nothing)
    else let
      poly = SamplesInfo
        { _groupPolyphony = Just 1
        , _groupCrossfade = 0.002
        }
      audios = AudioSamples poly
      track = RBFile.SamplesTrack $ foldr RTB.merge RTB.empty $ do
        (groupName, groupChips) <- chips
        return
          $ fmap (RBFile.SampleTrigger groupName)
          -- don't include sample if we didn't find its audio
          $ RTB.filter (\chip -> HS.member chip foundChips) groupChips
      in (Just (name, audios), Just (name, track))
  getDrumChipGroup (groupName, lanes)
    = (groupName, RTB.mapMaybe (\(l, chip) -> guard (elem l lanes) >> Just chip) $ dtx_Drums dtx)
  (songAudio, songTrack) = audioForChipsOverlap "audio-song" $ dtx_BGM dtx
  (guitarAudio, guitarTrack) = audioForChipGroups "audio-guitar" [("", snd <$> dtx_Guitar dtx)]
  (bassAudio, bassTrack) = audioForChipGroups "audio-bass" [("", snd <$> dtx_Bass dtx)]
  (kickAudio, kickTrack) = audioForChipGroups "audio-kick" $ map getDrumChipGroup
    [("", [BassDrum, LeftBass])]
  (snareAudio, snareTrack) = audioForChipGroups "audio-snare" $ map getDrumChipGroup
    [("", [Snare])]
  (kitAudio, kitTrack) = audioForChipGroups "audio-kit" $ map getDrumChipGroup
    [ ("HH", [HihatClose, HihatOpen, LeftPedal])
    , ("HT", [HighTom])
    , ("LT", [LowTom])
    , ("CY", [Cymbal])
    , ("FT", [FloorTom])
    , ("RC", [RideCymbal])
    , ("LC", [LeftCymbal])
    ]
  extraResults = flip map (HM.toList $ dtx_BGMExtra dtx) $ \(chan, chips) ->
    audioForChipGroups ("audio-extra-" <> chan) [("", chips)]
  audioExpr name = PlanAudio
    { _planExpr = Input $ Named name
    , _planPans = []
    , _planVols = []
    }
  audiosExpr names = PlanAudio
    { _planExpr = Mix $ fmap (Input . Named) names
    , _planPans = []
    , _planVols = []
    }
  songYaml' = songYaml
    { _audio = HM.union (_audio songYaml) $ HM.fromList $ catMaybes
      $ [songAudio, guitarAudio, bassAudio, kickAudio, snareAudio, kitAudio]
      <> map fst extraResults
    , _plans = HM.singleton "dtx" Plan
      { _song         = flip fmap songAudio $ \(name, _) -> audioExpr name
      , _countin      = Countin []
      , _planParts    = Parts $ HM.fromList $ concat
        [ toList $ flip fmap guitarAudio $ \(name, _) -> (FlexGuitar, PartSingle $ audioExpr name)
        , toList $ flip fmap bassAudio $ \(name, _) -> (FlexBass, PartSingle $ audioExpr name)
        , toList $ (FlexDrums ,) <$> case kitAudio of
          Just (name, _) -> Just PartDrumKit
            { drumsSplitKick = flip fmap kickAudio $ \(n, _) -> audioExpr n
            , drumsSplitSnare = flip fmap snareAudio $ \(n, _) -> audioExpr n
            , drumsSplitKit = audioExpr name
            }
          Nothing -> do
            xs <- NE.nonEmpty $ catMaybes [kickAudio, snareAudio, kitAudio]
            Just $ PartSingle $ case xs of
              (name, _) :| [] -> audioExpr name
              _               -> audiosExpr $ fmap fst xs
        , flip mapMaybe extraResults $ \(extraAudio, _) -> flip fmap extraAudio
          $ \(name, _) -> (FlexExtra name, PartSingle $ audioExpr name)
        ]
      , _crowd = Nothing
      , _planComments = []
      , _tuningCents = 0
      , _fileTempo = Nothing
      }
    }
  mid' = mid
    { RBFile.s_tracks = (RBFile.s_tracks mid)
      { RBFile.onyxSamples = Map.fromList $ catMaybes
        $ [songTrack, guitarTrack, bassTrack, kickTrack, snareTrack, kitTrack]
        <> map snd extraResults
      }
    }
  in (songYaml', mid')

importSet :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m [Import m]
importSet setDefPath = inside ("Loading DTX set.def from: " <> setDefPath) $ do
  songs <- stackIO $ loadSet setDefPath
  return $ map (importSetDef $ Just setDefPath) songs

importSetDef :: (SendMessage m, MonadIO m) => Maybe FilePath -> SetDef -> Import m
importSetDef setDefPath song level = do
  case (level, setDefPath) of
    (ImportFull, Just f) -> lg $ "Importing DTX from: " <> f
    _                    -> return ()
  -- TODO need to fix path separators (both backslash and yen)
  let relToSet = maybe id (\sdp -> (takeDirectory sdp </>)) setDefPath
      fs = map (relToSet . T.unpack)
        $ mapMaybe ($ song) [setL5File, setL4File, setL3File, setL2File, setL1File]
  diffs <- fmap catMaybes $ forM fs $ \f -> let
    fmt = case map toLower $ takeExtension f of
      ".gda" -> FormatGDA
      _      -> FormatDTX
    in stackIO (Dir.doesFileExist f) >>= \case
      True -> stackIO $ do
        dtx <- readDTXLines fmt <$> loadDTXLines f
        return $ Just (f, dtx)
      False -> do
        warn $ "Couldn't find difficulty from set.def: " <> f
        return Nothing
  (topDiffPath, topDiffDTX) <- case diffs of
    []    -> fatal "No difficulties found for song"
    d : _ -> return d

  addChipAudio <- case level of
    ImportQuick -> return id
    ImportFull  -> dtxAddChipAudio topDiffDTX topDiffPath

  let hasDrums  = not . RTB.null . dtx_Drums
      hasGuitar = not . RTB.null . dtx_Guitar
      hasBass   = not . RTB.null . dtx_Bass
  topDrumDiff <- case filter (hasDrums . snd) diffs of
    []       -> return Nothing
    (path, diff) : _ -> do
      unless (path == topDiffPath || level == ImportQuick) $ warn
        "Highest difficulty does not contain drums, but a lower one does. Audio might not be separated"
      return $ Just diff
  topGuitarDiff <- case filter (hasGuitar . snd) diffs of
    []       -> return Nothing
    (path, diff) : _ -> do
      unless (path == topDiffPath || level == ImportQuick) $ warn
        "Highest difficulty does not contain guitar, but a lower one does. Audio might not be separated"
      return $ Just diff
  topBassDiff <- case filter (hasBass . snd) diffs of
    [] -> return Nothing
    (path, diff) : _ -> do
      unless (path == topDiffPath || level == ImportQuick) $ warn
        "Highest difficulty does not contain bass, but a lower one does. Audio might not be separated"
      return $ Just diff
  let midiOnyx = case level of
        ImportQuick -> emptyChart
        ImportFull
          ->  maybe id dtxConvertDrums topDrumDiff
            $ maybe id dtxConvertGuitar topGuitarDiff
            $ maybe id dtxConvertBass topBassDiff
            $ dtxBaseMIDI topDiffDTX

  art <- case level of
    ImportQuick -> return Nothing
    ImportFull  -> case (takeDirectory topDiffPath </>) <$> dtx_PREIMAGE topDiffDTX of
      Nothing -> return Nothing
      Just f  -> stackIO (Dir.doesFileExist f) >>= \case
        False -> do
          warn $ "Couldn't find preview image: " <> f
          return Nothing
        True -> do
          let loc = "cover" <.> takeExtension f
          return $ Just $ SoftFile loc $ SoftReadable $ fileReadable f
  video <- case level of
    ImportQuick -> return Nothing
    ImportFull  -> case dtx_Video topDiffDTX of
      RNil -> return Nothing
      Wait posn chip RNil -> case HM.lookup chip $ dtx_AVI topDiffDTX of
        Just videoPath -> let
          fullVideoPath = takeDirectory topDiffPath </> videoPath
          in stackIO (Dir.doesFileExist fullVideoPath) >>= \case
            False -> do
              warn $ "Video file does not exist, skipping: " <> fullVideoPath
              return Nothing
            True -> return $ Just VideoInfo
              { _fileVideo      = SoftFile ("video" <.> takeExtension videoPath)
                $ SoftReadable $ fileReadable $ takeDirectory topDiffPath </> videoPath
              , _videoStartTime = Just $ negate $ realToFrac $ U.applyTempoMap (dtx_TempoMap topDiffDTX) posn
              , _videoEndTime   = Nothing
              , _videoLoop      = False
              }
        Nothing        -> do
          warn $ "Video chip not found: " <> T.unpack chip
          return Nothing
      _ -> do
        warn "Multiple video backgrounds, not importing"
        return Nothing

  let translateDifficulty Nothing    _   = Rank 1
      translateDifficulty (Just lvl) dec = let
        lvl' = (fromIntegral lvl + maybe 0 ((/ 10) . fromIntegral) dec) / 100 :: Rational
        in Rank $ max 1 $ round $ lvl' * 525 -- arbitrary scaling factor

  let yamlWithAudio = addChipAudio SongYaml
        { _metadata = def'
          { _title        = case setTitle song of
            ""    -> dtx_TITLE topDiffDTX
            title -> Just title
          , _artist       = dtx_ARTIST topDiffDTX
          , _comments     = toList $ dtx_COMMENT topDiffDTX
          , _genre        = dtx_GENRE topDiffDTX
          , _fileAlbumArt = art
          }
        , _global = def'
          { _backgroundVideo = video
          , _fileBackgroundImage = Nothing
          , _fileMidi = SoftFile "notes.mid" $ SoftChart midiOnyx
          , _fileSongAnim = Nothing
          }
        , _audio = HM.empty
        , _jammit = HM.empty
        , _plans = HM.empty
        , _targets = HM.empty
        , _parts = Parts $ HM.fromList $ catMaybes
          [ flip fmap topDrumDiff $ \diff -> (FlexDrums ,) def
            { partDrums = Just PartDrums
              { drumsDifficulty  = translateDifficulty (dtx_DLEVEL diff) (dtx_DLVDEC diff)
              , drumsMode        = DrumsFull
              , drumsKicks       = if any ((== LeftBass) . fst) $ dtx_Drums diff
                then KicksBoth
                else Kicks1x
              , drumsFixFreeform = False
              , drumsKit         = HardRockKit
              , drumsLayout      = StandardLayout
              , drumsFallback    = FallbackGreen
              , drumsFileDTXKit  = Nothing
              , drumsFullLayout  = FDStandard
              }
            }
          , flip fmap topGuitarDiff $ \diff -> (FlexGuitar ,) def
            { partGRYBO = Just def
              { gryboDifficulty = translateDifficulty (dtx_GLEVEL diff) (dtx_GLVDEC diff)
              }
            }
          , flip fmap topBassDiff $ \diff -> (FlexBass ,) def
            { partGRYBO = Just def
              { gryboDifficulty = translateDifficulty (dtx_BLEVEL diff) (dtx_BLVDEC diff)
              }
            }
          ]
        }
      (finalSongYaml, finalMidi) = case level of
        ImportQuick -> (yamlWithAudio, midiOnyx)
        ImportFull  -> dtxMakeAudioPlan topDiffDTX (yamlWithAudio, midiOnyx)
  return finalSongYaml
    { _global = (_global finalSongYaml)
      { _fileMidi = SoftFile "notes.mid" $ SoftChart finalMidi
      }
    }

importDTX :: (SendMessage m, MonadIO m) => FilePath -> Import m
importDTX fin level = do
  case level of
    ImportFull  -> lg $ "Importing DTX from: " <> fin
    ImportQuick -> return ()
  dtxLines <- stackIO $ loadDTXLines fin
  let setDef = SetDef
        { setTitle   = fromMaybe "" $ lookup "TITLE" dtxLines
        , setL1Label = Nothing
        , setL1File  = Just $ T.pack fin
        , setL2Label = Nothing
        , setL2File  = Nothing
        , setL3Label = Nothing
        , setL3File  = Nothing
        , setL4Label = Nothing
        , setL4File  = Nothing
        , setL5Label = Nothing
        , setL5File  = Nothing
        }
  importSetDef Nothing setDef level
