{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.DTXMania where

import           Control.Monad.Extra              (forM, guard, unless)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Char                        (toLower)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import           Data.Either                      (lefts, rights)
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
import qualified Onyx.MIDI.Track.Drums.True       as TD
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.MIDI.Track.FiveFret
import           Onyx.Project                     hiding (TargetDTX (..))
import           Onyx.StackTrace
import           Onyx.Util.Handle                 (fileReadable)
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.FilePath                  (takeDirectory, takeExtension,
                                                   (<.>), (</>))

dtxConvertDrums, dtxConvertGuitar, dtxConvertBass
  :: DTX -> F.Song (F.OnyxFile U.Beats) -> F.Song (F.OnyxFile U.Beats)
dtxConvertDrums dtx (F.Song tmap mmap onyx) = let
  importTrueDrums notes = mempty
    { TD.tdDifficulties = Map.singleton Expert $ let
      gems = flip RTB.mapMaybe notes $ \case
        HihatClose -> Just (TD.Hihat    , TD.GemHihatClosed, VelocityNormal)
        Snare      -> Just (TD.Snare    , TD.GemNormal     , VelocityNormal)
        BassDrum   -> Just (TD.Kick     , TD.GemNormal     , VelocityNormal)
        HighTom    -> Just (TD.Tom1     , TD.GemNormal     , VelocityNormal)
        LowTom     -> Just (TD.Tom2     , TD.GemNormal     , VelocityNormal)
        Cymbal     -> Just (TD.CrashR   , TD.GemNormal     , VelocityNormal)
        FloorTom   -> Just (TD.Tom3     , TD.GemNormal     , VelocityNormal)
        HihatOpen  -> Just (TD.Hihat    , TD.GemHihatOpen  , VelocityNormal)
        RideCymbal -> Just (TD.Ride     , TD.GemNormal     , VelocityNormal)
        LeftCymbal -> Just (TD.CrashL   , TD.GemNormal     , VelocityNormal)
        LeftPedal  -> Just (TD.HihatFoot, TD.GemNormal     , VelocityNormal)
        LeftBass   -> Nothing
      -- if no open hihat, assume hihats are unmarked
      hasOpenHihat = any (\(_, typ, _) -> typ == TD.GemHihatOpen) gems
      gems' = if hasOpenHihat
        then gems
        else fmap (\(gem, _, vel) -> (gem, TD.GemNormal, vel)) gems
      in (TD.makeTrueDifficultyDTX gems')
        { TD.tdKick2 = RTB.mapMaybe (\case LeftBass -> Just (); _ -> Nothing) notes
        }
    }
  in F.Song tmap mmap $ F.editOnyxPart F.FlexDrums
    (\opart -> opart { F.onyxPartTrueDrums = importTrueDrums $ fmap fst $ dtx_Drums dtx })
    onyx
dtxConvertGuitar = dtxConvertGB dtx_Guitar dtx_GuitarLong $ \onyx five -> F.editOnyxPart
  F.FlexGuitar
  (\opart -> opart { F.onyxPartGuitar = five })
  onyx
dtxConvertBass   = dtxConvertGB dtx_Bass   dtx_BassLong   $ \onyx five -> F.editOnyxPart
  F.FlexBass
  (\opart -> opart { F.onyxPartGuitar = five })
  onyx

dtxConvertGB
  :: (DTX -> RTB.T U.Beats ([Color], Chip))
  -> (DTX -> RTB.T U.Beats ())
  -> (f U.Beats -> FiveTrack U.Beats -> f U.Beats)
  -> DTX
  -> F.Song (f U.Beats)
  -> F.Song (f U.Beats)
dtxConvertGB getter getLong setter dtx (F.Song tmap mmap fixed) = let
  longLengths = let
    pairs = RTB.toPairList $ getLong dtx
    in RTB.fromPairList $ zipWith
      (\(dt, _) (len, _) -> (dt, len))
      pairs
      (drop 1 pairs)
  guitarToFive notes = mempty
    { fiveDifficulties = Map.singleton Expert $ emit5' $ let
      noSustains = RTB.flatten $ flip fmap notes $ \case
        [] -> [((Nothing, Strum), Nothing)]
        xs -> [((Just x , Strum), Nothing) | x <- xs ]
      merged = RTB.merge (Left <$> longLengths) (Right <$> noSustains)
      in RTB.flatten $ flip fmap (RTB.collectCoincident merged) $ \instant ->
        case lefts instant of
          sustain : _ -> [ (note, Just sustain) | (note, _) <- rights instant ]
          []          -> rights instant
    }
  in F.Song tmap mmap $ setter fixed $ guitarToFive $ fmap fst $ getter dtx

dtxBaseMIDI :: DTX -> F.Song (F.OnyxFile U.Beats)
dtxBaseMIDI dtx = F.Song (dtx_TempoMap dtx) (dtx_MeasureMap dtx) mempty

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
        { md5 = Nothing
        , frames = Nothing
        , commands = []
        , filePath = Just $ SoftFile ("samples" </> T.unpack chip <.> "wav") $ SoftAudio
          $ fixMono $ adjustVolume src
        , rate = Nothing
        , channels = 2
        })
  return $ \songYaml -> songYaml { audio = HM.fromList audio }

dtxMakeAudioPlan :: DTX
  -> (SongYaml SoftFile, F.Song (F.OnyxFile U.Beats))
  -> (SongYaml SoftFile, F.Song (F.OnyxFile U.Beats))
dtxMakeAudioPlan dtx (songYaml, mid) = let
  foundChips = HM.keysSet songYaml.audio
  audioForChipsOverlap name chips = if RTB.null chips
    then (Nothing, Nothing)
    else let
      poly = SamplesInfo
        { groupPolyphony = Nothing -- unlimited
        , groupCrossfade = 0.002 -- doesn't matter
        }
      audios = AudioSamples poly
      track = F.SamplesTrack
        $ fmap (\chip -> F.SampleTrigger "" chip)
        -- don't include sample if we didn't find its audio
        $ RTB.filter (\chip -> HS.member chip foundChips) chips
      in (Just (name, audios), Just (name, track))
  audioForChipGroups polyphony name chips = if all (RTB.null . snd) chips
    then (Nothing, Nothing)
    else let
      poly = SamplesInfo
        { groupPolyphony = Just polyphony
        , groupCrossfade = 0.002
        }
      audios = AudioSamples poly
      track = F.SamplesTrack $ foldr RTB.merge RTB.empty $ do
        (groupName, groupChips) <- chips
        return
          $ fmap (F.SampleTrigger groupName)
          -- don't include sample if we didn't find its audio
          $ RTB.filter (\chip -> HS.member chip foundChips) groupChips
      in (Just (name, audios), Just (name, track))
  getDrumChipGroup (groupName, lanes)
    = (groupName, RTB.mapMaybe (\(l, chip) -> guard (elem l lanes) >> Just chip) $ dtx_Drums dtx)
  (songAudio, songTrack) = audioForChipsOverlap "audio-song" $ dtx_BGM dtx
  (guitarAudio, guitarTrack) = audioForChipGroups 1 "audio-guitar" [("", snd <$> dtx_Guitar dtx)]
  (bassAudio, bassTrack) = audioForChipGroups 1 "audio-bass" [("", snd <$> dtx_Bass dtx)]
  -- giving each drum lane polyphony 2 based on info at
  -- https://osdn.net/projects/dtxmania/wiki/DTX%20data%20format
  (kickAudio, kickTrack) = audioForChipGroups 2 "audio-kick" $ map getDrumChipGroup
    [ ("BD", [BassDrum])
    , ("LB", [LeftBass])
    ]
  (snareAudio, snareTrack) = audioForChipGroups 2 "audio-snare" $ map getDrumChipGroup
    [("", [Snare])]
  (cymbalAudio, cymbalTrack) = audioForChipGroups 2 "audio-cymbals" $ map getDrumChipGroup
    [ ("HH", [HihatClose, HihatOpen])
    , ("LP", [LeftPedal])
    , ("CY", [Cymbal])
    , ("RC", [RideCymbal])
    , ("LC", [LeftCymbal])
    ]
  (tomAudio, tomTrack) = audioForChipGroups 2 "audio-toms" $ map getDrumChipGroup
    [ ("HT", [HighTom])
    , ("LT", [LowTom])
    , ("FT", [FloorTom])
    ]
  -- this may not behave exactly as DTX does, but close enough.
  -- LP notes should cut off any previous HH notes, and HHC should cut off HHO.
  -- but this implementation does mean HHC only has polyphony 1 - should revisit in future
  hihatSilencer = Just ("hihat-silencer", AudioSnippet $ Silence 2 $ CA.Frames 0)
  hihatSilencerTrigger = F.SampleTrigger
    { sampleGroup = "HH"
    , sampleAudio = "hihat-silencer"
    }
  cymbalTrack' = flip fmap cymbalTrack $ fmap $ \trk -> trk
    { F.sampleTriggers = RTB.merge (F.sampleTriggers trk) $
      RTB.flatten $ flip fmap (RTB.collectCoincident $ dtx_Drums dtx) $ \instant ->
        if any (\(lane, _) -> elem lane [LeftPedal, HihatClose]) instant
          then case length $ filter (\(lane, _) -> elem lane [HihatClose, HihatOpen]) instant of
            0 -> [hihatSilencerTrigger, hihatSilencerTrigger]
            1 -> [hihatSilencerTrigger]
            _ -> []
          else []
    }
  extraResults = flip map (HM.toList $ dtx_BGMExtra dtx) $ \(chan, chips) ->
    audioForChipGroups 1 ("audio-extra-" <> chan) [("", chips)]
  audioExpr name = Input $ Named name
  audiosExpr names = Mix $ fmap (Input . Named) names
  songYaml' = songYaml
    { audio = HM.union songYaml.audio $ HM.fromList $ catMaybes
      $ [songAudio, guitarAudio, bassAudio, kickAudio, snareAudio, cymbalAudio, tomAudio, hihatSilencer]
      <> map fst extraResults
    , plans = HM.singleton "dtx" $ StandardPlan StandardPlanInfo
      { song        = flip fmap songAudio $ \(name, _) -> audioExpr name
      , parts       = Parts $ HM.fromList $ concat
        [ toList $ flip fmap guitarAudio $ \(name, _) -> (F.FlexGuitar, PartSingle $ audioExpr name)
        , toList $ flip fmap bassAudio $ \(name, _) -> (F.FlexBass, PartSingle $ audioExpr name)
        , toList $ (F.FlexDrums ,) <$> case cymbalAudio of
          Just (name, _) -> Just PartDrumKit
            { kick  = flip fmap kickAudio $ \(n, _) -> audioExpr n
            , snare = flip fmap snareAudio $ \(n, _) -> audioExpr n
            , toms  = flip fmap tomAudio $ \(n, _) -> audioExpr n
            , kit   = audioExpr name
            }
          Nothing -> do
            xs <- NE.nonEmpty $ catMaybes [kickAudio, snareAudio, cymbalAudio, tomAudio]
            Just $ PartSingle $ case xs of
              (name, _) :| [] -> audioExpr name
              _               -> audiosExpr $ fmap fst xs
        , flip mapMaybe extraResults $ \(extraAudio, _) -> flip fmap extraAudio
          $ \(name, _) -> (F.FlexExtra name, PartSingle $ audioExpr name)
        ]
      , crowd       = Nothing
      , comments    = []
      , tuningCents = 0
      , fileTempo   = Nothing
      }
    }
  mid' = mid
    { F.s_tracks = (F.s_tracks mid)
      { F.onyxSamples = Map.fromList $ catMaybes
        $ [songTrack, guitarTrack, bassTrack, kickTrack, snareTrack, cymbalTrack', tomTrack]
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
              { fileVideo      = SoftFile ("video" <.> takeExtension videoPath)
                $ SoftReadable $ fileReadable $ takeDirectory topDiffPath </> videoPath
              , videoStartTime = Just $ negate $ realToFrac $ U.applyTempoMap (dtx_TempoMap topDiffDTX) posn
              , videoEndTime   = Nothing
              , videoLoop      = False
              }
        Nothing        -> do
          warn $ "Video chip not found: " <> T.unpack chip
          return Nothing
      _ -> do
        warn "Multiple video backgrounds, not importing"
        return Nothing

  let translateDifficulty Nothing _ = Rank 1
      -- some songs just have a 3-digit *LEVEL
      translateDifficulty (Just lvl) Nothing | lvl >= 100 = let
        lvl' = fromIntegral lvl / 1000 :: Rational
        in Rank $ max 1 $ round $ lvl' * difficultyScale
      -- but most use 2-digit *LEVEL + optional 1-digit *LVDEC
      translateDifficulty (Just lvl) dec = let
        lvl' = (fromIntegral lvl + maybe 0 ((/ 10) . fromIntegral) dec) / 100 :: Rational
        in Rank $ max 1 $ round $ lvl' * difficultyScale
      difficultyScale = 525 -- arbitrary scaling factor
      -- TODO also import to difficulty-dtx

  let yamlWithAudio = addChipAudio SongYaml
        { metadata = def'
          { title         = case setTitle song of
            ""    -> dtx_TITLE topDiffDTX
            title -> Just title
          , artist        = dtx_ARTIST topDiffDTX
          , loadingPhrase = dtx_COMMENT topDiffDTX
          , genre         = dtx_GENRE topDiffDTX
          , fileAlbumArt  = art
          }
        , global = def'
          { backgroundVideo = video
          , fileBackgroundImage = Nothing
          , fileMidi = SoftFile "notes.mid" $ SoftChart midiOnyx
          , fileSongAnim = Nothing
          }
        , audio = HM.empty
        , jammit = HM.empty
        , plans = HM.empty
        , targets = HM.empty
        , parts = Parts $ HM.fromList $ catMaybes
          [ flip fmap topDrumDiff $ \diff -> (F.FlexDrums, emptyPart
            { drums = Just $ let
              kicks = if any ((== LeftBass) . fst) $ dtx_Drums diff
                then KicksBoth
                else Kicks1x
              in (emptyPartDrums DrumsTrue kicks)
                { difficulty = translateDifficulty (dtx_DLEVEL diff) (dtx_DLVDEC diff)
                }
            })
          , flip fmap topGuitarDiff $ \diff -> (F.FlexGuitar, emptyPart
            { grybo = Just (def :: PartGRYBO)
              { difficulty = translateDifficulty (dtx_GLEVEL diff) (dtx_GLVDEC diff)
              }
            })
          , flip fmap topBassDiff $ \diff -> (F.FlexBass, emptyPart
            { grybo = Just (def :: PartGRYBO)
              { difficulty = translateDifficulty (dtx_BLEVEL diff) (dtx_BLVDEC diff)
              }
            })
          ]
        }
      (finalSongYaml, finalMidi) = case level of
        ImportQuick -> (yamlWithAudio, midiOnyx)
        ImportFull  -> dtxMakeAudioPlan topDiffDTX (yamlWithAudio, midiOnyx)
  return finalSongYaml
    { global = finalSongYaml.global
      { fileMidi = SoftFile "notes.mid" $ SoftChart finalMidi
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
