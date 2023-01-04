{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.FretsOnFire where

import           Codec.Picture                    (PixelRGBA8 (..),
                                                   convertRGBA8, readImage)
import           Codec.Picture.Types              (dropAlphaLayer)
import           Control.Arrow                    (first)
import           Control.Monad                    (forM, forM_, guard, unless,
                                                   void, when)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Char                        (isSpace, toLower)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (intercalate)
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   listToMaybe)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Numeric                          (readHex)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.Audio
import           Onyx.Build.Common                (backgroundColor, squareImage)
import qualified Onyx.FeedBack.Base               as FB
import qualified Onyx.FeedBack.Load               as FB
import qualified Onyx.FretsOnFire                 as FoF
import           Onyx.Guitar                      (applyStatus)
import           Onyx.Import.Base
import           Onyx.MIDI.Common
import           Onyx.MIDI.Track.Drums            as RBDrums
import           Onyx.MIDI.Track.File             (FlexPartName (..))
import qualified Onyx.MIDI.Track.File             as RBFile
import qualified Onyx.MIDI.Track.FiveFret         as RBFive
import           Onyx.MIDI.Track.ProGuitar        (nullPG)
import           Onyx.MIDI.Track.ProKeys          (nullPK)
import           Onyx.MIDI.Track.SixFret          (nullSix)
import           Onyx.MIDI.Track.Vocal
import qualified Onyx.MIDI.Track.Vocal.Legacy     as RBVox
import           Onyx.PhaseShift.Dance            (nullDance)
import           Onyx.Project                     hiding (Difficulty)
import           Onyx.StackTrace
import           Onyx.Util.Files                  (fixFileCase)
import           Onyx.Util.Handle                 (fileReadable)
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.FilePath

generateSwells
  :: (Eq a)
  => RTB.T U.Seconds (Bool, [a])
  -> (RTB.T U.Seconds Bool, RTB.T U.Seconds Bool)
generateSwells notes = let
  maxDistance = 0.155 :: U.Seconds -- actually 0.16 but being conservative
  go RNil = RNil
  go (Wait t1 (True, [mainGem]) secondOnward@(Wait t2 (True, secondSet) rest)) | t2 < maxDistance = let
    makeLane altGem = let
      thisType = if isJust altGem then LaneDouble else LaneSingle
      (lane, after) = case altGem of
        Nothing -> splitSingle mainGem rest
        Just g  -> splitDouble mainGem g rest
      laneBaseLength = t2 <> mconcat (RTB.getTimes lane)
      in case after of
        RNil
          -- lane goes to end; just make it go a bit past last note
          -> Wait t1 (thisType, True)
          $  Wait (laneBaseLength <> maxDistance) (thisType, False)
          $  RNil
        Wait t3 z rest'
          -- end lane at next note; we trim it back with fixFreeform later
          -> Wait t1 (thisType, True)
          $  Wait (laneBaseLength <> t3) (thisType, False)
          $  go $ Wait 0 z rest'
    in if elem mainGem secondSet -- the `elem` allows gravity blast lanes
      then makeLane Nothing
      else case secondSet of
        [g] -> makeLane $ Just g
        _   -> RTB.delay t1 $ go secondOnward
  go (Wait t _ rest) = RTB.delay t $ go rest
  splitSingle gem (Wait t (True, xs) rest)
    | elem gem xs && t < maxDistance -- the `elem` allows gravity blast lanes
    = case splitSingle gem rest of
      (lane, after) -> (Wait t xs lane, after)
  splitSingle _ rest = (RNil, rest)
  splitDouble gem1 gem2 (Wait t (True, [x]) rest)
    | x == gem1 && t < maxDistance
    = case splitDouble gem2 gem1 rest of
      (lane, after) -> (Wait t [x] lane, after)
  splitDouble _ _ rest = (RNil, rest)
  result = go notes
  laneType typ = fmap snd $ RTB.filter ((== typ) . fst) result
  in (laneType LaneSingle, laneType LaneDouble)

-- | Uses the PS swell lanes as a guide to make new valid ones.
redoSwells
  :: RBFile.Song (RBFile.FixedFile U.Beats)
  -> RBFile.Song (RBFile.FixedFile U.Beats)
redoSwells (RBFile.Song tmap mmap ps) = let
  fixTrack trk = let
    notes = RTB.collectCoincident
      $ RTB.filter (\(gem, _vel) -> gem /= RBDrums.Kick) $ drumGems $ fromMaybe mempty
      $ Map.lookup Expert $ drumDifficulties trk
    notesWithLanes = fmap (first $ not . null)
      $ flip applyStatus notes $ RTB.merge
        (fmap (('s',) . isJust) $ drumSingleRoll trk)
        (fmap (('d',) . isJust) $ drumDoubleRoll trk)
    (lanes1, lanes2) = generateSwells
      $ U.applyTempoTrack tmap notesWithLanes
    in trk
      { drumSingleRoll = fmap (\b -> guard b >> Just LaneExpert)
        $ RBFile.fixFreeform (void notes)
        $ U.unapplyTempoTrack tmap lanes1
      , drumDoubleRoll = fmap (\b -> guard b >> Just LaneExpert)
        $ RBFile.fixFreeform (void notes)
        $ U.unapplyTempoTrack tmap lanes2
      }
  in RBFile.Song tmap mmap ps
    { RBFile.fixedPartDrums       = fixTrack $ RBFile.fixedPartDrums       ps
    , RBFile.fixedPartDrums2x     = fixTrack $ RBFile.fixedPartDrums2x     ps
    , RBFile.fixedPartRealDrumsPS = fixTrack $ RBFile.fixedPartRealDrumsPS ps
    }

data LaneNotes = LaneSingle | LaneDouble
  deriving (Eq, Ord)

importFoF :: (SendMessage m, MonadIO m) => FilePath -> Import m
importFoF src level = do
  when (level == ImportFull) $ lg $ "Importing FoF/PS/CH song from folder: " <> src
  allFiles <- stackIO $ Dir.listDirectory src
  pathMid <- fixFileCase $ src </> "notes.mid"
  pathChart <- fixFileCase $ src </> "notes.chart"
  pathIni <- fixFileCase $ src </> "song.ini"
  let hasCymbalMarkers chart = flip any (FB.chartTracks chart) $ any $ \case
        FB.Note 66 _ -> True
        FB.Note 67 _ -> True
        FB.Note 68 _ -> True
        _            -> False
  (song, parsed, isChart, chartWithCymbals) <- stackIO (Dir.doesFileExist pathIni) >>= \case
    True -> do
      ini <- FoF.loadSong pathIni
      case level of
        ImportQuick -> return (ini, emptyChart, False, False)
        ImportFull  -> stackIO (Dir.doesFileExist pathMid) >>= \case
          True -> do
            lg "Found song.ini and notes.mid"
            mid <- loadFoFMIDI ini pathMid
            return (ini, mid, False, False)
          False -> stackIO (Dir.doesFileExist pathChart) >>= \case
            True -> do
              lg "Found song.ini and notes.chart"
              chart <- FB.chartToBeats <$> FB.loadChartFile pathChart
              mid <- FB.chartToMIDI chart
              -- if .ini delay is 0 or absent, CH uses .chart Offset
              ini' <- if fromMaybe 0 (FoF.delay ini) == 0
                then case FoF.delay $ FB.chartToIni chart of
                  Just 0     -> return ini
                  Nothing    -> return ini
                  chartDelay -> do
                    lg "Using .chart 'Offset' because .ini 'delay' is 0 or absent"
                    return ini{ FoF.delay = chartDelay }
                else return ini
              return (ini', mid, True, hasCymbalMarkers chart)
            False -> fatal "Found song.ini, but no notes.mid or notes.chart"
    False -> stackIO (Dir.doesFileExist pathChart) >>= \case
      True -> do
        lg "Found notes.chart but no song.ini. Metadata will come from .chart"
        chart <- FB.chartToBeats <$> FB.loadChartFile pathChart
        mid <- case level of
          ImportQuick -> return emptyChart
          ImportFull  -> FB.chartToMIDI chart
        return (FB.chartToIni chart, mid, True, hasCymbalMarkers chart)
      False -> fatal "No song.ini or notes.chart found"

  albumArt <- do
    let isImage f = case splitExtension $ map toLower f of
          (x, y) -> elem x ["album", "image"] && elem y [".png", ".jpg", ".jpeg"]
    case filter isImage allFiles of
      f : _ -> return $ Just $ SoftFile (map toLower f) $ SoftReadable $ fileReadable $ src </> f
      []    -> case filter ((== "label.png") . map toLower) allFiles of
        []    -> return Nothing
        f : _ -> inside "Converting label.png + cassettecolor to square art" $ do
          -- overlay label.png onto cassettecolor
          let hex s = case readHex s of
                [(n, "")] -> return n
                _         -> do
                  warn $ "Couldn't read hex number: " <> s
                  return 0
          bgColor <- case T.unpack <$> FoF.cassetteColor song of
            -- these are the only 2 formats FoF supports (Theme.py, function hexToColor)
            Just ['#', r, g, b] -> PixelRGBA8
              <$> hex [r] <*> hex [g] <*> hex [b] <*> return 255
            Just ['#', r1, r2, g1, g2, b1, b2] -> PixelRGBA8
              <$> hex [r1, r2] <*> hex [g1, g2] <*> hex [b1, b2] <*> return 255
            Nothing -> return $ PixelRGBA8 0 0 0 255
            Just s -> do
              warn $ "Unrecognized cassettecolor format: " <> s
              return $ PixelRGBA8 0 0 0 255
          img <- stackIO (readImage $ src </> f) >>= either fatal (return . convertRGBA8)
          let img' = backgroundColor bgColor $ squareImage 30 img
          return $ Just $ SoftFile "cover.png" $ SoftImage $ dropAlphaLayer img'
  backgroundImage <- case FoF.background song of
    _ | level == ImportQuick -> return Nothing
    Just v | not $ all isSpace v -> do -- PS background reference
      v' <- stackIO $ fixFileCase (src </> v) >>= Dir.makeAbsolute
      stackIO (Dir.doesFileExist v') >>= \case
        False -> do
          warn $ "song.ini references background " <> show v <> " but it wasn't found"
          return Nothing
        True -> return $ Just
          $ SoftFile ("background" <.> takeExtension v)
          $ SoftReadable $ fileReadable v'
    _ -> return $ let -- look for CH background
      isBackground f = case splitExtension $ map toLower f of
        ("background", ext) -> elem ext [".png", ".jpg", ".jpeg"]
        _                   -> False
      in case filter isBackground allFiles of
        []    -> Nothing
        f : _ -> Just $ SoftFile (map toLower f) $ SoftReadable $ fileReadable $ src </> f
  let loadAudioFile _ | level == ImportQuick = return Nothing
      loadAudioFile x = stackIO $ let
        tryExt ext = do
          let template = x <.> ext
          path <- fixFileCase $ src </> template
          Dir.doesFileExist path >>= \case
            True  -> return $ Just (template, path)
            False -> return Nothing
        in tryExt "opus" >>= \case
          Nothing -> tryExt "ogg" >>= \case
            Nothing -> tryExt "mp3" >>= \case
              Nothing        -> tryExt "wav"
              found@(Just _) -> return found
            found@(Just _) -> return found
          found@(Just _) -> return found
  audio_drums <- loadAudioFile "drums"
  audio_drums_1 <- loadAudioFile "drums_1"
  audio_drums_2 <- loadAudioFile "drums_2"
  audio_drums_3 <- loadAudioFile "drums_3"
  audio_drums_4 <- loadAudioFile "drums_4"
  audio_guitar <- loadAudioFile "guitar"
  audio_keys <- loadAudioFile "keys"
  audio_bass <- loadAudioFile "bass" -- this is only supported by CH and newer PS
  audio_rhythm <- loadAudioFile "rhythm" -- this is the "bass or rhythm" traditional audio
  audio_vocals <- loadAudioFile "vocals"
  audio_vocals_1 <- loadAudioFile "vocals_1"
  audio_vocals_2 <- loadAudioFile "vocals_2"
  audio_crowd <- loadAudioFile "crowd"
  audio_song <- loadAudioFile "song"
  let audioFiles = catMaybes
        [ audio_drums, audio_drums_1, audio_drums_2, audio_drums_3
        , audio_drums_4, audio_guitar, audio_keys, audio_rhythm, audio_bass
        , audio_vocals, audio_vocals_1, audio_vocals_2, audio_crowd, audio_song
        ]

  -- assume sole guitar is no-stems audio
  let onlyGuitar = case audioFiles of
        [(x, _)] | dropExtension x == "guitar" -> True
        _                                      -> False
  audioFilesWithChannels <- forM audioFiles $ \(template, srcPath) -> audioChannels srcPath >>= \case
    Nothing    -> fatal $ "Couldn't get channel count of audio file: " <> srcPath
    Just chans -> return ((template, srcPath), chans)

  let softAudio (template, path) = SoftFile template $ SoftReadable $ fileReadable path
      gtrAudio = if onlyGuitar then [] else toList audio_guitar
      bassAudio = toList audio_bass
      rhythmAudio = toList audio_rhythm
      keysAudio = toList audio_keys
      crowdAudio = toList audio_crowd
      voxAudio = catMaybes [audio_vocals, audio_vocals_1, audio_vocals_2]
      md0 = audio_drums
      md1 = audio_drums_1
      md2 = audio_drums_2
      md3 = audio_drums_3
      md4 = audio_drums_4
      (drumsAudio, kickAudio, snareAudio) = case (md1, md2, md3, md4) of
        (Just d1, Just d2, Just d3, Just d4) -> ([d3, d4], [d1], [d2]) -- GH config with separate toms vs cymbals
        _ -> case (md1, md2, md3) of
          (Just d1, Just d2, Just d3) -> ([d3], [d1], [d2]) -- RB drum mix 1, 2, or 3
          _ -> case (md1, md2) of
            (Just d1, Just d2) -> ([d2], [d1], []) -- RB drum mix 4
            _ -> case md0 of
              Just d0 -> ([d0], [], []) -- RB drum mix 0
              _       -> (catMaybes [md1, md2, md3, md4], [], []) -- either no drums, or weird configuration
      songAudio = toList $ if onlyGuitar then audio_guitar else audio_song

  let (delayAudio, delayMIDI) = case FoF.delay song of
        Nothing -> (id, id)
        Just n -> case compare n 0 of
          EQ -> (id, id)
          GT -> let
            secs = fromIntegral n / 1000
            midiDelay = ceiling secs
            audioDelay = fromIntegral midiDelay - secs
            in (Pad Start $ CA.Seconds audioDelay, RBFile.padFixedFile midiDelay)
          LT ->
            ( Pad Start $ CA.Seconds $ fromIntegral (abs n) / 1000
            , id
            )
      audioExpr auds = do
        auds' <- NE.nonEmpty $ map fst auds
        Just $ case auds' of
          aud :| [] -> PlanAudio
            { expr = delayAudio $ Input $ Named $ T.pack aud
            , pans = []
            , vols = []
            }
          _ -> PlanAudio
            { expr = delayAudio $ Mix $ fmap (Input . Named . T.pack) auds'
            , pans = []
            , vols = []
            }

  let toTier = maybe (Tier 1) $ \n -> Tier $ max 1 $ min 7 $ fromIntegral n + 1

  let maybePath2x = do
        guard $ level == ImportFull
        listToMaybe $ flip filter allFiles $ \f -> let
          lower = T.toLower $ T.pack f
          in map toLower (takeExtension f) == ".mid"
            && any (`T.isInfixOf` lower) ["expert+", "expertplus", "notes+"]
      -- expert+.mid is most common but Drum Projects 2 and 3 also have:
      -- expertplus.mid, notesexpert+.mid, (name of song)Expert+.mid
      -- some also have expert+.chart but we shouldn't have to support that
  forM_ maybePath2x $ \path2x -> do
    lg $ "Loading separate X+ file: " <> path2x
  -- TODO should we prefer the PS (95) format if there's also a separate midi?
  add2x <- case maybePath2x of
    Just path2x -> do
      parsed2x <- RBFile.loadMIDI $ src </> path2x
      let trk2x = RBFile.fixedPartDrums $ RBFile.s_tracks parsed2x
      return $ if nullDrums trk2x
        then id
        else \mid -> mid { RBFile.fixedPartDrums2x = trk2x }
    Nothing -> return id
  let (title, is2x) = case FoF.name song of
        Nothing   -> (Nothing, False)
        Just name -> first Just $ determine2xBass name
      hasKicks = if isJust maybePath2x
        || not (RTB.null $ drumKick2x $ RBFile.fixedPartDrums       $ RBFile.s_tracks parsed)
        || not (RTB.null $ drumKick2x $ RBFile.fixedPartRealDrumsPS $ RBFile.s_tracks parsed)
        then KicksBoth
        else if is2x then Kicks2x else Kicks1x

  let fixGHVox trks = trks
        { RBFile.fixedPartVocals = stripTags $ RBVox.vocalFromLegacy $ RBVox.fixGHVocals $ RBVox.vocalToLegacy $ RBFile.fixedPartVocals trks
        }
      swapFiveLane trks = if fromMaybe False $ FoF.fiveLaneDrums song
        then trks
          { RBFile.fixedPartDrums   = swapFiveLaneTrack $ RBFile.fixedPartDrums   trks
          , RBFile.fixedPartDrums2x = swapFiveLaneTrack $ RBFile.fixedPartDrums2x trks
          }
        else trks
      swapFiveLaneTrack trk = trk { drumDifficulties = fmap swapFiveLaneDiff $ drumDifficulties trk }
      swapFiveLaneDiff dd = dd
        { drumGems = flip fmap (drumGems dd) $ \case
          (RBDrums.Orange              , vel) -> (RBDrums.Pro RBDrums.Green (), vel)
          (RBDrums.Pro RBDrums.Green (), vel) -> (RBDrums.Orange              , vel)
          x                                   -> x
        }

  outputMIDI <- fixShortVoxPhrases $ checkEnableDynamics $ redoSwells parsed
    { RBFile.s_tracks = fixGHVox $ swapFiveLane $ removeDummyTracks $ add2x $ RBFile.s_tracks parsed
    }
  let outputFixed = RBFile.s_tracks outputMIDI
      midi = SoftFile "notes.mid" $ SoftChart $ case delayMIDI outputMIDI of
        RBFile.Song tempos sigs fixed -> RBFile.Song tempos sigs $ RBFile.fixedToOnyx fixed

  vidPath <- case FoF.video song of
    _ | level == ImportQuick -> return Nothing
    Just v | not $ all isSpace v -> do -- PS video
      v' <- stackIO $ fixFileCase (src </> v) >>= Dir.makeAbsolute
      exists <- stackIO $ Dir.doesFileExist v'
      unless exists $ warn $ "song.ini references video " <> show v <> " but it wasn't found"
      return $ guard exists >> Just v'
    _ -> case filter ((== "video") . map toLower . dropExtension) $ allFiles of -- CH video
      v : _ -> do
        v' <- stackIO $ Dir.makeAbsolute $ src </> v
        return $ Just v'
      []    -> return Nothing
  let videoInfo = flip fmap vidPath $ \f -> VideoInfo
        { fileVideo = SoftFile ("video" <.> takeExtension f) $ SoftReadable $ fileReadable f
        , videoStartTime = FoF.videoStartTime song
        , videoEndTime = FoF.videoEndTime song
        , videoLoop = fromMaybe False $ FoF.videoLoop song
        }
  -- TODO need to check how video start/end time interacts with audio delay,
  -- so we can adjust based on how we imported the delay

  -- In Phase Shift, if you put -1 as the difficulty for a part,
  -- it explicitly disables it, even if there is a MIDI track for it.
  -- Clone Hero doesn't do this at all and will still show the part.
  -- As a compromise, we use the PS behavior for .mid, and CH for .chart.
  let guardDifficulty getDiff = if isChart || True -- TODO temporarily doing this for midis also
        then True
        else getDiff song /= Just (-1)
      isnt :: (Eq a, Monoid a) => (a -> Bool) -> (RBFile.FixedFile U.Beats -> a) -> Bool
      isnt isEmpty f = not $ isEmpty $ f outputFixed
      vocalMode = if isnt nullVox RBFile.fixedPartVocals && guardDifficulty FoF.diffVocals && fmap T.toLower (FoF.charter song) /= Just "sodamlazy"
        then if isnt nullVox RBFile.fixedHarm2 && guardDifficulty FoF.diffVocalsHarm
          then if isnt nullVox RBFile.fixedHarm3
            then Just Vocal3
            else Just Vocal2
          else Just Vocal1
        else Nothing
      hasBass = isnt RBFive.nullFive RBFile.fixedPartBass && guardDifficulty FoF.diffBass
      hasRhythm = isnt RBFive.nullFive RBFile.fixedPartRhythm && guardDifficulty FoF.diffRhythm

  let hopoThreshold = case FoF.hopoFrequency song of
        Just ht -> ht
        -- TODO does PS interpret this as out of 480? or the midi's actual resolution?
        -- for C3 converts it should always be 480 though.
        Nothing -> case FoF.eighthNoteHOPO song of
          Just True -> 250 -- don't know exactly
          _         -> 170

  return SongYaml
    { metadata = def'
      { title        = title
      , artist       = FoF.artist song
      , album        = FoF.album song
      , genre        = FoF.genre song
      , year         = FoF.year song
      , fileAlbumArt = albumArt
      , trackNumber  = FoF.track song
      , comments     = []
      , author       = FoF.charter song
      -- TODO this probably needs to be adjusted for delay padding
      , previewStart = case FoF.previewStartTime song of
        Just ms | ms >= 0 -> Just $ PreviewSeconds $ fromIntegral ms / 1000
        _                 -> Nothing
      , previewEnd   = Nothing
      , difficulty   = toTier $ FoF.diffBand song
      , cover        = maybe False ("cover" `T.isInfixOf`) $ FoF.tags song
      }
    , global = def'
      { backgroundVideo = videoInfo
      , fileBackgroundImage = backgroundImage
      , fileMidi = midi
      , fileSongAnim = Nothing
      }
    , audio = HM.fromList $ flip map audioFilesWithChannels $ \(pair@(template, _srcPath), chans) ->
      (T.pack template, AudioFile AudioInfo
        { md5 = Nothing
        , frames = Nothing
        , commands = []
        , filePath = Just $ softAudio pair
        , rate = Nothing
        , channels = chans
        }
      )
    , jammit = HM.empty
    , plans = HM.singleton "fof" $ StandardPlan StandardPlanInfo
      { song         = audioExpr songAudio
      , countin      = Countin []
      , parts        = Parts $ HM.fromList $ concat
        [ case audioExpr gtrAudio of Nothing -> []; Just x -> [(FlexGuitar, PartSingle x)]
        , case (audioExpr bassAudio, audioExpr rhythmAudio) of
          -- the complicated assignment of rhythm.ogg/bass.ogg to rhythm/bass parts
          (Nothing, Nothing) -> []
          (Just b , Just r ) -> [(FlexBass, PartSingle b), (FlexExtra "rhythm", PartSingle r)]
          (Just b , Nothing) -> [(FlexBass, PartSingle b)]
          (Nothing, Just r ) -> case (hasBass, hasRhythm) of
            (True , True) -> [(FlexExtra "rhythm-bass", PartSingle r)] -- make up a part so neither bass nor rhythm gets the audio
            (False, True) -> [(FlexExtra "rhythm"     , PartSingle r)]
            _             -> [(FlexBass               , PartSingle r)]
        , case audioExpr keysAudio of Nothing -> []; Just x -> [(FlexKeys, PartSingle x)]
        , case audioExpr voxAudio of Nothing -> []; Just x -> [(FlexVocal, PartSingle x)]
        , case (audioExpr drumsAudio, audioExpr kickAudio, audioExpr snareAudio) of
          (Nothing, Nothing, Nothing) -> []
          (Just kit, Nothing, Nothing) -> [(FlexDrums, PartSingle kit)]
          (Just kit, kick, snare) -> [(FlexDrums, PartDrumKit
            { kit   = kit
            , kick  = kick
            , snare = snare
            })]
          _ -> error "FoF import: unsupported drums audio configuration (kick/snare but no kit)"
        ]
      , crowd = audioExpr crowdAudio
      , comments = []
      , tuningCents = 0
      , fileTempo = Nothing
      }
    , targets = HM.singleton "ps" $ PS def
      { ps_LoadingPhrase = FoF.loadingPhrase song
      }
    , parts = Parts $ HM.fromList
      [ ( FlexDrums, (emptyPart :: Part SoftFile)
        { drums = guard ((isnt nullDrums RBFile.fixedPartDrums || isnt nullDrums RBFile.fixedPartRealDrumsPS) && guardDifficulty FoF.diffDrums) >> Just PartDrums
          { difficulty = toTier $ FoF.diffDrums song
          , mode = let
            isFiveLane = FoF.fiveLaneDrums song == Just True || any
              (\(_, dd) -> any (\(gem, _vel) -> gem == RBDrums.Orange) $ drumGems dd)
              (Map.toList $ drumDifficulties $ RBFile.fixedPartDrums outputFixed)
            isReal = isnt nullDrums RBFile.fixedPartRealDrumsPS
            isPro = case FoF.proDrums song of
              Just b  -> b
              Nothing -> not (RTB.null $ drumToms $ RBFile.fixedPartDrums outputFixed)
                || chartWithCymbals -- handle the case where a .chart has cymbal markers, and no toms
            in if isFiveLane then Drums5
              else if isReal then DrumsReal
                else if isPro then DrumsPro
                  else Drums4
          , kicks = hasKicks
          , fixFreeform = False
          , kit = HardRockKit
          , layout = StandardLayout
          , fallback = if fromMaybe False $ FoF.drumFallbackBlue song
            then FallbackBlue
            else FallbackGreen
          , fileDTXKit = Nothing
          , fullLayout = FDStandard
          }
        })
      , ( FlexGuitar, (emptyPart :: Part SoftFile)
        { grybo = guard (isnt RBFive.nullFive RBFile.fixedPartGuitar && guardDifficulty FoF.diffGuitar) >> Just PartGRYBO
          { difficulty = toTier $ FoF.diffGuitar song
          , hopoThreshold = hopoThreshold
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proGuitar = let
          b =  (isnt nullPG RBFile.fixedPartRealGuitar   && guardDifficulty FoF.diffGuitarReal  )
            || (isnt nullPG RBFile.fixedPartRealGuitar22 && guardDifficulty FoF.diffGuitarReal22)
          in guard b >> Just PartProGuitar
            { difficulty    = toTier $ FoF.diffGuitarReal song
            , hopoThreshold = hopoThreshold
            , tuning        = def -- TODO actually import this
            , tuningRSBass  = Nothing
            , fixFreeform   = False
            , tones         = Nothing
            , pickedBass    = False
            }
        , ghl = guard (isnt nullSix RBFile.fixedPartGuitarGHL && guardDifficulty FoF.diffGuitarGHL) >> Just PartGHL
          { difficulty = toTier $ FoF.diffGuitarGHL song
          , hopoThreshold = hopoThreshold
          }
        })
      , ( FlexBass, (emptyPart :: Part SoftFile)
        { grybo = guard hasBass >> Just PartGRYBO
          { difficulty = toTier $ FoF.diffBass song
          , hopoThreshold = hopoThreshold
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proGuitar = let
          b =  (isnt nullPG RBFile.fixedPartRealBass   && guardDifficulty FoF.diffBassReal  )
            || (isnt nullPG RBFile.fixedPartRealBass22 && guardDifficulty FoF.diffBassReal22)
          in guard b >> Just PartProGuitar
            { difficulty    = toTier $ FoF.diffBassReal song
            , hopoThreshold = hopoThreshold
            , tuning        = def -- TODO actually import this
            , tuningRSBass  = Nothing
            , fixFreeform   = False
            , tones         = Nothing
            , pickedBass    = False
            }
        , ghl = guard (isnt nullSix RBFile.fixedPartBassGHL && guardDifficulty FoF.diffBassGHL) >> Just PartGHL
          { difficulty = toTier $ FoF.diffBassGHL song
          , hopoThreshold = hopoThreshold
          }
        })
      , ( FlexKeys, (emptyPart :: Part SoftFile)
        { grybo = guard (isnt RBFive.nullFive RBFile.fixedPartKeys && guardDifficulty FoF.diffKeys) >> Just PartGRYBO
          { difficulty = toTier $ FoF.diffKeys song
          , hopoThreshold = hopoThreshold
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proKeys = guard (isnt nullPK RBFile.fixedPartRealKeysX && guardDifficulty FoF.diffKeysReal) >> Just PartProKeys
          { difficulty = toTier $ FoF.diffKeysReal song
          , fixFreeform = False
          }
        })
      , ( FlexExtra "rhythm", (emptyPart :: Part SoftFile)
        { grybo = guard hasRhythm >> Just PartGRYBO
          { difficulty = toTier $ FoF.diffRhythm song
          , hopoThreshold = hopoThreshold
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        })
      , ( FlexExtra "guitar-coop", (emptyPart :: Part SoftFile)
        { grybo = guard (isnt RBFive.nullFive RBFile.fixedPartGuitarCoop && guardDifficulty FoF.diffGuitarCoop) >> Just PartGRYBO
          { difficulty = toTier $ FoF.diffGuitarCoop song
          , hopoThreshold = hopoThreshold
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        })
      , ( FlexVocal, (emptyPart :: Part SoftFile)
        { vocal = flip fmap vocalMode $ \vc -> PartVocal
          { difficulty = toTier $ FoF.diffVocals song
          , count = vc
          , gender = Nothing
          , key = Nothing
          , lipsyncRB3 = Nothing
          }
        })
      , ( FlexExtra "global", (emptyPart :: Part SoftFile)
        { dance = guard (isnt nullDance RBFile.fixedPartDance && guardDifficulty FoF.diffDance) >> Just PartDance
          { difficulty = toTier $ FoF.diffDance song
          }
        })
      ]
    }

removeDummyTracks :: (NNC.C t) => RBFile.FixedFile t -> RBFile.FixedFile t
removeDummyTracks trks = let
  five  fd = fd { RBFive.fiveDifficulties  = scan (onlyOn . RBFive.fiveGems) $ RBFive.fiveDifficulties  fd }
  drums dd = dd { RBDrums.drumDifficulties = scan RBDrums.drumGems           $ RBDrums.drumDifficulties dd }
  onlyOn = RTB.filter $ \case EdgeOn{} -> True; _ -> False
  scan getGems = Map.filter
    $ not
    . null
    . drop 5
    . RTB.toPairList
    . RTB.collectCoincident
    . getGems
  in trks
    { RBFile.fixedPartGuitar      = five  $ RBFile.fixedPartGuitar      trks
    , RBFile.fixedPartBass        = five  $ RBFile.fixedPartBass        trks
    , RBFile.fixedPartKeys        = five  $ RBFile.fixedPartKeys        trks
    , RBFile.fixedPartRhythm      = five  $ RBFile.fixedPartRhythm      trks
    , RBFile.fixedPartGuitarCoop  = five  $ RBFile.fixedPartGuitarCoop  trks
    , RBFile.fixedPartDrums       = drums $ RBFile.fixedPartDrums       trks
    , RBFile.fixedPartDrums2x     = drums $ RBFile.fixedPartDrums2x     trks
    , RBFile.fixedPartRealDrumsPS = drums $ RBFile.fixedPartRealDrumsPS trks
    }

fixShortVoxPhrases
  :: (SendMessage m)
  => RBFile.Song (RBFile.FixedFile U.Beats)
  -> StackTraceT m (RBFile.Song (RBFile.FixedFile U.Beats))
fixShortVoxPhrases song@(RBFile.Song tmap mmap ps)
  | not $ nullVox $ RBFile.fixedHarm1 ps = return song
    -- not touching songs with harmonies
  | not $ RTB.null $ vocalPhrase2 $ RBFile.fixedPartVocals ps = return song
    -- not touching songs with separate p1/p2 phrases
  | otherwise = inside "Track PART VOCALS" $ do
    let vox = RBFile.fixedPartVocals ps
        minLength = 1 :: U.Beats
        joinBools = joinEdgesSimple . fmap
          (\b -> if b then EdgeOn () () else EdgeOff ())
        phrases = RTB.toAbsoluteEventList 0 $ joinBools $ vocalPhrase1 vox
        od = Set.fromList $ ATB.getTimes $ RTB.toAbsoluteEventList 0
          $ RTB.filter id $ vocalOverdrive vox
        wiggle = 10/480
        isOD t = case Set.lookupGT (t NNC.-| wiggle) od of
          Nothing -> False
          Just t' -> t' < t + wiggle
        phrasesWithOD :: RTB.T U.Beats (Onyx.MIDI.Common.Edge () Bool)
        phrasesWithOD
          = splitEdgesSimple
          $ RTB.fromAbsoluteEventList
          $ ATB.fromPairList
          $ fmap (\(t, ((), (), len)) -> (t, ((), isOD t, len)))
          $ ATB.toPairList phrases
        fixed = fixPhrases phrasesWithOD
        fixPhrases = \case
          RNil -> RNil
          Wait dt on@EdgeOn{} (Wait len off@EdgeOff{} rest) ->
            if len >= minLength
              then Wait dt on $ Wait len off $ fixPhrases rest -- all good
              else let
                -- pull phrase start earlier
                len2 = min minLength $ len + dt
                dt2 = (len + dt) - len2
                in if len2 >= minLength
                  then Wait dt2 on $ Wait len2 off $ fixPhrases rest
                  else let
                    -- also push phrase end later
                    futureSpace = case rest of
                      RNil       -> minLength
                      Wait t _ _ -> t
                    len3 = min minLength $ len2 + futureSpace
                    usedSpace = len3 - len2
                    in Wait dt2 on $ Wait len3 off $ fixPhrases $ U.trackDrop usedSpace rest
          Wait dt x rest -> Wait dt x $ fixPhrases rest -- shouldn't happen
        vox' = vox
          { vocalPhrase1 = fmap (\case EdgeOn{} -> True; EdgeOff{} -> False) fixed
          , vocalOverdrive = flip RTB.mapMaybe fixed $ \case
            EdgeOn () True -> Just True
            EdgeOff   True -> Just False
            _              -> Nothing
          }
        differenceTimes = case bothFirstSecond phrasesWithOD fixed of
          (_, only1, _) -> ATB.getTimes $ RTB.toAbsoluteEventList 0
            $ RTB.collectCoincident only1
    case differenceTimes of
      [] -> return ()
      _  -> inside (intercalate ", " $ map (showPosition mmap) differenceTimes) $ do
        warn "Vocal phrase edges extended to be a minimum length of 1 beat"
    return $ RBFile.Song tmap mmap ps { RBFile.fixedPartVocals = vox' }

-- | Moves star power from the GH 1/2 format to the RB format, either if it is
-- specified in the song.ini, or automatically detected from the MIDI.
loadFoFMIDI :: (SendMessage m, MonadIO m, RBFile.ParseFile f) => FoF.Song -> FilePath -> StackTraceT m (RBFile.Song (f U.Beats))
loadFoFMIDI ini fp = do
  mid <- RBFile.loadRawMIDI fp
  let isGtrTrack trk = U.trackName trk `elem` map Just ["PART GUITAR", "PART BASS", "PART RHYTHM", "PART GUITAR COOP", "T1 GEMS"]
      midGH = case mid of
        F.Cons typ dvn trks -> F.Cons typ dvn $ flip map trks $ \trk -> if isGtrTrack trk
          then flip RTB.mapMaybe trk $ \e -> case isNoteEdgeCPV e of
            Just (c, 103, v) -> Just $ makeEdgeCPV c 116 v
            Just (_, 116, _) -> Nothing
            _                -> Just e
          else trk
      -- look for and remove fake OD notes used in lieu of star_power_note;
      -- this was seen in Bocaj Hero V
      midRB = case mid of
        F.Cons typ dvn trks -> fmap (F.Cons typ dvn) $ forM trks $ \trk -> if isGtrTrack trk
          then let
            od = flip RTB.mapMaybe trk $ \e -> case isNoteEdgeCPV e of
              Just (_, 116, Just _) -> Just ()
              _                     -> Nothing
            odless = flip RTB.filter trk $ \e -> case isNoteEdgeCPV e of
              Just (_, 116, _) -> False
              _                -> True
            in case RTB.toPairList od of
              -- look for 2 tiny OD phrases right next to each other
              [(_, ()), (x, ())] | x < (480 * 5) -> do
                lg "Removing thebocaj-style fake OD notes"
                return odless
              _                          -> return trk
          else return trk
      hasPitch n = not $ null $ do
        trk <- case mid of F.Cons _ _ trks -> trks
        guard $ isGtrTrack trk
        e <- toList trk
        case isNoteEdgeCPV e of
          Just (_, n', _) | n == n' -> [()]
          _                         -> []
  mid' <- case FoF.starPowerNote ini of
    Just 103 -> do
      lg "Star Power note specified in song.ini to be 103 (old GH format), converting to RB"
      return midGH
    Just 116 -> do
      lg "Star Power note specified in song.ini to be 116 (RB format)"
      midRB
    Nothing -> if hasPitch 103 && not (hasPitch 116)
      then do
        lg "MIDI auto-detected as old GH Star Power format, converting to RB"
        return midGH
      else do
        lg "MIDI auto-detected as RB Overdrive format, passing through unmodified"
        midRB
    Just n -> do
      warn $ "song.ini has unsupported Star Power pitch of " <> show n <> ", assuming RB format"
      midRB
  RBFile.readMIDIFile' mid'
