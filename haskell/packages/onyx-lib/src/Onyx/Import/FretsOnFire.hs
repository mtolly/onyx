{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.FretsOnFire where

import           Codec.Picture                    (PixelRGBA8 (..),
                                                   convertRGBA8, decodeImage)
import           Codec.Picture.Types              (dropAlphaLayer)
import           Control.Arrow                    (first)
import           Control.Monad                    (forM, forM_, guard, void,
                                                   when)
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isDigit, isSpace, toLower)
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
import           Onyx.MIDI.Track.Drums            as Drums
import qualified Onyx.MIDI.Track.Drums.Elite      as ED
import           Onyx.MIDI.Track.File             (FlexPartName (..))
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.MIDI.Track.Mania            (ManiaTrack (..),
                                                   danceDifficultyName)
import           Onyx.MIDI.Track.ProGuitar        (GtrTuning (..),
                                                   GuitarType (..), nullPG,
                                                   offsetsToTuning)
import           Onyx.MIDI.Track.ProKeys          (nullPK)
import           Onyx.MIDI.Track.SixFret          (nullSix)
import           Onyx.MIDI.Track.Vocal
import qualified Onyx.MIDI.Track.Vocal.Legacy     as RBVox
import           Onyx.Project                     hiding (Difficulty)
import           Onyx.StackTrace
import           Onyx.Util.Handle
import qualified Sound.MIDI.File                  as MIDI
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (dropExtension,
                                                   splitExtension,
                                                   takeExtension, (<.>))
import           Text.Read                        (readMaybe)

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
  :: F.Song (F.FixedFile U.Beats)
  -> F.Song (F.FixedFile U.Beats)
redoSwells (F.Song tmap mmap ps) = let
  fixTrack trk = let
    notes = RTB.collectCoincident
      $ RTB.filter (\(gem, _vel) -> gem /= Drums.Kick)
      $ (fromMaybe mempty $ Map.lookup Expert trk.drumDifficulties).drumGems

    -- for single rolls, don't process ones that include snares.
    -- otherwise we will mess up single rolls under gravity blasts
    allSingleRolls = joinEdgesSimple $ fmap
      (\case Just _ -> EdgeOn () (); Nothing -> EdgeOff ())
      trk.drumSingleRoll
    allSingleRollsWithAbsTime
      = RTB.fromAbsoluteEventList
      $ ATB.fromPairList
      $ map (\(t, ((), (), len)) -> (t, (t, len)))
      $ ATB.toPairList
      $ RTB.toAbsoluteEventList 0 allSingleRolls
    (singleRollsEdit, singleRollsLeave) = flip RTB.partition allSingleRollsWithAbsTime
      $ \(t, len) -> let
        thisLaneNotes = U.trackTake len $ U.trackDrop t notes
        isSnare (Drums.Red, _vel) = True
        isSnare _                 = False
        in all (all $ not . isSnare) thisLaneNotes
    singleRollsEditBools = splitEdgesBool $ snd <$> singleRollsEdit
    singleRollsLeaveBools = splitEdgesBool $ snd <$> singleRollsLeave

    notesWithLanes = fmap (first $ not . null)
      $ flip applyStatus notes $ RTB.merge
        (('s',)          <$> singleRollsEditBools)
        (('d',) . isJust <$> trk.drumDoubleRoll  )
    (lanes1, lanes2) = generateSwells
      $ U.applyTempoTrack tmap notesWithLanes
    in trk
      { drumSingleRoll = fmap (\b -> guard b >> Just LaneExpert)
        $ F.fixFreeform (void notes)
        $ RTB.merge singleRollsLeaveBools
        $ U.unapplyTempoTrack tmap lanes1
      , drumDoubleRoll = fmap (\b -> guard b >> Just LaneExpert)
        $ F.fixFreeform (void notes)
        $ U.unapplyTempoTrack tmap lanes2
      }
  in F.Song tmap mmap ps
    { F.fixedPartDrums       = fixTrack $ F.fixedPartDrums       ps
    , F.fixedPartDrums2x     = fixTrack $ F.fixedPartDrums2x     ps
    , F.fixedPartRealDrumsPS = fixTrack $ F.fixedPartRealDrumsPS ps
    }

data LaneNotes = LaneSingle | LaneDouble
  deriving (Eq, Ord)

importFoF :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> Import m
importFoF src dir level = do
  when (level == ImportFull) $ lg $ "Importing FoF/PS/CH song from: " <> src
  let hasCymbalMarkers chart = flip any (FB.chartTracks chart) $ any $ \case
        FB.Note 66 _ -> True
        FB.Note 67 _ -> True
        FB.Note 68 _ -> True
        _            -> False
  (song, parsed, isChart, chartWithCymbals) <- case findFileCI (pure "song.ini") dir of
    Just rIni -> do
      ini <- FoF.loadSong rIni
      case level of
        ImportQuick -> return (ini, emptyChart, False, False)
        ImportFull  -> case findFileCI (pure "notes.mid") dir of
          Just rMidi -> do
            lg "Found song.ini and notes.mid"
            mid <- loadFoFMIDI ini rMidi
            return (ini, mid, False, False)
          Nothing -> case findFileCI (pure "notes.chart") dir of
            Just rChart -> do
              lg "Found song.ini and notes.chart"
              chart <- FB.chartToBeats <$> FB.loadChartReadable rChart
              mid <- FB.chartToMIDI chart
              -- if .ini delay is 0 or absent, CH uses .chart Offset
              ini' <- if fromMaybe 0 ini.delay == 0
                then case (FB.chartToIni chart).delay of
                  Just 0     -> return ini
                  Nothing    -> return ini
                  chartDelay -> do
                    lg "Using .chart 'Offset' because .ini 'delay' is 0 or absent"
                    return ini { FoF.delay = chartDelay }
                else return ini
              return (ini', mid, True, hasCymbalMarkers chart)
            Nothing -> fatal "Found song.ini, but no notes.mid or notes.chart"
    Nothing -> case findFileCI (pure "notes.chart") dir of
      Just rChart -> do
        lg "Found notes.chart but no song.ini. Metadata will come from .chart"
        chart <- FB.chartToBeats <$> FB.loadChartReadable rChart
        mid <- case level of
          ImportQuick -> return emptyChart
          ImportFull  -> FB.chartToMIDI chart
        return (FB.chartToIni chart, mid, True, hasCymbalMarkers chart)
      Nothing -> fatal "No song.ini or notes.chart found"

  albumArt <- do
    let isImage f = case splitExtension $ T.unpack $ T.toLower f of
          (x, y) -> elem x ["album", "image"] && elem y [".png", ".jpg", ".jpeg"]
    case filter (isImage . fst) $ folderFiles dir of
      (name, rImage) : _ -> return $ Just $ SoftFile (map toLower $ T.unpack name) $ SoftReadable rImage
      []                 -> case findFileCI (pure "label.png") dir of
        Nothing     -> return Nothing
        Just rLabel -> inside "Converting label.png + cassettecolor to square art" $ do
          -- overlay label.png onto cassettecolor
          let hex s = case readHex s of
                [(n, "")] -> return n
                _         -> do
                  warn $ "Couldn't read hex number: " <> s
                  return 0
          bgColor <- case T.unpack <$> song.cassetteColor of
            -- these are the only 2 formats FoF supports (Theme.py, function hexToColor)
            Just ['#', r, g, b] -> PixelRGBA8
              <$> hex [r] <*> hex [g] <*> hex [b] <*> return 255
            Just ['#', r1, r2, g1, g2, b1, b2] -> PixelRGBA8
              <$> hex [r1, r2] <*> hex [g1, g2] <*> hex [b1, b2] <*> return 255
            Nothing -> return $ PixelRGBA8 0 0 0 255
            Just s -> do
              warn $ "Unrecognized cassettecolor format: " <> s
              return $ PixelRGBA8 0 0 0 255
          img <- stackIO (useHandle rLabel handleToByteString)
            >>= either fatal (return . convertRGBA8) . decodeImage . BL.toStrict
          let img' = backgroundColor bgColor $ squareImage 30 img
          return $ Just $ SoftFile "cover.png" $ SoftImage $ dropAlphaLayer img'

  backgroundImage <- case song.background of
    _ | level == ImportQuick -> return Nothing
    Just v | not $ all isSpace v -> do -- PS background reference
      -- Does not support a relative path that goes outside the folder
      case splitPath $ T.pack v of
        Nothing -> do
          warn "Couldn't interpret background path in song.ini"
          return Nothing
        Just path -> case findFileCI path dir of
          Nothing -> do
            warn $ "song.ini references background " <> show v <> " but it wasn't found"
            return Nothing
          Just rBG -> return $ Just
            $ SoftFile ("background" <.> map toLower (takeExtension v))
            $ SoftReadable rBG
    _ -> return $ let -- look for CH background
      isBackground f = case splitExtension $ T.unpack $ T.toLower f of
        ("background", ext) -> elem ext [".png", ".jpg", ".jpeg"]
        _                   -> False
      in case filter (isBackground . fst) $ folderFiles dir of
        []              -> Nothing
        (name, rBG) : _ -> Just $ SoftFile (map toLower $ T.unpack name) $ SoftReadable rBG

  let loadAudioFile _ | level == ImportQuick = return Nothing
      loadAudioFile x = stackIO $ let
        tryExt ext = do
          let template = x <.> ext
          case findFileCI (pure $ T.pack template) dir of
            Just rAudio -> return $ Just (template, rAudio)
            Nothing     -> return Nothing
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
  audioFilesWithChannels <- forM audioFiles $ \(template, rAudio) -> do
    chans <- audioChannelsReadable rAudio
    return ((template, rAudio), chans)

  let softAudio (template, rAudio) = SoftFile template $ SoftReadable rAudio
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
      (drumsAudio, kickAudio, snareAudio, tomsAudio) = case (md1, md2, md3, md4) of
        -- GH config with separate toms vs cymbals;
        -- note according to the phase shift forum post, cymbals are 3 and toms are 4
        -- (swapped from actual GH order)
        (Just d1, Just d2, Just d3, Just d4) -> ([d3], [d1], [d2], [d4])
        _ -> case (md1, md2, md3) of
          (Just d1, Just d2, Just d3) -> ([d3], [d1], [d2], []) -- RB drum mix 1, 2, or 3
          _ -> case (md1, md2) of
            (Just d1, Just d2) -> ([d2], [d1], [], []) -- RB drum mix 4
            _ -> case md0 of
              Just d0 -> ([d0], [], [], []) -- RB drum mix 0
              _       -> (catMaybes [md1, md2, md3, md4], [], [], []) -- either no drums, or weird configuration
      songAudio = toList $ if onlyGuitar then audio_guitar else audio_song

  let (delayAudio, delayPreview, delayMIDI) = case song.delay of
        Nothing -> (id, id, id)
        Just n -> case compare n 0 of
          EQ -> (id, id, id)
          GT -> let
            secs = fromIntegral n / 1000
            midiDelay = ceiling secs
            audioDelay = fromIntegral midiDelay - secs
            in (Pad Start $ CA.Seconds audioDelay, (+ realToFrac audioDelay), F.padFixedFile midiDelay)
          LT -> let
            audioDelay = fromIntegral (abs n) / 1000
            in (Pad Start $ CA.Seconds audioDelay, (+ realToFrac audioDelay), id)
      audioExpr auds = do
        auds' <- NE.nonEmpty $ map fst auds
        Just $ case auds' of
          aud :| [] -> delayAudio $ Input $ Named $ T.pack aud
          _         -> delayAudio $ Mix $ fmap (Input . Named . T.pack) auds'

  -- Used to clamp this at 7, but higher ones should be handled OK now
  let toTier = maybe (Tier 1) $ \n -> Tier $ max 1 $ fromIntegral n + 1

  let maybe2x = listToMaybe $ do
        guard $ level == ImportFull
        (name, r) <- folderFiles dir
        let lower = T.toLower name
        guard $ map toLower (takeExtension $ T.unpack lower) == ".mid"
          && any (`T.isInfixOf` lower) ["expert+", "expertplus", "notes+"]
        return (name, r)
      -- expert+.mid is most common but Drum Projects 2 and 3 also have:
      -- expertplus.mid, notesexpert+.mid, (name of song)Expert+.mid
      -- some also have expert+.chart but we shouldn't have to support that
  forM_ maybe2x $ \(name, _) -> do
    lg $ "Loading separate X+ file: " <> T.unpack name
  -- TODO should we prefer the PS (95) format if there's also a separate midi?
  add2x <- case maybe2x of
    Just (_, rMidi2x) -> do
      parsed2x <- F.loadMIDIReadable rMidi2x
      let trk2x = F.fixedPartDrums $ F.s_tracks parsed2x
      return $ if nullDrums trk2x
        then id
        else \mid -> mid { F.fixedPartDrums2x = trk2x }
    Nothing -> return id
  let (title, is2x) = case song.name of
        Nothing   -> (Nothing, False)
        Just name -> first Just $ determine2xBass name
      hasKicks = if isJust maybe2x
        || not (RTB.null parsed.s_tracks.fixedPartDrums.drumKick2x)
        || not (RTB.null parsed.s_tracks.fixedPartRealDrumsPS.drumKick2x)
        || not (maybe False (RTB.null . ED.edKicks2) $ Map.lookup Expert parsed.s_tracks.fixedPartEliteDrums.tdDifficulties)
        then KicksBoth
        else if is2x then Kicks2x else Kicks1x

  let fixGHVox trks = trks
        { F.fixedPartVocals = stripTags $ RBVox.vocalFromLegacy $ RBVox.fixGHVocals $ RBVox.vocalToLegacy $ F.fixedPartVocals trks
        }
      -- TODO ugh newer CH/YARG rips always use the in-order format (RYBOG) without
      -- the song.ini tag. we may just need to assume it's always the case
      swapFiveLane trks = if fromMaybe False song.fiveLaneDrums
        then trks
          { F.fixedPartDrums   = swapFiveLaneTrack $ F.fixedPartDrums   trks
          , F.fixedPartDrums2x = swapFiveLaneTrack $ F.fixedPartDrums2x trks
          }
        else trks
      swapFiveLaneTrack trk = trk { drumDifficulties = fmap swapFiveLaneDiff trk.drumDifficulties }
      swapFiveLaneDiff dd = dd
        { drumGems = flip fmap dd.drumGems $ \case
          (Drums.Orange            , vel) -> (Drums.Pro Drums.Green (), vel)
          (Drums.Pro Drums.Green (), vel) -> (Drums.Orange            , vel)
          x                               -> x
        }

  outputMIDI <- fixShortVoxPhrases $ checkEnableDynamics $ redoSwells parsed
    { F.s_tracks = fixGHVox $ swapFiveLane $ removeDummyTracks $ add2x $ F.s_tracks parsed
    }
  let outputFixed = F.s_tracks outputMIDI
      outputOnyx = case delayMIDI outputMIDI of
        F.Song tempos sigs fixed -> F.Song tempos sigs $ F.fixedToOnyx fixed
      midi = SoftFile "notes.mid" $ SoftChart outputOnyx

  maybeVideo <- case song.video of
    _ | level == ImportQuick -> return Nothing
    Just v | not $ all isSpace v -> do -- PS video
      -- Does not support a relative path that goes outside the folder
      case splitPath $ T.pack v of
        Nothing -> do
          warn "Couldn't interpret video path in song.ini"
          return Nothing
        Just path -> case findFileCI path dir of
          Nothing -> do
            warn $ "song.ini references video " <> show v <> " but it wasn't found"
            return Nothing
          Just rVideo -> return $ Just (NE.last path, rVideo)
    _ -> return -- CH video
      $ listToMaybe
      $ filter ((== "video") . map toLower . dropExtension . T.unpack . fst)
      $ folderFiles dir
  let videoInfo = flip fmap maybeVideo $ \(name, rVideo) -> VideoInfo
        { fileVideo = SoftFile ("video" <.> takeExtension (T.unpack name)) $ SoftReadable rVideo
        , videoStartTime = song.videoStartTime
        , videoEndTime = song.videoEndTime
        , videoLoop = fromMaybe False song.videoLoop
        }
  -- TODO need to check how video start/end time interacts with audio delay,
  -- so we can adjust based on how we imported the delay

  -- In Phase Shift, if you put -1 as the difficulty for a part,
  -- it explicitly disables it, even if there is a MIDI track for it.
  -- Clone Hero doesn't do this at all and will still show the part.
  -- As a compromise, we use the PS behavior for .mid, and CH for .chart.
  let guardDifficulty diff = if isChart || True -- TODO temporarily doing this for midis also
        then True
        else diff /= Just (-1)
      isnt :: (Eq a, Monoid a) => (a -> Bool) -> (F.FixedFile U.Beats -> a) -> Bool
      isnt isEmpty f = not $ isEmpty $ f outputFixed
      vocalMode = if isnt nullVox F.fixedPartVocals && guardDifficulty song.diffVocals && fmap T.toLower song.charter /= Just "sodamlazy"
        then if isnt nullVox F.fixedHarm2 && guardDifficulty song.diffVocalsHarm
          then if isnt nullVox F.fixedHarm3
            then Just Vocal3
            else Just Vocal2
          else Just Vocal1
        else Nothing
      hasBass = isnt Five.nullFive F.fixedPartBass && guardDifficulty song.diffBass
      hasRhythm = isnt Five.nullFive F.fixedPartRhythm && guardDifficulty song.diffRhythm

  let hopoThreshold = case song.hopoFrequency of
        Just ht -> ht
        -- TODO does PS interpret this as out of 480? or the midi's actual resolution?
        -- for C3 converts it should always be 480 though.
        Nothing -> case song.eighthNoteHOPO of
          Just True -> 250 -- don't know exactly
          _         -> 170

  year <- case song.year of
    Nothing -> return Nothing
    Just s  -> case readMaybe $ T.unpack $ T.takeWhile isDigit s of
      Just y  -> return $ Just y
      Nothing -> do
        warn "Couldn't recognize year from CH format"
        return Nothing

  tuningGtr <- case (song.realGuitarTuning, song.realGuitar22Tuning) of
    (Nothing, Nothing) -> return FoF.PSTuning
      { offsets = [0, 0, 0, 0, 0, 0]
      , name = Nothing
      }
    (Just t, Nothing) -> return t
    (Nothing, Just t) -> return t
    (Just x, Just y) -> do
      when (x.offsets /= y.offsets) $ warn "real_guitar_tuning and real_guitar_22_tuning are different; Onyx does not support this"
      return x

  tuningBass <- case (song.realBassTuning, song.realBass22Tuning) of
    (Nothing, Nothing) -> return FoF.PSTuning
      { offsets = [0, 0, 0, 0]
      , name = Nothing
      }
    (Just t, Nothing) -> return t
    (Nothing, Just t) -> return t
    (Just x, Just y) -> do
      when (x.offsets /= y.offsets) $ warn "real_bass_tuning and real_bass_22_tuning are different; Onyx does not support this"
      return x

  return SongYaml
    { metadata = def'
      { title        = title
      , artist       = song.artist
      , album        = song.album
      , genre        = song.genre
      , year         = year
      , fileAlbumArt = albumArt
      , trackNumber  = song.track
      , comments     = []
      , author       = song.charter
      , previewStart = case song.previewStartTime of
        Just ms | ms >= 0 -> Just $ PreviewSeconds $ delayPreview $ fromIntegral ms / 1000
        _                 -> Nothing
      , previewEnd   = case song.previewEndTime of
        Just ms | ms >= 0 -> Just $ PreviewSeconds $ delayPreview $ fromIntegral ms / 1000
        _                 -> Nothing
      , difficulty   = toTier song.diffBand
      , cover        = maybe False ("cover" `T.isInfixOf`) song.tags
      , loadingPhrase = song.loadingPhrase
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
        , case (audioExpr drumsAudio, audioExpr kickAudio, audioExpr snareAudio, audioExpr tomsAudio) of
          (Nothing, Nothing, Nothing, Nothing) -> []
          (Just kit, Nothing, Nothing, Nothing) -> [(FlexDrums, PartSingle kit)]
          (Just kit, kick, snare, toms) -> [(FlexDrums, PartDrumKit
            { kit   = kit
            , kick  = kick
            , toms  = toms
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
    , parts = Parts $ HM.fromList
      [ ( FlexDrums, (emptyPart :: Part SoftFile)
        { drums = do
          guard
            $ (isnt nullDrums F.fixedPartDrums || isnt nullDrums F.fixedPartRealDrumsPS || isnt ED.nullEliteDrums F.fixedPartEliteDrums)
            && guardDifficulty song.diffDrums
          let mode = let
                isTrue = isnt ED.nullEliteDrums F.fixedPartEliteDrums
                isFiveLane = song.fiveLaneDrums == Just True || any
                  (\(_, dd) -> any (\(gem, _vel) -> gem == Drums.Orange) dd.drumGems)
                  (Map.toList outputFixed.fixedPartDrums.drumDifficulties)
                isReal = isnt nullDrums F.fixedPartRealDrumsPS
                isPro = case song.proDrums of
                  Just b  -> b
                  Nothing -> not (RTB.null outputFixed.fixedPartDrums.drumToms)
                    || chartWithCymbals -- handle the case where a .chart has cymbal markers, and no toms
                in if
                  | isTrue     -> DrumsTrue
                  | isFiveLane -> Drums5
                  | isReal     -> DrumsReal
                  | isPro      -> DrumsPro
                  | otherwise  -> Drums4
          Just (emptyPartDrums mode hasKicks)
            { difficulty = toTier song.diffDrums
            , fallback = if fromMaybe False song.drumFallbackBlue
              then FallbackBlue
              else FallbackGreen
            }
        })
      , ( FlexGuitar, (emptyPart :: Part SoftFile)
        { grybo = guard (isnt Five.nullFive F.fixedPartGuitar && guardDifficulty song.diffGuitar) >> Just PartGRYBO
          { difficulty = toTier song.diffGuitar
          , hopoThreshold = hopoThreshold
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proGuitar = let
          b =  (isnt nullPG F.fixedPartRealGuitar   && guardDifficulty song.diffGuitarReal  )
            || (isnt nullPG F.fixedPartRealGuitar22 && guardDifficulty song.diffGuitarReal22)
          in guard b >> Just PartProGuitar
            { difficulty    = toTier song.diffGuitarReal
            , hopoThreshold = hopoThreshold
            , tuning        = (offsetsToTuning TypeGuitar tuningGtr.offsets)
              { gtrName = tuningGtr.name
              }
            , tuningRSBass  = Nothing
            , fixFreeform   = False
            , tones         = Nothing
            , pickedBass    = False
            }
        , ghl = guard (isnt nullSix F.fixedPartGuitarGHL && guardDifficulty song.diffGuitarGHL) >> Just PartGHL
          { difficulty = toTier song.diffGuitarGHL
          , hopoThreshold = hopoThreshold
          }
        })
      , ( FlexBass, (emptyPart :: Part SoftFile)
        { grybo = guard hasBass >> Just PartGRYBO
          { difficulty = toTier song.diffBass
          , hopoThreshold = hopoThreshold
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proGuitar = let
          b =  (isnt nullPG F.fixedPartRealBass   && guardDifficulty song.diffBassReal  )
            || (isnt nullPG F.fixedPartRealBass22 && guardDifficulty song.diffBassReal22)
          in guard b >> Just PartProGuitar
            { difficulty    = toTier song.diffBassReal
            , hopoThreshold = hopoThreshold
            , tuning        = (offsetsToTuning TypeBass tuningBass.offsets)
              { gtrName = tuningBass.name
              }
            , tuningRSBass  = Nothing
            , fixFreeform   = False
            , tones         = Nothing
            , pickedBass    = False
            }
        , ghl = guard (isnt nullSix F.fixedPartBassGHL && guardDifficulty song.diffBassGHL) >> Just PartGHL
          { difficulty = toTier song.diffBassGHL
          , hopoThreshold = hopoThreshold
          }
        })
      , ( FlexKeys, (emptyPart :: Part SoftFile)
        { grybo = guard (isnt Five.nullFive F.fixedPartKeys && guardDifficulty song.diffKeys) >> Just PartGRYBO
          { difficulty = toTier song.diffKeys
          , hopoThreshold = hopoThreshold
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proKeys = guard (isnt nullPK F.fixedPartRealKeysX && guardDifficulty song.diffKeysReal) >> Just PartProKeys
          { difficulty = toTier song.diffKeysReal
          , fixFreeform = False
          }
        })
      , ( FlexExtra "rhythm", (emptyPart :: Part SoftFile)
        { grybo = guard hasRhythm >> Just PartGRYBO
          { difficulty = toTier song.diffRhythm
          , hopoThreshold = hopoThreshold
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        })
      , ( FlexExtra "guitar-coop", (emptyPart :: Part SoftFile)
        { grybo = guard (isnt Five.nullFive F.fixedPartGuitarCoop && guardDifficulty song.diffGuitarCoop) >> Just PartGRYBO
          { difficulty = toTier song.diffGuitarCoop
          , hopoThreshold = hopoThreshold
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        })
      , ( FlexVocal, (emptyPart :: Part SoftFile)
        { vocal = flip fmap vocalMode $ \vc -> PartVocal
          { difficulty = toTier song.diffVocals
          , count = vc
          , gender = Nothing
          , key = Nothing
          , lipsyncRB3 = Nothing
          }
        })
      , ( FlexExtra "dance", (emptyPart :: Part SoftFile)
        { mania = do
          guard $ guardDifficulty song.diffDance
          let maniaDiffList = do
                diff <- [minBound .. maxBound]
                let name = danceDifficultyName diff
                maniaTrack <- toList
                  $ Map.lookup (FlexExtra "dance") outputOnyx.s_tracks.onyxParts
                  >>= Map.lookup name . (.onyxPartMania)
                guard $ not $ RTB.null maniaTrack.maniaNotes
                return name
          maniaDiffs <- NE.nonEmpty maniaDiffList
          Just PartMania
            { difficulty = toTier song.diffDance
            , keys = 4 -- could be lower? hopefully not higher
            , turntable = False
            , instrument = Nothing
            , charts = maniaDiffs
            }
        })
      ]
    }

removeDummyTracks :: (NNC.C t) => F.FixedFile t -> F.FixedFile t
removeDummyTracks trks = let
  five  fd = fd { Five.fiveDifficulties  = scan (onlyOn . (.fiveGems)) fd.fiveDifficulties }
  drums dd = dd { Drums.drumDifficulties = scan (.drumGems)            dd.drumDifficulties }
  onlyOn = RTB.filter $ \case EdgeOn{} -> True; _ -> False
  scan getGems = Map.filter
    $ not
    . null
    . drop 5
    . RTB.toPairList
    . RTB.collectCoincident
    . getGems
  in trks
    { F.fixedPartGuitar      = five  $ F.fixedPartGuitar      trks
    , F.fixedPartBass        = five  $ F.fixedPartBass        trks
    , F.fixedPartKeys        = five  $ F.fixedPartKeys        trks
    , F.fixedPartRhythm      = five  $ F.fixedPartRhythm      trks
    , F.fixedPartGuitarCoop  = five  $ F.fixedPartGuitarCoop  trks
    , F.fixedPartDrums       = drums $ F.fixedPartDrums       trks
    , F.fixedPartDrums2x     = drums $ F.fixedPartDrums2x     trks
    , F.fixedPartRealDrumsPS = drums $ F.fixedPartRealDrumsPS trks
    }

fixShortVoxPhrasesTrack
  :: VocalTrack U.Beats -> (VocalTrack U.Beats, [U.Beats])
fixShortVoxPhrasesTrack vox = let
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
  in (vox', differenceTimes)

fixShortVoxPhrases
  :: (SendMessage m)
  => F.Song (F.FixedFile U.Beats)
  -> StackTraceT m (F.Song (F.FixedFile U.Beats))
fixShortVoxPhrases song@(F.Song tmap mmap ps)
  | not $ nullVox $ F.fixedHarm1 ps = return song
    -- not touching songs with harmonies
  | not $ RTB.null $ vocalPhrase2 $ F.fixedPartVocals ps = return song
    -- not touching songs with separate p1/p2 phrases
  | otherwise = inside "Track PART VOCALS" $ do
    let vox = F.fixedPartVocals ps
        (vox', differenceTimes) = fixShortVoxPhrasesTrack vox
    case differenceTimes of
      [] -> return ()
      _  -> inside (intercalate ", " $ map (showPosition mmap) differenceTimes) $ do
        warn "Vocal phrase edges extended to be a minimum length of 1 beat"
    return $ F.Song tmap mmap ps { F.fixedPartVocals = vox' }

-- | Moves star power from the GH 1/2 format to the RB format, either if it is
-- specified in the song.ini, or automatically detected from the MIDI.
loadFoFMIDI :: (SendMessage m, MonadIO m, F.ParseFile f) => FoF.Song -> Readable -> StackTraceT m (F.Song (f U.Beats))
loadFoFMIDI ini r = do
  mid <- F.loadRawMIDIReadable r
  let isGtrTrack trk = U.trackName trk `elem` map Just ["PART GUITAR", "PART BASS", "PART RHYTHM", "PART GUITAR COOP", "T1 GEMS"]
      midGH = case mid of
        MIDI.Cons typ dvn trks -> MIDI.Cons typ dvn $ flip map trks $ \trk -> if isGtrTrack trk
          then flip RTB.mapMaybe trk $ \e -> case isNoteEdgeCPV e of
            Just (c, 103, v) -> Just $ makeEdgeCPV c 116 v
            Just (_, 116, _) -> Nothing
            _                -> Just e
          else trk
      -- look for and remove fake OD notes used in lieu of star_power_note;
      -- this was seen in Bocaj Hero V
      midRB = case mid of
        MIDI.Cons typ dvn trks -> fmap (MIDI.Cons typ dvn) $ forM trks $ \trk -> if isGtrTrack trk
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
        trk <- case mid of MIDI.Cons _ _ trks -> trks
        guard $ isGtrTrack trk
        e <- toList trk
        case isNoteEdgeCPV e of
          Just (_, n', _) | n == n' -> [()]
          _                         -> []
  mid' <- case ini.starPowerNote of
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
  F.readMIDIFile' mid'
