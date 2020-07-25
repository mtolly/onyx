{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Import (importFoF, importRBA, importSTFSDir, importSTFS, importMagma, importAmplitude, simpleRBAtoCON, Kicks(..)) where

import qualified Amplitude.File                   as Amp
import           Audio
import qualified C3
import           Codec.Picture                    (convertRGB8, readImage)
import           Config                           hiding (Difficulty)
import qualified Config
import           Control.Applicative              ((<|>))
import           Control.Arrow                    (first, second)
import           Control.Exception                (evaluate)
import           Control.Monad.Extra              (forM, forM_, guard, void)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isSpace, toLower)
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (Default, def)
import qualified Data.Digest.Pure.MD5             as MD5
import qualified Data.DTA                         as D
import           Data.DTA.Lex                     (scanStack)
import           Data.DTA.Parse                   (parseStack)
import qualified Data.DTA.Serialize               as D
import qualified Data.DTA.Serialize.Amplitude     as Amp
import qualified Data.DTA.Serialize.Magma         as RBProj
import qualified Data.DTA.Serialize.RB3           as D
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (elemIndex, intercalate, nub)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   listToMaybe, mapMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Tuple.Extra                 (fst3, snd3, thd3)
import           Difficulty
import qualified FeedBack.Load                    as FB
import qualified FretsOnFire                      as FoF
import           Guitars                          (applyStatus)
import           Image                            (DXTFormat (PNGXbox),
                                                   toDXT1File)
import           JSONData                         (toJSON, yamlEncodeFile)
import           Magma                            (getRBAFile)
import qualified Numeric.NonNegative.Class        as NNC
import           OSFiles                          (fixFileCase)
import           PhaseShift.Dance                 (nullDance)
import           PrettyDTA                        (C3DTAComments (..),
                                                   DTASingle (..),
                                                   readDTASingle,
                                                   readFileSongsDTA, readRB3DTA,
                                                   writeDTASingle)
import           Resources                        (rb3Updates)
import           RockBand.Codec.Drums             as RBDrums
import           RockBand.Codec.File              (FlexPartName (..))
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as RBFive
import           RockBand.Codec.ProGuitar         (GtrBase (..), GtrTuning (..),
                                                   nullPG)
import qualified RockBand.Codec.ProGuitar         as PG
import           RockBand.Codec.ProKeys           (nullPK)
import           RockBand.Codec.Six               (nullSix)
import           RockBand.Codec.Vocal
import           RockBand.Common
import qualified RockBand.Legacy.Vocal            as RBVox
import           Scripts                          (fixFreeform, loadFoFMIDI)
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           STFS.Package                     (extractSTFS, rb3pkg)
import qualified System.Directory                 as Dir
import           System.FilePath
import           Text.Decode                      (decodeGeneral)
import           Text.Read                        (readMaybe)

removeDummyTracks :: (NNC.C t) => RBFile.FixedFile t -> RBFile.FixedFile t
removeDummyTracks trks = let
  five  fd = fd { RBFive.fiveDifficulties  = scan RBFive.fiveGems  $ RBFive.fiveDifficulties  fd }
  drums dd = dd { RBDrums.drumDifficulties = scan RBDrums.drumGems $ RBDrums.drumDifficulties dd }
  scan getGems = Map.filter $ (> 5) . length . getGems
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
        phrasesWithOD :: RTB.T U.Beats (RockBand.Common.Edge () Bool)
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
                    usedSpace = len3 - len
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
      _  -> inside (intercalate ", " $ map (RBFile.showPosition mmap) differenceTimes) $ do
        warn "Vocal phrase edges extended to be a minimum length of 1 beat"
    return $ RBFile.Song tmap mmap ps { RBFile.fixedPartVocals = vox' }

generateSwells
  :: (Eq a)
  => RTB.T U.Seconds (Bool, [a])
  -> (RTB.T U.Seconds Bool, RTB.T U.Seconds Bool)
generateSwells notes = let
  maxDistance = 0.155 :: U.Seconds -- actually 0.16 but being conservative
  go RNil = RNil
  go (Wait t1 (True, [x]) (Wait t2 (True, [y]) rest)) | t2 < maxDistance = let
    thisType = if x == y then LaneSingle else LaneDouble
    in case splitLane x y rest of
      (lane, after) -> let
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
  go (Wait t _ rest) = RTB.delay t $ go rest
  splitLane gem1 gem2 (Wait t (True, [x]) rest)
    | x == gem1
    = case splitLane gem2 gem1 rest of
      (lane, after) -> (Wait t x lane, after)
  splitLane _ _ rest = (RNil, rest)
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
      $ RTB.filter (/= RBDrums.Kick) $ drumGems $ fromMaybe mempty
      $ Map.lookup Expert $ drumDifficulties trk
    notesWithLanes = fmap (first $ not . null)
      $ flip applyStatus notes $ RTB.merge
        (fmap (('s',) . isJust) $ drumSingleRoll trk)
        (fmap (('d',) . isJust) $ drumDoubleRoll trk)
    (lanes1, lanes2) = generateSwells
      $ U.applyTempoTrack tmap notesWithLanes
    in trk
      { drumSingleRoll = fmap (\b -> guard b >> Just LaneExpert)
        $ fixFreeform (void notes)
        $ U.unapplyTempoTrack tmap lanes1
      , drumDoubleRoll = fmap (\b -> guard b >> Just LaneExpert)
        $ fixFreeform (void notes)
        $ U.unapplyTempoTrack tmap lanes2
      }
  in RBFile.Song tmap mmap ps
    { RBFile.fixedPartDrums       = fixTrack $ RBFile.fixedPartDrums       ps
    , RBFile.fixedPartDrums2x     = fixTrack $ RBFile.fixedPartDrums2x     ps
    , RBFile.fixedPartRealDrumsPS = fixTrack $ RBFile.fixedPartRealDrumsPS ps
    }

data LaneNotes = LaneSingle | LaneDouble
  deriving (Eq, Ord)

importFoF :: (SendMessage m, MonadIO m) => FilePath -> FilePath -> StackTraceT m Kicks
importFoF src dest = do
  lg $ "Importing FoF/PS/CH song from folder: " <> src
  allFiles <- stackIO $ Dir.listDirectory src
  pathMid <- fixFileCase $ src </> "notes.mid"
  pathChart <- fixFileCase $ src </> "notes.chart"
  pathIni <- fixFileCase $ src </> "song.ini"
  (song, parsed, isChart) <- stackIO (Dir.doesFileExist pathIni) >>= \case
    True -> do
      ini <- FoF.loadSong pathIni
      stackIO (Dir.doesFileExist pathMid) >>= \case
        True -> do
          lg "Found song.ini and notes.mid"
          mid <- loadFoFMIDI ini pathMid
          return (ini, mid, False)
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
            return (ini', mid, True)
          False -> fatal "Found song.ini, but no notes.mid or notes.chart"
    False -> stackIO (Dir.doesFileExist pathChart) >>= \case
      True -> do
        lg "Found notes.chart but no song.ini. Metadata will come from .chart"
        chart <- FB.chartToBeats <$> FB.loadChartFile pathChart
        mid <- FB.chartToMIDI chart
        return (FB.chartToIni chart, mid, True)
      False -> fatal "No song.ini or notes.chart found"

  albumArt <- stackIO $ do
    let isImage f = case splitExtension $ map toLower f of
          (x, y) -> elem x ["album", "image"] && elem y [".png", ".jpg", ".jpeg"]
    case filter isImage allFiles of
      []    -> return Nothing
      f : _ -> do
        Dir.copyFile (src </> f) (dest </> map toLower f)
        return $ Just f

  let loadAudioFile x = stackIO $ let
        tryExt ext = do
          let template = x <.> ext
          path <- fixFileCase $ src </> template
          Dir.doesFileExist path >>= \case
            True -> do
              Dir.copyFile path $ dest </> template
              return $ Just template
            False -> return Nothing
        in tryExt "ogg" >>= \case
          Nothing -> tryExt "mp3" >>= \case
            Nothing -> tryExt "wav"
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
        [x] | dropExtension x == "guitar" -> True
        _                                 -> False
  audioFilesWithChannels <- forM audioFiles $ \af -> audioChannels (dest </> af) >>= \case
    Nothing    -> fatal $ "Couldn't get channel count of audio file: " <> af
    Just chans -> return (af, chans)

  let gtrAudio = if onlyGuitar then [] else toList audio_guitar
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
      audioExpr [] = Nothing
      audioExpr [aud] = Just PlanAudio
        { _planExpr = delayAudio $ Input $ Named $ T.pack aud
        , _planPans = []
        , _planVols = []
        }
      audioExpr auds = Just PlanAudio
        { _planExpr = delayAudio $ Mix $ map (Input . Named . T.pack) auds
        , _planPans = []
        , _planVols = []
        }

  let toTier = maybe (Tier 1) $ \n -> Tier $ max 1 $ min 7 $ fromIntegral n + 1

  let maybePath2x = listToMaybe $ flip filter allFiles $ \f -> let
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
      hasKicks = if isJust maybePath2x || not (RTB.null $ drumKick2x $ RBFile.fixedPartDrums $ RBFile.s_tracks parsed)
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
          RBDrums.Orange               -> RBDrums.Pro RBDrums.Green ()
          RBDrums.Pro RBDrums.Green () -> RBDrums.Orange
          x                            -> x
        }

  outputMIDI <- fixShortVoxPhrases $ redoSwells parsed
    { RBFile.s_tracks = fixGHVox $ swapFiveLane $ removeDummyTracks $ add2x $ RBFile.s_tracks parsed
    }
  let outputFixed = RBFile.s_tracks outputMIDI
  stackIO $ Save.toFile (dest </> "notes.mid") $ RBFile.showMIDIFile' $ delayMIDI outputMIDI

  -- TODO get this working with Clone Hero videos
  vid <- case FoF.video song of
    Nothing -> return Nothing
    Just s | all isSpace s -> return Nothing
    Just v -> inside "copying PS video file to onyx project" $ do
      v' <- fixFileCase $ src </> v
      stackIO $ Dir.copyFile v' (dest </> "video.avi")
      return $ Just "video.avi"

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

  stackIO $ yamlEncodeFile (dest </> "song.yml") $ toJSON SongYaml
    { _metadata = Metadata
      { _title        = title
      , _artist       = FoF.artist song
      , _album        = FoF.album song
      , _genre        = FoF.genre song
      , _subgenre     = Nothing
      , _year         = FoF.year song
      , _fileAlbumArt = albumArt
      , _trackNumber  = FoF.track song
      , _comments     = []
      , _key          = Nothing
      , _author       = FoF.charter song
      , _rating       = Unrated
      , _previewStart = case FoF.previewStartTime song of
        Just ms | ms >= 0 -> Just $ PreviewSeconds $ fromIntegral ms / 1000
        _                 -> Nothing
      , _previewEnd   = Nothing
      , _languages    = _languages def'
      , _convert      = _convert def'
      , _rhythmKeys   = _rhythmKeys def'
      , _rhythmBass   = _rhythmBass def'
      , _catEMH       = _catEMH def'
      , _expertOnly   = _expertOnly def'
      , _cover        = _cover def'
      , _difficulty   = toTier $ FoF.diffBand song
      }
    , _global = def'
    , _audio = HM.fromList $ flip map audioFilesWithChannels $ \(aud, chans) ->
      (T.pack aud, AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just aud
        , _rate = Nothing
        , _channels = chans
        }
      )
    , _jammit = HM.empty
    , _plans = HM.singleton "fof" Plan
      { _song         = audioExpr songAudio
      , _countin      = Countin []
      , _planParts    = Parts $ HM.fromList $ concat
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
            { drumsSplitKit = kit
            , drumsSplitKick = kick
            , drumsSplitSnare = snare
            })]
          _ -> error "FoF import: unsupported drums audio configuration (kick/snare but no kit)"
        ]
      , _crowd = audioExpr crowdAudio
      , _planComments = []
      , _tuningCents = 0
      , _fileTempo = Nothing
      }
    , _targets = HM.singleton "ps" $ PS def' { ps_FileVideo = vid }
    , _parts = Parts $ HM.fromList
      [ ( FlexDrums, def
        { partDrums = guard ((isnt nullDrums RBFile.fixedPartDrums || isnt nullDrums RBFile.fixedPartRealDrumsPS) && guardDifficulty FoF.diffDrums) >> Just PartDrums
          { drumsDifficulty = toTier $ FoF.diffDrums song
          , drumsMode = let
            isFiveLane = FoF.fiveLaneDrums song == Just True || any
              (\(_, dd) -> RBDrums.Orange `elem` drumGems dd)
              (Map.toList $ drumDifficulties $ RBFile.fixedPartDrums outputFixed)
            isReal = isnt nullDrums RBFile.fixedPartRealDrumsPS
            isPro = case FoF.proDrums song of
              Just b  -> b
              Nothing -> not $ RTB.null $ drumToms $ RBFile.fixedPartDrums outputFixed
            in if isFiveLane then Drums5
              else if isReal then DrumsReal
                else if isPro then DrumsPro
                  else Drums4
          , drumsKicks = hasKicks
          , drumsFixFreeform = False
          , drumsKit = HardRockKit
          , drumsLayout = StandardLayout
          , drumsFallback = if fromMaybe False $ FoF.drumFallbackBlue song
            then FallbackBlue
            else FallbackGreen
          }
        })
      , ( FlexGuitar, def
        { partGRYBO = guard (isnt RBFive.nullFive RBFile.fixedPartGuitar && guardDifficulty FoF.diffGuitar) >> Just PartGRYBO
          { gryboDifficulty = toTier $ FoF.diffGuitar song
          , gryboHopoThreshold = hopoThreshold
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProGuitar = let
          b =  (isnt nullPG RBFile.fixedPartRealGuitar   && guardDifficulty FoF.diffGuitarReal  )
            || (isnt nullPG RBFile.fixedPartRealGuitar22 && guardDifficulty FoF.diffGuitarReal22)
          in guard b >> Just PartProGuitar
            { pgDifficulty = toTier $ FoF.diffGuitarReal song
            , pgHopoThreshold = hopoThreshold
            , pgTuning = def
            , pgFixFreeform = False
            }
        , partGHL = guard (isnt nullSix RBFile.fixedPartGuitarGHL && guardDifficulty FoF.diffGuitarGHL) >> Just PartGHL
          { ghlDifficulty = toTier $ FoF.diffGuitarGHL song
          , ghlHopoThreshold = hopoThreshold
          }
        })
      , ( FlexBass, def
        { partGRYBO = guard hasBass >> Just PartGRYBO
          { gryboDifficulty = toTier $ FoF.diffBass song
          , gryboHopoThreshold = hopoThreshold
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProGuitar = let
          b =  (isnt nullPG RBFile.fixedPartRealBass   && guardDifficulty FoF.diffBassReal  )
            || (isnt nullPG RBFile.fixedPartRealBass22 && guardDifficulty FoF.diffBassReal22)
          in guard b >> Just PartProGuitar
            { pgDifficulty = toTier $ FoF.diffBassReal song
            , pgHopoThreshold = hopoThreshold
            , pgTuning = def
            , pgFixFreeform = False
            }
        , partGHL = guard (isnt nullSix RBFile.fixedPartBassGHL && guardDifficulty FoF.diffBassGHL) >> Just PartGHL
          { ghlDifficulty = toTier $ FoF.diffBassGHL song
          , ghlHopoThreshold = hopoThreshold
          }
        })
      , ( FlexKeys, def
        { partGRYBO = guard (isnt RBFive.nullFive RBFile.fixedPartKeys && guardDifficulty FoF.diffKeys) >> Just PartGRYBO
          { gryboDifficulty = toTier $ FoF.diffKeys song
          , gryboHopoThreshold = hopoThreshold
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProKeys = guard (isnt nullPK RBFile.fixedPartRealKeysX && guardDifficulty FoF.diffKeysReal) >> Just PartProKeys
          { pkDifficulty = toTier $ FoF.diffKeysReal song
          , pkFixFreeform = False
          }
        })
      , ( FlexExtra "rhythm", def
        { partGRYBO = guard hasRhythm >> Just PartGRYBO
          { gryboDifficulty = toTier $ FoF.diffRhythm song
          , gryboHopoThreshold = hopoThreshold
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        })
      , ( FlexVocal, def'
        { partVocal = flip fmap vocalMode $ \vc -> PartVocal
          { vocalDifficulty = toTier $ FoF.diffVocals song
          , vocalCount = vc
          , vocalGender = Nothing
          , vocalKey = Nothing
          , vocalLipsyncRB3 = Nothing
          , vocalLipsyncRB2 = Nothing
          }
        })
      , ( FlexExtra "global", def
        { partDance = guard (isnt nullDance RBFile.fixedPartDance && guardDifficulty FoF.diffDance) >> Just PartDance
          { danceDifficulty = toTier $ FoF.diffDance song
          }
        })
      ]
    }

  return hasKicks

determine2xBass :: T.Text -> (T.Text, Bool)
determine2xBass s = let
  stripInfix inf str = case T.breakOn inf str of
    (x, y) -> (x <>) <$> T.stripPrefix inf y
  in case stripInfix " (2x Bass Pedal)" s <|> stripInfix " (2X Bass Pedal)" s of
    Nothing -> (s , False)
    Just s' -> (s', True )

dtaIsRB3 :: D.SongPackage -> Bool
dtaIsRB3 pkg = maybe False (`elem` ["rb3", "rb3_dlc", "ugc_plus"]) $ D.gameOrigin pkg
  -- rbn1 songs have (game_origin rb2) (ugc 1)

dtaIsHarmonixRB3 :: D.SongPackage -> Bool
dtaIsHarmonixRB3 pkg = maybe False (`elem` ["rb3", "rb3_dlc"]) $ D.gameOrigin pkg

importSTFSDir :: (SendMessage m, MonadResource m) => Int -> FilePath -> Maybe FilePath -> FilePath -> StackTraceT m Kicks
importSTFSDir index temp mtemp2x dir = do
  packSongs <- readFileSongsDTA $ temp </> "songs/songs.dta"
  DTASingle top pkg comments <- case drop index packSongs of
    []            -> fatal $ "Couldn't find song index " <> show index
    (song, _) : _ -> return song
  updateDir <- stackIO rb3Updates
  let (title, auto2x) = determine2xBass $ D.name pkg
      is2x = fromMaybe auto2x $ c3dta2xBass comments
      meta = def'
        { _author = c3dtaAuthoredBy comments
        , _title = Just title
        , _convert = fromMaybe (_convert def') $ c3dtaConvert comments
        , _rhythmKeys = fromMaybe (_rhythmKeys def') $ c3dtaRhythmKeys comments
        , _rhythmBass = fromMaybe (_rhythmBass def') $ c3dtaRhythmBass comments
        , _catEMH = fromMaybe (_catEMH def') $ c3dtaCATemh comments
        , _expertOnly = fromMaybe (_expertOnly def') $ c3dtaExpertOnly comments
        , _languages = fromMaybe (_languages def') $ c3dtaLanguages comments
        }
      karaoke = fromMaybe False $ c3dtaKaraoke comments
      multitrack = fromMaybe False $ c3dtaMultitrack comments
      base = T.unpack $ D.songName $ D.song pkg
      -- Note: the base path does NOT necessarily have to be songs/foo/foo
      -- where foo is the top key of songs.dta. foo can be different!
      -- e.g. C3's "Escape from the City" has a top key 'SonicAdvCityEscape2x'
      -- and a 'name' of "songs/sonicadv2cityescape2x/sonicadv2cityescape2x"
      updateFile = do
        guard $ maybe False ("disc_update" `elem`) $ D.extraAuthoring pkg
        Just $ updateDir </> T.unpack top </> (T.unpack top ++ "_update.mid")
      hasKicks = if isJust mtemp2x then KicksBoth else if is2x then Kicks2x else Kicks1x
      mmilo = do
        guard $ dtaIsRB3 pkg
        Just $ temp </> takeDirectory base </> "gen" </> takeFileName base <.> "milo_xbox"
      missingArt = updateDir </> T.unpack top </> "gen" </> (T.unpack top ++ "_keep.png_xbox")
      with2xPath maybe2x = do
        albumArtFile <- case D.albumArt pkg of
          Just True -> stackIO (Dir.doesFileExist missingArt) >>= return . Just . \case
            True -> missingArt -- old rb1 song with album art on rb3 disc
            False -> temp </> takeDirectory base </> "gen" </> (takeFileName base ++ "_keep.png_xbox")
          _ -> return Nothing
        importRB3 pkg meta karaoke multitrack hasKicks
          (temp </> base <.> "mid") updateFile maybe2x (temp </> base <.> "mogg")
          ((, "cover.png_xbox") <$> albumArtFile) mmilo dir
        return hasKicks
  case mtemp2x of
    Nothing -> with2xPath Nothing
    Just temp2x -> do
      DTASingle _ pkg2x _ <- readDTASingle $ temp2x </> "songs/songs.dta"
      let base2x = T.unpack $ D.songName $ D.song pkg2x
      with2xPath $ Just (pkg2x, temp2x </> base2x <.> "mid")

importSTFS :: (SendMessage m, MonadResource m) => Int -> FilePath -> Maybe FilePath -> FilePath -> StackTraceT m Kicks
importSTFS index file file2x dir = tempDir "onyx_con" $ \temp -> do
  lg $ "Importing STFS file from: " ++ file
  forM_ file2x $ \f2x -> lg $ "Plus 2x Bass Pedal from: " ++ f2x
  stackIO $ extractSTFS file temp
  let with2xPath mtemp2x = importSTFSDir index temp mtemp2x dir
  case file2x of
    Nothing -> with2xPath Nothing
    Just f2x -> tempDir "onyx_con2x" $ \temp2x -> do
      stackIO $ extractSTFS f2x temp2x
      with2xPath $ Just temp2x

-- | Converts a Magma v2 RBA to CON without going through an import + recompile.
simpleRBAtoCON :: (SendMessage m, MonadResource m) => FilePath -> FilePath -> StackTraceT m ()
simpleRBAtoCON rba con = inside ("converting RBA " ++ show rba ++ " to CON " ++ show con) $ do
  tempDir "onyx_rba2con" $ \temp -> do
    md5 <- stackIO $ BL.readFile rba >>= evaluate . MD5.md5
    let shortName = "onyx" ++ take 10 (show md5)
    stackIO $ Dir.createDirectoryIfMissing True $ temp </> "songs" </> shortName </> "gen"
    getRBAFile 0 rba $ temp </> "temp_songs.dta"
    getRBAFile 1 rba $ temp </> "songs" </> shortName </> shortName <.> "mid"
    getRBAFile 2 rba $ temp </> "songs" </> shortName </> shortName <.> "mogg"
    getRBAFile 3 rba $ temp </> "songs" </> shortName </> "gen" </> shortName <.> "milo_xbox"
    getRBAFile 4 rba $ temp </> "temp_cover.bmp"
    -- 5 is weights.bin (empty in magma v2)
    getRBAFile 6 rba $ temp </> "temp_extra.dta"
    (_, pkg, isUTF8) <- readRB3DTA $ temp </> "temp_songs.dta"
    extra <- (if isUTF8 then D.readFileDTA_utf8' else D.readFileDTA_latin1') $ temp </> "temp_extra.dta"
    stackIO
      $ B8.writeFile (temp </> "songs/songs.dta")
      $ (if isUTF8 then TE.encodeUtf8 else B8.pack . T.unpack)
      $ writeDTASingle DTASingle
      { dtaTopKey = T.pack shortName
      , dtaSongPackage = pkg
        { D.song = (D.song pkg)
          { D.songName = T.pack $ "songs" </> shortName </> shortName
          }
        , D.songId = Just $ Right $ T.pack shortName
        }
      , dtaC3Comments = C3DTAComments
        { c3dtaCreatedUsing = Nothing
        , c3dtaAuthoredBy   = case extra of
          D.DTA _ (D.Tree _ [D.Parens (D.Tree _
            ( D.String "backend"
            : D.Parens (D.Tree _ [D.Sym "author", D.String s])
            : _
            ))])
            -> Just s
          _ -> Nothing
        , c3dtaSong         = Nothing
        , c3dtaLanguages    = Nothing -- TODO
        , c3dtaKaraoke      = Nothing
        , c3dtaMultitrack   = Nothing
        , c3dtaConvert      = Nothing
        , c3dta2xBass       = Nothing
        , c3dtaRhythmKeys   = Nothing
        , c3dtaRhythmBass   = Nothing
        , c3dtaCATemh       = Nothing
        , c3dtaExpertOnly   = Nothing
        }
      }
    stackIO $ readImage (temp </> "temp_cover.bmp") >>= \case
      Left err -> error err -- TODO
      Right dyn -> let
        out = temp </> "songs" </> shortName </> "gen" </> (shortName ++ "_keep.png_xbox")
        in BL.writeFile out $ toDXT1File PNGXbox $ convertRGB8 dyn
    stackIO $ do
      Dir.removeFile $ temp </> "temp_songs.dta"
      Dir.removeFile $ temp </> "temp_cover.bmp"
      Dir.removeFile $ temp </> "temp_extra.dta"
    let label = D.name pkg <> " (" <> D.artist pkg <> ")"
    rb3pkg label label temp con

importRBA :: (SendMessage m, MonadResource m) => FilePath -> Maybe FilePath -> FilePath -> StackTraceT m Kicks
importRBA file file2x dir = tempDir "onyx_rba" $ \temp -> do
  lg $ "Importing RBA file from: " ++ file
  forM_ file2x $ \f2x -> lg $ "Plus 2x Bass Pedal from: " ++ f2x
  getRBAFile 0 file $ temp </> "songs.dta"
  getRBAFile 1 file $ temp </> "notes.mid"
  getRBAFile 2 file $ temp </> "audio.mogg"
  getRBAFile 4 file $ temp </> "cover.bmp"
  getRBAFile 6 file $ temp </> "extra.dta"
  (_, pkg, isUTF8) <- readRB3DTA $ temp </> "songs.dta"
  mmilo <- if dtaIsRB3 pkg
    then do
      let milo = temp </> "lipsync.milo_xbox"
      getRBAFile 3 file milo
      return $ Just milo
    else return Nothing
  extra <- (if isUTF8 then D.readFileDTA_utf8' else D.readFileDTA_latin1') $ temp </> "extra.dta"
  let author = case extra of
        D.DTA _ (D.Tree _ [D.Parens (D.Tree _
          ( D.String "backend"
          : D.Parens (D.Tree _ [D.Sym "author", D.String s])
          : _
          ))])
          -> Just s
        _ -> Nothing
      (title, is2x) = determine2xBass $ D.name pkg
      -- TODO: import more stuff from the extra dta
      meta = def'
        { _author = author
        , _title = Just title
        }
  files2x <- forM file2x $ \f2x -> do
    let mid2x = temp </> "notes-2x.mid"
        dta2x = temp </> "songs-2x.dta"
    getRBAFile 0 f2x dta2x
    getRBAFile 1 f2x mid2x
    (_, pkg2x, _) <- readRB3DTA dta2x
    return (pkg2x, mid2x)
  let hasKicks = if isJust file2x then KicksBoth else if is2x then Kicks2x else Kicks1x
  importRB3 pkg meta False True hasKicks
    (temp </> "notes.mid") Nothing files2x (temp </> "audio.mogg")
    (Just (temp </> "cover.bmp", "cover.bmp")) mmilo dir
  return hasKicks

-- | Collects the contents of an RBA or CON file into an Onyx project.
importRB3 :: (SendMessage m, MonadResource m) => D.SongPackage -> Metadata f -> Bool -> Bool -> Kicks -> FilePath -> Maybe FilePath -> Maybe (D.SongPackage, FilePath) -> FilePath -> Maybe (FilePath, FilePath) -> Maybe FilePath -> FilePath -> StackTraceT m ()
importRB3 pkg meta karaoke multitrack hasKicks mid updateMid files2x mogg mcover mmilo dir = do
  stackIO $ Dir.copyFile mogg $ dir </> "audio.mogg"
  localMilo <- do
    -- if rbn2 and no vox, don't import milo
    if dtaIsHarmonixRB3 pkg || maybe False (/= 0) (HM.lookup "vocals" $ D.rank pkg)
      then forM mmilo $ \milo -> do
        let local = if dtaIsHarmonixRB3 pkg then "lipsync-venue.milo_xbox" else "lipsync.milo_xbox"
        stackIO $ Dir.copyFile milo $ dir </> local
        return local
      else return Nothing

  RBFile.Song temps sigs (RBFile.RawFile trks1x) <- RBFile.loadMIDI mid
  trksUpdate <- case updateMid of
    Nothing -> return []
    Just umid -> stackIO (Dir.doesFileExist umid) >>= \case
      True -> RBFile.rawTracks . RBFile.s_tracks <$> RBFile.loadMIDI umid
      False -> do
        warn $ "Expected to find disc update MIDI but it's not installed: " <> umid
        return []
  let updatedNames = map Just $ mapMaybe U.trackName trksUpdate
      trksUpdated
        = filter ((`notElem` updatedNames) . U.trackName) trks1x
        ++ trksUpdate
  trksAdd2x <- case files2x of
    Nothing -> return trksUpdated
    Just (_pkg2x, mid2x) -> do
      RBFile.Song _ _ (RBFile.RawFile trks2x) <- RBFile.loadMIDI mid2x
      drums1x <- RBFile.parseTracks trksUpdated "PART DRUMS"
      drums2x <- RBFile.parseTracks trks2x      "PART DRUMS"
      let notDrums = filter ((/= Just "PART DRUMS") . U.trackName) trksUpdated
      case combine1x2x drums1x drums2x of
        Left (pos, thing) -> do
          warn $ unwords
            [ "Using C3 format for 1x+2x drums."
            , "Couldn't condense to PS format due to difference in"
            , thing
            , "at"
            , RBFile.showPosition sigs pos
            ]
          let make2xTrack trk = case U.trackName trk of
                Just "PART DRUMS" -> Just $ U.setTrackName "PART DRUMS_2X" trk
                _                 -> Nothing
          return $ trksUpdated ++ mapMaybe make2xTrack trks2x
        Right combinedDrums -> return $ notDrums ++ do
          RBFile.s_tracks $ RBFile.showMIDITracks $ RBFile.Song temps sigs $ mempty
            { RBFile.fixedPartDrums = combinedDrums
            }
  stackIO $ Save.toFile (dir </> "notes.mid") $ RBFile.showMIDIFile'
    $ RBFile.Song temps sigs $ RBFile.RawFile trksAdd2x

  coverName <- case mcover of
    Nothing -> return Nothing
    Just (cover, coverName) -> stackIO (Dir.doesFileExist cover) >>= \case
      True -> errorToWarning $ do
        -- errorToWarning shouldn't be needed, but just in case
        stackIO $ Dir.copyFile cover $ dir </> coverName
        return coverName
      False -> do
        warn $ "Couldn't load album art: " <> cover
        return Nothing
  md5 <- stackIO $ show . MD5.md5 <$> BL.readFile (dir </> "audio.mogg")
  drumkit <- case D.drumBank pkg of
    Nothing -> return HardRockKit
    Just x -> case x of
      "sfx/kit01_bank.milo" -> return HardRockKit
      "sfx/kit02_bank.milo" -> return ArenaKit
      "sfx/kit03_bank.milo" -> return VintageKit
      "sfx/kit04_bank.milo" -> return TrashyKit
      "sfx/kit05_bank.milo" -> return ElectronicKit
      s -> do
        warn $ "Unrecognized drum bank " ++ show s
        return HardRockKit
  let diffMap :: HM.HashMap T.Text Config.Difficulty
      diffMap = let
        -- We assume that if every rank value is a tier boundary,
        -- it's a Magma-produced song where the author selected tiers.
        -- So we should import to tiers, not ranks.
        isTierBoundary (k, v) = case k of
          "drum"        -> (k,) <$> elemIndex v (0 : 1 : drumsDiffMap)
          "guitar"      -> (k,) <$> elemIndex v (0 : 1 : guitarDiffMap)
          "bass"        -> (k,) <$> elemIndex v (0 : 1 : bassDiffMap)
          "vocals"      -> (k,) <$> elemIndex v (0 : 1 : vocalDiffMap)
          "keys"        -> (k,) <$> elemIndex v (0 : 1 : keysDiffMap)
          "real_keys"   -> (k,) <$> elemIndex v (0 : 1 : keysDiffMap)
          "real_guitar" -> (k,) <$> elemIndex v (0 : 1 : proGuitarDiffMap)
          "real_bass"   -> (k,) <$> elemIndex v (0 : 1 : proBassDiffMap)
          "band"        -> (k,) <$> elemIndex v (0 : 1 : bandDiffMap)
          _             -> Nothing
        in case mapM isTierBoundary $ HM.toList $ D.rank pkg of
          Nothing    -> Rank                <$> D.rank pkg
          Just tiers -> Tier . fromIntegral <$> HM.fromList tiers
      hasRankStr s = maybe False (/= 0) $ HM.lookup s $ D.rank pkg
  vocalMode <- if hasRankStr "vocals"
    then case D.vocalParts $ D.song pkg of
      Nothing -> return $ Just Vocal1
      Just 0  -> return Nothing
      Just 1  -> return $ Just Vocal1
      Just 2  -> return $ Just Vocal2
      Just 3  -> return $ Just Vocal3
      n       -> fatal $ "Invalid vocal count of " ++ show n
    else return Nothing
  let hopoThresh = fromIntegral $ fromMaybe 170 $ D.hopoThreshold $ D.song pkg

  fixedTracks <- RBFile.s_tracks <$> RBFile.loadMIDI mid

  let drumEvents = RBFile.fixedPartDrums fixedTracks
  foundMix <- let
    drumMixes = do
      (_, dd) <- Map.toList $ drumDifficulties drumEvents
      (aud, _dsc) <- toList $ drumMix dd
      return aud
    in case drumMixes of
      [] -> return Nothing
      aud : auds -> if all (== aud) auds
        then return $ Just aud
        else do
          warn $ "Inconsistent drum mixes: " ++ show (nub drumMixes)
          return Nothing
  let instChans :: [(T.Text, [Int])]
      instChans = map (second $ map fromIntegral) $ D.fromDictList $ D.tracks $ D.song pkg
      drumChans = fromMaybe [] $ lookup "drum" instChans
  drumSplit <- if not $ hasRankStr "drum" then return Nothing else case foundMix of
    Nothing -> case drumChans of
      -- No drum mix seen in The Kill (30STM), has 5 drum channels
      [kitL, kitR] -> return $ Just $ PartSingle [kitL, kitR]
      [kick, snare, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) (Just [snare]) [kitL, kitR]
      [kick, snareL, snareR, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) (Just [snareL, snareR]) [kitL, kitR]
      [kickL, kickR, snareL, snareR, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kickL, kickR]) (Just [snareL, snareR]) [kitL, kitR]
      [kick, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) Nothing [kitL, kitR]
      _ -> do
        warn $ unwords
          [ "No drum mix (or inconsistent) and there are"
          , show $ length drumChans
          , "drum channels (expected 2-6)"
          ]
        return $ Just $ PartSingle drumChans
    Just RBDrums.D0 -> case drumChans of
      [kitL, kitR] -> return $ Just $ PartSingle [kitL, kitR]
      _ -> fatal $ "mix 0 needs 2 drums channels, " ++ show (length drumChans) ++ " given"
    Just RBDrums.D1 -> case drumChans of
      [kick, snare, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) (Just [snare]) [kitL, kitR]
      _ -> fatal $ "mix 1 needs 4 drums channels, " ++ show (length drumChans) ++ " given"
    Just RBDrums.D2 -> case drumChans of
      [kick, snareL, snareR, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) (Just [snareL, snareR]) [kitL, kitR]
      _ -> fatal $ "mix 2 needs 5 drums channels, " ++ show (length drumChans) ++ " given"
    Just RBDrums.D3 -> case drumChans of
      [kickL, kickR, snareL, snareR, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kickL, kickR]) (Just [snareL, snareR]) [kitL, kitR]
      _ -> fatal $ "mix 3 needs 6 drums channels, " ++ show (length drumChans) ++ " given"
    Just RBDrums.D4 -> case drumChans of
      [kick, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) Nothing [kitL, kitR]
      _ -> fatal $ "mix 4 needs 3 drums channels, " ++ show (length drumChans) ++ " given"

  let tone = fromMaybe Minor $ D.songTonality pkg
      -- Minor verified as default for PG chords if SK/VTN present and no song_tonality
      (skey, vkey) = case (D.songKey pkg, D.vocalTonicNote pkg) of
        (Just sk, Just vtn) -> (Just $ SongKey sk tone , Just vtn)
        (Just sk, Nothing ) -> (Just $ SongKey sk tone , Nothing )
        (Nothing, Just vtn) -> (Just $ SongKey vtn tone, Nothing )
        (Nothing, Nothing ) -> (Nothing                , Nothing )

  let bassBase = detectExtProBass fixedTracks

  stackIO $ yamlEncodeFile (dir </> "song.yml") $ toJSON SongYaml
    { _metadata = Metadata
      { _title        = _title meta <|> Just (D.name pkg)
      , _artist       = Just $ D.artist pkg
      , _album        = D.albumName pkg
      , _genre        = Just $ D.genre pkg
      , _subgenre     = D.subGenre pkg >>= T.stripPrefix "subgenre_"
      , _year         = Just $ fromIntegral $ D.yearReleased pkg
      , _fileAlbumArt = coverName
      , _trackNumber  = fromIntegral <$> D.albumTrackNumber pkg
      , _comments     = []
      , _difficulty   = fromMaybe (Tier 1) $ HM.lookup "band" diffMap
      , _key          = skey
      , _author       = _author meta
      , _rating       = toEnum $ fromIntegral $ D.rating pkg - 1
      , _previewStart = Just $ PreviewSeconds $ fromIntegral (fst $ D.preview pkg) / 1000
      , _previewEnd   = Just $ PreviewSeconds $ fromIntegral (snd $ D.preview pkg) / 1000
      , _languages    = _languages meta
      , _convert      = _convert meta
      , _rhythmKeys   = _rhythmKeys meta
      , _rhythmBass   = _rhythmBass meta
      , _catEMH       = _catEMH meta
      , _expertOnly   = _expertOnly meta
      , _cover        = not $ D.master pkg
      }
    , _global = def
      { _animTempo = D.animTempo pkg
      }
    , _audio = HM.empty
    , _jammit = HM.empty
    , _plans = HM.singleton "mogg" MoggPlan
      { _moggMD5 = T.pack md5
      , _moggParts = Parts $ HM.fromList $ concat
        [ [ (FlexGuitar, PartSingle ns) | ns <- toList $ lookup "guitar" instChans ]
        , [ (FlexBass  , PartSingle ns) | ns <- toList $ lookup "bass"   instChans ]
        , [ (FlexKeys  , PartSingle ns) | ns <- toList $ lookup "keys"   instChans ]
        , [ (FlexVocal , PartSingle ns) | ns <- toList $ lookup "vocals" instChans ]
        , [ (FlexDrums , ds           ) | Just ds <- [drumSplit] ]
        ]
      , _moggCrowd = maybe [] (map fromIntegral) $ D.crowdChannels $ D.song pkg
      , _pans = map realToFrac $ D.pans $ D.song pkg
      , _vols = map realToFrac $ D.vols $ D.song pkg
      , _planComments = []
      , _tuningCents = maybe 0 round $ D.tuningOffsetCents pkg
      , _fileTempo = Nothing
      , _karaoke = karaoke
      , _multitrack = multitrack
      }
    , _targets = let
      getSongID = \case
        Left  i -> guard (i /= 0) >> Just (Left i)
        Right k -> Just $ Right k
      songID1x = D.songId pkg >>= getSongID
      songID2x = if hasKicks == Kicks2x
        then songID1x
        else files2x >>= D.songId . fst >>= getSongID
      version1x = songID1x >> Just (D.version pkg)
      version2x = songID2x >> fmap (D.version . fst) files2x
      targetShared = def'
        { rb3_Harmonix = dtaIsHarmonixRB3 pkg
        , rb3_FileMilo = localMilo
        }
      target1x = ("rb3", RB3 targetShared
        { rb3_2xBassPedal = False
        , rb3_SongID = songID1x
        , rb3_Version = version1x
        })
      target2x = ("rb3-2x", RB3 targetShared
        { rb3_2xBassPedal = True
        , rb3_SongID = songID2x
        , rb3_Version = version2x
        })
      in HM.fromList $ concat [[target1x | hasKicks /= Kicks2x], [target2x | hasKicks /= Kicks1x]]
    , _parts = Parts $ HM.fromList
      [ ( FlexDrums, def
        { partDrums = guard (hasRankStr "drum") >> Just PartDrums
          { drumsDifficulty = fromMaybe (Tier 1) $ HM.lookup "drum" diffMap
          , drumsMode = DrumsPro
          , drumsKicks = hasKicks
          , drumsFixFreeform = False
          , drumsKit = drumkit
          , drumsLayout = StandardLayout -- TODO import this
          , drumsFallback = FallbackGreen
          }
        })
      , ( FlexGuitar, def
        { partGRYBO = guard (hasRankStr "guitar") >> Just PartGRYBO
          { gryboDifficulty = fromMaybe (Tier 1) $ HM.lookup "guitar" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProGuitar = guard (hasRankStr "real_guitar") >> Just PartProGuitar
          { pgDifficulty = fromMaybe (Tier 1) $ HM.lookup "real_guitar" diffMap
          , pgHopoThreshold = hopoThresh
          , pgTuning = GtrTuning
            { gtrBase = Guitar6
            , gtrOffsets = fromMaybe [] $ map fromIntegral <$> D.realGuitarTuning pkg
            , gtrGlobal = 0
            }
          , pgFixFreeform = False
          }
        })
      , ( FlexBass, def
        { partGRYBO = guard (hasRankStr "bass") >> Just PartGRYBO
          { gryboDifficulty = fromMaybe (Tier 1) $ HM.lookup "bass" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProGuitar = guard (hasRankStr "real_bass") >> Just PartProGuitar
          { pgDifficulty = fromMaybe (Tier 1) $ HM.lookup "real_bass" diffMap
          , pgHopoThreshold = hopoThresh
          , pgTuning = GtrTuning
            { gtrBase = bassBase
            , gtrOffsets = fromMaybe [] $ map fromIntegral <$> D.realBassTuning pkg
            , gtrGlobal = 0
            }
          , pgFixFreeform = False
          }
        })
      , ( FlexKeys, def
        { partGRYBO = guard (hasRankStr "keys") >> Just PartGRYBO
          { gryboDifficulty = fromMaybe (Tier 1) $ HM.lookup "keys" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProKeys = guard (hasRankStr "real_keys") >> Just PartProKeys
          { pkDifficulty = fromMaybe (Tier 1) $ HM.lookup "real_keys" diffMap
          , pkFixFreeform = False
          }
        })
      , ( FlexVocal, def'
        { partVocal = flip fmap vocalMode $ \vc -> PartVocal
          { vocalDifficulty = fromMaybe (Tier 1) $ HM.lookup "vocals" diffMap
          , vocalCount = vc
          , vocalGender = D.vocalGender pkg
          , vocalKey = vkey
          -- TODO actually extract lipsync
          , vocalLipsyncRB3 = Nothing
          , vocalLipsyncRB2 = Nothing
          }
        })
      ]
    }

detectExtProBass :: RBFile.FixedFile t -> GtrBase
detectExtProBass trks = let
  strs = do
    trk <- [RBFile.fixedPartRealBass trks, RBFile.fixedPartRealBass22 trks]
    diff <- toList $ PG.pgDifficulties trk
    (str, _) <- toList (PG.pgNotes diff) >>= toList
    return str
  in if elem PG.S1 strs
    then PG.GtrCustom [28, 33, 38, 43, 47, 52] -- bass with 2 high gtr strings
    else if elem PG.S2 strs
      then PG.GtrCustom [28, 33, 38, 43, 47] -- bass with 1 high gtr string
      else Bass4

importMagma :: (SendMessage m, MonadIO m) => FilePath -> FilePath -> StackTraceT m Kicks
importMagma fin dir = do
  lg $ "Importing Magma project from: " <> fin

  let oldDir = takeDirectory fin
      locate f = fixFileCase $ oldDir </> f
  RBProj.RBProj rbproj <- stackIO (D.readFileDTA fin) >>= D.unserialize D.stackChunks

  let mid = T.unpack (RBProj.midiFile $ RBProj.midi rbproj)
  stackIO $ locate mid >>= \m -> Dir.copyFile m (dir </> "notes.mid")
  bassBase <- detectExtProBass . RBFile.s_tracks
    <$> RBFile.loadMIDI (dir </> "notes.mid")

  c3 <- do
    pathC3 <- fixFileCase $ fin -<.> "c3"
    hasC3 <- stackIO $ Dir.doesFileExist pathC3
    if hasC3
      then fmap Just $ stackIO (decodeGeneral <$> B8.readFile pathC3) >>= C3.readC3
      else return Nothing

  let art = fromMaybe (T.unpack $ RBProj.albumArtFile $ RBProj.albumArt rbproj)
        $ C3.songAlbumArt <$> c3
      art' = "album" <.> map toLower (takeExtension art)
  stackIO $ locate art >>= \a -> Dir.copyFile a (dir </> art')

  let hopoThresh = case fmap C3.hopoThresholdIndex c3 of
        Nothing -> 170
        Just 0  -> 90
        Just 1  -> 130
        Just 2  -> 170
        Just 3  -> 250
        Just _  -> 170

  -- TODO detect silent audio files and don't import them
  let getTrack s f = let
        aud = f $ RBProj.tracks rbproj
        in if RBProj.audioEnabled aud
          then do
            src <- locate $ T.unpack $ RBProj.audioFile aud
            let dst = s -<.> map toLower (takeExtension src)
            stackIO $ Dir.copyFile src (dir </> dst)
            return $ Just
              ( PlanAudio
                { _planExpr = Input $ Named $ T.pack s
                , _planPans = map realToFrac $ RBProj.pan aud
                , _planVols = map realToFrac $ RBProj.vol aud
                }
              , ( T.pack s
                , AudioFile AudioInfo
                  { _md5 = Nothing
                  , _frames = Nothing
                  , _filePath = Just dst
                  , _commands = []
                  , _rate = Nothing
                  , _channels = fromIntegral $ RBProj.channels aud
                  }
                )
              )
          else return Nothing
  drums <- getTrack "drums" RBProj.drumKit
  kick <- getTrack "kick" RBProj.drumKick
  snare <- getTrack "snare" RBProj.drumSnare
  gtr <- getTrack "guitar" RBProj.guitar
  bass <- getTrack "bass" RBProj.bass
  keys <- getTrack "keys" RBProj.keys
  vox <- getTrack "vocal" RBProj.vocals
  song <- getTrack "song" RBProj.backing
  crowd <- case c3 >>= C3.crowdAudio of
    Nothing -> return Nothing
    Just f  -> do
      src <- locate $ T.unpack f
      let s = "crowd"
          dst = s -<.> map toLower (takeExtension src)
      stackIO $ Dir.copyFile src (dir </> dst)
      chans <- audioChannels src >>= \case
        Just c -> return c
        Nothing -> do
          warn "Couldn't detect crowd audio channels; assuming 2."
          return 2
      return $ Just
        ( PlanAudio
          { _planExpr = Input $ Named $ T.pack s
          , _planPans = []
          , _planVols = toList $ c3 >>= C3.crowdVol
          }
        , ( T.pack s
          , AudioFile AudioInfo
            { _md5 = Nothing
            , _frames = Nothing
            , _filePath = Just dst
            , _commands = []
            , _rate = Nothing
            , _channels = chans
            }
          )
        )
  let allAudio = map snd $ catMaybes [drums, kick, snare, gtr, bass, keys, vox, song, crowd]

  let (title, is2x) = case c3 of
        Nothing     -> determine2xBass $ RBProj.songName $ RBProj.metadata rbproj
        Just c3file -> (C3.song c3file, C3.is2xBass c3file)
      -- TODO support dual 1x+2x projects
      targetName = if is2x then "rb3-2x" else "rb3"
      target = def
        { rb3_2xBassPedal = is2x
        , rb3_SongID = c3 >>= \c3file -> if C3.useNumericID c3file
          then fmap Left $ readMaybe $ T.unpack (C3.uniqueNumericID c3file)
          else case C3.customID c3file of "" -> Nothing; cid -> Just $ Right cid
        , rb3_Version = fromIntegral . C3.version <$> c3
        }

  let readTuning c3fn k = case c3 >>= c3fn of
        Nothing -> return Nothing
        Just tune -> errorToWarning (scanStack tune >>= parseStack) >>= \case
          Just (D.DTA _ (D.Tree _ [D.Parens (D.Tree _ [D.Sym k', D.Parens (D.Tree _ mints)])])) | k == k' ->
            case mapM (\case D.Int i -> Just $ fromIntegral i; _ -> Nothing) mints of
              Just ints -> return $ Just ints
              Nothing   -> warn "Non-integer value in tuning" >> return Nothing
          _ -> warn "Couldn't read DTA-snippet tuning format" >> return Nothing
  tuneGtr <- inside "Reading pro guitar tuning" $ readTuning C3.proGuitarTuning "real_guitar_tuning"
  tuneBass <- inside "Reading pro bass tuning" $ readTuning C3.proBassTuning4 "real_bass_tuning"

  stackIO $ yamlEncodeFile (dir </> "song.yml") $ toJSON SongYaml
    { _metadata = Metadata
      { _title        = Just title
      , _artist       = Just $ maybe (RBProj.artistName $ RBProj.metadata rbproj) C3.artist c3
      , _album        = Just $ maybe (RBProj.albumName $ RBProj.metadata rbproj) C3.album c3
      , _genre        = Just $ RBProj.genre $ RBProj.metadata rbproj
      , _subgenre     = Just $ RBProj.subGenre $ RBProj.metadata rbproj
      , _year         = Just $ fromIntegral $ RBProj.yearReleased $ RBProj.metadata rbproj
      , _fileAlbumArt = Just art'
      , _trackNumber  = Just $ fromIntegral $ RBProj.trackNumber $ RBProj.metadata rbproj
      , _comments     = []
      , _difficulty   = Tier $ RBProj.rankBand $ RBProj.gamedata rbproj
      , _key          = fmap (`SongKey` Major) $ c3 >>= C3.tonicNote
      , _author       = Just $ RBProj.author $ RBProj.metadata rbproj
      , _rating       = case fmap C3.songRating c3 of
        Nothing -> Unrated
        Just 1  -> FamilyFriendly
        Just 2  -> SupervisionRecommended
        Just 3  -> Mature
        Just 4  -> Unrated
        Just _  -> Unrated
      , _previewStart = Just $ PreviewSeconds $ fromIntegral (RBProj.previewStartMs $ RBProj.gamedata rbproj) / 1000
      , _previewEnd   = Nothing
      , _languages    = let
        lang s f = [s | fromMaybe False $ f $ RBProj.languages rbproj]
        in concat
          [ lang "English"  RBProj.english
          , lang "French"   RBProj.french
          , lang "Italian"  RBProj.italian
          , lang "Spanish"  RBProj.spanish
          , lang "German"   RBProj.german
          , lang "Japanese" RBProj.japanese
          ]
      , _convert      = maybe False C3.convert c3
      , _rhythmKeys   = maybe False C3.rhythmKeys c3
      , _rhythmBass   = maybe False C3.rhythmBass c3
      , _catEMH       = False -- not stored in .c3 file
      , _expertOnly   = maybe False C3.expertOnly c3
      , _cover        = maybe False (not . C3.isMaster) c3
      }
    , _global = Global
      { _fileMidi = "notes.mid"
      , _fileSongAnim = Nothing
      , _autogenTheme = case RBProj.autogenTheme $ RBProj.midi rbproj of
        Left theme -> theme
        Right _str -> RBProj.DefaultTheme -- TODO
      , _animTempo    = Right $ RBProj.animTempo $ RBProj.gamedata rbproj
      }
    , _audio = HM.fromList allAudio
    , _jammit = HM.empty
    , _plans = HM.singleton "rbproj" Plan
      { _song = fmap fst song
      , _countin = Countin []
      , _planParts = Parts $ HM.fromList $ concat
        [ case drums of
          Nothing -> []
          Just (drumsAud, _) ->
            [(FlexDrums, case (kick, snare) of
              (Nothing, Nothing) -> PartSingle drumsAud
              _ -> PartDrumKit (fmap fst kick) (fmap fst snare) drumsAud
            )]
        , toList $ fmap (\(aud, _) -> (FlexGuitar, PartSingle aud)) gtr
        , toList $ fmap (\(aud, _) -> (FlexBass  , PartSingle aud)) bass
        , toList $ fmap (\(aud, _) -> (FlexKeys  , PartSingle aud)) keys
        , toList $ fmap (\(aud, _) -> (FlexVocal , PartSingle aud)) vox
        ]
      , _crowd = fmap fst crowd
      , _planComments = []
      , _tuningCents = maybe 0 C3.tuningCents c3 -- TODO use this, or Magma.tuningOffsetCents?
      , _fileTempo = Nothing
      }
    , _targets = HM.singleton targetName $ RB3 target
    , _parts = Parts $ HM.fromList
      [ ( FlexDrums, def
        { partDrums = guard (isJust drums) >> Just PartDrums
          { drumsDifficulty = Tier $ RBProj.rankDrum $ RBProj.gamedata rbproj
          , drumsMode = DrumsPro -- TODO set to Drums4 for magma v1?
          , drumsKicks = if is2x then Kicks2x else Kicks1x
          , drumsFixFreeform = False
          , drumsKit = case fmap C3.drumKitSFX c3 of
            Nothing -> HardRockKit
            Just 0  -> HardRockKit
            Just 1  -> ArenaKit
            Just 2  -> VintageKit
            Just 3  -> TrashyKit
            Just 4  -> ElectronicKit
            Just _  -> HardRockKit
          , drumsLayout = StandardLayout
          , drumsFallback = FallbackGreen
          }
        })
      , ( FlexGuitar, def
        { partGRYBO = guard (isJust gtr) >> Just PartGRYBO
          { gryboDifficulty = Tier $ RBProj.rankGuitar $ RBProj.gamedata rbproj
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProGuitar = do
          diff <- guard (isJust gtr) >> c3 >>= C3.proGuitarDiff
          Just PartProGuitar
            { pgDifficulty = Tier $ rankToTier proGuitarDiffMap $ fromIntegral diff
            , pgHopoThreshold = hopoThresh
            , pgTuning = GtrTuning
              { gtrBase = Guitar6
              , gtrOffsets = fromMaybe [] tuneGtr
              , gtrGlobal = 0
              }
            , pgFixFreeform = False
            }
        })
      , ( FlexBass, def
        { partGRYBO = guard (isJust bass) >> Just PartGRYBO
          { gryboDifficulty = Tier $ RBProj.rankBass $ RBProj.gamedata rbproj
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProGuitar = do
          diff <- guard (isJust gtr) >> c3 >>= C3.proBassDiff
          Just PartProGuitar
            { pgDifficulty = Tier $ rankToTier proBassDiffMap $ fromIntegral diff
            , pgHopoThreshold = hopoThresh
            , pgTuning = GtrTuning
              { gtrBase = bassBase
              , gtrOffsets = fromMaybe [] tuneBass
              , gtrGlobal = 0
              }
            , pgFixFreeform = False
            }
        })
      , ( FlexKeys, def
        { partGRYBO = guard (isJust keys) >> Just PartGRYBO
          { gryboDifficulty = Tier $ RBProj.rankKeys $ RBProj.gamedata rbproj
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProKeys = guard (isJust keys && maybe False (not . C3.disableProKeys) c3) >> Just PartProKeys
          { pkDifficulty = Tier $ RBProj.rankProKeys $ RBProj.gamedata rbproj
          , pkFixFreeform = False
          }
        })
      , ( FlexVocal, def'
        { partVocal = guard (isJust vox) >> Just PartVocal
          { vocalDifficulty = Tier $ RBProj.rankVocals $ RBProj.gamedata rbproj
          , vocalCount = if
            | RBProj.dryVoxEnabled $ RBProj.part2 $ RBProj.dryVox rbproj -> Vocal3
            | RBProj.dryVoxEnabled $ RBProj.part1 $ RBProj.dryVox rbproj -> Vocal2
            | otherwise                                                  -> Vocal1
          , vocalGender = Just $ RBProj.vocalGender $ RBProj.gamedata rbproj
          , vocalKey = Nothing
          , vocalLipsyncRB3 = Nothing
          , vocalLipsyncRB2 = Nothing
          }
        })
      ]
    }

  return $ if is2x then Kicks2x else Kicks1x

bothFirstSecond :: (NNC.C t, Ord a) => RTB.T t a -> RTB.T t a -> (RTB.T t a, RTB.T t a, RTB.T t a)
bothFirstSecond t1 t2 = let
  result = fmap eachInstant $ RTB.collectCoincident $ RTB.merge (fmap Left t1) (fmap Right t2)
  eachInstant es = let
    xs = Set.fromList $ lefts es
    ys = Set.fromList $ rights es
    in  ( Set.toList $ Set.intersection xs ys
        , Set.toList $ Set.difference xs ys
        , Set.toList $ Set.difference ys xs
        )
  in
    ( RTB.flatten $ fmap fst3 result
    , RTB.flatten $ fmap snd3 result
    , RTB.flatten $ fmap thd3 result
    )

firstDifference :: (NNC.C t, Ord a) => RTB.T t a -> RTB.T t a -> Maybe t
firstDifference rtb1 rtb2 = let
  go !posn n1 n2 = case (RTB.viewL n1, RTB.viewL n2) of
    (Nothing, Nothing) -> Nothing
    (Just ((t1, x1), rtb1'), Just ((t2, x2), rtb2'))
      | t1 /= t2  -> Just $ posn <> min t1 t2
      | x1 /= x2  -> Just $ posn <> t1
      | otherwise -> go (posn <> t1) rtb1' rtb2'
    (Just ((t1, _), _), Nothing) -> Just $ posn <> t1
    (Nothing, Just ((t2, _), _)) -> Just $ posn <> t2
  in go NNC.zero (RTB.normalize rtb1) (RTB.normalize rtb2)

combine1x2x
  :: DrumTrack U.Beats
  -> DrumTrack U.Beats
  -> Either (U.Beats, String) (DrumTrack U.Beats)
combine1x2x dt1 dt2 = do
  let stopIfDifferent str x y = case firstDifference x y of
        Nothing -> Right ()
        Just t  -> Left (t, str)
      stopUnlessEmpty str x = case RTB.viewL x of
        Nothing          -> Right ()
        Just ((t, _), _) -> Left (t, str)
  stopIfDifferent "mood" (drumMood dt1) (drumMood dt2)
  stopIfDifferent "toms" (drumToms dt1) (drumToms dt2)
  stopIfDifferent "single roll" (drumSingleRoll dt1) (drumSingleRoll dt2)
  stopIfDifferent "double roll" (drumDoubleRoll dt1) (drumDoubleRoll dt2)
  stopIfDifferent "overdrive" (drumOverdrive dt1) (drumOverdrive dt2)
  stopIfDifferent "activation" (drumActivation dt1) (drumActivation dt2)
  stopIfDifferent "solo" (drumSolo dt1) (drumSolo dt2)
  stopIfDifferent "player 1 phrase" (drumPlayer1 dt1) (drumPlayer1 dt2)
  stopIfDifferent "player 2 phrase" (drumPlayer2 dt1) (drumPlayer2 dt2)
  stopIfDifferent "animation" (drumAnimation dt1) (drumAnimation dt2)
  stopUnlessEmpty "pitch 95 in 1x track" $ drumKick2x dt1
  stopUnlessEmpty "pitch 95 in 2x track" $ drumKick2x dt2
  results <- forM [Easy, Medium, Hard, Expert] $ \diff -> do
    let dd1 = fromMaybe mempty $ Map.lookup diff $ drumDifficulties dt1
        dd2 = fromMaybe mempty $ Map.lookup diff $ drumDifficulties dt2
    stopIfDifferent (show diff ++ " mix events") (drumMix dd1) (drumMix dd2)
    stopIfDifferent (show diff ++ " Phase Shift modifiers") (drumPSModifiers dd1) (drumPSModifiers dd2)
    case diff of
      Expert -> case bothFirstSecond (drumGems dd1) (drumGems dd2) of
        (_both, only1x, only2x) -> do
          stopUnlessEmpty "Expert notes" only1x
          stopUnlessEmpty "Expert notes" $ RTB.filter (/= Kick) only2x
          Right $ Just $ void only2x
      _ -> do
        stopIfDifferent (show diff ++ " notes") (drumGems dd1) (drumGems dd2)
        Right Nothing
  Right dt1 { drumKick2x = foldr RTB.merge RTB.empty $ catMaybes results }

importAmplitude :: (SendMessage m, MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
importAmplitude fin dout = do
  song <- stackIO (D.readFileDTA fin) >>= D.unserialize D.stackChunks
  let moggPath = takeDirectory fin </> T.unpack (Amp.mogg_path song)
      midPath  = takeDirectory fin </> T.unpack (Amp.midi_path song)
      previewStart = realToFrac (Amp.preview_start_ms song) / 1000
      previewEnd = previewStart + realToFrac (Amp.preview_length_ms song) / 1000
  md5 <- stackIO $ BL.readFile moggPath >>= evaluate . MD5.md5
  RBFile.Song temps sigs amp <- RBFile.loadMIDI midPath
  let getChannels n = case Amp.tracks song !! (n - 1) of
        (_, (chans, _)) -> map fromIntegral chans
      freestyle = do
        (_, (ns, event)) <- Amp.tracks song
        guard $ "event:/FREESTYLE" `T.isPrefixOf` event
        map fromIntegral ns
      parts = do
        (n, Amp.Catch inst name trk) <- Map.toList $ Amp.ampTracks amp
        return (FlexExtra name, getChannels n, inst, trk)
  stackIO $ Dir.createDirectoryIfMissing False dout
  stackIO $ Save.toFile (dout </> "notes.mid") $ RBFile.showMIDIFile'
    $ RBFile.Song temps sigs mempty
      { RBFile.onyxParts = Map.fromList $ do
        (name, _, _, trk) <- parts
        return (name, mempty { RBFile.onyxCatch = trk })
      }
  stackIO $ Dir.copyFile moggPath $ dout </> "audio.mogg"
  stackIO $ yamlEncodeFile (dout </> "song.yml") $ toJSON SongYaml
    { _metadata = def'
      { _title        = Just $ Amp.title song
      , _artist       = Just $ case Amp.artist_short song of
        "Harmonix" -> Amp.artist song -- human love
        artist     -> artist
      , _previewStart = Just $ PreviewSeconds previewStart
      , _previewEnd   = Just $ PreviewSeconds previewEnd
      }
    , _global = def'
    , _audio = HM.empty
    , _jammit = HM.empty
    , _plans = HM.singleton "mogg" MoggPlan
      { _moggMD5 = T.pack $ show md5
      , _moggParts = Parts $ HM.fromList $ do
        (name, chans, _, _) <- parts
        return (name, PartSingle chans)
      , _moggCrowd = freestyle -- so it's hidden from web player
      , _pans = map realToFrac $ Amp.pans song
      , _vols = map realToFrac $ Amp.vols song
      , _planComments = []
      , _tuningCents = 0
      , _fileTempo = Nothing
      , _karaoke = False
      , _multitrack = True
      }
    , _targets = HM.empty
    , _parts = Parts $ HM.fromList $ do
      (name, _, inst, _) <- parts
      return (name, def' { partAmplitude = Just (PartAmplitude inst) })
    }

def' :: (Default (f FilePath)) => f FilePath
def' = def
