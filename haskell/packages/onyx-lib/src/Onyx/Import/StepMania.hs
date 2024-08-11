{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.StepMania where

import           Control.Applicative                  ((<|>))
import           Control.Monad.Extra                  (firstJustM, guard, when)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Char                            (isAlphaNum, toLower)
import qualified Data.Conduit.Audio                   as CA
import qualified Data.EventList.Relative.TimeBody     as RTB
import           Data.Foldable                        (toList)
import qualified Data.HashMap.Strict                  as HM
import           Data.List.Extra                      (nubOrd, sortOn)
import qualified Data.List.NonEmpty                   as NE
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       mapMaybe)
import qualified Data.Text                            as T
import           Onyx.Audio                           (Audio (..), Edge (..),
                                                       audioChannels)
import           Onyx.DDR.DWI
import           Onyx.DDR.SM
import           Onyx.Harmonix.DTA.Serialize.RockBand (AnimTempo (..))
import           Onyx.Import.Base
import           Onyx.MIDI.Common                     (blipEdgesRBNice,
                                                       pattern ANil, pattern At,
                                                       pattern Wait)
import qualified Onyx.MIDI.Track.File                 as F
import           Onyx.MIDI.Track.Mania
import           Onyx.PhaseShift.Dance                (NoteType (..))
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Files                      (fixFileCase)
import           Onyx.Util.Handle
import qualified Sound.MIDI.Util                      as U
import           System.Directory                     (doesFileExist)
import           System.FilePath

data TempoChunk
  = TempoPlay U.Beats U.BPS
  | TempoStop U.BPS -- always lasts 1 beat

-- Translate the DDR-style tempo map with stops into one with no stops
buildSMTempo :: SM -> (U.TempoMap, U.MeasureMap, U.Beats -> (U.Beats, Maybe U.Beats))
buildSMTempo sm = let
  -- TODO support negative BPM somehow
  allTempoChanges :: Map.Map U.Beats U.BPS
  allTempoChanges = Map.fromList $ map (\(b, bpm) -> (realToFrac b, realToFrac bpm / 60)) $ sm_BPMS sm
  buildBar :: Int -> [TempoChunk]
  buildBar i = let
    barLength, srcBeatStart, srcBeatEnd :: U.Beats
    barLength = 4 -- will change when we add .ssc
    srcBeatStart = fromIntegral i * 4 -- will be more complicated with .ssc
    srcBeatEnd = srcBeatStart + barLength
    startTempo :: U.BPS
    startTempo = maybe 2 snd $ Map.lookupLE srcBeatStart allTempoChanges
    thisBarTempoChanges :: Map.Map U.Beats U.BPS
    thisBarTempoChanges = fst $ Map.split srcBeatEnd $ snd $ Map.split srcBeatStart allTempoChanges
    thisBarStops :: Map.Map U.Beats U.Seconds
    thisBarStops = Map.fromList $ mapMaybe
      (\(b, s) -> let
        b' = realToFrac b :: U.Beats
        in guard (srcBeatStart <= b' && b' < srcBeatEnd) >> Just (b', realToFrac s)
      )
      (sm_STOPS sm)
    makeChunks pos = if pos >= srcBeatEnd
      then []
      else let
        tempo = maybe startTempo snd $ Map.lookupLE pos thisBarTempoChanges
        stop = Map.lookup pos thisBarStops
        chunkEnd = foldr min srcBeatEnd $ catMaybes
          [ fst <$> Map.lookupGT pos thisBarTempoChanges
          , fst <$> Map.lookupGT pos thisBarStops
          ]
        in maybe id (\secs -> (TempoStop (U.makeTempo (1 :: U.Beats) secs) :)) stop
          $ (TempoPlay (chunkEnd - pos) tempo :)
          $ makeChunks chunkEnd
    in makeChunks srcBeatStart
  numBars = foldr max 1 $ map (length . smn_Notes) $ sm_NOTES sm
  builtBars = take numBars $ map buildBar [0 ..]
  tempoMap = U.tempoMapFromBPS
    $ foldr ($) RTB.empty
    $ map (\case TempoPlay b bps -> Wait 0 bps . RTB.delay b; TempoStop bps -> Wait 0 bps . RTB.delay 1)
    $ concat builtBars
  measureMap = U.measureMapFromLengths U.Error
    $ foldr ($) RTB.empty
    $ map (\chunks -> let
      barLength = 4 + sum (map (\case TempoStop{} -> 1; TempoPlay{} -> 0) chunks)
      in Wait 0 barLength . RTB.delay barLength
      )
    $ builtBars
  translator = Map.fromList $ makeTranslator False 0 0 $ concat builtBars
  makeTranslator isStop src dest = \case
    [] -> []
    TempoPlay len _ : rest -> (src, (dest, isStop)) : makeTranslator False (src + len) (dest + len) rest
    TempoStop _ : rest -> makeTranslator True src (dest + 1) rest
  translatePosition pos = case Map.lookupLE pos translator of
    Nothing -> (pos, Nothing) -- shouldn't happen!
    Just (src, (dest, isStop)) -> case pos - src of
      0    -> (dest, if isStop then Just $ dest + 1 else Nothing)
      diff -> (dest + diff, Nothing)
  in (tempoMap, measureMap, translatePosition)

getManiaTrack
  :: SMNotes
  -> (U.Beats -> (U.Beats, Maybe U.Beats))
  -> ManiaTrack U.Beats
getManiaTrack smn translate = let
  sourceChars :: RTB.T U.Beats (Int, Char)
  sourceChars
    = RTB.filter (\(_, x) -> x /= '0')
    $ RTB.flatten
    $ fmap (zip [0..] . T.unpack)
    $ foldr ($) RTB.empty
    $ concatMap (\divisions -> let
      barLength = 4 :: U.Beats -- will change in .ssc
      divisionLength = barLength / fromIntegral (length divisions)
      in map (\line -> Wait 0 line . RTB.delay divisionLength) divisions
      )
    $ smn_Notes smn
  maxKey = foldr max 0 $ fst <$> toList sourceChars
  in ManiaTrack
    { maniaNotes = blipEdgesRBNice $ foldr RTB.merge RTB.empty $ do
      key <- [0 .. maxKey]
      let filtered = RTB.mapMaybe (\(key', c) -> guard (key == key') >> Just c) sourceChars
          go ANil = ANil
          go (At t x rest) = case x of
            -- normal notes: sustain if on a stop
            '1' -> At posStart ((), (key, NoteNormal), sustainOnStop) $ go rest
            '2' -> At posStart ((), (key, NoteNormal), normalSustain) $ go rest
            '4' -> At posStart ((), (key, NoteRoll), normalSustain) $ go rest
            'M' -> At posStart ((), (key, NoteMine), sustainOnStop) $ go rest -- do sustains work?
            'L' -> At posStart ((), (key, NoteLift), sustainOnStop) $ go rest -- do sustains work?
            '3' -> go rest -- ignore, hopefully it stopped a previous hold/roll
            _   -> go rest -- ignore unrecognized note types; could also assume NoteNormal
            where (posStart, posEnd) = translate t
                  sustainOnStop = subtract posStart <$> posEnd
                  normalSustain = case rest of
                    ANil -> Nothing -- shouldn't happen?
                    -- this is expected to be a '3' but we'll stop for any next event on this key
                    At t2 _ _ -> case translate t2 of
                      (t2', _) -> Just $ t2' - posStart
      return $ RTB.fromAbsoluteEventList $ go $ RTB.toAbsoluteEventList 0 filtered
    , maniaOverdrive = RTB.empty
    }

importSM :: (SendMessage m, MonadIO m) => FilePath -> Import m
importSM src level = do
  when (level == ImportFull) $ lg $ "Importing StepMania song from: " <> src
  sm <- case map toLower $ takeExtension src of
    ".sm"  -> stackIO (loadSMLines src) >>= readSM
    ".dwi" -> stackIO (loadSMLines src) >>= fmap convertDWItoSM . readDWI
    _      -> fatal $ "Unrecognized StepMania format extension: " <> src
  -- both of these are encodings for "warps"
  when (any (\(_, bpm) -> bpm < 0) $ sm_BPMS sm) $ fatal
    "Song contains negative BPMs, which are not supported yet."
  when (any (\(_, t) -> t < 0) $ sm_STOPS sm) $ fatal
    "Song contains negative stops, which are not supported yet."
  let (delayAudio, delayMIDI) = case sm_OFFSET sm of
        Nothing -> (id, id)
        Just n -> case compare n 0 of
          -- sign is opposite how FoF delay works, positive offset = midi starts before audio
          EQ -> (id, id)
          LT -> let
            secs = realToFrac $ abs n :: Double
            midiDelay = ceiling secs
            audioDelay = fromIntegral midiDelay - secs
            in (Pad Start $ CA.Seconds audioDelay, F.padAnyFile midiDelay)
          GT ->
            ( Pad Start $ CA.Seconds $ realToFrac n
            , id
            )
      importableSMN smn = elem (smn_ChartType smn) ["dance-single", "pump-single"]
  mid <- case level of
    ImportQuick -> return emptyChart
    ImportFull -> do
      let (tempos, sigs, translate) = buildSMTempo sm
      return $ delayMIDI $ F.Song tempos sigs mempty
        { F.onyxParts = Map.singleton (F.FlexExtra "dance") mempty
          { F.onyxPartMania = Map.fromList $ do
            smn <- sm_NOTES sm
            guard $ importableSMN smn
            return (smn_Difficulty smn, getManiaTrack smn translate)
          }
        }

  let audioOptions = catMaybes
        [ (\f -> takeDirectory src </> T.unpack f) <$> sm_MUSIC sm
        , Just $ src -<.> "ogg"
        , Just $ src -<.> "mp3"
        ]
  audio <- fmap (fromMaybe HM.empty) $ flip firstJustM audioOptions $ \opt -> do
    p <- fixFileCase opt
    stackIO (doesFileExist p) >>= \case
      False -> return Nothing
      True -> stackIO (audioChannels p) >>= \case
        Nothing -> fatal $ "Couldn't get channel count of audio file: " <> p
        Just chans -> return $ Just $ HM.singleton "audio-file" $ AudioFile AudioInfo
          { md5 = Nothing
          , frames = Nothing
          , filePath = Just
            $ SoftFile ("audio" <> takeExtension p)
            $ SoftReadable $ fileReadable p
          , commands = []
          , rate = Nothing
          , channels = chans
          }

  let imageExts = ["png", "jpg", "jpeg"]
      isImageExt f = elem (dropWhile (== '.') $ map toLower $ takeExtension f) imageExts
      addImageExts f = map (f <.>) imageExts
      bannerOptions = concat
        -- try jacket (square art) first
        [ filter isImageExt $ (\f -> takeDirectory src </> T.unpack f) <$> toList (sm_JACKET sm)
        , addImageExts $ dropExtension src <> "-jacket"
        -- have seen a video in BANNER field, so we verify extensions
        , filter isImageExt $ (\f -> takeDirectory src </> T.unpack f) <$> toList (sm_BANNER sm)
        , addImageExts $ dropExtension src
        , addImageExts $ dropExtension src <> "-banner"
        ]
      bgOptions = concat
        [ filter isImageExt $ (\f -> takeDirectory src </> T.unpack f) <$> toList (sm_BACKGROUND sm)
        , addImageExts $ dropExtension src <> "-bg"
        ]
  banner <- flip firstJustM bannerOptions $ \opt -> do
    path' <- fixFileCase opt
    stackIO (doesFileExist path') >>= \case
      True  -> return $ Just $ SoftFile ("banner" <.> takeExtension path') $ SoftReadable $ fileReadable path'
      False -> return Nothing
  background <- flip firstJustM bgOptions $ \opt -> do
    path' <- fixFileCase opt
    stackIO (doesFileExist path') >>= \case
      True  -> return $ Just $ SoftFile ("background" <.> takeExtension path') $ SoftReadable $ fileReadable path'
      False -> return Nothing

  -- disabled for now (various format issues to figure out)
  {-
  let videoExts = ["avi", "mp4"] -- others?
      _isVideoExt f = elem (map toLower $ takeExtension f) videoExts
      addVideoExts f = map (f <.>) videoExts
      videoOptions = concat
        [ addVideoExts $ dropExtension src
        ]
  video <- flip firstJustM videoOptions $ \opt -> do
    p <- fixFileCase opt
    stackIO (doesFileExist p) >>= \case
      False -> return Nothing
      True -> return $ Just VideoInfo
        { fileVideo = SoftFile ("video" <.> takeExtension p) $ SoftReadable $ fileReadable p
        , videoStartTime = Nothing
        , videoEndTime = Nothing
        , videoLoop = False -- ?
        }
  -}
  let video = Nothing

  -- logic to include subtitle, and juggle jp/transliterated versions
  let combineTitleSub (Just t) (Just st) = Just $ t <> if T.any isAlphaNum $ T.take 1 st
        then " - " <> st
        else " "   <> st
      combineTitleSub t        st        = t <|> st
      titleOrig = combineTitleSub (sm_TITLE sm) (sm_SUBTITLE sm)
      titleTranslit = case (sm_TITLETRANSLIT sm, sm_SUBTITLETRANSLIT sm) of
        (Nothing, Nothing) -> Nothing
        _                  -> combineTitleSub
          (sm_TITLETRANSLIT    sm <|> sm_TITLE    sm)
          (sm_SUBTITLETRANSLIT sm <|> sm_SUBTITLE sm)
      (title, titleJP) = case (titleTranslit, titleOrig) of
        (Nothing, Just jp) -> (Just jp, Nothing)
        pair               -> pair
      (artist, artistJP) = case (sm_ARTISTTRANSLIT sm, sm_ARTIST sm) of
        (Nothing, Just jp) -> (Just jp, Nothing)
        pair               -> pair

  let diffNameList
        = map smn_Difficulty
        $ sortOn smn_NumericalMeter
        $ filter importableSMN
        $ sm_NOTES sm
  diffNames <- maybe (fatal "No .sm importable difficulties found") return $ NE.nonEmpty diffNameList

  return SongYaml
    { metadata = def'
      { title = title
      , titleJP = titleJP
      , artist = artist
      , artistJP = artistJP
      , genre = sm_GENRE sm
      , author = case filter (not . T.null) $ nubOrd $ map smn_Author $ sm_NOTES sm of
        [] -> Nothing
        xs -> Just $ T.intercalate ", " xs
      , fileAlbumArt = banner
      -- TODO these need to be adjusted for delay padding
      , previewStart = PreviewSeconds . realToFrac <$> sm_SAMPLESTART sm
      , previewEnd = do
        start <- sm_SAMPLESTART sm
        len <- sm_SAMPLELENGTH sm
        return $ PreviewSeconds $ realToFrac $ start + len
      }
    , jammit = HM.empty
    , audio = audio
    , plans = HM.singleton "sm" $ StandardPlan StandardPlanInfo
      { song = do
        guard $ not $ HM.null audio
        return $ delayAudio $ Input $ Named "audio-file"
      , parts = Parts HM.empty
      , crowd = Nothing
      , comments = []
      , tuningCents = 0
      , fileTempo = Nothing
      }
    , targets = HM.empty
    , parts = Parts $ HM.singleton (F.FlexExtra "dance") (emptyPart :: Part SoftFile)
      { mania = Just PartMania
        { difficulty = Tier $ max 1 $ let
          -- as a hack, get max meter value and subtract 4 (so 10 becomes 6)
          meters
            = map smn_NumericalMeter
            $ filter importableSMN
            $ sm_NOTES sm
          in fromIntegral $ foldr max 0 meters - 4
        -- TODO I guess we ought to have each chart able to have its own keys count
        , keys = foldr max 1 $ do
          smn <- sm_NOTES sm
          case concat $ smn_Notes smn of
            line : _ -> [T.length line]
            []       -> []
        , turntable = False
        , instrument = Nothing
        , charts = diffNames
        }
      }
    , global = Global
      { fileMidi = SoftFile "notes.mid" $ SoftChart mid
      , fileSongAnim = Nothing
      , autogenTheme = Nothing
      , animTempo = Left KTempoMedium
      , backgroundVideo = video
      , fileBackgroundImage = background
      }
    }
