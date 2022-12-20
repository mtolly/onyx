{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Onyx.Import.StepMania where

import           Control.Applicative                  ((<|>))
import           Control.Monad.Extra                  (firstJustM, guard, when)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Char                            (isAlphaNum, toLower)
import qualified Data.Conduit.Audio                   as CA
import           Data.Default.Class                   (def)
import qualified Data.EventList.Relative.TimeBody     as RTB
import           Data.Foldable                        (toList)
import qualified Data.HashMap.Strict                  as HM
import           Data.List.Extra                      (nubOrd)
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
import           Onyx.PhaseShift.Dance
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

getDanceDifficulty
  :: SMNotes
  -> (U.Beats -> (U.Beats, Maybe U.Beats))
  -> DanceDifficulty U.Beats
getDanceDifficulty smn translate = let
  sourceChars :: RTB.T U.Beats (Arrow, Char)
  sourceChars
    = RTB.filter (\(_, x) -> x /= '0')
    $ RTB.flatten
    $ fmap (zip [ArrowL, ArrowD, ArrowU, ArrowR] . T.unpack)
    $ foldr ($) RTB.empty
    $ concatMap (\divisions -> let
      barLength = 4 :: U.Beats -- will change in .ssc
      divisionLength = barLength / fromIntegral (length divisions)
      in map (\line -> Wait 0 line . RTB.delay divisionLength) divisions
      )
    $ smn_Notes smn
  in DanceDifficulty
    { danceNotes = blipEdgesRBNice $ foldr RTB.merge RTB.empty $ do
      arrow <- [minBound .. maxBound]
      let filtered = RTB.mapMaybe (\(arrow', c) -> guard (arrow == arrow') >> Just c) sourceChars
          go ANil = ANil
          go (At t x rest) = case x of
            -- normal notes: sustain if on a stop
            '1' -> At posStart ((), (arrow, NoteNormal), sustainOnStop) $ go rest
            '2' -> At posStart ((), (arrow, NoteNormal), normalSustain) $ go rest
            '4' -> At posStart ((), (arrow, NoteRoll), normalSustain) $ go rest
            'M' -> At posStart ((), (arrow, NoteMine), sustainOnStop) $ go rest -- do sustains work?
            'L' -> At posStart ((), (arrow, NoteLift), sustainOnStop) $ go rest -- do sustains work?
            '3' -> go rest -- ignore, hopefully it stopped a previous hold/roll
            _ -> go rest -- ignore unrecognized note types; could also assume NoteNormal
            where (posStart, posEnd) = translate t
                  sustainOnStop = subtract posStart <$> posEnd
                  normalSustain = case rest of
                    ANil -> Nothing -- shouldn't happen?
                    -- this is expected to be a '3' but we'll stop for any next event on this arrow
                    At t2 _ _ -> case translate t2 of
                      (t2', _) -> Just $ t2' - posStart
      return $ RTB.fromAbsoluteEventList $ go $ RTB.toAbsoluteEventList 0 filtered
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
  mid <- case level of
    ImportQuick -> return emptyChart
    ImportFull -> do
      let (tempos, sigs, translate) = buildSMTempo sm
          getDanceTrack k = let
            matches = flip mapMaybe (sm_NOTES sm) $ \smn -> if smn_ChartType smn == k
              then case smn_Difficulty smn of
                "Beginner"  -> Just (SMBeginner , smn)
                "Easy"      -> Just (SMEasy     , smn)
                "Medium"    -> Just (SMMedium   , smn)
                "Hard"      -> Just (SMHard     , smn)
                "Challenge" -> Just (SMChallenge, smn)
                _           -> Nothing -- TODO warn?
              else Nothing
            in DanceTrack
              { danceDifficulties = Map.fromList $ map
                (\(diff, smn) -> (diff, getDanceDifficulty smn translate))
                matches
              , danceOverdrive = RTB.empty
              }
      return $ delayMIDI $ F.Song tempos sigs mempty
        { F.onyxParts = Map.singleton (F.FlexExtra "global") mempty
          { F.onyxPartDance = getDanceTrack "dance-single"
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
          { _md5 = Nothing
          , _frames = Nothing
          , _filePath = Just
            $ SoftFile ("audio" <> takeExtension p)
            $ SoftReadable $ fileReadable p
          , _commands = []
          , _rate = Nothing
          , _channels = chans
          }

  let imageExts f = map (f <.>) ["png", "jpg", "jpeg"]
      bannerOptions = concat
        -- try jacket (square art) first
        [ (\f -> takeDirectory src </> T.unpack f) <$> toList (sm_JACKET sm)
        , imageExts $ dropExtension src <> "-jacket"
        , (\f -> takeDirectory src </> T.unpack f) <$> toList (sm_BANNER sm)
        , imageExts $ dropExtension src
        , imageExts $ dropExtension src <> "-banner"
        ]
      bgOptions = concat
        [ (\f -> takeDirectory src </> T.unpack f) <$> toList (sm_BACKGROUND sm)
        , imageExts $ dropExtension src <> "-bg"
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

  return SongYaml
    { _metadata = def'
      { _title = title
      , _titleJP = titleJP
      , _artist = artist
      , _artistJP = artistJP
      , _genre = sm_GENRE sm
      , _author = case filter (not . T.null) $ nubOrd $ map smn_Author $ sm_NOTES sm of
        [] -> Nothing
        xs -> Just $ T.intercalate ", " xs
      , _fileAlbumArt = banner
      -- TODO these need to be adjusted for delay padding
      , _previewStart = PreviewSeconds . realToFrac <$> sm_SAMPLESTART sm
      , _previewEnd = do
        start <- sm_SAMPLESTART sm
        len <- sm_SAMPLELENGTH sm
        return $ PreviewSeconds $ realToFrac $ start + len
      }
    , _jammit = HM.empty
    , _audio = audio
    , _plans = HM.singleton "sm" Plan
      { _song = flip fmap (sm_MUSIC sm) $ \_ -> PlanAudio
        { _planExpr = delayAudio $ Input $ Named "audio-file"
        , _planPans = []
        , _planVols = []
        }
      , _countin = Countin []
      , _planParts = Parts HM.empty
      , _crowd = Nothing
      , _planComments = []
      , _tuningCents = 0
      , _fileTempo = Nothing
      }
    , _targets = HM.empty
    , _parts = Parts $ HM.singleton (F.FlexExtra "global") def
      { partDance = Just PartDance
        { danceDifficulty = Tier $ max 1 $ let
          -- as a hack, get max meter value and subtract 4 (so 10 becomes 6)
          meters
            = map smn_NumericalMeter
            $ filter (\smn -> smn_ChartType smn == "dance-single")
            $ sm_NOTES sm
          in fromIntegral $ foldr max 0 meters - 4
        }
      }
    , _global = Global
      { _fileMidi = SoftFile "notes.mid" $ SoftChart mid
      , _fileSongAnim = Nothing
      , _autogenTheme = Nothing
      , _animTempo = Left KTempoMedium
      , _backgroundVideo = Nothing
      , _fileBackgroundImage = background
      }
    }
