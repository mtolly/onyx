{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Onyx.Import.Osu where

import           Control.Monad                    (forM, guard, when)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Bits                        (testBit)
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isDigit, toLower)
import qualified Data.Conduit.Audio               as CA
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, mapMaybe)
import           Data.Scientific                  (Scientific)
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           Onyx.Audio
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (blipEdgesRB_)
import           Onyx.MIDI.Track.Drums
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.MIDI.Track.Mania
import           Onyx.Osu.Base
import           Onyx.PhaseShift.Dance            (NoteType (NoteNormal))
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Taiko.Base
import           Onyx.Util.Handle
import           Onyx.Util.Text.Decode            (decodeGeneral)
import           Onyx.Zip.Load                    (loadZipStrictBytes)
import           Optics                           ((.~))
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeExtension, (<.>))
import           Text.Read                        (readMaybe)

importOsu :: (SendMessage m, MonadIO m) => Bool -> FilePath -> StackTraceT m [Import m]
importOsu separateSongs f = do

  tree <- stackIO $ loadZipStrictBytes Nothing f

  loadedOsus <- fmap catMaybes $ forM (folderFiles tree) \(name, getBS) -> do
    inside ("Loading: " <> T.unpack name) do
      case map toLower $ takeExtension $ T.unpack name of
        ".osu" -> fmap Just do
          txt <- fmap decodeGeneral $ stackIO getBS
          splitOsuFile txt >>= readOsu . snd
        _ -> return Nothing

  -- currently only loading Mania and Taiko charts
  let filteredOsus = filter (\osu -> elem osu.general.mode [1, 3]) loadedOsus
  when (null filteredOsus) $ warn "No importable .osu files found in .osz"
  osuSets <- if separateSongs
    then return $ map (\x -> (x, [x])) filteredOsus
    else case filteredOsus of
      []    -> return []
      -- arbitrarily pick one .osu to use for metadata and timing
      o : _ -> return [(o, filteredOsus)]

  return $ osuSets <&> \(primary, osus) level -> do

    let timingMid = getOsuTiming primary

    convertedTracks <- fmap catMaybes $ forM osus \osu -> case osu.metadata.version of
      Nothing -> return Nothing
      Just version -> case osu.general.mode of
        1 -> let
          track = taikoToTrack timingMid.tempos osu
          partName = if separateSongs
            then F.PartDrums
            else F.PartName version
          in return $ Just (partName, osu, Left track)
        3 -> let
          track = maniaToTrack timingMid.tempos osu
          partName = if separateSongs
            then F.PartKeys
            else F.PartName version
          in return $ Just (partName, osu, Right track)
        _ -> return Nothing

    -- Note, we do not yet handle "Sample" commands in the events list.
    -- See "92190 Yuuna Sasara feat. Tai no Kobone - Imperishable Night 2006"
    -- for usage of this, and also "AudioFilename: virtual" which I assume means
    -- basically no single backing track (all Sample + hit sounds)

    audio <- case level of
      ImportQuick -> return Nothing
      ImportFull -> do
        audioBytes <- case splitPath primary.general.audioFilename >>= (`findFileCI` tree) of
          Nothing    -> fatal $ "Couldn't find audio file: " <> T.unpack primary.general.audioFilename
          Just getBS -> BL.fromStrict <$> stackIO getBS
        let audioName = "audio" <.> takeExtension (T.unpack primary.general.audioFilename)
        return $ Just ("osu-audio-file", audioName, makeHandle audioName $ byteStringSimpleHandle audioBytes)

    taikoSamples <- case level of
      ImportQuick -> return []
      ImportFull -> if any (\osu -> osu.general.mode == 1) loadedOsus -- are we loading any taiko charts
        then loadTaikoSamples
        else return []
    let taikoSampleTracks = do
          (partName, _, track) <- convertedTracks
          case track of
            Left  drums  -> [(taikoKeySamples partName, taikoSampleTrack drums)]
            Right _mania -> []

    background <- case level of
      ImportQuick -> return Nothing
      ImportFull -> let
        backgrounds = do
          event <- V.toList primary.events
          guard $ event.eventType == Right 0
          name <- toList $ event.eventParams V.!? 1
          getBG <- toList $ splitPath name >>= (`findFileCI` tree)
          return (name, getBG)
        in case backgrounds of
          []                -> return Nothing
          (name, getBG) : _ -> do
            let newName = "background" <.> takeExtension (T.unpack name)
            bytes <- BL.fromStrict <$> stackIO getBG
            return $ Just $ SoftFile newName $ SoftReadable
              $ makeHandle newName $ byteStringSimpleHandle bytes

    let addVersion title = case (separateSongs, primary.metadata.version) of
          (True, Just version) -> title <> " [" <> version <> "]"
          _                    -> title

    return SongYaml
      { metadata = def'
        { title = addVersion <$> primary.metadata.title
        , titleJP = addVersion <$> primary.metadata.titleUnicode
        , artist = primary.metadata.artist
        , artistJP = primary.metadata.artistUnicode
        , previewStart = if primary.general.previewTime >= 0
          then Just $ PreviewSeconds $ fromIntegral primary.general.previewTime / 1000
          else Nothing
        , fileAlbumArt = background -- just so there's not nothing
        , author = case nubOrd $ mapMaybe (.metadata.creator) osus of
          []      -> Nothing
          authors -> Just $ T.intercalate ", " authors
        }
      , global = def'
        { backgroundVideo = Nothing -- TODO
        , fileBackgroundImage = background
        , fileMidi = SoftFile "notes.mid" $ SoftChart case level of
          ImportFull  -> timingMid
            { F.tracks = mempty
              { F.onyxParts = Map.fromList do
                (partName, _, track) <- convertedTracks
                case track of
                  Left  drums -> return (partName, mempty { F.onyxPartDrums = drums })
                  Right mania -> return (partName, mempty { F.onyxPartMania = Map.singleton "chart" mania })
              , F.onyxSamples = Map.fromList taikoSampleTracks
              }
            }
          ImportQuick -> emptyChart
        , fileMidiOriginal = Nothing
        , fileSongAnim = Nothing
        }
      , jammit = HM.empty
      , audio = HM.fromList let
        audioFiles = (toList audio <> taikoSamples) <&> \(key, fileName, r) ->
          (key, AudioFile AudioInfo
            { md5 = Nothing
            , frames = Nothing
            , filePath = Just $ SoftFile fileName $ SoftReadable r
            , commands = []
            , rate = Nothing
            , channels = 2 -- TODO maybe verify
            }
          )
        audioSamples = do
          (key, _) <- taikoSampleTracks
          return (key, AudioSamples SamplesInfo
            { groupPolyphony = Nothing
            , groupCrossfade = 0
            })
        in audioFiles <> audioSamples
      , plans = HM.singleton "osu-audio" $ StandardPlan StandardPlanInfo
        { song = audio <&> \(key, fileName, _) -> let
          -- We now skip initial decoder delay when decoding mp3s via ffmpeg.
          -- But for some reason osu charts seem to expect that .ogg also
          -- is going to get this amount skipped? Very weird
          extraDelay = case map toLower $ takeExtension fileName of
            ".mp3" -> id
            _      -> Drop Start $ CA.Frames 1105
          in extraDelay $ Input $ Named key
        , parts = Parts $ HM.fromList do
          (partName, _, track) <- convertedTracks
          case track of
            Left  _drums -> [(partName, PartSingle $ Input $ Named $ taikoKeySamples partName)]
            Right _mania -> []
        , crowd = Nothing
        , comments = []
        , tuningCents = 0
        , fileTempo = Nothing
        }
      , targets = HM.empty
      , parts = Parts $ HM.fromList do
        (partName, osu, track) <- convertedTracks
        case track of
          Left _drums -> return (partName, emptyPart
            & #drums .~ Just (emptyPartDrums Drums4 Kicks1x)
            )
          Right _mania -> return (partName, emptyPart & #mania .~ Just do
            ModeMania $ return ManiaChart
              { name = "chart"
              , keys = fromIntegral $ maniaColumnCount osu
              , style = if osu.general.specialStyle then ManiaTurntable else ManiaDefault
              , difficulty = Tier 1 -- ?
              }
            )
      }

maniaColumnCount :: OsuFile -> Integer
maniaColumnCount osu = min 10 $ max 1 $ maybe 10 round osu.difficulty.circleSize

-- fix cases where hit objects that should be simultaneous are off by 1 ms
unslopHitObjects :: [OsuHitObject] -> [OsuHitObject]
unslopHitObjects = go Nothing where
  go _           []           = []
  go Nothing     (hit : rest) = hit : go (Just hit) rest
  go (Just prev) (hit : rest) = let
    hit' :: OsuHitObject
    hit' = if hit.time == prev.time + 1
      then hit & #time .~ prev.time
      else hit
    in hit' : go (Just hit') rest

getManiaChart :: OsuFile -> [(U.Seconds, (Integer, Maybe U.Seconds))]
getManiaChart osu = let
  columnCount = maniaColumnCount osu
  xToColumn :: Integer -> Integer
  xToColumn x = max 0 $ min (columnCount - 1) $ quot (x * columnCount) 512
  msToSecs :: Integer -> U.Seconds
  msToSecs ms = fromIntegral ms / 1000
  in do
    hit <- unslopHitObjects $ V.toList osu.hitObjects
    note <- if testBit hit.type_ 0 -- hit circle
      then return (xToColumn hit.x, Nothing)
      else if testBit hit.type_ 7 -- hold
        then let
          endTime = fmap msToSecs $ hit.objectParams V.!? 0 >>= readMaybe . takeWhile isDigit . T.unpack
          in return (xToColumn hit.x, endTime)
        else []
    return (msToSecs hit.time, note)

maniaToTrack :: U.TempoMap -> OsuFile -> ManiaTrack U.Beats
maniaToTrack tmap osu = let
  mania = getManiaChart osu
  in ManiaTrack
    { notes = blipEdgesRB_ $ RTB.fromAbsoluteEventList $ ATB.fromPairList $
      mania >>= \(secs, (column, endHold)) -> let
        startBeats = U.unapplyTempoMap tmap secs
        holdBeats = (\endSecs -> U.unapplyTempoMap tmap endSecs - startBeats) <$> endHold
        in return (startBeats, ((fromIntegral column, NoteNormal), holdBeats))
    , overdrive = RTB.empty
    }

taikoToTrack :: U.TempoMap -> OsuFile -> DrumTrack U.Beats
taikoToTrack tmap osu
  = taikoEventsToTrack tmap
  $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ getTaikoChart osu

getTaikoChart :: OsuFile -> [(U.Seconds, TaikoEvent U.Seconds)]
getTaikoChart osu = let
  msToSecs :: Integer -> U.Seconds
  msToSecs ms = fromIntegral ms / 1000
  timingLookup, uninheritedLookup :: Map.Map Scientific OsuTimingPoint
  timingLookup = Map.fromList do
    tp <- V.toList osu.timingPoints
    return (tp.time, tp)
  uninheritedLookup = Map.filter (.uninherited) timingLookup
  in do
    hit <- unslopHitObjects $ V.toList osu.hitObjects
    let whistleClap = testBit hit.hitSound 1 || testBit hit.hitSound 3
        finish      = testBit hit.hitSound 2
    event <- if testBit hit.type_ 0 -- hit circle
      then return case (whistleClap, finish) of
        (False, False) -> TaikoSingle TaikoCenter
        (True , False) -> TaikoSingle TaikoRim
        (False, True ) -> TaikoSingle TaikoCenterLarge
        (True , True ) -> TaikoSingle TaikoRimLarge
      else if testBit hit.type_ 1 -- slider (drum roll)
        then toList do
          -- https://osu.ppy.sh/wiki/en/Client/File_formats/Osu_%28file_format%29#sliders says:
          -- length / (SliderMultiplier * 100 * SV) * beatLength = ms of one "side" of a slider
          lengthPixels <- (hit.objectParams V.!? 2) >>= readMaybe . T.unpack :: Maybe Scientific
          sliderMultiplier <- osu.difficulty.sliderMultiplier
          thisTiming <- snd <$> Map.lookupLE (fromIntegral hit.time) timingLookup
          thisUninherited <- if thisTiming.uninherited
            then return thisTiming
            else snd <$> Map.lookupLE (fromIntegral hit.time) uninheritedLookup
          -- is slides ever not 1 for taiko charts?
          slides <- (hit.objectParams V.!? 1) >>= readMaybe . T.unpack :: Maybe Integer
          let beatLength = thisUninherited.beatLength
              sv = if thisTiming.uninherited then 1
                else negate $ thisTiming.beatLength / 100
              sideLengthMS :: Rational
              sideLengthMS = toRational lengthPixels / toRational (sliderMultiplier * 100 * sv) * toRational beatLength
              totalLengthMS :: Integer
              totalLengthMS = floor $ sideLengthMS * fromIntegral slides
          return $ (if finish then TaikoRollLarge else TaikoRoll) $ msToSecs totalLengthMS
        else if testBit hit.type_ 3 -- spinner (denden)
          then do
            endMS <- toList $ (hit.objectParams V.!? 0) >>= readMaybe . T.unpack
            return $ TaikoDenden $ msToSecs $ endMS - hit.time
          else []
    return (msToSecs hit.time, event)
