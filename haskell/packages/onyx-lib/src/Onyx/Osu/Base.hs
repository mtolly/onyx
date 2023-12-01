{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Osu.Base where

import           Control.Monad                    (forM, when)
import           Data.Char                        (isAlphaNum)
import           Data.Default.Class
import           Data.Either
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           Onyx.MIDI.Track.Beat             (BeatEvent (..))
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.StackTrace
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

data OsuFile = OsuFile
  { general      :: OsuGeneral
  , editor       :: OsuEditor
  , metadata     :: OsuMetadata
  , difficulty   :: OsuDifficulty
  , events       :: V.Vector OsuEvent
  , timingPoints :: V.Vector OsuTimingPoint
  , colours      :: V.Vector OsuColour
  , hitObjects   :: V.Vector OsuHitObject
  } deriving (Show)

data OsuGeneral = OsuGeneral
  { audioFilename            :: T.Text
  , audioLeadIn              :: Integer
  , previewTime              :: Integer
  , countdown                :: Integer
  , sampleSet                :: T.Text
  , stackLeniency            :: Scientific
  , mode                     :: Integer
  , letterboxInBreaks        :: Bool
  , useSkinSprites           :: Bool
  , overlayPosition          :: T.Text
  , skinPreference           :: T.Text
  , epilepsyWarning          :: Bool
  , countdownOffset          :: Integer
  , specialStyle             :: Bool
  , widescreenStoryboard     :: Bool
  , samplesMatchPlaybackRate :: Bool
  } deriving (Show)

instance Default OsuGeneral where
  def = OsuGeneral
    { audioFilename            = ""
    , audioLeadIn              = 0
    , previewTime              = -1
    , countdown                = 1
    , sampleSet                = "Normal"
    , stackLeniency            = 0.7
    , mode                     = 0
    , letterboxInBreaks        = False
    , useSkinSprites           = True
    , overlayPosition          = "NoChange"
    , skinPreference           = ""
    , epilepsyWarning          = False
    , countdownOffset          = 0
    , specialStyle             = False
    , widescreenStoryboard     = False
    , samplesMatchPlaybackRate = False
    }

data OsuEditor = OsuEditor
  { bookmarks       :: V.Vector Integer
  , distanceSpacing :: Maybe Scientific
  , beatDivisor     :: Maybe Integer
  , gridSize        :: Maybe Integer
  , timelineZoom    :: Maybe Scientific
  } deriving (Show)

instance Default OsuEditor where
  def = OsuEditor
    { bookmarks       = V.empty
    , distanceSpacing = Nothing
    , beatDivisor     = Nothing
    , gridSize        = Nothing
    , timelineZoom    = Nothing
    }

data OsuMetadata = OsuMetadata
  { title         :: Maybe T.Text
  , titleUnicode  :: Maybe T.Text
  , artist        :: Maybe T.Text
  , artistUnicode :: Maybe T.Text
  , creator       :: Maybe T.Text
  , version       :: Maybe T.Text
  , source        :: Maybe T.Text
  , tags          :: V.Vector T.Text
  , beatmapID     :: Maybe Integer
  , beatmapSetID  :: Maybe Integer
  } deriving (Show)

instance Default OsuMetadata where
  def = OsuMetadata
    { title         = Nothing
    , titleUnicode  = Nothing
    , artist        = Nothing
    , artistUnicode = Nothing
    , creator       = Nothing
    , version       = Nothing
    , source        = Nothing
    , tags          = V.empty
    , beatmapID     = Nothing
    , beatmapSetID  = Nothing
    }

data OsuDifficulty = OsuDifficulty
  { hpDrainRate       :: Maybe Scientific
  , circleSize        :: Maybe Scientific
  , overallDifficulty :: Maybe Scientific
  , approachRate      :: Maybe Scientific
  , sliderMultiplier  :: Maybe Scientific
  , sliderTickRate    :: Maybe Scientific
  } deriving (Show)

instance Default OsuDifficulty where
  def = OsuDifficulty
    { hpDrainRate       = Nothing
    , circleSize        = Nothing
    , overallDifficulty = Nothing
    , approachRate      = Nothing
    , sliderMultiplier  = Nothing
    , sliderTickRate    = Nothing
    }

data OsuEvent = OsuEvent
  { eventType   :: Either T.Text Integer
  -- simple backgrounds start with time, but storyboard commands do not. leaving as is
  , eventParams :: V.Vector T.Text
  } deriving (Show)

data OsuTimingPoint = OsuTimingPoint
  -- official docs say time should be int.
  -- but a few songs e.g. "85930 Famishin - Mecha Koi Ranman- (Piano Ver.)" have a decimal
  { time        :: Scientific
  , beatLength  :: Scientific
  , meter       :: Integer
  , sampleSet   :: Integer
  , sampleIndex :: Integer
  , volume      :: Integer
  , uninherited :: Bool
  , effects     :: Integer
  } deriving (Show)

data OsuColour = OsuColour
  { option :: T.Text
  , red    :: Integer
  , green  :: Integer
  , blue   :: Integer
  } deriving (Show)

data OsuHitObject = OsuHitObject
  { x            :: Integer
  , y            :: Integer
  , time         :: Integer
  , type_        :: Integer
  , hitSound     :: Integer
  , objectParams :: V.Vector T.Text -- may include hitSample info
  } deriving (Show)

splitOsuFile :: (SendMessage m) => T.Text -> StackTraceT m (Maybe Integer, [(T.Text, [T.Text])])
splitOsuFile f = do
  (version, linesRest) <- case T.lines $ T.filter (/= '\r') f of
    allLines@(line1 : linesRest) -> case T.stripPrefix "osu file format v" line1 of
      Just strVersion -> case readMaybe $ T.unpack strVersion of
        Just v  -> return (Just v, linesRest)
        Nothing -> do
          warn $ "Couldn't get osu format version number: " <> show line1
          return (Nothing, allLines)
      Nothing -> do
        warn $ "Couldn't recognize osu format line: " <> show line1
        return (Nothing, allLines)
    [] -> fail "osu file is empty"
  let sortLineType ln = case T.stripPrefix "[" ln >>= T.stripSuffix "]" of
        Just sectionName -> Left sectionName
        Nothing          -> Right ln
      sortedLines
        = map sortLineType
        $ filter (\ln -> not $ T.null ln || ("//" `T.isPrefixOf` ln))
        $ map T.strip linesRest
      getSections rest = case dropWhile isRight rest of
        Left sectionName : rest' -> let
          (thisSection, rest'') = span isRight rest'
          in (sectionName, rights thisSection) : getSections rest''
        _ -> []
  return (version, getSections sortedLines)

splitCommaList :: T.Text -> [T.Text]
splitCommaList "" = []
splitCommaList s = case T.stripPrefix "\"" s of
  Just startQuote -> case T.break (== '"') startQuote of
    (quote, rest) -> quote : splitCommaList (T.dropWhile (== ',') $ T.dropWhile (== '"') rest)
  Nothing         -> case T.break (== ',') s of
    (atom , rest) -> atom : splitCommaList (T.dropWhile (== ',') rest)

readOsu :: (SendMessage m) => [(T.Text, [T.Text])] -> StackTraceT m OsuFile
readOsu sections = do
  let splitKeyValue strip = mapMaybe $ \ln -> case T.span isAlphaNum ln of
        (key, rest) -> case T.stripPrefix ":" rest of
          Just value -> Just (key, maybeStrip value)
          Nothing    -> Nothing
        where maybeStrip = if strip then T.strip else id
      getSection k = concatMap snd $ filter ((== k) . fst) sections
      stringOr dflt = return . fromMaybe dflt
      parseOrNothing Nothing  = return Nothing
      parseOrNothing (Just s) = do
        let result = readMaybe $ T.unpack s
        when (isNothing result) $ warn $ "Couldn't parse as appropriate number type: " <> show s
        return result
      parseOr dflt Nothing  = return dflt
      parseOr dflt (Just s) = case readMaybe $ T.unpack s of
        Nothing -> do
          warn $ "Couldn't parse as appropriate number type: " <> show s
          return dflt
        Just n  -> return n
      parseOrFail s = case readMaybe $ T.unpack s of
        Nothing -> fatal $ "Couldn't parse as appropriate number type: " <> show s
        Just n  -> return n
      boolOrFail = \case
        "0" -> return False
        "1" -> return True
        s   -> fatal $ "Couldn't parse as boolean: " <> show s
      boolOr dflt = \case
        Nothing  -> return dflt
        Just "0" -> return False
        Just "1" -> return True
        Just s   -> do
          warn $ "Couldn't parse as boolean: " <> show s
          return dflt

  general <- do
    let section = splitKeyValue True $ getSection "General"
        d = def :: OsuGeneral
    audioFilename            <- stringOr d.audioFilename            $ lookup "AudioFilename"            section
    audioLeadIn              <- parseOr  d.audioLeadIn              $ lookup "AudioLeadIn"              section
    previewTime              <- parseOr  d.previewTime              $ lookup "PreviewTime"              section
    countdown                <- parseOr  d.countdown                $ lookup "Countdown"                section
    sampleSet                <- stringOr d.sampleSet                $ lookup "SampleSet"                section
    stackLeniency            <- parseOr  d.stackLeniency            $ lookup "StackLeniency"            section
    mode                     <- parseOr  d.mode                     $ lookup "Mode"                     section
    letterboxInBreaks        <- boolOr   d.letterboxInBreaks        $ lookup "LetterboxInBreaks"        section
    useSkinSprites           <- boolOr   d.useSkinSprites           $ lookup "UseSkinSprites"           section
    overlayPosition          <- stringOr d.overlayPosition          $ lookup "OverlayPosition"          section
    skinPreference           <- stringOr d.skinPreference           $ lookup "SkinPreference"           section
    epilepsyWarning          <- boolOr   d.epilepsyWarning          $ lookup "EpilepsyWarning"          section
    countdownOffset          <- parseOr  d.countdownOffset          $ lookup "CountdownOffset"          section
    specialStyle             <- boolOr   d.specialStyle             $ lookup "SpecialStyle"             section
    widescreenStoryboard     <- boolOr   d.widescreenStoryboard     $ lookup "WidescreenStoryboard"     section
    samplesMatchPlaybackRate <- boolOr   d.samplesMatchPlaybackRate $ lookup "SamplesMatchPlaybackRate" section
    return OsuGeneral{..}

  editor <- do
    let section = splitKeyValue True $ getSection "Editor"
    bookmarks       <- fmap (V.fromList . catMaybes) $ do
      forM (T.splitOn "," $ fromMaybe "" $ lookup "Bookmarks" section) $ \case
        "" -> return Nothing
        s  -> do
          let result = readMaybe $ T.unpack s
          when (isNothing result) $ warn $ "Couldn't parse bookmark number: " <> show s
          return result
    distanceSpacing <- parseOrNothing $ lookup "DistanceSpacing" section
    beatDivisor     <- parseOrNothing $ lookup "BeatDivisor" section
    gridSize        <- parseOrNothing $ lookup "GridSize" section
    timelineZoom    <- parseOrNothing $ lookup "TimelineZoom" section
    return OsuEditor{..}

  metadata <- do
    let section = splitKeyValue False $ getSection "Metadata"
    title         <- return         $ lookup "Title"         section
    titleUnicode  <- return         $ lookup "TitleUnicode"  section
    artist        <- return         $ lookup "Artist"        section
    artistUnicode <- return         $ lookup "ArtistUnicode" section
    creator       <- return         $ lookup "Creator"       section
    version       <- return         $ lookup "Version"       section
    source        <- return         $ lookup "Source"        section
    tags          <- fmap (V.fromList . T.words) $ stringOr "" $ lookup "Tags" section
    beatmapID     <- parseOrNothing $ lookup "BeatmapID"     section
    beatmapSetID  <- parseOrNothing $ lookup "BeatmapSetID"  section
    return OsuMetadata{..}

  difficulty <- do
    let section = splitKeyValue False $ getSection "Difficulty"
    hpDrainRate       <- parseOrNothing $ lookup "HpDrainRate"       section
    circleSize        <- parseOrNothing $ lookup "CircleSize"        section
    overallDifficulty <- parseOrNothing $ lookup "OverallDifficulty" section
    approachRate      <- parseOrNothing $ lookup "ApproachRate"      section
    sliderMultiplier  <- parseOrNothing $ lookup "SliderMultiplier"  section
    sliderTickRate    <- parseOrNothing $ lookup "SliderTickRate"    section
    return OsuDifficulty{..}

  events <- fmap (V.fromList . catMaybes) $ forM (getSection "Events") $ \ln -> do
    case splitCommaList ln of
      x : rest -> let
        eventType = maybe (Left x) Right $ readMaybe $ T.unpack x
        eventParams = V.fromList rest
        in return $ Just OsuEvent{..}
      _ -> do
        warn $ "Less than 2 values for event: " <> show ln
        return Nothing

  timingPoints <- fmap (V.fromList . catMaybes) $ forM (getSection "TimingPoints") $ \ln -> do
    case splitCommaList ln of
      [a, b, c, d, e, f, g, h] -> errorToWarning $ do
        time        <- parseOrFail a
        beatLength  <- parseOrFail b
        meter       <- parseOrFail c
        sampleSet   <- parseOrFail d
        sampleIndex <- parseOrFail e
        volume      <- parseOrFail f
        uninherited <- boolOrFail  g
        effects     <- parseOrFail h
        return OsuTimingPoint{..}
      _ -> do
        warn $ "Wrong value count (not 8) for event: " <> show ln
        return Nothing

  colours <- fmap (V.fromList . catMaybes) $ forM (getSection "Colours") $ \ln -> do
    case map T.strip $ T.splitOn ":" ln of
      [option, rgb] -> case splitCommaList rgb of
        [r, g, b] -> errorToWarning $ do
          red   <- parseOrFail r
          green <- parseOrFail g
          blue  <- parseOrFail b
          return OsuColour{..}
        _ -> do
          warn $ "Wrong value count (not 3) for RGB colour: " <> show ln
          return Nothing
      _ -> do
        warn $ "More than one colon in colour entry: " <> show ln
        return Nothing

  hitObjects <- fmap (V.fromList . catMaybes) $ forM (getSection "HitObjects") $ \ln -> do
    case splitCommaList ln of
      a : b : c : d : e : rest -> errorToWarning $ do
        x        <- parseOrFail a
        y        <- parseOrFail b
        time     <- parseOrFail c
        type_    <- parseOrFail d
        hitSound <- parseOrFail e
        let objectParams = V.fromList rest
        return OsuHitObject{..}
      _ -> do
        warn $ "Less than 5 value count for hit object: " <> show ln
        return Nothing

  return OsuFile{..}

-- Returns an infinite list of beat events
getOsuBeatlines :: OsuFile -> [(Integer, BeatEvent)]
getOsuBeatlines osu = let
  points = filter (.uninherited) $ V.toList $ osu.timingPoints
  beatlineStream :: OsuTimingPoint -> [(Integer, BeatEvent)]
  beatlineStream otp = let
    pattern = Bar : replicate (fromIntegral otp.meter - 1) Beat
    times = map (\i -> floor $ otp.time + otp.beatLength * fromIntegral i) [(0 :: Integer) ..]
    in zip times $ cycle pattern
  in case points of
    [] -> zip [0, 500 ..] $ cycle [Bar, Beat, Beat, Beat] -- hopefully shouldn't happen, fallback to 4/4 120bpm
    p : ps -> let
      prePointsTime = if p.time <= 0
        then [] -- first timing point is right at start (or before start, see discardNegativeTimes below)
        else let
          -- need to make some bars before first timing point
          numPreLines :: Integer
          numPreLines = quot (floor p.time) 600 + 1 -- this gives reasonable tempos (100-150 bpm) for not-too-short timespans
          portion = toRational p.time / toRational numPreLines
          times = map (\i -> floor $ toRational i * portion) [0 .. numPreLines - 1]
          pattern = [Bar, Beat, Beat, Beat]
          in zip times $ cycle pattern
      makeStream currentPoint [] = beatlineStream currentPoint
      makeStream currentPoint (nextPoint : restPoints) = let
        -- we stop this stream slightly before the next one,
        -- otherwise we get super high tempos if there's too small a gap
        cutoff = floor $ nextPoint.time - 200 -- 200 ms for 1 beat gives 300 bpm
        initStream
          = takeWhile ((< cutoff) . fst)
          $ beatlineStream currentPoint
        in initStream <> makeStream nextPoint restPoints
      -- timing points can start at a negative position!
      -- so, discard beatlines before 0 and make sure there is one at 0.
      discardNegativeTimes = ((0, Bar) :) . dropWhile ((== 0) . fst) . dropWhile ((< 0) . fst)
      in discardNegativeTimes $ prePointsTime <> makeStream p ps

getOsuTiming :: OsuFile -> F.Song ()
getOsuTiming osu = let
  cutoffTime = 10000 + max
    (V.foldr (max . (.time)) 0 osu.hitObjects) -- maybe need to take sustain length into account?
    (V.foldr (max . floor . (.time)) 0 osu.timingPoints)
  beatlines = takeWhile ((< cutoffTime) . fst) $ getOsuBeatlines osu
  temposBPS = zipWith
    (\(t1, _) (t2, _) -> let
      secs = fromIntegral (t2 - t1) / 1000 :: U.Seconds
      in U.makeTempo (1 :: U.Beats) secs)
    beatlines
    (drop 1 beatlines)
  tempos = U.tempoMapFromBPS $ RTB.fromPairList $ zip (0 : repeat 1) temposBPS
  getBarLengths lns = case break ((== Bar) . snd) $ drop 1 lns of
    (thisBarThinLines, rest) -> case rest of
      []    -> []
      _ : _ -> (1 + length thisBarThinLines) : getBarLengths rest
  barLengths = map (fromIntegral :: Int -> U.Beats) $ getBarLengths beatlines
  sigs = U.measureMapFromLengths U.Truncate $ RTB.fromPairList $ zip
    (0 : barLengths)
    barLengths
  in F.Song tempos sigs ()
