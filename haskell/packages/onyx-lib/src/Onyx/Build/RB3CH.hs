{-
Shared logic used by the build processes for RB3 and CH targets.
(This should be refactored/split at some point...)
-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Build.RB3CH
( processRBPad, processRB, processPS, processTiming
, findProblems
, TrackAdjust(..)
, magmaLegalTempos
, basicTiming, BasicTiming(..)
, makeMoods
, buildDrums
) where

import           Control.Monad.Extra
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Trans.Writer.Strict (execWriter, tell)
import qualified Data.EventList.Absolute.TimeBody  as ATB
import qualified Data.EventList.Relative.TimeBody  as RTB
import           Data.Foldable                     (toList)
import           Data.List.Extra                   (nubOrd)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe, isNothing)
import qualified Data.Text                         as T
import qualified Numeric.NonNegative.Class         as NNC
import           Onyx.Difficulty
import           Onyx.Guitar
import           Onyx.Keys.Ranges
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read                    (mapTrack)
import           Onyx.MIDI.Track.Beat
import           Onyx.MIDI.Track.Drums             as Drums
import qualified Onyx.MIDI.Track.Drums.Elite       as ED
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File              as F
import           Onyx.MIDI.Track.FiveFret          as RBFive
import           Onyx.MIDI.Track.Mania             (maniaToDance)
import           Onyx.MIDI.Track.ProGuitar
import           Onyx.MIDI.Track.ProKeys
import           Onyx.MIDI.Track.Rocksmith
import           Onyx.MIDI.Track.SixFret
import           Onyx.MIDI.Track.Venue
import           Onyx.MIDI.Track.VenueGen          (buildCamera, buildLighting)
import           Onyx.MIDI.Track.Vocal
import qualified Onyx.MIDI.Track.Vocal.Legacy      as RBVox
import           Onyx.Mode
import           Onyx.Overdrive
import           Onyx.Project
import           Onyx.Reductions
import           Onyx.Sections                     (makeDisplaySection)
import           Onyx.StackTrace
import qualified Sound.MIDI.Util                   as U

data SharedTarget f
  = SharedTargetRB (TargetRB3 f) (Maybe (TargetRB2 f))
  | SharedTargetPS (TargetPS f)

processRBPad
  :: (SendMessage m, MonadIO m)
  => (TargetRB3 f, Maybe (TargetRB2 f))
  -> SongYaml f
  -> F.Song (F.OnyxFile U.Beats)
  -> Drums.Audio
  -> StackTraceT m U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> StackTraceT m (F.Song (F.FixedFile U.Beats), DifficultyRB3, Maybe VocalCount, Int)
processRBPad (a, b) c d e f = do
  (mid, diffs, vc) <- processMIDI (SharedTargetRB a b) c d e f
  -- TODO we probably should run fixBrokenUnisons before autoreductions
  legal <- if a.legalTempos then magmaLegalTemposFile mid else return mid
  (mid', pad) <- fixNotelessOD legal >>= fixBrokenUnisons >>= magmaPad . fixBeatTrack'
  return (proKeysODOnlyExpert mid', psDifficultyRB3 diffs, vc, pad)

processRB
  :: (SendMessage m, MonadIO m)
  => (TargetRB3 f, Maybe (TargetRB2 f))
  -> SongYaml f
  -> F.Song (F.OnyxFile U.Beats)
  -> Drums.Audio
  -> StackTraceT m U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> StackTraceT m (F.Song (F.FixedFile U.Beats), DifficultyRB3, Maybe VocalCount)
processRB (a, b) c d e f = do
  (mid, diffs, vc) <- processMIDI (SharedTargetRB a b) c d e f
  -- TODO we probably should run fixBrokenUnisons before autoreductions
  legal <- if a.legalTempos then magmaLegalTemposFile mid else return mid
  mid' <- fmap fixBeatTrack' $ fixNotelessOD legal >>= fixBrokenUnisons
  return (proKeysODOnlyExpert mid', psDifficultyRB3 diffs, vc)

processPS
  :: (SendMessage m, MonadIO m)
  => TargetPS f
  -> SongYaml f
  -> F.Song (F.OnyxFile U.Beats)
  -> Drums.Audio
  -> StackTraceT m U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> StackTraceT m (F.Song (F.FixedFile U.Beats), DifficultyPS, Maybe VocalCount)
processPS a b c d e = do
  (mid, diffs, vc) <- processMIDI (SharedTargetPS a) b c d e
  return (proKeysODAllDifficulties mid, diffs, vc)

proKeysODOnlyExpert :: F.Song (F.FixedFile U.Beats) -> F.Song (F.FixedFile U.Beats)
proKeysODOnlyExpert s = s
  { F.tracks = s.tracks
    { F.fixedPartRealKeysH = s.tracks.fixedPartRealKeysH { pkOverdrive = RTB.empty }
    , F.fixedPartRealKeysM = s.tracks.fixedPartRealKeysM { pkOverdrive = RTB.empty }
    , F.fixedPartRealKeysE = s.tracks.fixedPartRealKeysE { pkOverdrive = RTB.empty }
    }
  }

-- For Phase Shift, put pro keys overdrive on the lower difficulty tracks so they show up.
proKeysODAllDifficulties :: F.Song (F.FixedFile U.Beats) -> F.Song (F.FixedFile U.Beats)
proKeysODAllDifficulties s = s
  { F.tracks = s.tracks
    { F.fixedPartRealKeysH = s.tracks.fixedPartRealKeysH { pkOverdrive = od }
    , F.fixedPartRealKeysM = s.tracks.fixedPartRealKeysM { pkOverdrive = od }
    , F.fixedPartRealKeysE = s.tracks.fixedPartRealKeysE { pkOverdrive = od }
    }
  } where
    od = s.tracks.fixedPartRealKeysX.pkOverdrive

-- | Magma gets mad if you put an event like [idle_realtime] before 2 beats in.
-- But lots of Harmonix charts do this...
-- Note that actually 2 should be "2 BEAT events" but see fixBeatTrack for where
-- we ensure that the 3rd BEAT event is no later than 2 MIDI beats.
noEarlyMood :: RTB.T U.Beats Mood -> RTB.T U.Beats Mood
noEarlyMood rtb = case U.trackSplit 2 rtb of
  (early, later) -> case RTB.viewR early of
    Nothing          -> rtb -- no problem
    Just (_, (_, x)) -> case U.trackTakeZero later of
      []    -> RTB.cons 2 x later -- take the last mood before beat 2 and place it at 2
      _ : _ -> RTB.delay 2 later -- just drop early moods since a new one gets set at 2

data BasicTiming = BasicTiming
  { timingEnd        :: U.Beats
  , timingMusicStart :: U.Beats
  , timingMusicEnd   :: U.Beats
  , timingBeat       :: BeatTrack U.Beats
  } deriving (Show)

processTiming
  :: (SendMessage m)
  => F.Song (F.OnyxFile U.Beats)
  -> StackTraceT m U.Seconds
  -> StackTraceT m (F.Song (F.OnyxFile U.Beats))
processTiming input getAudioLength = do
  BasicTiming{..} <- basicTiming False input getAudioLength
  return input
    { F.tracks = input.tracks
      { F.onyxBeat = timingBeat
      , F.onyxEvents = input.tracks.onyxEvents
        { eventsEnd        = RTB.singleton timingEnd        ()
        , eventsMusicStart = RTB.singleton timingMusicStart ()
        , eventsMusicEnd   = RTB.singleton timingMusicEnd   ()
        }
      }
    }

-- | Retrieves or generates [end], [music_start], [music_end], and the BEAT track.
basicTiming
  :: (SendMessage m, F.HasEvents f, F.ParseFile f)
  => Bool
  -> F.Song (f U.Beats)
  -> StackTraceT m U.Seconds
  -> StackTraceT m BasicTiming
basicTiming shouldLog input@(F.Song tempos mmap trks) getAudioLength = do
  let showPosn = showPosition mmap
      logMaybe = if shouldLog then lg else const $ return ()
  -- If there's no @[end]@, put it after all MIDI events and audio files.
  timingEnd <- case RTB.viewL $ eventsEnd $ F.getEventsTrack trks of
    Just ((t, _), _) -> return t
    Nothing -> do
      audLen <- U.unapplyTempoMap tempos <$> getAudioLength
      let thirtySecs = U.unapplyTempoMap tempos (30.05 :: U.Seconds)
          absTimes = ATB.getTimes . RTB.toAbsoluteEventList 0
          lastMIDIEvent = foldr max 0
            $ concatMap absTimes (F.showMIDITracks input).tracks
            -- ++ absTimes (U.tempoMapToBPS tempos)
            -- (why did I also consider tempos here originally?
            -- maybe beacuse we weren't dropping tempos past [end])
          endPosn = fromInteger $ ceiling $ max thirtySecs $ max audLen lastMIDIEvent + 4
      logMaybe $ unwords
        [ "Placing [end] at " <> showPosn endPosn <> "."
        , "Last MIDI event is at " <> showPosn lastMIDIEvent <> ","
        , "longest audio file ends at " <> showPosn audLen <> ","
        , "minimum Magma length (30s) is " <> showPosn thirtySecs
        ]
      return endPosn
  timingBeat <- let
    trk = beatLines $ F.getBeatTrack trks
    in if RTB.null trk
      then do
        logMaybe "No BEAT track found; automatic one generated from time signatures."
        return $ BeatTrack $ U.trackTake timingEnd $ makeBeatTrack mmap
      else return $ BeatTrack trk
  -- If [music_start] is before 2 beats,
  -- Magma will add auto [idle] events there in instrument tracks, and then error...
  let musicStartMin = 2 :: U.Beats
  timingMusicStart <- case RTB.viewL $ eventsMusicStart $ F.getEventsTrack trks of
    Just ((t, _), _) -> if t < musicStartMin
      then do
        warn $ "[music_start] is too early. Moving to " ++ showPosn musicStartMin
        return musicStartMin
      else return t
    Nothing -> do
      logMaybe $ "[music_start] is missing. Placing at " ++ showPosn musicStartMin
      return musicStartMin
  timingMusicEnd <- case RTB.viewL $ eventsMusicEnd $ F.getEventsTrack trks of
    Just ((t, _), _) -> return t
    Nothing -> do
      logMaybe $ unwords
        [ "[music_end] is missing. [end] is at"
        , showPosn timingEnd
        , "so [music_end] will be at"
        , showPosn $ timingEnd - 2
        ]
      return $ timingEnd - 2
  return BasicTiming{..}

makeMoods' :: (NNC.C t) => t -> t -> RTB.T t (LongNote s a) -> RTB.T t Mood
makeMoods' buffer gap rtb = let
  edges = RTB.collectCoincident $ U.trackJoin $ flip fmap rtb $ \case
    Blip{}    -> Wait NNC.zero True  $ Wait buffer False RNil
    NoteOn{}  -> Wait NNC.zero True  RNil
    NoteOff{} -> Wait buffer   False RNil
  go held = \case
    RNil -> RNil
    Wait dt changes rest -> let
      held' = held + sum (map (\b -> if b then 1 else -1) changes)
      in if held == 0 && held' /= 0
        then Wait dt Mood_play $ go held' rest
        else if held /= 0 && held' == 0
          then case rest of
            RNil -> Wait dt Mood_idle_realtime RNil
            Wait dt' _ _ -> if dt' > gap
              then Wait dt Mood_idle $ go held' rest
              else RTB.delay dt $ go held' rest
          else RTB.delay dt $ go held' rest
  removeDupes = \case
    Wait tx x (Wait ty y rest) | x == y -> removeDupes $ Wait tx x $ RTB.delay ty rest
    Wait tx x rest                      -> Wait tx x $ removeDupes rest
    RNil                                -> RNil
  in removeDupes $ go (0 :: Int) edges

makeMoods :: U.TempoMap -> BasicTiming -> RTB.T U.Beats (LongNote s a) -> RTB.T U.Beats Mood
makeMoods tmap timing
  = U.trackTake (timingEnd timing)
  . U.unapplyTempoTrack tmap
  . makeMoods' (0.4 :: U.Seconds) 2
  . U.applyTempoTrack tmap

buildDrums
  :: F.FlexPartName
  -> SharedTarget f
  -> F.Song (F.OnyxFile U.Beats)
  -> BasicTiming
  -> SongYaml f
  -> Maybe (DrumTrack U.Beats, Maybe (ED.EliteDrumTrack U.Beats))
buildDrums drumsPart target (F.Song tempos mmap trks) timing songYaml = do
  bd <- getPart drumsPart songYaml >>= anyDrums

  let drumTarget = case target of
        SharedTargetRB rb3 _ -> if rb3.is2xBassPedal
          then DrumTargetRB2x
          else DrumTargetRB1x
        SharedTargetPS _ -> DrumTargetCH
      modeInput = ModeInput
        { tempo  = tempos
        , events = F.getEventsTrack trks
        , part   = F.getFlexPart drumsPart trks
        }
      drumResult = bd drumTarget modeInput
      src = drumResultToTrack drumResult

      sections = trks.onyxEvents.eventsSections

      drumEachDiff f dt = dt { drumDifficulties = fmap f dt.drumDifficulties }
      drumsRemoveBRE = case removeBRE target trks.onyxEvents of
        Just BRERemover{..}
          -> drumEachDiff (\dd -> dd { drumGems = breRemoveBlips dd.drumGems })
          . (\dt -> dt
            { drumSingleRoll = breRemoveLanes dt.drumSingleRoll
            , drumDoubleRoll = breRemoveLanes dt.drumDoubleRoll
            })
        Nothing -> id

      addMoods dt = dt
        { drumMood = noEarlyMood $ if RTB.null dt.drumMood
          then makeMoods tempos timing $ Blip () () <$ dt.drumAnimation
          else dt.drumMood
        }

      hasDynamics :: DrumTrack U.Beats -> Bool
      hasDynamics dt = flip any dt.drumDifficulties $ \dd ->
        flip any dd.drumGems $ \(_gem, vel) ->
          vel /= VelocityNormal
      enableDynamics dt = dt
        -- this will use the no-brackets version, so Magma is ok with it
        { drumEnableDynamics = if hasDynamics dt then RTB.singleton 0 () else RTB.empty
        }

      -- For better compatibility (Moonscraper, YARG) and a nicer looking MIDI output,
      -- we use the five_lane_drums format, which has RYBOG in order
      -- instead of RYBGO.
      flipGreenOrange = case (target, drumResult.settings.mode) of
        (SharedTargetPS{}, Drums5) -> drumEachDiff $ \dd -> dd
          { drumGems = flip fmap dd.drumGems $ \(gem, vel) -> let
            gem' = case gem of
              Drums.Pro Drums.Green () -> Drums.Orange
              Drums.Orange             -> Drums.Pro Drums.Green ()
              _                        -> gem
            in (gem', vel)
          }
        _ -> id

      mainResult
        = (\dt -> dt { drumPlayer1 = RTB.empty, drumPlayer2 = RTB.empty })
        $ flipGreenOrange
        $ drumsRemoveBRE
        $ addMoods
        $ enableDynamics
        $ drumsComplete mmap sections
        $ src

  return (mainResult, drumResult.eliteDrums)

data BRERemover t = BRERemover
  { breRemoveEdges :: forall s a. (Ord s, Ord a) => RTB.T t (Edge s a) -> RTB.T t (Edge s a)
  , breRemoveBools :: RTB.T t Bool -> RTB.T t Bool
  , breRemoveLanes :: RTB.T t (Maybe LaneDifficulty) -> RTB.T t (Maybe LaneDifficulty)
  , breRemoveBlips :: forall a. RTB.T t a -> RTB.T t a
  }

-- TODO make this more resilient, probably based on the "chop" functions
-- so it can correctly handle stuff beyond just removing notes
removeBRE :: (NNC.C t) => SharedTarget f -> EventsTrack t -> Maybe (BRERemover t)
removeBRE SharedTargetRB{} evts = case (eventsCoda evts, eventsCodaResume evts) of
  (Wait t1 () _, Wait t2 () _) -> Just $ let
    blips xs = trackGlue t2 (U.trackTake t1 xs) (U.trackDrop t2 xs)
    in BRERemover
      { breRemoveBlips = blips
      , breRemoveEdges = splitEdgesSimple . blips . joinEdgesSimple
      , breRemoveBools
        = fmap (\case EdgeOn () () -> True; EdgeOff () -> False)
        . splitEdgesSimple . blips . joinEdgesSimple
        . fmap (\b -> if b then EdgeOn () () else EdgeOff ())
      , breRemoveLanes
        = fmap (\case EdgeOn ld () -> Just ld; EdgeOff () -> Nothing)
        . splitEdgesSimple . blips . joinEdgesSimple
        . fmap (\case Just ld -> EdgeOn ld (); Nothing -> EdgeOff ())
      }
  _                            -> Nothing
removeBRE SharedTargetPS{} _ = Nothing

addFiveMoods
  :: U.TempoMap
  -> BasicTiming
  -> FiveTrack U.Beats
  -> FiveTrack U.Beats
addFiveMoods tempos timing ft = ft
  { fiveMood = noEarlyMood $ if RTB.null ft.fiveMood
    then makeMoods tempos timing $ let
      expert = fromMaybe mempty $ Map.lookup Expert ft.fiveDifficulties
      in splitEdges $ (\(_, len) -> ((), (), len)) <$> edgeBlips_ minSustainLengthRB expert.fiveGems
    else ft.fiveMood
  }

buildFive
  :: F.FlexPartName
  -> SharedTarget f
  -> F.Song (F.OnyxFile U.Beats)
  -> BasicTiming
  -> Bool
  -> SongYaml f
  -> Maybe (FiveTrack U.Beats)
buildFive fivePart target (F.Song tempos mmap trks) timing toKeys songYaml = case getPart fivePart songYaml >>= anyFiveFret of
  Nothing -> Nothing
  Just getFive -> Just $ let
    gtrType = case (target, toKeys) of
      (_               , True ) -> FiveTypeKeys
      (SharedTargetRB{}, False) -> FiveTypeGuitar
      (SharedTargetPS{}, False) -> FiveTypeGuitarExt
    result = completeFiveResult toKeys mmap $ getFive gtrType ModeInput
      { tempo  = tempos
      , events = trks.onyxEvents
      , part   = F.getFlexPart fivePart trks
      }
    gap = fromIntegral result.settings.sustainGap / 480
    breRemover = removeBRE target trks.onyxEvents
    makeDifficultyRB3
      = emit5'
      . fromClosed'
      . no5NoteChords
      . noOpenNotes result.settings.detectMutedOpens
      . noTaps
      . (if toKeys then id else noExtendedSustains' standardBlipThreshold gap)
      . maybe id (\x -> breRemoveBlips x) breRemover -- note: lambda needed for GHC 9+ (if DeepSubsumption not on) due to forall weirdness
    makeDifficultyPS = emit5'
    finalSteps
      = (if result.settings.fixFreeform then F.fixFreeformFive else id)
      . addFiveMoods tempos timing
    in finalSteps FiveTrack
      { fiveDifficulties = fmap
        (case target of SharedTargetRB{} -> makeDifficultyRB3; SharedTargetPS{} -> makeDifficultyPS)
        result.notes
      , fiveMood = result.other.fiveMood
      , fiveHandMap = result.other.fiveHandMap
      , fiveStrumMap = result.other.fiveStrumMap
      , fiveFretPosition = if result.settings.smoothFrets
        then U.unapplyTempoTrack tempos
          $ smoothFretPosition
          $ U.applyTempoTrack tempos
          $ result.other.fiveFretPosition
        else result.other.fiveFretPosition
      , fiveTremolo = maybe id breRemoveLanes breRemover result.other.fiveTremolo
      , fiveTrill = maybe id breRemoveLanes breRemover result.other.fiveTrill
      , fiveOverdrive = result.other.fiveOverdrive
      , fiveBRE = result.other.fiveBRE
      , fiveSolo = result.other.fiveSolo
      , fivePlayer1 = RTB.empty
      , fivePlayer2 = RTB.empty
      }

deleteBRE :: F.Song (F.OnyxFile U.Beats) -> F.Song (F.OnyxFile U.Beats)
deleteBRE song = song
  { F.tracks = song.tracks
    { F.onyxEvents = song.tracks.onyxEvents
      { eventsCoda       = RTB.empty
      , eventsCodaResume = RTB.empty
      }
    , F.onyxParts = flip fmap song.tracks.onyxParts $ \opart -> opart
      { F.onyxPartDrums        = deleteNormal opart.onyxPartDrums
      , F.onyxPartDrums2x      = deleteNormal opart.onyxPartDrums2x
      , F.onyxPartRealDrumsPS  = deleteNormal opart.onyxPartRealDrumsPS
      , F.onyxPartEliteDrums   = deleteElite  opart.onyxPartEliteDrums
      , F.onyxPartGuitar       = opart.onyxPartGuitar       { RBFive.fiveBRE = RTB.empty }
      , F.onyxPartKeys         = opart.onyxPartKeys         { RBFive.fiveBRE = RTB.empty }
      , F.onyxPartGuitarExt    = opart.onyxPartGuitarExt    { RBFive.fiveBRE = RTB.empty }
      , F.onyxPartRealGuitar   = opart.onyxPartRealGuitar   { pgBRE = RTB.empty }
      , F.onyxPartRealGuitar22 = opart.onyxPartRealGuitar22 { pgBRE = RTB.empty }
      , F.onyxPartRealKeysE    = opart.onyxPartRealKeysE    { pkBRE = RTB.empty }
      , F.onyxPartRealKeysM    = opart.onyxPartRealKeysM    { pkBRE = RTB.empty }
      , F.onyxPartRealKeysH    = opart.onyxPartRealKeysH    { pkBRE = RTB.empty }
      , F.onyxPartRealKeysX    = opart.onyxPartRealKeysX    { pkBRE = RTB.empty }
      }
    }
  } where
    applyCoda = case song.tracks.onyxEvents.eventsCoda of
      Wait t _ _ -> U.trackTake t
      RNil       -> id
    deleteNormal drums = drums
      { drumActivation = applyCoda drums.drumActivation
      }
    deleteElite fd = fd
      { ED.tdActivation = applyCoda fd.tdActivation
      }

processMIDI
  :: (SendMessage m, MonadIO m)
  => SharedTarget f
  -> SongYaml f
  -> F.Song (F.OnyxFile U.Beats)
  -> Drums.Audio
  -> StackTraceT m U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> StackTraceT m (F.Song (F.FixedFile U.Beats), DifficultyPS, Maybe VocalCount) -- ^ output midi, filtered difficulties, vocal count
processMIDI target songYaml origInput mixMode getAudioLength = inside "Processing MIDI for RB3/PS" $ do
  let input@(F.Song tempos mmap trks) = case target of
        SharedTargetPS ps | not $ ps.bigRockEnding -> deleteBRE origInput
        _                                          -> origInput
  timing@BasicTiming{..} <- basicTiming True input getAudioLength
  let targetPS = case target of
        SharedTargetPS ps    -> ps
        SharedTargetRB rb3 _ -> TargetPS
          { common        = rb3.common
          , guitar        = rb3.guitar
          , bass          = rb3.bass
          , drums         = rb3.drums
          , keys          = rb3.keys
          , vocal         = rb3.vocal
          , rhythm        = F.FlexExtra "undefined"
          , guitarCoop    = F.FlexExtra "undefined"
          , dance         = F.FlexExtra "undefined"
          , bigRockEnding = True
          , audioFormat   = "ogg"
          }
      originalRanks = difficultyPS targetPS songYaml

      eventsInput = trks.onyxEvents
      (crowd, crowdClap) = if RTB.null (eventsCrowd eventsInput) && RTB.null (eventsCrowdClap eventsInput)
        then
          ( RTB.singleton timingMusicStart CrowdRealtime
          , RTB.singleton timingMusicStart False
          )
        else (eventsCrowd eventsInput, eventsCrowdClap eventsInput)
      eventsTrack = mempty
        { eventsMusicStart = RTB.singleton timingMusicStart ()
        , eventsMusicEnd   = RTB.singleton timingMusicEnd ()
        , eventsEnd        = RTB.singleton timingEnd ()
        , eventsCoda       = eventsCoda eventsInput
        , eventsCrowd      = crowd
        , eventsCrowdClap  = crowdClap
        , eventsSections   = eventsSections eventsInput
        , eventsBacking    = if RTB.null (eventsBacking eventsInput) && not (nullDrums drumsTrack)
          then let
            makeBacking evts = nubOrd $ flip map evts $ \(gem, _vel) -> case gem of
              Drums.Kick               -> BackingKick
              Drums.Red                -> BackingSnare
              Drums.Pro _ Drums.Cymbal -> BackingHihat
              Drums.Pro _ Drums.Tom    -> BackingKick
              Drums.Orange             -> BackingHihat
            in RTB.flatten $ fmap makeBacking $ RTB.collectCoincident $ computePro (Just Expert) drumsTrack
          else eventsBacking eventsInput
        }
      eventsTrackPS = eventsTrack
        { eventsSections = makeDisplaySection <$> eventsSections eventsTrack
        }
      drumsPart = case target of
        SharedTargetRB rb3 _ -> rb3.drums
        SharedTargetPS ps    -> ps.drums
      (drumsTrack, eliteDrumsTrack) = case buildDrums drumsPart target input timing songYaml of
        Nothing        -> (mempty, Nothing)
        Just (dt, mtd) -> let
          -- remove nonstandardized parts of true drums spec
          mtd' = flip fmap mtd $ \td -> td
            { ED.tdSticking     = RTB.empty
            , ED.tdChipOverride = RTB.empty
            , ED.tdFooting      = RTB.empty
            }
          in (setDrumMix mixMode dt, mtd')
      makeGRYBOTrack toKeys fpart = fromMaybe mempty $ buildFive fpart target input timing toKeys songYaml

      guitarPart = case target of
        SharedTargetRB rb3 _ -> rb3.guitar
        SharedTargetPS ps    -> ps.guitar
      bassPart = case target of
        SharedTargetRB rb3 _ -> rb3.bass
        SharedTargetPS ps    -> ps.bass

      -- TODO: pgHopoThreshold
      makeProGtrTracks gtype fpart = case getPart fpart songYaml >>= (.proGuitar) of
        Nothing -> return (mempty, mempty)
        Just pg -> let
          src = F.getFlexPart fpart trks
          tuning = tuningPitches pg.tuning { gtrGlobal = 0 }
          applyType x = x
            { pgTrainer = (\(_, trainer) -> (gtype, trainer)) <$> x.pgTrainer
            , pgBRE     = (\(_, b      ) -> (gtype, b      )) <$> x.pgBRE
            }
          extendedTuning = length pitches > case gtype of
            TypeGuitar -> 6
            TypeBass   -> 4
          pitches = tuningPitches pg.tuning { gtrGlobal = 0 }
          defaultFlat = maybe False songKeyUsesFlats songYaml.metadata.key
          f = applyType
            . (if pg.fixFreeform then F.fixFreeformPG else id) . protarComplete
            . autoHandPosition . moveStrings
            . (if extendedTuning then freezeChordNames pitches defaultFlat else id)
            . autoChordRoot tuning
            . pgRemoveBRE
          pgRemoveBRE trk = case removeBRE target trks.onyxEvents of
            Just BRERemover{..} -> trk
              { pgDifficulties = flip fmap trk.pgDifficulties $ \pgd -> pgd
                { pgNotes = breRemoveEdges pgd.pgNotes
                }
              , pgTremolo = breRemoveLanes trk.pgTremolo
              , pgTrill = breRemoveLanes trk.pgTrill
              }
            Nothing -> trk
          src17  = src.onyxPartRealGuitar
          src22  = src.onyxPartRealGuitar22
          srcRSG = src.onyxPartRSGuitar
          srcRSB = src.onyxPartRSBass
          in do
            let src22' = case (nullPG src22 && nullPG src17, nullRS srcRSG, nullRS srcRSB) of
                  (True, False, _    ) -> convertRStoPG tempos srcRSG
                  (True, True , False) -> convertRStoPG tempos srcRSB
                  (_   , _    , _    ) -> src22
            let mustang = f $ fretLimit 17 $ if nullPG src17  then src22' else src17
                squier  = f $ fretLimit 22 $ if nullPG src22' then src17  else src22'
            return (mustang, if mustang == squier then mempty else squier)

  (proGtr , proGtr22 ) <- makeProGtrTracks TypeGuitar guitarPart
  (proBass, proBass22) <- makeProGtrTracks TypeBass   bassPart

  let guitar = makeGRYBOTrack False guitarPart
      bass   = makeGRYBOTrack False bassPart

      rhythmPart = case target of
        SharedTargetRB {} -> F.FlexExtra "undefined"
        SharedTargetPS ps -> ps.rhythm
      rhythmPS = makeGRYBOTrack False rhythmPart

      guitarCoopPart = case target of
        SharedTargetRB {} -> F.FlexExtra "undefined"
        SharedTargetPS ps -> ps.guitarCoop
      guitarCoopPS = makeGRYBOTrack False guitarCoopPart

      sixEachDiff f st = st
        { sixDifficulties = fmap f $ sixDifficulties st
        }
      guitarGHL = case getPart guitarPart songYaml >>= (.ghl) of
        Nothing  -> mempty
        Just ghl -> sixEachDiff
          ( \sd ->
            emit6'
          . applyForces (getForces6 sd)
          . strumHOPOTap HOPOsRBGuitar (fromIntegral ghl.hopoThreshold / 480)
          . edgeBlips_ minSustainLengthRB
          $ sixGems sd
          ) (F.getFlexPart guitarPart trks).onyxPartSix
      bassGHL = case getPart bassPart songYaml >>= (.ghl) of
        Nothing  -> mempty
        Just ghl -> sixEachDiff
          ( \sd ->
            emit6'
          . applyForces (getForces6 sd)
          . strumHOPOTap HOPOsRBGuitar (fromIntegral ghl.hopoThreshold / 480)
          . edgeBlips_ minSustainLengthRB
          $ sixGems sd
          ) (F.getFlexPart bassPart trks).onyxPartSix

      keysPart = case target of
        SharedTargetRB rb3 _ -> rb3.keys
        SharedTargetPS ps    -> ps.keys
      (tk, tkRH, tkLH, tpkX, tpkH, tpkM, tpkE) = case getPart keysPart songYaml of
        Nothing -> (mempty, mempty, mempty, mempty, mempty, mempty, mempty)
        Just part -> if isNothing (anyFiveFret part) && isNothing (anyProKeys part)
          then (mempty, mempty, mempty, mempty, mempty, mempty, mempty)
          else let
            basicKeys = makeGRYBOTrack True keysPart
            fpart = F.getFlexPart keysPart trks
            keysDiff diff = case anyProKeys part of
              Just builder -> let
                result = builder ModeInput
                  { tempo  = tempos
                  , events = F.getEventsTrack trks
                  , part   = F.getFlexPart keysPart trks
                  }
                in fromMaybe mempty $ Map.lookup diff result.difficulties
              Nothing -> F.keysToProKeys diff basicKeys
            pkd1 `orIfNull` pkd2 = if length (pkNotes pkd1) < 5 then pkd2 else pkd1
            eachPKDiff = ffPro . fixPSRange . fixPKMood . completeRanges
              . (case removeBRE target trks.onyxEvents of
                Just BRERemover{..} -> \pk -> pk
                  { pkNotes     = breRemoveEdges $ pkNotes     pk
                  , pkGlissando = breRemoveBools $ pkGlissando pk
                  , pkTrill     = breRemoveBools $ pkTrill     pk
                  }
                Nothing -> id)
            keysExpert = eachPKDiff $ keysDiff Expert
            keysHard   = eachPKDiff $ keysDiff Hard   `orIfNull` pkReduce Hard   mmap keysOD keysExpert
            keysMedium = eachPKDiff $ keysDiff Medium `orIfNull` pkReduce Medium mmap keysOD keysHard
            keysEasy   = eachPKDiff $ keysDiff Easy   `orIfNull` pkReduce Easy   mmap keysOD keysMedium
            fixPKMood x = x { pkMood = noEarlyMood $ pkMood x }
            keysOD = pkOverdrive keysExpert
            originalRH = fpart.onyxPartKeysAnimRH
            originalLH = fpart.onyxPartKeysAnimLH
            (animRH, animLH) = if nullPK originalRH && nullPK originalLH
              then (mempty { pkNotes = pkNotes keysExpert }, mempty)
              else (originalRH, originalLH)
            ffBasic = if fmap (.fixFreeform) part.grybo == Just True
              then F.fixFreeformFive
              else id
            ffPro = if fmap (.fixFreeform) part.proKeys == Just True
              then F.fixFreeformPK
              else id
            -- nemo's checker doesn't like if you include this stuff on PART KEYS
            removeGtrStuff ft = ft
              { fiveFretPosition = RTB.empty
              , fiveHandMap = RTB.empty
              , fiveStrumMap = RTB.empty
              , fiveTremolo = RTB.empty
              , fiveDifficulties = flip Map.mapWithKey ft.fiveDifficulties $ \diff fd ->
                if elem diff [Easy, Medium]
                  then fd { fiveForceStrum = RTB.empty, fiveForceHOPO = RTB.empty }
                  else fd
              }
            in  ( ffBasic $ removeGtrStuff basicKeys
                , animRH
                , animLH
                , keysExpert
                , keysHard
                , keysMedium
                , keysEasy
                )

  let vocalPart = case target of
        SharedTargetRB rb3 _ -> rb3.vocal
        SharedTargetPS ps    -> ps.vocal
      voxCount = fmap (.count) $ getPart vocalPart songYaml >>= (.vocal)
      partVox = (F.getFlexPart vocalPart trks).onyxPartVocals
      harm1   = (F.getFlexPart vocalPart trks).onyxHarm1
      harm2   = (F.getFlexPart vocalPart trks).onyxHarm2
      harm3   = (F.getFlexPart vocalPart trks).onyxHarm3
      someV `withNotesOf` otherV = someV
        { vocalLyrics = vocalLyrics otherV
        , vocalNotes = vocalNotes otherV
        }
      makeSoloVocals harm =
        (Just Vocal1, if nullVox partVox then harm else partVox, mempty, mempty, mempty)
  (editCount, trkPV, trkHarm1, trkHarm2, trkHarm3) <- case voxCount of
    Nothing -> return (Nothing, mempty, mempty, mempty, mempty)
    Just Vocal1 -> case (nullVox partVox, nullVox harm1) of
      (False, _) -> return (Just Vocal1, partVox, mempty, mempty, mempty)
      (True, False) -> return (Just Vocal1, harm1, mempty, mempty, mempty)
      (True, True) -> do
        lg "Vocals (1) is empty (both PART VOCALS and HARM1); disabling"
        return (Nothing, mempty, mempty, mempty, mempty)
    Just Vocal2 -> case (nullVox harm1, nullVox harm2) of
      (False, False) -> return (Just Vocal2, partVox, harm1, harm2, mempty)
      (False, True) -> do
        lg "Vocals (2) has empty HARM2; switching to solo vocals"
        return $ makeSoloVocals harm1
      (True, False) -> do
        lg "Vocals (2) has empty HARM1; switching HARM2 to solo vocals"
        return $ makeSoloVocals $ harm1 `withNotesOf` harm2
      (True, True) -> do
        lg "Vocals (2) is empty; disabling"
        return (Nothing, mempty, mempty, mempty, mempty)
    Just Vocal3 -> case (nullVox harm1, nullVox harm2, nullVox harm3) of
      (False, False, False) -> return (Just Vocal3, partVox, harm1, harm2, harm3)
      (False, False, True) -> do
        lg "Vocals (3) has empty HARM2; switching to 2 harmonies"
        return (Just Vocal2, partVox, harm1, harm2, mempty)
      (False, True, False) -> do
        lg "Vocals (3) has empty HARM3; switching to 2 harmonies"
        return (Just Vocal2, partVox, harm1, harm2 `withNotesOf` harm3, mempty)
      (True, False, False) -> do
        lg "Vocals (3) has empty HARM1; switching to 2 harmonies"
        return (Just Vocal2, partVox, harm1 `withNotesOf` harm2, harm2 `withNotesOf` harm3, mempty)
      (False, True, True) -> do
        lg "Vocals (3) only has notes in HARM1; switching to solo vocals"
        return $ makeSoloVocals harm1
      (True, False, True) -> do
        lg "Vocals (3) only has notes in HARM2; switching to solo vocals"
        return $ makeSoloVocals $ harm1 `withNotesOf` harm2
      (True, True, False) -> do
        lg "Vocals (3) only has notes in HARM3; switching to solo vocals"
        return $ makeSoloVocals $ harm1 `withNotesOf` harm3
      (True, True, True) -> do
        lg "Vocals (3) is empty; disabling"
        return (Nothing, mempty, mempty, mempty, mempty)
  let trkVox = if editCount >= Just Vocal2 && nullVox trkPV
        then RBVox.harm1ToPartVocals trkHarm1
        else trkPV
      autoVoxMood vox = makeMoods tempos timing $ flip fmap (vocalNotes vox) $ \case
        (_, True)  -> NoteOn () ()
        (_, False) -> NoteOff ()
      trkVox'   = trkVox
        { vocalMood = noEarlyMood $ if RTB.null $ vocalMood trkVox
          then case voxCount of
            Nothing     -> RTB.empty
            Just Vocal1 -> autoVoxMood trkVox
            Just _      -> autoVoxMood trkHarm1
          else vocalMood trkVox
        }
      trkHarm1' = trkHarm1 { vocalMood = RTB.empty }
      trkHarm2' = trkHarm2 { vocalMood = RTB.empty }
      trkHarm3' = trkHarm3 { vocalMood = RTB.empty }
      -- we used to do asciiLyrics here for PS, but removing now since we can give UTF-8 midis to CH
      voxPSCH = hashTalkies
      -- CH lyrics don't handle ^ correctly, so we replace with the standard #.
      -- specifically "word-^" results in the hyphen being shown.
      hashTalkies vt = vt { vocalLyrics = T.replace "^" "#" <$> vocalLyrics vt }

  drumsTrack' <- let
    fills = RTB.normalize drumsTrack.drumActivation
    fixCloseFills rtb = case RTB.viewL rtb of
      Just ((tx, True), rtb') -> case RTB.viewL rtb' of
        Just ((ty, False), rtb'')
          | tx < 2.5
          -> fixCloseFills $ RTB.delay (tx + ty) rtb''
        _ -> RTB.cons tx True $ fixCloseFills rtb'
      Just ((tx, False), rtb') -> RTB.cons tx False $ fixCloseFills rtb'
      Nothing -> RTB.empty
    fixedFills = U.unapplyTempoTrack tempos $ fixCloseFills $ U.applyTempoTrack tempos fills
    in do
      when (fills /= fixedFills) $ warn
        "Removing some drum fills because they are too close (within 2.5 seconds)"
      return drumsTrack { drumActivation = fixedFills }

  -- remove tempo and time signature events on or after [end].
  -- found in some of bluzer's RB->PS converts. magma complains
  let tempos'
        = U.tempoMapFromBPS
        $ U.trackTake timingEnd
        $ U.tempoMapToBPS tempos
      mmap'
        = U.measureMapFromTimeSigs U.Error
        $ U.trackTake timingEnd
        $ U.measureMapToTimeSigs mmap

  let partsForVenue = concat
        [ [Guitar | not $ nullFive  guitar     ]
        , [Bass   | not $ nullFive  bass       ]
        , [Drums  | not $ nullDrums drumsTrack']
        , [Keys   | not $ nullFive  tk         ]
        , [Vocal  | not $ nullVox   trkVox'    ]
        ]
  camera <- stackIO $ buildCamera partsForVenue trks.onyxCamera
  let isPS = case target of SharedTargetRB{} -> False; SharedTargetPS{} -> True
      compileVenue = case target of
        -- previously I had a trackTake here, because
        -- "otherwise new blips introduced in rb3->rb2 can go past the end event"
        -- now replaced with chopTake, but I don't think it actually prevents
        -- blip note-off from going past [end]?
        SharedTargetRB _rb3 (Just _rb2) -> chopTake timingEnd . compileVenueRB2
        _                               -> compileVenueRB3
      venue = compileVenue $ mconcat
        [ trks.onyxVenue
        , buildLighting trks.onyxLighting
        , camera
        ]
      editRanks = case editCount of
        Nothing -> originalRanks
          { psDifficultyRB3 = (psDifficultyRB3 originalRanks)
            { rb3VocalRank = 0
            , rb3VocalTier = 0
            }
          }
        Just _ -> originalRanks
      dance = case target of
        SharedTargetRB {} -> mempty
        SharedTargetPS ps -> let
          fpart = ps.dance
          in case getPart fpart songYaml >>= (.mania) of
            Nothing -> mempty
            Just pm -> if pm.keys <= 4
              then let
                -- TODO if the difficulties are named right, map them as is
                diffMapping = zip
                  (reverse [minBound .. maxBound])
                  (reverse $ toList pm.charts)
                in maniaToDance diffMapping (F.getFlexPart fpart trks).onyxPartMania
              else mempty -- TODO autochart down!
  return (F.Song tempos' mmap' F.FixedFile
    { F.fixedBeat = timingBeat
    , F.fixedEvents = if isPS then eventsTrackPS else eventsTrack
    , F.fixedVenue = venue
    , F.fixedPartDrums = drumsTrack'
    , F.fixedPartDrums2x = mempty
    , F.fixedPartRealDrumsPS = mempty
    , F.fixedPartEliteDrums = if isPS then fromMaybe mempty eliteDrumsTrack else mempty
    , F.fixedPartGuitar = guitar
    , F.fixedPartGuitarGHL = if isPS then guitarGHL else mempty
    , F.fixedPartBass = bass
    , F.fixedPartBassGHL = if isPS then bassGHL else mempty
    , F.fixedPartRhythm = if isPS then rhythmPS else mempty
    , F.fixedPartGuitarCoop = if isPS then guitarCoopPS else mempty
    , F.fixedPartRealGuitar   = proGtr
    , F.fixedPartRealGuitar22 = proGtr22
    , F.fixedPartRealBass     = proBass
    , F.fixedPartRealBass22   = proBass22
    , F.fixedPartKeys = tk
    , F.fixedPartKeysAnimRH = tkRH
    , F.fixedPartKeysAnimLH = tkLH
    , F.fixedPartRealKeysE = tpkE
    , F.fixedPartRealKeysM = tpkM
    , F.fixedPartRealKeysH = tpkH
    , F.fixedPartRealKeysX = tpkX
    , F.fixedPartVocals = (if isPS then voxPSCH else id) trkVox'
    , F.fixedHarm1 = (if isPS then voxPSCH else id) trkHarm1'
    , F.fixedHarm2 = (if isPS then voxPSCH else id) trkHarm2'
    , F.fixedHarm3 = (if isPS then voxPSCH else id) trkHarm3'
    , F.fixedPartDance = if isPS then dance else mempty
    , F.fixedLipsync1 = mempty
    , F.fixedLipsync2 = mempty
    , F.fixedLipsync3 = mempty
    , F.fixedLipsync4 = mempty
    , F.fixedLipsyncJohn = mempty
    , F.fixedLipsyncPaul = mempty
    , F.fixedLipsyncGeorge = mempty
    , F.fixedLipsyncRingo = mempty
    }, editRanks, editCount)

magmaLegalTemposFile :: (SendMessage m) => F.Song (F.FixedFile U.Beats) -> StackTraceT m (F.Song (F.FixedFile U.Beats))
magmaLegalTemposFile rb3 = let
  endTime = case RTB.viewL rb3.tracks.fixedEvents.eventsEnd of
    Nothing           -> 0 -- shouldn't happen
    Just ((dt, _), _) -> dt
  in magmaLegalTempos endTime rb3.tempos rb3.timesigs >>= \case
    (Nothing, _, _, _) -> return rb3
    (Just msg, newTempos, newSigs, TrackAdjust adjuster) -> do
      warn msg
      return $ F.Song newTempos newSigs $ mapTrack adjuster rb3.tracks

newtype TrackAdjust = TrackAdjust (forall a. RTB.T U.Beats a -> RTB.T U.Beats a)

magmaLegalTempos :: (SendMessage m) => U.Beats -> U.TempoMap -> U.MeasureMap -> StackTraceT m
  ( Maybe String
  , U.TempoMap
  , U.MeasureMap
  , TrackAdjust -- adjusts each event track
  )
magmaLegalTempos endTime tmap mmap = do
  res <- errorToWarning $ magmaLegalTemposNoWipe endTime tmap mmap
  return $ case res of
    Nothing -> let
      tmap' = U.tempoMapFromBPS $ RTB.singleton 0 4 -- 240 bpm, reasonably fast
      in  ( Just "Wiping tempo map due to unfixable tempo changes, beat lines might be weird!"
          , tmap'
          , U.measureMapFromTimeSigs U.Ignore $ RTB.singleton 0 $ U.TimeSig 4 1
          , TrackAdjust $ U.unapplyTempoTrack tmap' . U.applyTempoTrack tmap
          )
    Just (n, tmap', mmap', adjust) ->
      ( case n of
        0 -> Nothing
        _ -> Just $ "Stretching/squashing " ++ show n ++ " measures to keep tempos in Magma-legal range"
      , tmap', mmap', adjust
      )

{-
if a measure has a <40bpm tempo:
- double all tempos inside
- stretch it out to twice the length, moving events appropriately
- bump up the numerator of the measure's time signature by 2, so e.g. 5/4 becomes 10/4
if a measure has a >300bpm tempo:
- halve all tempos inside
- squish it to half the length, moving events appropriately
- bump up the denominator of the measure's time signature by 2, so e.g. 5/4 becomes 5/8
-}
magmaLegalTemposNoWipe :: (Monad m) => U.Beats -> U.TempoMap -> U.MeasureMap -> StackTraceT m
  ( Int -- num of measures adjusted
  , U.TempoMap
  , U.MeasureMap
  , TrackAdjust -- adjusts each event track
  )
magmaLegalTemposNoWipe endTime tempos sigs = do
  let allTempos = U.tempoMapToBPS tempos
      allSigs = U.measureMapToTimeSigs sigs
      numMeasures = fst (U.applyMeasureMap sigs endTime) + 1
      minTempo =  40 / 60 :: U.BPS
      maxTempo = 300 / 60 :: U.BPS
  measureChanges <- forM [0 .. numMeasures - 1] $ \msr -> do
    let msrStart = U.unapplyMeasureMap sigs (msr, 0)
        msrLength = U.unapplyMeasureMap sigs (msr + 1, 0) - msrStart
        msrEvents = U.trackTake msrLength $ U.trackDrop msrStart allTempos
        initialTempo = case RTB.viewL msrEvents of
          Just ((0, _), _) -> id
          _ -> case RTB.viewR $ U.trackTake msrStart allTempos of
            Nothing            -> id
            Just (_, (_, bps)) -> (bps :)
        msrTempos = initialTempo $ RTB.getBodies msrEvents
        go :: (Monad m) => Int -> U.BPS -> U.BPS -> StackTraceT m Int
        go currentPower2 slowest fastest
          | slowest < minTempo = if maxTempo / 2 < fastest
            then fatal $ "Can't make measure " ++ show msr ++ " tempos Magma-legal"
            else go (currentPower2 + 1) (slowest * 2) (fastest * 2)
          | maxTempo < fastest = if slowest < minTempo * 2
            then fatal $ "Can't make measure " ++ show msr ++ " tempos Magma-legal"
            else go (currentPower2 - 1) (slowest / 2) (fastest / 2)
          | otherwise = return currentPower2
    change <- go 0 (minimum msrTempos) (maximum msrTempos)
    return (msrLength, change)
  let numChanges = length $ filter ((/= 0) . snd) measureChanges
  if numChanges == 0
    then return (0, tempos, sigs, TrackAdjust id)
    else let
      stretchTrack [] trk = trk
      stretchTrack ((len, change) : changes) trk = let
        (msr, rest) = U.trackSplit len trk
        stretch = (* (2 ^^ change))
        in trackGlue (stretch len) (RTB.mapTime stretch msr)
          $ stretchTrack changes rest
      stretchTempos [] _ trk = trk
      stretchTempos ((len, change) : changes) tempo trk = let
        (msr, rest) = U.trackSplit len trk
        stretch :: (Fractional a) => a -> a
        stretch = (* (2 ^^ change))
        tempo' = case RTB.viewR msr of
          Just (_, (_, tmp)) -> tmp
          Nothing            -> tempo
        msr' = case RTB.viewL msr of
          Just ((0, _), _) -> msr
          _                -> RTB.cons 0 tempo msr
        in trackGlue (stretch len) (RTB.mapTime stretch $ RTB.mapBody stretch msr')
          $ stretchTempos changes tempo' rest
      stretchSigs [] _ trk = trk
      stretchSigs ((len, change) : changes) sig trk = let
        (msr, rest) = U.trackSplit len trk
        stretch :: (Fractional a) => a -> a
        stretch = (* (2 ^^ change))
        stretchSig tsig = case compare change 0 of
          EQ -> tsig
          GT -> tsig { U.timeSigLength = stretch $ U.timeSigLength tsig }
          LT -> U.TimeSig
            { U.timeSigLength = stretch $ U.timeSigLength tsig
            , U.timeSigUnit   = stretch $ U.timeSigUnit   tsig
            }
        sig' = case U.trackTakeZero msr of
          s : _ -> s
          []    -> sig
        in trackGlue (stretch len) (RTB.singleton 0 $ stretchSig sig')
          $ stretchSigs changes sig' rest
      in return
        ( numChanges
        , U.tempoMapFromBPS $ stretchTempos measureChanges 2 allTempos
        , U.measureMapFromTimeSigs U.Truncate $ stretchSigs measureChanges (U.TimeSig 4 1) allSigs
        , TrackAdjust $ stretchTrack measureChanges
        )

fixBeatTrack' :: F.Song (F.FixedFile U.Beats) -> F.Song (F.FixedFile U.Beats)
fixBeatTrack' rb3 = let
  endTime = case RTB.viewL rb3.tracks.fixedEvents.eventsEnd of
    Nothing           -> 0 -- shouldn't happen
    Just ((dt, _), _) -> dt
  in rb3
    { F.tracks = rb3.tracks
      { F.fixedBeat = fixBeatTrack endTime rb3.tracks.fixedBeat
      }
    }

fixBeatTrack :: U.Beats -> BeatTrack U.Beats -> BeatTrack U.Beats
fixBeatTrack endPosn = let
  -- can't have 2 barlines in a row
  fixDoubleDownbeat = RTB.fromPairList . fixDoubleDownbeat' . RTB.toPairList
  fixDoubleDownbeat' = \case
    (t1, Bar) : rest@((_, Bar) : _)
      -> (t1, Beat) : fixDoubleDownbeat' rest
    (t, x) : rest -> (t, x) : fixDoubleDownbeat' rest
    [] -> []
  -- can't have less than 240 ticks between beats ("faster than double time")
  fixFastBeats rtb = case RTB.viewL rtb of
    Just ((dt, b), rtb') -> if dt < (240/480) && dt /= 0
      then fixFastBeats $ RTB.delay dt rtb'
      else RTB.cons dt b $ fixFastBeats rtb'
    Nothing -> rtb
  -- last beat must be no more than 480, and no less than 15, ticks from [end]
  fixLastBeat rtb = if RTB.null $ U.trackDrop (endPosn - (480/480)) rtb
    then RTB.insert (endPosn - (240/480)) Beat rtb
    else rtb
  -- this fixes weird errors where Magma says seemingly wrong things about moods.
  -- * when magma says a mood event is "less than 2 beats from the beginning",
  --   that actually means "before the 3rd BEAT event".
  -- * but, magma also "sees" [idle] events 2 quarter notes in for some reason,
  --   so in practice the 3rd BEAT event has to be no later than that.
  fixFirst2Beats rtb = if sum (take 3 $ RTB.getTimes rtb) <= 2
    then rtb
    else RTB.cons 0 Bar $ RTB.cons 1 Beat $ RTB.cons 1 Beat $ RTB.delay 0.5 $ U.trackDrop 2.5 rtb
  in BeatTrack . fixFirst2Beats . fixDoubleDownbeat . fixLastBeat . fixFastBeats . U.trackTake (endPosn - (14/480)) . beatLines

magmaPad
  :: (SendMessage m)
  => F.Song (F.FixedFile U.Beats)
  -> StackTraceT m (F.Song (F.FixedFile U.Beats), Int)
magmaPad rb3@(F.Song tmap _ trks) = let
  firstEvent rtb = case RTB.viewL rtb of
    Just ((dt, _), _) -> dt
    Nothing           -> 999
  firstNoteBeats = foldr min 999 $ concat
    [ map (firstEvent . (.drumGems)) $ Map.elems trks.fixedPartDrums.drumDifficulties
    , map (firstEvent . (.fiveGems)) $ Map.elems trks.fixedPartGuitar.fiveDifficulties
    , map (firstEvent . (.fiveGems)) $ Map.elems trks.fixedPartBass.fiveDifficulties
    , map (firstEvent . (.fiveGems)) $ Map.elems trks.fixedPartKeys.fiveDifficulties
    , map (firstEvent . (.pgNotes )) $ Map.elems trks.fixedPartRealGuitar.pgDifficulties
    , map (firstEvent . (.pgNotes )) $ Map.elems trks.fixedPartRealGuitar22.pgDifficulties
    , map (firstEvent . (.pgNotes )) $ Map.elems trks.fixedPartRealBass.pgDifficulties
    , map (firstEvent . (.pgNotes )) $ Map.elems trks.fixedPartRealBass22.pgDifficulties
    , map (firstEvent . (.pkNotes ))
      [ trks.fixedPartRealKeysE
      , trks.fixedPartRealKeysM
      , trks.fixedPartRealKeysH
      , trks.fixedPartRealKeysX
      ]
    , map (firstEvent . (.vocalNotes))
      [ trks.fixedPartVocals
      , trks.fixedHarm1
      , trks.fixedHarm2
      , trks.fixedHarm3
      ]
    ]
  firstNoteSeconds = U.applyTempoMap tmap firstNoteBeats
  -- magma says 2.45s but even 2.571 can cause problems on non-bns expert!
  padSeconds = max 0 $ ceiling $ 2.6 - (realToFrac firstNoteSeconds :: Rational)
  in case padSeconds of
    0 -> do
      return (rb3, 0)
    _ -> do
      warn $ "Padding song by " ++ show padSeconds ++ "s due to early notes."
      return (F.padFixedFile padSeconds rb3, padSeconds)

findProblems :: F.Song (F.OnyxFile U.Beats) -> [String]
findProblems song = execWriter $ do
  -- Every discobeat mix event should be simultaneous with,
  -- or immediately followed by, a set of notes not including red or yellow.
  let drums = (F.getFlexPart F.FlexDrums song.tracks).onyxPartDrums
      discos = foldr RTB.merge RTB.empty $ do
        d <- [minBound .. maxBound]
        let diff = fromMaybe mempty $ Map.lookup d drums.drumDifficulties
        return $ flip RTB.mapMaybe diff.drumMix $ \case
          (_, Drums.Disco) -> Just d
          _                -> Nothing
      badDiscos = void $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ filter isBadDisco $ ATB.toPairList $ RTB.toAbsoluteEventList 0 discos
      drumsDiff d = (fromMaybe mempty $ Map.lookup d drums.drumDifficulties).drumGems
      isBadDisco (t, diff) = case RTB.viewL $ RTB.collectCoincident $ U.trackDrop t $ drumsDiff diff of
        Just ((_, evts), _) | any isDiscoGem evts -> True
        _                                         -> False
      isDiscoGem = \case
        (Drums.Red               , _) -> True
        (Drums.Pro Drums.Yellow _, _) -> True
        _                             -> False
  -- Don't have a vocal phrase that ends simultaneous with a lyric event.
  -- In static vocals, this puts the lyric in the wrong phrase.
  let vox   = RBVox.vocalToLegacy $ (F.getFlexPart F.FlexVocal song.tracks).onyxPartVocals
      harm1 = RBVox.vocalToLegacy $ (F.getFlexPart F.FlexVocal song.tracks).onyxHarm1
      harm2 = RBVox.vocalToLegacy $ (F.getFlexPart F.FlexVocal song.tracks).onyxHarm2
      harm3 = RBVox.vocalToLegacy $ (F.getFlexPart F.FlexVocal song.tracks).onyxHarm3
      phraseOff = RBVox.Phrase False
      isLyric = \case RBVox.Lyric _ -> True; _ -> False
      voxBugs = flip RTB.mapMaybe (RTB.collectCoincident vox) $ \evts -> do
        guard $ elem phraseOff evts && any isLyric evts
        return ()
      harm1Bugs = flip RTB.mapMaybe (RTB.collectCoincident harm1) $ \evts -> do
        guard $ elem phraseOff evts && any isLyric evts
        return ()
      harm2Bugs = flip RTB.mapMaybe (RTB.collectCoincident $ RTB.merge harm2 harm3) $ \evts -> do
        guard $ elem phraseOff evts && any isLyric evts
        return ()
  -- Put it all together and show the error positions.
  let showPositions :: RTB.T U.Beats () -> [String]
      showPositions
        = map (showPosition song.timesigs)
        . ATB.getTimes
        . RTB.toAbsoluteEventList 0
      message rtb msg = forM_ (showPositions rtb) $ \pos ->
        tell [pos ++ ": " ++ msg]
  message badDiscos "discobeat drum event is followed immediately by red or yellow gem"
  message voxBugs "PART VOCALS vocal phrase ends simultaneous with a lyric"
  message harm1Bugs "HARM1 vocal phrase ends simultaneous with a lyric"
  message harm2Bugs "HARM2 vocal phrase ends simultaneous with a (HARM2 or HARM3) lyric"
