{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand3
( processRB3Pad, processPS, processTiming
, findProblems
, TrackAdjust(..)
, magmaLegalTempos
, basicTiming, BasicTiming(..)
) where

import           Config
import           Control.Monad.Extra
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer.Strict (execWriter, tell)
import qualified Data.EventList.Absolute.TimeBody  as ATB
import qualified Data.EventList.Relative.TimeBody  as RTB
import           Data.List.Extra                   (nubOrd)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe, isJust)
import qualified Data.Text                         as T
import           Difficulty
import           Guitars
import qualified Numeric.NonNegative.Class         as NNC
import           OneFoot
import           Overdrive
import           ProKeysRanges
import           Reductions
import           RockBand.Codec                    (mapTrack)
import           RockBand.Codec.Beat
import           RockBand.Codec.Drums              as RBDrums
import           RockBand.Codec.Events
import qualified RockBand.Codec.File               as RBFile
import           RockBand.Codec.Five
import           RockBand.Codec.ProGuitar
import           RockBand.Codec.ProKeys
import           RockBand.Codec.Six
import           RockBand.Codec.Venue
import           RockBand.Codec.VenueGen           (buildCamera, buildLighting)
import           RockBand.Codec.Vocal
import           RockBand.Common
import qualified RockBand.Legacy.Vocal             as RBVox
import           RockBand.Sections                 (makePSSection)
import           Rocksmith.MIDI
import qualified Sound.MIDI.Util                   as U

processRB3Pad
  :: (SendMessage m, MonadIO m)
  => TargetRB3 f
  -> SongYaml f
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> StackTraceT m U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> StackTraceT m (RBFile.Song (RBFile.FixedFile U.Beats), DifficultyRB3, Maybe VocalCount, Int)
processRB3Pad a b c d e = do
  (mid, diffs, vc) <- processMIDI (Left a) b c d e
  -- TODO we probably should run fixBrokenUnisons before autoreductions
  (mid', pad) <- magmaLegalTemposFile mid >>= fixNotelessOD >>= fixBrokenUnisons >>= magmaPad . fixBeatTrack'
  return (mid', psDifficultyRB3 diffs, vc, pad)

processPS
  :: (SendMessage m, MonadIO m)
  => TargetPS
  -> SongYaml f
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> StackTraceT m U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> StackTraceT m (RBFile.Song (RBFile.FixedFile U.Beats), DifficultyPS, Maybe VocalCount)
processPS = processMIDI . Right

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
  => RBFile.Song (RBFile.OnyxFile U.Beats)
  -> StackTraceT m U.Seconds
  -> StackTraceT m (RBFile.Song (RBFile.OnyxFile U.Beats))
processTiming input getAudioLength = do
  BasicTiming{..} <- basicTiming input getAudioLength
  return input
    { RBFile.s_tracks = (RBFile.s_tracks input)
      { RBFile.onyxBeat = timingBeat
      , RBFile.onyxEvents = (RBFile.onyxEvents $ RBFile.s_tracks input)
        { eventsEnd        = RTB.singleton timingEnd        ()
        , eventsMusicStart = RTB.singleton timingMusicStart ()
        , eventsMusicEnd   = RTB.singleton timingMusicEnd   ()
        }
      }
    }

-- | Retrieves or generates [end], [music_start], [music_end], and the BEAT track.
basicTiming
  :: (SendMessage m, RBFile.HasEvents f, RBFile.ParseFile f)
  => RBFile.Song (f U.Beats)
  -> StackTraceT m U.Seconds
  -> StackTraceT m BasicTiming
basicTiming input@(RBFile.Song tempos mmap trks) getAudioLength = do
  let showPosition = RBFile.showPosition mmap
  -- If there's no @[end]@, put it after all MIDI events and audio files.
  timingEnd <- case RTB.viewL $ eventsEnd $ RBFile.getEventsTrack trks of
    Just ((t, _), _) -> return t
    Nothing -> do
      audLen <- U.unapplyTempoMap tempos <$> getAudioLength
      let thirtySecs = U.unapplyTempoMap tempos (30.05 :: U.Seconds)
          absTimes = ATB.getTimes . RTB.toAbsoluteEventList 0
          lastMIDIEvent = foldr max 0
            $ concatMap absTimes (RBFile.s_tracks $ RBFile.showMIDITracks input)
            ++ absTimes (U.tempoMapToBPS tempos)
          endPosn = fromInteger $ ceiling $ max thirtySecs $ max audLen lastMIDIEvent + 4
      warn $ unwords
        [ "Placing [end] at " <> showPosition endPosn <> "."
        , "Last MIDI event is at " <> showPosition lastMIDIEvent <> ","
        , "longest audio file ends at " <> showPosition audLen <> ","
        , "minimum Magma length (30s) is " <> showPosition thirtySecs
        ]
      return endPosn
  timingBeat <- let
    trk = beatLines $ RBFile.getBeatTrack trks
    in if RTB.null trk
      then do
        warn "No BEAT track found; automatic one generated from time signatures."
        return $ BeatTrack $ U.trackTake timingEnd $ makeBeatTrack mmap
      else return $ BeatTrack trk
  -- If [music_start] is before 2 beats,
  -- Magma will add auto [idle] events there in instrument tracks, and then error...
  let musicStartMin = 2 :: U.Beats
  timingMusicStart <- case RTB.viewL $ eventsMusicStart $ RBFile.getEventsTrack trks of
    Just ((t, _), _) -> if t < musicStartMin
      then do
        warn $ "[music_start] is too early. Moving to " ++ showPosition musicStartMin
        return musicStartMin
      else return t
    Nothing -> do
      warn $ "[music_start] is missing. Placing at " ++ showPosition musicStartMin
      return musicStartMin
  timingMusicEnd <- case RTB.viewL $ eventsMusicEnd $ RBFile.getEventsTrack trks of
    Just ((t, _), _) -> return t
    Nothing -> do
      warn $ unwords
        [ "[music_end] is missing. [end] is at"
        , showPosition timingEnd
        , "so [music_end] will be at"
        , showPosition $ timingEnd - 2
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
  :: RBFile.FlexPartName
  -> Either (TargetRB3 f) TargetPS
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> BasicTiming
  -> SongYaml f
  -> Maybe (DrumTrack U.Beats)
buildDrums drumsPart target (RBFile.Song tempos mmap trks) timing@BasicTiming{..} songYaml = case getPart drumsPart songYaml >>= partDrums of
  Nothing -> Nothing
  Just pd -> Just $ let
    psKicks = case drumsKicks pd of
      Kicks2x -> mapTrack (U.unapplyTempoTrack tempos) . phaseShiftKicks 0.18 0.11 . mapTrack (U.applyTempoTrack tempos)
      _       -> id
    sections = fmap snd $ eventsSections $ RBFile.onyxEvents trks
    finish = sloppyDrums . changeMode . psKicks . drumsComplete mmap sections
    sloppyDrums = drumEachDiff $ \dd -> dd { drumGems = fixSloppyNotes (10 / 480) $ drumGems dd }
    fiveToFourTrack = drumEachDiff $ \dd -> dd
      { drumGems = RBDrums.fiveToFour
        (case drumsFallback pd of
          FallbackBlue  -> RBDrums.Blue
          FallbackGreen -> RBDrums.Green
        )
        (drumGems dd)
      }
    drumEachDiff f dt = dt { drumDifficulties = fmap f $ drumDifficulties dt }
    noToms dt = dt { drumToms = RTB.empty }
    allToms dt = dt
      { drumToms = RTB.fromPairList
        [ (0      , (RBDrums.Yellow, RBDrums.Tom   ))
        , (0      , (RBDrums.Blue  , RBDrums.Tom   ))
        , (0      , (RBDrums.Green , RBDrums.Tom   ))
        , (timingEnd, (RBDrums.Yellow, RBDrums.Cymbal))
        , (0      , (RBDrums.Blue  , RBDrums.Cymbal))
        , (0      , (RBDrums.Green , RBDrums.Cymbal))
        ]
      }
    changeMode = case (drumsMode pd, target) of
      (DrumsFull, _         ) -> id
      (DrumsReal, _         ) -> id
      (DrumsPro , _         ) -> id
      -- TODO convert 5 to pro, not just basic.
      (Drums5   , Left  _rb3) -> allToms . fiveToFourTrack
      (Drums5   , Right _ps ) -> noToms
      (Drums4   , Right _ps ) -> noToms
      (Drums4   , Left  _rb3) -> allToms
    flex = RBFile.getFlexPart drumsPart trks
    -- TODO support DrumsFull
    trk1x = RBFile.onyxPartDrums flex
    trk2x = RBFile.onyxPartDrums2x flex
    trkReal = RBFile.onyxPartRealDrumsPS flex
    trkReal' = RBDrums.psRealToPro trkReal
    onlyPSReal = all nullDrums [trk1x, trk2x] && not (nullDrums trkReal)
    pro1x = if onlyPSReal then trkReal' else trk1x
    pro2x = if onlyPSReal then trkReal' else trk2x
    ps1x = finish $ if nullDrums pro1x then pro2x else pro1x
    ps2x = finish $ if nullDrums pro2x then pro1x else pro2x
    psPS = if not $ RTB.null $ drumKick2x pro1x then ps1x else ps2x
    autoAnims
      = U.unapplyTempoTrack tempos
      $ autoDrumAnimation (0.25 :: U.Seconds)
      $ fmap fst
      $ U.applyTempoTrack tempos
      $ computePro (Just Expert) ps1x
    addAnims dt = if RTB.null $ drumAnimation dt
      then dt { drumAnimation = autoAnims }
      else dt
    addMoods dt = dt
      { drumMood = noEarlyMood $ if RTB.null $ drumMood dt
        then makeMoods tempos timing $ Blip () () <$ drumAnimation dt
        else drumMood dt
      }
    drumsRemoveBRE = case removeBRE target $ RBFile.onyxEvents trks of
      Just BRERemover{..}
        -> drumEachDiff (\dd -> dd { drumGems = breRemoveBlips $ drumGems dd })
        . (\dt -> dt
          { drumSingleRoll = breRemoveLanes $ drumSingleRoll dt
          , drumDoubleRoll = breRemoveLanes $ drumDoubleRoll dt
          })
      Nothing -> id
    -- Note: drumMix must be applied *after* drumsComplete.
    -- Otherwise the automatic EMH mix events could prevent lower difficulty generation.
    in (if drumsFixFreeform pd then RBFile.fixFreeformDrums else id)
      $ (\dt -> dt { drumPlayer1 = RTB.empty, drumPlayer2 = RTB.empty })
      $ drumsRemoveBRE
      $ addMoods
      $ addAnims
      $ case target of
        Left rb3 -> drumEachDiff (\dd -> dd { drumPSModifiers = RTB.empty }) $
          if rb3_2xBassPedal rb3
            then rockBand2x ps2x
            else rockBand1x ps1x
        Right _ -> psPS { drumOverdrive = fixTapOff $ drumOverdrive psPS }

data BRERemover t = BRERemover
  { breRemoveEdges :: forall s a. (Ord s, Ord a) => RTB.T t (Edge s a) -> RTB.T t (Edge s a)
  , breRemoveBools :: RTB.T t Bool -> RTB.T t Bool
  , breRemoveLanes :: RTB.T t (Maybe LaneDifficulty) -> RTB.T t (Maybe LaneDifficulty)
  , breRemoveBlips :: forall a. RTB.T t a -> RTB.T t a
  }

-- TODO make this more resilient, probably based on the "chop" functions
-- so it can correctly handle stuff beyond just removing notes
removeBRE :: (NNC.C t) => Either (TargetRB3 f) TargetPS -> EventsTrack t -> Maybe (BRERemover t)
removeBRE (Left _rb3) evts = case (eventsCoda evts, eventsCodaResume evts) of
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
removeBRE (Right _ps) _ = Nothing

addFiveMoods
  :: U.TempoMap
  -> BasicTiming
  -> FiveTrack U.Beats
  -> FiveTrack U.Beats
addFiveMoods tempos timing ft = ft
  { fiveMood = noEarlyMood $ if RTB.null $ fiveMood ft
    then makeMoods tempos timing $ let
      expert = fromMaybe mempty $ Map.lookup Expert $ fiveDifficulties ft
      in splitEdges $ (\(_, len) -> ((), (), len)) <$> edgeBlipsRB_ (fiveGems expert)
    else fiveMood ft
  }

buildFive
  :: RBFile.FlexPartName
  -> Either (TargetRB3 f) TargetPS
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> BasicTiming
  -> Bool
  -> SongYaml f
  -> Maybe (FiveTrack U.Beats)
buildFive fivePart target (RBFile.Song tempos mmap trks) timing toKeys songYaml = case getPart fivePart songYaml >>= partGRYBO of
  Nothing    -> Nothing
  Just grybo -> Just $ let
    src = RBFile.getFlexPart fivePart trks
    gtrType = case (target, toKeys) of
      (_         , True ) -> RBFile.FiveTypeKeys
      (Left  _rb3, False) -> RBFile.FiveTypeGuitar
      (Right _ps , False) -> RBFile.FiveTypeGuitarExt
    (trackOrig, algo) = RBFile.selectGuitarTrack gtrType src
    track
      = (\fd -> fd { fivePlayer1 = RTB.empty, fivePlayer2 = RTB.empty })
      $ (if gryboFixFreeform grybo then RBFile.fixFreeformFive else id)
      $ gryboComplete (guard (not toKeys) >> Just ht) mmap trackOrig
    ht = gryboHopoThreshold grybo
    fiveEachDiff f ft = ft { fiveDifficulties = fmap f $ fiveDifficulties ft }
    gap = fromIntegral (gryboSustainGap grybo) / 480
    breRemover = removeBRE target $ RBFile.onyxEvents trks
    forRB3 = fiveEachDiff $ \fd ->
        emit5'
      . fromClosed'
      . no5NoteChords'
      . noOpenNotes'
      . noTaps'
      . (if toKeys then id else noExtendedSustains' standardBlipThreshold gap)
      . applyForces (getForces5 fd)
      . strumHOPOTap' algo (fromIntegral ht / 480)
      . fixSloppyNotes (10 / 480)
      . maybe id breRemoveBlips breRemover
      . closeNotes'
      $ fd
    forPS = fiveEachDiff $ \fd ->
        emit5'
      . applyForces (getForces5 fd)
      . strumHOPOTap' algo (fromIntegral ht / 480)
      . fixSloppyNotes (10 / 480)
      . openNotes'
      $ fd
    chSPFix ft = ft { fiveOverdrive = fixTapOff $ fiveOverdrive ft }
    forAll
      = addFiveMoods tempos timing
      . (if gryboSmoothFrets grybo then smoothFrets else id)
      . (\ft -> ft
        { fiveTremolo = maybe id breRemoveLanes breRemover $ fiveTremolo ft
        , fiveTrill   = maybe id breRemoveLanes breRemover $ fiveTrill   ft
        })
    smoothFrets x = x
      { fiveFretPosition
        = U.unapplyTempoTrack tempos
        $ smoothFretPosition
        $ U.applyTempoTrack tempos
        $ fiveFretPosition x
      }
    in forAll $ case target of
      Left  _rb3 -> forRB3 track
      Right _ps  -> chSPFix $ forPS track

processMIDI
  :: (SendMessage m, MonadIO m)
  => Either (TargetRB3 f) TargetPS
  -> SongYaml f
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> StackTraceT m U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> StackTraceT m (RBFile.Song (RBFile.FixedFile U.Beats), DifficultyPS, Maybe VocalCount) -- ^ output midi, filtered difficulties, vocal count
processMIDI target songYaml input@(RBFile.Song tempos mmap trks) mixMode getAudioLength = inside "Processing MIDI for RB3/PS" $ do
  timing@BasicTiming{..} <- basicTiming input getAudioLength
  let targetPS = case target of
        Right tps -> tps
        Left trb3 -> TargetPS
          { ps_Common        = rb3_Common trb3
          , ps_Guitar        = rb3_Guitar trb3
          , ps_Bass          = rb3_Bass trb3
          , ps_Drums         = rb3_Drums trb3
          , ps_Keys          = rb3_Keys trb3
          , ps_Vocal         = rb3_Vocal trb3
          , ps_Rhythm        = RBFile.FlexExtra "undefined"
          , ps_GuitarCoop    = RBFile.FlexExtra "undefined"
          , ps_Dance         = RBFile.FlexExtra "undefined"
          , ps_LoadingPhrase = Nothing
          }
      originalRanks = difficultyPS targetPS songYaml

      eventsInput = RBFile.onyxEvents trks
      (crowd, crowdClap) = if RTB.null (eventsCrowd eventsInput) && RTB.null (eventsCrowdClap eventsInput)
        then
          ( RTB.singleton timingMusicStart CrowdRealtime
          , RTB.singleton timingMusicStart False
          )
        else (eventsCrowd eventsInput, eventsCrowdClap eventsInput)
      eventsTrack = EventsTrack
        { eventsMusicStart = RTB.singleton timingMusicStart ()
        , eventsMusicEnd   = RTB.singleton timingMusicEnd ()
        , eventsEnd        = RTB.singleton timingEnd ()
        , eventsCoda       = eventsCoda eventsInput
        , eventsCodaResume = RTB.empty
        , eventsCrowd      = crowd
        , eventsCrowdClap  = crowdClap
        , eventsSections   = eventsSections eventsInput
        , eventsBacking    = if RTB.null (eventsBacking eventsInput) && not (nullDrums drumsTrack)
          then let
            makeBacking evts = nubOrd $ flip map evts $ \(gem, _vel) -> case gem of
              RBDrums.Kick                 -> BackingKick
              RBDrums.Red                  -> BackingSnare
              RBDrums.Pro _ RBDrums.Cymbal -> BackingHihat
              RBDrums.Pro _ RBDrums.Tom    -> BackingKick
              RBDrums.Orange               -> BackingHihat
            in RTB.flatten $ fmap makeBacking $ RTB.collectCoincident $ computePro (Just Expert) drumsTrack
          else eventsBacking eventsInput
        }
      eventsTrackPS = eventsTrack
        { eventsSections = makePSSection . snd <$> eventsSections eventsTrack
        }
      drumsPart = either rb3_Drums ps_Drums target
      drumsTrack = case buildDrums drumsPart target input timing songYaml of
        Nothing -> mempty
        Just dt -> setDrumMix mixMode dt
      makeGRYBOTrack toKeys fpart = fromMaybe mempty $ buildFive fpart target input timing toKeys songYaml

      guitarPart = either rb3_Guitar ps_Guitar target
      bassPart = either rb3_Bass ps_Bass target

      -- TODO: pgHopoThreshold
      makeProGtrTracks gtype fpart = case getPart fpart songYaml >>= partProGuitar of
        Nothing -> return (mempty, mempty)
        Just pg -> let
          src = RBFile.getFlexPart fpart trks
          tuning = tuningPitches (pgTuning pg) { gtrGlobal = 0 }
          applyType x = x
            { pgTrainer = (\(_, trainer) -> (gtype, trainer)) <$> pgTrainer x
            , pgBRE     = (\(_, b      ) -> (gtype, b      )) <$> pgBRE     x
            }
          extendedTuning = length pitches > case gtype of
            TypeGuitar -> 6
            TypeBass   -> 4
          pitches = tuningPitches (pgTuning pg) { gtrGlobal = 0 }
          defaultFlat = maybe False songKeyUsesFlats $ _key $ _metadata songYaml
          f = applyType
            . (if pgFixFreeform pg then RBFile.fixFreeformPG else id) . protarComplete
            . autoHandPosition . moveStrings
            . (if extendedTuning then freezeChordNames pitches defaultFlat else id)
            . autoChordRoot tuning
            . pgRemoveBRE
          pgRemoveBRE trk = case removeBRE target $ RBFile.onyxEvents trks of
            Just BRERemover{..} -> trk
              { pgDifficulties = flip fmap (pgDifficulties trk) $ \pgd -> pgd
                { pgNotes = breRemoveEdges $ pgNotes pgd
                }
              , pgTremolo = breRemoveLanes $ pgTremolo trk
              , pgTrill = breRemoveLanes $ pgTrill trk
              }
            Nothing -> trk
          src17  = RBFile.onyxPartRealGuitar   src
          src22  = RBFile.onyxPartRealGuitar22 src
          srcRSG = RBFile.onyxPartRSGuitar     src
          srcRSB = RBFile.onyxPartRSBass       src
          in do
            src22' <- case (nullPG src22 && nullPG src17, nullRS srcRSG, nullRS srcRSB) of
              (True, False, _    ) -> convertRStoPG srcRSG
              (True, True , False) -> convertRStoPG srcRSB
              (_   , _    , _    ) -> return src22
            let mustang = f $ fretLimit 17 $ if nullPG src17  then src22' else src17
                squier  = f $ fretLimit 22 $ if nullPG src22' then src17  else src22'
            return (mustang, if mustang == squier then mempty else squier)

  (proGtr , proGtr22 ) <- makeProGtrTracks TypeGuitar guitarPart
  (proBass, proBass22) <- makeProGtrTracks TypeBass   bassPart

  let hasProtarNotFive partName = case getPart partName songYaml of
        Just part -> case (partGRYBO part, partProGuitar part) of
          (Nothing, Just _) -> True
          _                 -> False
        Nothing -> False

      guitar = if hasProtarNotFive guitarPart
        then addFiveMoods tempos timing $ RBFile.protarToGrybo proGtr
        else makeGRYBOTrack False guitarPart

      bass = if hasProtarNotFive bassPart
        then addFiveMoods tempos timing $ RBFile.protarToGrybo proBass
        else makeGRYBOTrack False bassPart

      rhythmPart = either (const $ RBFile.FlexExtra "undefined") ps_Rhythm target
      rhythmPS = makeGRYBOTrack False rhythmPart

      guitarCoopPart = either (const $ RBFile.FlexExtra "undefined") ps_GuitarCoop target
      guitarCoopPS = makeGRYBOTrack False guitarCoopPart

      sixEachDiff f st = st
        { sixDifficulties = fmap f $ sixDifficulties st
        , sixOverdrive = fixTapOff $ sixOverdrive st
        }
      guitarGHL = case getPart guitarPart songYaml >>= partGHL of
        Nothing  -> mempty
        Just ghl -> sixEachDiff
          ( \sd ->
            emit6'
          . applyForces (getForces6 sd)
          . strumHOPOTap' HOPOsRBGuitar (fromIntegral (ghlHopoThreshold ghl) / 480)
          . edgeBlipsRB_
          $ sixGems sd
          ) $ RBFile.onyxPartSix $ RBFile.getFlexPart guitarPart trks
      bassGHL = case getPart bassPart songYaml >>= partGHL of
        Nothing  -> mempty
        Just ghl -> sixEachDiff
          ( \sd ->
            emit6'
          . applyForces (getForces6 sd)
          . strumHOPOTap' HOPOsRBGuitar (fromIntegral (ghlHopoThreshold ghl) / 480)
          . edgeBlipsRB_
          $ sixGems sd
          ) $ RBFile.onyxPartSix $ RBFile.getFlexPart bassPart trks

      keysPart = either rb3_Keys ps_Keys target
      (tk, tkRH, tkLH, tpkX, tpkH, tpkM, tpkE) = case getPart keysPart songYaml of
        Nothing -> (mempty, mempty, mempty, mempty, mempty, mempty, mempty)
        Just part -> case (partGRYBO part, partProKeys part) of
          (Nothing, Nothing) -> (mempty, mempty, mempty, mempty, mempty, mempty, mempty)
          _ -> let
            basicKeys = gryboComplete Nothing mmap
              $ case partGRYBO part of
                Nothing -> addFiveMoods tempos timing $ RBFile.expertProKeysToKeys keysExpert
                Just _  -> makeGRYBOTrack True keysPart
            fpart = RBFile.getFlexPart keysPart trks
            keysDiff diff = if isJust $ partProKeys part
              then case diff of
                Easy   -> RBFile.onyxPartRealKeysE fpart
                Medium -> RBFile.onyxPartRealKeysM fpart
                Hard   -> RBFile.onyxPartRealKeysH fpart
                Expert -> RBFile.onyxPartRealKeysX fpart
              else RBFile.keysToProKeys diff basicKeys
            pkd1 `orIfNull` pkd2 = if length (pkNotes pkd1) < 5 then pkd2 else pkd1
            eachPKDiff = ffPro . fixPSRange . fixPKMood . completeRanges
              . (case removeBRE target $ RBFile.onyxEvents trks of
                Just BRERemover{..} -> \pk -> pk
                  { pkNotes     = breRemoveEdges $ pkNotes     pk
                  , pkGlissando = breRemoveBools $ pkGlissando pk
                  , pkTrill     = breRemoveBools $ pkTrill     pk
                  }
                Nothing -> id)
              . (\pk -> pk { pkNotes = fixSloppyNotes (10 / 480) $ pkNotes pk })
            keysExpert = eachPKDiff $ keysDiff Expert
            keysHard   = eachPKDiff $ keysDiff Hard   `orIfNull` pkReduce Hard   mmap keysOD keysExpert
            keysMedium = eachPKDiff $ keysDiff Medium `orIfNull` pkReduce Medium mmap keysOD keysHard
            keysEasy   = eachPKDiff $ keysDiff Easy   `orIfNull` pkReduce Easy   mmap keysOD keysMedium
            fixPKMood x = x { pkMood = noEarlyMood $ pkMood x }
            keysOD = pkOverdrive keysExpert
            originalRH = RBFile.onyxPartKeysAnimRH fpart
            originalLH = RBFile.onyxPartKeysAnimLH fpart
            (animRH, animLH) = if nullPK originalRH && nullPK originalLH
              then (mempty { pkNotes = pkNotes keysExpert }, mempty)
              else (originalRH, originalLH)
            ffBasic = if fmap gryboFixFreeform (partGRYBO part) == Just True
              then RBFile.fixFreeformFive
              else id
            ffPro = if fmap pkFixFreeform (partProKeys part) == Just True
              then RBFile.fixFreeformPK
              else id
            -- nemo's checker doesn't like if you include this stuff on PART KEYS
            removeGtrStuff ft = ft
              { fiveFretPosition = RTB.empty
              , fiveHandMap = RTB.empty
              , fiveStrumMap = RTB.empty
              , fiveTremolo = RTB.empty
              , fiveDifficulties = flip Map.mapWithKey (fiveDifficulties ft) $ \diff fd ->
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

  let vocalPart = either rb3_Vocal ps_Vocal target
      voxCount = fmap vocalCount $ getPart vocalPart songYaml >>= partVocal
      partVox = RBFile.onyxPartVocals $ RBFile.getFlexPart vocalPart trks
      harm1   = RBFile.onyxHarm1 $ RBFile.getFlexPart vocalPart trks
      harm2   = RBFile.onyxHarm2 $ RBFile.getFlexPart vocalPart trks
      harm3   = RBFile.onyxHarm3 $ RBFile.getFlexPart vocalPart trks
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
        (_, True) -> NoteOn () ()
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
      voxPSCH = hashTalkies . asciiLyrics -- ascii lyrics needed for PS, could remove for CH?
      -- CH lyrics don't handle ^ correctly, so we replace with the standard #.
      -- specifically "word-^" results in the hyphen being shown.
      hashTalkies vt = vt { vocalLyrics = T.replace "^" "#" <$> vocalLyrics vt }

  drumsTrack' <- let
    fills = RTB.normalize $ drumActivation drumsTrack
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
  camera <- stackIO $ buildCamera partsForVenue $ RBFile.onyxCamera trks
  let isPS = case target of Left _rb3 -> False; Right _ps -> True
      venue = compileVenueRB3 $ mconcat
        [ RBFile.onyxVenue trks
        , buildLighting $ RBFile.onyxLighting trks
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
  return (RBFile.Song tempos' mmap' RBFile.FixedFile
    { RBFile.fixedBeat = timingBeat
    , RBFile.fixedEvents = if isPS then eventsTrackPS else eventsTrack
    , RBFile.fixedVenue = venue
    , RBFile.fixedPartDrums = drumsTrack'
    , RBFile.fixedPartDrums2x = mempty
    , RBFile.fixedPartRealDrumsPS = mempty
    , RBFile.fixedPartGuitar = guitar
    , RBFile.fixedPartGuitarGHL = if isPS then guitarGHL else mempty
    , RBFile.fixedPartBass = bass
    , RBFile.fixedPartBassGHL = if isPS then bassGHL else mempty
    , RBFile.fixedPartRhythm = if isPS then rhythmPS else mempty
    , RBFile.fixedPartGuitarCoop = if isPS then guitarCoopPS else mempty
    , RBFile.fixedPartRealGuitar   = proGtr
    , RBFile.fixedPartRealGuitar22 = proGtr22
    , RBFile.fixedPartRealBass     = proBass
    , RBFile.fixedPartRealBass22   = proBass22
    , RBFile.fixedPartKeys = tk
    , RBFile.fixedPartKeysAnimRH = tkRH
    , RBFile.fixedPartKeysAnimLH = tkLH
    , RBFile.fixedPartRealKeysE = tpkE
    , RBFile.fixedPartRealKeysM = tpkM
    , RBFile.fixedPartRealKeysH = tpkH
    , RBFile.fixedPartRealKeysX = tpkX
    , RBFile.fixedPartVocals = (if isPS then voxPSCH else id) trkVox'
    , RBFile.fixedHarm1 = (if isPS then voxPSCH else id) trkHarm1'
    , RBFile.fixedHarm2 = (if isPS then voxPSCH else id) trkHarm2'
    , RBFile.fixedHarm3 = (if isPS then voxPSCH else id) trkHarm3'
    , RBFile.fixedPartDance = mempty
    , RBFile.fixedLipsync1 = mempty
    , RBFile.fixedLipsync2 = mempty
    , RBFile.fixedLipsync3 = mempty
    , RBFile.fixedLipsync4 = mempty
    }, editRanks, editCount)

magmaLegalTemposFile :: (SendMessage m) => RBFile.Song (RBFile.FixedFile U.Beats) -> StackTraceT m (RBFile.Song (RBFile.FixedFile U.Beats))
magmaLegalTemposFile rb3 = let
  endTime = case RTB.viewL $ eventsEnd $ RBFile.fixedEvents $ RBFile.s_tracks rb3 of
    Nothing           -> 0 -- shouldn't happen
    Just ((dt, _), _) -> dt
  in magmaLegalTempos endTime (RBFile.s_tempos rb3) (RBFile.s_signatures rb3) >>= \case
    (Nothing, _, _, _) -> return rb3
    (Just msg, newTempos, newSigs, TrackAdjust adjuster) -> do
      warn msg
      return $ RBFile.Song newTempos newSigs $ mapTrack adjuster $ RBFile.s_tracks rb3

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

fixBeatTrack' :: RBFile.Song (RBFile.FixedFile U.Beats) -> RBFile.Song (RBFile.FixedFile U.Beats)
fixBeatTrack' rb3 = let
  endTime = case RTB.viewL $ eventsEnd $ RBFile.fixedEvents $ RBFile.s_tracks rb3 of
    Nothing           -> 0 -- shouldn't happen
    Just ((dt, _), _) -> dt
  in rb3
    { RBFile.s_tracks = (RBFile.s_tracks rb3)
      { RBFile.fixedBeat = fixBeatTrack endTime $ RBFile.fixedBeat $ RBFile.s_tracks rb3
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
  => RBFile.Song (RBFile.FixedFile U.Beats)
  -> StackTraceT m (RBFile.Song (RBFile.FixedFile U.Beats), Int)
magmaPad rb3@(RBFile.Song tmap _ trks) = let
  firstEvent rtb = case RTB.viewL rtb of
    Just ((dt, _), _) -> dt
    Nothing           -> 999
  firstNoteBeats = foldr min 999 $ concat
    [ map (firstEvent . drumGems) $ Map.elems $ drumDifficulties $ RBFile.fixedPartDrums        trks
    , map (firstEvent . fiveGems) $ Map.elems $ fiveDifficulties $ RBFile.fixedPartGuitar       trks
    , map (firstEvent . fiveGems) $ Map.elems $ fiveDifficulties $ RBFile.fixedPartBass         trks
    , map (firstEvent . fiveGems) $ Map.elems $ fiveDifficulties $ RBFile.fixedPartKeys         trks
    , map (firstEvent . pgNotes ) $ Map.elems $ pgDifficulties   $ RBFile.fixedPartRealGuitar   trks
    , map (firstEvent . pgNotes ) $ Map.elems $ pgDifficulties   $ RBFile.fixedPartRealGuitar22 trks
    , map (firstEvent . pgNotes ) $ Map.elems $ pgDifficulties   $ RBFile.fixedPartRealBass     trks
    , map (firstEvent . pgNotes ) $ Map.elems $ pgDifficulties   $ RBFile.fixedPartRealBass22   trks
    , map (firstEvent . pkNotes )
      [ RBFile.fixedPartRealKeysE trks
      , RBFile.fixedPartRealKeysM trks
      , RBFile.fixedPartRealKeysH trks
      , RBFile.fixedPartRealKeysX trks
      ]
    , map (firstEvent . vocalNotes)
      [ RBFile.fixedPartVocals trks
      , RBFile.fixedHarm1      trks
      , RBFile.fixedHarm2      trks
      , RBFile.fixedHarm3      trks
      ]
    ]
  firstNoteSeconds = U.applyTempoMap tmap firstNoteBeats
  -- magma says 2.45s but account for some float error
  -- TODO this needs to be bumped! even 2.571 causes problems on non-bns expert
  padSeconds = max 0 $ ceiling $ 2.451 - (realToFrac firstNoteSeconds :: Rational)
  in case padSeconds of
    0 -> do
      return (rb3, 0)
    _ -> do
      warn $ "Padding song by " ++ show padSeconds ++ "s due to early notes."
      return (RBFile.padFixedFile padSeconds rb3, padSeconds)

findProblems :: RBFile.Song (RBFile.OnyxFile U.Beats) -> [String]
findProblems song = execWriter $ do
  -- Every discobeat mix event should be simultaneous with,
  -- or immediately followed by, a set of notes not including red or yellow.
  let drums = RBFile.onyxPartDrums $ RBFile.getFlexPart RBFile.FlexDrums $ RBFile.s_tracks song
      discos = foldr RTB.merge RTB.empty $ do
        d <- [minBound .. maxBound]
        let diff = fromMaybe mempty $ Map.lookup d $ drumDifficulties drums
        return $ flip RTB.mapMaybe (drumMix diff) $ \case
          (_, RBDrums.Disco) -> Just d
          _                  -> Nothing
      badDiscos = void $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ filter isBadDisco $ ATB.toPairList $ RTB.toAbsoluteEventList 0 discos
      drumsDiff d = drumGems $ fromMaybe mempty $ Map.lookup d $ drumDifficulties drums
      isBadDisco (t, diff) = case RTB.viewL $ RTB.collectCoincident $ U.trackDrop t $ drumsDiff diff of
        Just ((_, evts), _) | any isDiscoGem evts -> True
        _                                         -> False
      isDiscoGem = \case
        (RBDrums.Red                 , _) -> True
        (RBDrums.Pro RBDrums.Yellow _, _) -> True
        _                                 -> False
  -- Don't have a vocal phrase that ends simultaneous with a lyric event.
  -- In static vocals, this puts the lyric in the wrong phrase.
  let vox = RBVox.vocalToLegacy $ RBFile.onyxPartVocals $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
      harm1 = RBVox.vocalToLegacy $ RBFile.onyxHarm1 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
      harm2 = RBVox.vocalToLegacy $ RBFile.onyxHarm2 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
      harm3 = RBVox.vocalToLegacy $ RBFile.onyxHarm3 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
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
        = map (RBFile.showPosition $ RBFile.s_signatures song)
        . ATB.getTimes
        . RTB.toAbsoluteEventList 0
      message rtb msg = forM_ (showPositions rtb) $ \pos ->
        tell [pos ++ ": " ++ msg]
  message badDiscos "discobeat drum event is followed immediately by red or yellow gem"
  message voxBugs "PART VOCALS vocal phrase ends simultaneous with a lyric"
  message harm1Bugs "HARM1 vocal phrase ends simultaneous with a lyric"
  message harm2Bugs "HARM2 vocal phrase ends simultaneous with a (HARM2 or HARM3) lyric"
