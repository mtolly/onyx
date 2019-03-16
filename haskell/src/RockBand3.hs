{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module RockBand3 (processRB3Pad, processPS, processTiming, findProblems, TrackAdjust(..), magmaLegalTempos) where

import           Config
import           Control.Monad.Extra
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer.Strict (execWriter, tell)
import qualified Data.EventList.Absolute.TimeBody  as ATB
import qualified Data.EventList.Relative.TimeBody  as RTB
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe, isJust)
import           Guitars
import           OneFoot
import           Overdrive                         (fixBrokenUnisons)
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
import           Scripts
import qualified Sound.MIDI.Util                   as U

processRB3Pad
  :: TargetRB3
  -> SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> Staction U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> Staction (RBFile.Song (RBFile.FixedFile U.Beats), Int)
processRB3Pad a b c d e = do
  res <- processMIDI (Left a) b c d e
  -- TODO we probably should run fixBrokenUnisons before autoreductions
  magmaLegalTemposFile res >>= fixBrokenUnisons >>= magmaPad . fixBeatTrack'

processPS
  :: TargetPS
  -> SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> Staction U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> Staction (RBFile.Song (RBFile.FixedFile U.Beats))
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

processTiming
  :: (SendMessage m)
  => RBFile.Song (RBFile.OnyxFile U.Beats)
  -> StackTraceT m U.Seconds
  -> StackTraceT m (RBFile.Song (RBFile.OnyxFile U.Beats))
processTiming input getAudioLength = do
  (endPosn, musicStartPosn, musicEndPosn, beatTrack) <- basicTiming input getAudioLength
  return input
    { RBFile.s_tracks = (RBFile.s_tracks input)
      { RBFile.onyxBeat = beatTrack
      , RBFile.onyxEvents = (RBFile.onyxEvents $ RBFile.s_tracks input)
        { eventsEnd        = RTB.singleton endPosn        ()
        , eventsMusicStart = RTB.singleton musicStartPosn ()
        , eventsMusicEnd   = RTB.singleton musicEndPosn   ()
        }
      }
    }

-- | Retrieves or generates [end], [music_start], [music_end], and the BEAT track.
basicTiming
  :: (SendMessage m)
  => RBFile.Song (RBFile.OnyxFile U.Beats)
  -> StackTraceT m U.Seconds
  -> StackTraceT m (U.Beats, U.Beats, U.Beats, BeatTrack U.Beats)
basicTiming input@(RBFile.Song tempos mmap trks) getAudioLength = do
  let showPosition = RBFile.showPosition . U.applyMeasureMap mmap
  -- | If there's no [end], put it after all MIDI events and audio files.
  endPosn <- case RTB.viewL $ eventsEnd $ RBFile.onyxEvents trks of
    Just ((t, _), _) -> return t
    Nothing -> do
      audLen <- U.unapplyTempoMap tempos <$> getAudioLength
      let absTimes = ATB.getTimes . RTB.toAbsoluteEventList 0
          lastMIDIEvent = foldr max 0
            $ concatMap absTimes (RBFile.s_tracks $ RBFile.showMIDITracks input)
            ++ absTimes (U.tempoMapToBPS tempos)
          endPosn = fromInteger $ round $ max audLen lastMIDIEvent + 4
      warn $ unwords
        [ "[end] is missing. The last MIDI event is at"
        , showPosition lastMIDIEvent
        , "and the longest audio file ends at"
        , showPosition audLen
        , "so [end] will be at"
        , showPosition endPosn
        ]
      return endPosn
  beatTrack <- let
    trk = beatLines $ RBFile.onyxBeat trks
    in if RTB.null trk
      then do
        warn "No BEAT track found; automatic one generated from time signatures."
        return $ BeatTrack $ U.trackTake endPosn $ makeBeatTrack mmap
      else return $ BeatTrack trk
  -- If [music_start] is before 2 beats,
  -- Magma will add auto [idle] events there in instrument tracks, and then error...
  let musicStartMin = 2 :: U.Beats
  musicStartPosn <- case RTB.viewL $ eventsMusicStart $ RBFile.onyxEvents trks of
    Just ((t, _), _) -> if t < musicStartMin
      then do
        warn $ "[music_start] is too early. Moving to " ++ showPosition musicStartMin
        return musicStartMin
      else return t
    Nothing -> do
      warn $ "[music_start] is missing. Placing at " ++ showPosition musicStartMin
      return musicStartMin
  musicEndPosn <- case RTB.viewL $ eventsMusicEnd $ RBFile.onyxEvents trks of
    Just ((t, _), _) -> return t
    Nothing -> do
      warn $ unwords
        [ "[music_end] is missing. [end] is at"
        , showPosition endPosn
        , "so [music_end] will be at"
        , showPosition $ endPosn - 2
        ]
      return $ endPosn - 2
  return (endPosn, musicStartPosn, musicEndPosn, beatTrack)

processMIDI
  :: Either TargetRB3 TargetPS
  -> SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> Staction U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> Staction (RBFile.Song (RBFile.FixedFile U.Beats))
processMIDI target songYaml input@(RBFile.Song tempos mmap trks) mixMode getAudioLength = inside "Processing MIDI for RB3/PS" $ do
  (endPosn, musicStartPosn, musicEndPosn, beatTrack) <- basicTiming input getAudioLength
  let eventsInput = RBFile.onyxEvents trks
      (crowd, crowdClap) = if RTB.null (eventsCrowd eventsInput) && RTB.null (eventsCrowdClap eventsInput)
        then
          ( RTB.singleton musicStartPosn CrowdRealtime
          , RTB.singleton musicStartPosn False
          )
        else (eventsCrowd eventsInput, eventsCrowdClap eventsInput)
      eventsTrack = EventsTrack
        { eventsMusicStart = RTB.singleton musicStartPosn ()
        , eventsMusicEnd   = RTB.singleton musicEndPosn ()
        , eventsEnd        = RTB.singleton endPosn ()
        , eventsCoda       = eventsCoda eventsInput
        , eventsCrowd      = crowd
        , eventsCrowdClap  = crowdClap
        , eventsSections   = eventsSections eventsInput
        , eventsBacking    = eventsBacking eventsInput
        }
      eventsTrackPS = eventsTrack
        { eventsSections = (makePSSection . snd) <$> eventsSections eventsTrack
        }
      drumsPart = either rb3_Drums ps_Drums target
      drumsTrack = case getPart drumsPart songYaml >>= partDrums of
        Nothing -> mempty
        Just pd -> let
          psKicks = case drumsKicks pd of
            Kicks2x -> mapTrack (U.unapplyTempoTrack tempos) . phaseShiftKicks 0.18 0.11 . mapTrack (U.applyTempoTrack tempos)
            _       -> id
          sections = fmap snd $ eventsSections eventsInput
          finish = sloppyDrums . changeMode . psKicks . setDrumMix mixMode . drumsComplete mmap sections
          sloppyDrums = drumEachDiff $ \dd -> dd { drumGems = fixSloppyNotes (10 / 480) $ drumGems dd }
          fiveToFour instant = flip map instant $ \case
            RBDrums.Orange -> let
              color = if
                | RBDrums.Pro RBDrums.Blue  () `elem` instant -> RBDrums.Green
                | RBDrums.Pro RBDrums.Green () `elem` instant -> RBDrums.Blue
                | otherwise -> case drumsFallback pd of
                  FallbackBlue  -> RBDrums.Blue
                  FallbackGreen -> RBDrums.Green
              in RBDrums.Pro color ()
            x -> x
          fiveToFourTrack = drumEachDiff $ \dd -> dd
            { drumGems = RTB.flatten $ fmap fiveToFour $ RTB.collectCoincident $ drumGems dd
            }
          drumEachDiff f dt = dt { drumDifficulties = fmap f $ drumDifficulties dt }
          noToms dt = dt { drumToms = RTB.empty }
          allToms dt = dt
            { drumToms = RTB.fromPairList
              [ (0      , (RBDrums.Yellow, RBDrums.Tom   ))
              , (0      , (RBDrums.Blue  , RBDrums.Tom   ))
              , (0      , (RBDrums.Green , RBDrums.Tom   ))
              , (endPosn, (RBDrums.Yellow, RBDrums.Cymbal))
              , (0      , (RBDrums.Blue  , RBDrums.Cymbal))
              , (0      , (RBDrums.Green , RBDrums.Cymbal))
              ]
            }
          changeMode = case (drumsMode pd, target) of
            (DrumsPro, _         ) -> id
            -- TODO convert 5 to pro, not just basic.
            (Drums5  , Left  _rb3) -> allToms . fiveToFourTrack
            (Drums5  , Right _ps ) -> noToms
            (Drums4  , Right _ps ) -> noToms
            (Drums4  , Left  _rb3) -> allToms
          flex = RBFile.getFlexPart drumsPart trks
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
          -- Note: drumMix must be applied *after* drumsComplete.
          -- Otherwise the automatic EMH mix events could prevent lower difficulty generation.
          in (if drumsFixFreeform pd then fixFreeformDrums else id)
            $ (\dt -> dt { drumPlayer1 = RTB.empty, drumPlayer2 = RTB.empty })
            $ case target of
              Left rb3 -> if rb3_2xBassPedal rb3
                then rockBand2x ps2x
                else rockBand1x ps1x
              Right _ -> psPS
      makeGRYBOTrack toKeys fpart src = case getPart fpart songYaml >>= partGRYBO of
        Nothing -> (mempty, mempty)
        Just grybo -> let
          (trackOrig, isKeys) = if nullFive $ RBFile.onyxPartGuitar src
            then (RBFile.onyxPartKeys src, True)
            else (RBFile.onyxPartGuitar src, False)
          track
            = (\fd -> fd { fivePlayer1 = RTB.empty, fivePlayer2 = RTB.empty })
            $ (if gryboFixFreeform grybo then fixFreeformFive else id)
            $ gryboComplete (guard toKeys >> Just ht) mmap trackOrig
          ht = gryboHopoThreshold grybo
          algo = if isKeys then HOPOsRBKeys else HOPOsRBGuitar
          fiveEachDiff f ft = ft { fiveDifficulties = fmap f $ fiveDifficulties ft }
          gap = fromIntegral (gryboSustainGap grybo) / 480
          forRB3 = fiveEachDiff $ \fd ->
              emit5'
            . fromClosed'
            . no5NoteChords'
            . noOpenNotes' (gryboDropOpenHOPOs grybo)
            . noTaps'
            . (if toKeys then id else noExtendedSustains' standardBlipThreshold gap)
            . applyForces (getForces5 fd)
            . strumHOPOTap' algo (fromIntegral ht / 480)
            . fixSloppyNotes (10 / 480)
            . closeNotes'
            $ fd
          forPS = fiveEachDiff $ \fd ->
              emit5'
            . applyForces (getForces5 fd)
            . strumHOPOTap' algo (fromIntegral ht / 480)
            . fixSloppyNotes (10 / 480)
            . openNotes'
            $ fd
          forAll x = x
            { fiveMood = noEarlyMood $ fiveMood x
            {-
            , fiveFretPosition
              = U.unapplyTempoTrack tempos
              $ smoothFretPosition
              $ U.applyTempoTrack tempos
              $ fiveFretPosition x
            -}
            }
          in (forAll $ forRB3 track, forAll $ forPS track)

      hasProtarNotFive partName = case getPart partName songYaml of
        Just part -> case (partGRYBO part, partProGuitar part) of
          (Nothing, Just _) -> True
          _                 -> False
        Nothing -> False

      guitarPart = either rb3_Guitar ps_Guitar target
      (guitarRB3, guitarPS) = if hasProtarNotFive guitarPart
        then (\x -> (x, x)) $ protarToGrybo proGtr
        else makeGRYBOTrack False guitarPart $ RBFile.getFlexPart guitarPart trks

      bassPart = either rb3_Bass ps_Bass target
      (bassRB3, bassPS) = if hasProtarNotFive bassPart
        then (\x -> (x, x)) $ protarToGrybo proBass
        else makeGRYBOTrack False bassPart $ RBFile.getFlexPart bassPart trks

      rhythmPart = either (const $ RBFile.FlexExtra "undefined") ps_Rhythm target
      (_, rhythmPS) = makeGRYBOTrack False rhythmPart $ RBFile.getFlexPart rhythmPart trks

      guitarCoopPart = either (const $ RBFile.FlexExtra "undefined") ps_GuitarCoop target
      (_, guitarCoopPS) = makeGRYBOTrack False guitarCoopPart $ RBFile.getFlexPart guitarCoopPart trks

      sixEachDiff f st = st { sixDifficulties = fmap f $ sixDifficulties st }
      guitarGHL = case getPart guitarPart songYaml >>= partGHL of
        Nothing  -> mempty
        Just ghl -> sixEachDiff
          ( \sd ->
            emit6'
          . applyForces (getForces6 sd)
          . strumHOPOTap' HOPOsRBGuitar (fromIntegral (ghlHopoThreshold ghl) / 480)
          $ sixGems sd
          ) $ RBFile.onyxPartSix $ RBFile.getFlexPart guitarPart trks
      bassGHL = case getPart bassPart songYaml >>= partGHL of
        Nothing  -> mempty
        Just ghl -> sixEachDiff
          ( \sd ->
            emit6'
          . applyForces (getForces6 sd)
          . strumHOPOTap' HOPOsRBGuitar (fromIntegral (ghlHopoThreshold ghl) / 480)
          $ sixGems sd
          ) $ RBFile.onyxPartSix $ RBFile.getFlexPart bassPart trks

      -- TODO: pgHopoThreshold
      makeProGtrTracks fpart = case getPart fpart songYaml >>= partProGuitar of
        Nothing -> (mempty, mempty)
        Just pg -> let
          src = RBFile.getFlexPart fpart trks
          tuning = tuningPitches (pgTuning pg) { gtrGlobal = 0 }
          f = (if pgFixFreeform pg then fixFreeformPG else id) . protarComplete
            . autoHandPosition . moveStrings . autoChordRoot tuning
          src17 = RBFile.onyxPartRealGuitar   src
          src22 = RBFile.onyxPartRealGuitar22 src
          mustang = f $ fretLimit 17 $ if nullPG src17 then src22 else src17
          squier  = f $ fretLimit 22 $ if nullPG src22 then src17 else src22
          in (mustang, if mustang == squier then mempty else squier)
      (proGtr , proGtr22 ) = makeProGtrTracks guitarPart
      (proBass, proBass22) = makeProGtrTracks bassPart

      keysPart = either rb3_Keys ps_Keys target
      (tk, tkRH, tkLH, tpkX, tpkH, tpkM, tpkE) = case getPart keysPart songYaml of
        Nothing -> (mempty, mempty, mempty, mempty, mempty, mempty, mempty)
        Just part -> case (partGRYBO part, partProKeys part) of
          (Nothing, Nothing) -> (mempty, mempty, mempty, mempty, mempty, mempty, mempty)
          _ -> let
            basicKeysSrc = RBFile.getFlexPart keysPart trks
            basicKeys = gryboComplete Nothing mmap
              $ case partGRYBO part of
                Nothing -> expertProKeysToKeys keysExpert
                Just _  -> fst $ makeGRYBOTrack True keysPart basicKeysSrc
            fpart = RBFile.getFlexPart keysPart trks
            keysDiff diff = if isJust $ partProKeys part
              then case diff of
                Easy   -> RBFile.onyxPartRealKeysE fpart
                Medium -> RBFile.onyxPartRealKeysM fpart
                Hard   -> RBFile.onyxPartRealKeysH fpart
                Expert -> RBFile.onyxPartRealKeysX fpart
              else keysToProKeys diff basicKeys
            pkd1 `orIfNull` pkd2 = if length (pkNotes pkd1) < 5 then pkd2 else pkd1
            eachPKDiff = ffPro . fixPSRange . fixPKMood . completeRanges . (\pk -> pk { pkNotes = fixSloppyNotes (10 / 480) $ pkNotes pk })
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
              then fixFreeformFive
              else id
            ffPro = if fmap pkFixFreeform (partProKeys part) == Just True
              then fixFreeformPK
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

      vocalPart = either rb3_Vocal ps_Vocal target
      (trkVox, trkHarm1, trkHarm2, trkHarm3) = case getPart vocalPart songYaml >>= partVocal of
        Nothing -> (mempty, mempty, mempty, mempty)
        Just pv -> case vocalCount pv of
          Vocal3 -> (partVox', harm1 , harm2 , harm3 )
          Vocal2 -> (partVox', harm1 , harm2 , mempty)
          Vocal1 -> (partVox', mempty, mempty, mempty)
        where partVox = RBFile.onyxPartVocals $ RBFile.getFlexPart vocalPart trks
              partVox' = if nullVox partVox then harm1ToPartVocals harm1 else partVox
              harm1   = RBFile.onyxHarm1 $ RBFile.getFlexPart vocalPart trks
              harm2   = RBFile.onyxHarm2 $ RBFile.getFlexPart vocalPart trks
              harm3   = RBFile.onyxHarm3 $ RBFile.getFlexPart vocalPart trks
      trkVox'   = trkVox   { vocalMood = noEarlyMood $ vocalMood trkVox   }
      trkHarm1' = trkHarm1 { vocalMood = noEarlyMood $ vocalMood trkHarm1 }
      trkHarm2' = trkHarm2 { vocalMood = noEarlyMood $ vocalMood trkHarm2 }
      trkHarm3' = trkHarm3 { vocalMood = noEarlyMood $ vocalMood trkHarm3 }

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
      return drumsTrack { drumActivation = fixedFills, drumMood = noEarlyMood $ drumMood drumsTrack }

  -- remove tempo and time signature events on or after [end].
  -- found in some of bluzer's RB->PS converts. magma complains
  let tempos'
        = U.tempoMapFromBPS
        $ U.trackTake endPosn
        $ U.tempoMapToBPS tempos
      mmap'
        = U.measureMapFromTimeSigs U.Error
        $ U.trackTake endPosn
        $ U.measureMapToTimeSigs mmap

  let partsForVenue = concat
        [ [Guitar | not $ nullFive  guitarRB3  ]
        , [Bass   | not $ nullFive  bassRB3    ]
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
      -- hack: if author is testing lighting but no camera cuts yet, just show stage
      venue' = if RTB.null (venueCameraRB3 venue) && not (RTB.null $ venueLighting venue)
        then venue { venueCameraRB3 = RTB.singleton 0 V3_coop_all_near }
        else venue
  return $ RBFile.Song tempos' mmap' RBFile.FixedFile
    { RBFile.fixedBeat = beatTrack
    , RBFile.fixedEvents = if isPS then eventsTrackPS else eventsTrack
    , RBFile.fixedVenue = venue'
    , RBFile.fixedPartDrums = drumsTrack'
    , RBFile.fixedPartDrums2x = mempty
    , RBFile.fixedPartRealDrumsPS = mempty
    , RBFile.fixedPartGuitar = if isPS then guitarPS else guitarRB3
    , RBFile.fixedPartGuitarGHL = if isPS then guitarGHL else mempty
    , RBFile.fixedPartBass = if isPS then bassPS else bassRB3
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
    , RBFile.fixedPartVocals = (if isPS then asciiLyrics else id) trkVox'
    , RBFile.fixedHarm1 = (if isPS then asciiLyrics else id) trkHarm1'
    , RBFile.fixedHarm2 = (if isPS then asciiLyrics else id) trkHarm2'
    , RBFile.fixedHarm3 = (if isPS then asciiLyrics else id) trkHarm3'
    }

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
        RBDrums.Red -> True
        RBDrums.Pro RBDrums.Yellow _ -> True
        _ -> False
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
        = map (RBFile.showPosition . U.applyMeasureMap (RBFile.s_signatures song))
        . ATB.getTimes
        . RTB.toAbsoluteEventList 0
      message rtb msg = forM_ (showPositions rtb) $ \pos ->
        tell [pos ++ ": " ++ msg]
  message badDiscos "discobeat drum event is followed immediately by red or yellow gem"
  message voxBugs "PART VOCALS vocal phrase ends simultaneous with a lyric"
  message harm1Bugs "HARM1 vocal phrase ends simultaneous with a lyric"
  message harm2Bugs "HARM2 vocal phrase ends simultaneous with a (HARM2 or HARM3) lyric"
