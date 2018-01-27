{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module RockBand3 (processRB3, processRB3Pad, processPS, findProblems, TrackAdjust(..), magmaLegalTempos') where

import           Config
import           Control.Monad.Extra
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer       (execWriter, tell)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Maybe                       (isJust)
import           Development.Shake
import           Guitars
import           Numeric.NonNegative.Class        ((-|))
import           OneFoot
import           ProKeysRanges
import           Reductions
import qualified RockBand.Beat                    as Beat
import           RockBand.Common
import qualified RockBand.Drums                   as RBDrums
import qualified RockBand.Events                  as Events
import qualified RockBand.File                    as RBFile
import qualified RockBand.FiveButton              as Five
import qualified RockBand.ProGuitar               as ProGtr
import qualified RockBand.ProKeys                 as ProKeys
import           RockBand.Sections                (getSection, makePSSection)
import qualified RockBand.Vocals                  as RBVox
import           Scripts
import qualified Sound.MIDI.Util                  as U

processRB3
  :: TargetRB3
  -> SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> Action U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> StackTraceT (QueueLog Action) (RBFile.Song (RBFile.RB3File U.Beats))
processRB3 a b c d e = do
  res <- processMIDI (Left a) b c d e
  magmaLegalTempos $ fmap fst res

processRB3Pad
  :: TargetRB3
  -> SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> Action U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> StackTraceT (QueueLog Action) (RBFile.Song (RBFile.RB3File U.Beats), Int)
processRB3Pad a b c d e = do
  res <- processMIDI (Left a) b c d e
  magmaLegalTempos (fmap fst res) >>= magmaPad

processPS
  :: TargetPS
  -> SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> Action U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> StackTraceT (QueueLog Action) (RBFile.Song (RBFile.PSFile U.Beats))
processPS a b c d e = fmap (fmap snd) $ processMIDI (Right a) b c d e

processMIDI
  :: Either TargetRB3 TargetPS
  -> SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> Action U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> StackTraceT (QueueLog Action) (RBFile.Song (RBFile.RB3File U.Beats, RBFile.PSFile U.Beats))
processMIDI target songYaml input@(RBFile.Song tempos mmap trks) mixMode getAudioLength = inside "Processing MIDI for RB3/PS" $ do
  let showPosition = RBFile.showPosition . U.applyMeasureMap mmap
      eventsRaw = RBFile.onyxEvents trks
      eventsList = ATB.toPairList $ RTB.toAbsoluteEventList 0 eventsRaw
  -- If there's no [end], put it after all MIDI events and audio files.
  endPosn' <- case [ t | (t, Events.End) <- eventsList ] of
    t : _ -> return t
    [] -> do
      audLen <- U.unapplyTempoMap tempos <$> lift (lift getAudioLength)
      let absTimes = ATB.getTimes . RTB.toAbsoluteEventList 0
          lastMIDIEvent = foldr max 0
            $ concatMap absTimes (RBFile.s_tracks $ RBFile.showMIDITracks input)
            ++ absTimes (U.tempoMapToBPS tempos)
          endPosition = fromInteger $ round $ max audLen lastMIDIEvent + 4
      warn $ unwords
        [ "[end] is missing. The last MIDI event is at"
        , showPosition lastMIDIEvent
        , "and the longest audio file ends at"
        , showPosition audLen
        , "so [end] will be at"
        , showPosition endPosition
        ]
      return endPosition
  -- If we are generating an automatic BEAT track,
  -- we have to round the [end] position up a bit,
  -- to make sure the last BEAT note-off doesn't come after [end].
  (beatTrack, endPosn) <- let
    trk = RBFile.onyxBeat trks
    in if RTB.null trk
      then do
        warn "No BEAT track found; automatic one generated from time signatures."
        let alignedEnd = fromInteger $ ceiling endPosn'
        return (U.trackTake alignedEnd $ makeBeatTrack mmap, alignedEnd)
      else return (trk, endPosn')
  -- If [music_start] is before 2 beats,
  -- Magma will add auto [idle] events there in instrument tracks, and then error...
  let musicStartMin = 2 :: U.Beats
  musicStartPosn <- case [ t | (t, Events.MusicStart) <- eventsList ] of
    t : _ -> if t < musicStartMin
      then do
        warn $ "[music_start] is too early. Moving to " ++ showPosition musicStartMin
        return musicStartMin
      else return t
    []    -> do
      warn $ "[music_start] is missing. Placing at " ++ showPosition musicStartMin
      return musicStartMin
  musicEndPosn <- case [ t | (t, Events.MusicEnd) <- eventsList ] of
    t : _ -> return t
    []    -> do
      warn $ unwords
        [ "[music_end] is missing. [end] is at"
        , showPosition endPosn
        , "so [music_end] will be at"
        , showPosition $ endPosn - 2
        ]
      return $ endPosn - 2
  let untouchedEvent = \case
        Events.MusicStart -> False
        Events.MusicEnd -> False
        Events.End -> False
        _ -> True
      crowdEvents =
        [ Events.CrowdRealtime
        , Events.CrowdIntense
        , Events.CrowdNormal
        , Events.CrowdMellow
        , Events.CrowdNoclap
        , Events.CrowdClap
        ]
      defaultNoCrowd evts = if any (`elem` evts) crowdEvents
        then evts
        else RTB.insert musicStartPosn Events.CrowdRealtime
          $  RTB.insert musicStartPosn Events.CrowdNoclap evts
      eventsTrack
        = defaultNoCrowd
        $ RTB.insert musicStartPosn Events.MusicStart
        $ RTB.insert musicEndPosn Events.MusicEnd
        $ RTB.insert endPosn Events.End
        $ RTB.filter untouchedEvent eventsRaw
      eventsTrackPS = (\e -> maybe e makePSSection $ getSection e) <$> eventsTrack
      drumsPart = either rb3_Drums ps_Drums target
      drumsTrack = case getPart drumsPart songYaml >>= partDrums of
        Nothing -> RTB.empty
        Just pd -> let
          trk1x = RBFile.flexPartDrums $ RBFile.getFlexPart drumsPart trks
          trk2x = RBFile.flexPartDrums2x $ RBFile.getFlexPart drumsPart trks
          psKicks = if drumsAuto2xBass pd
            then U.unapplyTempoTrack tempos . phaseShiftKicks 0.18 0.11 . U.applyTempoTrack tempos
            else id
          sections = flip RTB.mapMaybe eventsRaw $ \case
            Events.SectionRB3 s -> Just s
            Events.SectionRB2 s -> Just s
            _                   -> Nothing
          finish = promarkers . psKicks . drumMix mixMode . drumsComplete mmap sections
          promarkers = if drumsPro pd
            then id
            else  RTB.insert 0       (RBDrums.ProType RBDrums.Yellow RBDrums.Tom   )
              .   RTB.insert 0       (RBDrums.ProType RBDrums.Blue   RBDrums.Tom   )
              .   RTB.insert 0       (RBDrums.ProType RBDrums.Green  RBDrums.Tom   )
              .   RTB.insert endPosn (RBDrums.ProType RBDrums.Yellow RBDrums.Cymbal)
              .   RTB.insert endPosn (RBDrums.ProType RBDrums.Blue   RBDrums.Cymbal)
              .   RTB.insert endPosn (RBDrums.ProType RBDrums.Green  RBDrums.Cymbal)
              .   RTB.filter (\case RBDrums.ProType _ _ -> False; _ -> True)
          ps1x = finish $ if RTB.null trk1x then trk2x else trk1x
          ps2x = finish $ if RTB.null trk2x then trk1x else trk2x
          psPS = if elem RBDrums.Kick2x trk1x then ps1x else ps2x
          -- Note: drumMix must be applied *after* drumsComplete.
          -- Otherwise the automatic EMH mix events could prevent lower difficulty generation.
          in (if drumsFixFreeform pd then fixFreeformDrums else id) $ case target of
            Left rb3 -> if rb3_2xBassPedal rb3
              then rockBand2x ps2x
              else rockBand1x ps1x
            Right _ -> psPS
      makeGRYBOTrack toKeys fpart src = case getPart fpart songYaml >>= partGRYBO of
        Nothing -> (RTB.empty, RTB.empty)
        Just grybo -> let
          track
            = (if gryboFixFreeform grybo then fixFreeformFive else id)
            $ gryboComplete (guard toKeys >> Just ht) mmap
            $ RBFile.flexFiveButton src
          ht = gryboHopoThreshold grybo
          algo = if RBFile.flexFiveIsKeys src then HOPOsRBKeys else HOPOsRBGuitar
          forRB3 = eachDifficulty
            $ emit5
            . fromClosed
            . noTaps
            . (if toKeys then id else noExtendedSustains standardBlipThreshold standardSustainGap)
            . strumHOPOTap algo (fromIntegral ht / 480)
            . closeNotes
          forPS = eachDifficulty
            $ emit5
            . strumHOPOTap algo (fromIntegral ht / 480)
            . openNotes
          in (forRB3 track, forPS track)

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

      guitarGHL = case getPart guitarPart songYaml >>= partGHL of
        Nothing  -> RTB.empty
        Just ghl -> eachDifficulty
          ( emit6
          . strumHOPOTap HOPOsRBGuitar (fromIntegral (ghlHopoThreshold ghl) / 480)
          . ghlNotes
          ) $ RBFile.flexGHL $ RBFile.getFlexPart guitarPart trks
      bassGHL = case getPart bassPart songYaml >>= partGHL of
        Nothing -> RTB.empty
        Just ghl -> eachDifficulty
          ( emit6
          . strumHOPOTap HOPOsRBGuitar (fromIntegral (ghlHopoThreshold ghl) / 480)
          . ghlNotes
          ) $ RBFile.flexGHL $ RBFile.getFlexPart bassPart trks

      -- TODO: pgHopoThreshold
      makeProGtrTracks fpart = case getPart fpart songYaml >>= partProGuitar of
        Nothing -> (RTB.empty, RTB.empty)
        Just pg -> let
          src = RBFile.getFlexPart fpart trks
          tuning = zipWith (+) ProGtr.standardGuitar $ case pgTuning pg of
            []   -> repeat 0
            offs -> offs
          f = (if pgFixFreeform pg then fixFreeformPG else id)
            . copyExpert . ProGtr.autoHandPosition . ProGtr.autoChordRoot tuning
          src17 = RBFile.flexPartRealGuitar   src
          src22 = RBFile.flexPartRealGuitar22 src
          mustang = f $ ProGtr.lowerOctaves 17 $ if RTB.null src17 then src22 else src17
          squier  = f $ ProGtr.lowerOctaves 22 $ if RTB.null src22 then src17 else src22
          in (mustang, if mustang == squier then RTB.empty else squier)
      (proGtr , proGtr22 ) = makeProGtrTracks guitarPart
      (proBass, proBass22) = makeProGtrTracks bassPart

      keysPart = either rb3_Keys ps_Keys target
      (tk, tkRH, tkLH, tpkX, tpkH, tpkM, tpkE) = case getPart keysPart songYaml of
        Nothing -> (RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty)
        Just part -> case (partGRYBO part, partProKeys part) of
          (Nothing, Nothing) -> (RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty)
          _ -> let
            basicKeysSrc = RBFile.getFlexPart keysPart trks
            basicKeys = gryboComplete Nothing mmap $ case partGRYBO part of
              Nothing -> expertProKeysToKeys keysExpert
              Just _  -> fst $ makeGRYBOTrack True keysPart basicKeysSrc
            fpart = RBFile.getFlexPart keysPart trks
            keysDiff diff = if isJust $ partProKeys part
              then case diff of
                Easy   -> RBFile.flexPartRealKeysE fpart
                Medium -> RBFile.flexPartRealKeysM fpart
                Hard   -> RBFile.flexPartRealKeysH fpart
                Expert -> RBFile.flexPartRealKeysX fpart
              else keysToProKeys diff basicKeys
            rtb1 `orIfNull` rtb2 = if length rtb1 < 5 then rtb2 else rtb1
            keysExpert = completeRanges $ keysDiff Expert
            keysHard   = completeRanges $ keysDiff Hard   `orIfNull` pkReduce Hard   mmap keysOD keysExpert
            keysMedium = completeRanges $ keysDiff Medium `orIfNull` pkReduce Medium mmap keysOD keysHard
            keysEasy   = completeRanges $ keysDiff Easy   `orIfNull` pkReduce Easy   mmap keysOD keysMedium
            keysOD = flip RTB.mapMaybe keysExpert $ \case
              ProKeys.Overdrive b -> Just b
              _                   -> Nothing
            originalRH = RBFile.flexPartKeysAnimRH fpart
            originalLH = RBFile.flexPartKeysAnimLH fpart
            (animRH, animLH) = if RTB.null originalRH && RTB.null originalLH
              then (RTB.filter (\case ProKeys.Note _ -> True; _ -> False) keysExpert, RTB.empty)
              else (originalRH, originalLH)
            ffBasic = if fmap gryboFixFreeform (partGRYBO part) == Just True
              then fixFreeformFive
              else id
            ffPro = if fmap pkFixFreeform (partProKeys part) == Just True
              then fixFreeformPK
              else id
            -- nemo's checker doesn't like if you include this stuff on PART KEYS
            removeGtrStuff = RTB.filter $ \case
              Five.FretPosition{} -> False
              Five.HandMap     {} -> False
              Five.StrumMap    {} -> False
              Five.Tremolo     {} -> False
              Five.DiffEvent Easy   Five.Force{} -> False
              Five.DiffEvent Medium Five.Force{} -> False
              _                   -> True
            in  ( ffBasic $ removeGtrStuff basicKeys
                , animRH
                , animLH
                , ffPro $ ProKeys.fixPSRange keysExpert
                ,         ProKeys.fixPSRange keysHard
                ,         ProKeys.fixPSRange keysMedium
                ,         ProKeys.fixPSRange keysEasy
                )

      vocalPart = either rb3_Vocal ps_Vocal target
      (trkVox, trkHarm1, trkHarm2, trkHarm3) = case getPart vocalPart songYaml >>= partVocal of
        Nothing -> (RTB.empty, RTB.empty, RTB.empty, RTB.empty)
        Just pv -> case vocalCount pv of
          Vocal3 -> (partVox', harm1, harm2, harm3)
          Vocal2 -> (partVox', harm1, harm2, RTB.empty)
          Vocal1 -> (partVox', RTB.empty, RTB.empty, RTB.empty)
        where partVox = RBFile.flexPartVocals $ RBFile.getFlexPart vocalPart trks
              partVox' = if RTB.null partVox then harm1ToPartVocals harm1 else partVox
              harm1   = RBFile.flexHarm1 $ RBFile.getFlexPart vocalPart trks
              harm2   = RBFile.flexHarm2 $ RBFile.getFlexPart vocalPart trks
              harm3   = RBFile.flexHarm3 $ RBFile.getFlexPart vocalPart trks

  drumsTrack' <- let
    (fills, notFills) = flip RTB.partitionMaybe drumsTrack $ \case
      RBDrums.Activation b -> Just b
      _ -> Nothing
    fills' = RTB.normalize fills
    fixCloseFills rtb = case RTB.viewL rtb of
      Just ((tx, True), rtb') -> case RTB.viewL rtb' of
        Just ((ty, False), rtb'')
          | tx < 2.5
          -> fixCloseFills $ RTB.delay (tx + ty) rtb''
        _ -> RTB.cons tx True $ fixCloseFills rtb'
      Just ((tx, False), rtb') -> RTB.cons tx False $ fixCloseFills rtb'
      Nothing -> RTB.empty
    fixedFills = U.unapplyTempoTrack tempos $ fixCloseFills $ U.applyTempoTrack tempos fills'
    in do
      when (fills' /= fixedFills) $ warn
        "Removing some drum fills because they are too close (within 2.5 seconds)"
      return $ RTB.merge (fmap RBDrums.Activation fixedFills) notFills

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

  return $ RBFile.Song tempos' mmap'
    ( RBFile.RB3File
      { RBFile.rb3Beat = beatTrack
      , RBFile.rb3Events = eventsTrack
      , RBFile.rb3Venue = RBFile.onyxVenue trks
      , RBFile.rb3PartDrums = drumsTrack'
      , RBFile.rb3PartGuitar = guitarRB3
      , RBFile.rb3PartBass = bassRB3
      , RBFile.rb3PartRealGuitar   = proGtr
      , RBFile.rb3PartRealGuitar22 = proGtr22
      , RBFile.rb3PartRealBass     = proBass
      , RBFile.rb3PartRealBass22   = proBass22
      , RBFile.rb3PartKeys = tk
      , RBFile.rb3PartKeysAnimRH = tkRH
      , RBFile.rb3PartKeysAnimLH = tkLH
      , RBFile.rb3PartRealKeysE = tpkE
      , RBFile.rb3PartRealKeysM = tpkM
      , RBFile.rb3PartRealKeysH = tpkH
      , RBFile.rb3PartRealKeysX = tpkX
      , RBFile.rb3PartVocals = trkVox
      , RBFile.rb3Harm1 = trkHarm1
      , RBFile.rb3Harm2 = trkHarm2
      , RBFile.rb3Harm3 = trkHarm3
      }
    , RBFile.PSFile
      { RBFile.psBeat = beatTrack
      , RBFile.psEvents = eventsTrackPS
      , RBFile.psVenue = RBFile.onyxVenue trks
      , RBFile.psPartDrums = drumsTrack'
      , RBFile.psPartDrums2x = RTB.empty
      , RBFile.psPartRealDrumsPS = RTB.empty
      , RBFile.psPartGuitar = guitarPS
      , RBFile.psPartGuitarGHL = guitarGHL
      , RBFile.psPartBass = bassPS
      , RBFile.psPartBassGHL = bassGHL
      , RBFile.psPartRhythm = rhythmPS
      , RBFile.psPartGuitarCoop = guitarCoopPS
      , RBFile.psPartRealGuitar   = proGtr
      , RBFile.psPartRealGuitar22 = proGtr22
      , RBFile.psPartRealBass     = proBass
      , RBFile.psPartRealBass22   = proBass22
      , RBFile.psPartKeys = tk
      , RBFile.psPartKeysAnimRH = tkRH
      , RBFile.psPartKeysAnimLH = tkLH
      , RBFile.psPartRealKeysE = tpkE
      , RBFile.psPartRealKeysM = tpkM
      , RBFile.psPartRealKeysH = tpkH
      , RBFile.psPartRealKeysX = tpkX
      , RBFile.psPartRealKeysPS_E = RTB.empty
      , RBFile.psPartRealKeysPS_M = RTB.empty
      , RBFile.psPartRealKeysPS_H = RTB.empty
      , RBFile.psPartRealKeysPS_X = RTB.empty
      , RBFile.psPartVocals = RBVox.asciiLyrics <$> trkVox
      , RBFile.psHarm1 = RBVox.asciiLyrics <$> trkHarm1
      , RBFile.psHarm2 = RBVox.asciiLyrics <$> trkHarm2
      , RBFile.psHarm3 = RBVox.asciiLyrics <$> trkHarm3
      }
    )

magmaLegalTempos :: (SendMessage m) => RBFile.Song (RBFile.RB3File U.Beats) -> StackTraceT m (RBFile.Song (RBFile.RB3File U.Beats))
magmaLegalTempos rb3 = let
  endTime = case RTB.viewL $ RTB.filter (== Events.End) $ RBFile.rb3Events $ RBFile.s_tracks rb3 of
    Nothing           -> 0 -- shouldn't happen
    Just ((dt, _), _) -> dt
  in magmaLegalTempos' endTime (RBFile.s_tempos rb3) (RBFile.s_signatures rb3) >>= \case
    (0, _, _, _) -> return rb3
    (numChanges, newTempos, newSigs, TrackAdjust adjuster) -> do
      warn $ "Stretching/squashing " ++ show numChanges ++ " measures to keep tempos in Magma-legal range"
      let events = adjuster $ RBFile.rb3Events $ RBFile.s_tracks rb3
          endPosn = case RTB.getTimes $ RTB.filter (== Events.End) events of
            [bts] -> bts
            _     -> 0 -- eh
      return RBFile.Song
        { RBFile.s_tracks = RBFile.RB3File
          { RBFile.rb3PartDrums        = adjuster $ RBFile.rb3PartDrums        $ RBFile.s_tracks rb3
          , RBFile.rb3PartGuitar       = adjuster $ RBFile.rb3PartGuitar       $ RBFile.s_tracks rb3
          , RBFile.rb3PartBass         = adjuster $ RBFile.rb3PartBass         $ RBFile.s_tracks rb3
          , RBFile.rb3PartKeys         = adjuster $ RBFile.rb3PartKeys         $ RBFile.s_tracks rb3
          , RBFile.rb3PartRealGuitar   = adjuster $ RBFile.rb3PartRealGuitar   $ RBFile.s_tracks rb3
          , RBFile.rb3PartRealGuitar22 = adjuster $ RBFile.rb3PartRealGuitar22 $ RBFile.s_tracks rb3
          , RBFile.rb3PartRealBass     = adjuster $ RBFile.rb3PartRealBass     $ RBFile.s_tracks rb3
          , RBFile.rb3PartRealBass22   = adjuster $ RBFile.rb3PartRealBass22   $ RBFile.s_tracks rb3
          , RBFile.rb3PartRealKeysE    = adjuster $ RBFile.rb3PartRealKeysE    $ RBFile.s_tracks rb3
          , RBFile.rb3PartRealKeysM    = adjuster $ RBFile.rb3PartRealKeysM    $ RBFile.s_tracks rb3
          , RBFile.rb3PartRealKeysH    = adjuster $ RBFile.rb3PartRealKeysH    $ RBFile.s_tracks rb3
          , RBFile.rb3PartRealKeysX    = adjuster $ RBFile.rb3PartRealKeysX    $ RBFile.s_tracks rb3
          , RBFile.rb3PartKeysAnimLH   = adjuster $ RBFile.rb3PartKeysAnimLH   $ RBFile.s_tracks rb3
          , RBFile.rb3PartKeysAnimRH   = adjuster $ RBFile.rb3PartKeysAnimRH   $ RBFile.s_tracks rb3
          , RBFile.rb3PartVocals       = adjuster $ RBFile.rb3PartVocals       $ RBFile.s_tracks rb3
          , RBFile.rb3Harm1            = adjuster $ RBFile.rb3Harm1            $ RBFile.s_tracks rb3
          , RBFile.rb3Harm2            = adjuster $ RBFile.rb3Harm2            $ RBFile.s_tracks rb3
          , RBFile.rb3Harm3            = adjuster $ RBFile.rb3Harm3            $ RBFile.s_tracks rb3
          , RBFile.rb3Events           = events
          , RBFile.rb3Beat             = fixLastBeat endPosn $ adjuster $ RBFile.rb3Beat $ RBFile.s_tracks rb3
          , RBFile.rb3Venue            = adjuster $ RBFile.rb3Venue            $ RBFile.s_tracks rb3
          }
        , RBFile.s_tempos = newTempos
        , RBFile.s_signatures = newSigs
        }

newtype TrackAdjust = TrackAdjust (forall a. RTB.T U.Beats a -> RTB.T U.Beats a)

{-
if a measure has a <40bpm tempo:
- double all tempos inside
- stretch it out to twice the length, moving events appropriately
- bump up the numerator of the measure's time signature by 2, so e.g. 5/4 becomes 10/4
if a measure has a >300bpm tempo:
- halve all tempos inside
- squish it to half the length, moving events appropriately
- bump up the denominator of the measure's time signature by 2, so e.g. 5/4 becomes 5/8
finally, make new default BEAT track if we changed anything
-}
magmaLegalTempos' :: (Monad m) => U.Beats -> U.TempoMap -> U.MeasureMap -> StackTraceT m
  ( Int -- num of measures adjusted
  , U.TempoMap
  , U.MeasureMap
  , TrackAdjust -- adjusts each event track
  )
magmaLegalTempos' endTime tempos sigs = do
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

fixLastBeat :: U.Beats -> RTB.T U.Beats Beat.Event -> RTB.T U.Beats Beat.Event
fixLastBeat endPosn beatTrk = case ATB.viewR $ RTB.toAbsoluteEventList 0 beatTrk of
  Nothing -> beatTrk -- whatever
  Just (_, (beatPosn, _)) -> let
    distance = endPosn -| beatPosn
    in if distance > 1
      then RTB.insert (beatPosn + distance / 2) Beat.Beat beatTrk
      else if distance <= 0.5
        then beatTrk -- might need to remove a BEAT event in this case? dunno
        else beatTrk -- all good

magmaPad
  :: (SendMessage m)
  => RBFile.Song (RBFile.RB3File U.Beats)
  -> StackTraceT m (RBFile.Song (RBFile.RB3File U.Beats), Int)
magmaPad rb3@(RBFile.Song tmap _ trks) = let
  firstEvent rtb = case RTB.viewL rtb of
    Just ((dt, _), _) -> dt
    Nothing           -> 999
  firstNoteBeats = foldr min 999
    [ firstEvent $ RTB.filter drumNote  $ RBFile.rb3PartDrums        trks
    , firstEvent $ RTB.filter gryboNote $ RBFile.rb3PartGuitar       trks
    , firstEvent $ RTB.filter gryboNote $ RBFile.rb3PartBass         trks
    , firstEvent $ RTB.filter gryboNote $ RBFile.rb3PartKeys         trks
    , firstEvent $ RTB.filter ptarNote  $ RBFile.rb3PartRealGuitar   trks
    , firstEvent $ RTB.filter ptarNote  $ RBFile.rb3PartRealGuitar22 trks
    , firstEvent $ RTB.filter ptarNote  $ RBFile.rb3PartRealBass     trks
    , firstEvent $ RTB.filter ptarNote  $ RBFile.rb3PartRealBass22   trks
    , firstEvent $ RTB.filter pkeyNote  $ RBFile.rb3PartRealKeysE    trks
    , firstEvent $ RTB.filter pkeyNote  $ RBFile.rb3PartRealKeysM    trks
    , firstEvent $ RTB.filter pkeyNote  $ RBFile.rb3PartRealKeysH    trks
    , firstEvent $ RTB.filter pkeyNote  $ RBFile.rb3PartRealKeysX    trks
    , firstEvent $ RTB.filter voxNote   $ RBFile.rb3PartVocals       trks
    , firstEvent $ RTB.filter voxNote   $ RBFile.rb3Harm1            trks
    , firstEvent $ RTB.filter voxNote   $ RBFile.rb3Harm2            trks
    , firstEvent $ RTB.filter voxNote   $ RBFile.rb3Harm3            trks
    ]
  drumNote = \case RBDrums.DiffEvent _ (RBDrums.Note _) -> True; _ -> False
  gryboNote = \case Five.DiffEvent _ (Five.Note _) -> True; _ -> False
  ptarNote = \case ProGtr.DiffEvent _ (ProGtr.Note _) -> True; _ -> False
  pkeyNote = \case ProKeys.Note{} -> True; _ -> False
  voxNote = \case RBVox.Note{} -> True; _ -> False
  firstNoteSeconds = U.applyTempoMap tmap firstNoteBeats
  -- magma says 2.45s but account for some float error
  padSeconds = max 0 $ ceiling $ 2.451 - (realToFrac firstNoteSeconds :: Rational)
  in case padSeconds of
    0 -> do
      return (rb3, 0)
    _ -> do
      warn $ "Padding song by " ++ show padSeconds ++ "s due to early notes."
      return (RBFile.padRB3MIDI padSeconds rb3, padSeconds)

findProblems :: RBFile.Song (RBFile.OnyxFile U.Beats) -> [String]
findProblems song = execWriter $ do
  -- Every discobeat mix event should be simultaneous with,
  -- or immediately followed by, a set of notes not including red or yellow.
  let drums = RBFile.flexPartDrums $ RBFile.getFlexPart RBFile.FlexDrums $ RBFile.s_tracks song
      discos = flip RTB.mapMaybe drums $ \case
        RBDrums.DiffEvent d (RBDrums.Mix _ RBDrums.Disco) -> Just d
        _ -> Nothing
      badDiscos = void $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ filter isBadDisco $ ATB.toPairList $ RTB.toAbsoluteEventList 0 discos
      drumsDiff d = flip RTB.mapMaybe drums $ \case
        RBDrums.DiffEvent d' (RBDrums.Note gem) | d == d' -> Just gem
        _ -> Nothing
      isBadDisco (t, diff) = case RTB.viewL $ RTB.collectCoincident $ U.trackDrop t $ drumsDiff diff of
        Just ((_, evts), _) | any isDiscoGem evts -> True
        _                   -> False
      isDiscoGem = \case
        RBDrums.Red -> True
        RBDrums.Pro RBDrums.Yellow _ -> True
        _ -> False
  -- Don't have a vocal phrase that ends simultaneous with a lyric event.
  -- In static vocals, this puts the lyric in the wrong phrase.
  let vox = RBFile.flexPartVocals $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
      harm1 = RBFile.flexHarm1 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
      harm2 = RBFile.flexHarm2 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
      harm3 = RBFile.flexHarm3 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
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
