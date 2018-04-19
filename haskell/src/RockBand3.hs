{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module RockBand3 (processRB3Pad, processPS, findProblems, TrackAdjust(..), magmaLegalTempos') where

import           Config
import           Control.Monad.Extra
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer       (execWriter, tell)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Maybe                       (isJust)
import           Guitars
import           OneFoot
import           Overdrive                        (fixBrokenUnisons)
import           ProKeysRanges
import           Reductions
import           RockBand.Codec                   (mapTrack)
import           RockBand.Codec.Beat
import           RockBand.Codec.Drums
import           RockBand.Codec.Events
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.Venue             (compileVenueRB3)
import           RockBand.Common
import qualified RockBand.Drums                   as RBDrums
import qualified RockBand.Events                  as Events
import qualified RockBand.FiveButton              as Five
import qualified RockBand.GHL                     as Six
import qualified RockBand.ProGuitar               as ProGtr
import qualified RockBand.ProKeys                 as ProKeys
import           RockBand.Sections                (makePSSection)
import qualified RockBand.Vocals                  as RBVox
import           Scripts
import qualified Sound.MIDI.Util                  as U

processRB3Pad
  :: TargetRB3
  -> SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> Staction U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> Staction (RBFile.Song (RBFile.FixedFile U.Beats), Int)
processRB3Pad a b c d e = do
  res <- processMIDI (Left a) b c d e
  magmaLegalTempos (fmap fst res) >>= fixBrokenUnisons >>= magmaPad . fixBeatTrack'

processPS
  :: TargetPS
  -> SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> Staction U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> Staction (RBFile.Song (RBFile.FixedFile U.Beats))
processPS a b c d e = fmap (fmap snd) $ processMIDI (Right a) b c d e

processMIDI
  :: Either TargetRB3 TargetPS
  -> SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> Staction U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> Staction (RBFile.Song (RBFile.FixedFile U.Beats, RBFile.FixedFile U.Beats))
processMIDI target songYaml input@(RBFile.Song tempos mmap trks) mixMode getAudioLength = inside "Processing MIDI for RB3/PS" $ do
  let showPosition = RBFile.showPosition . U.applyMeasureMap mmap
      eventsInput = RBFile.onyxEvents trks
  -- If there's no [end], put it after all MIDI events and audio files.
  endPosn <- case RTB.viewL $ eventsEnd eventsInput of
    Just ((t, _), _) -> return t
    Nothing -> do
      audLen <- U.unapplyTempoMap tempos <$> getAudioLength
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
  musicStartPosn <- case RTB.viewL $ eventsMusicStart eventsInput of
    Just ((t, _), _) -> if t < musicStartMin
      then do
        warn $ "[music_start] is too early. Moving to " ++ showPosition musicStartMin
        return musicStartMin
      else return t
    Nothing -> do
      warn $ "[music_start] is missing. Placing at " ++ showPosition musicStartMin
      return musicStartMin
  musicEndPosn <- case RTB.viewL $ eventsMusicEnd eventsInput of
    Just ((t, _), _) -> return t
    Nothing -> do
      warn $ unwords
        [ "[music_end] is missing. [end] is at"
        , showPosition endPosn
        , "so [music_end] will be at"
        , showPosition $ endPosn - 2
        ]
      return $ endPosn - 2
  let (crowd, crowdClap) = if RTB.null (eventsCrowd eventsInput) && RTB.null (eventsCrowdClap eventsInput)
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
        Nothing -> RTB.empty
        Just pd -> let
          psKicks = if drumsAuto2xBass pd
            then U.unapplyTempoTrack tempos . phaseShiftKicks 0.18 0.11 . U.applyTempoTrack tempos
            else id
          sections = fmap snd $ eventsSections eventsInput
          finish = RBDrums.drumsFromLegacy . changeMode . psKicks . drumMix_precodec mixMode . drumsComplete mmap sections . RBDrums.drumsToLegacy
          fiveToFour instant = flip map instant $ \case
            RBDrums.Note RBDrums.Orange -> let
              color = if
                | RBDrums.Note (RBDrums.Pro RBDrums.Blue  ()) `elem` instant -> RBDrums.Green
                | RBDrums.Note (RBDrums.Pro RBDrums.Green ()) `elem` instant -> RBDrums.Blue
                | otherwise -> case drumsFallback pd of
                  FallbackBlue  -> RBDrums.Blue
                  FallbackGreen -> RBDrums.Green
              in RBDrums.Note $ RBDrums.Pro color ()
            x -> x
          noToms = RTB.filter (\case RBDrums.ProType{} -> False; _ -> True)
          allToms
            = RTB.insert 0       (RBDrums.ProType RBDrums.Yellow RBDrums.Tom   )
            . RTB.insert 0       (RBDrums.ProType RBDrums.Blue   RBDrums.Tom   )
            . RTB.insert 0       (RBDrums.ProType RBDrums.Green  RBDrums.Tom   )
            . RTB.insert endPosn (RBDrums.ProType RBDrums.Yellow RBDrums.Cymbal)
            . RTB.insert endPosn (RBDrums.ProType RBDrums.Blue   RBDrums.Cymbal)
            . RTB.insert endPosn (RBDrums.ProType RBDrums.Green  RBDrums.Cymbal)
            . noToms
          changeMode = case (drumsMode pd, target) of
            (DrumsPro, _         ) -> id
            -- TODO convert 5 to pro, not just basic.
            (Drums5  , Left  _rb3) -> allToms
              . eachDifficulty (RTB.flatten . fmap fiveToFour . RTB.collectCoincident)
            (Drums5  , Right _ps ) -> noToms
            (Drums4  , Right _ps ) -> noToms
            (Drums4  , Left  _rb3) -> allToms
          flex = RBFile.getFlexPart drumsPart trks
          trk1x = RBFile.onyxPartDrums flex
          trk2x = RBFile.onyxPartDrums2x flex
          trkReal = RBFile.onyxPartRealDrumsPS flex
          trkReal' = RBDrums.psRealToPro trkReal
          onlyPSReal = all (== mempty) [trk1x, trk2x] && not (trkReal == mempty)
          pro1x = if onlyPSReal then trkReal' else trk1x
          pro2x = if onlyPSReal then trkReal' else trk2x
          ps1x = finish $ if pro1x == mempty then pro2x else pro1x
          ps2x = finish $ if pro2x == mempty then pro1x else pro2x
          psPS = if not $ RTB.null $ drumKick2x pro1x then ps1x else ps2x
          -- Note: drumMix must be applied *after* drumsComplete.
          -- Otherwise the automatic EMH mix events could prevent lower difficulty generation.
          in (if drumsFixFreeform pd then fixFreeformDrums_precodec else id)
            $ RTB.filter (\case RBDrums.Player1{} -> False; RBDrums.Player2{} -> False; _ -> True)
            $ case target of
              Left rb3 -> if rb3_2xBassPedal rb3
                then rockBand2x $ RBDrums.drumsToLegacy ps2x
                else rockBand1x $ RBDrums.drumsToLegacy ps1x
              Right _ -> RBDrums.drumsToLegacy psPS
      makeGRYBOTrack toKeys fpart src = case getPart fpart songYaml >>= partGRYBO of
        Nothing -> (RTB.empty, RTB.empty)
        Just grybo -> let
          (trackOrig, isKeys) = if RBFile.onyxPartGuitar src == mempty
            then (RBFile.onyxPartKeys src, True)
            else (RBFile.onyxPartGuitar src, False)
          track
            = RTB.filter (\case Five.Player1{} -> False; Five.Player2{} -> False; _ -> True)
            $ (if gryboFixFreeform grybo then fixFreeformFive_precodec else id)
            $ gryboComplete (guard toKeys >> Just ht) mmap trackOrig
          ht = gryboHopoThreshold grybo
          algo = if isKeys then HOPOsRBKeys else HOPOsRBGuitar
          forRB3 = eachDifficulty
            $ emit5
            . fromClosed
            . no5NoteChords
            . noOpenNotes (gryboDropOpenHOPOs grybo)
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
        then (\x -> (x, x)) $ protarToGrybo_precodec proGtr
        else makeGRYBOTrack False guitarPart $ RBFile.getFlexPart guitarPart trks

      bassPart = either rb3_Bass ps_Bass target
      (bassRB3, bassPS) = if hasProtarNotFive bassPart
        then (\x -> (x, x)) $ protarToGrybo_precodec proBass
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
          ) $ RBFile.onyxPartSix $ RBFile.getFlexPart guitarPart trks
      bassGHL = case getPart bassPart songYaml >>= partGHL of
        Nothing -> RTB.empty
        Just ghl -> eachDifficulty
          ( emit6
          . strumHOPOTap HOPOsRBGuitar (fromIntegral (ghlHopoThreshold ghl) / 480)
          . ghlNotes
          ) $ RBFile.onyxPartSix $ RBFile.getFlexPart bassPart trks

      -- TODO: pgHopoThreshold
      makeProGtrTracks fpart = case getPart fpart songYaml >>= partProGuitar of
        Nothing -> (RTB.empty, RTB.empty)
        Just pg -> let
          src = RBFile.getFlexPart fpart trks
          tuning = zipWith (+) ProGtr.standardGuitar $ case pgTuning pg of
            []   -> repeat 0
            offs -> offs
          f = (if pgFixFreeform pg then fixFreeformPG_precodec else id)
            . copyExpert . ProGtr.autoHandPosition . ProGtr.autoChordRoot tuning
          src17 = RBFile.onyxPartRealGuitar   src
          src22 = RBFile.onyxPartRealGuitar22 src
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
              Nothing -> expertProKeysToKeys_precodec keysExpert
              Just _  -> fst $ makeGRYBOTrack True keysPart basicKeysSrc
            fpart = RBFile.getFlexPart keysPart trks
            keysDiff diff = if isJust $ partProKeys part
              then case diff of
                Easy   -> RBFile.onyxPartRealKeysE fpart
                Medium -> RBFile.onyxPartRealKeysM fpart
                Hard   -> RBFile.onyxPartRealKeysH fpart
                Expert -> RBFile.onyxPartRealKeysX fpart
              else keysToProKeys_precodec diff basicKeys
            rtb1 `orIfNull` rtb2 = if length rtb1 < 5 then rtb2 else rtb1
            keysExpert = completeRanges $ keysDiff Expert
            keysHard   = completeRanges $ keysDiff Hard   `orIfNull` pkReduce Hard   mmap keysOD keysExpert
            keysMedium = completeRanges $ keysDiff Medium `orIfNull` pkReduce Medium mmap keysOD keysHard
            keysEasy   = completeRanges $ keysDiff Easy   `orIfNull` pkReduce Easy   mmap keysOD keysMedium
            keysOD = flip RTB.mapMaybe keysExpert $ \case
              ProKeys.Overdrive b -> Just b
              _                   -> Nothing
            originalRH = RBFile.onyxPartKeysAnimRH fpart
            originalLH = RBFile.onyxPartKeysAnimLH fpart
            (animRH, animLH) = if RTB.null originalRH && RTB.null originalLH
              then (RTB.filter (\case ProKeys.Note _ -> True; _ -> False) keysExpert, RTB.empty)
              else (originalRH, originalLH)
            ffBasic = if fmap gryboFixFreeform (partGRYBO part) == Just True
              then fixFreeformFive_precodec
              else id
            ffPro = if fmap pkFixFreeform (partProKeys part) == Just True
              then fixFreeformPK_precodec
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
        where partVox = RBFile.onyxPartVocals $ RBFile.getFlexPart vocalPart trks
              partVox' = if RTB.null partVox then harm1ToPartVocals_precodec harm1 else partVox
              harm1   = RBFile.onyxHarm1 $ RBFile.getFlexPart vocalPart trks
              harm2   = RBFile.onyxHarm2 $ RBFile.getFlexPart vocalPart trks
              harm3   = RBFile.onyxHarm3 $ RBFile.getFlexPart vocalPart trks

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
    ( mempty
      { RBFile.fixedBeat = beatTrack
      , RBFile.fixedEvents = eventsTrack
      , RBFile.fixedVenue = compileVenueRB3 $ RBFile.onyxVenue trks
      , RBFile.fixedPartDrums = RBDrums.drumsFromLegacy drumsTrack'
      , RBFile.fixedPartGuitar = Five.fiveFromLegacy guitarRB3
      , RBFile.fixedPartBass = Five.fiveFromLegacy bassRB3
      , RBFile.fixedPartRealGuitar   = ProGtr.pgFromLegacy proGtr
      , RBFile.fixedPartRealGuitar22 = ProGtr.pgFromLegacy proGtr22
      , RBFile.fixedPartRealBass     = ProGtr.pgFromLegacy proBass
      , RBFile.fixedPartRealBass22   = ProGtr.pgFromLegacy proBass22
      , RBFile.fixedPartKeys = Five.fiveFromLegacy tk
      , RBFile.fixedPartKeysAnimRH = ProKeys.pkFromLegacy tkRH
      , RBFile.fixedPartKeysAnimLH = ProKeys.pkFromLegacy tkLH
      , RBFile.fixedPartRealKeysE = ProKeys.pkFromLegacy tpkE
      , RBFile.fixedPartRealKeysM = ProKeys.pkFromLegacy tpkM
      , RBFile.fixedPartRealKeysH = ProKeys.pkFromLegacy tpkH
      , RBFile.fixedPartRealKeysX = ProKeys.pkFromLegacy tpkX
      , RBFile.fixedPartVocals = RBVox.vocalFromLegacy trkVox
      , RBFile.fixedHarm1 = RBVox.vocalFromLegacy trkHarm1
      , RBFile.fixedHarm2 = RBVox.vocalFromLegacy trkHarm2
      , RBFile.fixedHarm3 = RBVox.vocalFromLegacy trkHarm3
      }
    , RBFile.FixedFile
      { RBFile.fixedBeat = beatTrack
      , RBFile.fixedEvents = eventsTrackPS
      , RBFile.fixedVenue = compileVenueRB3 $ RBFile.onyxVenue trks
      , RBFile.fixedPartDrums = RBDrums.drumsFromLegacy drumsTrack'
      , RBFile.fixedPartDrums2x = mempty
      , RBFile.fixedPartRealDrumsPS = mempty
      , RBFile.fixedPartGuitar = Five.fiveFromLegacy guitarPS
      , RBFile.fixedPartGuitarGHL = Six.sixFromLegacy guitarGHL
      , RBFile.fixedPartBass = Five.fiveFromLegacy bassPS
      , RBFile.fixedPartBassGHL = Six.sixFromLegacy bassGHL
      , RBFile.fixedPartRhythm = Five.fiveFromLegacy rhythmPS
      , RBFile.fixedPartGuitarCoop = Five.fiveFromLegacy guitarCoopPS
      , RBFile.fixedPartRealGuitar   = ProGtr.pgFromLegacy proGtr
      , RBFile.fixedPartRealGuitar22 = ProGtr.pgFromLegacy proGtr22
      , RBFile.fixedPartRealBass     = ProGtr.pgFromLegacy proBass
      , RBFile.fixedPartRealBass22   = ProGtr.pgFromLegacy proBass22
      , RBFile.fixedPartKeys = Five.fiveFromLegacy tk
      , RBFile.fixedPartKeysAnimRH = ProKeys.pkFromLegacy tkRH
      , RBFile.fixedPartKeysAnimLH = ProKeys.pkFromLegacy tkLH
      , RBFile.fixedPartRealKeysE = ProKeys.pkFromLegacy tpkE
      , RBFile.fixedPartRealKeysM = ProKeys.pkFromLegacy tpkM
      , RBFile.fixedPartRealKeysH = ProKeys.pkFromLegacy tpkH
      , RBFile.fixedPartRealKeysX = ProKeys.pkFromLegacy tpkX
      , RBFile.fixedPartVocals = RBVox.vocalFromLegacy $ RBVox.asciiLyrics <$> trkVox
      , RBFile.fixedHarm1 = RBVox.vocalFromLegacy $ RBVox.asciiLyrics <$> trkHarm1
      , RBFile.fixedHarm2 = RBVox.vocalFromLegacy $ RBVox.asciiLyrics <$> trkHarm2
      , RBFile.fixedHarm3 = RBVox.vocalFromLegacy $ RBVox.asciiLyrics <$> trkHarm3
      }
    )

magmaLegalTempos :: (SendMessage m) => RBFile.Song (RBFile.FixedFile U.Beats) -> StackTraceT m (RBFile.Song (RBFile.FixedFile U.Beats))
magmaLegalTempos rb3 = let
  endTime = case RTB.viewL $ eventsEnd $ RBFile.fixedEvents $ RBFile.s_tracks rb3 of
    Nothing           -> 0 -- shouldn't happen
    Just ((dt, _), _) -> dt
  in magmaLegalTempos' endTime (RBFile.s_tempos rb3) (RBFile.s_signatures rb3) >>= \case
    (0, _, _, _) -> return rb3
    (numChanges, newTempos, newSigs, TrackAdjust adjuster) -> do
      warn $ "Stretching/squashing " ++ show numChanges ++ " measures to keep tempos in Magma-legal range"
      return $ RBFile.Song newTempos newSigs $ mapTrack adjuster $ RBFile.s_tracks rb3

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

fixBeatTrack' :: RBFile.Song (RBFile.FixedFile U.Beats) -> RBFile.Song (RBFile.FixedFile U.Beats)
fixBeatTrack' rb3 = let
  endTime = case RTB.viewL $ eventsEnd $ RBFile.fixedEvents $ RBFile.s_tracks rb3 of
    Nothing           -> 0 -- shouldn't happen
    Just ((dt, _), _) -> dt
  in rb3
    { RBFile.s_tracks = (RBFile.s_tracks rb3)
      { RBFile.fixedBeat = BeatTrack $ fixBeatTrack endTime $ beatLines $ RBFile.fixedBeat $ RBFile.s_tracks rb3
      }
    }

fixBeatTrack :: U.Beats -> RTB.T U.Beats BeatEvent -> RTB.T U.Beats BeatEvent
fixBeatTrack endPosn trk = let
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
  in fixDoubleDownbeat $ fixLastBeat $ fixFastBeats $ U.trackTake (endPosn - (14/480)) trk

magmaPad
  :: (SendMessage m)
  => RBFile.Song (RBFile.FixedFile U.Beats)
  -> StackTraceT m (RBFile.Song (RBFile.FixedFile U.Beats), Int)
magmaPad rb3@(RBFile.Song tmap _ trks) = let
  firstEvent rtb = case RTB.viewL rtb of
    Just ((dt, _), _) -> dt
    Nothing           -> 999
  firstNoteBeats = foldr min 999
    [ firstEvent $ RTB.filter drumNote  $ RBDrums.drumsToLegacy $ RBFile.fixedPartDrums        trks
    , firstEvent $ RTB.filter gryboNote $ Five.fiveToLegacy     $ RBFile.fixedPartGuitar       trks
    , firstEvent $ RTB.filter gryboNote $ Five.fiveToLegacy     $ RBFile.fixedPartBass         trks
    , firstEvent $ RTB.filter gryboNote $ Five.fiveToLegacy     $ RBFile.fixedPartKeys         trks
    , firstEvent $ RTB.filter ptarNote  $ ProGtr.pgToLegacy     $ RBFile.fixedPartRealGuitar   trks
    , firstEvent $ RTB.filter ptarNote  $ ProGtr.pgToLegacy     $ RBFile.fixedPartRealGuitar22 trks
    , firstEvent $ RTB.filter ptarNote  $ ProGtr.pgToLegacy     $ RBFile.fixedPartRealBass     trks
    , firstEvent $ RTB.filter ptarNote  $ ProGtr.pgToLegacy     $ RBFile.fixedPartRealBass22   trks
    , firstEvent $ RTB.filter pkeyNote  $ ProKeys.pkToLegacy    $ RBFile.fixedPartRealKeysE    trks
    , firstEvent $ RTB.filter pkeyNote  $ ProKeys.pkToLegacy    $ RBFile.fixedPartRealKeysM    trks
    , firstEvent $ RTB.filter pkeyNote  $ ProKeys.pkToLegacy    $ RBFile.fixedPartRealKeysH    trks
    , firstEvent $ RTB.filter pkeyNote  $ ProKeys.pkToLegacy    $ RBFile.fixedPartRealKeysX    trks
    , firstEvent $ RTB.filter voxNote   $ RBVox.vocalToLegacy   $ RBFile.fixedPartVocals       trks
    , firstEvent $ RTB.filter voxNote   $ RBVox.vocalToLegacy   $ RBFile.fixedHarm1            trks
    , firstEvent $ RTB.filter voxNote   $ RBVox.vocalToLegacy   $ RBFile.fixedHarm2            trks
    , firstEvent $ RTB.filter voxNote   $ RBVox.vocalToLegacy   $ RBFile.fixedHarm3            trks
    ]
  drumNote = \case RBDrums.DiffEvent _ (RBDrums.Note _) -> True; _ -> False
  gryboNote = \case Five.DiffEvent _ (Five.Note _) -> True; _ -> False
  ptarNote = \case ProGtr.DiffEvent _ (ProGtr.Note _) -> True; _ -> False
  pkeyNote = \case ProKeys.Note{} -> True; _ -> False
  voxNote = \case RBVox.Note{} -> True; _ -> False
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
  let drums = RBDrums.drumsToLegacy $ RBFile.onyxPartDrums $ RBFile.getFlexPart RBFile.FlexDrums $ RBFile.s_tracks song
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
