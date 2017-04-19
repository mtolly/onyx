{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module RockBand3 (MIDIProcessor(..), findProblems) where

import           Config                           hiding (Target (PS))
import           Control.Monad.Extra
import           Control.Monad.Trans.Writer       (execWriter, tell)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Maybe                       (isJust)
import           Development.Shake
import           OneFoot
import           ProKeysRanges
import           Reductions
import           RockBand.Common
import qualified RockBand.Drums                   as RBDrums
import qualified RockBand.Events                  as Events
import qualified RockBand.File                    as RBFile
import qualified RockBand.FiveButton              as Five
import           RockBand.PhaseShiftMessage       (PSWrap (..), discardPS,
                                                   psMessages)
import qualified RockBand.ProGuitar               as ProGtr
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Vocals                  as RBVox
import           Scripts
import qualified Sound.MIDI.Util                  as U

class MIDIProcessor target output | target -> output where
  processMIDI
    :: target
    -> SongYaml
    -> RBFile.Song (RBFile.OnyxFile U.Beats)
    -> RBDrums.Audio
    -> Action U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
    -> Action (RBFile.Song (output U.Beats))

instance MIDIProcessor TargetRB3 RBFile.RB3File where
  processMIDI a b c d e = fmap (fmap fst) $ processRB3 (Left a) b c d e

instance MIDIProcessor TargetPS RBFile.PSFile where
  processMIDI a b c d e = fmap (fmap snd) $ processRB3 (Right a) b c d e

processRB3
  :: Either TargetRB3 TargetPS
  -> SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> RBDrums.Audio
  -> Action U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> Action (RBFile.Song (RBFile.RB3File U.Beats, RBFile.PSFile U.Beats))
processRB3 target songYaml input@(RBFile.Song tempos mmap trks) mixMode getAudioLength = do
  let showPosition = RBFile.showPosition . U.applyMeasureMap mmap
      eventsRaw = discardPS $ RBFile.onyxEvents trks
      eventsList = ATB.toPairList $ RTB.toAbsoluteEventList 0 eventsRaw
      -- if _fixFreeform $ _options songYaml then fixRolls input else input
  -- If there's no [end], put it after all MIDI events and audio files.
  endPosn' <- case [ t | (t, Events.End) <- eventsList ] of
    t : _ -> return t
    [] -> do
      audLen <- U.unapplyTempoMap tempos <$> getAudioLength
      let absTimes = ATB.getTimes . RTB.toAbsoluteEventList 0
          lastMIDIEvent = foldr max 0
            $ concatMap absTimes (RBFile.s_tracks $ RBFile.showMIDITracks input)
            ++ absTimes (U.tempoMapToBPS tempos)
          endPosition = fromInteger $ round $ max audLen lastMIDIEvent + 4
      putNormal $ unwords
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
    trk = discardPS $ RBFile.onyxBeat trks
    in if RTB.null trk
      then do
        putNormal "Generating a BEAT track..."
        let alignedEnd = fromInteger $ ceiling endPosn'
        return (U.trackTake alignedEnd $ makeBeatTrack mmap, alignedEnd)
      else return (trk, endPosn')
  -- If [music_start] is before 2 beats,
  -- Magma will add auto [idle] events there in instrument tracks, and then error...
  musicStartPosn <- case [ t | (t, Events.MusicStart) <- eventsList ] of
    t : _ -> if t < 2
      then do
        putNormal $ "[music_start] is too early. Moving to " ++ showPosition 2
        return 2
      else return t
    []    -> do
      putNormal $ "[music_start] is missing. Placing at " ++ showPosition 2
      return 2
  musicEndPosn <- case [ t | (t, Events.MusicEnd) <- eventsList ] of
    t : _ -> return t
    []    -> do
      putNormal $ unwords
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
      drumsPart = either rb3_Drums ps_Drums target
      drumsTrack = case getPart drumsPart songYaml >>= partDrums of
        Nothing -> RTB.empty
        Just pd -> let
          trk1x = discardPS $ RBFile.flexPartDrums $ RBFile.getFlexPart drumsPart trks
          trk2x = discardPS $ RBFile.flexPartDrums2x $ RBFile.getFlexPart drumsPart trks
          psKicks = if drumsAuto2xBass pd
            then U.unapplyTempoTrack tempos . phaseShiftKicks 0.18 0.11 . U.applyTempoTrack tempos
            else id
          sections = flip RTB.mapMaybe eventsRaw $ \case
            Events.PracticeSection s -> Just s
            _                        -> Nothing
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
      guitarPart = either rb3_Guitar ps_Guitar target
      guitarMsgs = psMessages $ RBFile.flexFiveButton $ RBFile.getFlexPart guitarPart trks
      guitarSrc = RBFile.getFlexPart guitarPart trks
      guitarTrack = case getPart guitarPart songYaml >>= partGRYBO of
        Nothing -> RTB.empty
        Just grybo -> (if RBFile.flexFiveIsKeys guitarSrc then Five.keysToGuitar (fromIntegral (gryboHopoThreshold grybo) / 480) else id)
          $ (if gryboFixFreeform grybo then fixFreeformFive else id)
          $ gryboComplete (Just $ gryboHopoThreshold grybo) mmap
          $ discardPS $ RBFile.flexFiveButton guitarSrc
      bassPart = either rb3_Bass ps_Bass target
      bassMsgs = psMessages $ RBFile.flexFiveButton $ RBFile.getFlexPart bassPart trks
      bassSrc = RBFile.getFlexPart bassPart trks
      bassTrack = case getPart bassPart songYaml >>= partGRYBO of
        Nothing -> RTB.empty
        Just grybo -> (if RBFile.flexFiveIsKeys bassSrc then Five.keysToGuitar $ fromIntegral (gryboHopoThreshold grybo) / 480 else id)
          $ (if gryboFixFreeform grybo then fixFreeformFive else id)
          $ gryboComplete (Just $ gryboHopoThreshold grybo) mmap
          $ discardPS $ RBFile.flexFiveButton bassSrc
      (proGtr, proGtr22) = case getPart guitarPart songYaml >>= partProGuitar of
        Nothing -> (RTB.empty, RTB.empty)
        Just _pg -> let
          src = RBFile.getFlexPart guitarPart trks
          -- TODO: pgFixFreeform
          f = ProGtr.copyExpert . ProGtr.autoHandPosition . discardPS
          in (f $ RBFile.flexPartRealGuitar src, f $ RBFile.flexPartRealGuitar22 src)
      (proBass, proBass22) = case getPart bassPart songYaml >>= partProGuitar of
        Nothing -> (RTB.empty, RTB.empty)
        Just _pg -> let
          src = RBFile.getFlexPart bassPart trks
          -- TODO: pgFixFreeform
          f = ProGtr.copyExpert . ProGtr.autoHandPosition . discardPS
          in (f $ RBFile.flexPartRealGuitar src, f $ RBFile.flexPartRealGuitar22 src)
      keysPart = either rb3_Keys ps_Keys target
      (tk, tkRH, tkLH, tpkX, tpkH, tpkM, tpkE) = case getPart keysPart songYaml of
        Nothing -> (RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty)
        Just part -> case (partGRYBO part, partProKeys part) of
          (Nothing, Nothing) -> (RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty)
          _ -> let
            basicKeysSrc = RBFile.getFlexPart keysPart trks
            basicKeys = gryboComplete Nothing mmap $ case partGRYBO part of
              Nothing -> expertProKeysToKeys keysExpert
              Just grybo
                -> (if RBFile.flexFiveIsKeys basicKeysSrc then id else Five.forceAllNotes $ fromIntegral (gryboHopoThreshold grybo) / 480)
                $ discardPS $ RBFile.flexFiveButton basicKeysSrc
            keysDiff diff = if isJust $ partProKeys part
              then discardPS $ case diff of
                Easy   -> RBFile.flexPartRealKeysE $ RBFile.getFlexPart keysPart trks
                Medium -> RBFile.flexPartRealKeysM $ RBFile.getFlexPart keysPart trks
                Hard   -> RBFile.flexPartRealKeysH $ RBFile.getFlexPart keysPart trks
                Expert -> RBFile.flexPartRealKeysX $ RBFile.getFlexPart keysPart trks
              else keysToProKeys diff basicKeys
            rtb1 `orIfNull` rtb2 = if length rtb1 < 5 then rtb2 else rtb1
            keysExpert = completeRanges $ keysDiff Expert
            keysHard   = completeRanges $ keysDiff Hard   `orIfNull` pkReduce Hard   mmap keysOD keysExpert
            keysMedium = completeRanges $ keysDiff Medium `orIfNull` pkReduce Medium mmap keysOD keysHard
            keysEasy   = completeRanges $ keysDiff Easy   `orIfNull` pkReduce Easy   mmap keysOD keysMedium
            keysOD = flip RTB.mapMaybe keysExpert $ \case
              ProKeys.Overdrive b -> Just b
              _                   -> Nothing
            keysAnim = flip RTB.filter keysExpert $ \case
              ProKeys.Note _ -> True
              _              -> False
            ffBasic = if fmap gryboFixFreeform (partGRYBO part) == Just True
              then fixFreeformFive
              else id
            ffPro = if fmap pkFixFreeform (partProKeys part) == Just True
              then fixFreeformPK
              else id
            in  ( ffBasic basicKeys
                , keysAnim
                , RTB.empty
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
        where partVox = discardPS $ RBFile.flexPartVocals $ RBFile.getFlexPart vocalPart trks
              partVox' = if RTB.null partVox then harm1ToPartVocals harm1 else partVox
              harm1   = discardPS $ RBFile.flexHarm1 $ RBFile.getFlexPart vocalPart trks
              harm2   = discardPS $ RBFile.flexHarm2 $ RBFile.getFlexPart vocalPart trks
              harm3   = discardPS $ RBFile.flexHarm3 $ RBFile.getFlexPart vocalPart trks
  return $ RBFile.Song tempos mmap
    ( RBFile.RB3File
      { RBFile.rb3Beat = beatTrack
      , RBFile.rb3Events = eventsTrack
      , RBFile.rb3Venue = discardPS $ RBFile.onyxVenue trks
      , RBFile.rb3PartDrums = drumsTrack
      , RBFile.rb3PartGuitar = guitarTrack
      , RBFile.rb3PartBass = bassTrack
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
      { RBFile.psBeat = fmap RB beatTrack
      , RBFile.psEvents = fmap RB eventsTrack
      , RBFile.psVenue = RBFile.onyxVenue trks
      , RBFile.psPartDrums = fmap RB drumsTrack
      , RBFile.psPartGuitar = RTB.merge (fmap RB guitarTrack) (fmap PS guitarMsgs)
      , RBFile.psPartBass = RTB.merge (fmap RB bassTrack) (fmap PS bassMsgs)
      , RBFile.psPartRealGuitar   = fmap RB proGtr
      , RBFile.psPartRealGuitar22 = fmap RB proGtr22
      , RBFile.psPartRealBass     = fmap RB proBass
      , RBFile.psPartRealBass22   = fmap RB proBass22
      , RBFile.psPartKeys = fmap RB tk
      , RBFile.psPartKeysAnimRH = fmap RB tkRH
      , RBFile.psPartKeysAnimLH = fmap RB tkLH
      , RBFile.psPartRealKeysE = fmap RB tpkE
      , RBFile.psPartRealKeysM = fmap RB tpkM
      , RBFile.psPartRealKeysH = fmap RB tpkH
      , RBFile.psPartRealKeysX = fmap RB tpkX
      , RBFile.psPartRealKeysPsE = RTB.empty
      , RBFile.psPartRealKeysPsM = RTB.empty
      , RBFile.psPartRealKeysPsH = RTB.empty
      , RBFile.psPartRealKeysPsX = RTB.empty
      , RBFile.psPartVocals = fmap RB trkVox
      , RBFile.psHarm1 = fmap RB trkHarm1
      , RBFile.psHarm2 = fmap RB trkHarm2
      , RBFile.psHarm3 = fmap RB trkHarm3
      }
    )

findProblems :: RBFile.Song (RBFile.OnyxFile U.Beats) -> [String]
findProblems song = execWriter $ do
  -- Don't have a kick at the start of a drum roll.
  -- It screws up the roll somehow and causes spontaneous misses.
  let drums = discardPS $ RBFile.flexPartDrums $ RBFile.getFlexPart RBFile.FlexDrums $ RBFile.s_tracks song
      kickSwells = flip RTB.mapMaybe (RTB.collectCoincident drums) $ \evts -> do
        let kick = RBDrums.DiffEvent Expert $ RBDrums.Note RBDrums.Kick
            swell1 = RBDrums.SingleRoll True
            swell2 = RBDrums.DoubleRoll True
        guard $ elem kick evts && (elem swell1 evts || elem swell2 evts)
        return ()
  -- Every discobeat mix event should be simultaneous with,
  -- or immediately followed by, a set of notes not including red or yellow.
  let discos = flip RTB.mapMaybe drums $ \case
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
  let vox = discardPS $ RBFile.flexPartVocals $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
      harm1 = discardPS $ RBFile.flexHarm1 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
      harm2 = discardPS $ RBFile.flexHarm2 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
      harm3 = discardPS $ RBFile.flexHarm3 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song
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
  message kickSwells "kick note is simultaneous with start of drum roll"
  message badDiscos "discobeat drum event is followed immediately by red or yellow gem"
  message voxBugs "PART VOCALS vocal phrase ends simultaneous with a lyric"
  message harm1Bugs "HARM1 vocal phrase ends simultaneous with a lyric"
  message harm2Bugs "HARM2 vocal phrase ends simultaneous with a (HARM2 or HARM3) lyric"
