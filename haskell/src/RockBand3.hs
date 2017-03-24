{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module RockBand3 (Kicks(..), processMIDI, findProblems) where

import           Config                           hiding (Target (PS))
import           Control.Monad.Extra
import           Control.Monad.Trans.Writer       (execWriter, tell)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Development.Shake
import           OneFoot
import           ProKeysRanges
import           Reductions
import           RockBand.Common
import qualified RockBand.Drums                   as RBDrums
import qualified RockBand.Events                  as Events
import qualified RockBand.File                    as RBFile
import           RockBand.PhaseShiftMessage       (PSWrap (..), discardPS,
                                                   psMessages)
import qualified RockBand.ProGuitar               as ProGtr
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Vocals                  as RBVox
import           Scripts
import qualified Sound.MIDI.Util                  as U

data Kicks = Kicks1x | Kicks2x | KicksPS
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

processMIDI
  :: SongYaml
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> Kicks
  -> RBDrums.Audio
  -> Action U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> Action (RBFile.Song (RBFile.RB3File U.Beats, RBFile.PSFile U.Beats))
processMIDI songYaml input kicks mixMode getAudioLength = do
  let showPosition = RBFile.showPosition . U.applyMeasureMap mmap
      eventsRaw = discardPS $ RBFile.onyxEvents trks
      eventsList = ATB.toPairList $ RTB.toAbsoluteEventList 0 eventsRaw
      input'@(RBFile.Song tempos mmap trks) = if _fixFreeform $ _options songYaml
        then fixRolls input
        else input
  -- If there's no [end], put it after all MIDI events and audio files.
  endPosn' <- case [ t | (t, Events.End) <- eventsList ] of
    t : _ -> return t
    [] -> do
      audLen <- U.unapplyTempoMap tempos <$> getAudioLength
      let absTimes = ATB.getTimes . RTB.toAbsoluteEventList 0
          lastMIDIEvent = foldr max 0
            $ concatMap absTimes (RBFile.s_tracks $ RBFile.showMIDITracks input')
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
      drumsTrack = if not $ _hasDrums $ _instruments songYaml
        then RTB.empty
        else let
          trk1x = discardPS $ RBFile.onyxPartDrums trks
          trk2x = discardPS $ RBFile.onyxPartDrums2x trks
          psKicks = if _auto2xBass $ _options songYaml
            then U.unapplyTempoTrack tempos . phaseShiftKicks 0.18 0.11 . U.applyTempoTrack tempos
            else id
          sections = flip RTB.mapMaybe eventsRaw $ \case
            Events.PracticeSection s -> Just s
            _                        -> Nothing
          finish = promarkers . psKicks . drumMix mixMode . drumsComplete mmap sections
          promarkers = if _proDrums $ _options songYaml
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
          in case kicks of
            KicksPS -> psPS
            Kicks1x -> rockBand1x ps1x
            Kicks2x -> rockBand2x ps2x
      guitarMsgs = psMessages $ RBFile.onyxPartGuitar trks
      guitarTrack = if not $ _hasGuitar $ _instruments songYaml
        then RTB.empty
        else gryboComplete (Just $ _hopoThreshold $ _options songYaml) mmap
          $ discardPS $ RBFile.onyxPartGuitar trks
      bassMsgs = psMessages $ RBFile.onyxPartBass trks
      bassTrack = if not $ _hasBass $ _instruments songYaml
        then RTB.empty
        else gryboComplete (Just $ _hopoThreshold $ _options songYaml) mmap
          $ discardPS $ RBFile.onyxPartBass trks
      (proGtr, proGtr22) = if not $ _hasProGuitar $ _instruments songYaml
        then (RTB.empty, RTB.empty)
        else
          ( ProGtr.copyExpert $ ProGtr.autoHandPosition $ discardPS $ RBFile.onyxPartRealGuitar   trks
          , ProGtr.copyExpert $ ProGtr.autoHandPosition $ discardPS $ RBFile.onyxPartRealGuitar22 trks
          )
      (proBass, proBass22) = if not $ _hasProBass $ _instruments songYaml
        then (RTB.empty, RTB.empty)
        else
          ( ProGtr.copyExpert $ ProGtr.autoHandPosition $ discardPS $ RBFile.onyxPartRealBass   trks
          , ProGtr.copyExpert $ ProGtr.autoHandPosition $ discardPS $ RBFile.onyxPartRealBass22 trks
          )
      (tk, tkRH, tkLH, tpkX, tpkH, tpkM, tpkE) = if not $ hasAnyKeys $ _instruments songYaml
        then (RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty, RTB.empty)
        else let
          basicKeys = gryboComplete Nothing mmap $ if _hasKeys $ _instruments songYaml
            then discardPS $ RBFile.onyxPartKeys trks
            else expertProKeysToKeys keysExpert
          keysDiff diff = if _hasProKeys $ _instruments songYaml
            then discardPS $ case diff of
              Easy   -> RBFile.onyxPartRealKeysE trks
              Medium -> RBFile.onyxPartRealKeysM trks
              Hard   -> RBFile.onyxPartRealKeysH trks
              Expert -> RBFile.onyxPartRealKeysX trks
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
          in  ( basicKeys
              , keysAnim
              , RTB.empty
              , ProKeys.fixPSRange keysExpert
              , ProKeys.fixPSRange keysHard
              , ProKeys.fixPSRange keysMedium
              , ProKeys.fixPSRange keysEasy
              )
      (trkVox, trkHarm1, trkHarm2, trkHarm3) = case _hasVocal $ _instruments songYaml of
        Vocal0 -> (RTB.empty, RTB.empty, RTB.empty, RTB.empty)
        Vocal1 -> (partVox', RTB.empty, RTB.empty, RTB.empty)
        Vocal2 -> (partVox', harm1, harm2, RTB.empty)
        Vocal3 -> (partVox', harm1, harm2, harm3)
        where partVox = discardPS $ RBFile.onyxPartVocals trks
              partVox' = if RTB.null partVox then harm1ToPartVocals harm1 else partVox
              harm1   = discardPS $ RBFile.onyxHarm1 trks
              harm2   = discardPS $ RBFile.onyxHarm2 trks
              harm3   = discardPS $ RBFile.onyxHarm3 trks
  return $ RBFile.Song tempos mmap $
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
  let drums = discardPS $ RBFile.onyxPartDrums $ RBFile.s_tracks song
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
  let vox = discardPS $ RBFile.onyxPartVocals $ RBFile.s_tracks song
      harm1 = discardPS $ RBFile.onyxHarm1 $ RBFile.s_tracks song
      harm2 = discardPS $ RBFile.onyxHarm2 $ RBFile.s_tracks song
      harm3 = discardPS $ RBFile.onyxHarm3 $ RBFile.s_tracks song
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
