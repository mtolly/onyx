{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module RockBand3 (Kicks(..), processMIDI, findProblems) where

import qualified RockBand.File as RBFile
import Config
import qualified Sound.MIDI.Util as U
import Development.Shake
import           Reductions
import           ProKeysRanges
import           Scripts
import           OneFoot
import           Control.Monad.Extra
import qualified Data.EventList.Absolute.TimeBody      as ATB
import qualified Data.EventList.Relative.TimeBody      as RTB
import qualified RockBand.Drums                        as RBDrums
import qualified RockBand.Events                       as Events
import qualified RockBand.ProKeys                      as ProKeys
import qualified RockBand.ProGuitar                      as ProGtr
import qualified RockBand.Vocals                       as RBVox
import      RockBand.Common
import Control.Monad.Trans.Writer (execWriter, tell)

data Kicks = Kicks1x | Kicks2x | KicksPS
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

processMIDI
  :: SongYaml
  -> RBFile.Song U.Beats
  -> Kicks
  -> RBDrums.Audio
  -> Action U.Seconds -- ^ Gets the length of the longest audio file, if necessary.
  -> Action (RBFile.Song U.Beats)
processMIDI songYaml input kicks mixMode getAudioLength = do
  let showPosition = RBFile.showPosition . U.applyMeasureMap (RBFile.s_signatures input)
      trks = RBFile.s_tracks input
      tempos = RBFile.s_tempos input
      mergeTracks = foldr RTB.merge RTB.empty
      eventsRaw = mergeTracks [ t | RBFile.Events t <- trks ]
      eventsList = ATB.toPairList $ RTB.toAbsoluteEventList 0 eventsRaw
  -- If there's no [end], put it after all MIDI events and audio files.
  endPosn' <- case [ t | (t, Events.End) <- eventsList ] of
    t : _ -> return t
    [] -> do
      audLen <- U.unapplyTempoMap tempos <$> getAudioLength
      let absTimes = ATB.getTimes . RTB.toAbsoluteEventList 0
          lastMIDIEvent = foldr max 0 $ concatMap (absTimes . RBFile.showTrack) trks ++ absTimes (U.tempoMapToBPS tempos)
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
    trk = mergeTracks [ t | RBFile.Beat t <- trks ]
    in if RTB.null trk
      then do
        putNormal "Generating a BEAT track..."
        let alignedEnd = fromInteger $ ceiling endPosn'
        return (RBFile.Beat $ U.trackTake alignedEnd $ makeBeatTrack $ RBFile.s_signatures input, alignedEnd)
      else return (RBFile.Beat trk, endPosn')
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
        = RBFile.Events
        $ defaultNoCrowd
        $ RTB.insert musicStartPosn Events.MusicStart
        $ RTB.insert musicEndPosn Events.MusicEnd
        $ RTB.insert endPosn Events.End
        $ RTB.filter untouchedEvent eventsRaw
      venueTracks = let
        trk = mergeTracks [ t | RBFile.Venue t <- trks ]
        in guard (not $ RTB.null trk) >> [RBFile.Venue trk]
      drumsTracks = case kicks of
        Kicks1x -> drums1p
        Kicks2x -> drums2p
        KicksPS -> drumsPS
      (drumsPS, drums1p, drums2p) = if not $ _hasDrums $ _instruments songYaml
        then ([], [], [])
        else let
          trk1x = mergeTracks [ t | RBFile.PartDrums   t <- trks ]
          trk2x = mergeTracks [ t | RBFile.PartDrums2x t <- trks ]
          psKicks = if _auto2xBass $ _options songYaml
            then U.unapplyTempoTrack tempos . phaseShiftKicks 0.18 0.11 . U.applyTempoTrack tempos
            else id
          sections = flip RTB.mapMaybe eventsRaw $ \case
            Events.PracticeSection s -> Just s
            _                        -> Nothing
          finish = promarkers . psKicks . drumMix mixMode . drumsComplete (RBFile.s_signatures input) sections
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
          in  ( [RBFile.PartDrums psPS]
              , [RBFile.PartDrums $ rockBand1x ps1x]
              , [RBFile.PartDrums $ rockBand2x ps2x]
              )
      guitarTracks = if not $ _hasGuitar $ _instruments songYaml
        then []
        else (: []) $ RBFile.PartGuitar $ gryboComplete (Just $ _hopoThreshold $ _options songYaml) (RBFile.s_signatures input)
          $ mergeTracks [ t | RBFile.PartGuitar t <- trks ]
      bassTracks = if not $ _hasBass $ _instruments songYaml
        then []
        else (: []) $ RBFile.PartBass $ gryboComplete (Just $ _hopoThreshold $ _options songYaml) (RBFile.s_signatures input)
          $ mergeTracks [ t | RBFile.PartBass t <- trks ]
      proGuitarTracks = if not $ _hasProGuitar $ _instruments songYaml
        then []
        else map RBFile.copyExpert $ let
          mustang = ProGtr.autoHandPosition $ mergeTracks [ t | RBFile.PartRealGuitar   t <- trks ]
          squier  = ProGtr.autoHandPosition $ mergeTracks [ t | RBFile.PartRealGuitar22 t <- trks ]
          in [ RBFile.PartRealGuitar   mustang | not $ RTB.null mustang ]
          ++ [ RBFile.PartRealGuitar22 squier  | not $ RTB.null squier  ]
      proBassTracks = if not $ _hasProGuitar $ _instruments songYaml
        then []
        else map RBFile.copyExpert $ let
          mustang = ProGtr.autoHandPosition $ mergeTracks [ t | RBFile.PartRealBass   t <- trks ]
          squier  = ProGtr.autoHandPosition $ mergeTracks [ t | RBFile.PartRealBass22 t <- trks ]
          in [ RBFile.PartRealBass   mustang | not $ RTB.null mustang ]
          ++ [ RBFile.PartRealBass22 squier  | not $ RTB.null squier  ]
      keysTracks = if not $ hasAnyKeys $ _instruments songYaml
        then []
        else let
          basicKeys = gryboComplete Nothing (RBFile.s_signatures input) $ if _hasKeys $ _instruments songYaml
            then mergeTracks [ t | RBFile.PartKeys t <- trks ]
            else expertProKeysToKeys keysExpert
          keysDiff diff = if _hasProKeys $ _instruments songYaml
            then mergeTracks [ t | RBFile.PartRealKeys diff' t <- trks, diff == diff' ]
            else keysToProKeys diff basicKeys
          rtb1 `orIfNull` rtb2 = if length rtb1 < 5 then rtb2 else rtb1
          keysExpert = completeRanges $ keysDiff Expert
          keysHard   = completeRanges $ keysDiff Hard   `orIfNull` pkReduce Hard   (RBFile.s_signatures input) keysOD keysExpert
          keysMedium = completeRanges $ keysDiff Medium `orIfNull` pkReduce Medium (RBFile.s_signatures input) keysOD keysHard
          keysEasy   = completeRanges $ keysDiff Easy   `orIfNull` pkReduce Easy   (RBFile.s_signatures input) keysOD keysMedium
          keysOD = flip RTB.mapMaybe keysExpert $ \case
            ProKeys.Overdrive b -> Just b
            _                   -> Nothing
          keysAnim = flip RTB.filter keysExpert $ \case
            ProKeys.Note _ -> True
            _              -> False
          in  [ RBFile.PartKeys            basicKeys
              , RBFile.PartKeysAnimRH      keysAnim
              , RBFile.PartKeysAnimLH      RTB.empty
              , RBFile.PartRealKeys Expert keysExpert
              , RBFile.PartRealKeys Hard   keysHard
              , RBFile.PartRealKeys Medium keysMedium
              , RBFile.PartRealKeys Easy   keysEasy
              ]
      vocalTracks = case _hasVocal $ _instruments songYaml of
        Vocal0 -> []
        Vocal1 ->
          [ RBFile.PartVocals partVox'
          ]
        Vocal2 ->
          [ RBFile.PartVocals partVox'
          , RBFile.Harm1 harm1
          , RBFile.Harm2 harm2
          ]
        Vocal3 ->
          [ RBFile.PartVocals partVox'
          , RBFile.Harm1 harm1
          , RBFile.Harm2 harm2
          , RBFile.Harm3 harm3
          ]
        where partVox = mergeTracks [ t | RBFile.PartVocals t <- trks ]
              partVox' = if RTB.null partVox then harm1ToPartVocals harm1 else partVox
              harm1   = mergeTracks [ t | RBFile.Harm1      t <- trks ]
              harm2   = mergeTracks [ t | RBFile.Harm2      t <- trks ]
              harm3   = mergeTracks [ t | RBFile.Harm3      t <- trks ]
  return input
    { RBFile.s_tracks = map fixRolls $ concat
      [ [beatTrack, eventsTrack]
      , venueTracks
      , drumsTracks
      , guitarTracks
      , bassTracks
      , proGuitarTracks
      , proBassTracks
      , keysTracks
      , vocalTracks
      ]
    }

findProblems :: RBFile.Song U.Beats -> [String]
findProblems song = execWriter $ do
  -- Don't have a kick at the start of a drum roll.
  -- It screws up the roll somehow and causes spontaneous misses.
  let drums = foldr RTB.merge RTB.empty [ t | RBFile.PartDrums t <- RBFile.s_tracks song ]
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
      badDiscos = fmap (const ()) $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ filter isBadDisco $ ATB.toPairList $ RTB.toAbsoluteEventList 0 discos
      drumsDiff d = flip RTB.mapMaybe drums $ \case
        RBDrums.DiffEvent d' (RBDrums.Note gem) | d == d' -> Just gem
        _ -> Nothing
      isBadDisco (t, diff) = case RTB.viewL $ RTB.collectCoincident $ U.trackDrop t $ drumsDiff diff of
        Just ((_, evts), _) | any isDiscoGem evts -> True
        _ -> False
      isDiscoGem = \case
        RBDrums.Red -> True
        RBDrums.Pro RBDrums.Yellow _ -> True
        _ -> False
  -- Don't have a vocal phrase that ends simultaneous with a lyric event.
  -- In static vocals, this puts the lyric in the wrong phrase.
  let vox = foldr RTB.merge RTB.empty [ t | RBFile.PartVocals t <- RBFile.s_tracks song ]
      harm1 = foldr RTB.merge RTB.empty [ t | RBFile.Harm1 t <- RBFile.s_tracks song ]
      harm2 = foldr RTB.merge RTB.empty [ t | RBFile.Harm2 t <- RBFile.s_tracks song ]
      harm3 = foldr RTB.merge RTB.empty [ t | RBFile.Harm3 t <- RBFile.s_tracks song ]
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
