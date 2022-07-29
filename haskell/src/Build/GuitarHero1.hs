{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Build.GuitarHero1 where

import           Audio
import           Build.Common
import           Config
import           Control.Monad.Codec.Onyx         (valueId)
import           Control.Monad.Extra
import           Control.Monad.Random             (evalRand, mkStdGen)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import           Data.Bifunctor                   (bimap, first)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import           Data.Conduit.Audio
import qualified Data.DTA                         as D
import qualified Data.DTA.Serialize               as D
import qualified Data.DTA.Serialize.GH1           as D
import qualified Data.DTA.Serialize.Magma         as Magma
import           Data.DTA.Serialize.RB3           (AnimTempo (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Hashable                    (Hashable, hash)
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust, isNothing)
import qualified Data.Text                        as T
import           Development.Shake                hiding (phony, (%>), (&%>))
import           Development.Shake.FilePath
import           GuitarHeroI.File
import           GuitarHeroII.Audio               (writeVGSMultiRate)
import           GuitarHeroII.Convert             (GH2AudioSection (..),
                                                   adjustSongText)
import           GuitarHeroII.PartGuitar          (HandMap (..),
                                                   PartDifficulty (..))
import           Guitars
import           Overdrive                        (removeNotelessOD, voidEdgeOn)
import           Reductions                       (gryboComplete)
import qualified RockBand.Codec.Drums             as RB
import qualified RockBand.Codec.File              as F
import qualified RockBand.Codec.Five              as RB
import qualified RockBand.Codec.Vocal             as RB
import           RockBand.Common
import           RockBand3                        (BasicTiming (..),
                                                   basicTiming, drumsToFive,
                                                   makeMoods)
import qualified Sound.MIDI.Util                  as U

gh1Pad
  :: (SendMessage m)
  => F.Song (GH1File U.Beats)
  -> StackTraceT m (F.Song (GH1File U.Beats), Int)
gh1Pad rb3@(F.Song tmap _ trks) = let
  firstEvent rtb = case RTB.viewL rtb of
    Just ((dt, _), _) -> dt
    Nothing           -> 999
  firstNoteBeats = foldr min 999 $ map (firstEvent . partGems) $ Map.elems $ gemsDifficulties $ gh1T1Gems trks
  firstNoteSeconds = U.applyTempoMap tmap firstNoteBeats
  -- no idea but just going with similar value to what Magma enforces for RB
  padSeconds = max 0 $ ceiling $ 2.6 - (realToFrac firstNoteSeconds :: Rational)
  in case padSeconds of
    0 -> do
      return (rb3, 0)
    _ -> do
      warn $ "Padding song by " ++ show padSeconds ++ "s due to early notes."
      return (F.padAnyFile padSeconds rb3, padSeconds)

data GH1Audio = GH1Audio
  { gh1AudioSections :: [GH2AudioSection]
  , gh1LeadChannels  :: [Int]
  , gh1BackChannels  :: [Int]
  , gh1LeadTrack     :: F.FlexPartName
  , gh1AnimBass      :: Maybe F.FlexPartName
  , gh1AnimDrums     :: Maybe F.FlexPartName
  , gh1AnimVocal     :: Maybe F.FlexPartName
  , gh1AnimKeys      :: Maybe F.FlexPartName
  }

computeGH1Audio
  :: (Monad m)
  => SongYaml f
  -> TargetGH1
  -> (F.FlexPartName -> Bool) -- True if part has own audio
  -> StackTraceT m GH1Audio
computeGH1Audio song target hasAudio = do
  let hasFiveOrDrums = \case
        Nothing   -> False
        Just part -> isJust (partGRYBO part) || isJust (partDrums part)
  gh1LeadTrack <- if hasFiveOrDrums $ getPart (gh1_Guitar target) song
    then return $ gh1_Guitar target
    else fatal "computeGH1Audio: no lead guitar part selected"
  let leadAudio = hasAudio gh1LeadTrack
      gh1AudioSections =
        [ GH2Band
        , if leadAudio then GH2PartStereo gh1LeadTrack else GH2Silent
        ]
      gh1LeadChannels = if leadAudio then [2, 3] else [2]
      gh1BackChannels = [0, 1] -- should always be this for our output
      gh1AnimBass  = gh1_Bass  target <$ (getPart (gh1_Bass  target) song >>= partGRYBO)
      gh1AnimDrums = gh1_Drums target <$ (getPart (gh1_Drums target) song >>= partDrums)
      gh1AnimVocal = gh1_Vocal target <$ (getPart (gh1_Vocal target) song >>= partVocal)
      gh1AnimKeys  = gh1_Keys  target <$ (getPart (gh1_Keys  target) song >>= partGRYBO)
  return GH1Audio{..}

midiRB3toGH1
  :: (SendMessage m)
  => SongYaml f
  -> GH1Audio
  -> F.Song (F.OnyxFile U.Beats)
  -> StackTraceT m U.Seconds
  -> StackTraceT m (F.Song (GH1File U.Beats), Int)
midiRB3toGH1 song audio inputMid@(F.Song tmap mmap onyx) getAudioLen = do
  timing <- basicTiming inputMid getAudioLen
  let makePlayBools origMoods gems = let
        moods = if RTB.null origMoods
          then makeMoods tmap timing gems
          else origMoods
        in flip fmap moods $ \case
          Mood_idle_realtime -> False
          Mood_idle          -> False
          Mood_idle_intense  -> False
          Mood_play          -> True
          Mood_mellow        -> True
          Mood_intense       -> True
          Mood_play_solo     -> True
      makeDiffs fpart rbg = do
        let makeDiff diff fdiff = do
              od <- removeNotelessOD
                mmap
                [(fpart, [(show diff, voidEdgeOn $ RB.fiveGems fdiff)])]
                ((fpart,) <$> RB.fiveOverdrive rbg)
              return PartDifficulty
                { partStarPower = snd <$> od
                , partPlayer1   = RB.fivePlayer1 rbg
                , partPlayer2   = RB.fivePlayer2 rbg
                , partGems      = RB.fiveGems fdiff
                }
        fmap Map.fromList
          $ mapM (\(diff, fdiff) -> (diff,) <$> makeDiff diff fdiff)
          $ Map.toList $ RB.fiveDifficulties rbg
      getLeadData fpart = case getPart fpart song >>= partGRYBO of
        Nothing -> case getPart fpart song >>= partDrums of
          Nothing -> fatal "No guitar or drums info set up for lead guitar part"
          Just pd -> return $ let
            trackOrig = buildDrumTarget
              DrumTargetRB2x
              pd
              (timingEnd timing)
              tmap
              (F.getFlexPart fpart onyx)
            in drumsToFive inputMid trackOrig
        Just grybo -> return $ let
          src = F.getFlexPart fpart onyx
          (trackOrig, algo) = getFive src
          gap = fromIntegral (gryboSustainGap grybo) / 480
          ht = gryboHopoThreshold grybo
          fiveEachDiff f ft = ft { RB.fiveDifficulties = fmap f $ RB.fiveDifficulties ft }
          toGtr = fiveEachDiff $ \fd ->
              emit5'
            . fromClosed'
            . noOpenNotes'
            . noTaps'
            . noExtendedSustains' standardBlipThreshold gap
            . applyForces (getForces5 fd)
            . strumHOPOTap' algo (fromIntegral ht / 480)
            . fixSloppyNotes (10 / 480)
            . closeNotes'
            $ fd
          in gryboComplete (Just ht) mmap $ toGtr trackOrig
      getFive = F.selectGuitarTrack F.FiveTypeGuitar
  rbGuitar <- getLeadData $ gh1LeadTrack audio
  leadDiffs <- makeDiffs (gh1LeadTrack audio) rbGuitar
  gh1Pad $ F.Song tmap mmap GH1File
    { gh1T1Gems   = GemsTrack
      { gemsDifficulties = leadDiffs
      , gemsMouthOpen    = case gh1AnimVocal audio of
        Nothing   -> RTB.empty
        Just part -> fmap snd $ RB.vocalNotes $ F.onyxPartVocals $ F.getFlexPart part onyx
      }
    , gh1Anim     = AnimTrack
      { animFretPosition = first FretPosition <$> RB.fiveFretPosition rbGuitar
      , animHandMap = flip fmap (RB.fiveHandMap rbGuitar) $ \case
        RB.HandMap_Default   -> HandMap_Default
        RB.HandMap_NoChords  -> HandMap_NoChords
        RB.HandMap_AllChords -> HandMap_AllChords
        RB.HandMap_Solo      -> HandMap_Solo
        RB.HandMap_DropD     -> HandMap_DropD2
        RB.HandMap_DropD2    -> HandMap_DropD2
        RB.HandMap_AllBend   -> HandMap_Solo
        RB.HandMap_Chord_C   -> HandMap_Default
        RB.HandMap_Chord_D   -> HandMap_Default
        RB.HandMap_Chord_A   -> HandMap_Default
      }
    , gh1Triggers = mempty -- don't know what these are yet
    , gh1Events   = EventsTrack
      { eventsList = foldr RTB.merge RTB.empty
        [ fmap (\b -> if b then Event_gtr_on else Event_gtr_off)
          $ makePlayBools (RB.fiveMood rbGuitar)
          $ maybe mempty (splitEdges . edgeBlips minSustainLengthRB . RB.fiveGems)
          $ Map.lookup Expert $ RB.fiveDifficulties rbGuitar
        , case gh1AnimVocal audio of
          Nothing -> RTB.empty
          Just part -> let
            trk = F.onyxPartVocals $ F.getFlexPart part onyx
            in fmap (\b -> if b then Event_sing_on else Event_sing_off)
              $ makePlayBools (RB.vocalMood trk)
              $ fmap (\case (p, True) -> NoteOn () p; (p, False) -> NoteOff p)
              $ RB.vocalNotes trk
        , case gh1AnimBass audio of
          Nothing -> RTB.empty
          Just part -> let
            trk = fst $ getFive $ F.getFlexPart part onyx
            in fmap (\b -> if b then Event_bass_on else Event_bass_off)
              $ makePlayBools (RB.fiveMood trk)
              $ maybe mempty (splitEdges . edgeBlips minSustainLengthRB . RB.fiveGems)
              $ Map.lookup Expert $ RB.fiveDifficulties trk
        , case gh1AnimKeys audio of
          Nothing -> RTB.empty
          Just part -> let
            trk = fst $ getFive $ F.getFlexPart part onyx
            in fmap (\b -> if b then Event_keys_on else Event_keys_off)
              $ makePlayBools (RB.fiveMood trk)
              $ maybe mempty (splitEdges . edgeBlips minSustainLengthRB . RB.fiveGems)
              $ Map.lookup Expert $ RB.fiveDifficulties trk
        , case gh1AnimDrums audio of
          Nothing -> RTB.empty
          Just part -> let
            trk = F.onyxPartDrums $ F.getFlexPart part onyx
            anims = RB.drumAnimation $ RB.fillDrumAnimation (0.25 :: U.Seconds) tmap trk
            in fmap (\b -> if b then Event_drum_on else Event_drum_off)
              $ makePlayBools (RB.drumMood trk) $ Blip () () <$ anims
        , RTB.singleton (timingEnd timing) Event_end
        ]
      }
    }

bandMembers :: SongYaml f -> GH1Audio -> Maybe [Either D.BandMember T.Text]
bandMembers song audio = let
  vocal = case gh1AnimVocal audio of
    Nothing    -> Nothing
    Just fpart -> Just $ fromMaybe Magma.Male $ getPart fpart song >>= partVocal >>= vocalGender
  bass = True -- we'll just assume there's always a bassist, don't know if required (all songs on disc have one)
  keys = isJust $ gh1AnimKeys audio
  drums = True -- gh2 crashes if missing, haven't tested gh1 but likely the same
  in case (vocal, bass, keys, drums) of
    (Just Magma.Male, True, False, True) -> Nothing -- the default configuration
    _                                    -> Just $ map Left $ concat
      [ toList $ (\case Magma.Male -> D.SINGER_MALE_METAL; Magma.Female -> D.SINGER_FEMALE_METAL) <$> vocal
      , [D.BASS_METAL     | bass ]
      , [D.KEYBOARD_METAL | keys && isNothing vocal] -- singer overrides keyboard, can't have both
      , [D.DRUMMER_METAL  | drums]
      ]

makeGH1DTA :: SongYaml f -> T.Text -> (Int, Int) -> GH1Audio -> T.Text -> D.SongPackage
makeGH1DTA song key preview audio title = D.SongPackage
  { D.name = adjustSongText title
  , D.artist = adjustSongText $ getArtist $ _metadata song
  , D.song = D.Song
    { D.songName = "songs/" <> key <> "/" <> key
    , D.tracks = 1
    , D.slip_tracks = [map fromIntegral $ gh1LeadChannels audio]
    -- note, we don't actually use GH2PartMono
    , D.pans = gh1AudioSections audio >>= \case
      GH2PartStereo _ -> [-1, 1]
      GH2PartMono   _ -> [0]
      GH2Band         -> [-1, 1]
      GH2Silent       -> [0]
    , D.vols = gh1AudioSections audio >>= \case
      GH2PartStereo _ -> [0, 0]
      GH2PartMono   _ -> [20 * (log 2 / log 10)] -- compensate for half volume later
      GH2Band         -> [0, 0]
      GH2Silent       -> [0]
    , D.cores = gh1AudioSections audio >>= \case
      GH2PartStereo _ -> [1, 1]
      GH2PartMono   _ -> [1]
      GH2Band         -> [-1, -1]
      GH2Silent       -> [-1]
    , D.solo = ["riffs", "standard"]
    }
  , D.band = bandMembers song audio
  , D.bank = "sfx/song_default"
  , D.bpm = 120 -- does this do anything?
  , D.animTempo = KTempoMedium
  , D.preview = bimap fromIntegral fromIntegral preview
  , D.midiFile = "songs/" <> key <> "/" <> key <> ".mid"
  , D.quickplay = evalRand D.randomQuickplay $ mkStdGen $ hash key
  }

hashGH1 :: (Hashable f) => SongYaml f -> TargetGH1 -> Int
hashGH1 songYaml gh1 = let
  hashed =
    ( gh1
    , _title $ _metadata songYaml
    , _artist $ _metadata songYaml
    )
  in 1000000000 + (hash hashed `mod` 1000000000)

gh1Rules :: BuildInfo -> FilePath -> TargetGH1 -> QueueLog Rules ()
gh1Rules buildInfo dir gh1 = do

  let songYaml = biSongYaml buildInfo
      rel = biRelative buildInfo

  (planName, plan) <- case getPlan (tgt_Plan $ gh1_Common gh1) songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show gh1
    Just pair -> return pair
  let planDir = rel $ "gen/plan" </> T.unpack planName
      defaultID = hashGH1 songYaml gh1
      key = fromMaybe (makeShortName defaultID songYaml) $ gh1_Key gh1
      pkg = T.unpack key

  let loadPartAudioCheck = case plan of
        Plan{..}     -> return $ \part -> HM.member part $ getParts _planParts
        MoggPlan{..} -> do
          silentChans <- shk $ read <$> readFile' (planDir </> "silent-channels.txt")
          return $ \part -> case HM.lookup part $ getParts _moggParts of
            Nothing    -> False
            Just chans -> any (`notElem` (silentChans :: [Int])) $ concat $ toList chans

  [dir </> "gh1/notes.mid", dir </> "gh1/pad.txt"] &%> \[out, pad] -> do
    input <- F.shakeMIDI $ planDir </> "processed.mid"
    hasAudio <- loadPartAudioCheck
    audio <- computeGH1Audio songYaml gh1 hasAudio
    (mid, padSeconds) <- midiRB3toGH1 songYaml audio
      (applyTargetMIDI (gh1_Common gh1) input)
      (getAudioLength buildInfo planName plan)
    F.saveMIDI out mid
    stackIO $ writeFile pad $ show padSeconds

  let loadGH1Midi = F.shakeMIDI $ dir </> "gh1/notes.mid" :: Staction (F.Song (GH1File U.Beats))
      correctAudioLength mid = do
        endTime <- case RTB.filter (== Event_end) $ eventsList $ gh1Events $ F.s_tracks mid of
          RNil       -> fatal "panic! couldn't find [end] event in GH1 output midi"
          Wait t _ _ -> return $ U.applyTempoMap (F.s_tempos mid) t
        return $ endTime + 5
        -- previously we went 0.5s past [end], but that still had issues,
        -- particularly in GH2 practice mode when playing the last section
      -- for vgs, separate sources so silence can be encoded at low sample rate
      gh1SourcesVGS = do
        hasAudio <- loadPartAudioCheck
        audio <- computeGH1Audio songYaml gh1 hasAudio
        mid <- loadGH1Midi
        srcs <- forM (gh1AudioSections audio) $ \case
          GH2PartStereo part -> getPartSource buildInfo [(-1, 0), (1, 0)] planName plan part 1
          -- This halves the volume, so we set vols in .dta to compensate
          GH2PartMono part -> applyVolsMono [0] <$> getPartSource buildInfo [(-1, 0), (1, 0)] planName plan part 1
          GH2Band -> sourceSongCountin buildInfo (gh1_Common gh1) mid 0 True planName plan
            [ (gh1LeadTrack audio, 1)
            ]
          GH2Silent -> return $ silent (Seconds 0) 11025 1
        pad <- shk $ read <$> readFile' (dir </> "gh1/pad.txt")
        audioLen <- correctAudioLength mid
        let applyOffset = case compare (gh1_Offset gh1) 0 of
              EQ -> id
              GT -> dropStart $ Seconds          $ gh1_Offset gh1
              LT -> padStart  $ Seconds $ negate $ gh1_Offset gh1
            toEachSource
              = setAudioLength audioLen
              . applyOffset
              . padAudio pad
              . applyTargetAudio (gh1_Common gh1) mid
        return $ fmap toEachSource srcs

  dir </> "gh1/audio.vgs" %> \out -> do
    srcs <- gh1SourcesVGS
    stackIO $ runResourceT $ writeVGSMultiRate out $ map (mapSamples integralSample) srcs

  [dir </> "gh1/songs.dta", dir </> "gh1/songs-inner.dta"] &%> \[out, outInner] -> do
    input <- F.shakeMIDI $ planDir </> "processed.mid"
    hasAudio <- loadPartAudioCheck
    audio <- computeGH1Audio songYaml gh1 hasAudio
    -- TODO previewBounds doesn't account for padding due to early notes. not a big deal though
    let inner = D.serialize (valueId D.stackChunks) $ makeGH1DTA
          songYaml
          key
          (previewBounds songYaml (input :: F.Song (F.OnyxFile U.Beats)))
          audio
          (targetTitle songYaml $ GH1 gh1)
    stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
      [ D.Parens $ D.Tree 0 $ D.Sym key : D.treeChunks (D.topTree inner) ]
    stackIO $ D.writeFileDTA_latin1 outInner inner

  dir </> "gh1/symbol" %> \out -> do
    stackIO $ B.writeFile out $ B8.pack pkg

  phony (dir </> "gh1") $ shk $ need $
    [ dir </> "gh1/notes.mid"
    , dir </> "gh1/audio.vgs"
    , dir </> "gh1/songs.dta"
    , dir </> "gh1/songs-inner.dta"
    , dir </> "gh1/symbol"
    ]
