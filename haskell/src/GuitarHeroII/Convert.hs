{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module GuitarHeroII.Convert where

import           Config
import           Control.Monad                    (guard, void)
import           Control.Monad.Trans.StackTrace
import           Data.Bifunctor                   (bimap)
import qualified Data.DTA.Serialize               as D
import qualified Data.DTA.Serialize.GH2           as D
import           Data.DTA.Serialize.RB3           (AnimTempo (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T
import           GuitarHeroII.BandBass
import           GuitarHeroII.BandDrums
import           GuitarHeroII.BandKeys
import           GuitarHeroII.BandSinger
import           GuitarHeroII.Events
import           GuitarHeroII.File
import           GuitarHeroII.PartGuitar
import           GuitarHeroII.Triggers
import           Guitars
import           Overdrive                        (removeNotelessOD)
import           Reductions                       (gryboComplete)
import qualified RockBand.Codec.Drums             as RB
import qualified RockBand.Codec.Events            as RB
import qualified RockBand.Codec.File              as F
import qualified RockBand.Codec.Five              as RB
import qualified RockBand.Codec.Vocal             as RB
import           RockBand.Common                  (Difficulty (..), Edge (..),
                                                   Mood (..))
import           RockBand.Sections                (makeGH2Section)
import           RockBand3                        (BasicTiming (..),
                                                   basicTiming)
import qualified Sound.MIDI.Util                  as U

gh2Pad
  :: (SendMessage m)
  => F.Song (GH2File U.Beats)
  -> StackTraceT m (F.Song (GH2File U.Beats), Int)
gh2Pad rb3@(F.Song tmap _ trks) = let
  firstEvent rtb = case RTB.viewL rtb of
    Just ((dt, _), _) -> dt
    Nothing           -> 999
  firstNoteBeats = foldr min 999 $ concat
    [ map (firstEvent . partGems) $ Map.elems $ partDifficulties $ gh2PartGuitar     trks
    , map (firstEvent . partGems) $ Map.elems $ partDifficulties $ gh2PartBass       trks
    , map (firstEvent . partGems) $ Map.elems $ partDifficulties $ gh2PartRhythm     trks
    , map (firstEvent . partGems) $ Map.elems $ partDifficulties $ gh2PartGuitarCoop trks
    ]
  firstNoteSeconds = U.applyTempoMap tmap firstNoteBeats
  -- no idea but just going with similar value to what Magma enforces for RB
  padSeconds = max 0 $ ceiling $ 2.6 - (realToFrac firstNoteSeconds :: Rational)
  in case padSeconds of
    0 -> do
      return (rb3, 0)
    _ -> do
      warn $ "Padding song by " ++ show padSeconds ++ "s due to early notes."
      return (F.padAnyFile padSeconds rb3, padSeconds)

data GH2AudioSection
  = GH2Part F.FlexPartName -- stereo
  | GH2Band -- stereo
  | GH2Silent -- mono

data GH2Audio = GH2Audio
  { gh2AudioSections :: [GH2AudioSection]
  , gh2LeadChannels  :: [Int]
  , gh2CoopChannels  :: [Int]
  , gh2LeadTrack     :: F.FlexPartName
  , gh2CoopTrack     :: F.FlexPartName
  , gh2CoopType      :: GH2Coop
  , gh2Practice      :: [Maybe F.FlexPartName]
  , gh2LeadPractice  :: Int
  , gh2CoopPractice  :: Int
  }

computeGH2Audio
  :: (Monad m)
  => SongYaml f
  -> TargetGH2
  -> Plan f
  -> StackTraceT m GH2Audio
computeGH2Audio song target plan = do
  let hasAudio part = case plan of
        Plan{..}     -> HM.member part $ getParts _planParts
        -- TODO this needs to be fixed to not count silent channels
        MoggPlan{..} -> HM.member part $ getParts _moggParts
  gh2LeadTrack <- case getPart (gh2_Guitar target) song >>= partGRYBO of
    Nothing -> fatal "computeGH2Audio: no lead guitar part selected"
    Just _  -> return $ gh2_Guitar target
  let specifiedCoop = case gh2_Coop target of
        GH2Bass   -> gh2_Bass   target
        GH2Rhythm -> gh2_Rhythm target
      (gh2CoopTrack, gh2CoopType) = case getPart specifiedCoop song >>= partGRYBO of
        Nothing -> (gh2LeadTrack , GH2Rhythm      )
        Just _  -> (specifiedCoop, gh2_Coop target)
      leadAudio = hasAudio gh2LeadTrack
      coopAudio = gh2LeadTrack /= gh2CoopTrack && hasAudio gh2CoopTrack
      bandSection   = [GH2Band                                              ]
      leadSection   = [GH2Part gh2LeadTrack | leadAudio                     ]
      coopSection   = [GH2Part gh2CoopTrack | coopAudio                     ]
      silentSection = [GH2Silent            | not leadAudio || not coopAudio]
      gh2AudioSections = concat [bandSection, leadSection, coopSection, silentSection]
      count = \case
        GH2Part _ -> 2
        GH2Band   -> 2
        GH2Silent -> 1
      indexes before section = take (sum $ map count section) [sum (map count before) ..]
      gh2LeadChannels = if leadAudio
        then indexes bandSection leadSection
        else indexes (concat [bandSection, leadSection, coopSection]) silentSection
      gh2CoopChannels = if coopAudio
        then indexes (concat [bandSection, leadSection]) coopSection
        else indexes (concat [bandSection, leadSection, coopSection]) silentSection
      (gh2Practice, gh2LeadPractice, gh2CoopPractice) = case (leadAudio, coopAudio) of
        -- From testing, you can't just have 1 channel and assign it to both lead and bass/rhythm;
        -- you get no audio. I'm guessing any channels for an instrument other than the one
        -- you're playing are muted, even if they're also assigned to the one you're playing.
        (False, False) -> ([Nothing          , Nothing          ], 0, 1)
        (True , False) -> ([Just gh2LeadTrack, Nothing          ], 0, 1)
        (False, True)  -> ([Just gh2CoopTrack, Nothing          ], 0, 1)
        (True , True)  -> ([Just gh2LeadTrack, Just gh2CoopTrack], 0, 1)
  return GH2Audio{..}

midiRB3toGH2
  :: (SendMessage m)
  => SongYaml f
  -> TargetGH2
  -> GH2Audio
  -> F.Song (F.OnyxFile U.Beats)
  -> StackTraceT m U.Seconds
  -> StackTraceT m (F.Song (GH2File U.Beats), Int)
midiRB3toGH2 song target audio inputMid@(F.Song tmap mmap onyx) getAudioLength = do
  timing <- basicTiming inputMid getAudioLength
  let makeMoods moods = let
        bools = flip fmap moods $ \case
          Mood_idle_realtime -> False
          Mood_idle          -> False
          Mood_idle_intense  -> False
          Mood_play          -> True
          Mood_mellow        -> True
          Mood_intense       -> True
          Mood_play_solo     -> True
        in (const () <$> RTB.filter not bools, const () <$> RTB.filter id bools) -- (idle, play)
      makePartGuitar fpart rbg = do
        let makeDiff diff fdiff = do
              od <- removeNotelessOD
                mmap
                [(fpart, [(show diff, void $ RB.fiveGems fdiff)])]
                ((fpart,) <$> RB.fiveOverdrive rbg)
              return PartDifficulty
                { partStarPower = snd <$> od
                , partPlayer1   = RB.fivePlayer1 rbg
                , partPlayer2   = RB.fivePlayer2 rbg
                , partGems      = RB.fiveGems fdiff
                }
            (idle, play) = makeMoods $ RB.fiveMood rbg
        diffs <- fmap Map.fromList
          $ mapM (\(diff, fdiff) -> (diff,) <$> makeDiff diff fdiff)
          $ Map.toList $ RB.fiveDifficulties rbg
        return mempty
          { partDifficulties = diffs
          , partFretPosition = RB.fiveFretPosition rbg
          , partIdle         = idle
          , partPlay         = play
          , partHandMap      = flip fmap (RB.fiveHandMap rbg) $ \case
            RB.HandMap_Default   -> HandMap_Default
            RB.HandMap_NoChords  -> HandMap_NoChords
            RB.HandMap_AllChords -> HandMap_Default
            RB.HandMap_Solo      -> HandMap_Solo
            RB.HandMap_DropD     -> HandMap_DropD2
            RB.HandMap_DropD2    -> HandMap_DropD2
            RB.HandMap_AllBend   -> HandMap_Solo
            RB.HandMap_Chord_C   -> HandMap_Default
            RB.HandMap_Chord_D   -> HandMap_Default
            RB.HandMap_Chord_A   -> HandMap_Default
          }
      makeGRYBO fpart = case getPart fpart song >>= partGRYBO of
        Nothing -> return mempty
        Just grybo -> let
          src = F.getFlexPart fpart onyx
          (trackOrig, algo) = getFive src
          gap = fromIntegral (gryboSustainGap grybo) / 480
          ht = gryboHopoThreshold grybo
          fiveEachDiff f ft = ft { RB.fiveDifficulties = fmap f $ RB.fiveDifficulties ft }
          toGtr = fiveEachDiff $ \fd ->
              emit5'
            . fromClosed'
            . no5NoteChords'
            . noOpenNotes'
            . noTaps'
            . noExtendedSustains' standardBlipThreshold gap
            . applyForces (getForces5 fd)
            . strumHOPOTap' algo (fromIntegral ht / 480)
            . fixSloppyNotes (10 / 480)
            . closeNotes'
            $ fd
          in makePartGuitar fpart $ gryboComplete (Just ht) mmap $ toGtr trackOrig
      makeBandBass trk = mempty
        { bassIdle  = idle
        , bassPlay  = play
        , bassStrum
          = fmap (const ())
          . RTB.collectCoincident
          . RTB.filter (\case EdgeOn{} -> True; EdgeOff{} -> False)
          . RB.fiveGems
          . fromMaybe mempty
          . Map.lookup Expert
          $ RB.fiveDifficulties trk
        } where (idle, play) = makeMoods $ RB.fiveMood trk
      makeBandDrums trk = mempty
        { drumsIdle = idle
        , drumsPlay = play
        , drumsKick  = fmap (const ()) $ flip RTB.filter (RB.drumAnimation trk) $ \case
          RB.KickRF -> True
          _         -> False
        , drumsCrash = fmap (const ()) $ flip RTB.filter (RB.drumAnimation trk) $ \case
          RB.Crash1{}        -> True
          RB.Crash2{}        -> True
          RB.Crash1RHChokeLH -> True
          RB.Crash2RHChokeLH -> True
          _                  -> False
        } where (idle, play) = makeMoods $ RB.drumMood trk
      makeBandKeys trk = let
        (idle, play) = makeMoods $ RB.fiveMood trk
        in mempty { keysIdle = idle, keysPlay = play }
      makeBandSinger trk = let
        (idle, play) = makeMoods $ RB.vocalMood trk
        in mempty { singerIdle = idle, singerPlay = play }
      events = mempty
        { eventsSections      = fmap (makeGH2Section . snd)
          $ RB.eventsSections $ F.onyxEvents onyx
        , eventsOther         = foldr RTB.merge RTB.empty
          [ RTB.cons (timingMusicStart timing) MusicStart RTB.empty
          , RTB.cons (timingEnd        timing) End        RTB.empty
          ]
        }
      triggers = mempty
        { triggersBacking = RB.eventsBacking $ F.onyxEvents onyx
        }
      getFive = F.selectGuitarTrack F.FiveTypeGuitar
  gh2PartGuitar <- makeGRYBO $ gh2LeadTrack audio
  coopTrack <- if gh2CoopTrack audio == gh2LeadTrack audio
    then return gh2PartGuitar
    else makeGRYBO $ gh2CoopTrack audio
  let gh2 = GH2File
        { gh2PartGuitarCoop = mempty
        , gh2PartBass = case gh2CoopType audio of
            GH2Rhythm -> mempty
            GH2Bass   -> coopTrack
        , gh2PartRhythm = case gh2CoopType audio of
            GH2Rhythm -> coopTrack
            GH2Bass   -> mempty
        , gh2BandBass       = makeBandBass $ fst $ getFive $ F.getFlexPart (gh2_Bass target) onyx
        , gh2BandDrums      = makeBandDrums $ F.onyxPartDrums $ F.getFlexPart (gh2_Drums target) onyx
        , gh2BandKeys       = makeBandKeys $ fst $ getFive $ F.getFlexPart (gh2_Keys target) onyx
        , gh2BandSinger     = makeBandSinger $ F.onyxPartVocals $ F.getFlexPart (gh2_Vocal target) onyx
        , gh2Events         = events
        , gh2Triggers       = triggers
        , ..
        }
  gh2Pad $ F.Song tmap mmap gh2

makeGH2DTA :: SongYaml f -> T.Text -> (Int, Int) -> TargetGH2 -> GH2Audio -> T.Text -> D.SongPackage
makeGH2DTA song key preview target audio title = D.SongPackage
  { D.name = title
  , D.artist = getArtist $ _metadata song
  , D.caption = guard (not $ _cover $ _metadata song) >> Just "performed_by"
  , D.song = D.Song
    { D.songName      = "songs/" <> key <> "/" <> key
    , D.tracks        = D.DictList
      [ ("guitar", map fromIntegral $ gh2LeadChannels audio)
      , (coop    , map fromIntegral $ gh2CoopChannels audio)
      ]
    , D.pans          = gh2AudioSections audio >>= \case
      GH2Part _ -> [-1, 1]
      GH2Band   -> [-1, 1]
      GH2Silent -> [0]
    , D.vols          = gh2AudioSections audio >>= \case
      GH2Part _ -> [0, 0]
      GH2Band   -> [0, 0]
      GH2Silent -> [0]
    , D.cores         = gh2AudioSections audio >>= \case
      GH2Part p -> if p == gh2LeadTrack audio then [1, 1] else [-1, -1]
      GH2Band   -> [-1, -1]
      GH2Silent -> [-1]
    , D.midiFile      = "songs/" <> key <> "/" <> key <> ".mid"
    , D.hopoThreshold = Nothing
    }
  , D.animTempo = KTempoMedium
  , D.preview = bimap fromIntegral fromIntegral preview
  , D.quickplay = gh2_Quickplay target
  , D.practiceSpeeds = Just [100, 90, 75, 60]
  , D.songCoop = Nothing
  , D.songPractice1 = Just $ prac 90
  , D.songPractice2 = Just $ prac 75
  , D.songPractice3 = Just $ prac 60
  , D.band = Nothing -- TODO
  } where
    coop = case gh2CoopType audio of GH2Bass -> "bass"; GH2Rhythm -> "rhythm"
    prac :: Int -> D.Song
    prac speed = D.Song
      { D.songName = "songs/" <> key <> "/" <> key <> "_p" <> T.pack (show speed)
      , D.tracks = D.DictList
        [ ("guitar", [fromIntegral $ gh2LeadPractice audio])
        , (coop    , [fromIntegral $ gh2CoopPractice audio])
        ]
      , D.pans = 0 <$ gh2Practice audio
      , D.vols = 0 <$ gh2Practice audio
      , D.cores = (-1) <$ gh2Practice audio
      , D.midiFile = "songs/" <> key <> "/" <> key <> ".mid"
      , D.hopoThreshold = Nothing
      }

makeGH2DTA360 :: SongYaml f -> T.Text -> (Int, Int) -> TargetGH2 -> GH2Audio -> T.Text -> D.SongPackage
makeGH2DTA360 song key preview target audio title = D.SongPackage
  { D.name = title
  , D.artist = getArtist $ _metadata song
  , D.caption = guard (not $ _cover $ _metadata song) >> Just "performed_by"
  , D.song = D.Song
    { D.songName      = "songs/" <> key <> "/" <> key
    , D.tracks        = D.DictList
      [ ("guitar", map fromIntegral $ gh2LeadChannels audio)
      , (coop    , map fromIntegral $ gh2CoopChannels audio)
      ]
    , D.pans          = gh2AudioSections audio >>= \case
      GH2Part _ -> [-1, 1]
      GH2Band   -> [-1, 1]
      GH2Silent -> [0]
    , D.vols          = gh2AudioSections audio >>= \case
      GH2Part _ -> [0, 0]
      GH2Band   -> [0, 0]
      GH2Silent -> [0]
    , D.cores         = gh2AudioSections audio >>= \case
      GH2Part p -> if p == gh2LeadTrack audio then [1, 1] else [-1, -1]
      GH2Band   -> [-1, -1]
      GH2Silent -> [-1]
    , D.midiFile      = "songs/" <> key <> "/" <> key <> ".mid"
    , D.hopoThreshold = Nothing
    }
  , D.animTempo = KTempoMedium
  , D.preview = bimap fromIntegral fromIntegral preview
  , D.quickplay = gh2_Quickplay target
  , D.practiceSpeeds = Nothing
  , D.songCoop = Nothing
  , D.songPractice1 = Nothing
  , D.songPractice2 = Nothing
  , D.songPractice3 = Nothing
  , D.band = Nothing -- TODO
  } where
    coop = case gh2_Coop target of GH2Bass -> "bass"; GH2Rhythm -> "rhythm"
