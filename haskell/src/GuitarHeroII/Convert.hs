{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module GuitarHeroII.Convert where

import           Config
import           Control.Monad                    (guard, void)
import           Control.Monad.Random             (evalRand, mkStdGen)
import           Control.Monad.Trans.StackTrace
import           Data.Bifunctor                   (bimap)
import qualified Data.DTA.Serialize               as D
import qualified Data.DTA.Serialize.GH2           as D
import qualified Data.DTA.Serialize.Magma         as Magma
import           Data.DTA.Serialize.RB3           (AnimTempo (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Hashable                    (hash)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust, isNothing)
import qualified Data.Text                        as T
import           GuitarHeroII.BandBass
import           GuitarHeroII.BandDrums
import           GuitarHeroII.BandKeys
import           GuitarHeroII.BandSinger
import           GuitarHeroII.Events
import           GuitarHeroII.File
import           GuitarHeroII.PartDrum
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
                                                   LongNote (..), Mood (..),
                                                   edgeBlipsRB, splitEdges)
import           RockBand.Sections                (makeGH2Section)
import           RockBand3                        (BasicTiming (..),
                                                   basicTiming)
import qualified RockBand3                        as RB3
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
  = GH2PartStereo F.FlexPartName
  | GH2PartMono F.FlexPartName
  | GH2Band -- stereo
  | GH2Silent -- mono

data GH2Audio = GH2Audio
  { gh2AudioSections :: [GH2AudioSection]
  , gh2LeadChannels  :: [Int]
  , gh2CoopChannels  :: [Int]
  , gh2DrumChannels  :: [Int]
  , gh2LeadTrack     :: F.FlexPartName
  , gh2CoopTrack     :: F.FlexPartName
  , gh2DrumTrack     :: Maybe F.FlexPartName
  , gh2CoopType      :: GH2Coop
  , gh2AnimBass      :: Maybe F.FlexPartName
  , gh2AnimDrums     :: Maybe F.FlexPartName
  , gh2AnimVocal     :: Maybe F.FlexPartName
  , gh2AnimKeys      :: Maybe F.FlexPartName
  , gh2Practice      :: [Maybe F.FlexPartName]
  , gh2LeadPractice  :: Int
  , gh2CoopPractice  :: Int
  , gh2DrumPractice  :: Maybe Int
  }

computeGH2Audio
  :: (Monad m)
  => SongYaml f
  -> TargetGH2
  -> (F.FlexPartName -> Bool) -- True if part has own audio
  -> StackTraceT m GH2Audio
computeGH2Audio song target hasAudio = do
  gh2LeadTrack <- case getPart (gh2_Guitar target) song >>= partGRYBO of
    Nothing -> fatal "computeGH2Audio: no lead guitar part selected"
    Just _  -> return $ gh2_Guitar target
  gh2DrumTrack <- case getPart (gh2_Drums target) song >>= partDrums of
    Nothing -> return Nothing
    Just _  -> return $ do
      guard $ gh2_DrumChart target
      Just $ gh2_Drums target
  let specifiedCoop = case gh2_Coop target of
        GH2Bass   -> gh2_Bass   target
        GH2Rhythm -> gh2_Rhythm target
      (gh2CoopTrack, gh2CoopType) = case getPart specifiedCoop song >>= partGRYBO of
        Nothing -> (gh2LeadTrack , GH2Rhythm      )
        Just _  -> (specifiedCoop, gh2_Coop target)
      leadAudio = hasAudio gh2LeadTrack
      coopAudio = gh2LeadTrack /= gh2CoopTrack && hasAudio gh2CoopTrack
      drumAudio = maybe False hasAudio gh2DrumTrack
      count = \case
        GH2PartStereo _ -> 2
        GH2PartMono   _ -> 1
        GH2Band         -> 2
        GH2Silent       -> 1
      maxVGSChannels = 6

      -- order: band, guitar, bass, drums, silent
      bandSection = [GH2Band]
      silentSection = do
        guard $ not leadAudio || not coopAudio || (isJust gh2DrumTrack && not drumAudio)
        [GH2Silent]
      drumSection = do
        guard drumAudio
        GH2PartStereo <$> toList gh2DrumTrack
      remainingLeadCoop = maxVGSChannels - sum (map count $ concat [bandSection, silentSection, drumSection])
      leadSection = do
        guard leadAudio
        if (coopAudio && remainingLeadCoop >= 3) || (not coopAudio && remainingLeadCoop >= 2)
          then [GH2PartStereo gh2LeadTrack]
          else [GH2PartMono   gh2LeadTrack]
      coopSection = do
        guard coopAudio
        if maxVGSChannels - sum (map count $ concat [bandSection, silentSection, drumSection, leadSection]) >= 2
          then [GH2PartStereo gh2CoopTrack]
          else [GH2PartMono   gh2CoopTrack]

      gh2AudioSections = concat [bandSection, leadSection, coopSection, drumSection, silentSection]
      indexes before section = take (sum $ map count section) [sum (map count before) ..]
      gh2LeadChannels = if leadAudio
        then indexes bandSection leadSection
        else indexes (concat [bandSection, leadSection, coopSection, drumSection]) silentSection
      gh2CoopChannels = if coopAudio
        then indexes (concat [bandSection, leadSection]) coopSection
        else indexes (concat [bandSection, leadSection, coopSection, drumSection]) silentSection
      gh2DrumChannels = if drumAudio
        then indexes (concat [bandSection, leadSection, coopSection]) drumSection
        else if isJust gh2DrumTrack
          then indexes (concat [bandSection, leadSection, coopSection, drumSection]) silentSection
          else []
      (gh2Practice, gh2LeadPractice, gh2CoopPractice, gh2DrumPractice) = if gh2_PracticeAudio target
        then let
          -- From testing, you can't just have 1 channel and assign it to both lead and bass/rhythm;
          -- you get no audio. I'm guessing any channels for an instrument other than the one
          -- you're playing are muted, even if they're also assigned to the one you're playing.
          contentsLead = guard leadAudio >> Just gh2LeadTrack
          contentsCoop = guard coopAudio >> Just gh2CoopTrack
          contentsDrum = case gh2DrumTrack of
            Nothing -> []
            Just _  -> [guard drumAudio >> gh2DrumTrack]
          allContents = [contentsLead, contentsCoop] <> contentsDrum
          in (allContents, 0, 1, guard (not $ null contentsDrum) >> Just 2)
        else ([Nothing], 0, 0, 0 <$ gh2DrumTrack) -- we'll make a single mono silent track
      gh2AnimBass  = gh2_Bass  target <$ (getPart (gh2_Bass  target) song >>= partGRYBO)
      gh2AnimDrums = gh2_Drums target <$ (getPart (gh2_Drums target) song >>= partDrums)
      gh2AnimVocal = gh2_Vocal target <$ (getPart (gh2_Vocal target) song >>= partVocal)
      gh2AnimKeys  = gh2_Keys  target <$ (getPart (gh2_Keys  target) song >>= partGRYBO)
  return GH2Audio{..}

midiRB3toGH2
  :: (SendMessage m)
  => SongYaml f
  -> TargetGH2
  -> GH2Audio
  -> F.Song (F.OnyxFile U.Beats)
  -> StackTraceT m U.Seconds
  -> StackTraceT m (F.Song (GH2File U.Beats), Int)
midiRB3toGH2 song _target audio inputMid@(F.Song tmap mmap onyx) getAudioLength = do
  timing <- basicTiming inputMid getAudioLength
  let makeMoods origMoods gems = let
        moods = if RTB.null origMoods
          then RB3.makeMoods tmap timing gems
          else origMoods
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
            (idle, play) = makeMoods (RB.fiveMood rbg)
              $ maybe mempty (splitEdges . edgeBlipsRB . RB.fiveGems)
              $ Map.lookup Expert $ RB.fiveDifficulties rbg
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
            RB.HandMap_AllChords -> HandMap_AllChords
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
            . noOpenNotes'
            . noTaps'
            . noExtendedSustains' standardBlipThreshold gap
            . applyForces (getForces5 fd)
            . strumHOPOTap' algo (fromIntegral ht / 480)
            . fixSloppyNotes (10 / 480)
            . closeNotes'
            $ fd
          in makePartGuitar fpart $ gryboComplete (Just ht) mmap $ toGtr trackOrig
      makeDrum fpart = case getPart fpart song >>= partDrums of
        Nothing -> mempty
        Just pd -> let
          trackOrig = buildDrumTarget
            DrumTargetRB1x
            pd
            (timingEnd timing)
            tmap
            (F.getFlexPart fpart onyx)
          in GH2DrumTrack
            { gh2drumDifficulties = flip fmap (RB.drumDifficulties trackOrig) $ \diff -> GH2DrumDifficulty
              { gh2drumStarPower = RB.drumOverdrive trackOrig
              , gh2drumPlayer1   = RB.drumPlayer1 trackOrig
              , gh2drumPlayer2   = RB.drumPlayer2 trackOrig
              , gh2drumGems      = fmap fst $ RB.drumGems diff
              }
            }
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
        } where (idle, play) = makeMoods (RB.fiveMood trk)
                  $ maybe mempty (splitEdges . edgeBlipsRB . RB.fiveGems)
                  $ Map.lookup Expert $ RB.fiveDifficulties trk
      makeBandDrums trk = mempty
        { drumsIdle = idle
        , drumsPlay = play
        , drumsKick  = void $ flip RTB.filter anims $ \case
          RB.KickRF -> True
          _         -> False
        , drumsCrash = void $ RTB.collectCoincident $ flip RTB.filter anims $ \case
          RB.Crash1{}        -> True
          RB.Crash2{}        -> True
          RB.Crash1RHChokeLH -> True
          RB.Crash2RHChokeLH -> True
          _                  -> False
        } where (idle, play) = makeMoods (RB.drumMood trk) $ Blip () () <$ anims
                anims = RB.drumAnimation $ RB.fillDrumAnimation (0.25 :: U.Seconds) tmap trk
      makeBandKeys trk = let
        (idle, play) = makeMoods (RB.fiveMood trk)
          $ maybe mempty (splitEdges . edgeBlipsRB . RB.fiveGems)
          $ Map.lookup Expert $ RB.fiveDifficulties trk
        in mempty { keysIdle = idle, keysPlay = play }
      makeBandSinger trk = let
        (idle, play) = makeMoods (RB.vocalMood trk)
          $ fmap (\case (p, True) -> NoteOn () p; (p, False) -> NoteOff p)
          $ RB.vocalNotes trk
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
        , gh2PartDrum = maybe mempty makeDrum $ gh2DrumTrack audio
        , gh2BandBass       = maybe mempty (\part -> makeBandBass $ fst $ getFive      $ F.getFlexPart part onyx) $ gh2AnimBass  audio
        , gh2BandDrums      = maybe mempty (\part -> makeBandDrums $ F.onyxPartDrums   $ F.getFlexPart part onyx) $ gh2AnimDrums audio
        , gh2BandKeys       = maybe mempty (\part -> makeBandKeys $ fst $ getFive      $ F.getFlexPart part onyx) $ gh2AnimKeys  audio
        , gh2BandSinger     = maybe mempty (\part -> makeBandSinger $ F.onyxPartVocals $ F.getFlexPart part onyx) $ gh2AnimVocal audio
        , gh2Events         = events
        , gh2Triggers       = triggers
        , ..
        }
  gh2Pad $ F.Song tmap mmap gh2

bandMembers :: SongYaml f -> GH2Audio -> Maybe [Either D.BandMember T.Text]
bandMembers song audio = let
  vocal = case gh2AnimVocal audio of
    Nothing    -> Nothing
    Just fpart -> Just $ fromMaybe Magma.Male $ getPart fpart song >>= partVocal >>= vocalGender
  bass = True -- we'll just assume there's always a bassist (not required though - Jordan)
  keys = isJust $ gh2AnimKeys audio
  drums = True -- crashes if missing
  in case (vocal, bass, keys, drums) of
    (Just Magma.Male, True, False, True) -> Nothing -- the default configuration
    _                                    -> Just $ map Left $ concat
      [ toList $ (\case Magma.Male -> D.MetalSinger; Magma.Female -> D.FemaleSinger) <$> vocal
      , [D.MetalBass     | bass ]
      , [D.MetalKeyboard | keys && isNothing vocal] -- singer overrides keyboard, can't have both
      , [D.MetalDrummer  | drums]
      ]

adjustSongText :: T.Text -> T.Text
adjustSongText = T.replace "&" "+"
  -- GH2 doesn't have & in the start-of-song-display font.
  -- In the song list it just displays as + anyway

makeGH2DTA :: SongYaml f -> T.Text -> (Int, Int) -> TargetGH2 -> GH2Audio -> T.Text -> D.SongPackage
makeGH2DTA song key preview target audio title = D.SongPackage
  { D.name = adjustSongText title
  , D.artist = adjustSongText $ getArtist $ _metadata song
  , D.caption = guard (not $ _cover $ _metadata song) >> Just "performed_by"
  , D.song = D.Song
    { D.songName      = "songs/" <> key <> "/" <> key
    , D.tracks        = D.DictList $ filter (\(_, ns) -> not $ null ns)
      [ ("guitar", map fromIntegral $ gh2LeadChannels audio)
      , (coop    , map fromIntegral $ gh2CoopChannels audio)
      , ("drum"  , map fromIntegral $ gh2DrumChannels audio)
      ]
    , D.pans          = gh2AudioSections audio >>= \case
      GH2PartStereo _ -> [-1, 1]
      GH2PartMono   _ -> [0]
      GH2Band         -> [-1, 1]
      GH2Silent       -> [0]
    , D.vols          = gh2AudioSections audio >>= \case
      GH2PartStereo _ -> [0, 0]
      GH2PartMono   _ -> [20 * (log 2 / log 10)] -- compensate for half volume later
      GH2Band         -> [0, 0]
      GH2Silent       -> [0]
    , D.cores         = gh2AudioSections audio >>= \case
      GH2PartStereo p -> if p == gh2LeadTrack audio then [1, 1] else [-1, -1]
      GH2PartMono p -> if p == gh2LeadTrack audio then [1] else [-1]
      GH2Band   -> [-1, -1]
      GH2Silent -> [-1]
    , D.midiFile      = "songs/" <> key <> "/" <> key <> ".mid"
    , D.hopoThreshold = Nothing
    }
  , D.animTempo = KTempoMedium
  , D.preview = bimap fromIntegral fromIntegral preview
  , D.quickplay = evalRand D.randomQuickplay $ mkStdGen $ hash key
  , D.practiceSpeeds = Just [100, 90, 75, 60]
  , D.songCoop = Nothing
  , D.songPractice1 = Just $ prac 90
  , D.songPractice2 = Just $ prac 75
  , D.songPractice3 = Just $ prac 60
  , D.band = bandMembers song audio
  } where
    coop = case gh2CoopType audio of GH2Bass -> "bass"; GH2Rhythm -> "rhythm"
    prac :: Int -> D.Song
    prac speed = D.Song
      { D.songName = if gh2_PracticeAudio target
        then "songs/" <> key <> "/" <> key <> "_p" <> T.pack (show speed)
        else "songs/" <> key <> "/" <> key <> "_empty"
      , D.tracks = D.DictList $ filter (\(_, ns) -> not $ null ns)
        [ ("guitar", [fromIntegral $ gh2LeadPractice audio])
        , (coop    , [fromIntegral $ gh2CoopPractice audio])
        , ("drum"  , maybe [] (\n -> [fromIntegral n]) $ gh2DrumPractice audio)
        ]
      , D.pans = 0 <$ gh2Practice audio
      , D.vols = 0 <$ gh2Practice audio
      , D.cores = (-1) <$ gh2Practice audio
      , D.midiFile = "songs/" <> key <> "/" <> key <> ".mid"
      , D.hopoThreshold = Nothing
      }

makeGH2DTA360 :: SongYaml f -> T.Text -> (Int, Int) -> TargetGH2 -> GH2Audio -> T.Text -> D.SongPackage
makeGH2DTA360 song key preview target audio title = D.SongPackage
  { D.name = adjustSongText title
  , D.artist = adjustSongText $ getArtist $ _metadata song
  , D.caption = guard (not $ _cover $ _metadata song) >> Just "performed_by"
  , D.song = D.Song
    { D.songName      = "songs/" <> key <> "/" <> key
    , D.tracks        = D.DictList $ filter (\(_, ns) -> not $ null ns)
      [ ("guitar", map fromIntegral $ gh2LeadChannels audio)
      , (coop    , map fromIntegral $ gh2CoopChannels audio)
      , ("drum"  , map fromIntegral $ gh2DrumChannels audio)
      ]
    , D.pans          = gh2AudioSections audio >>= \case
      GH2PartStereo _ -> [-1, 1]
      GH2PartMono   _ -> [0]
      GH2Band         -> [-1, 1]
      GH2Silent       -> [0]
    , D.vols          = gh2AudioSections audio >>= \case
      GH2PartStereo _ -> [0, 0]
      GH2PartMono   _ -> [20 * (log 2 / log 10)] -- compensate for half volume later
      GH2Band         -> [0, 0]
      GH2Silent       -> [0]
    , D.cores         = gh2AudioSections audio >>= \case
      GH2PartStereo p -> if p == gh2LeadTrack audio then [1, 1] else [-1, -1]
      GH2PartMono p -> if p == gh2LeadTrack audio then [1] else [-1]
      GH2Band       -> [-1, -1]
      GH2Silent     -> [-1]
    , D.midiFile      = "songs/" <> key <> "/" <> key <> ".mid"
    , D.hopoThreshold = Nothing
    }
  , D.animTempo = KTempoMedium
  , D.preview = bimap fromIntegral fromIntegral preview
  , D.quickplay = evalRand D.randomQuickplay $ mkStdGen $ hash key
  , D.practiceSpeeds = Nothing
  , D.songCoop = Nothing
  , D.songPractice1 = Nothing
  , D.songPractice2 = Nothing
  , D.songPractice3 = Nothing
  , D.band = bandMembers song audio
  } where
    coop = case gh2_Coop target of GH2Bass -> "bass"; GH2Rhythm -> "rhythm"
