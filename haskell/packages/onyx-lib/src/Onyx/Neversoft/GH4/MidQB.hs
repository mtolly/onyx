{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Neversoft.GH4.MidQB where

import           Control.Monad                    (forM, guard)
import           Data.Bits                        (testBit, (.&.))
import qualified Data.ByteString                  as B
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (sort)
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           Data.Word
import           Onyx.Guitar                      (emit5')
import           Onyx.MIDI.Common                 (Difficulty (..),
                                                   StrumHOPOTap (..))
import qualified Onyx.MIDI.Track.Drums            as Drums
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.Neversoft.CRC               (qbKeyCRC)
import           Onyx.Neversoft.GH3.MidQB         (findSection, groupBy3,
                                                   listOfPairs, listOfTriples,
                                                   readGH3TempoMap, toSeconds)
import           Onyx.Neversoft.GH4.Metadata
import           Onyx.Neversoft.GH5.Note          (Single (..), VocalNote (..),
                                                   convertVocals)
import           Onyx.Neversoft.QB
import           Onyx.Sections                    (simpleSection)
import           Onyx.StackTrace
import qualified Sound.MIDI.Util                  as U

data GH4MidQB = GH4MidQB
  { gh4TimeSignatures  :: [(Word32, Word32, Word32)]
  , gh4FretBars        :: [Word32]

  , gh4Guitar          :: GH4Part
  , gh4Rhythm          :: GH4Part
  , gh4Drum            :: GH4Part
  , gh4Aux             :: GH4Part
  , gh4GuitarCoop      :: GH4Part
  , gh4RhythmCoop      :: GH4Part

  -- , gh4BossBattleP1 :: () -- dlc50_bossbattlep1
  -- , gh4BossBattleP2 :: () -- dlc50_bossbattlep2

  , gh4GuitarMarkers   :: [(Word32, Word32)] -- dlc50_guitar_markers
  , gh4RhythmMarkers   :: [(Word32, Word32)] -- dlc50_rhythm_markers
  , gh4DrumMarkers     :: [(Word32, Word32)] -- dlc50_drum_markers

  , gh4EasyDrumFill    :: [(Word32, Word32)] -- dlc50_easy_drumfill
  , gh4MediumDrumFill  :: [(Word32, Word32)] -- dlc50_medium_drumfill
  , gh4HardDrumFill    :: [(Word32, Word32)] -- dlc50_hard_drumfill
  , gh4ExpertDrumFill  :: [(Word32, Word32)] -- dlc50_expert_drumfill

  -- , gh4EasyDrumUnmute   :: () -- dlc50_easy_drumunmute
  -- , gh4MediumDrumUnmute :: () -- dlc50_medium_drumunmute
  -- , gh4HardDrumUnmute   :: () -- dlc50_hard_drumunmute
  -- , gh4ExpertDrumUnmute :: () -- dlc50_expert_drumunmute

  -- , gh4BackgroundNotes :: GH4Background [Word32]
  -- dlc50_scripts_notes
  -- dlc50_anim_notes
  -- dlc50_triggers_notes
  -- dlc50_cameras_notes
  -- dlc50_lightshow_notes
  -- dlc50_crowd_notes
  -- dlc50_drums_notes

  -- , gh4Background :: GH4Background GH3AnimEvent
  -- dlc50_scripts
  -- dlc50_anim
  -- dlc50_triggers
  -- dlc50_cameras
  -- dlc50_lightshow
  -- dlc50_crowd
  -- dlc50_drums

  -- , gh4Performance :: [GH3AnimEvent]
  -- dlc50_performance

  , gh4SongVocals      :: [(Word32, Word32, Word32)] -- dlc50_song_vocals
  , gh4VocalsFreeform  :: [(Word32, Word32, Word32)] -- dlc50_vocals_freeform
  , gh4VocalsPhrases   :: [(Word32, Word32)] -- dlc50_vocals_phrases
  , gh4VocalsNoteRange :: (Word32, Word32) -- dlc50_vocals_note_range
  , gh4Lyrics          :: [(Word32, Word32)] -- dlc50_lyrics
  , gh4VocalsMarkers   :: [(Word32, Word32)] -- dlc50_vocals_markers

  } deriving (Show)

data GH4Part = GH4Part
  { gh4Easy        :: GH4Difficulty
  , gh4Medium      :: GH4Difficulty
  , gh4Hard        :: GH4Difficulty
  , gh4Expert      :: GH4Difficulty
  , gh4FaceOffStar :: [(Word32, Word32, Word32)]
  , gh4FaceOffP1   :: [(Word32, Word32)]
  , gh4FaceOffP2   :: [(Word32, Word32)]
  } deriving (Show)

data GH4Difficulty = GH4Difficulty
  { gh4Notes       :: [(Word32, Word32)]
  , gh4StarPower   :: [(Word32, Word32, Word32)]
  , gh4BattleStars :: [(Word32, Word32, Word32)]
  , gh4Tapping     :: [(Word32, Word32, Word32)]
  -- dlc50_expert_whammycontroller
  } deriving (Show)

data GH4Background a = GH4Background
  { gh4Scripts   :: [a]
  , gh4Anim      :: [a]
  , gh4Triggers  :: [a]
  , gh4Cameras   :: [a]
  , gh4LightShow :: [a]
  , gh4Crowd     :: [a]
  , gh4Drums     :: [a]
  } deriving (Show)

parseGH4MidQB :: (Monad m) => B.ByteString -> [QBSection Word32 Word32] -> StackTraceT m GH4MidQB
parseGH4MidQB dlc qb = do

  gh4TimeSignatures <- findSection qb (dlc <> "_timesig") listOfTriples
  gh4FretBars       <- findSection qb (dlc <> "_fretbars") $ \case
    QBArrayOfFloatRaw [] -> return []
    QBArrayOfInteger ns  -> return ns
    _                    -> fatal "Expected array of integers for fretbars"

  gh4Guitar     <- parsePart dlc qb ""
  gh4Rhythm     <- parsePart dlc qb "rhythm_"
  gh4Drum       <- parsePart dlc qb "drum_"
  gh4Aux        <- parsePart dlc qb "aux_"
  gh4GuitarCoop <- parsePart dlc qb "guitarcoop_"
  gh4RhythmCoop <- parsePart dlc qb "rhythmcoop_"

  gh4GuitarMarkers <- findSection qb (dlc <> "_guitar_markers") parseMarkersGH4
  gh4RhythmMarkers <- findSection qb (dlc <> "_rhythm_markers") parseMarkersGH4
  gh4DrumMarkers <- findSection qb (dlc <> "_drum_markers") parseMarkersGH4

  gh4EasyDrumFill <- findSection qb (dlc <> "_easy_drumfill") listOfPairs
  gh4MediumDrumFill <- findSection qb (dlc <> "_medium_drumfill") listOfPairs
  gh4HardDrumFill <- findSection qb (dlc <> "_hard_drumfill") listOfPairs
  gh4ExpertDrumFill <- findSection qb (dlc <> "_expert_drumfill") listOfPairs

  gh4SongVocals <- findSection qb (dlc <> "_song_vocals") $ \case
    QBArrayOfFloatRaw [] -> return []
    QBArrayOfInteger ns  -> groupBy3 ns
    _                    -> fatal "Expected array of integers for vocals notes"
  gh4VocalsFreeform <- findSection qb (dlc <> "_vocals_freeform") listOfTriples
  gh4VocalsPhrases <- findSection qb (dlc <> "_vocals_phrases") $ \case
    QBArrayOfFloatRaw [] -> return []
    QBArrayOfInteger ns  -> groupBy2 ns
    _                    -> fatal "Expected array of integers for vocals phrases"
  gh4VocalsNoteRange <- findSection qb (dlc <> "_vocals_note_range") $ \case
    QBArrayOfInteger [x, y] -> return (x, y)
    _                       -> fatal "Unexpected vocals note range contents, expected array of 2 integers"
  gh4Lyrics <- findSection qb (dlc <> "_lyrics") parseLyrics
  gh4VocalsMarkers <- findSection qb (dlc <> "_vocals_markers") parseMarkersGH4

  return GH4MidQB{..}

parsePart :: (Monad m) => B.ByteString -> [QBSection Word32 Word32] -> B.ByteString -> StackTraceT m GH4Part
parsePart dlc qb part = let
  parseTrack diff = do
    gh4Notes       <- findSection qb (dlc <> "_song_" <> part <> diff) $ \case
      QBArrayOfFloatRaw [] -> return []
      QBArrayOfInteger ns  -> groupBy2 ns
      _                    -> fatal "Expected array of integers for notes"
    gh4StarPower   <- findSection qb (dlc <> "_" <> part <> diff <> "_star") listOfTriples
    gh4BattleStars <- findSection qb (dlc <> "_" <> part <> diff <> "_starbattlemode") listOfTriples
    gh4Tapping     <- findSection qb (dlc <> "_" <> part <> diff <> "_tapping") listOfTriples
    return GH4Difficulty{..}
  in do
    gh4Easy   <- parseTrack "easy"
    gh4Medium <- parseTrack "medium"
    gh4Hard   <- parseTrack "hard"
    gh4Expert <- parseTrack "expert"
    gh4FaceOffStar <- findSection qb (dlc <> "_" <> part <> "faceoffstar") listOfTriples
    gh4FaceOffP1 <- findSection qb (dlc <> "_" <> part <> "faceoffp1") listOfPairs
    gh4FaceOffP2 <- findSection qb (dlc <> "_" <> part <> "faceoffp2") listOfPairs
    return GH4Part{..}

groupBy2 :: (Monad m) => [w] -> StackTraceT m [(w, w)]
groupBy2 = go [] where
  go pairs []             = return $ reverse pairs
  go pairs (x : y : rest) = go ((x, y) : pairs) rest
  go _     _              = fatal "Expected a list whose length is a multiple of 2"

parseMarkersGH4 :: (Monad m) => QBArray Word32 Word32 -> StackTraceT m [(Word32, Word32)]
parseMarkersGH4 = \case
  QBArrayOfFloatRaw [] -> return []
  QBArrayOfStruct marks -> forM marks $ \case
    QBStructHeader : items -> let
      time      = [v | QBStructItemInteger       k v <- items, k == qbKeyCRC "time"  ]
      marker    = [v | QBStructItemQbKeyString   k v <- items, k == qbKeyCRC "marker"] -- guitar/rhythm/drum
      markerVox = [v | QBStructItemQbKeyStringQs k v <- items, k == qbKeyCRC "marker"] -- vocals
      in case (time, marker, markerVox) of
        ([t], [m], []) -> return (t, m)
        ([t], [], [m]) -> return (t, m)
        _              -> fatal $ "Unexpected contents of marker: " <> show items
    _ -> fatal "No struct header in marker"
  _ -> fatal "Expected array of structs for markers"

parseLyrics :: (Monad m) => QBArray Word32 Word32 -> StackTraceT m [(Word32, Word32)]
parseLyrics = \case
  QBArrayOfFloatRaw [] -> return []
  QBArrayOfStruct lyrics -> forM lyrics $ \case
    QBStructHeader : items -> let
      time   = [v | QBStructItemInteger       k v <- items, k == qbKeyCRC "time"]
      textQs = [v | QBStructItemQbKeyStringQs k v <- items, k == qbKeyCRC "text"]
      in case (time, textQs) of
        ([t], [txt]) -> return (t, txt)
        _            -> fatal $ "Unexpected contents of lyric: " <> show items
    _ -> fatal "No struct header in lyric"
  _ -> fatal "Expected array of structs for lyrics"

{-
from James Bond Theme (dlc50), strings matched with the help of Addy's code

- 2644139194 dlc50_timesig
- 3652363555 dlc50_fretbars
- 3313040141 dlc50_song_easy
- 917628940 dlc50_song_medium
- 1292861000 dlc50_song_hard
- 3218685689 dlc50_song_expert
- 2957282466 dlc50_easy_star
- 2810006772 dlc50_medium_star
- 1479831812 dlc50_hard_star
- 2282998790 dlc50_expert_star
- 2519936851 dlc50_easy_starbattlemode
- 2718248962 dlc50_medium_starbattlemode
- 3386050792 dlc50_hard_starbattlemode
- 746428181 dlc50_expert_starbattlemode
- 853557778 dlc50_easy_tapping
- 3459141441 dlc50_medium_tapping
- 503483876 dlc50_hard_tapping
- 3924245700 dlc50_expert_tapping
- 3963153447 dlc50_easy_whammycontroller
- 3264264689 dlc50_medium_whammycontroller
- 1308848607 dlc50_hard_whammycontroller
- 2409727316 dlc50_expert_whammycontroller
- 3139566328 dlc50_faceoffstar
- 4283151686 dlc50_faceoffp1
- 1715660028 dlc50_faceoffp2
- 1024875386 dlc50_song_rhythm_easy
- 2149145787 dlc50_song_rhythm_medium
- 3043089983 dlc50_song_rhythm_hard
- 158435918 dlc50_song_rhythm_expert
- 3604294046 dlc50_rhythm_easy_star
- 1915880701 dlc50_rhythm_medium_star
- 1051055160 dlc50_rhythm_hard_star
- 1566344207 dlc50_rhythm_expert_star
- 1099122678 dlc50_rhythm_easy_starbattlemode
- 840713844 dlc50_rhythm_medium_starbattlemode
- 509829197 dlc50_rhythm_hard_starbattlemode
- 3160727907 dlc50_rhythm_expert_starbattlemode
- 1273607678 dlc50_rhythm_easy_tapping
- 524086551 dlc50_rhythm_medium_tapping
- 1728805384 dlc50_rhythm_hard_tapping
- 955621010 dlc50_rhythm_expert_tapping
- 2082576977 dlc50_rhythm_easy_whammycontroller
- 43197329 dlc50_rhythm_medium_whammycontroller
- 3726247849 dlc50_rhythm_hard_whammycontroller
- 1336024884 dlc50_rhythm_expert_whammycontroller
- 1852748529 dlc50_rhythm_faceoffstar
- 2581205114 dlc50_rhythm_faceoffp1
- 13844928 dlc50_rhythm_faceoffp2
- 2300904793 dlc50_song_drum_easy
- 2992890650 dlc50_song_drum_medium
- 22183964 dlc50_song_drum_hard
- 990590447 dlc50_song_drum_expert
- 2016793496 dlc50_drum_easy_star
- 2030048921 dlc50_drum_medium_star
- 2420452926 dlc50_drum_hard_star
- 1450088043 dlc50_drum_expert_star
- 3674338279 dlc50_drum_easy_starbattlemode
- 2915666035 dlc50_drum_medium_starbattlemode
- 2229392476 dlc50_drum_hard_starbattlemode
- 598814564 dlc50_drum_expert_starbattlemode
- 20783789 dlc50_drum_easy_tapping
- 3770789811 dlc50_drum_medium_tapping
- 769650011 dlc50_drum_hard_tapping
- 3339246646 dlc50_drum_expert_tapping
- 3824440406 dlc50_drum_easy_whammycontroller
- 823051572 dlc50_drum_medium_whammycontroller
- 1104136622 dlc50_drum_hard_whammycontroller
- 2084567441 dlc50_drum_expert_whammycontroller
- 1700569237 dlc50_drum_faceoffstar
- 926607996 dlc50_drum_faceoffp1
- 2922625990 dlc50_drum_faceoffp2
- 480794854 dlc50_song_aux_easy
- 3303336361 dlc50_song_aux_medium
- 2497699235 dlc50_song_aux_hard
- 1301036892 dlc50_song_aux_expert
- 2157047550 dlc50_aux_easy_star
- 315584013 dlc50_aux_medium_star
- 1759581016 dlc50_aux_hard_star
- 1034022655 dlc50_aux_expert_star
- 2702196125 dlc50_aux_easy_starbattlemode
- 80055465 dlc50_aux_medium_starbattlemode
- 4277148198 dlc50_aux_hard_starbattlemode
- 2327653310 dlc50_aux_expert_starbattlemode
- 4130650760 dlc50_aux_easy_tapping
- 4142306091 dlc50_aux_medium_tapping
- 3671487870 dlc50_aux_hard_tapping
- 3509575854 dlc50_aux_expert_tapping
- 1257787532 dlc50_aux_easy_whammycontroller
- 1303676340 dlc50_aux_medium_whammycontroller
- 3905146228 dlc50_aux_hard_whammycontroller
- 8763665 dlc50_aux_expert_whammycontroller
- 244570113 dlc50_aux_faceoffstar
- 3483293466 dlc50_aux_faceoffp1
- 1452770976 dlc50_aux_faceoffp2
- 907079284 dlc50_song_guitarcoop_easy
- 2777759024 dlc50_song_guitarcoop_medium
- 3194448689 dlc50_song_guitarcoop_hard
- 754576325 dlc50_song_guitarcoop_expert
- 40544944 dlc50_guitarcoop_easy_star
- 226757620 dlc50_guitarcoop_medium_star
- 3927594774 dlc50_guitarcoop_hard_star
- 585796358 dlc50_guitarcoop_expert_star
- 529902402 dlc50_guitarcoop_easy_starbattlemode
- 453631373 dlc50_guitarcoop_medium_starbattlemode
- 1081445625 dlc50_guitarcoop_hard_starbattlemode
- 2507242138 dlc50_guitarcoop_expert_starbattlemode
- 843758421 dlc50_guitarcoop_easy_tapping
- 3044531345 dlc50_guitarcoop_medium_tapping
- 514356387 dlc50_guitarcoop_hard_tapping
- 2461970196 dlc50_guitarcoop_expert_tapping
- 1429471656 dlc50_guitarcoop_easy_whammycontroller
- 2687507132 dlc50_guitarcoop_medium_whammycontroller
- 4144995408 dlc50_guitarcoop_hard_whammycontroller
- 3976275481 dlc50_guitarcoop_expert_whammycontroller
- 299417080 dlc50_guitarcoop_faceoffstar
- 1298502484 dlc50_guitarcoop_faceoffp1
- 3563897582 dlc50_guitarcoop_faceoffp2
- 2303902141 dlc50_song_rhythmcoop_easy
- 343462700 dlc50_song_rhythmcoop_medium
- 19219704 dlc50_song_rhythmcoop_hard
- 2635072985 dlc50_song_rhythmcoop_expert
- 243102891 dlc50_rhythmcoop_easy_star
- 3319346090 dlc50_rhythmcoop_medium_star
- 3859646733 dlc50_rhythmcoop_hard_star
- 3937907544 dlc50_rhythmcoop_expert_star
- 2915047856 dlc50_rhythmcoop_easy_starbattlemode
- 418135623 dlc50_rhythmcoop_medium_starbattlemode
- 4062199307 dlc50_rhythmcoop_hard_starbattlemode
- 2526320976 dlc50_rhythmcoop_expert_starbattlemode
- 3193019038 dlc50_rhythmcoop_easy_tapping
- 3507506769 dlc50_rhythmcoop_medium_tapping
- 2461226344 dlc50_rhythmcoop_hard_tapping
- 4141432276 dlc50_rhythmcoop_expert_tapping
- 1456595554 dlc50_rhythmcoop_easy_whammycontroller
- 2821100895 dlc50_rhythmcoop_medium_whammycontroller
- 4109008794 dlc50_rhythmcoop_hard_whammycontroller
- 3843533306 dlc50_rhythmcoop_expert_whammycontroller
- 3649406374 dlc50_rhythmcoop_faceoffstar
- 1098009935 dlc50_rhythmcoop_faceoffp1
- 3631946997 dlc50_rhythmcoop_faceoffp2
- 1225330805 dlc50_bossbattlep1
- 3489677775 dlc50_bossbattlep2
- 4106632631 dlc50_guitar_markers
- 1720246916 dlc50_rhythm_markers
- 2555045162 dlc50_drum_markers
- 3949401753 dlc50_easy_drumfill
- 435804378 dlc50_medium_drumfill
- 3214254975 dlc50_hard_drumfill
- 2215405062 dlc50_expert_drumfill
- 3203732991 dlc50_easy_drumunmute
- 427627536 dlc50_medium_drumunmute
- 232411748 dlc50_hard_drumunmute
- 405380477 dlc50_expert_drumunmute
- 2796637521 dlc50_scripts_notes
- 1507474171 dlc50_anim_notes
- 943360417 dlc50_triggers_notes
- 3272782430 dlc50_cameras_notes
- 3319322458 dlc50_lightshow_notes
- 3336830456 dlc50_crowd_notes
- 2570366775 dlc50_drums_notes
- 3056662657 dlc50_scripts
- 583501934 dlc50_anim
- 2272913074 dlc50_triggers
- 7424213 dlc50_cameras
- 3655194031 dlc50_lightshow
- 878187448 dlc50_crowd
- 2755106322 dlc50_drums
- 1953376944 dlc50_performance
- 3889954703 dlc50_song_vocals
- 280182175 dlc50_vocals_freeform
- 94578876 dlc50_vocals_phrases
- 3161667033 dlc50_vocals_note_range
- 1905153808 dlc50_lyrics
- 1446002506 dlc50_vocals_markers
-}

gh4ToMidi :: SongInfoGH4 -> HM.HashMap Word32 T.Text -> HM.HashMap Word32 T.Text -> GH4MidQB -> F.Song (F.FixedFile U.Beats)
gh4ToMidi info bankLyrics bankMarkers gh4 = let
  tempos = readGH3TempoMap (gh4TimeSignatures gh4) (gh4FretBars gh4)
  toBeats :: Word32 -> U.Beats
  toBeats = U.unapplyTempoMap tempos . toSeconds
  fromPairs ps = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort ps
  markers = fromPairs $ do
    (t, markerKey) <- gh4GuitarMarkers gh4
    let marker = case HM.lookup markerKey bankMarkers of
          -- just like WoR, not all songs have this but it overrides the default "end shortly after last note"
          Just "\\L_ENDOFSONG" -> Left ()
          Just "/L_ENDOFSONG"  -> Left () -- does this actually work? seen in .qs
          Just s               -> Right $ simpleSection $ stripBackL s
          Nothing              -> Right $ simpleSection $ T.pack $ show markerKey
    return (toBeats t, marker)
  fixed = mempty
    { F.fixedPartGuitar = getGuitarBass $ gh4Guitar gh4
    , F.fixedPartBass = getGuitarBass $ gh4Rhythm gh4
    , F.fixedPartDrums = getDrums $ gh4Drum gh4
    , F.fixedPartVocals = let
      notes = [ VocalNote time (fromIntegral dur) (fromIntegral pitch) | (time, dur, pitch) <- gh4SongVocals gh4 ]
      lyrics = do
        (time, lyricQs) <- gh4Lyrics gh4
        lyric <- toList $ HM.lookup lyricQs bankLyrics
        return $ Single time $ stripBackL lyric
      phrases = map fst $ gh4VocalsPhrases gh4
      in convertVocals toBeats notes lyrics phrases []
    , F.fixedEvents = mempty
      { eventsSections = RTB.mapMaybe (\case Right sect -> Just sect; _ -> Nothing) markers
      , eventsEnd      = RTB.mapMaybe (\case Left  ()   -> Just ()  ; _ -> Nothing) markers
      }
    }
  -- is this sustain cutoff still right? no trim this time
  sustainThreshold :: Word32
  sustainThreshold = floor $ (U.applyTempoMap tempos 1 / 2) * 1000
  getGuitarBassDiff diff = emit5' $ fromPairs $ do
    (time, bits) <- gh4Notes diff
    let pos = toBeats time
        len = bits .&. 0xFFFF -- think this is right but should confirm
        lenTrimmed = if len > sustainThreshold && not isOpen -- open notes don't sustain in game
          then len
          else 0
        lenBeats = case toBeats (time + lenTrimmed) - pos of
          0 -> Nothing
          l -> Just l
        isOpen = bits `testBit` 21
        sht = if any (\(tapPos, tapLen, _) -> tapPos <= time && time < tapPos + tapLen) $ gh4Tapping diff
          then Tap
          else if bits `testBit` 22
            then HOPO
            else Strum
    concat
      [ [(pos, ((Just Five.Green , sht), lenBeats)) | bits `testBit` 16]
      , [(pos, ((Just Five.Red   , sht), lenBeats)) | bits `testBit` 17]
      , [(pos, ((Just Five.Yellow, sht), lenBeats)) | bits `testBit` 18]
      , [(pos, ((Just Five.Blue  , sht), lenBeats)) | bits `testBit` 19]
      , [(pos, ((Just Five.Orange, sht), lenBeats)) | bits `testBit` 20]
      , [(pos, ((Nothing         , sht), lenBeats)) | isOpen           ]
      ]
  getGuitarBass part = mempty
    { Five.fiveDifficulties = Map.fromList
      [ (Easy  , getGuitarBassDiff $ gh4Easy   part)
      , (Medium, getGuitarBassDiff $ gh4Medium part)
      , (Hard  , getGuitarBassDiff $ gh4Hard   part)
      , (Expert, getGuitarBassDiff $ gh4Expert part)
      ]
    , Five.fiveOverdrive = makeSpan $ fmap (\(time, len, _) -> (time, len)) $ gh4StarPower $ gh4Expert part
    , Five.fivePlayer1 = makeSpan $ gh4FaceOffP1 part
    , Five.fivePlayer2 = makeSpan $ gh4FaceOffP2 part
    }
  getDrumsDiff diff = mempty
    { Drums.drumGems = fromPairs $ do
      (time, bits) <- gh4Notes diff
      let pos = toBeats time
          dynamicBit n = if bits `testBit` n then Drums.VelocityAccent else Drums.VelocityNormal
      -- TODO handle drum sustains, translate to lanes?
      concat
        [ [ (pos, (Drums.Pro Drums.Green  (), dynamicBit 23       )) | bits `testBit` 16 ]
        , [ (pos, (Drums.Red                , dynamicBit 24       )) | bits `testBit` 17 ]
        , [ (pos, (Drums.Pro Drums.Yellow (), dynamicBit 25       )) | bits `testBit` 18 ]
        , [ (pos, (Drums.Pro Drums.Blue   (), dynamicBit 26       )) | bits `testBit` 19 ]
        , [ (pos, (Drums.Orange             , dynamicBit 27       )) | bits `testBit` 20 ]
        -- GHM X+ has separate 1x/2x kick notes, unlike GH5/WoR which adds the 2 together to get X+.
        -- For now since we don't support the separate tracks,
        -- only include 1x kicks if they are also 2x.
        , [ (pos, (Drums.Kick               , Drums.VelocityNormal))
          | bits `testBit` 21 && (not (gh4DoubleKick info) || bits `testBit` 29)
          ]
        ]
    }
  getDrums part = mempty
    { Drums.drumDifficulties = Map.fromList
      [ (Easy  , getDrumsDiff $ gh4Easy   part)
      , (Medium, getDrumsDiff $ gh4Medium part)
      , (Hard  , getDrumsDiff $ gh4Hard   part)
      , (Expert, getDrumsDiff $ gh4Expert part)
      ]
    , Drums.drumOverdrive = makeSpan $ fmap (\(time, len, _) -> (time, len)) $ gh4StarPower $ gh4Expert part
    , Drums.drumPlayer1 = makeSpan $ gh4FaceOffP1 part
    , Drums.drumPlayer2 = makeSpan $ gh4FaceOffP2 part
    , Drums.drumKick2x = fromPairs $ do
      (time, bits) <- gh4Notes $ gh4Expert part
      guard $ bits `testBit` 29 && not (bits `testBit` 21)
      return (toBeats time, ())
    }
  makeSpan spans = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort $ do
    (time, len) <- spans
    [(toBeats time, True), (toBeats $ time + len, False)]
  in F.Song
    { F.s_tempos = tempos
    , F.s_signatures = U.measureMapFromTimeSigs U.Truncate $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ do
      (time, num, den) <- gh4TimeSignatures gh4
      let unit = 4 / fromIntegral den
          len = fromIntegral num * unit
      return (toBeats time, U.TimeSig len unit)
    , F.s_tracks = fixed
    }

-- Some DLC doesn't include a .qs file with markers because they are all already on the disc
worldTourDiscMarkers :: HM.HashMap Word32 T.Text
worldTourDiscMarkers = HM.fromList
  [ (0x0b620297, "'Pretty' Hardcore")
  , (0xda1c4243, "...she's no high climber")
  , (0x10e4158a, "...stole your water")
  , (0xcf5de29a, "...swear and kick and beg us")
  , (0xff69a1b3, "/L_ENDOFSONG") -- does this actually work?
  , (0x22aa3391, "78-E")
  , (0x00167369, "\\L_ENDOFSONG")
  , (0x6dae2510, "All Your Intro Are Belong To Us")
  , (0x6283f7bb, "Almost There...")
  , (0x7decdcb5, "American Rock!")
  , (0xd5e81011, "Analgesic Interlude 1")
  , (0xfec543d2, "Analgesic Interlude 2")
  , (0xd2d71591, "Anthem!")
  , (0x33619be0, "Arachnid-Solo")
  , (0x7fa09db1, "Back to the Intro")
  , (0xf5b0bb80, "Band Enters")
  , (0xfcfad5ab, "Bard's Anonymous")
  , (0x1eb8d4d4, "Bass Intro Again")
  , (0x6b3a489f, "Bass Intro")
  , (0x8b8d8774, "Bass Solo")
  , (0xf58d80e1, "Beautiful Lead 1")
  , (0x44fdfc80, "Beautiful Lead 2 / Solo")
  , (0x186f5a99, "Bend It, Like Wreckem'")
  , (0x9e75bc83, "Black Label Lunch")
  , (0x1f7d9853, "Black Widow Breakdown")
  , (0x98b7c262, "Blizzard of Thoth 1")
  , (0xb39a91a1, "Blizzard of Thoth 2")
  , (0xaa81a0e0, "Blizzard of Thoth 3")
  , (0xe5c03627, "Blizzard of Thoth 4")
  , (0xfcdb0766, "Blizzard of Thoth 5")
  , (0xd7f654a5, "Blizzard of Thoth 6")
  , (0xceed65e4, "Blizzard of Thoth 7")
  , (0x4975792b, "Blizzard of Thoth 8")
  , (0xeb38058d, "Born a' Ramblin")
  , (0x6931920b, "Break 1")
  , (0x421cc1c8, "Break 2")
  , (0x460eebfd, "Break A")
  , (0x6d23b83e, "Break B")
  , (0x7438897f, "Break C")
  , (0x25cb3cac, "Break Down 1")
  , (0x0ee66f6f, "Break Down 2")
  , (0xc9b0f42a, "Break Down Riff")
  , (0xde1417af, "Break Down")
  , (0x79564b7d, "Break that Heart 1")
  , (0x527b18be, "Break that Heart 2")
  , (0x4b6029ff, "Break that Heart 3")
  , (0x0421bf38, "Break that Heart 4")
  , (0x35e56a1a, "Break")
  , (0xff01a2e1, "Breakdizzle 1")
  , (0xd42cf122, "Breakdizzle 2")
  , (0x1885de2c, "Breakdown 1")
  , (0x33a88def, "Breakdown 2")
  , (0x1a365c57, "Breakdown Riff 1")
  , (0x311b0f94, "Breakdown Riff 2")
  , (0x28003ed5, "Breakdown Riff 3")
  , (0x4d604e4d, "Breakdown")
  , (0x9aa08258, "Breakin' it Down")
  , (0x38147d8f, "Bridge 1")
  , (0x7bcdcf55, "Bridge 1A")
  , (0x50e09c96, "Bridge 1B")
  , (0x49fbadd7, "Bridge 1C")
  , (0x13392e4c, "Bridge 2")
  , (0x697860bb, "Bridge 2A")
  , (0x42553378, "Bridge 2B")
  , (0x5b4e0239, "Bridge 2C")
  , (0x0a221f0d, "Bridge 3")
  , (0x456389ca, "Bridge 4")
  , (0x172b0479, "Bridge A")
  , (0x3c0657ba, "Bridge B")
  , (0xe35ed309, "Bridge Solo")
  , (0x41254a70, "Bridge to Nowhere")
  , (0x283c9475, "Bridge to Verse")
  , (0x4874e3de, "Bridge")
  , (0x95853ac9, "Bring It On Down")
  , (0x0bd96e11, "BRMC Has Left The Building")
  , (0xd44f66ac, "Brooklyn")
  , (0xc29ddaa5, "Build Up to Solo")
  , (0x960936b3, "Build-Up")
  , (0x1968ae09, "Building It Up")
  , (0x7885cd33, "Chorus 1 ")
  , (0x0c658976, "Chorus 1")
  , (0x046397d5, "Chorus 1A")
  , (0x2f4ec416, "Chorus 1B")
  , (0x2748dab5, "Chorus 2")
  , (0x16d6383b, "Chorus 2A")
  , (0x3dfb6bf8, "Chorus 2B")
  , (0x24e05ab9, "Chorus 2C")
  , (0x6ba1cc7e, "Chorus 2D")
  , (0x15a4eee0, "Chorus 3 / Bridge")
  , (0xb5147cf7, "Chorus 3 A")
  , (0x9e392f34, "Chorus 3 B")
  , (0x3e53ebf4, "Chorus 3")
  , (0x3bee7bfc, "Chorus 3a")
  , (0xae6a5f5e, "Chorus 3A")
  , (0x10c3283f, "Chorus 3b")
  , (0x85470c9d, "Chorus 3B")
  , (0x9c5c3ddc, "Chorus 3C")
  , (0x71127d33, "Chorus 4")
  , (0x33bd67e7, "Chorus 4A")
  , (0x18903424, "Chorus 4B")
  , (0x018b0565, "Chorus 4C")
  , (0x81714d4a, "Chorus 5 / Outro")
  , (0xf8a6412c, "Chorus 5 to Outro")
  , (0x68094c72, "Chorus 5")
  , (0x8b010082, "Chorus 5A")
  , (0xa02c5341, "Chorus 5B")
  , (0x43241fb1, "Chorus 6")
  , (0xb8e74abb, "Chorus Again")
  , (0x16357c52, "Chorus")
  , (0x0ccf62a5, "Coda")
  , (0x15a4505d, "Confess Your Love")
  , (0xcf1d5948, "Da Boom!")
  , (0x59ae5159, "Demolition Main Riff 1")
  , (0x7283029a, "Demolition Main Riff 2")
  , (0x62554f49, "Detention Riff 1a")
  , (0x49781c8a, "Detention Riff 1b")
  , (0x70e0e0a7, "Detention Riff 2a")
  , (0x5bcdb364, "Detention Riff 2b")
  , (0xc85c87c2, "Detention Riff 3a")
  , (0x9d50ed63, "Disaster Riff 1")
  , (0xd2edf4b7, "Disaster Riff 2 / Outro")
  , (0xd109bed4, "Down 'n Dirty")
  , (0xc6d8eb7c, "Drum and Bass Solo")
  , (0xd5845438, "Drum Intro")
  , (0xe48e6dcf, "Eddie Shreds!  Part 1")
  , (0xcfa33e0c, "Eddie Shreds!  Part 2")
  , (0xd6b80f4d, "Eddie Shreds!  Part 3")
  , (0x227e4ce9, "Eight-Legged Licks A")
  , (0x09531f2a, "Eight-Legged Licks B")
  , (0x13b1dd85, "Electric Sitar Lead")
  , (0xe722d6e0, "End Anthem!")
  , (0xa2b532b8, "End Chorus")
  , (0x9701bbbd, "End Spammery")
  , (0x69db4030, "Ending")
  , (0x73d43df2, "Enter Barker")
  , (0xfd8401d5, "Enter Drums, Enter Bass")
  , (0x99016114, "Enter Guitar")
  , (0xe0e01553, "Extra Credit 1")
  , (0xcbcd4690, "Extra Credit 2")
  , (0x50310153, "Fast Riff")
  , (0x875957ec, "Final Bridge")
  , (0xd918c860, "Final Chorus")
  , (0xe4ec4d08, "Final Verse")
  , (0xdc089db5, "Freak Riff")
  , (0x3afb8f34, "Fugue")
  , (0xff92a60a, "Fuzz")
  , (0x92804d35, "Fuzzy Twinkles")
  , (0xa2a93970, "Go Lenny Go!")
  , (0xdebf1515, "Go Time 1")
  , (0xf59246d6, "Go Time 2")
  , (0xec897797, "Go Time 3")
  , (0x13883900, "GO!")
  , (0xb3ba963e, "Go!")
  , (0x059442d5, "Goodbye American Woman")
  , (0x8b23d74f, "Grab Bag of Nails")
  , (0x7f87236e, "Grind Control")
  , (0x006ec9e1, "Grind Me Under 1")
  , (0xcb8e4ddf, "Grind Me Under 2a")
  , (0xe0a31e1c, "Grind Me Under 2b")
  , (0xb6d9610e, "Guitar Alone")
  , (0x38d7120c, "Guitar and Vox")
  , (0x53a4e7aa, "Guitar Break")
  , (0xef038171, "Guitar Interlude")
  , (0x37feebf3, "Guitar Intro 1")
  , (0x1cd3b830, "Guitar Intro 2")
  , (0x05c88971, "Guitar Intro 3")
  , (0x4a891fb6, "Guitar Intro 4")
  , (0x1e45f3de, "Guitar Intro Again")
  , (0xe27be92e, "Guitar Intro Break")
  , (0x6e1ad472, "Guitar Intro")
  , (0x373507a2, "Guitar Lead A")
  , (0x1c185461, "Guitar Lead B")
  , (0x3a68f2f6, "Guitar Lead")
  , (0xf28d5538, "Guitar Solo 1A")
  , (0xd9a006fb, "Guitar Solo 1B")
  , (0xc0bb37ba, "Guitar Solo 1C")
  , (0xab4b2ac1, "Guitar Solo 2")
  , (0xaf5900f4, "Guitar Solo A")
  , (0x84745337, "Guitar Solo B")
  , (0xc3b291d9, "Guitar solo part 1")
  , (0xe89fc21a, "Guitar solo part 2")
  , (0x2ff6f301, "Guitar Solo")
  , (0x15a131db, "Half-a-Man Outro")
  , (0x6cc0724f, "Head 1")
  , (0x47ed218c, "Head 2")
  , (0x3a7585ef, "Heartbreaker Riff 1 / Intro")
  , (0x3d5027fc, "Heartbreaker Riff 2")
  , (0x244b16bd, "Heartbreaker Riff 3")
  , (0x04dec2c5, "High Rolling Outro")
  , (0x942b7c50, "Holland Tunnel")
  , (0x9f3e4261, "Hook 1")
  , (0xb41311a2, "Hook 2")
  , (0xd315287d, "Instro-Verse A")
  , (0xf8387bbe, "Instro-Verse B")
  , (0x69e1c166, "Instromedley 1")
  , (0x42cc92a5, "Instromedley 2")
  , (0x5bd7a3e4, "Instromedley 3")
  , (0x024287c0, "Interlude")
  , (0x74433c15, "Into the Night 2")
  , (0x6d580d54, "Into the Night 3")
  , (0x22199b93, "Into the Night 4")
  , (0xf8bf03cd, "Into The Pit")
  , (0x2753b6e2, "Into the Verse 1")
  , (0x0c7ee521, "Into the Verse 2")
  , (0x1565d460, "Into the Verse 3")
  , (0xe6f4ee6e, "Into the Verse")
  , (0x5dda5efe, "Intro / Into the Night ")
  , (0xd4247b02, "Intro / Jam Riff")
  , (0x8ed29793, "Intro 1")
  , (0xa5ffc450, "Intro 2")
  , (0xbce4f511, "Intro 3")
  , (0xa1edee65, "Intro A")
  , (0x579c269b, "Intro A: Piano")
  , (0x8ac0bda6, "Intro B")
  , (0xda3c68e8, "Intro B: Guitars")
  , (0xb8641384, "Intro Continued")
  , (0x7245fc18, "Intro Lead")
  , (0xbb6f4684, "Intro Revisited")
  , (0x1bc4d015, "Intro Riff Again")
  , (0xc3574541, "Intro Riff")
  , (0x67dbfdef, "Intro Solo")
  , (0x085b59c2, "Intro")
  , (0x1fe8f8f0, "Jam Riff 2")
  , (0xef1243b7, "Jam that Outro")
  , (0x3586c969, "Jazzy Interlude")
  , (0x589e6d63, "Jersey City")
  , (0x6c09a190, "Jersey Licks")
  , (0xba055444, "Jk... NOW It's Over")
  , (0xf10d336b, "Jump of Society!")
  , (0xe79dcf53, "Just Kidding!")
  , (0x2ee18c67, "Key Change")
  , (0x28bf4665, "Key Solo")
  , (0xb3c1eabb, "Keys to the Abbey")
  , (0x592a38e7, "Kick Out that Solo A")
  , (0x72076b24, "Kick Out that Solo B")
  , (0x6b1c5a65, "Kick Out that Solo C")
  , (0xf86331f8, "Latin Breakdown 1A")
  , (0xd34e623b, "Latin Breakdown 1B")
  , (0xead69e16, "Latin Breakdown 2A")
  , (0xc1fbcdd5, "Latin Breakdown 2B")
  , (0xd8e0fc94, "Latin Breakdown 2C")
  , (0x3edd5718, "Lead")
  , (0x72aad7e4, "Let There Be Rhodes")
  , (0xb731ea90, "Lock In The Groove")
  , (0xe091a41d, "Long, Sweet Ambient Intro")
  , (0xbfe3fdde, "Lost All the Chorus")
  , (0x2a020528, "Lost and Loaded")
  , (0x3be25dac, "Loverly A")
  , (0x10cf0e6f, "Loverly B")
  , (0x115c3370, "Lovin the Riff")
  , (0x8a95a5bc, "Mad Meat ")
  , (0xbdaaa2d4, "Main Riff 1")
  , (0x9687f117, "Main Riff 2")
  , (0x8f9cc056, "Main Riff 3")
  , (0xff4c9157, "Main Riff")
  , (0xd19b3acc, "Manhattan")
  , (0xa58a58ca, "Metal Mouthwash")
  , (0x8d73bbc5, "Middle 8")
  , (0xd0339d59, "Money In The Tank")
  , (0x48b40c47, "Newark")
  , (0x123a4c56, "Not the Enemy 1")
  , (0x39171f95, "Not the Enemy 2")
  , (0x0c3efaf3, "Not the Enemy Final")
  , (0x9034c5e6, "NOT!")
  , (0x6966b476, "Ok Now It's the Outro")
  , (0x19179053, "One Armed Riffage")
  , (0xfa3d7faa, "One Last Bass Intro")
  , (0x53e04def, "One Last Chorus")
  , (0xdb7fdf2c, "One last Chorus")
  , (0x396e54d6, "One More Guitar Solo")
  , (0x8aa59b6b, "Organ Lead")
  , (0x24a5ec21, "Outro A")
  , (0x0f88bfe2, "Outro B")
  , (0x16938ea3, "Outro C")
  , (0x25124ebb, "Outro Lead")
  , (0x249404f8, "Outro")
  , (0xaeb9e89e, "Party Time!")
  , (0xb42424e8, "Pennsylvania")
  , (0x4eb6ff93, "Piano Solo")
  , (0x8616277c, "Pre Chorus 1")
  , (0xad3b74bf, "Pre Chorus 2")
  , (0xb42045fe, "Pre Chorus 3")
  , (0x75bde897, "Pre-Chorus 1")
  , (0x5e90bb54, "Pre-Chorus 2")
  , (0x478b8a15, "Pre-Chorus 3")
  , (0xd107d568, "Pre-Verse")
  , (0x80142b80, "Prisoner's Riff")
  , (0x03fc0912, "Quick Riff 1")
  , (0x28d15ad1, "Quick Riff 2")
  , (0xeed4cff5, "Rain Season")
  , (0xa2691a74, "Ready?")
  , (0x5378ee54, "Refrain 1")
  , (0x7855bd97, "Refrain 2")
  , (0xbf444876, "Resolution Riff")
  , (0xfe3c27ca, "Rhoad to 28 Part 1")
  , (0xd5117409, "Rhoad to 28 Part 2")
  , (0xd96b0cef, "Rhythm Section Intro")
  , (0xd7de3f4e, "Riff 1")
  , (0x31fee62c, "Riff 1A")
  , (0x1ad3b5ef, "Riff 1B")
  , (0xfcf36c8d, "Riff 2")
  , (0x234b49c2, "Riff 2A")
  , (0x08661a01, "Riff 2B")
  , (0x117d2b40, "Riff 2C")
  , (0xba395dde, "Riff 311 A")
  , (0x91140e1d, "Riff 311 B")
  , (0x880f3f5c, "Riff 311 C")
  , (0xf33b81e9, "Riff de Calor")
  , (0xcdc3d20f, "Riff of Truth 1")
  , (0xe6ee81cc, "Riff of Truth 2")
  , (0x2deb51c7, "Riff Riff Riff")
  , (0x22fe9c2b, "Scream Your Bridge Out")
  , (0x6f512368, "SDMF Riff")
  , (0x5daea188, "Send the Poor 1")
  , (0x7683f24b, "Send the Poor 2")
  , (0xa5ce57ad, "Set?")
  , (0xb53fac96, "Shake it, don't break it")
  , (0x75781035, "Shred or Alive")
  , (0x829c290c, "Six-String Opera")
  , (0x242831b7, "Slippery When Wet")
  , (0xf7aa4e66, "Solo 1")
  , (0x4ba12958, "Solo 1A")
  , (0x608c7a9b, "Solo 1B")
  , (0xdcbc981d, "Solo 2 A")
  , (0xf791cbde, "Solo 2 B")
  , (0xdc871da5, "Solo 2")
  , (0x591486b6, "Solo 2A")
  , (0x7239d575, "Solo 2B")
  , (0x6b22e434, "Solo 2C")
  , (0x246372f3, "Solo 2D")
  , (0x3d7843b2, "Solo 2E")
  , (0xc59c2ce4, "Solo 3")
  , (0xe1a8e1d3, "Solo 3A")
  , (0xca85b210, "Solo 3B")
  , (0xd39e8351, "Solo 3C")
  , (0x9cdf1596, "Solo 3D")
  , (0x85c424d7, "Solo 3E")
  , (0x8addba23, "Solo 4")
  , (0x7c7fd96a, "Solo 4A")
  , (0x57528aa9, "Solo 4B")
  , (0x93c68b62, "Solo 5")
  , (0xb8ebd8a1, "Solo 6")
  , (0xa1f0e9e0, "Solo 7")
  , (0x2668f52f, "Solo 8")
  , (0x3f73c46e, "Solo 9")
  , (0xd8953790, "Solo A")
  , (0xf3b86453, "Solo B")
  , (0x7e107ebb, "Solo Break 1")
  , (0x553d2d78, "Solo Break 2")
  , (0xeaa35512, "Solo C")
  , (0xa5e2c3d5, "Solo D")
  , (0xbcf9f294, "Solo E")
  , (0x97d4a157, "Solo F")
  , (0xbc86b82a, "Solo from the Rooftops")
  , (0x35e2bb24, "Solo Out")
  , (0x595faa03, "Solo Start!")
  , (0xdc718948, "Solo Your Heart Out A")
  , (0xf75cda8b, "Solo Your Heart Out B")
  , (0xee47ebca, "Solo Your Heart Out C")
  , (0x2b4356ef, "Solo")
  , (0x3a9515ce, "SPAM!")
  , (0x8c52e974, "Spider-riff 1")
  , (0xa77fbab7, "Spider-riff 2")
  , (0xf621b4f9, "Start the Song Over and Do It Again")
  , (0xa065f689, "Streets on Fire")
  , (0x2d0a642e, "Sunday Drive 2")
  , (0xec87353a, "Sunday Drive")
  , (0x7c439dfe, "Synth Intro")
  , (0xc5a61e08, "Talk Box Intro")
  , (0x97267a4d, "Taptastic Solo Intro")
  , (0x2be53d3f, "Tasty Keyboard Solo")
  , (0x7c2a0db5, "Teacher's Coming!")
  , (0x222d9fcc, "Temperature's Rising")
  , (0xe2d84cd5, "That Shimmering Bridge")
  , (0xec87f469, "The Assassins Riff 1")
  , (0xc7aaa7aa, "The Assassins Riff 2")
  , (0xbf5a5169, "The Chug-a-lug of Truth")
  , (0x0fdbb2fc, "The Depths 1")
  , (0x24f6e13f, "The Depths 2")
  , (0x26175a12, "The Overkill Outro")
  , (0x01654e23, "The Solo Begins")
  , (0x73ab6a6d, "The Song Ends")
  , (0x596d0c8d, "Too Far from Chorus")
  , (0x796af771, "Toy Bridge")
  , (0xfede44a6, "Truck Drivin' Man A")
  , (0xd5f31765, "Truck Drivin' Man B")
  , (0x11b9e62b, "Turnpike")
  , (0x0006155e, "Twinkles")
  , (0x44e9bb0e, "Undertow Riff")
  , (0xaf49e8b8, "Verse 1")
  , (0x6baf9ff7, "Verse 1A")
  , (0x4082cc34, "Verse 1B")
  , (0x05fc6aff, "Verse 2 ")
  , (0xc6d83cc7, "Verse 2 Intro")
  , (0x8464bb7b, "Verse 2")
  , (0x791a3019, "Verse 2A")
  , (0x523763da, "Verse 2B")
  , (0x4b2c529b, "Verse 2C")
  , (0xe22fc835, "Verse 3 to Outro")
  , (0x9d7f8a3a, "Verse 3")
  , (0xc1a6577c, "Verse 3A")
  , (0xea8b04bf, "Verse 3B")
  , (0xd23e1cfd, "Verse 4")
  , (0xcb252dbc, "Verse 5")
  , (0x22fc3576, "Verse Solo")
  , (0x8bc5f5e9, "Vocal Break 2")
  , (0x18e8cc6e, "Vocal Break")
  , (0x62439da6, "Vocal Solo")
  , (0xc52e698b, "Wings Breakdizzle A")
  , (0xee033a48, "Wings Breakdizzle B")
  , (0x88e38ef6, "Zakk Goes Wylde")
  ]
