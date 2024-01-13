{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Neversoft.GH4.MidQB where

import           Control.Monad                    (forM)
import           Data.Bits                        (testBit, (.&.))
import qualified Data.ByteString                  as B
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (sort)
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           Data.Word
import           Onyx.Guitar                      (emit5')
import           Onyx.MIDI.Common                 (Difficulty (..),
                                                   StrumHOPOTap (..))
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.Neversoft.CRC               (qbKeyCRC)
import           Onyx.Neversoft.GH3.MidQB         (findSection, groupBy3,
                                                   listOfPairs, listOfTriples,
                                                   readGH3TempoMap, toSeconds)
import           Onyx.Neversoft.QB
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

gh4ToMidi :: HM.HashMap Word32 T.Text -> GH4MidQB -> F.Song (F.FixedFile U.Beats)
gh4ToMidi bank gh4 = let
  tempos = readGH3TempoMap (gh4TimeSignatures gh4) (gh4FretBars gh4)
  toBeats :: Word32 -> U.Beats
  toBeats = U.unapplyTempoMap tempos . toSeconds
  fromPairs ps = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort ps
  fixed = mempty
    { F.fixedPartGuitar = getGuitarBass $ gh4Guitar gh4
    , F.fixedPartBass = getGuitarBass $ gh4Rhythm gh4
    , F.fixedPartDrums = mempty
    , F.fixedPartVocals = mempty
    }
  -- is this sustain cutoff still right? no trim this time
  sustainThreshold :: Word32
  sustainThreshold = floor $ (U.applyTempoMap tempos 1 / 2) * 1000
  getGuitarBassTrack trk = emit5' $ fromPairs $ do
    (time, bits) <- gh4Notes trk
    let pos = toBeats time
        len = bits .&. 0xFFFF -- think this is right but should confirm
        lenTrimmed = if len > sustainThreshold && not isOpen -- open notes don't sustain in game
          then len
          else 0
        lenBeats = case toBeats (time + lenTrimmed) - pos of
          0 -> Nothing
          l -> Just l
        isOpen = bits `testBit` 21
        sht = if any (\(tapPos, tapLen, _) -> tapPos <= time && time < tapPos + tapLen) $ gh4Tapping trk
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
      [ (Easy  , getGuitarBassTrack $ gh4Easy   part)
      , (Medium, getGuitarBassTrack $ gh4Medium part)
      , (Hard  , getGuitarBassTrack $ gh4Hard   part)
      , (Expert, getGuitarBassTrack $ gh4Expert part)
      ]
    , Five.fiveOverdrive = makeSpan $ fmap (\(time, len, _) -> (time, len)) $ gh4StarPower $ gh4Expert part
    , Five.fivePlayer1 = makeSpan $ gh4FaceOffP1 part
    , Five.fivePlayer2 = makeSpan $ gh4FaceOffP2 part
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
