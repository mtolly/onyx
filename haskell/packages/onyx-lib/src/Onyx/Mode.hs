-- Extracting and converting parts between different gameplay modes
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE StrictData            #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Mode where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (guard)
import           Data.Bifunctor                   (first)
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (find)
import           Data.Functor                     (void)
import           Data.List.Extra                  (nubOrd, sort)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust,
                                                   listToMaybe)
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.AutoChart                   (autoChart)
import           Onyx.Drums.OneFoot               (phaseShiftKicks, rockBand1x,
                                                   rockBand2x)
import           Onyx.Guitar
import           Onyx.MIDI.Common                 (StrumHOPOTap (..),
                                                   pattern RNil, pattern Wait)
import qualified Onyx.MIDI.Common                 as RB
import           Onyx.MIDI.Read                   (mapTrack)
import qualified Onyx.MIDI.Track.Drums            as D
import qualified Onyx.MIDI.Track.Drums.Full       as FD
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.MIDI.Track.Mania
import           Onyx.MIDI.Track.ProGuitar        (GtrFret, GtrString,
                                                   getStringIndex,
                                                   tuningPitches)
import           Onyx.MIDI.Track.ProKeys
import           Onyx.MIDI.Track.Rocksmith
import           Onyx.Project
import qualified Sound.MIDI.Util                  as U

data ModeInput = ModeInput
  { tempo  :: U.TempoMap
  , events :: EventsTrack U.Beats
  , part   :: F.OnyxPart U.Beats
  }

------------------------------------------------------------------

data FiveResult = FiveResult
  { settings :: PartGRYBO
  , notes    :: Map.Map RB.Difficulty (RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats))
  , other    :: Five.FiveTrack U.Beats
  , source   :: T.Text
  }

data FiveType
  = FiveTypeGuitar    -- prefer no extended sustains + no open notes
  | FiveTypeKeys      -- prefer    extended sustains + no open notes
  | FiveTypeGuitarExt -- prefex    extended sustains +    open notes
  deriving (Eq, Show)

type BuildFive = FiveType -> ModeInput -> FiveResult

nativeFiveFret :: Part f -> Maybe BuildFive
nativeFiveFret part = flip fmap part.grybo $ \grybo ftype input -> let
  gtr  = (F.onyxPartGuitar    input.part, HOPOsRBGuitar)
  keys = (F.onyxPartKeys      input.part, HOPOsRBKeys  )
  ext  = (F.onyxPartGuitarExt input.part, HOPOsRBGuitar)
  trks = case ftype of
    FiveTypeGuitar    -> [gtr, ext, keys]
    FiveTypeKeys      -> [keys, ext, gtr] -- prefer ext due to sustains? or gtr due to no opens? dunno
    FiveTypeGuitarExt -> [ext, gtr, keys]
  -- TODO maybe fill in lower difficulties from secondary tracks
  (trk, algo) = fromMaybe (mempty, HOPOsRBGuitar) $ find (not . Five.nullFive . fst) trks
  in FiveResult
    { settings = grybo
    , notes    = flip fmap (Five.fiveDifficulties trk) $ \diff ->
      applyForces (getForces5 diff)
        $ strumHOPOTap algo (fromIntegral grybo.hopoThreshold / 480)
        $ computeFiveFretNotes diff
    , other    = trk
    , source = "five-fret chart"
    }

anyFiveFret :: Part f -> Maybe BuildFive
anyFiveFret p
  = nativeFiveFret p
  <|> proGuitarToFiveFret p
  <|> proKeysToFiveFret p
  <|> maniaToFiveFret p
  <|> fmap convertDrumsToFive (nativeDrums p)

convertDrumsToFive :: BuildDrums -> BuildFive
convertDrumsToFive bd _ftype input = let
  drumResult = bd DrumTargetRB2x input
  in FiveResult
    { settings = (def :: PartGRYBO)
      { difficulty = drumResult.settings.difficulty
      }
    , notes = flip fmap drumResult.notes $ \drumGems ->
      strumHOPOTap HOPOsRBGuitar (170/480) $ flip fmap drumGems $ \(gem, _velocity) -> let
        color = case gem of
          D.Kick           -> Five.Green
          D.Red            -> Five.Red
          D.Pro D.Yellow _ -> Five.Yellow
          D.Pro D.Blue   _ -> Five.Blue
          D.Pro D.Green  _ -> Five.Orange
          D.Orange         -> Five.Orange -- won't happen because we called buildDrums with RB3 target
        in (Just color, Nothing)
    , other = Five.FiveTrack
      { Five.fiveDifficulties = Map.empty
      , Five.fiveMood         = D.drumMood drumResult.other
      , Five.fiveHandMap      = RTB.empty
      , Five.fiveStrumMap     = RTB.empty
      , Five.fiveFretPosition = RTB.empty
      , Five.fiveTremolo      = RTB.empty -- TODO include these sometimes?
      , Five.fiveTrill        = RTB.empty -- TODO include these sometimes?
      , Five.fiveOverdrive    = D.drumOverdrive drumResult.other
      , Five.fiveBRE          = let
        -- only copy over a fill that is actually a BRE
        coda = fmap (fst . fst) $ RTB.viewL $ eventsCoda input.events
        in case coda of
          Nothing -> RTB.empty
          Just c  -> RTB.delay c $ U.trackDrop c $ D.drumActivation drumResult.other
      , Five.fiveSolo         = D.drumSolo drumResult.other
      , Five.fivePlayer1      = D.drumPlayer1 drumResult.other
      , Five.fivePlayer2      = D.drumPlayer2 drumResult.other
      }
    , source = "converted drum chart to five-fret"
    }

------------------------------------------------------------------

data DrumResult = DrumResult
  { settings   :: PartDrums ()
  , notes      :: Map.Map RB.Difficulty (RTB.T U.Beats (D.Gem D.ProType, D.DrumVelocity))
  , other      :: D.DrumTrack U.Beats -- includes 2x kicks when CH/GH format is requested
  , animations :: RTB.T U.Beats D.Animation
  , source     :: T.Text
  }

data DrumTarget
  = DrumTargetRB1x -- pro, 1x
  | DrumTargetRB2x -- pro, 2x
  | DrumTargetCH -- pro, x+
  | DrumTargetGH -- 5-lane, x+

type BuildDrums = DrumTarget -> ModeInput -> DrumResult

nativeDrums :: Part f -> Maybe BuildDrums
nativeDrums part = flip fmap part.drums $ \pd dtarget input -> let

  src1x   =                             F.onyxPartDrums       input.part
  src2x   =                             F.onyxPartDrums2x     input.part
  srcReal = D.psRealToPro             $ F.onyxPartRealDrumsPS input.part
  srcFull = FD.convertFullDrums False $ F.onyxPartFullDrums   input.part
  srcsRB = case dtarget of
    DrumTargetRB1x -> [src1x, src2x]
    _              -> [src2x, src1x]
  srcList = case pd.mode of
    DrumsReal -> srcReal : srcsRB
    DrumsFull -> srcFull : srcsRB
    _         -> srcsRB
  src = fromMaybe mempty $ find (not . D.nullDrums) srcList

  stepAddKicks = case pd.kicks of
    Kicks2x -> mapTrack (U.unapplyTempoTrack input.tempo) . phaseShiftKicks 0.18 0.11 . mapTrack (U.applyTempoTrack input.tempo)
    _       -> id

  isRBTarget = case dtarget of
    DrumTargetRB1x -> True
    DrumTargetRB2x -> True
    _              -> False

  stepRBKicks = case dtarget of
    DrumTargetRB1x -> rockBand1x
    DrumTargetRB2x -> rockBand2x
    _              -> id

  drumEachDiff f dt = dt { D.drumDifficulties = fmap f $ D.drumDifficulties dt }
  step5to4 = if pd.mode == Drums5 && isRBTarget
    then drumEachDiff $ \dd -> dd
      { D.drumGems = D.fiveToFour
        (case pd.fallback of
          FallbackBlue  -> D.Blue
          FallbackGreen -> D.Green
        )
        (D.drumGems dd)
      }
    else id

  isBasicSource = case pd.mode of
    Drums4 -> True
    Drums5 -> True
    _      -> False

  src' = step5to4 $ stepRBKicks $ stepAddKicks src

  modifyProType ptype = if isBasicSource
    then if isRBTarget then D.Tom else D.Cymbal
    else ptype

  -- TODO pro to 5 conversion (for GH target)
  -- Move logic from Neversoft.Export to here

  in DrumResult
    { settings = void pd
    , notes = Map.fromList $ do
      diff <- [minBound .. maxBound]
      let gems = first (fmap modifyProType) <$> D.computePro (Just diff) src'
      guard $ not $ RTB.null gems
      return (diff, gems)
    , other = src'
    , animations = buildDrumAnimation pd input.tempo input.part
    , source = "drum chart"
    }

anyDrums :: Part f -> Maybe BuildDrums
anyDrums = nativeDrums

buildDrumAnimation
  :: PartDrums f
  -> U.TempoMap
  -> F.OnyxPart U.Beats
  -> RTB.T U.Beats D.Animation
buildDrumAnimation pd tmap opart = let
  rbTracks = map ($ opart) [F.onyxPartRealDrumsPS, F.onyxPartDrums2x, F.onyxPartDrums]
  inRealTime f = U.unapplyTempoTrack tmap . f . U.applyTempoTrack tmap
  closeTime = 0.25 :: U.Seconds
  in case filter (not . RTB.null) $ map D.drumAnimation rbTracks of
    anims : _ -> anims
    []        -> case pd.mode of
      DrumsFull -> inRealTime (FD.autoFDAnimation closeTime)
        $ FD.getDifficulty (Just RB.Expert) $ F.onyxPartFullDrums opart
      -- TODO this could be made better for modes other than pro
      _ -> inRealTime (D.autoDrumAnimation closeTime)
        $ fmap fst $ D.computePro (Just RB.Expert)
        $ case filter (not . D.nullDrums) rbTracks of
          trk : _ -> trk
          []      -> mempty

------------------------------------------------------------------

-- TODO transition to nativeDrums
buildDrumTarget
  :: DrumTarget
  -> PartDrums f
  -> U.Beats
  -> U.TempoMap
  -> F.OnyxPart U.Beats
  -> D.DrumTrack U.Beats
buildDrumTarget tgt pd timingEnd tmap opart = let

  src1x   =                             F.onyxPartDrums       opart
  src2x   =                             F.onyxPartDrums2x     opart
  srcReal = D.psRealToPro             $ F.onyxPartRealDrumsPS opart
  srcFull = FD.convertFullDrums False $ F.onyxPartFullDrums   opart
  srcsRB = case tgt of
    DrumTargetRB1x -> [src1x, src2x]
    _              -> [src2x, src1x]
  srcList = case pd.mode of
    DrumsReal -> srcReal : srcsRB
    DrumsFull -> srcFull : srcsRB
    _         -> srcsRB
  src = fromMaybe mempty $ find (not . D.nullDrums) srcList

  stepAddKicks = case pd.kicks of
    Kicks2x -> mapTrack (U.unapplyTempoTrack tmap) . phaseShiftKicks 0.18 0.11 . mapTrack (U.applyTempoTrack tmap)
    _       -> id

  isRBTarget = case tgt of
    DrumTargetRB1x -> True
    DrumTargetRB2x -> True
    _              -> False

  stepRBKicks = case tgt of
    DrumTargetRB1x -> rockBand1x
    DrumTargetRB2x -> rockBand2x
    _              -> id

  drumEachDiff f dt = dt { D.drumDifficulties = fmap f $ D.drumDifficulties dt }
  step5to4 = if pd.mode == Drums5 && isRBTarget
    then drumEachDiff $ \dd -> dd
      { D.drumGems = D.fiveToFour
        (case pd.fallback of
          FallbackBlue  -> D.Blue
          FallbackGreen -> D.Green
        )
        (D.drumGems dd)
      }
    else id

  isBasicSource = case pd.mode of
    Drums4 -> True
    Drums5 -> True
    _      -> False

  noToms dt = dt { D.drumToms = RTB.empty }
  allToms dt = dt
    { D.drumToms = RTB.fromPairList
      [ (0        , (D.Yellow, D.Tom   ))
      , (0        , (D.Blue  , D.Tom   ))
      , (0        , (D.Green , D.Tom   ))
      , (timingEnd, (D.Yellow, D.Cymbal))
      , (0        , (D.Blue  , D.Cymbal))
      , (0        , (D.Green , D.Cymbal))
      ]
    }
  stepToms = if isBasicSource
    then if isRBTarget
      then allToms
      else noToms
    else id

  -- TODO pro to 5 conversion (for GH target)
  -- Move logic from Neversoft.Export to here

  in stepToms $ step5to4 $ stepRBKicks $ stepAddKicks src

------------------------------------------------------------------

simplifyChord :: [Int] -> [Int]
simplifyChord pitches = case pitches of
  [_]    -> pitches
  [_, _] -> pitches
  _      -> let
    sorted = sort pitches
    keys = nubOrd $ map (`rem` 12) pitches
    in if length keys <= 2
      then take 2 sorted -- power chords or octaves become max 2-note
      else take 3 sorted -- otherwise max 3-note
      -- maybe have a smarter way of thinning? (preserve unique keys)

-- Turn RS linked sustains into RB style (link child either merged into first
-- note if same fret, or marked as HOPO if different fret)
applyLinks :: (NNC.C t) => RTB.T t (RB.Edge (GtrFret, [RSModifier]) GtrString) -> RTB.T t (RB.Edge (GtrFret, [RSModifier]) GtrString)
applyLinks = \case
  Wait t edge rest -> case edge of
    RB.EdgeOff _ -> Wait t edge $ applyLinks rest
    RB.EdgeOn (fret, mods) str -> if elem ModLink mods
      then let
        findThisOff = \case
          RB.EdgeOff str' -> guard (str == str') >> Just ()
          _               -> Nothing
        findNextOn = \case
          RB.EdgeOn fretMods str' -> guard (str == str') >> Just fretMods
          _                       -> Nothing
        in case U.extractFirst findThisOff rest of
          Nothing -> Wait t edge $ applyLinks rest
          Just ((thisLength, ()), rest') -> case U.extractFirst findNextOn rest' of
            Nothing -> Wait t edge $ applyLinks rest
            Just ((beforeNextOn, (nextFret, nextMods)), rest'') -> if fret == nextFret
              -- if linking to same fret, join the two notes together, and move slide/link mods from 2nd note to 1st
              then let
                mods' = filter (/= ModLink) mods <> let
                  moveBack = \case
                    ModSlide        _ -> True
                    ModSlideUnpitch _ -> True
                    ModLink           -> True
                    _                 -> False
                  in filter moveBack nextMods
                in applyLinks $ Wait t (RB.EdgeOn (fret, mods') str) rest''
              -- if linking to a different fret, don't remove any notes, but mark 2nd note as "HOPO"
              else Wait t edge
                $ applyLinks
                $ RTB.insert thisLength (RB.EdgeOff str)
                $ RTB.insert beforeNextOn (RB.EdgeOn (nextFret, ModHammerOn : nextMods) str)
                $ rest''
      else Wait t edge $ applyLinks rest
  RNil -> RNil

-- For more GHRB-like style, turns some strums into hopos, and some hopos into taps
adjustRocksmithHST
  :: U.TempoMap
  -> RTB.T U.Beats [((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)]
  -> RTB.T U.Beats [((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)]
adjustRocksmithHST tempos = let
  timeLong, timeShort :: U.Seconds
  timeLong  = 0.5
  timeShort = 0.13 -- may want to adjust this, could be too high for some situations
  go = \case
    -- tap, hopo: hopo should become tap
    Wait t1 chord1@(((_, Tap), _) : _) (Wait t2 chord2@(((_, HOPO), _) : _) rest)
      ->  Wait t1 chord1
        $ go
        $ Wait t2 [ ((color, Tap), len) | ((color, _), len) <- chord2 ] rest
    -- long gap, hopo, tap: hopo should become tap
    -- TODO support more than 1 hopo before tap
    Wait t1 chord1@(((_, HOPO), _) : _) rest@(Wait _ (((_, Tap), _) : _) _)
      | t1 >= timeLong
      ->  Wait t1 [ ((color, Tap), len) | ((color, _), len) <- chord1 ]
        $ go rest
    -- strum/hopo, short gap, strum: second note should become hopo under certain conditions
    Wait t1 note1@[((color1, sht1), _)] (Wait t2 [((color2, Strum), len2)] rest)
      |    t2 <= timeShort -- short gap between notes
        && color1 /= color2 -- different single gems
        && sht1 /= Tap -- first note is strum or hopo
        && isJust color2 -- second note isn't an open note (was fret-hand-mute in RS)
      ->  Wait t1 note1
        $ go
        $ Wait t2 [((color2, HOPO), len2)] rest
    -- otherwise nothing to change
    Wait t x rest -> Wait t x $ go rest
    RNil -> RNil
  in U.unapplyTempoTrack tempos . go . U.applyTempoTrack tempos

proGuitarToFiveFret :: Part f -> Maybe BuildFive
proGuitarToFiveFret part = flip fmap part.proGuitar $ \ppg _ftype input -> let
  in FiveResult
    { settings = (def :: PartGRYBO)
      { difficulty = ppg.difficulty
      }
    , notes = let
      -- TODO
      -- * extend notes out during handshape sections
      -- * turn tremolo sustains into stream of strummed notes on regular rhythm
      -- * maybe split bent notes into multiple
      chosenTrack = fromMaybe RTB.empty $ listToMaybe $ filter (not . RTB.null)
        [ applyLinks $ getNotesWithModifiers $ F.onyxPartRSGuitar input.part
        , applyLinks $ getNotesWithModifiers $ F.onyxPartRSBass input.part
        -- TODO support RB protar tracks
        ]
      strings = tuningPitches ppg.tuning
      toPitch (fret, str) = (strings !! getStringIndex 6 str) + fret
      chorded
        = RTB.toAbsoluteEventList 0
        $ guitarify'
        $ fmap (\((fret, mods), str, len) -> (((fret, str), mods), guard (len >= standardBlipThreshold) >> Just len))
        $ RB.joinEdgesSimple chosenTrack
      autoResult = autoChart 5 $ do
        (bts, (notes, _len)) <- ATB.toPairList chorded
        pitch <- simplifyChord $ nubOrd
          -- Don't give fret-hand-mute notes to the autochart,
          -- then below they will automatically become open notes
          [ toPitch fretStr | (fretStr, mods) <- notes, notElem ModMute mods ]
        return (realToFrac bts, pitch)
      autoMap = foldr (Map.unionWith (<>)) Map.empty $ map
        (\(pos, fret) -> Map.singleton (realToFrac pos) [fret])
        autoResult
      in Map.singleton RB.Expert
        $ RTB.flatten
        $ adjustRocksmithHST input.tempo
        $ RTB.fromAbsoluteEventList
        $ ATB.fromPairList
        $ map (\(posn, (chord, len)) -> let
          allMods = chord >>= snd
          hst = if elem ModHammerOn allMods || elem ModPullOff allMods
            then HOPO
            else if elem ModTap allMods
              then Tap
              else Strum
          notes = do
            fret <- maybe [Nothing] (map (Just . toEnum)) $ Map.lookup posn autoMap
            return ((fret, hst), len)
          in (posn, notes)
          )
        $ ATB.toPairList
        $ chorded
    , other = mempty -- TODO when RB pro tracks are supported, add overdrive, solos, etc.
    , source = "converted Rocksmith chart to five-fret"
    }

proKeysToFiveFret :: Part f -> Maybe BuildFive
proKeysToFiveFret part = flip fmap part.proKeys $ \ppk _ftype input -> let
  in FiveResult
    { settings = (def :: PartGRYBO)
      { difficulty = ppk.difficulty
      }
    , notes = let
      chorded
        = RTB.toAbsoluteEventList 0
        $ guitarify'
        $ fmap (\((), key, len) -> (fromEnum key, guard (len >= standardBlipThreshold) >> Just len))
        $ RB.joinEdgesSimple $ pkNotes $ F.onyxPartRealKeysX input.part
      autoResult = autoChart 5 $ do
        (bts, (notes, _len)) <- ATB.toPairList chorded
        pitch <- simplifyChord notes
        return (realToFrac bts, pitch)
      autoMap = foldr (Map.unionWith (<>)) Map.empty $ map
        (\(pos, fret) -> Map.singleton (realToFrac pos) [fret])
        autoResult
      in Map.singleton RB.Expert
        $ RTB.flatten
        $ RTB.fromAbsoluteEventList
        $ ATB.fromPairList
        $ map (\(posn, (_, len)) -> let
          notes = do
            fret <- maybe [Just Five.Green] (map (Just . toEnum)) $ Map.lookup posn autoMap
            return ((fret, Tap), len)
          in (posn, notes)
          )
        $ ATB.toPairList
        $ chorded
    , other = mempty -- TODO overdrive, solos, etc.
    , source = "converted Pro Keys chart to five-fret"
    }

maniaToFiveFret :: Part f -> Maybe BuildFive
maniaToFiveFret part = flip fmap part.mania $ \pm _ftype input -> let
  in FiveResult
    { settings = def :: PartGRYBO
    , notes = Map.singleton RB.Expert $ if pm.keys <= 5
      then fmap (\(k, len) -> ((Just $ toEnum k, Tap), len))
        $ RB.edgeBlips_ RB.minSustainLengthRB
        $ maniaNotes $ F.onyxPartMania input.part
        -- TODO maybe offset if less than 4 keys? like RYB for 3-key
      else let
        chorded
          = RTB.toAbsoluteEventList 0
          $ guitarify'
          $ fmap (\((), key, len) -> (key, guard (len >= standardBlipThreshold) >> Just len))
          $ RB.joinEdgesSimple $ maniaNotes $ F.onyxPartMania input.part
        autoResult = autoChart 5 $ do
          (bts, (notes, _len)) <- ATB.toPairList chorded
          pitch <- simplifyChord notes
          return (realToFrac bts, pitch)
        autoMap = foldr (Map.unionWith (<>)) Map.empty $ map
          (\(pos, fret) -> Map.singleton (realToFrac pos) [fret])
          autoResult
        in RTB.flatten
          $ RTB.fromAbsoluteEventList
          $ ATB.fromPairList
          $ map (\(posn, (_, len)) -> let
            notes = do
              fret <- maybe [Just Five.Green] (map (Just . toEnum)) $ Map.lookup posn autoMap
              return ((fret, Tap), len)
            in (posn, notes)
            )
          $ ATB.toPairList
          $ chorded
    , other = mempty
    , source = "converted Mania chart to five-fret"
    }
