-- Extracting and converting parts between different gameplay modes
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
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
import           Data.Maybe                       (fromMaybe, listToMaybe)
import qualified Data.Text                        as T
import           Onyx.AutoChart                   (autoChart)
import           Onyx.Drums.OneFoot               (phaseShiftKicks, rockBand1x,
                                                   rockBand2x)
import           Onyx.Guitar
import qualified Onyx.MIDI.Common                 as RB
import           Onyx.MIDI.Read                   (mapTrack)
import qualified Onyx.MIDI.Track.Drums            as D
import qualified Onyx.MIDI.Track.Drums.Full       as FD
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.MIDI.Track.ProGuitar        (getStringIndex,
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
  , notes    :: Map.Map RB.Difficulty (RTB.T U.Beats ((Maybe Five.Color, RB.StrumHOPOTap), Maybe U.Beats))
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
  -- <|> proGuitarToFiveFret p
  -- <|> proKeysToFiveFret p
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

proGuitarToFiveFret :: Part f -> Maybe BuildFive
proGuitarToFiveFret part = flip fmap part.proGuitar $ \ppg _ftype input -> let
  in FiveResult
    { settings = (def :: PartGRYBO)
      { difficulty = ppg.difficulty
      }
    , notes = let
      -- TODO extend notes out during handshape sections
      -- TODO compute rs modifiers in advance, use them to
      -- * assign strum/hopo/tap
      -- * use opens for fret hand mutes
      -- * join linked notes together if same fret, or mark link child as hopo if different fret
      -- * maybe split bent notes into multiple
      chosenTrack = fromMaybe RTB.empty $ listToMaybe $ filter (not . RTB.null)
        [ rsNotes $ F.onyxPartRSGuitar input.part
        , rsNotes $ F.onyxPartRSBass input.part
        -- TODO support RB protar tracks
        ]
      strings = tuningPitches ppg.tuning
      toPitch (fret, str) = (strings !! getStringIndex 6 str) + fret
      chorded
        = RTB.toAbsoluteEventList 0
        $ guitarify'
        $ fmap (\(fret, str, len) -> ((fret, str), guard (len >= standardBlipThreshold) >> Just len))
        $ RB.joinEdgesSimple chosenTrack
      autoResult = autoChart 5 $ do
        (bts, (notes, _len)) <- ATB.toPairList chorded
        pitch <- simplifyChord $ nubOrd $ map toPitch notes
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
            fret <- maybe [Nothing] (map (Just . toEnum)) $ Map.lookup posn autoMap
            return ((fret, RB.Strum), len)
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
            return ((fret, RB.Tap), len)
          in (posn, notes)
          )
        $ ATB.toPairList
        $ chorded
    , other = mempty -- TODO overdrive, solos, etc.
    , source = "converted Pro Keys chart to five-fret"
    }
