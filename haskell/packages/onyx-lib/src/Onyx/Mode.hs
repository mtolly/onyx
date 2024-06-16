-- Extracting and converting parts between different gameplay modes
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Mode where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (forM, guard)
import           Control.Monad.Random.Class       (uniform, uniformMay)
import           Control.Monad.Random.Strict      (evalRand, mkStdGen)
import           Data.Bifunctor                   (first, second)
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (find, toList)
import           Data.Functor                     (void)
import           Data.Hashable                    (hash)
import           Data.List.Extra                  (nubOrd, sort, unsnoc)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   listToMaybe)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Onyx.AutoChart                   (autoChart)
import           Onyx.Drums.OneFoot               (phaseShiftKicks, rockBand1x,
                                                   rockBand2x)
import           Onyx.Guitar
import           Onyx.MIDI.Common                 (Difficulty (..), Key (..),
                                                   StrumHOPOTap (..),
                                                   pattern RNil, pattern Wait)
import qualified Onyx.MIDI.Common                 as RB
import           Onyx.MIDI.Read                   (mapTrack)
import qualified Onyx.MIDI.Track.Drums            as D
import qualified Onyx.MIDI.Track.Drums.Elite      as ED
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.MIDI.Track.Mania
import           Onyx.MIDI.Track.ProGuitar
import           Onyx.MIDI.Track.ProKeys
import           Onyx.MIDI.Track.Rocksmith
import           Onyx.PhaseShift.Dance
import           Onyx.Project
import qualified Sound.MIDI.Util                  as U

data ModeInput = ModeInput
  { tempo  :: U.TempoMap
  , events :: EventsTrack U.Beats
  , part   :: F.OnyxPart U.Beats
  }

------------------------------------------------------------------

data FiveResult = FiveResult
  { settings  :: PartGRYBO
  , notes     :: Map.Map RB.Difficulty (RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats))
  , other     :: Five.FiveTrack U.Beats
  , source    :: T.Text
  , autochart :: Bool
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
    { settings  = grybo
    , notes     = flip fmap (Five.fiveDifficulties trk) $ \diff ->
      applyForces (getForces5 diff)
        $ strumHOPOTap algo (fromIntegral grybo.hopoThreshold / 480)
        $ computeFiveFretNotes diff
    , other     = trk
    , source    = "five-fret chart"
    , autochart = False
    }

anyFiveFret :: Part f -> Maybe BuildFive
anyFiveFret p
  = nativeFiveFret p
  <|> proGuitarToFiveFret p
  <|> proKeysToFiveFret p
  <|> maniaToFiveFret p
  <|> danceToFiveFret p
  <|> fmap convertDrumsToFive (nativeDrums p)

convertFiveToDrums :: BuildFive -> BuildDrums
convertFiveToDrums bf _dtarget input = let
  fiveResult = bf FiveTypeKeys input
  notes :: [(RB.Difficulty, RTB.T U.Beats (D.Gem D.ProType))]
  notes = do
    (diff, trk) <- Map.toList fiveResult.notes
    let trk' = noOpenNotes fiveResult.settings.detectMutedOpens trk
        eachInstant inst = let
          handGems = inst >>= \((color, _sht), _len) -> case color of
            Five.Green  -> []
            Five.Red    -> [D.Red]
            Five.Yellow -> [D.Pro D.Yellow D.Tom]
            Five.Blue   -> [D.Pro D.Blue   D.Tom]
            Five.Orange -> [D.Pro D.Green  D.Tom]
          kick = [ D.Kick | ((Five.Green, _), _) <- inst ]
          -- keep the min and max of the hand gems
          handGems' = case sort handGems of
            []          -> []
            gem1 : rest -> gem1 : take 1 (reverse rest)
          in kick <> handGems'
    return (diff, RTB.flatten $ fmap eachInstant $ RTB.collectCoincident trk')
  in DrumResult
    { settings = PartDrums
      { difficulty    = fiveResult.settings.difficulty
      , mode          = Drums4
      , kicks         = Kicks1x -- lol
      , fixFreeform   = True
      , kit           = HardRockKit
      , layout        = StandardLayout
      , fallback      = FallbackGreen
      , fileDTXKit    = Nothing
      , trueLayout    = []
      , difficultyDTX = Nothing
      }
    , notes = Map.fromList $ map (second $ fmap (, D.VelocityNormal)) notes
    , other = mempty
      { D.drumSolo = fiveResult.other.fiveSolo
      }
    , hasRBMarks = False
    , animations
      = U.unapplyTempoTrack input.tempo
      $ D.autoDrumAnimation 0.25
      $ U.applyTempoTrack input.tempo
      $ fromMaybe RTB.empty $ lookup Expert notes
      :: RTB.T U.Beats D.Animation
    , source = "converted five-fret chart to drums"
    , autochart = False
    , eliteDrums = Nothing
    }

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
    , autochart = False
    }

------------------------------------------------------------------

data DrumResult = DrumResult
  { settings   :: PartDrums ()
  , notes      :: Map.Map RB.Difficulty (RTB.T U.Beats (D.Gem D.ProType, D.DrumVelocity))
  , other      :: D.DrumTrack U.Beats -- includes 2x kicks when CH/GH format is requested
  , animations :: RTB.T U.Beats D.Animation
  , hasRBMarks :: Bool -- True if `other` includes correct tom markers and mix events
  , source     :: T.Text
  , autochart  :: Bool
  , eliteDrums :: Maybe (ED.EliteDrumTrack U.Beats)
  }

data DrumTarget
  = DrumTargetRB1x -- pro, 1x
  | DrumTargetRB2x -- pro, 2x
  | DrumTargetCH -- pro, x+
  | DrumTargetGH -- 5-lane, x+
  deriving (Eq)

type BuildDrums = DrumTarget -> ModeInput -> DrumResult

nativeDrums :: Part f -> Maybe BuildDrums
nativeDrums part = flip fmap part.drums $ \pd dtarget input -> let

  src1x   =                                          F.onyxPartDrums       input.part
  src2x   =                                          F.onyxPartDrums2x     input.part
  srcReal = D.psRealToPro                          $ F.onyxPartRealDrumsPS input.part
  srcTrue = snd $ ED.convertEliteDrums input.tempo $ F.onyxPartEliteDrums  input.part
  srcsRB = case dtarget of
    DrumTargetRB1x -> [src1x, src2x]
    _              -> [src2x, src1x]
  srcList = case pd.mode of
    DrumsReal -> srcsRB <> [srcReal]
    DrumsTrue -> srcsRB <> [srcTrue]
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

  diffFiveToPro = D.fiveToPro
    (case pd.fallback of
      FallbackBlue  -> D.Blue
      FallbackGreen -> D.Green
    ) . D.drumGems

  isBasicSource = case pd.mode of
    Drums4 -> True
    Drums5 -> True
    _      -> False

  src'
    = (if pd.fixFreeform then F.fixFreeformDrums else id)
    $ stepRBKicks $ stepAddKicks src

  -- for RB3 we want to mark all notes as tom.
  -- however for CH/PS output we want no tom markers as they will handle as non-pro correctly.
  basicToProType = if isRBTarget then D.Tom else D.Cymbal

  -- TODO pro to 5 conversion (for GH target)
  -- Move logic from Neversoft.Export to here

  in DrumResult
    { settings = void pd
    , notes = Map.fromList $ do
      diff <- [minBound .. maxBound]
      let gems = if elem dtarget [DrumTargetCH, DrumTargetGH] && pd.mode == Drums5
            then fmap (first $ fmap $ const D.Cymbal)
              $ D.drumGems $ fromMaybe mempty $ Map.lookup diff $ D.drumDifficulties src'
            else case pd.mode of
              Drums4 -> first (basicToProType <$) <$> D.computePro (Just diff) src'
              Drums5 -> diffFiveToPro $ fromMaybe mempty $ Map.lookup diff $ D.drumDifficulties src'
              _      -> D.computePro (Just diff) src'
      guard $ not $ RTB.null gems
      return (diff, gems)
    , other = src'
    , animations = buildDrumAnimation pd input.tempo input.part
    , hasRBMarks = not isBasicSource
    , source = "drum chart"
    , autochart = False
    , eliteDrums = do
      guard $ pd.mode == DrumsTrue
      Just $ F.onyxPartEliteDrums input.part
    }

anyDrums :: Part f -> Maybe BuildDrums
anyDrums p
  = nativeDrums p
  <|> maniaToDrums p
  <|> danceToDrums p
  <|> fmap convertFiveToDrums (nativeFiveFret p)

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
      DrumsTrue -> inRealTime (ED.eliteDrumsToAnimation closeTime)
        $ ED.getDifficulty (Just Expert) $ F.onyxPartEliteDrums opart
      -- TODO this could be made better for modes other than pro
      _ -> inRealTime (D.autoDrumAnimation closeTime)
        $ fmap fst $ D.computePro (Just Expert)
        $ case filter (not . D.nullDrums) rbTracks of
          trk : _ -> trk
          []      -> mempty

------------------------------------------------------------------

data ProKeysResult = ProKeysResult
  { settings     :: PartProKeys
  , difficulties :: Map.Map RB.Difficulty (ProKeysTrack U.Beats)
  , source       :: T.Text
  , autochart    :: Bool
  }

type BuildProKeys = ModeInput -> ProKeysResult

anyProKeys :: Part f -> Maybe BuildProKeys
anyProKeys p
  = nativeProKeys p
  <|> maniaToProKeys p

nativeProKeys :: Part f -> Maybe BuildProKeys
nativeProKeys part = flip fmap part.proKeys $ \ppk input -> let
  in ProKeysResult
    { settings = ppk
    , difficulties = Map.fromList
      [ (Expert, input.part.onyxPartRealKeysX)
      , (Hard  , input.part.onyxPartRealKeysH)
      , (Medium, input.part.onyxPartRealKeysM)
      , (Easy  , input.part.onyxPartRealKeysE)
      ]
    , source = "pro keys chart"
    , autochart = False
    }

maniaToProKeys :: Part f -> Maybe BuildProKeys
maniaToProKeys part = do
  pm <- part.mania
  guard $ pm.keys > 5 -- don't make pro keys if this chart fits in basic keys
  guard $ pm.keys <= 10 -- for now, only use white keys and don't try to autochart down
  Just $ \input -> let
    (range, keys) = case pm.keys of
      1 -> (RangeF, [                                                    BlueGreen D                                                    ])
      3 -> (RangeF, [                                       BlueGreen C, BlueGreen D, BlueGreen E                                       ])
      5 -> (RangeF, [                          RedYellow B, BlueGreen C, BlueGreen D, BlueGreen E, BlueGreen F                          ])
      7 -> (RangeF, [             RedYellow A, RedYellow B, BlueGreen C, BlueGreen D, BlueGreen E, BlueGreen F, BlueGreen G             ])
      9 -> (RangeF, [RedYellow G, RedYellow A, RedYellow B, BlueGreen C, BlueGreen D, BlueGreen E, BlueGreen F, BlueGreen G, BlueGreen A])
      2 -> (RangeC, [                                                    RedYellow G, RedYellow A                                                    ])
      4 -> (RangeC, [                                       RedYellow F, RedYellow G, RedYellow A, RedYellow B                                       ])
      6 -> (RangeC, [                          RedYellow E, RedYellow F, RedYellow G, RedYellow A, RedYellow B, BlueGreen C                          ])
      8 -> (RangeC, [             RedYellow D, RedYellow E, RedYellow F, RedYellow G, RedYellow A, RedYellow B, BlueGreen C, BlueGreen D             ])
      _ -> (RangeC, [RedYellow C, RedYellow D, RedYellow E, RedYellow F, RedYellow G, RedYellow A, RedYellow B, BlueGreen C, BlueGreen D, BlueGreen E])
    in ProKeysResult
      { settings = PartProKeys
        { difficulty  = Tier 1
        , fixFreeform = False
        }
      , difficulties = Map.singleton Expert mempty
        { pkLanes = RTB.singleton 0 range
        , pkNotes = fmap (fmap (keys !!)) $ maniaChordSnap $ maniaNotes input.part.onyxPartMania
        }
      , source = "converted Mania chart to pro keys"
      , autochart = False
      }

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

-- For more GHRB-like style, turns some strums into hopos, and some hopos into taps
adjustRocksmithHST
  :: U.TempoMap
  -> RTB.T U.Beats [((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)]
  -> RTB.T U.Beats [((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)]
adjustRocksmithHST tempos = let
  timeLong, timeShort :: U.Seconds
  timeLong  = 0.5
  timeShort = 0.1
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

-- Only adding things that are currently used in converting to 5-fret
loadProtarOutput :: ProGuitarTrack U.Beats -> RSRockBandOutput
loadProtarOutput pgt = let
  -- could pass hopo threshold in via ModeInput if we really wanted
  result = guitarifyFull (170/480) $ fromMaybe mempty $ Map.lookup Expert $ pgDifficulties pgt
  in RSRockBandOutput
    { notesWithHandshapes = flip fmap result $ \(sht, notes, sustain) ->
      ( do
        (str, fret, typ) <- notes
        guard $ typ /= ArpeggioForm
        let mods = concat
              [ [ModHammerOn          | sht == HOPO ]
              , [ModTap               | sht == Tap  ]
              , [ModMute              | typ == Muted]
              , [ModSlideUnpitch fret | isSlide     ] -- important to not chart muted slides as opens
              ]
            isSlide = case sustain of
              Just (_len, maybeSlide) -> isJust maybeSlide
              Nothing                 -> False
        return (str, fret, mods)
      , fst <$> sustain
      , Nothing
      )
    , tremoloMarkers = RTB.empty
    }

proGuitarToFiveFret :: Part f -> Maybe BuildFive
proGuitarToFiveFret part = flip fmap part.proGuitar $ \ppg _ftype input -> let
  -- TODO
  -- * maybe split bent notes into multiple
  (rsOutput, maybePG) = fromMaybe (RSRockBandOutput RTB.empty RTB.empty, Nothing) $ listToMaybe $ catMaybes
    [ do
      guard $ not $ RTB.null $ rsNotes $ F.onyxPartRSGuitar input.part
      return (rsToRockBand (TremoloBeats 0.25) input.tempo $ F.onyxPartRSGuitar input.part, Nothing)
    , do
      guard $ not $ RTB.null $ rsNotes $ F.onyxPartRSBass input.part
      return (rsToRockBand (TremoloBeats 0.25) input.tempo $ F.onyxPartRSBass input.part, Nothing)
    , do
      let pg = F.onyxPartRealGuitar22 input.part
      guard $ not $ nullPG pg
      return (loadProtarOutput pg, Just pg)
    , do
      let pg = F.onyxPartRealGuitar input.part
      guard $ not $ nullPG pg
      return (loadProtarOutput pg, Just pg)
    ]
  chorded = RTB.toAbsoluteEventList 0 $ notesWithHandshapes rsOutput
  strings = tuningPitches ppg.tuning
  toPitch str fret = (strings !! getStringIndex 6 str) + fret
  isSlide = \case
    ModSlide{}        -> True
    ModSlideUnpitch{} -> True
    _                 -> False
  autoResult = autoChart 5 $ do
    (bts, (notes, _len, _shape)) <- ATB.toPairList chorded
    pitch <- simplifyChord $ nubOrd $ do
      (str, fret, mods) <- notes
      -- Don't give fret-hand-mute notes to the autochart,
      -- then below they will automatically become open notes
      guard $ notElem ModMute mods || any isSlide mods
      return $ toPitch str fret
    return (realToFrac bts, pitch)
  autoMap = foldr (Map.unionWith (<>)) Map.empty $ map
    (\(pos, fret) -> Map.singleton (realToFrac pos) [fret])
    autoResult
  in FiveResult
    { settings = (def :: PartGRYBO)
      { difficulty = ppg.difficulty
      }
    , notes = Map.singleton Expert
      $ RTB.flatten
      $ adjustRocksmithHST input.tempo
      $ RTB.fromAbsoluteEventList
      $ ATB.fromPairList
      $ map (\(posn, (chord, len, _shape)) -> let
        allMods = chord >>= \(_, _, mods) -> mods
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
    , other = case maybePG of
      Nothing -> mempty
      Just pg -> mempty
        { Five.fiveOverdrive = pgOverdrive   pg
        , Five.fiveSolo      = pgSolo        pg
        , Five.fiveBRE       = snd <$> pgBRE pg
        }
    , source = "converted Rocksmith chart to five-fret"
    , autochart = True
    }

proKeysToFiveFret :: Part f -> Maybe BuildFive
proKeysToFiveFret part = flip fmap part.proKeys $ \ppk _ftype input -> let
  expertPK = F.onyxPartRealKeysX input.part
  in FiveResult
    { settings = (def :: PartGRYBO)
      { difficulty = ppk.difficulty
      }
    , notes = let
      chorded
        = RTB.toAbsoluteEventList 0
        $ guitarify'
        $ fmap (\((), key, len) -> (fromEnum key, guard (len >= standardBlipThreshold) >> Just len))
        $ RB.joinEdgesSimple $ pkNotes expertPK
      autoResult = autoChart 5 $ do
        (bts, (notes, _len)) <- ATB.toPairList chorded
        pitch <- simplifyChord notes
        return (realToFrac bts, pitch)
      autoMap = foldr (Map.unionWith (<>)) Map.empty $ map
        (\(pos, fret) -> Map.singleton (realToFrac pos) [fret])
        autoResult
      in Map.singleton Expert
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
    , other = mempty
      { Five.fiveOverdrive = pkOverdrive expertPK
      , Five.fiveSolo      = pkSolo      expertPK
      , Five.fiveBRE       = pkBRE       expertPK
      , Five.fiveMood      = pkMood      expertPK
      -- could do trill, but may not be valid if autochart was weird
      }
    , source = "converted Pro Keys chart to five-fret"
    , autochart = True
    }

maniaChordSnap :: RTB.T U.Beats (RB.Edge () Int) -> RTB.T U.Beats (RB.Edge () Int)
maniaChordSnap = U.trackJoin . RTB.flatten . go . RTB.collectCoincident where
  window = 11 / 480 :: U.Beats
  go :: RTB.T U.Beats [RB.Edge () Int] -> RTB.T U.Beats [RTB.T U.Beats (RB.Edge () Int)]
  go = \case
    Wait dt instant rest -> if any (\case RB.EdgeOn{} -> True; _ -> False) instant
      then case U.trackSplit window rest of
        (inWindow, outWindow) -> let
          toFlatten = concat $ instant : RTB.getBodies inWindow
          instant' = do
            key <- nubOrd $ map (\case RB.EdgeOn () k -> k; RB.EdgeOff k -> k) toFlatten
            let thisKeyBools = toFlatten >>= \case
                  RB.EdgeOn () k | k == key -> [True]
                  RB.EdgeOff   k | k == key -> [False]
                  _                         -> []
                boolEdge False = RB.EdgeOff   key
                boolEdge True  = RB.EdgeOn () key
            case thisKeyBools of
              []                -> []
              firstBool : bools -> case unsnoc bools of
                Nothing            -> [RTB.singleton 0 $ boolEdge firstBool]
                Just (_, lastBool) -> return $ case (firstBool, lastBool) of
                  (True , True ) -> RTB.singleton 0 $ boolEdge True
                  (False, False) -> RTB.singleton 0 $ boolEdge False
                  (False, True ) -> Wait 0 (boolEdge False) $ Wait 0 (boolEdge True) RNil
                  (True , False) -> Wait 0 (boolEdge True) $ Wait (1/480) (boolEdge False) RNil
          in Wait dt instant' $ go $ RTB.delay window outWindow
      else Wait dt (map (RTB.singleton 0) instant) $ go rest
    RNil -> RNil

maniaToFiveFret :: Part f -> Maybe BuildFive
maniaToFiveFret part = flip fmap part.mania $ \pm _ftype input -> let
  in FiveResult
    { settings = def :: PartGRYBO
    , notes = Map.singleton Expert $ if pm.keys <= 5
      then fmap (\(k, len) -> ((Just $ toEnum k, Tap), len))
        $ RB.edgeBlips_ RB.minSustainLengthRB
        $ maniaChordSnap
        $ maniaNotes $ F.onyxPartMania input.part
        -- TODO maybe offset if less than 4 keys? like RYB for 3-key
      else let
        chorded
          = RTB.toAbsoluteEventList 0
          $ guitarify'
          $ fmap (\((), key, len) -> (key, guard (len >= standardBlipThreshold) >> Just len))
          $ RB.joinEdgesSimple $ maniaChordSnap $ maniaNotes $ F.onyxPartMania input.part
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
    , autochart = pm.keys > 5
    }

danceToFiveFret :: Part f -> Maybe BuildFive
danceToFiveFret part = flip fmap part.dance $ \pd _ftype input -> FiveResult
  { settings = def
    { difficulty = pd.difficulty
    }
  , notes = Map.fromList $ do
    (diff, dd) <- zip [Expert, Hard, Medium, Easy] $ getDanceDifficulties $ F.onyxPartDance input.part
    let five :: RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)
        five
          = RTB.mapMaybe (\case
            ((_    , NoteMine), _  ) -> Nothing
            ((arrow, _       ), len) -> Just
              ((Just $ toEnum $ fromEnum arrow, Tap), len)
            -- this turns rolls into sustains, probably fine but may want to revisit
            )
          $ RB.edgeBlips_ RB.minSustainLengthRB $ danceNotes dd
    return (diff, five)
  , other = mempty
    { Five.fiveOverdrive = danceOverdrive $ F.onyxPartDance input.part
    }
  , source = "converted dance chart to five-fret"
  , autochart = False
  }

danceToDrums :: Part f -> Maybe BuildDrums
danceToDrums part = flip fmap part.dance $ \pd dtarget input -> let
  notes :: [(RB.Difficulty, RTB.T U.Beats (D.Gem D.ProType))]
  notes = do
    (diff, dd) <- zip [Expert, Hard, Medium, Easy] $ getDanceDifficulties $ F.onyxPartDance input.part
    let diffNotes
          = RTB.flatten
          $ fmap (\xs -> case xs of
            -- max 2 notes at a time
            _ : _ : _ : _ -> [minimum xs, maximum xs]
            _             -> xs
            )
          $ RTB.collectCoincident
          $ RTB.mapMaybe (\case
            RB.EdgeOn _ (arrow, typ) | typ /= NoteMine -> Just $ case arrow of
              ArrowL -> D.Red
              ArrowD -> D.Pro D.Yellow D.Tom
              ArrowU -> D.Pro D.Blue   D.Tom
              ArrowR -> D.Pro D.Green  D.Tom
            _                                          -> Nothing
            )
          $ danceNotes dd
    return (diff, diffNotes)
  in DrumResult
    { settings = PartDrums
      { difficulty  = pd.difficulty
      , mode        = case dtarget of
        DrumTargetGH -> Drums5
        _            -> Drums4
      , kicks       = Kicks1x
      , fixFreeform = True
      , kit         = HardRockKit
      , layout      = StandardLayout
      , fallback    = FallbackGreen
      , fileDTXKit  = Nothing
      , trueLayout  = []
      , difficultyDTX = Nothing
      }
    , notes = Map.fromList $ map (second $ fmap (, D.VelocityNormal)) notes
    , other = mempty
    , hasRBMarks = False
    , animations
      = U.unapplyTempoTrack input.tempo
      $ D.autoDrumAnimation 0.25
      $ U.applyTempoTrack input.tempo
      $ fromMaybe RTB.empty $ lookup Expert notes
      :: RTB.T U.Beats D.Animation
    , source = "converted dance chart to drums"
    , autochart = False
    , eliteDrums = Nothing
    }

maniaToDrums :: Part f -> Maybe BuildDrums
maniaToDrums part = flip fmap part.mania $ \pm dtarget input -> let
  inputNotes :: RTB.T U.Beats Int
  inputNotes
    = RTB.flatten
    $ fmap (\xs -> case xs of
      -- max 2 notes at a time
      _ : _ : _ : _ -> [minimum xs, maximum xs]
      _             -> xs
      )
    $ RTB.collectCoincident
    $ RTB.mapMaybe (\case RB.EdgeOn _ n -> Just n; RB.EdgeOff _ -> Nothing)
    $ maniaChordSnap
    $ maniaNotes $ F.onyxPartMania input.part
  notes :: RTB.T U.Beats (D.Gem D.ProType)
  notes = if pm.keys <= laneCount
    then keyToDrum <$> inputNotes
    else RTB.fromAbsoluteEventList $ ATB.fromPairList
      $ map (\(t, n) -> (realToFrac t, keyToDrum n))
      $ autoChart laneCount
      $ map (first realToFrac) $ ATB.toPairList $ RTB.toAbsoluteEventList 0 inputNotes
  laneCount = case dtarget of
    DrumTargetGH -> 5
    _            -> 4
  -- TODO maybe put turntable lane on kick?
  keyToDrum :: Int -> D.Gem D.ProType
  keyToDrum n = case dtarget of
    DrumTargetGH -> [D.Red, D.Pro D.Yellow D.Tom, D.Pro D.Blue D.Tom, D.Orange, D.Pro D.Green D.Tom] !! n
    _            -> [D.Red, D.Pro D.Yellow D.Tom, D.Pro D.Blue D.Tom,           D.Pro D.Green D.Tom] !! n
  mode = case dtarget of
    DrumTargetGH -> Drums5
    _            -> Drums4
  in DrumResult
    { settings = emptyPartDrums mode Kicks1x
    , notes = Map.singleton Expert $ (, D.VelocityNormal) <$> notes
    , other = mempty
    , hasRBMarks = False
    , animations
      = U.unapplyTempoTrack input.tempo
      $ D.autoDrumAnimation 0.25
      $ U.applyTempoTrack input.tempo notes
      :: RTB.T U.Beats D.Animation
    , source = "converted Mania chart to drums"
    , autochart = pm.keys > laneCount
    , eliteDrums = Nothing
    }

drumResultToTrack :: DrumResult -> D.DrumTrack U.Beats
drumResultToTrack dr = if dr.hasRBMarks
  then dr.other
    { D.drumAnimation = dr.animations
    }
  else dr.other
    { D.drumDifficulties = flip fmap dr.notes $ \notes -> D.DrumDifficulty
      -- TODO we still need to apply discobeat! flip gems + include mix events
      { D.drumGems = first void <$> notes
      , D.drumMix = RTB.empty
      , D.drumPSModifiers = RTB.empty
      }
    , D.drumToms = let
      makeColorTomMarkers :: RTB.T U.Beats D.ProType -> RTB.T U.Beats D.ProType
      makeColorTomMarkers
        = RTB.mapMaybe (\case
          (True , D.Tom) -> Just D.Tom
          (False, D.Tom) -> Just D.Cymbal
          _              -> Nothing
          )
        . cleanEdges
        . U.trackJoin
        . fmap (\typ -> RTB.fromPairList [(0, (True, typ)), (1/480, (False, typ))])
      getColorDiff ybg = RTB.mapMaybe $ \case
        (D.Pro ybg' typ, _) | ybg == ybg' -> Just typ
        _                                 -> Nothing
      getColor ybg = let
        allDiffs = foldr RTB.merge RTB.empty $ map (getColorDiff ybg) $ Map.elems dr.notes
        getUniform = \case
          x : xs -> guard (all (== x) xs) >> Just x
          []     -> Nothing -- shouldn't happen (collectCoincident)
        in case mapM getUniform $ RTB.collectCoincident allDiffs of
          Just noConflicts -> noConflicts
          Nothing          -> getColorDiff ybg $ fromMaybe RTB.empty $ Map.lookup Expert dr.notes
      in foldr RTB.merge RTB.empty $ do
        ybg <- [D.Yellow, D.Blue, D.Green]
        return $ fmap (ybg,) $ makeColorTomMarkers $ getColor ybg
    , D.drumAnimation = dr.animations
    }

drumNoteShuffle :: D.DrumTrack U.Beats -> D.DrumTrack U.Beats
drumNoteShuffle dt = let
  randomSeed = hash $ show dt
  newTrack = drumResultToTrack DrumResult
    { settings   = emptyPartDrums DrumsPro Kicks1x
    , notes      = Map.fromList $ do
      let lanes = RTB.merge
            (maybe (0 :: Int, False) (const (0, True)) <$> dt.drumSingleRoll)
            (maybe (1       , False) (const (1, True)) <$> dt.drumDoubleRoll)
          colors = [D.Red, D.Pro D.Yellow (), D.Pro D.Blue (), D.Pro D.Green ()]
      diff <- [Easy .. Expert]
      let gems = D.computePro (Just diff) dt
          laneAnnotated = applyStatus lanes $ RTB.collectCoincident gems
          thisSeed = mkStdGen $ randomSeed + fromEnum diff
          newGems = RTB.flatten $ flip evalRand thisSeed $ RTB.fromPairList <$> do
            forM (RTB.toPairList laneAnnotated) $ \(t, (activeLanes, instant)) -> do
              instant' <- if null activeLanes
                then do
                  let kick = any (\case (D.Kick, _) -> True; _ -> False) instant
                      numHands = length instant - if kick then 1 else 0
                  hand1 <- uniformMay $ guard (numHands >= 1) >> colors
                  hand2 <- uniformMay $ do
                    guard $ numHands >= 2
                    filter ((/= hand1) . Just) colors
                  type1 <- uniform [D.Tom, D.Cymbal]
                  type2 <- uniform [D.Tom, D.Cymbal]
                  return $ catMaybes
                    [ guard kick >> Just (D.Kick, D.VelocityNormal)
                    , (\gem -> (type1 <$ gem, D.VelocityNormal)) <$> hand1
                    , (\gem -> (type2 <$ gem, D.VelocityNormal)) <$> hand2
                    ]
                else return instant
              return (t, instant')
      return (diff, newGems)
    , other      = dt
    , animations = dt.drumAnimation
    , hasRBMarks = False
    , source     = ""
    , autochart  = False
    , eliteDrums = Nothing
    }
  audio = listToMaybe $ do
    dd <- Map.elems dt.drumDifficulties
    (a, _) <- RTB.getBodies dd.drumMix
    return a
  in maybe id D.setDrumMix audio newTrack

fiveNoteShuffle :: HOPOsAlgorithm -> U.Beats -> Five.FiveTrack U.Beats -> Five.FiveTrack U.Beats
fiveNoteShuffle algo hopoThreshold ft = let
  randomSeed = hash $ show ft
  in fiveResultToTrack FiveResult
    { settings  = def
    , notes     = flip Map.mapWithKey ft.fiveDifficulties $ \diff fd -> let
      chorded = guitarify'
        $ applyForces (getForces5 fd)
        $ strumHOPOTap algo hopoThreshold
        $ computeFiveFretNotes fd
      thisSeed = mkStdGen $ randomSeed + fromEnum diff
      go prev = \case
        Wait dt (thisPairs, len) rest -> do
          let sht = case thisPairs of
                (_, sht1) : _ -> sht1
                []            -> Strum
              this = map fst thisPairs
              thisLength = length this
          (new, newSet) <- if any isJust this
            then uniform $ do
              -- pick from all N-note chords which are different from the previously picked chord
              opt <- concat <$> sequence [ [[], [Just color]] | color <- [Five.Green .. Five.Orange] ]
              guard $ length opt == thisLength
              let optSet = Set.fromList opt
              guard $ maybe True (optSet /=) prev
              return (opt, optSet)
            else return (this, Set.fromList this) -- open note, don't shuffle
          Wait dt [ ((x, sht), len) | x <- new ] <$> go (Just newSet) rest
        RNil -> return RNil
      in RTB.flatten $ flip evalRand thisSeed $ go Nothing chorded
    , other     = ft
    , source    = ""
    , autochart = False
    }

fiveResultToTrack :: FiveResult -> Five.FiveTrack U.Beats
fiveResultToTrack ft = ft.other
  { Five.fiveDifficulties = Map.fromList $ do
    diff <- [Easy .. Expert]
    gems <- toList $ Map.lookup diff ft.notes
    return (diff, emit5' gems)
  }
