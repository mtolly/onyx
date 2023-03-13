{-
Onyx-designed MIDI track format for storing a Rocksmith arrangement.
(Does not support storing lower dynamic difficulty levels.)
-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module Onyx.MIDI.Track.Rocksmith
( RocksmithTrack(..)
, ToneLetter(..)
, RSModifier(..)
, buildRS
, RSOutput(..)
, ChordInfo(..)
, ChordLocation(..)
, buildRSVocals
, backportAnchors
, convertRStoPG
, nullRS
, getNotesWithModifiers
, applyLinks
) where

import           Control.Applicative              (liftA2, (<|>))
import           Control.Monad                    (forM, guard, when)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Lazy   (evalStateT, get, put)
import           Control.Monad.Trans.State.Strict (modify)
import           Data.Char                        (isDigit)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Milli)
import           Data.Foldable                    (toList)
import           Data.List.Extra                  (elemIndex, nubOrd, partition,
                                                   sort)
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DeriveHelpers
import           Onyx.Guitar                      (applyStatus1, cleanEdges,
                                                   guitarify')
import           Onyx.MIDI.Common                 hiding (RB3Instrument (..))
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.ProGuitar
import           Onyx.MIDI.Track.Vocal
import           Onyx.Rocksmith.ArrangementXML
import           Onyx.StackTrace
import           Onyx.Util.Text.Transform         (showTimestamp)
import           Onyx.Vocal.DryVox                (vocalTubes)
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

data RocksmithTrack t = RocksmithTrack
  { rsNotes      :: RTB.T t (Edge GtrFret GtrString)
  , rsModifiers  :: RTB.T t ([GtrString], [RSModifier]) -- empty string lists = apply to all notes at this time
  , rsAnchorLow  :: RTB.T t GtrFret
  , rsAnchorHigh :: RTB.T t GtrFret -- if not given, defaults to low + 3 (for a width of 4)
  , rsTones      :: RTB.T t ToneLetter
  , rsBends      :: RTB.T t ([GtrString], Milli)
  , rsPhrases    :: RTB.T t T.Text -- phrase name; repeat same name for multiple iterations of one phrase
  , rsSections   :: RTB.T t T.Text
  , rsHandShapes :: RTB.T t (Edge GtrFret GtrString)
  , rsChords     :: RTB.T t ChordInfo
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (RocksmithTrack t)

nullRS :: RocksmithTrack t -> Bool
nullRS = RTB.null . rsNotes

data ChordInfo = ChordInfo
  { ciLocation :: ChordLocation
  , ciOnce     :: Maybe [GtrString]
  -- ^ only apply to this instant, and to certain strings (low to high, if empty then all strings)
  , ciFingers  :: [Finger] -- ^ low string to high, only non-open strings
  , ciArpeggio :: Bool
  , ciNop      :: Bool -- ^ I don't know what this is but it's a flag in the sng chord
  , ciName     :: Maybe T.Text
  } deriving (Eq, Ord, Show)

data ChordLocation
  = ChordLocNotes
  | ChordLocShape
  | ChordLocAll
  deriving (Eq, Ord, Show)

data ToneLetter = ToneA | ToneB | ToneC | ToneD
  deriving (Eq, Ord, Show, Enum, Bounded)

data Finger
  = FingerThumb
  | FingerIndex
  | FingerMiddle
  | FingerRing
  | FingerPinky
  deriving (Eq, Ord, Show, Enum)

data RSModifier
  = ModSustain -- forces sustain even if short midi note
  | ModVibrato Int -- strength? seen 40, 80, maybe others
  | ModHammerOn
  | ModPullOff
  | ModSlide Int -- fret
  | ModSlideUnpitch Int -- fret
  | ModMute
  | ModPalmMute
  | ModAccent
  | ModLink
  | ModHarmonic
  | ModHarmonicPinch
  -- left hand info is in rsChords
  | ModRightHand Finger
  -- these next 3 might have an extra int parameter in xml/sng? no idea if it matters
  | ModTap
  | ModSlap
  | ModPluck
  | ModTremolo
  | ModPickUp
  | ModPickDown
  | ModIgnore
  deriving (Eq, Ord, Show)

instance TraverseTrack RocksmithTrack where
  traverseTrack fn (RocksmithTrack a b c d e f g h i j) = RocksmithTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h
    <*> fn i <*> fn j

parseStrings :: [T.Text] -> Maybe ([GtrString], [T.Text])
parseStrings = \case
  "*" : rest -> Just ([], rest)
  ns  : rest | T.all isDigit ns -> do
    strs <- forM (T.unpack ns) $ \n -> lookup n
      [('0', S6), ('1', S5), ('2', S4), ('3', S3), ('4', S2), ('5', S1)]
    Just (strs, rest)
  rest -> Just ([], rest)

unparseStrings :: [GtrString] -> T.Text
unparseStrings [] = "*"
unparseStrings strs = let
  eachStr = \case
    S1 -> '5'
    S2 -> '4'
    S3 -> '3'
    S4 -> '2'
    S5 -> '1'
    S6 -> '0'
    -- don't make sense
    S7 -> 'F'
    S8 -> 'E'
  in T.pack $ map eachStr strs

lookupFinger :: T.Text -> Maybe Finger
lookupFinger = \case
  "T" -> Just FingerThumb
  "0" -> Just FingerThumb
  "1" -> Just FingerIndex
  "2" -> Just FingerMiddle
  "3" -> Just FingerRing
  "4" -> Just FingerPinky
  _   -> Nothing

parseModifiers :: [T.Text] -> Maybe ([GtrString], [RSModifier])
parseModifiers cmd = let
  go = \case
    [] -> Just []
    "sustain"       :     rest -> cont rest $ pure ModSustain
    "vibrato"       : n : rest -> cont rest $      ModVibrato <$> readMaybe (T.unpack n)
    "hammeron"      :     rest -> cont rest $ pure ModHammerOn
    "pulloff"       :     rest -> cont rest $ pure ModPullOff
    "slide"         : n : rest -> cont rest $      ModSlide <$> readMaybe (T.unpack n)
    "slideunpitch"  : n : rest -> cont rest $      ModSlideUnpitch <$> readMaybe (T.unpack n)
    "mute"          :     rest -> cont rest $ pure ModMute
    "palmmute"      :     rest -> cont rest $ pure ModPalmMute
    "accent"        :     rest -> cont rest $ pure ModAccent
    "link"          :     rest -> cont rest $ pure ModLink
    "harmonic"      :     rest -> cont rest $ pure ModHarmonic
    "harmonicpinch" :     rest -> cont rest $ pure ModHarmonicPinch
    "righthand"     : f : rest -> cont rest $      ModRightHand <$> lookupFinger f
    "tap"           :     rest -> cont rest $ pure ModTap
    "slap"          :     rest -> cont rest $ pure ModSlap
    "pluck"         :     rest -> cont rest $ pure ModPluck
    "pop"           :     rest -> cont rest $ pure ModPluck
    "tremolo"       :     rest -> cont rest $ pure ModTremolo
    "pickup"        :     rest -> cont rest $ pure ModPickUp
    "pickdown"      :     rest -> cont rest $ pure ModPickDown
    "ignore"        :     rest -> cont rest $ pure ModIgnore
    _                          -> Nothing
    where cont rest mx = (:) <$> mx <*> go rest
  in do
    (strs, rest) <- parseStrings cmd
    mods <- go rest
    Just (strs, mods)

unparseModifiers :: ([GtrString], [RSModifier]) -> [T.Text]
unparseModifiers (strs, mods) = let
  eachMod = \case
    ModSustain        -> ["sustain"                       ]
    ModVibrato n      -> ["vibrato"      , T.pack $ show n]
    ModHammerOn       -> ["hammeron"                      ]
    ModPullOff        -> ["pulloff"                       ]
    ModSlide n        -> ["slide"        , T.pack $ show n]
    ModSlideUnpitch n -> ["slideunpitch" , T.pack $ show n]
    ModMute           -> ["mute"                          ]
    ModPalmMute       -> ["palmmute"                      ]
    ModAccent         -> ["accent"                        ]
    ModLink           -> ["link"                          ]
    ModHarmonic       -> ["harmonic"                      ]
    ModHarmonicPinch  -> ["harmonicpinch"                 ]
    ModRightHand f    -> ["righthand"    , T.pack $ show $ fromEnum f]
    ModTap            -> ["tap"                           ]
    ModSlap           -> ["slap"                          ]
    ModPluck          -> ["pluck"                         ]
    ModTremolo        -> ["tremolo"                       ]
    ModPickUp         -> ["pickup"                        ]
    ModPickDown       -> ["pickdown"                      ]
    ModIgnore         -> ["ignore"                        ]
  in unparseStrings strs : (mods >>= eachMod)

parseBend :: [T.Text] -> Maybe ([GtrString], Milli)
parseBend cmd = do
  (strs, rest) <- parseStrings cmd
  bend <- case rest of
    ["bend", x] -> readMaybe $ T.unpack x
    _           -> Nothing
  Just (strs, bend)

unparseBend :: ([GtrString], Milli) -> [T.Text]
unparseBend (strs, bend) = [unparseStrings strs, "bend", T.pack $ show bend]

parseChord :: [T.Text] -> Maybe ChordInfo
parseChord = evalStateT $ do
  get >>= \case
    "chord" : xs -> put xs
    _            -> lift Nothing
  ciLocation <- get >>= \case
    "notes" : xs -> put xs >> return ChordLocNotes
    "shape" : xs -> put xs >> return ChordLocShape
    _            -> return ChordLocAll
  ciOnce <- get >>= \case
    "once" : xs -> do
      (strs, rest) <- lift $ parseStrings xs
      put rest
      return $ Just strs
    _           -> return Nothing
  ciFingers <- get >>= \case
    "_" : xs -> put xs >> return []
    x   : xs -> do
      fingers <- lift $ mapM lookupFinger $ map T.singleton $ T.unpack x
      put xs
      return fingers
    []       -> lift Nothing
  ciArpeggio <- get >>= \case
    "arp" : xs -> put xs >> return True
    _          -> return False
  ciNop <- get >>= \case
    "nop" : xs -> put xs >> return True
    _          -> return False
  ciName <- get >>= return . \case
    [] -> Nothing
    xs -> Just $ T.unwords xs
  return ChordInfo{..}

unparseChord :: ChordInfo -> [T.Text]
unparseChord ChordInfo{..} = concat
  [ ["chord"]
  , case ciLocation of
    ChordLocAll   -> []
    ChordLocNotes -> ["notes"]
    ChordLocShape -> ["shape"]
  , case ciOnce of
    Nothing   -> []
    Just strs -> ["once", unparseStrings strs]
  , case ciFingers of
    [] -> ["_"]
    _  -> [T.pack $ ciFingers >>= show . fromEnum]
  , ["arp" | ciArpeggio]
  , ["nop" | ciNop]
  , maybe [] T.words ciName
  ]

instance ParseTrack RocksmithTrack where
  parseTrack = do
    let parseNotes root = let
          fs = \case
            EdgeOn fret str -> (str, (0, Just $ fret + 100))
            EdgeOff str     -> (str, (0, Nothing))
          fp = \case
            (str, (_, Just v))  -> EdgeOn (v - 100) str
            (str, (_, Nothing)) -> EdgeOff str
          in dimap (fmap fs) (fmap fp) $ condenseMap $ eachKey each $ \str -> edgesCV $ root + getStringIndex 6 str
    rsNotes      <- rsNotes =. parseNotes 96
    rsModifiers  <- rsModifiers =. commandMatch' parseModifiers unparseModifiers
    let fretNumber n = let
          fs fret = (0, fret + 100)
          fp (_c, v) = v - 100
          in fatBlips (1/8) $ dimap (fmap fs) (fmap fp) $ blipCV n
    rsTones      <- (rsTones =.) $ condenseMap_ $ eachKey each $ commandMatch . \case
      ToneA -> ["tone", "a"]
      ToneB -> ["tone", "b"]
      ToneC -> ["tone", "c"]
      ToneD -> ["tone", "d"]
    rsBends      <- rsBends      =. commandMatch' parseBend unparseBend
    -- short notation for phrase/section combinations
    Codec
      { codecIn = lift $ modify $ \mt -> mt
        { midiCommands = let
          f = \case
            "riff" : xs -> [["section", "riff"], ("phrase" : xs)]
            ["ng"     ] -> [["section", "noguitar"], ["phrase", "NG"]]
            ["start"  ] -> [["phrase", "default"]]
            ["end"    ] -> [["section", "noguitar"], ["phrase", "END"]]
            x           -> [x]
          in RTB.flatten $ f <$> midiCommands mt
        }
      , codecOut = const $ return ()
      }
    rsPhrases    <- rsPhrases    =. commandMatch'
      (\case
        "phrase" : xs -> Just $ T.unwords xs
        _             -> Nothing
      )
      (\xs -> "phrase" : T.words xs)
    rsSections   <- rsSections   =. commandMatch'
      (\case
        "section" : xs -> Just $ T.unwords xs
        _              -> Nothing
      )
      (\xs -> "section" : T.words xs)
    rsHandShapes <- rsHandShapes =. parseNotes 84
    rsChords     <- rsChords     =. commandMatch' parseChord unparseChord
    (rsAnchorLow, rsAnchorHigh) <- statusBlips $ liftA2 (,)
      (rsAnchorLow  =. fretNumber 108)
      (rsAnchorHigh =. fretNumber 109)
    return RocksmithTrack{..}

data RSOutput = RSOutput
  { rso_level            :: Level
  , rso_tones            :: RTB.T U.Seconds ToneLetter
  , rso_sections         :: V.Vector Section
  , rso_phrases          :: V.Vector Phrase
  , rso_phraseIterations :: V.Vector PhraseIteration
  , rso_chordTemplates   :: V.Vector ChordTemplate
  }

data ChordBank = ChordBank
  { cb_notes       :: Map.Map (Map.Map GtrString GtrFret) ChordInfo
  , cb_notes_once  :: Map.Map [GtrString] ChordInfo
  , cb_shapes      :: Map.Map (Map.Map GtrString GtrFret) ChordInfo
  , cb_shapes_once :: Map.Map [GtrString] ChordInfo
  }

data ChordsEvent
  = CENote (GtrString, GtrFret)
  | CEShape (GtrString, GtrFret)
  | CEChord ChordInfo
  deriving (Eq, Ord)

buildChordBank :: (NNC.C t) => RocksmithTrack t -> RTB.T t ChordBank
buildChordBank trk = let
  events = foldr RTB.merge RTB.empty
    [ RTB.mapMaybe (\case EdgeOn f s -> Just $ CENote (s, f); _ -> Nothing) $ rsNotes trk
    , RTB.mapMaybe (\case EdgeOn f s -> Just $ CEShape (s, f); _ -> Nothing) $ rsHandShapes trk
    , fmap CEChord $ rsChords trk
    ]
  go _    RNil               = RNil
  go bank (Wait t evts rest) = let
    funs = do
      CEChord ci <- evts
      concat
        [ do
          guard $ ciLocation ci /= ChordLocShape
          let notes = [n | CENote n <- evts]
          case ciOnce ci of
            Nothing -> do
              guard $ length notes >= 2
              return $ \b -> b { cb_notes = Map.insert (Map.fromList notes) ci $ cb_notes b }
            Just strs -> do
              return $ \b -> b { cb_notes_once = Map.insert strs ci $ cb_notes_once b }
        , do
          guard $ ciLocation ci /= ChordLocNotes
          let shapes = [n | CEShape n <- evts]
          case ciOnce ci of
            Nothing -> do
              guard $ length shapes >= 2
              return $ \b -> b { cb_shapes = Map.insert (Map.fromList shapes) ci $ cb_shapes b }
            Just strs -> do
              return $ \b -> b { cb_shapes_once = Map.insert strs ci $ cb_shapes_once b }
        ]
    bankNew = foldr ($) bank funs
    bankDropUnsaved = bankNew
      { cb_notes_once  = Map.empty
      , cb_shapes_once = Map.empty
      }
    in Wait t bankNew $ go bankDropUnsaved rest
  in go (ChordBank Map.empty Map.empty Map.empty Map.empty) $ RTB.collectCoincident events

data FretConstraint
  = FretRelease ConstraintString
  | FretBlip    ConstraintString GtrFret
  | FretHold    ConstraintString GtrFret
  | FretZero -- just used to ensure that we make an anchor even if the first note is open
  deriving (Eq, Ord)

data ConstraintString
  = NoteString  Int
  | ShapeString Int
  deriving (Eq, Ord)

initialAnchor :: Int -> ATB.T U.Seconds FretConstraint -> Maybe (Int, Int)
initialAnchor capo consts = let
  frets = map (\fs -> (minimum fs, maximum fs)) $ ATB.getBodies $ ATB.collectCoincident $ flip ATB.mapMaybe consts $ \case
    FretBlip _ f -> Just f
    FretHold _ f -> Just f
    _            -> Nothing
  -- TODO replace NE.head with NE.last or something more complicated? (closer to previous anchor)
  go possibleMins []                          = (NE.head possibleMins, NE.head possibleMins + 3)
  go possibleMins ((nextMin, nextMax) : rest) = let
    valid anchorMin = anchorMin <= nextMin && nextMax <= anchorMin + 3
    in case NE.nonEmpty $ NE.filter valid possibleMins of
      Nothing          -> (NE.head possibleMins, NE.head possibleMins + 3)
      Just newPossible -> go newPossible rest
  in case frets of
    []                                       -> Nothing
    first@(fmin, fmax) : _ | fmax - fmin > 3 -> Just first
    _                                        -> Just $ let
      possibleMins = (capo + 1) :| [ capo + 2 .. 21 ]
      in go possibleMins frets

autoAnchors :: [Note] -> Int -> [(ChordTemplate, U.Seconds, U.Seconds)] -> Map.Map U.Seconds (GtrFret, GtrFret)
autoAnchors allNotes capo shapes = let
  noteConstraints = ATB.fromPairList $ sort $ allNotes >>= \note ->
    case n_fret note of
      0 -> [(n_time note, FretZero)]
      _ -> case n_sustain note of
        Nothing -> [(n_time note, FretBlip (NoteString $ n_string note) (n_fret note))]
        Just sust -> concat
          [ [(n_time note, FretHold (NoteString $ n_string note) (n_fret note))]
          , [(n_time note <> sust, FretRelease $ NoteString $ n_string note)]
          , case n_slideTo note <|> n_slideUnpitchTo note of
            Just slide -> [(n_time note <> sust, FretBlip (NoteString $ n_string note) slide)]
            Nothing -> []
          ]
  shapeConstraints = ATB.fromPairList $ sort $ shapes >>= \(template, tstart, tend) -> let
    pairs = do
      (str, mfret) <-
        [ (0, ct_fret0 template)
        , (1, ct_fret1 template)
        , (2, ct_fret2 template)
        , (3, ct_fret3 template)
        , (4, ct_fret4 template)
        , (5, ct_fret5 template)
        ]
      fret <- toList mfret
      guard $ fret /= 0
      return (str, fret)
    in case pairs of
      [] -> [(tstart, FretZero)]
      _  -> do
        (str, fret) <- pairs
        [(tstart, FretHold (ShapeString str) fret), (tend, FretRelease $ ShapeString str)]
  constraints = ATB.merge noteConstraints shapeConstraints
  buildAnchors _ _ ANil = ANil
  buildAnchors prevAnchor@(prevMin, prevMax) fretState (At t evts rest) = let
    released = foldr Map.delete fretState [ s | FretRelease s <- evts ]
    permState = foldr (uncurry Map.insert) released [ (s, f) | FretHold s f <- evts ]
    fretsNow = case Map.elems $ foldr (uncurry Map.insert) permState [ (s, f) | FretBlip s f <- evts ] of
      []    -> Nothing
      frets -> Just (minimum frets, maximum frets)
    thisAnchor = case fretsNow of
      Nothing -> case initialAnchor capo $ ATB.flatten rest of
        Just anchor -> anchor -- jump to next position in advance
        -- TODO maybe don't jump in advance unless there is an (open) note at this point?
        Nothing     -> (prevMin, min (prevMin + 3) prevMax) -- shrink if more wide than usual
      Just (minFret, maxFret) -> case (prevMin <= minFret, maxFret <= prevMax) of
        (True, True)   -> if prevMax - prevMin == 3
          then prevAnchor -- continue previous anchor because it works
          else let
            -- shrink anchor, previous extra width not needed anymore
            thisMin = min minFret $ prevMax - 3
            thisMax = max (thisMin + 3) maxFret
            in (thisMin, thisMax)
        (True, False)  -> (min minFret (maxFret - 3), maxFret) -- need to move up
        (False, True)  -> (minFret, max (minFret + 3) maxFret) -- need to move down
        (False, False) -> (minFret, maxFret) -- need to grow
    in At t thisAnchor $ buildAnchors thisAnchor permState rest
  in Map.fromList $ ATB.toPairList
    $ RTB.toAbsoluteEventList 0
    $ noRedundantStatus
    $ RTB.fromAbsoluteEventList
    $ buildAnchors (fromMaybe (capo + 1, capo + 4) $ initialAnchor capo constraints) Map.empty
    $ ATB.collectCoincident constraints

backportAnchors :: U.TempoMap -> RocksmithTrack U.Beats -> RSOutput -> RocksmithTrack U.Beats
backportAnchors tmap trk rso = let
  anchors
    = U.unapplyTempoTrack tmap
    $ RTB.fromAbsoluteEventList
    $ ATB.fromPairList
    $ sort
    $ map (\a -> (an_time a, (an_fret a, an_fret a + an_width a - 1)))
    $ toList $ lvl_anchors $ rso_level rso
  in trk
    { rsAnchorLow  = fmap fst anchors
    , rsAnchorHigh = fmap snd anchors
    }

fillUnnamedPhrases :: RTB.T t T.Text -> RTB.T t T.Text
fillUnnamedPhrases ps = let
  named = Set.fromList $ RTB.getBodies ps
  canAdd = do
    i <- [1..] :: [Int]
    let phrase = T.pack $ "p" <> show i
    guard $ Set.notMember phrase named
    return phrase
  go RNil  _                       = RNil
  go (Wait t "" rest) (new : news) = Wait t new $ go rest news
  go (Wait t p  rest) news         = Wait t p   $ go rest news
  in go ps canAdd

-- Turn RS linked sustains into RB style (link child either merged into first
-- note if same fret, or marked as HOPO if different fret)
applyLinks :: (NNC.C t) => RTB.T t (Edge (GtrFret, [RSModifier]) GtrString) -> RTB.T t (Edge (GtrFret, [RSModifier]) GtrString)
applyLinks = \case
  Wait t thisEdge rest -> case thisEdge of
    EdgeOff _ -> Wait t thisEdge $ applyLinks rest
    EdgeOn (fret, mods) str -> if elem ModLink mods
      then let
        findThisOff = \case
          EdgeOff str' -> guard (str == str') >> Just ()
          _            -> Nothing
        findNextOn = \case
          EdgeOn fretMods str' -> guard (str == str') >> Just fretMods
          _                    -> Nothing
        in case U.extractFirst findThisOff rest of
          Nothing -> Wait t thisEdge $ applyLinks rest
          Just ((thisLength, ()), rest') -> case U.extractFirst findNextOn rest' of
            Nothing -> Wait t thisEdge $ applyLinks rest
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
                in applyLinks $ Wait t (EdgeOn (fret, mods') str) rest''
              -- if linking to a different fret, don't remove any notes, but mark 2nd note as "HOPO"
              else Wait t thisEdge
                $ applyLinks
                $ RTB.insert thisLength (EdgeOff str)
                $ RTB.insert beforeNextOn (EdgeOn (nextFret, ModHammerOn : nextMods) str)
                $ rest''
      else Wait t thisEdge $ applyLinks rest
  RNil -> RNil

-- TODO actually use this in buildRS
getNotesWithModifiers :: (NNC.C t) => RocksmithTrack t -> RTB.T t (Edge (GtrFret, [RSModifier]) GtrString)
getNotesWithModifiers trk = let
  merged = RTB.collectCoincident $ RTB.merge (Left <$> rsModifiers trk) (Right <$> rsNotes trk)
  in RTB.flatten $ flip fmap merged $ \instant -> let
    modsNow = [ x | Left x <- instant ]
    modsForString str = concat [ mods | (strs, mods) <- modsNow, null strs || elem str strs ]
    edgesNow = [ x | Right x <- instant ]
    in flip map edgesNow $ \case
      EdgeOn fret str -> EdgeOn (fret, modsForString str) str
      EdgeOff     str -> EdgeOff str

buildRS :: (SendMessage m) => U.TempoMap -> Int -> RocksmithTrack U.Beats -> StackTraceT m RSOutput
buildRS tmap capo trk = do
  let applyCapo 0 = 0
      applyCapo f = f + capo
      phrases = fillUnnamedPhrases $ rsPhrases trk
      insideTime t = inside $ T.unpack $ showTimestamp t
      numberSections _ [] = []
      numberSections counts ((t, sect) : rest) = let
        n = fromMaybe 0 $ Map.lookup sect counts
        thisSection = Section
          { sect_name = sect
          , sect_number = n + 1
          , sect_startTime = U.applyTempoMap tmap t
          }
        counts' = Map.insert sect (n + 1) counts
        in thisSection : numberSections counts' rest
      uniquePhrases = nubOrd $ toList phrases
      modifierMap = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0
        $ RTB.collectCoincident $ U.applyTempoTrack tmap $ rsModifiers trk
      lookupModifier t str = let
        atTime = fromMaybe [] $ Map.lookup t modifierMap
        in concat [ mods | (strs, mods) <- atTime, null strs || elem str strs ]
      bendMap = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0
        $ RTB.collectCoincident $ U.applyTempoTrack tmap $ rsBends trk
      lookupBends t len str = let
        (_, startBend, m1) = Map.splitLookup t bendMap
        (m2, endBend, _) = Map.splitLookup (t <> len) m1
        m3 = maybe id (Map.insert t) startBend
          $ maybe id (Map.insert $ t <> len) endBend m2
        -- TODO do we need a separate event for "bends right at the end of a sustain"?
        in do
          (bendTime, evts) <- Map.toList m3
          (strs, bend) <- evts
          guard $ null strs || elem str strs
          return (bendTime, bend)
      makeNote t (fret, str, len) = let
        mods = lookupModifier t str
        bends = lookupBends t len str
        in Note
          { n_time           = t
          , n_string         = case str of
            S6 -> 0
            S5 -> 1
            S4 -> 2
            S3 -> 3
            S2 -> 4
            S1 -> 5
            _  -> -1
          , n_fret           = applyCapo fret
          , n_sustain        = let
            startBeats = U.unapplyTempoMap tmap t
            endBeats = U.unapplyTempoMap tmap $ t <> len
            forcesSustain = \case
              ModSustain        -> True
              ModLink           -> True
              ModSlide _        -> True
              ModSlideUnpitch _ -> True
              ModTremolo        -> True
              _                 -> False
            in if endBeats - startBeats >= (1/3) || any forcesSustain mods
              then Just len
              else Nothing
          , n_vibrato        = listToMaybe [ n | ModVibrato n <- mods ]
          , n_hopo           = False -- I don't think you need this?
          , n_hammerOn       = elem ModHammerOn mods
          , n_pullOff        = elem ModPullOff mods
          , n_slideTo        = listToMaybe [ applyCapo n | ModSlide n <- mods ]
          , n_slideUnpitchTo = listToMaybe [ applyCapo n | ModSlideUnpitch n <- mods ]
          , n_mute           = elem ModMute mods
          , n_palmMute       = elem ModPalmMute mods
          , n_accent         = elem ModAccent mods
          , n_linkNext       = elem ModLink mods
          , n_bend           = case bends of
            [] -> Nothing
            _  -> Just $ maximum $ map snd bends
          , n_bendValues     = V.fromList $ flip map bends $ \(bendTime, bend) -> BendValue
            { bv_time = bendTime
            , bv_step = Just bend
            }
          , n_harmonic       = elem ModHarmonic mods
          , n_harmonicPinch  = elem ModHarmonicPinch mods
          , n_leftHand       = Nothing -- if chord, assigned later when we look up the chord info
          , n_rightHand      = listToMaybe [ fromEnum n | ModRightHand n <- mods ]
          , n_tap            = elem ModTap mods
          , n_slap           = if elem ModSlap mods then Just 1 else Nothing
          , n_pluck          = if elem ModPluck mods then Just 1 else Nothing
          , n_tremolo        = elem ModTremolo mods
          , n_pickDirection  = Nothing -- TODO
          , n_ignore         = elem ModIgnore mods || fret > 22
          -- frets 23 and 24, you need to set to ignore or the scoring doesn't work right (can't get 100%).
          -- EOF does the same thing
          }
      chordBank = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ U.applyTempoTrack tmap $ buildChordBank trk
      makeTemplate cinfo notes = let
        sortedNotes = sort [(str, fret) | (fret, str, _) <- notes]
        assignFingers []       []                = return []
        assignFingers []       _ns               = do
          -- when (any ((/= 0) . snd) ns) $ warn $ "No fingers assigned for chord: " <> show ns
          return []
        assignFingers fs       ((str, 0) : rest) = ((str, 0, Nothing) :) <$> assignFingers fs rest
        assignFingers (f : fs) ((str, n) : rest) = ((str, n, Just f ) :) <$> assignFingers fs rest
        assignFingers _        _                 = fatal $ unwords
          [ "Mismatched chord fingers: can't match"
          , show notes
          , "with"
          , show cinfo
          ]
        finish assigned = ChordTemplate
          { ct_chordName   = fromMaybe "" $ ciName cinfo
          , ct_displayName = fromMaybe "" (ciName cinfo)
            <> (if ciArpeggio cinfo then "-arp" else "")
            <> (if ciNop      cinfo then "-nop" else "")
          , ct_finger0     = listToMaybe [fromEnum finger | (S6, _, Just finger) <- assigned]
          , ct_finger1     = listToMaybe [fromEnum finger | (S5, _, Just finger) <- assigned]
          , ct_finger2     = listToMaybe [fromEnum finger | (S4, _, Just finger) <- assigned]
          , ct_finger3     = listToMaybe [fromEnum finger | (S3, _, Just finger) <- assigned]
          , ct_finger4     = listToMaybe [fromEnum finger | (S2, _, Just finger) <- assigned]
          , ct_finger5     = listToMaybe [fromEnum finger | (S1, _, Just finger) <- assigned]
          , ct_fret0       = listToMaybe [applyCapo fret | (S6, fret) <- sortedNotes]
          , ct_fret1       = listToMaybe [applyCapo fret | (S5, fret) <- sortedNotes]
          , ct_fret2       = listToMaybe [applyCapo fret | (S4, fret) <- sortedNotes]
          , ct_fret3       = listToMaybe [applyCapo fret | (S3, fret) <- sortedNotes]
          , ct_fret4       = listToMaybe [applyCapo fret | (S2, fret) <- sortedNotes]
          , ct_fret5       = listToMaybe [applyCapo fret | (S1, fret) <- sortedNotes]
          }
        in finish <$> assignFingers (ciFingers cinfo) sortedNotes
      makeNoteChord t cinfo noteGroup = do
        template <- makeTemplate cinfo noteGroup
        let madeNotes = do
              triple <- noteGroup
              let note = makeNote t triple
              return note
                { n_leftHand = case n_string note of
                  0 -> ct_finger0 template
                  1 -> ct_finger1 template
                  2 -> ct_finger2 template
                  3 -> ct_finger3 template
                  4 -> ct_finger4 template
                  5 -> ct_finger5 template
                  _ -> Nothing
                }
        return $ Right (template, madeNotes)
      makeShapeChord t cinfo noteGroup = do
        template <- makeTemplate cinfo noteGroup
        let startTime = t
            endTime = t <> case head noteGroup of (_, _, len) -> len
        return (template, startTime, endTime)
      findOnceChords makeChord = let
        go chordsSoFar noteGroup [] = return $ Right (noteGroup, chordsSoFar)
        go chordsSoFar noteGroup ((strs, cinfo) : cinfos) = let
          noteStrings = [ str | (_, str, _) <- noteGroup ]
          chordStrings = if null strs then noteStrings else strs
          in if all (`elem` noteStrings) chordStrings
            then do
              let (usedNotes, unusedNotes) = partition (\(_, str, _) -> elem str chordStrings) noteGroup
              chord <- makeChord cinfo usedNotes
              go (chord : chordsSoFar) unusedNotes cinfos
            else return $ Left cinfo
        in go []
  notesAndChords <- let
    notes = RTB.collectCoincident $ joinEdgesSimple $ U.applyTempoTrack tmap $ rsNotes trk
    in fmap concat $ forM (ATB.toPairList $ RTB.toAbsoluteEventList 0 $ notes) $ \(t, noteGroup) -> insideTime t $ do
      let (bankTime, bankState) = fromMaybe
            (t, ChordBank Map.empty Map.empty Map.empty Map.empty)
            (Map.lookupLE t chordBank)
          oneTimeChords = if bankTime == t then Map.toList $ cb_notes_once bankState else []
      findOnceChords (makeNoteChord t) noteGroup oneTimeChords >>= \case
        Left cinfo -> do
          warn $ "Couldn't apply one-time chord (notes) event: " <> show cinfo
          -- don't bother trying to make chords, at least for now
          return $ map (Left . makeNote t) noteGroup
        Right ([], onceChords) -> return onceChords
        Right (noteGroup', onceChords) -> (onceChords <>) <$> case noteGroup' of
          [triple] -> return [Left $ makeNote t triple]
          _        -> case [ len | (_, _, len) <- noteGroup' ] of
            x : xs | any (/= x) xs ->
              -- uneven lengths, emit as single notes. e.g. unison bend in 25 or 6 to 4 Lead
              return $ map (Left . makeNote t) noteGroup'
            _ -> let
              key = Map.fromList [ (str, fret) | (fret, str, _) <- noteGroup' ]
              in case Map.lookupLE t chordBank >>= Map.lookup key . cb_notes . snd of
                Just cinfo -> (: []) <$> makeNoteChord t cinfo noteGroup'
                Nothing -> do
                  warn $ "Not making simultaneous notes into a chord due to no chord mapping:  " <> show key
                  return $ map (Left . makeNote t) noteGroup'
      -- note: if you have any chords in the notes, you need at least one handshape, otherwise CST crashes
  shapes <- concat <$> let
    shapeNotes = RTB.collectCoincident $ joinEdgesSimple $ U.applyTempoTrack tmap $ rsHandShapes trk
    in forM (ATB.toPairList $ RTB.toAbsoluteEventList 0 $ shapeNotes) $ \(t, noteGroup) -> insideTime t $ do
      let (bankTime, bankState) = fromMaybe
            (t, ChordBank Map.empty Map.empty Map.empty Map.empty)
            (Map.lookupLE t chordBank)
          oneTimeChords = if bankTime == t then Map.toList $ cb_shapes_once bankState else []
      findOnceChords (makeShapeChord t) noteGroup oneTimeChords >>= \case
        Left cinfo -> do
          warn $ "Couldn't apply one-time chord (shape) event: " <> show cinfo
          return []
        Right ([], onceChords) -> return onceChords
        Right (noteGroup', onceChords) -> (onceChords <>) <$> let
          key = Map.fromList [ (str, fret) | (fret, str, _) <- noteGroup' ]
          in case Map.lookupLE t chordBank >>= Map.lookup key . cb_shapes . snd of
            Just cinfo -> (: []) <$> makeShapeChord t cinfo noteGroup'
            Nothing -> do
              warn $ "Couldn't find handshape chord mapping for " <> show key
              return []
  let allNotes = notesAndChords >>= \case
        Left  note       -> [note]
        Right (_, notes) -> notes
      chordTemplates = nubOrd
        $  [ template | Right (template, _) <- notesAndChords ]
        <> [ template | (template, _, _)    <- shapes         ]
      chordTemplateIndexes = Map.fromList $ zip chordTemplates [0..]
  case length phrases of
    n -> when (n > 100) $ warn $ "There are " <> show n <> " phrases; more than 100 phrases won't display correctly in game"
  return RSOutput
    { rso_level = Level
      { lvl_difficulty    = 0
      , lvl_notes         = V.fromList [ n | Left n <- notesAndChords ]
      , lvl_chords        = V.fromList $ do
        Right (template, notes) <- notesAndChords
        return Chord
          { chd_time         = n_time $ head notes
          , chd_chordId      = fromMaybe (-1) $ Map.lookup template chordTemplateIndexes
          , chd_accent       = all n_accent notes
          , chd_highDensity  = False -- TODO what does this do exactly? is it visual or scoring?
          , chd_palmMute     = all n_palmMute notes
          , chd_fretHandMute = all n_mute notes
          , chd_linkNext     = all n_linkNext notes
          , chd_chordNotes   = V.fromList notes
          , chd_ignore       = all n_ignore notes
          , chd_hopo         = False -- is this required?
          , chd_strum        = Nothing -- TODO get from n_pickDirection
          }
      , lvl_handShapes    = V.fromList $ do
        (template, startTime, endTime) <- shapes
        return HandShape
          { hs_chordId   = fromMaybe (-1) $ Map.lookup template chordTemplateIndexes
          , hs_startTime = startTime
          , hs_endTime   = endTime
          }
      , lvl_fretHandMutes = mempty
      , lvl_anchors       = let
        -- note: according to EOF, highest min-fret for an anchor is 21
        anchorMap = if RTB.null $ rsAnchorLow trk
          then autoAnchors allNotes capo shapes
          else let
            merged = RTB.collectCoincident $ RTB.merge (Left <$> rsAnchorLow trk) (Right <$> rsAnchorHigh trk)
            bounds = U.applyTempoTrack tmap $ flip RTB.mapMaybe merged $ \evts ->
              case ([ low | Left low <- evts ], [ high | Right high <- evts ]) of
                (low : _, high : _) -> Just (applyCapo low, applyCapo high)
                (low : _, []      ) -> Just (applyCapo low, applyCapo $ low + 3)
                _                   -> Nothing
            in Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ noRedundantStatus bounds
        -- each phrase needs to have an anchor at the start (or at least before its first note)
        addPhraseAnchors m []   = m
        addPhraseAnchors m (t : ts) = case Map.lookupLE t m of
          Nothing          -> addPhraseAnchors m ts
          Just (_, anchor) -> addPhraseAnchors (Map.insert t anchor m) ts
        anchorMap' = addPhraseAnchors anchorMap $ ATB.getTimes
          $ RTB.toAbsoluteEventList 0 $ U.applyTempoTrack tmap phrases
        in V.fromList $ flip map (Map.toList anchorMap') $ \(t, (low, high)) -> Anchor
          { an_time  = t
          , an_fret  = low
          , an_width = max 1 $ high - low + 1
          }
      }
    , rso_tones = U.applyTempoTrack tmap $ rsTones trk
    , rso_sections = V.fromList $ numberSections Map.empty
      $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ rsSections trk
    , rso_phrases = V.fromList $ flip map uniquePhrases $ \phrase -> Phrase
      { ph_maxDifficulty = 0
      , ph_name          = phrase
      , ph_disparity     = Nothing
      , ph_ignore        = False
      , ph_solo          = False
      }
    , rso_phraseIterations
      = V.fromList
      $ map (\(t, phrase) -> PhraseIteration
        { pi_time       = U.applyTempoMap tmap t
        , pi_phraseId   = fromMaybe (-1) $ elemIndex phrase uniquePhrases
        , pi_variation  = Nothing
        , pi_heroLevels = V.empty
        })
      $ ATB.toPairList
      $ RTB.toAbsoluteEventList 0 phrases
    , rso_chordTemplates = V.fromList chordTemplates
    }

data VocalEvent
  = VocalNoteEnd
  | VocalPhraseEnd
  | VocalNote T.Text Int
  deriving (Eq, Ord) -- constructor order is important

buildRSVocals :: U.TempoMap -> VocalTrack U.Beats -> Vocals
buildRSVocals tmap vox = Vocals $ V.fromList $ let
  tubes = vocalTubes vox
  pitches = fmap fst $ RTB.filter snd $ vocalNotes vox
  tubeEvents = flip fmap (applyStatus1 (Octave60 C) pitches tubes) $ \case
    (p, Just lyric) -> VocalNote lyric $ 36 + fromEnum p
    (_, Nothing   ) -> VocalNoteEnd
  phraseEnds = RTB.filter not $ RTB.merge (vocalPhrase1 vox) (vocalPhrase2 vox)
  evts = U.applyTempoTrack tmap $ RTB.normalize $
    RTB.merge tubeEvents $ fmap (const VocalPhraseEnd) phraseEnds
  go = \case
    Wait dt1 (VocalNote lyric pitch) (Wait dt2 VocalNoteEnd rest) -> let
      lyric1 = fromMaybe lyric $ T.stripSuffix "#" lyric <|> T.stripSuffix "^" lyric
      lyric2 = case T.stripSuffix "=" lyric1 of
        Just x  -> x <> "--"
        Nothing -> lyric1
      lyric3 = case rest of
        Wait _ VocalPhraseEnd _ -> lyric2 <> "+"
        _                       -> lyric2
      -- probably don't need to handle $ because this only runs on PART VOCALS.
      -- TODO maybe fix the weird "two vowels in one syllable" char
      noteFn = \t -> Vocal
        { voc_time = t
        , voc_note = pitch
        , voc_length = dt2
        , voc_lyric = lyric3
        }
      in Wait dt1 noteFn $ RTB.delay dt2 $ go rest
    Wait dt _ rest -> RTB.delay dt $ go rest
    RNil -> RNil
  in map (\(t, f) -> f t) $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ go evts

convertRStoPG :: (SendMessage m) => RocksmithTrack U.Beats -> StackTraceT m (ProGuitarTrack U.Beats)
convertRStoPG rs = let

  calculatedNotes :: RTB.T U.Beats ([(GtrString, GtrFret, [RSModifier])], Maybe U.Beats)
  calculatedNotes
    = guitarify'
    $ fmap (\((fret, mods), str, len) -> ((str, fret, mods), len))
    $ edgeBlips minSustainLengthRB
    $ applyLinks
    $ getNotesWithModifiers rs

  calculatedShapes :: RTB.T U.Beats ([(GtrString, GtrFret)], U.Beats)
  calculatedShapes
    = fmap (\(shape, (), len) -> (shape, len))
    $ joinEdgesSimple
    $ noOverlapShapes []
    $ RTB.collectCoincident $ rsHandShapes rs
  noOverlapShapes cur = \case
    RNil             -> RNil -- cur should be empty, hopefully
    Wait dt new rest -> let
      ons = [ (str, fret) | EdgeOn fret str <- new ]
      in if null ons
        then Wait dt (EdgeOff ()) $ noOverlapShapes [] rest
        else if null cur
          then Wait dt                       (EdgeOn ons ()) $ noOverlapShapes ons rest
          else Wait dt (EdgeOff ()) $ Wait 0 (EdgeOn ons ()) $ noOverlapShapes ons rest

  combinedNotesShapes :: RTB.T U.Beats
    ( [(GtrString, GtrFret, [RSModifier])] -- real notes
    , Maybe U.Beats -- notes length
    , Maybe ([(GtrString, GtrFret)], U.Beats) -- handshape starting here, list is ghost notes on top of real notes
    )
  combinedNotesShapes
    = RTB.mapMaybe (\instant -> let
      notes = lefts instant
      shapes = rights instant
      in case (notes, shapes) of
        ([], _) -> Nothing
        ((chord, len) : _, []) -> Just (chord, len, Nothing)
        ((chord, len) : _, shape : _) -> let
          mapChord = Map.fromList [ (str, fret) | (str, fret, _) <- chord ]
          mapShape = Map.fromList $ fst shape
          -- union uses left in case of duplicate, so this checks if mapShape is a superset of mapChord
          in if Map.union mapChord mapShape == mapShape
            then let
              ghosts = Map.difference mapShape mapChord
              in Just (chord, len, Just (Map.toList ghosts, snd shape))
            else Just (chord, len, Nothing)
      )
    $ RTB.collectCoincident
    $ RTB.merge (Left <$> calculatedNotes) (Right <$> calculatedShapes)

  convertedNotes :: RTB.T U.Beats (Edge GtrFret (GtrString, NoteType))
  convertedNotes
    = blipEdgesRB
    $ RTB.flatten
    $ fmap (\(chord, len, maybeShape) -> let
      realNotes = do
        (str, fret, mods) <- chord
        let ntype
              | elem ModMute mods = Muted
              | elem ModTap  mods = Tapped
              | otherwise         = NormalNote
        return (fret, (str, ntype), len)
      ghostNotes = case maybeShape of
        Nothing -> []
        Just (shapeExtra, _shapeLength) -> do
          (str, fret) <- shapeExtra
          return (fret, (str, ArpeggioForm), len)
      in realNotes <> ghostNotes
      )
    $ combinedNotesShapes

  convertedForces :: RTB.T U.Beats (Edge () PGForce)
  convertedForces
    = fmap (\(b, x) -> if b then EdgeOn () x else EdgeOff x)
    $ cleanEdges
    $ U.trackJoin
    $ fmap (\(chord, _) -> let
      allMods = chord >>= \(_, _, mods) -> mods
      force = if any (`elem` allMods) [ModHammerOn, ModPullOff, ModTap]
        then PGForceHOPO
        else PGForceStrum
      in RTB.fromPairList [(0, (True, force)), (1/480, (False, force))]
      )
    $ calculatedNotes

  convertedArpeggio :: RTB.T U.Beats Bool
  convertedArpeggio
    = U.trackJoin
    $ RTB.mapMaybe (\(_, _, maybeShape) -> case maybeShape of
      Nothing               -> Nothing
      Just (_, shapeLength) -> Just $ RTB.fromPairList
        [(0, True), (shapeLength, False)]
      )
    $ combinedNotesShapes

  chordBank = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ buildChordBank rs

  convertedChordNames :: RTB.T U.Beats (Maybe T.Text)
  convertedChordNames
    = RTB.fromAbsoluteEventList
    $ ATB.fromPairList
    $ mapMaybe (\(t, (chord, _, maybeShape)) -> do
      (shape, _shapeLength) <- maybeShape
      case Map.lookupLE t chordBank of
        Nothing               -> Just (t, Nothing)
        Just (bankTime, bank) -> let
          targetChord = Map.fromList $ [ (str, fret) | (str, fret, _) <- chord ] <> shape
          lookupOnce = do
            guard $ bankTime == t
            Map.lookup (Map.keys targetChord) $ cb_shapes_once bank
          lookupNotOnce = Map.lookup targetChord $ cb_shapes bank
          in Just (t, (lookupOnce <|> lookupNotOnce) >>= ciName)
      )
    $ ATB.toPairList
    $ RTB.toAbsoluteEventList 0 combinedNotesShapes

  -- TODO check if there's any chord name character substitution we need to do.
  -- sharp/flat should already be good to go (#/b in both formats apparently)

  desiredSlides :: RTB.T U.Beats Slide
  desiredSlides
    = RTB.mapMaybe (\(chord, _) -> let
      directions = do
        (_, fret, mods) <- chord
        targetFret <- mods >>= \case
          ModSlide        n -> [n]
          ModSlideUnpitch n -> [n]
          _                 -> []
        case compare fret targetFret of
          EQ -> []
          LT -> [SlideUp]
          GT -> [SlideDown]
      in listToMaybe directions
      )
    $ calculatedNotes

  resultDefaultSlides = mempty
    { pgNotes = convertedNotes
    , pgForce = convertedForces
    , pgArpeggio = convertedArpeggio
    , pgSlide = NormalSlide <$ desiredSlides
    , pgChordName = convertedChordNames
    }

  defaultSlideDirections :: RTB.T U.Beats Slide
  defaultSlideDirections
    = RTB.mapMaybe (\(_, _, sustain) -> sustain >>= snd)
    $ guitarifyFull (170/480) resultDefaultSlides

  fixedSlideMarkers :: RTB.T U.Beats SlideType
  fixedSlideMarkers
    = RTB.flatten
    $ fmap (\instant -> case (lefts instant, rights instant) of
      (desired : _, got : _) -> [if desired == got then NormalSlide else ReversedSlide]
      (desired    , _      ) -> NormalSlide <$ desired -- probably shouldn't happen but maybe possible
      )
    $ RTB.collectCoincident
    $ RTB.merge (Left <$> desiredSlides) (Right <$> defaultSlideDirections)

  resultFixedSlides = resultDefaultSlides
    { pgSlide = fixedSlideMarkers
    }

  in return mempty
    { pgDifficulties = Map.singleton Expert resultFixedSlides
    -- TODO pgTremolo
    }

{-
planning for convertRStoPG

NOTE PROPERTIES

hammeron/pulloff: force hopo (could also force other notes to strum?)

channel priority
mute: set mute channel
tap: set tap channel (maybe also force hopo to be sure)
(rest don't do anything)
vibrato: set bend channel
harmonic: set harmonic channel
pinch harmonic: set pinch harmonic channel

slide: add slide (compute up/down), maybe try to add endpoint note (hopo)
slideunpitch: add slide (compute up/down)

ignore: maybe optionally add mute channel?

slap/pop: nope
accent: nope
palmmute: probably ignore

tremolo: turn into many notes with tremolo lane?
link: figure out proper way of combining notes

CHORDS

in general, just turn handshapes/arpeggios into arpeggios.
chord names can just be brought over exactly that way.
EXCEPT when the contents are an unchanging chord, can just have the chords. (sustain maybe?)
but, what if the arpeggio starts with a note not in the arp shape? (not supported in RB)
could start the arpeggio right before that note maybe.
-}
