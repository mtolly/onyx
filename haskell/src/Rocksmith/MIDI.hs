{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module Rocksmith.MIDI
( RocksmithTrack(..)
, ToneLetter(..)
, RSModifier(..)
, buildRS
, RSOutput(..)
, ChordInfo(..)
, ChordLocation(..)
) where

import           Control.Monad                    (forM)
import           Control.Monad.Codec
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Milli)
import           Data.Foldable                    (toList)
import           Data.List.Extra                  (elemIndex, nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import           RockBand.Codec
import           RockBand.Codec.ProGuitar
import           RockBand.Common
import           Rocksmith.ArrangementXML
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

data RocksmithTrack t = RocksmithTrack
  { rsNotes      :: RTB.T t (Edge GtrFret GtrString)
  , rsModifiers  :: RTB.T t ([GtrString], [RSModifier]) -- empty string lists = apply to all notes at this time
  , rsAnchorLow  :: RTB.T t GtrFret
  , rsAnchorHigh :: RTB.T t GtrFret -- if not given, defaults to low + 3 (for a width of 4)
  , rsTones      :: RTB.T t ToneLetter
  , rsBends      :: RTB.T t ([GtrString], Milli) -- TODO I suspect we also need to support "unk5"
  , rsPhrases    :: RTB.T t T.Text -- phrase name; repeat same name for multiple iterations of one phrase
  , rsSections   :: RTB.T t T.Text
  , rsHandShapes :: RTB.T t (Edge GtrFret GtrString)
  , rsChords     :: RTB.T t ChordInfo
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (RocksmithTrack t)

data ChordInfo = ChordInfo
  { ciLocation :: ChordLocation
  , ciFingers  :: [Finger] -- low string to high
  , ciArpeggio :: Bool
  , ciNop      :: Bool -- I don't know what this is but it's a flag in the sng chord
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
  | ModBend Milli -- pre bend
  | ModHarmonic
  | ModHarmonicPinch
  -- left hand info is in rsChords
  | ModRightHand Finger
  | ModTap
  | ModSlap Int -- what is the number? or is it just bool?
  | ModPluck Int -- what is the number? or is it just bool?
  | ModTremolo
  | ModPickUp
  | ModPickDown
  | ModIgnore -- does this mean don't try to detect it?
  deriving (Eq, Ord, Show)

instance TraverseTrack RocksmithTrack where
  traverseTrack fn (RocksmithTrack a b c d e f g h i j) = RocksmithTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h
    <*> fn i <*> fn j

parseStrings :: [T.Text] -> Maybe ([GtrString], [T.Text])
parseStrings = \case
  "*" : rest -> Just ([], rest)
  ns  : rest -> do
    strs <- forM (T.unpack ns) $ \n -> lookup n
      [('0', S6), ('1', S5), ('2', S4), ('3', S3), ('4', S2), ('5', S1)]
    Just (strs, rest)
  _ -> Nothing

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
    "prebend"       : n : rest -> cont rest $      ModBend <$> readMaybe (T.unpack n)
    "harmonic"      :     rest -> cont rest $ pure ModHarmonic
    "harmonicpinch" :     rest -> cont rest $ pure ModHarmonicPinch
    "righthand"     : f : rest -> cont rest $      ModRightHand <$> lookupFinger f
    "tap"           :     rest -> cont rest $ pure ModTap
    "slap"          : n : rest -> cont rest $      ModSlap <$> readMaybe (T.unpack n)
    "pluck"         : n : rest -> cont rest $      ModPluck <$> readMaybe (T.unpack n)
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
    ModBend n         -> ["prebend"      , T.pack $ show n]
    ModHarmonic       -> ["harmonic"                      ]
    ModHarmonicPinch  -> ["harmonicpinch"                 ]
    ModRightHand f    -> ["righthand"    , T.pack $ show $ fromEnum f]
    ModTap            -> ["tap"                           ]
    ModSlap n         -> ["slap"         , T.pack $ show n]
    ModPluck n        -> ["pluck"        , T.pack $ show n]
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
parseChord = go where
  go = \case
    "chord" : xs -> parseLocation xs
    _            -> Nothing
  parseLocation = \case
    "notes" : xs -> parseFingers ChordLocNotes xs
    "shape" : xs -> parseFingers ChordLocShape xs
    xs           -> parseFingers ChordLocAll xs
  parseFingers loc = \case
    "_" : xs -> parseArp loc [] xs
    x   : xs -> do
      fingers <- mapM lookupFinger $ map T.singleton $ T.unpack x
      parseArp loc fingers xs
    _        -> Nothing
  parseArp loc fingers = \case
    "arp" : xs -> parseNop loc fingers True xs
    xs         -> parseNop loc fingers False xs
  parseNop loc fingers arp = \case
    "nop" : xs -> parseName loc fingers arp True xs
    xs         -> parseName loc fingers arp False xs
  parseName loc fingers arp nop = Just . \case
    [] -> ChordInfo loc fingers arp nop Nothing
    xs -> ChordInfo loc fingers arp nop $ Just $ T.unwords xs

unparseChord :: ChordInfo -> [T.Text]
unparseChord ChordInfo{..} = concat
  [ ["chord"]
  , case ciLocation of
    ChordLocAll   -> []
    ChordLocNotes -> ["notes"]
    ChordLocShape -> ["shape"]
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
            EdgeOff str -> (str, (0, Nothing))
          fp = \case
            (str, (_, Just v)) -> EdgeOn (v - 100) str
            (str, (_, Nothing)) -> EdgeOff str
          in dimap (fmap fs) (fmap fp) $ condenseMap $ eachKey each $ \str -> edgesCV $ root + getStringIndex 6 str
    rsNotes      <- rsNotes =. parseNotes 96
    rsModifiers  <- rsModifiers =. commandMatch' parseModifiers unparseModifiers
    let fretNumber n = let
          fs fret = (0, fret + 100)
          fp (_c, v) = v - 100
          in fatBlips (1/8) $ dimap (fmap fs) (fmap fp) $ blipCV n
    rsAnchorLow  <- rsAnchorLow  =. fretNumber 108
    rsAnchorHigh <- rsAnchorHigh =. fretNumber 109
    rsTones      <- (rsTones =.) $ condenseMap_ $ eachKey each $ commandMatch . \case
      ToneA -> ["tone", "a"]
      ToneB -> ["tone", "b"]
      ToneC -> ["tone", "c"]
      ToneD -> ["tone", "d"]
    rsBends      <- rsBends      =. commandMatch' parseBend unparseBend
    rsPhrases    <- rsPhrases    =. commandMatch'
      (\case
        ["phrase", x] -> Just x
        _             -> Nothing
      )
      (\x -> ["phrase", x])
    rsSections   <- rsSections   =. commandMatch'
      (\case
        ["section", x] -> Just x
        _              -> Nothing
      )
      (\x -> ["section", x])
    rsHandShapes <- rsHandShapes =. parseNotes 84
    rsChords     <- rsChords     =. commandMatch' parseChord unparseChord
    return RocksmithTrack{..}

data RSOutput = RSOutput
  { rso_level            :: Level
  , rso_tones            :: RTB.T U.Seconds ToneLetter
  , rso_sections         :: V.Vector Section
  , rso_phrases          :: V.Vector Phrase
  , rso_phraseIterations :: V.Vector PhraseIteration
  , rso_chordTemplates   :: V.Vector ChordTemplate
  }

buildRS :: U.TempoMap -> RocksmithTrack U.Beats -> RSOutput
buildRS tmap trk = RSOutput
  { rso_level = Level
    { lvl_difficulty    = 0
    , lvl_notes         = V.fromList allNotes
    , lvl_chords        = mempty
    , lvl_handShapes    = mempty
    , lvl_fretHandMutes = mempty
    , lvl_anchors       = V.singleton $ let
      -- TODO real anchors (manual or auto)
      -- TODO make sure we write out an anchor at the start of each phrase! (before its first note)
      -- note: according to EOF, highest min-fret for an anchor is 21
      minFret = max 1 $ minimum $ map n_fret allNotes
      maxFret = maximum $ map n_fret allNotes
      in Anchor
        { an_time  = 0
        , an_fret  = minFret
        , an_width = fromIntegral $ max 1 $ maxFret - minFret
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
    $ RTB.toAbsoluteEventList 0
    $ rsPhrases trk
  , rso_chordTemplates = V.empty
  } where
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
    uniquePhrases = nubOrd $ toList $ rsPhrases trk
    modifierMap = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0
      $ RTB.collectCoincident $ U.applyTempoTrack tmap $ rsModifiers trk
    lookupModifier t str = let
      atTime = fromMaybe [] $ Map.lookup t modifierMap
      in concat [ mods | (strs, mods) <- atTime, null strs || elem str strs ]
    allNotes = let
      notes = joinEdgesSimple $ U.applyTempoTrack tmap $ rsNotes trk
      in flip map (ATB.toPairList $ RTB.toAbsoluteEventList 0 notes) $ \(t, (fret, str, len)) -> let
        mods = lookupModifier t str
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
          , n_fret           = fret
          , n_sustain        = let
            startBeats = U.unapplyTempoMap tmap t
            endBeats = U.unapplyTempoMap tmap $ t <> len
            in if endBeats - startBeats >= (1/3)
              then Just len
              else Nothing
          , n_vibrato        = listToMaybe [ n | ModVibrato n <- mods ]
          , n_hopo           = False -- I don't think you need this?
          , n_hammerOn       = elem ModHammerOn mods
          , n_pullOff        = elem ModPullOff mods
          , n_slideTo        = listToMaybe [ n | ModSlide n <- mods ]
          , n_slideUnpitchTo = listToMaybe [ n | ModSlideUnpitch n <- mods ]
          , n_mute           = elem ModMute mods
          , n_palmMute       = elem ModPalmMute mods
          , n_accent         = elem ModAccent mods
          , n_linkNext       = elem ModLink mods
          , n_bend           = listToMaybe [ n | ModBend n <- mods ]
          , n_bendValues     = V.empty -- TODO
          , n_harmonic       = elem ModHarmonic mods
          , n_harmonicPinch  = elem ModHarmonicPinch mods
          , n_leftHand       = Nothing -- TODO
          , n_rightHand      = listToMaybe [ fromEnum n | ModRightHand n <- mods ]
          , n_tap            = elem ModTap mods
          , n_slap           = listToMaybe [ n | ModSlap n <- mods ]
          , n_pluck          = listToMaybe [ n | ModPluck n <- mods ]
          , n_tremolo        = elem ModTremolo mods
          , n_pickDirection  = Nothing -- TODO
          , n_ignore         = elem ModIgnore mods
          }
