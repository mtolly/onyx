{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}
module Onyx.MIDI.Track.FiveFret where

import           Control.Monad                    (void, when)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (modify)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.Codec.Common                (emptyCodec)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import qualified Onyx.PhaseShift.Message          as PS
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util                  as U

data Color = Green | Red | Yellow | Blue | Orange
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | These don't actually correspond to 20 different frets;
-- see <http://i.imgur.com/fRg6Vo9.png> by Orange Harrison
data FretPosition
  = Fret40 -- ^ the nut
  | Fret41
  | Fret42
  | Fret43
  | Fret44
  | Fret45
  | Fret46
  | Fret47
  | Fret48
  | Fret49
  | Fret50
  | Fret51
  | Fret52
  | Fret53
  | Fret54
  | Fret55
  | Fret56
  | Fret57
  | Fret58
  | Fret59 -- ^ roughly fret 12
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Controls the fretting hand animation of a guitarist/bassist.
data HandMap
  = HandMap_Default
  -- ^ Normal fingering. Single gems = single fingers, gems with duration =
  -- vibrato, chord gems = chords.
  | HandMap_NoChords  -- ^ All single fingers/vibrato.
  | HandMap_AllChords -- ^ All chords.
  | HandMap_Solo      -- ^ D major shape for all chords, vibrato for all chord sustains.
  | HandMap_DropD     -- ^ Open hand for all green gems, all other gems are chords.
  | HandMap_DropD2    -- ^ Open hand for all green gems.
  | HandMap_AllBend   -- ^ All ring finger high vibrato.
  | HandMap_Chord_C   -- ^ All C chord shape.
  | HandMap_Chord_D   -- ^ All D chord shape.
  | HandMap_Chord_A   -- ^ All A minor chord shape.
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Command HandMap where
  fromCommand hm = ["map", T.pack $ show hm]
  toCommand = reverseLookup each fromCommand

-- | Controls the strumming animation for a bassist.
data StrumMap
  = StrumMap_Default
  | StrumMap_Pick
  | StrumMap_SlapBass
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Command StrumMap where
  fromCommand sm = ["map", T.pack $ show sm]
  toCommand = reverseLookup each fromCommand

data FiveTrack t = FiveTrack
  { fiveDifficulties :: Map.Map Difficulty (FiveDifficulty t)
  , fiveMood         :: RTB.T t Mood
  , fiveHandMap      :: RTB.T t HandMap
  , fiveStrumMap     :: RTB.T t StrumMap
  , fiveFretPosition :: RTB.T t (FretPosition, Bool)
  , fiveTremolo      :: RTB.T t (Maybe LaneDifficulty)
  , fiveTrill        :: RTB.T t (Maybe LaneDifficulty)
  , fiveOverdrive    :: RTB.T t Bool
  , fiveBRE          :: RTB.T t Bool
  , fiveSolo         :: RTB.T t Bool
  , fivePlayer1      :: RTB.T t Bool
  , fivePlayer2      :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (FiveTrack t)

nullFive :: FiveTrack t -> Bool
nullFive = all (RTB.null . (.fiveGems)) . toList . (.fiveDifficulties)

instance TraverseTrack FiveTrack where
  traverseTrack fn (FiveTrack a b c d e f g h i j k l) = FiveTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h
    <*> fn i <*> fn j <*> fn k <*> fn l

instance ChopTrack FiveTrack where
  chopTake t ft = FiveTrack
    { fiveDifficulties = chopTake t <$> ft.fiveDifficulties
    , fiveMood         = U.trackTake t                 ft.fiveMood
    , fiveHandMap      = U.trackTake t                 ft.fiveHandMap
    , fiveStrumMap     = U.trackTake t                 ft.fiveStrumMap
    , fiveFretPosition = chopEachPair (chopTakeBool t) ft.fiveFretPosition
    , fiveTremolo      = chopTakeMaybe t               ft.fiveTremolo
    , fiveTrill        = chopTakeMaybe t               ft.fiveTrill
    , fiveOverdrive    = chopTakeBool t                ft.fiveOverdrive
    , fiveBRE          = chopTakeBool t                ft.fiveBRE
    , fiveSolo         = chopTakeBool t                ft.fiveSolo
    , fivePlayer1      = chopTakeBool t                ft.fivePlayer1
    , fivePlayer2      = chopTakeBool t                ft.fivePlayer2
    }
  chopDrop t ft = FiveTrack
    { fiveDifficulties = chopDrop t <$> ft.fiveDifficulties
    , fiveMood         = chopDropStatus t              ft.fiveMood
    , fiveHandMap      = chopDropStatus t              ft.fiveHandMap
    , fiveStrumMap     = chopDropStatus t              ft.fiveStrumMap
    , fiveFretPosition = chopEachPair (chopDropBool t) ft.fiveFretPosition
    , fiveTremolo      = chopDropMaybe t               ft.fiveTremolo
    , fiveTrill        = chopDropMaybe t               ft.fiveTrill
    , fiveOverdrive    = chopDropBool t                ft.fiveOverdrive
    , fiveBRE          = chopDropBool t                ft.fiveBRE
    , fiveSolo         = chopDropBool t                ft.fiveSolo
    , fivePlayer1      = chopDropBool t                ft.fivePlayer1
    , fivePlayer2      = chopDropBool t                ft.fivePlayer2
    }

data FiveDifficulty t = FiveDifficulty
  { fiveForceStrum :: RTB.T t Bool
  , fiveForceHOPO  :: RTB.T t Bool
  , fiveTap        :: RTB.T t Bool
  , fiveOpen       :: RTB.T t Bool
  , fiveGems       :: RTB.T t (Edge () (Maybe Color))
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (FiveDifficulty t)

instance ChopTrack FiveDifficulty where
  chopTake t fd = FiveDifficulty
    { fiveForceStrum = chopTakeBool t fd.fiveForceStrum
    , fiveForceHOPO  = chopTakeBool t fd.fiveForceHOPO
    , fiveTap        = chopTakeBool t fd.fiveTap
    , fiveOpen       = chopTakeBool t fd.fiveOpen
    , fiveGems       = chopTakeEdge t fd.fiveGems
    }
  chopDrop t fd = FiveDifficulty
    { fiveForceStrum = chopDropBool t fd.fiveForceStrum
    , fiveForceHOPO  = chopDropBool t fd.fiveForceHOPO
    , fiveTap        = chopDropBool t fd.fiveTap
    , fiveOpen       = chopDropBool t fd.fiveOpen
    , fiveGems       = chopDropEdge t fd.fiveGems
    }

instance TraverseTrack FiveDifficulty where
  traverseTrack fn (FiveDifficulty a b c d e) = FiveDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e

instance ParseTrack FiveTrack where
  parseTrack = do

    -- preprocess CH pitch 104 format for tap sections
    Codec
      { codecIn = lift $ modify $ \mt -> let
        p104 = fromMaybe RTB.empty $ Map.lookup (V.toPitch 104) (midiNotes mt)
        in if RTB.null p104 then mt else let
          newPS = flip fmap p104 $ PS.PSMessage Nothing PS.TapNotes . \case
            EdgeOn {} -> True
            EdgeOff{} -> False
          in mt
            { midiPhaseShift = RTB.merge newPS $ midiPhaseShift mt
            , midiNotes = Map.delete (V.toPitch 104) $ midiNotes mt
            }
      , codecOut = const $ return ()
      }

    -- check for CH text event to determine whether to read new open note format on all difficulties
    enhancedOpens <- Codec
      { codecIn = do
        evts <- slurpTrack $ \mt -> let
          (brackets  , cmds'  ) = RTB.partition (== ["ENHANCED_OPENS"]) $ midiCommands mt
          (noBrackets, lyrics') = RTB.partition (== "ENHANCED_OPENS") $ midiLyrics mt
          mt' = mt
            { midiCommands = cmds'
            , midiLyrics   = lyrics'
            }
          in (RTB.merge (void brackets) (void noBrackets), mt')
        return $ not $ RTB.null evts
      , codecOut = \trk -> let
        hasEnhancedOpens = not $ null $ do
          diff <- Map.elems trk.fiveDifficulties
          e <- RTB.getBodies diff.fiveGems
          Nothing <- toList e
          return ()
        in do
          when hasEnhancedOpens $ modify $ Wait 0 $ E.MetaEvent $ Meta.TextEvent "[ENHANCED_OPENS]"
          return hasEnhancedOpens
      }

    fiveMood         <- (.fiveMood      ) =. command
    fiveHandMap      <- (.fiveHandMap   ) =. command
    fiveStrumMap     <- (.fiveStrumMap  ) =. command
    fiveFretPosition <- (=.) (.fiveFretPosition) $ condenseMap $ eachKey each
      $ \posn -> case posn of
        Fret59 | enhancedOpens -> emptyCodec
        _                      -> edges $ fromEnum posn + 40
    fiveTremolo      <- (.fiveTremolo   ) =. edgesLanes 126
    fiveTrill        <- (.fiveTrill     ) =. edgesLanes 127
    fiveOverdrive    <- (.fiveOverdrive ) =. edges 116
    fiveBRE          <- (.fiveBRE       ) =. edgesBRE [120 .. 124]
    fiveSolo         <- (.fiveSolo      ) =. edges 103
    fivePlayer1      <- (.fivePlayer1   ) =. edges 105
    fivePlayer2      <- (.fivePlayer2   ) =. edges 106
    fiveDifficulties <- (=.) (.fiveDifficulties) $ eachKey each $ \diff -> fatBlips (1/8) $ do
      let base = case diff of
            Easy   -> 60
            Medium -> 72
            Hard   -> 84
            Expert -> 96
      fiveForceStrum <- (.fiveForceStrum) =. edges (base + 6)
      fiveForceHOPO  <- (.fiveForceHOPO ) =. edges (base + 5)
      fiveTap        <- (.fiveTap       ) =. sysexPS diff PS.TapNotes
      fiveOpen       <- (.fiveOpen      ) =. sysexPS diff PS.OpenStrum
      chordSnap [base - 1 .. base + 4]
      fiveGems       <- (.fiveGems      ) =. do
        translateEdges $ condenseMap $ eachKey (Nothing : map Just each) $ \case
          -- Always support pitch 95 opens for expert since I am already using it in my midis
          Nothing -> if enhancedOpens || diff == Expert
            then edges $ base - 1
            else emptyCodec
          Just Green  -> edges $ base + 0
          Just Red    -> edges $ base + 1
          Just Yellow -> edges $ base + 2
          Just Blue   -> edges $ base + 3
          Just Orange -> edges $ base + 4
      return FiveDifficulty{..}
    return FiveTrack{..}

smoothFretPosition :: (NNC.C t, Fractional t) => RTB.T t (FretPosition, Bool) -> RTB.T t (FretPosition, Bool)
smoothFretPosition = \case
  Wait t1 (p1, False) (Wait t2 (p2, True) rest) | t2 > 0 -> let
    steps = case compare p1 p2 of
      EQ -> [p1]
      LT -> if succ p1 == p2
        then [p1]
        else [succ p1 .. pred p2]
      GT -> if pred p1 == p2
        then [p1]
        else [pred p1, pred (pred p1) .. succ p2]
    stepDuration = t2 / fromIntegral (length steps)
    in Wait t1 (p1, False)
      $ foldr ($) (Wait 0 (p2, True) $ smoothFretPosition rest)
      $ map (\fret -> Wait 0 (fret, True) . Wait stepDuration (fret, False)) steps
  Wait t pb rest -> Wait t pb $ smoothFretPosition rest
  RNil -> RNil
