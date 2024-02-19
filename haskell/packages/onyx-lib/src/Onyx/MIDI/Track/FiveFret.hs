{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module Onyx.MIDI.Track.FiveFret where

import           Control.Monad                    (void)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (modify)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isNothing)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import qualified Onyx.PhaseShift.Message          as PS
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
nullFive = all (RTB.null . fiveGems) . toList . fiveDifficulties

instance TraverseTrack FiveTrack where
  traverseTrack fn (FiveTrack a b c d e f g h i j k l) = FiveTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h
    <*> fn i <*> fn j <*> fn k <*> fn l

instance ChopTrack FiveTrack where
  chopTake t ft = FiveTrack
    { fiveDifficulties = chopTake t <$> fiveDifficulties ft
    , fiveMood         = U.trackTake t                 $ fiveMood ft
    , fiveHandMap      = U.trackTake t                 $ fiveHandMap ft
    , fiveStrumMap     = U.trackTake t                 $ fiveStrumMap ft
    , fiveFretPosition = chopEachPair (chopTakeBool t) $ fiveFretPosition ft
    , fiveTremolo      = chopTakeMaybe t               $ fiveTremolo ft
    , fiveTrill        = chopTakeMaybe t               $ fiveTrill ft
    , fiveOverdrive    = chopTakeBool t                $ fivePlayer2 ft
    , fiveBRE          = chopTakeBool t                $ fivePlayer2 ft
    , fiveSolo         = chopTakeBool t                $ fivePlayer2 ft
    , fivePlayer1      = chopTakeBool t                $ fivePlayer1 ft
    , fivePlayer2      = chopTakeBool t                $ fivePlayer2 ft
    }
  chopDrop t ft = FiveTrack
    { fiveDifficulties = chopDrop t <$> fiveDifficulties ft
    , fiveMood         = chopDropStatus t              $ fiveMood ft
    , fiveHandMap      = chopDropStatus t              $ fiveHandMap ft
    , fiveStrumMap     = chopDropStatus t              $ fiveStrumMap ft
    , fiveFretPosition = chopEachPair (chopDropBool t) $ fiveFretPosition ft
    , fiveTremolo      = chopDropMaybe t               $ fiveTremolo ft
    , fiveTrill        = chopDropMaybe t               $ fiveTrill ft
    , fiveOverdrive    = chopDropBool t                $ fivePlayer2 ft
    , fiveBRE          = chopDropBool t                $ fivePlayer2 ft
    , fiveSolo         = chopDropBool t                $ fivePlayer2 ft
    , fivePlayer1      = chopDropBool t                $ fivePlayer1 ft
    , fivePlayer2      = chopDropBool t                $ fivePlayer2 ft
    }

data FiveDifficulty t = FiveDifficulty
  { fiveForceStrum :: RTB.T t Bool
  , fiveForceHOPO  :: RTB.T t Bool
  , fiveTap        :: RTB.T t Bool
  , fiveOpen       :: RTB.T t Bool
  , fiveGems       :: RTB.T t (Edge () Color)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (FiveDifficulty t)

instance ChopTrack FiveDifficulty where
  chopTake t fd = FiveDifficulty
    { fiveForceStrum = chopTakeBool t $ fiveForceStrum fd
    , fiveForceHOPO  = chopTakeBool t $ fiveForceHOPO  fd
    , fiveTap        = chopTakeBool t $ fiveTap        fd
    , fiveOpen       = chopTakeBool t $ fiveOpen       fd
    , fiveGems       = chopTakeEdge t $ fiveGems       fd
    }
  chopDrop t fd = FiveDifficulty
    { fiveForceStrum = chopDropBool t $ fiveForceStrum fd
    , fiveForceHOPO  = chopDropBool t $ fiveForceHOPO  fd
    , fiveTap        = chopDropBool t $ fiveTap        fd
    , fiveOpen       = chopDropBool t $ fiveOpen       fd
    , fiveGems       = chopDropEdge t $ fiveGems       fd
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
      , codecOut = const $ return False
      }

    fiveMood         <- fiveMood         =. command
    fiveHandMap      <- fiveHandMap      =. command
    fiveStrumMap     <- fiveStrumMap     =. command
    fiveFretPosition <- (fiveFretPosition =.) $ condenseMap $ eachKey each
      $ \posn -> edges $ fromEnum posn + 40
      -- TODO does this need to check enhancedOpens to ignore high fret
    fiveTremolo      <- fiveTremolo      =. edgesLanes 126
    fiveTrill        <- fiveTrill        =. edgesLanes 127
    fiveOverdrive    <- fiveOverdrive    =. edges 116
    fiveBRE          <- fiveBRE          =. edgesBRE [120 .. 124]
    fiveSolo         <- fiveSolo         =. edges 103
    fivePlayer1      <- fivePlayer1      =. edges 105
    fivePlayer2      <- fivePlayer2      =. edges 106
    fiveDifficulties <- (fiveDifficulties =.) $ eachKey each $ \diff -> fatBlips (1/8) $ do
      let base = case diff of
            Easy   -> 60
            Medium -> 72
            Hard   -> 84
            Expert -> 96
      fiveForceStrum <- fiveForceStrum =. edges (base + 6)
      fiveForceHOPO  <- fiveForceHOPO  =. edges (base + 5)
      fiveTap        <- fiveTap        =. sysexPS diff PS.TapNotes
      fiveOpen'      <- fiveOpen       =. sysexPS diff PS.OpenStrum
      chordSnap [base - 1 .. base + 4]
      fiveGems'      <- (fiveGems =.) $ translateEdges $ condenseMap $ eachKey each $ edges . \case
        Green  -> base + 0
        Red    -> base + 1
        Yellow -> base + 2
        Blue   -> base + 3
        Orange -> base + 4
      -- Always support pitch 95 opens for expert since I am already using it in my midis
      (fiveGems, fiveOpen) <- if enhancedOpens || diff == Expert
        then do
          opens <- Codec
            { codecIn  = codecIn $ matchEdges (edges $ base - 1)
            , codecOut = const $ return RTB.empty
            }
          if RTB.null opens
            then return (fiveGems', fiveOpen')
            else let
              -- Need to pull back the opens to not overlap with any other notes
              pulledBack = splitEdgesBool
                $ fmap snd
                $ RTB.filter (\(color, _) -> isNothing color)
                $ noOpenExtSustainsSimple $ RTB.merge
                  ((Nothing,) <$> opens)
                  (RTB.mapMaybe (\case EdgeOn () color -> Just (Just color, 0); _ -> Nothing) fiveGems')
              newGems = (\b -> if b then EdgeOn () Green else EdgeOff Green) <$> pulledBack
              in return (RTB.merge fiveGems' newGems, RTB.merge fiveOpen' pulledBack)
        else return (fiveGems', fiveOpen')
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

-- Doesn't care about blips vs sustains, just pulls back CH-format opens to next note-on
noOpenExtSustainsSimple :: (NNC.C t, Num t) => RTB.T t (Maybe color, t) -> RTB.T t (Maybe color, t)
noOpenExtSustainsSimple = let
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, [(Nothing, len1)]), rtb') -> let
      len2 = case RTB.viewL rtb' of
        Nothing            -> len1
        Just ((dt', _), _) -> min dt' len1
      in RTB.cons dt [(Nothing, len2)] $ go rtb'
    Just ((dt, xs), rtb') -> RTB.cons dt xs $ go rtb'
  in RTB.flatten . go . RTB.collectCoincident
