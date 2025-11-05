{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StrictData          #-}
module Onyx.MIDI.Track.FiveFret where

import           Control.Monad                    (void, when)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (modify)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor                     ((<&>))
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
  { difficulties :: Map.Map Difficulty (FiveDifficulty t)
  , mood         :: RTB.T t Mood
  , handMap      :: RTB.T t HandMap
  , strumMap     :: RTB.T t StrumMap
  , fretPosition :: RTB.T t (FretPosition, Bool)
  , tremolo      :: RTB.T t (Maybe LaneDifficulty)
  , trill        :: RTB.T t (Maybe LaneDifficulty)
  , overdrive    :: RTB.T t Bool
  , bre          :: RTB.T t Bool
  , solo         :: RTB.T t Bool
  , player1      :: RTB.T t Bool
  , player2      :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (FiveTrack t)

nullFive :: FiveTrack t -> Bool
nullFive = all (RTB.null . (.gems)) . toList . (.difficulties)

instance TraverseTrack FiveTrack where
  traverseTrack fn (FiveTrack a b c d e f g h i j k l) = FiveTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h
    <*> fn i <*> fn j <*> fn k <*> fn l

instance ChopTrack FiveTrack where
  chopTake t ft = FiveTrack
    { difficulties = chopTake t <$>                ft.difficulties
    , mood         = U.trackTake t                 ft.mood
    , handMap      = U.trackTake t                 ft.handMap
    , strumMap     = U.trackTake t                 ft.strumMap
    , fretPosition = chopEachPair (chopTakeBool t) ft.fretPosition
    , tremolo      = chopTakeMaybe t               ft.tremolo
    , trill        = chopTakeMaybe t               ft.trill
    , overdrive    = chopTakeBool t                ft.overdrive
    , bre          = chopTakeBool t                ft.bre
    , solo         = chopTakeBool t                ft.solo
    , player1      = chopTakeBool t                ft.player1
    , player2      = chopTakeBool t                ft.player2
    }
  chopDrop t ft = FiveTrack
    { difficulties = chopDrop t <$> ft.difficulties
    , mood         = chopDropStatus t              ft.mood
    , handMap      = chopDropStatus t              ft.handMap
    , strumMap     = chopDropStatus t              ft.strumMap
    , fretPosition = chopEachPair (chopDropBool t) ft.fretPosition
    , tremolo      = chopDropMaybe t               ft.tremolo
    , trill        = chopDropMaybe t               ft.trill
    , overdrive    = chopDropBool t                ft.overdrive
    , bre          = chopDropBool t                ft.bre
    , solo         = chopDropBool t                ft.solo
    , player1      = chopDropBool t                ft.player1
    , player2      = chopDropBool t                ft.player2
    }

data FiveDifficulty t = FiveDifficulty
  { forceStrum :: RTB.T t Bool
  , forceHOPO  :: RTB.T t Bool
  , tap        :: RTB.T t Bool
  , open       :: RTB.T t Bool
  , gems       :: RTB.T t (Edge () (Maybe Color))
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (FiveDifficulty t)

instance ChopTrack FiveDifficulty where
  chopTake t fd = FiveDifficulty
    { forceStrum = chopTakeBool t fd.forceStrum
    , forceHOPO  = chopTakeBool t fd.forceHOPO
    , tap        = chopTakeBool t fd.tap
    , open       = chopTakeBool t fd.open
    , gems       = chopTakeEdge t fd.gems
    }
  chopDrop t fd = FiveDifficulty
    { forceStrum = chopDropBool t fd.forceStrum
    , forceHOPO  = chopDropBool t fd.forceHOPO
    , tap        = chopDropBool t fd.tap
    , open       = chopDropBool t fd.open
    , gems       = chopDropEdge t fd.gems
    }

instance TraverseTrack FiveDifficulty where
  traverseTrack fn (FiveDifficulty a b c d e) = FiveDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e

instance ParseTrack FiveTrack where
  parseTrack = do

    -- preprocess CH pitch 104 format for tap sections
    Codec
      { codecIn = lift $ modify \mt -> let
        p104 = fromMaybe RTB.empty $ Map.lookup (V.toPitch 104) mt.midiNotes
        in if RTB.null p104 then mt else let
          newPS = p104 <&> PS.PSMessage Nothing PS.TapNotes . \case
            EdgeOn {} -> True
            EdgeOff{} -> False
          in mt
            { midiPhaseShift = RTB.merge newPS mt.midiPhaseShift
            , midiNotes = Map.delete (V.toPitch 104) mt.midiNotes
            }
      , codecOut = const $ return ()
      }

    -- check for CH text event to determine whether to read new open note format on all difficulties
    enhancedOpens <- Codec
      { codecIn = do
        evts <- slurpTrack \mt -> let
          (brackets  , cmds'  ) = RTB.partition (== ["ENHANCED_OPENS"]) mt.midiCommands
          (noBrackets, lyrics') = RTB.partition (== "ENHANCED_OPENS") mt.midiLyrics
          mt' = mt
            { midiCommands = cmds'
            , midiLyrics   = lyrics'
            }
          in (RTB.merge (void brackets) (void noBrackets), mt')
        return $ not $ RTB.null evts
      , codecOut = \trk -> let
        hasEnhancedOpens = not $ null do
          diff <- Map.elems trk.difficulties
          e <- RTB.getBodies diff.gems
          Nothing <- toList e
          return ()
        in do
          when hasEnhancedOpens $ modify $ Wait 0 $ E.MetaEvent $ Meta.TextEvent "[ENHANCED_OPENS]"
          return hasEnhancedOpens
      }

    mood         <- (.mood      ) =. command
    handMap      <- (.handMap   ) =. command
    strumMap     <- (.strumMap  ) =. command
    fretPosition <- (=.) (.fretPosition) $ condenseMap $ eachKey each
      \posn -> case posn of
        Fret59 | enhancedOpens -> emptyCodec
        _                      -> edges $ fromEnum posn + 40
    tremolo      <- (.tremolo   ) =. edgesLanes 126
    trill        <- (.trill     ) =. edgesLanes 127
    overdrive    <- (.overdrive ) =. edges 116
    bre          <- (.bre       ) =. edgesBRE [120 .. 124]
    solo         <- (.solo      ) =. edges 103
    player1      <- (.player1   ) =. edges 105
    player2      <- (.player2   ) =. edges 106
    difficulties <- (=.) (.difficulties) $ eachKey each \diff -> fatBlips (1/8) do
      let base = case diff of
            Easy   -> 60
            Medium -> 72
            Hard   -> 84
            Expert -> 96
      forceStrum <- (.forceStrum) =. edges (base + 6)
      forceHOPO  <- (.forceHOPO ) =. edges (base + 5)
      tap        <- (.tap       ) =. sysexPS diff PS.TapNotes
      open       <- (.open      ) =. sysexPS diff PS.OpenStrum
      chordSnap [base - 1 .. base + 4]
      gems       <- (.gems      ) =. do
        translateEdges $ condenseMap $ eachKey (Nothing : map Just each) \case
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
