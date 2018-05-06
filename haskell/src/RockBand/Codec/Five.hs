{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.Codec.Five where

import           Control.Monad                    (guard, (>=>))
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (modify)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           Data.Maybe                       (isJust)
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common
import qualified RockBand.PhaseShiftMessage       as PS
import           Text.Read                        (readMaybe)

data Color = Green | Red | Yellow | Blue | Orange
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | These don't actually correspond to 20 different frets;
-- see http://i.imgur.com/fRg6Vo9.png by Orange Harrison
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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command HandMap where
  fromCommand hm = ["map", T.pack $ show hm]
  toCommand = reverseLookup each fromCommand

-- | Controls the strumming animation for a bassist.
data StrumMap
  = StrumMap_Default
  | StrumMap_Pick
  | StrumMap_SlapBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command StrumMap where
  fromCommand sm = ["map", T.pack $ show sm]
  toCommand = reverseLookup each fromCommand

data OnyxCloseEvent = OnyxCloseEvent Difficulty Int

instance Command OnyxCloseEvent where
  fromCommand (OnyxCloseEvent diff offset) =
    ["onyx", "close", T.toLower $ T.pack $ show diff, T.pack $ show offset]
  toCommand cmd = do
    ["onyx", "close", d, n] <- Just cmd
    diff <- reverseLookup each (T.toLower . T.pack . show) d
    offset <- readMaybe $ T.unpack n
    return $ OnyxCloseEvent diff offset

data FiveTrack t = FiveTrack
  { fiveDifficulties :: Map.Map Difficulty (FiveDifficulty t)
  , fiveMood         :: RTB.T t Mood
  , fiveHandMap      :: RTB.T t HandMap
  , fiveStrumMap     :: RTB.T t StrumMap
  , fiveFretPosition :: RTB.T t (FretPosition, Bool)
  , fiveTremolo      :: RTB.T t Bool
  , fiveTrill        :: RTB.T t Bool
  , fiveOverdrive    :: RTB.T t Bool
  , fiveBRE          :: RTB.T t Bool
  , fiveSolo         :: RTB.T t Bool
  , fivePlayer1      :: RTB.T t Bool
  , fivePlayer2      :: RTB.T t Bool
  } deriving (Eq, Ord, Show)

nullFive :: FiveTrack t -> Bool
nullFive = all (RTB.null . fiveGems) . toList . fiveDifficulties

instance (NNC.C t) => Monoid (FiveTrack t) where
  mempty = FiveTrack Map.empty RTB.empty
    RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
    RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
  mappend
    (FiveTrack a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
    (FiveTrack b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12)
    = FiveTrack
      (Map.unionWith mappend a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)
      (RTB.merge a5 b5)
      (RTB.merge a6 b6)
      (RTB.merge a7 b7)
      (RTB.merge a8 b8)
      (RTB.merge a9 b9)
      (RTB.merge a10 b10)
      (RTB.merge a11 b11)
      (RTB.merge a12 b12)

instance TraverseTrack FiveTrack where
  traverseTrack fn (FiveTrack a b c d e f g h i j k l) = FiveTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h
    <*> fn i <*> fn j <*> fn k <*> fn l

data FiveDifficulty t = FiveDifficulty
  { fiveForceStrum :: RTB.T t Bool
  , fiveForceHOPO  :: RTB.T t Bool
  , fiveTap        :: RTB.T t Bool
  , fiveOpen       :: RTB.T t Bool
  , fiveOnyxClose  :: RTB.T t Int
  , fiveGems       :: RTB.T t (Color, Maybe t)
  } deriving (Eq, Ord, Show)

instance TraverseTrack FiveDifficulty where
  traverseTrack fn (FiveDifficulty a b c d e f) = FiveDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> traverseBlipSustain fn f

instance (NNC.C t) => Monoid (FiveDifficulty t) where
  mempty = FiveDifficulty RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
  mappend
    (FiveDifficulty a1 a2 a3 a4 a5 a6)
    (FiveDifficulty b1 b2 b3 b4 b5 b6)
    = FiveDifficulty
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)
      (RTB.merge a5 b5)
      (RTB.merge a6 b6)

instance ParseTrack FiveTrack where
  parseTrack = do
    -- preprocess pitch 95 as a shortcut for 96 + open note sysex
    Codec
      { codecIn = let
        p95 x = case isNoteEdgeCPV x of
          Just (c, 95, v) ->
            [ makeEdgeCPV c 96 v
            , PS.unparsePSSysEx $ PS.PSMessage (Just Expert) PS.OpenStrum $ isJust v
            ]
          _ -> [x]
        in lift $ modify $ RTB.flatten . fmap p95
      , codecOut = const $ return ()
      }
    fiveMood         <- fiveMood         =. command
    fiveHandMap      <- fiveHandMap      =. command
    fiveStrumMap     <- fiveStrumMap     =. command
    fiveFretPosition <- (fiveFretPosition =.) $ condenseMap $ eachKey each
      $ \posn -> edges $ fromEnum posn + 40
    fiveTremolo      <- fiveTremolo      =. edges 126
    fiveTrill        <- fiveTrill        =. edges 127
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
      fiveOpen       <- fiveOpen       =. sysexPS diff PS.OpenStrum
      fiveOnyxClose  <- fiveOnyxClose  =. let
        parse = readCommand' >=> \(OnyxCloseEvent diff' n) -> guard (diff == diff') >> Just n
        unparse n = showCommand' $ OnyxCloseEvent diff n
        in single parse unparse
      fiveGems       <- (fiveGems =.) $ blipSustainRB $ condenseMap $ eachKey each $ matchEdges . edges . \case
        Green  -> base + 0
        Red    -> base + 1
        Yellow -> base + 2
        Blue   -> base + 3
        Orange -> base + 4
      return FiveDifficulty{..}
    return FiveTrack{..}
