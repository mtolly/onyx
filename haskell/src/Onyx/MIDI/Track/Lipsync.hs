{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Onyx.MIDI.Track.Lipsync where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Text                        as T
import           Data.Word
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.Vocal            (Pitch)
import           Text.Read                        (readMaybe)

data LipsyncTrack t = LipsyncTrack
  { lipLyrics   :: RTB.T t T.Text
  , lipNotes    :: RTB.T t (Pitch, Bool)
  , lipLanguage :: RTB.T t LyricLanguage
  , lipEvents   :: RTB.T t (VisemeEvent VisemeGraph T.Text)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (LipsyncTrack t)

instance TraverseTrack LipsyncTrack where
  traverseTrack fn (LipsyncTrack a b c d) = LipsyncTrack
    <$> fn a <*> fn b <*> fn c <*> fn d

instance ParseTrack LipsyncTrack where
  parseTrack = do
    lipLyrics <- lipLyrics =. lyrics
    lipNotes <- (lipNotes =.)
      $ condenseMap $ eachKey each $ edges . (+ 36) . fromEnum
    lipLanguage <- (lipLanguage =.)
      $ condenseMap_ $ eachKey each $ commandMatch . \case
        LyricEnglish -> ["lang", "en"]
        LyricGerman  -> ["lang", "de"]
        LyricSpanish -> ["lang", "es"]
    lipEvents <- lipEvents =. command
    return LipsyncTrack{..}

data LyricLanguage
  = LyricEnglish
  | LyricGerman
  | LyricSpanish
  deriving (Eq, Ord, Show, Enum, Bounded)

data VisemeGraph
  = GraphHold -- ^ hold this weight until its next event
  | GraphLinear -- ^ linear transition from this weight to the next one
  | GraphEaseInExpo -- ^ exponential transition from this weight to the next one
  deriving (Eq, Ord, Show)

data VisemeEvent g a = VisemeEvent
  { visemeKey    :: a
  , visemeWeight :: Word8
  , visemeGraph  :: g
  } deriving (Eq, Ord, Show, Functor)

instance Command (VisemeEvent VisemeGraph T.Text) where
  toCommand = \case
    v : w : rest -> VisemeEvent v <$> readMaybe (T.unpack w) <*> case rest of
      []       -> Just GraphLinear
      ["hold"] -> Just GraphHold
      ["ease"] -> Just GraphEaseInExpo
      _        -> Nothing
    _      -> Nothing
  fromCommand (VisemeEvent v w g) = v : T.pack (show w) : case g of
    GraphLinear     -> []
    GraphHold       -> ["hold"]
    GraphEaseInExpo -> ["ease"]

data MagmaViseme
  = Viseme_Blink
  | Viseme_Brow_aggressive
  | Viseme_Brow_down
  | Viseme_Brow_pouty
  | Viseme_Brow_up
  | Viseme_Bump_hi
  | Viseme_Bump_lo
  | Viseme_Cage_hi
  | Viseme_Cage_lo
  | Viseme_Church_hi
  | Viseme_Church_lo
  | Viseme_Earth_hi
  | Viseme_Earth_lo
  | Viseme_Eat_hi
  | Viseme_Eat_lo
  | Viseme_Fave_hi
  | Viseme_Fave_lo
  | Viseme_If_hi
  | Viseme_If_lo
  | Viseme_New_hi
  | Viseme_New_lo
  | Viseme_Oat_hi
  | Viseme_Oat_lo
  | Viseme_Ox_hi
  | Viseme_Ox_lo
  | Viseme_Roar_hi
  | Viseme_Roar_lo
  | Viseme_Size_hi
  | Viseme_Size_lo
  | Viseme_Squint
  | Viseme_Though_hi
  | Viseme_Though_lo
  | Viseme_Told_hi
  | Viseme_Told_lo
  | Viseme_Wet_hi
  | Viseme_Wet_lo
  deriving (Eq, Ord, Show, Enum, Bounded)

data BeatlesViseme
  = Viseme_head_rot_neg_x
  | Viseme_head_rot_neg_y
  | Viseme_head_rot_neg_z
  | Viseme_head_rot_pos_x
  | Viseme_head_rot_pos_y
  | Viseme_head_rot_pos_z
  | Viseme_jaw_fwd
  | Viseme_jaw_left
  | Viseme_jaw_open
  | Viseme_jaw_right
  | Viseme_l_brow_dn
  | Viseme_l_brow_up
  | Viseme_l_cheek_puff
  | Viseme_l_frown
  | Viseme_l_lids
  | Viseme_l_lip_pull
  | Viseme_l_lolid_up
  | Viseme_l_lolip_dn
  | Viseme_l_lolip_roll
  | Viseme_l_lolip_up
  | Viseme_l_mouth_pucker
  | Viseme_l_open_pucker
  | Viseme_l_smile_closed
  | Viseme_l_smile_open
  | Viseme_l_sneer_narrow
  | Viseme_l_squint
  | Viseme_l_uplip_roll
  | Viseme_l_uplip_up
  | Viseme_m_brow_dn
  | Viseme_m_brow_up
  | Viseme_m_lips_close
  | Viseme_r_brow_dn
  | Viseme_r_brow_up
  | Viseme_r_cheek_puff
  | Viseme_r_frown
  | Viseme_r_lids
  | Viseme_r_lip_pull
  | Viseme_r_lolid_up
  | Viseme_r_lolip_dn
  | Viseme_r_lolip_roll
  | Viseme_r_lolip_up
  | Viseme_r_mouth_pucker
  | Viseme_r_open_pucker
  | Viseme_r_smile_closed
  | Viseme_r_smile_open
  | Viseme_r_sneer_narrow
  | Viseme_r_squint
  | Viseme_r_uplip_roll
  | Viseme_r_uplip_up
  | Viseme_tongue_dn
  | Viseme_tongue_out
  | Viseme_tongue_roll
  | Viseme_tongue_up
  deriving (Eq, Ord, Show, Enum, Bounded)

data GH2Viseme
  = GH2_Eat
  | GH2_If
  | GH2_Ox
  | GH2_Oat
  | GH2_Earth
  | GH2_Size
  | GH2_Church
  | GH2_Fave
  | GH2_Though
  | GH2_Bump
  | GH2_New
  | GH2_Told
  | GH2_Roar
  | GH2_Wet
  | GH2_Cage
  | GH2_Orientation_Head_Pitch
  | GH2_Orientation_Head_Roll
  | GH2_Orientation_Head_Yaw
  | GH2_Gaze_Eye_Pitch
  | GH2_Gaze_Eye_Yaw
  | GH2_Emphasis_Head_Pitch
  | GH2_Emphasis_Head_Roll
  | GH2_Emphasis_Head_Yaw
  | GH2_Eyebrow_Raise
  | GH2_Blink
  deriving (Eq, Ord, Show, Enum, Bounded)
