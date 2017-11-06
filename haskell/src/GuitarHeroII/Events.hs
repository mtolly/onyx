{- |
EVENTS
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GuitarHeroII.Events where

import           Control.Monad                    ((>=>))
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.Parse

data Event
  = CrowdHalfTempo
  | CrowdNormalTempo
  | CrowdFastTempo
  | CrowdLightersOff
  | CrowdLightersSlow
  | CrowdLightersFast
  | SyncWag
  | SyncHeadBang
  | BandJump
  | Verse
  | Chorus
  | Solo
  | Lighting Lighting
  | PracticeSection T.Text
  | MusicStart
  | End
  deriving (Eq, Ord, Show, Read)

data Lighting
  = Lighting_
  | Lighting_blackout
  | Lighting_chase
  | Lighting_flare
  | Lighting_strobe
  | Lighting_sweep
  | Lighting_color1
  | Lighting_color2
  -- TODO laid to rest has [lighting (f)]
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command Lighting where
  -- TODO Institutionalized and The Light That Blinds have [lighting(foo)] with no space
  fromCommand x = case T.stripPrefix "Lighting_" $ T.pack $ show x of
    Just s  -> ["lighting", "(" <> s <> ")"]
    Nothing -> error "panic! couldn't strip Lighting_ from venue event"
  toCommand = reverseLookup each fromCommand

instanceMIDIEvent [t| Event |] Nothing $

  [ commandPair ["verse"] [p| Verse |]
  , commandPair ["chorus"] [p| Chorus |]
  , commandPair ["solo"] [p| Solo |]
  , commandPair ["crowd_half_tempo"] [p| CrowdHalfTempo |]
  , commandPair ["crowd_normal_tempo"] [p| CrowdNormalTempo |]
  , commandPair ["crowd_fast_tempo"] [p| CrowdFastTempo |]
  , commandPair ["crowd_lighters_off"] [p| CrowdLightersOff |]
  , commandPair ["crowd_lighters_slow"] [p| CrowdLightersSlow |]
  , commandPair ["crowd_lighters_fast"] [p| CrowdLightersFast |]
  , commandPair ["sync_wag"] [p| SyncWag |]
  , commandPair ["sync_head_bang"] [p| SyncHeadBang |]
  , commandPair ["band_jump"] [p| BandJump |] -- yes we can has [sync_band_jump]
  , commandPair ["music_start"] [p| MusicStart |]
  , commandPair ["end"] [p| End |]

  , ( [e| one $ firstEventWhich $ readCommand' >=> \case
        ["section", s] -> Just $ PracticeSection s
        _              -> Nothing
      |]
    , [e| \case PracticeSection s -> RTB.singleton NNC.zero $ showCommand' ["section", s] |]
    )

  , ( [e| one $ mapParseOne Lighting parseCommand |]
    , [e| \case Lighting m -> unparseCommand m |]
    )

  ]
