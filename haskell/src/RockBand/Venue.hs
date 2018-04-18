-- | The RB3 (RBN2) VENUE track format.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module RockBand.Venue where

import           RockBand.Codec.Venue (Camera3 (..), LightingShared (..),
                                       LightingSplit (..), PostProcess3 (..))
import           RockBand.Parse

data Event
  = Camera Camera3
  | SingalongGuitarKeys Bool
  | SingalongDrums Bool
  | SingalongBassKeys Bool
  | SpotlightKeys Bool
  | SpotlightVocal Bool
  | SpotlightGuitar Bool
  | SpotlightBass Bool
  | SpotlightDrums Bool
  | PostProcess PostProcess3
  | LightingShared LightingShared
  | LightingSplit LightingSplit
  | BonusFX
  | BonusFXOptional
  deriving (Eq, Ord, Show, Read)

instanceMIDIEvent [t| Event |] Nothing
  [ ( [e| one $ mapParseOne Camera parseCommand |]
    , [e| \case Camera m -> unparseCommand m |]
    )
  , edge 87 $ applyB [p| SingalongGuitarKeys |]
  , edge 86 $ applyB [p| SingalongDrums |]
  , edge 85 $ applyB [p| SingalongBassKeys |]
  , edge 41 $ applyB [p| SpotlightKeys |]
  , edge 40 $ applyB [p| SpotlightVocal |]
  , edge 39 $ applyB [p| SpotlightGuitar |]
  , edge 38 $ applyB [p| SpotlightDrums |] -- RBN2 docs incorrectly say this is bass
  , edge 37 $ applyB [p| SpotlightBass |] -- RBN2 docs incorrectly say this is drums
  , ( [e| one $ mapParseOne PostProcess parseCommand |]
    , [e| \case PostProcess m -> unparseCommand m |]
    )
  , ( [e| one $ mapParseOne LightingShared parseCommand |]
    , [e| \case LightingShared m -> unparseCommand m |]
    )
  , commandPair ["lighting", "(verse)"] [p| LightingSplit Lighting_verse |]
  , commandPair ["lighting", "(chorus)"] [p| LightingSplit Lighting_chorus |]
  , commandPair ["first"] [p| LightingSplit Lighting_first |]
  , commandPair ["prev"] [p| LightingSplit Lighting_prev |]
  , commandPair ["next"] [p| LightingSplit Lighting_next |]
  , commandPair ["bonusfx"] [p| BonusFX |]
  , commandPair ["bonusfx_optional"] [p| BonusFXOptional |]
  ]
