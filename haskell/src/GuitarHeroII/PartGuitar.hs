{- |
PART GUITAR, PART GUITAR COOP, PART BASS, PART RHYTHM
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GuitarHeroII.PartGuitar where

import           RockBand.Common
import           RockBand.FiveButton (Color (..), FretPosition (..))
import           RockBand.Parse

data Event
  = FretPosition FretPosition Bool
  | Idle
  | Play
  | HandMapDefault
  | HandMapDropD2
  | HandMapSolo
  | HandMapNoChords
  | StrumMapSlapBass
  | Wail Bool -- ^ headbang or similar. probably like [intense] in RB
  | Solo Bool -- ^ fire hands, special animations
  | OwFace Bool
  | HalfTempo
  | NormalTempo
  | DoubleTempo
  | Unknown110 Bool -- ^ some kind of sustain thing?
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show, Read)

data DiffEvent
  = StarPower Bool
  | Player1 Bool
  | Player2 Bool
  | Note (LongNote () Color)
  deriving (Eq, Ord, Show, Read)

instanceMIDIEvent [t| Event |] Nothing $

  -- TODO 38 and 39 seen in Institutionalized, Jessica, John the Fisherman
  [ edge 40 $ applyB [p| FretPosition Fret40 |]
  , edge 41 $ applyB [p| FretPosition Fret41 |]
  , edge 42 $ applyB [p| FretPosition Fret42 |]
  , edge 43 $ applyB [p| FretPosition Fret43 |]
  , edge 44 $ applyB [p| FretPosition Fret44 |]
  , edge 45 $ applyB [p| FretPosition Fret45 |]
  , edge 46 $ applyB [p| FretPosition Fret46 |]
  , edge 47 $ applyB [p| FretPosition Fret47 |]
  , edge 48 $ applyB [p| FretPosition Fret48 |]
  , edge 49 $ applyB [p| FretPosition Fret49 |]
  , edge 50 $ applyB [p| FretPosition Fret50 |]
  , edge 51 $ applyB [p| FretPosition Fret51 |]
  , edge 52 $ applyB [p| FretPosition Fret52 |]
  , edge 53 $ applyB [p| FretPosition Fret53 |]
  , edge 54 $ applyB [p| FretPosition Fret54 |]
  , edge 55 $ applyB [p| FretPosition Fret55 |]
  , edge 56 $ applyB [p| FretPosition Fret56 |]
  , edge 57 $ applyB [p| FretPosition Fret57 |]
  , edge 58 $ applyB [p| FretPosition Fret58 |]
  , edge 59 $ applyB [p| FretPosition Fret59 |]

  ] ++ noteParser 60 [p| Green |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 61 [p| Red |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 62 [p| Yellow |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 63 [p| Blue |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 64 [p| Orange |] (\p -> [p| DiffEvent Easy (Note $p) |]) ++
  [ edge 67 $ \_b -> [p| DiffEvent Easy (StarPower $(boolP _b)) |]
  , edge 69 $ \_b -> [p| DiffEvent Easy (Player1   $(boolP _b)) |]
  , edge 70 $ \_b -> [p| DiffEvent Easy (Player2   $(boolP _b)) |]

  ] ++ noteParser 72 [p| Green |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 73 [p| Red |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 74 [p| Yellow |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 75 [p| Blue |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 76 [p| Orange |] (\p -> [p| DiffEvent Medium (Note $p) |]) ++
  [ edge 79 $ \_b -> [p| DiffEvent Medium (StarPower $(boolP _b)) |]
  , edge 81 $ \_b -> [p| DiffEvent Medium (Player1   $(boolP _b)) |]
  , edge 82 $ \_b -> [p| DiffEvent Medium (Player2   $(boolP _b)) |]

  ] ++ noteParser 84 [p| Green |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 85 [p| Red |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 86 [p| Yellow |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 87 [p| Blue |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 88 [p| Orange |] (\p -> [p| DiffEvent Hard (Note $p) |]) ++
  [ edge 91 $ \_b -> [p| DiffEvent Hard (StarPower $(boolP _b)) |]
  , edge 93 $ \_b -> [p| DiffEvent Hard (Player1   $(boolP _b)) |]
  , edge 94 $ \_b -> [p| DiffEvent Hard (Player2   $(boolP _b)) |]

  ] ++ noteParser 96 [p| Green |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 97 [p| Red |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 98 [p| Yellow |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 99 [p| Blue |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 100 [p| Orange |] (\p -> [p| DiffEvent Expert (Note $p) |]) ++
  [ edge 103 $ \_b -> [p| DiffEvent Expert (StarPower $(boolP _b)) |]
  , edge 105 $ \_b -> [p| DiffEvent Expert (Player1   $(boolP _b)) |]
  , edge 106 $ \_b -> [p| DiffEvent Expert (Player2   $(boolP _b)) |]

  , edge 110 $ applyB [p| Unknown110 |]

  -- Strutter has 119 on PART RHYTHM and BAND BASS

  , commandPair ["idle"] [p| Idle |]
  , commandPair ["play"] [p| Play |]
  , commandPair ["wail_on"] [p| Wail True |]
  , commandPair ["wail_off"] [p| Wail False |]
  , commandPair ["solo_on"] [p| Solo True |]
  , commandPair ["solo_off"] [p| Solo False |]
  , commandPair ["ow_face_on"] [p| OwFace True |]
  , commandPair ["ow_face_off"] [p| OwFace False |]
  , commandPair ["half_tempo"] [p| HalfTempo |]
  , commandPair ["normal_tempo"] [p| NormalTempo |]
  , commandPair ["double_tempo"] [p| DoubleTempo |]
  , commandPair ["map", "HandMap_Default"] [p| HandMapDefault |]
  , commandPair ["map", "HandMap_DropD2"] [p| HandMapDropD2 |]
  , commandPair ["map", "HandMap_Solo"] [p| HandMapSolo |]
  , commandPair ["map", "HandMap_NoChords"] [p| HandMapNoChords |]
  , commandPair ["map", "StrumMap_SlapBass"] [p| StrumMapSlapBass |]

  ]
