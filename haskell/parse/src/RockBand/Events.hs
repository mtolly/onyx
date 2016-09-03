{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module RockBand.Events where

import           Control.Monad                    ((>=>))
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (stripPrefix)
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.Parse
import           RockBand.Sections                (makeRB3Section)

data Event
  = MusicStart
  | MusicEnd
  | End
  | Coda
  | CrowdRealtime
  | CrowdIntense
  | CrowdNormal
  | CrowdMellow
  | CrowdNoclap
  | CrowdClap
  | PracticeSection T.Text
  | PracticeKick
  | PracticeSnare
  | PracticeHihat
  deriving (Eq, Ord, Show, Read)

instanceMIDIEvent [t| Event |]

  [ commandPair ["music_start"] [p| MusicStart |]
  , commandPair ["music_end"] [p| MusicEnd |]
  , commandPair ["end"] [p| End |]
  , commandPair ["coda"] [p| Coda |]
  , commandPair ["crowd_realtime"] [p| CrowdRealtime |]
  , commandPair ["crowd_intense"] [p| CrowdIntense |]
  , commandPair ["crowd_normal"] [p| CrowdNormal |]
  , commandPair ["crowd_mellow"] [p| CrowdMellow |]
  , commandPair ["crowd_noclap"] [p| CrowdNoclap |]
  , commandPair ["crowd_clap"] [p| CrowdClap |]
  , ( [e| firstEventWhich $ readCommand' >=> \case
        "section" : ws -> Just $ PracticeSection $ T.pack $ unwords ws
        [s] -> PracticeSection . T.pack <$> stripPrefix "prc_" s
        _   -> Nothing
      |]
    , [e| \case PracticeSection s -> RTB.singleton NNC.zero $ makeRB3Section s |]
    )
  , blip 24 [p| PracticeKick |]
  , blip 25 [p| PracticeSnare |]
  , blip 26 [p| PracticeHihat |]
  ]
