{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
module RockBand.Events where

import           Control.Monad                    ((>=>))
import           Data.Data
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.Parse

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
  | SectionRB2 T.Text
  | SectionRB3 T.Text
  | PracticeKick
  | PracticeSnare
  | PracticeHihat
  deriving (Eq, Ord, Show, Read, Typeable, Data)

instanceMIDIEvent [t| Event |] Nothing

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
  , ( [e| one $ firstEventWhich $ readCommand' >=> \case
        "section" : ws -> Just $ SectionRB2 $ T.unwords ws
        _              -> Nothing
      |]
    , [e| \case SectionRB2 s -> RTB.singleton NNC.zero $ showCommand' ["section", s] |]
    )
  , ( [e| one $ firstEventWhich $ readCommand' >=> \case
        [s] -> SectionRB3 <$> T.stripPrefix "prc_" s
        _   -> Nothing
      |]
    , [e| \case SectionRB3 s -> RTB.singleton NNC.zero $ showCommand' ["prc_" <> s] |]
    )
  , blip 24 [p| PracticeKick |]
  , blip 25 [p| PracticeSnare |]
  , blip 26 [p| PracticeHihat |]
  ]
