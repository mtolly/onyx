{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
module Onyx.Taiko.Base where

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Functor                     ((<&>))
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T
import           Onyx.MIDI.Common                 (Difficulty (..),
                                                   LaneDifficulty (..),
                                                   pattern RNil, pattern Wait)
import           Onyx.MIDI.Read                   (mapTrack)
import           Onyx.MIDI.Track.Drums
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.Resources                   (getResourcesPath)
import           Onyx.Util.Handle                 (Readable, fileReadable)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  ((</>))

-- Shared format for importing Taiko no Tatsujin style charts

data TaikoEvent t
  = TaikoSingle TaikoNote
  | TaikoRoll t
  | TaikoRollLarge t
  | TaikoDenden t
  deriving (Eq, Ord, Show)

data TaikoNote
  = TaikoCenter
  | TaikoRim
  | TaikoCenterLarge
  | TaikoRimLarge
  deriving (Eq, Ord, Show)

-- convert Taiko to 4-lane drums;
-- Y/B are center, R/G are rim, hands alternate automatically
taikoEventsToTrack :: U.TempoMap -> RTB.T U.Seconds (TaikoEvent U.Seconds) -> DrumTrack U.Beats
taikoEventsToTrack tmap events = let

  red    = (Red          , VelocityNormal)
  yellow = (Pro Yellow (), VelocityNormal)
  blue   = (Pro Blue   (), VelocityNormal)
  green  = (Pro Green  (), VelocityNormal)

  laneSpeed = 0.080 :: U.Seconds -- 80 ms for consistent combo
  -- make lanes into single notes if they're too short to have 2 notes
  events' = events <&> \case
    TaikoRoll      t | t <= laneSpeed -> TaikoSingle TaikoCenter
    TaikoRollLarge t | t <= laneSpeed -> TaikoSingle TaikoCenterLarge
    TaikoDenden    t | t <= laneSpeed -> TaikoSingle TaikoCenter
    event                             -> event

  normalNotes = flip RTB.mapMaybe events' \case
    TaikoSingle x -> Just x
    _             -> Nothing

  makeLane len = Wait 0 (Just LaneExpert) $ Wait len Nothing RNil
  doubleLanes = U.trackJoin $ events' <&> \case
    TaikoRoll      len -> makeLane len
    TaikoRollLarge len -> makeLane len
    TaikoDenden    len -> makeLane len
    _                  -> RNil

  laneNotes = U.trackJoin $ events' <&> \case
    TaikoRoll      len -> U.trackTake len $ RTB.fromPairList $ zip (0 : repeat laneSpeed) $ repeat TaikoCenter
    TaikoRollLarge len -> U.trackTake len $ RTB.fromPairList $ zip (0 : repeat laneSpeed) $ repeat TaikoCenter
    TaikoDenden    len -> U.trackTake len $ RTB.fromPairList $ zip (0 : repeat laneSpeed) $ cycle [TaikoCenter, TaikoRim]
    TaikoSingle    _   -> RNil

  applySticking nextHand = \case
    RNil -> RNil
    Wait t TaikoCenter      rest
      -> Wait t (case nextHand of LH -> yellow; RH -> blue ) $ applySticking flipHand rest
    Wait t TaikoRim         rest
      -> Wait t (case nextHand of LH -> red   ; RH -> green) $ applySticking flipHand rest
    Wait t TaikoCenterLarge rest
      -> Wait t yellow $ Wait 0 blue  $ applySticking RH rest
    Wait t TaikoRimLarge    rest
      -> Wait t red    $ Wait 0 green $ applySticking RH rest
    where flipHand = case nextHand of LH -> RH; RH -> LH

  in mapTrack (U.unapplyTempoTrack tmap) mempty
    { difficulties = Map.singleton Expert mempty
      { gems = applySticking RH $ RTB.merge normalNotes laneNotes
      }
    , doubleRoll = doubleLanes
    }

-- Drum hit samples

taikoKeyNormal, taikoKeyClap, taikoKeyFinish, taikoKeyWhistle :: T.Text
taikoKeyNormal  = "hit-normal"
taikoKeyClap    = "hit-clap"
taikoKeyFinish  = "hit-finish"
taikoKeyWhistle = "hit-whistle"

taikoKeySamples :: F.PartName -> T.Text
taikoKeySamples k = "samples-" <> F.getPartName k

loadTaikoSamples :: (MonadIO m) => m [(T.Text, FilePath, Readable)]
loadTaikoSamples = do
  dir <- liftIO $ getResourcesPath "sfx/osu-argon-pro"
  return
    [ (taikoKeyNormal , "sfx/hit-normal.ogg" , fileReadable $ dir </> "drum-hitnormal.ogg" )
    , (taikoKeyClap   , "sfx/hit-clap.ogg"   , fileReadable $ dir </> "drum-hitclap.ogg"   )
    , (taikoKeyFinish , "sfx/hit-finish.ogg" , fileReadable $ dir </> "drum-hitfinish.ogg" )
    , (taikoKeyWhistle, "sfx/hit-whistle.ogg", fileReadable $ dir </> "drum-hitwhistle.ogg")
    ]

taikoSampleTrack :: DrumTrack U.Beats -> F.SamplesTrack U.Beats
taikoSampleTrack drums = F.SamplesTrack
  { F.sampleNotes = RTB.empty
  , F.sampleTriggers
    = RTB.flatten
    $ fmap (\instant -> let
      gems   = map fst instant
      center = any (`elem` gems) [Pro Yellow (), Pro Blue  ()]
      rim    = any (`elem` gems) [Red          , Pro Green ()]
      double = not $ null $ drop 1 instant
      in concat
        [ [F.simpleTrigger taikoKeyNormal  | center          ]
        , [F.simpleTrigger taikoKeyClap    | rim             ]
        , [F.simpleTrigger taikoKeyFinish  | center && double]
        , [F.simpleTrigger taikoKeyWhistle | rim    && double]
        ]
      )
    $ RTB.collectCoincident
    $ (.gems)
    $ fromMaybe mempty $ Map.lookup Expert drums.difficulties
  }
