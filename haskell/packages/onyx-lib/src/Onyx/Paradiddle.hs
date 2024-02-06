{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE ViewPatterns          #-}
module Onyx.Paradiddle where

import           Control.Monad.Codec
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ask)
import qualified Data.Aeson                 as A
import           Data.Scientific            (Scientific)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Linear                     (V3 (..))
import           Onyx.Codec.Common
import           Onyx.Codec.JSON
import           Onyx.StackTrace
import           Text.Read                  (readMaybe)

data RLRR = RLRR
  { version           :: Scientific -- odd type choice
  , recordingMetadata :: RecordingMetadata
  , audioFileData     :: AudioFileData
  , highwaySettings   :: Maybe HighwaySettings
  , instruments       :: [Instrument]
  , events            :: [Event]
  , bpmEvents         :: [BPMEvent]
  } deriving (Show)

instance StackJSON RLRR where
  stackJSON = asObject "RLRR" $ do
    version           <- (.version          ) =. req         "version"           stackJSON
    recordingMetadata <- (.recordingMetadata) =. req         "recordingMetadata" stackJSON
    audioFileData     <- (.audioFileData    ) =. req         "audioFileData"     stackJSON
    highwaySettings   <- (.highwaySettings  ) =. opt Nothing "highwaySettings"   stackJSON
    instruments       <- (.instruments      ) =. req         "instruments"       stackJSON
    events            <- (.events           ) =. req         "events"            stackJSON
    bpmEvents         <- (.bpmEvents        ) =. req         "bpmEvents"         stackJSON
    return RLRR{..}

data RecordingMetadata = RecordingMetadata
  { title          :: T.Text
  , description    :: T.Text
  , coverImagePath :: Maybe T.Text
  , artist         :: Maybe T.Text
  , creator        :: Maybe T.Text
  , length_        :: Scientific
  , complexity     :: Scientific
  } deriving (Show)

instance StackJSON RecordingMetadata where
  stackJSON = asObject "RecordingMetadata" $ do
    title          <- (.title         ) =. req         "title"          stackJSON
    description    <- (.description   ) =. req         "description"    stackJSON
    coverImagePath <- (.coverImagePath) =. opt Nothing "coverImagePath" stackJSON
    artist         <- (.artist        ) =. opt Nothing "artist"         stackJSON
    creator        <- (.creator       ) =. opt Nothing "creator"        stackJSON
    length_        <- (.length_       ) =. req         "length"         stackJSON
    complexity     <- (.complexity    ) =. req         "complexity"     stackJSON
    return RecordingMetadata{..}

data AudioFileData = AudioFileData
  { songTracks        :: [T.Text]
  , drumTracks        :: [T.Text]
  , calibrationOffset :: Scientific
  } deriving (Show)

instance StackJSON AudioFileData where
  stackJSON = asObject "AudioFileData" $ do
    songTracks        <- (.songTracks       ) =. req "songTracks"        stackJSON
    drumTracks        <- (.drumTracks       ) =. req "drumTracks"        stackJSON
    calibrationOffset <- (.calibrationOffset) =. req "calibrationOffset" stackJSON
    return AudioFileData{..}

data HighwaySettings = HighwaySettings
  { ghostNotes          :: Maybe Bool
  , accentNotes         :: Maybe Bool
  , ghostNoteThreshold  :: Maybe Scientific
  , accentNoteThreshold :: Maybe Scientific
  } deriving (Eq, Show)

instance StackJSON HighwaySettings where
  stackJSON = asObject "HighwaySettings" $ do
    ghostNotes           <- (.ghostNotes         ) =. opt Nothing "ghostNotes"          stackJSON
    accentNotes          <- (.accentNotes        ) =. opt Nothing "accentNotes"         stackJSON
    ghostNoteThreshold   <- (.ghostNoteThreshold ) =. opt Nothing "ghostNoteThreshold"  stackJSON
    accentNoteThreshold  <- (.accentNoteThreshold) =. opt Nothing "accentNoteThreshold" stackJSON
    return HighwaySettings{..}

data Instrument = Instrument
  { name     :: T.Text
  , class_   :: T.Text
  , location :: V3 Scientific
  , rotation :: V3 Scientific
  , scale    :: V3 Scientific
  -- overrideData             :: Maybe T.Text
  -- openMidiNote             :: Maybe Int
  -- closedMidiNote           :: Maybe Int
  -- pedalCloseVelocityScalar :: Maybe Scientific
  -- pedalCloseSoundsEnabled  :: Maybe Bool
  -- volumeMultiplier         :: Maybe Scientific
  -- pitchMultiplier          :: Maybe Scientific
  -- isMuted                  :: Maybe Bool
  -- midiNotes                :: Maybe [Int]
  -- midiChannel              :: Maybe Int
  -- physicsAnimEnabled       :: Maybe Bool
  -- centerRadius             :: Maybe Scientific
  } deriving (Show)

{-
in "location": [X, Y, Z]
from drummer's perspective:
X must be forward/back?
Y is left/right, higher is right
Z is up/down, higher is up
-}

jsonV3 :: (Monad m) => JSONCodec m (V3 Scientific)
jsonV3 = Codec
  { codecOut = makeOut $ \(V3 x y z) -> A.toJSON [x, y, z]
  , codecIn = lift ask >>= \case
    A.Array (V.toList -> [A.Number x, A.Number y, A.Number z])
      -> return $ V3 x y z
    _ -> fatal "Expected an array of 3 numbers"
  }

instance StackJSON Instrument where
  stackJSON = asObject "Instrument" $ do
    name     <- (.name    ) =. req "name"     stackJSON
    class_   <- (.class_  ) =. req "class"    stackJSON
    location <- (.location) =. req "location" jsonV3
    rotation <- (.rotation) =. req "rotation" jsonV3
    scale    <- (.scale   ) =. req "scale"    jsonV3
    return Instrument{..}

data Event = Event
  { name :: T.Text
  , vel  :: Scientific
  , loc  :: Scientific
  , time :: Scientific -- can be string or number
  } deriving (Show)

stringOrNumber :: (Monad m) => JSONCodec m Scientific
stringOrNumber = Codec
  { codecOut = makeOut A.toJSON
  , codecIn = lift ask >>= \case
    A.Number n -> return n
    A.String t -> case readMaybe $ T.unpack t of
      Just n  -> return n
      Nothing -> fatal $ "Couldn't parse number string: " <> show t
    x -> fatal $ "Expected a number but found: " <> show x
  }

instance StackJSON Event where
  stackJSON = asObject "Event" $ do
    name <- (.name) =. req "name" stackJSON
    vel  <- (.vel ) =. req "vel"  stackJSON
    loc  <- (.loc ) =. req "loc"  stackJSON
    time <- (.time) =. req "time" stringOrNumber
    return Event{..}

data BPMEvent = BPMEvent
  { bpm  :: Scientific
  , time :: Scientific
  } deriving (Show)

instance StackJSON BPMEvent where
  stackJSON = asObject "BPMEvent" $ do
    bpm  <- (.bpm ) =. req "bpm"  stackJSON
    time <- (.time) =. req "time" stackJSON
    return BPMEvent{..}
