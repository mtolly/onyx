{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RockBand.Codec.ProGuitar where

import           Control.Monad.Trans.StackTrace
import           Data.Default.Class               (Default (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           RockBand.Codec
import           RockBand.Common
import           RockBand.ProGuitar               (GtrFret, GtrString (..),
                                                   NoteType (..),
                                                   SlideType (..),
                                                   StrumArea (..))
import qualified Sound.MIDI.Util                  as U

data GuitarType = TypeGuitar | TypeBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ProGuitarTrack t = ProGuitarTrack
  { pgTrainers     :: RTB.T t (GuitarType, Trainer)
  , pgTremolo      :: RTB.T t Bool
  , pgTrill        :: RTB.T t Bool
  , pgOverdrive    :: RTB.T t Bool
  , pgBRE          :: RTB.T t (GuitarType, Bool)
  , pgSolo         :: RTB.T t Bool
  , pgHandPosition :: RTB.T t GtrFret
  , pgChordRoot    :: RTB.T t Key
  , pgNoChordNames :: RTB.T t Bool
  , pgSlashChords  :: RTB.T t Bool
  , pgFlatChords   :: RTB.T t Bool -- TODO does this swap when key signature uses flats?
  , pgOnyxOctave   :: RTB.T t GtrFret -- "move these notes 12 frets down if needed" section
  , pgMystery45    :: RTB.T t Bool
  , pgMystery69    :: RTB.T t Bool
  , pgMystery93    :: RTB.T t Bool
  , pgDifficulties :: Map.Map Difficulty (ProGuitarDifficulty t)
  } deriving (Eq, Ord, Show)

data ProGuitarDifficulty t = ProGuitarDifficulty
  { pgChordName    :: RTB.T t (Maybe T.Text)
  , pgForceHOPO    :: RTB.T t Bool
  , pgSlide        :: RTB.T t (SlideType, Bool)
  , pgArpeggio     :: RTB.T t Bool
  , pgPartialChord :: RTB.T t (StrumArea, Bool)
  , pgAllFrets     :: RTB.T t Bool
  , pgMysteryBFlat :: RTB.T t Bool
  -- TODO EOF format sysexes
  , pgNotes        :: RTB.T t (GtrString, (NoteType, GtrFret, Maybe t))
  } deriving (Eq, Ord, Show)

instance Default (ProGuitarDifficulty t) where
  def = ProGuitarDifficulty
    RTB.empty RTB.empty RTB.empty RTB.empty
    RTB.empty RTB.empty RTB.empty RTB.empty

parsePG :: (SendMessage m) => TrackCodec m U.Beats (ProGuitarTrack U.Beats)
parsePG = undefined
