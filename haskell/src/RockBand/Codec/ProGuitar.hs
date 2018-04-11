{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
module RockBand.Codec.ProGuitar where

import           Control.Monad                    (guard, (>=>))
import           Control.Monad.Codec
import           Control.Monad.Trans.StackTrace
import           Data.Default.Class               (Default (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust)
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common
import           RockBand.ProGuitar               (GtrChannel (..), GtrFret,
                                                   GtrString (..),
                                                   NoteType (..),
                                                   SlideType (..),
                                                   StrumArea (..))
import qualified Sound.MIDI.Util                  as U

data GuitarType = TypeGuitar | TypeBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ProGuitarTrack t = ProGuitarTrack
  { pgTrainer      :: RTB.T t (GuitarType, Trainer)
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

channelEdges
  :: (Show a, GtrChannel a, SendMessage m, NNC.C t)
  => Int -> TrackEvent m t (a, Maybe Int)
channelEdges p = let
  src = edgesCV p
  in Codec
    { codecIn = do
      trk <- codecIn src
      flip traverse trk $ \(c, v) -> do
        c' <- case lookup c channelMap of
          Just c' -> return c'
          Nothing  -> do
            let c' = minBound
            warn $ "Unrecognized channel " ++ show c ++ "; using default value of " ++ show c'
            return c'
        return (c', v)
    , codecOut = \x -> do
      _ <- codecOut src $ fmap (\(c', v) -> (encodeChannel c', v)) x
      return x
    }

channelEdges_
  :: (Show a, GtrChannel a, SendMessage m, NNC.C t)
  => Int -> TrackEvent m t (a, Bool)
channelEdges_ = let
  -- note, this has to be at least 100 because Nemo's MIDI checker
  -- complains (incorrectly) if slide notes have velocity < 100.
  fs (c, b) = (c, guard b >> Just 100)
  fp (c, v) = (c, isJust v)
  in dimap (fmap fs) (fmap fp) . channelEdges

parsePG :: (SendMessage m) => TrackCodec m U.Beats (ProGuitarTrack U.Beats)
parsePG = do
  pgTrainer   <- pgTrainer   =. let
    parse = readCommand' >=> \case
      (t, k) | k == T.pack "pg" -> Just (TypeGuitar, t)
             | k == T.pack "pb" -> Just (TypeBass  , t)
      _ -> Nothing
    unparse (TypeGuitar, t) = showCommand' (t, T.pack "pg")
    unparse (TypeBass  , t) = showCommand' (t, T.pack "pb")
    in single parse unparse
  pgTremolo      <- pgTremolo =. edges 126
  pgTrill        <- pgTrill =. edges 127
  pgOverdrive    <- pgOverdrive =. edges 116
  pgBRE          <- pgBRE =. undefined
  pgSolo         <- pgSolo =. edges 115
  pgHandPosition <- pgHandPosition =. let
    fs fret = (0, fret + 100)
    fp (_c, v) = v - 100
    in dimap (fmap fs) (fmap fp) $ blipCV 108
  pgChordRoot    <- (pgChordRoot =.) $ condenseMap_ $ eachKey each $ blip . \case
    E  -> 4
    F  -> 5
    Fs -> 6
    G  -> 7
    Gs -> 8
    A  -> 9
    As -> 10
    B  -> 11
    C  -> 12
    Cs -> 13
    D  -> 14
    Ds -> 15
  pgNoChordNames <- pgNoChordNames =. edges 17
  pgSlashChords  <- pgSlashChords =. edges 16
  pgFlatChords   <- pgFlatChords =. edges 18
  pgOnyxOctave   <- pgOnyxOctave =. undefined
  pgMystery45    <- pgMystery45 =. edges 45
  pgMystery69    <- pgMystery69 =. edges 69
  pgMystery93    <- pgMystery93 =. edges 93
  pgDifficulties <- (pgDifficulties =.) $ eachKey each $ \diff -> do
    let base = case diff of
          Easy   -> 24
          Medium -> 48
          Hard   -> 72
          Expert -> 96
    pgNotes        <- (pgNotes =.) $ condenseMap $ eachKey each $ \str -> let
      fs (typ, fret, mlen) = (typ, fret + 100, fromMaybe (1/32) mlen)
      fp (typ, v, len) = (typ, v - 100, guard (len > (1/3)) >> Just len)
      in dimap (fmap fs) (fmap fp) $ matchEdgesCV $ channelEdges (base + fromEnum str)
    pgForceHOPO    <- pgForceHOPO =. edges (base + 6)
    pgSlide        <- pgSlide =. channelEdges_ (base + 7)
    pgArpeggio     <- pgArpeggio =. edges (base + 8)
    pgPartialChord <- pgPartialChord =. channelEdges_ (base + 9)
    pgMysteryBFlat <- pgMysteryBFlat =. edges (base + 10)
    pgAllFrets     <- pgAllFrets =. edges (base + 11)
    pgChordName    <- pgChordName =. undefined
    return ProGuitarDifficulty{..}
  return ProGuitarTrack{..}
