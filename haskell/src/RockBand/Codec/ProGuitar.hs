{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.Codec.ProGuitar where

import           Control.Monad                    (forM, guard, (>=>))
import           Control.Monad.Codec
import           Control.Monad.Trans.StackTrace
import           Data.Default.Class               (Default (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust)
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common
import           Text.Read                        (readMaybe)

data NoteType
  = NormalNote
  | ArpeggioForm
  | Bent
  | Muted
  | Tapped
  | Harmonic
  | PinchHarmonic
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SlideType = NormalSlide | ReversedSlide | MysterySlide3 | MysterySlide2
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data StrumArea = High | Mid | Low | MysteryStrum0
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type GtrFret = Int

data GtrString = S6 | S5 | S4 | S3 | S2 | S1
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

class (Enum a, Bounded a) => GtrChannel a where
  encodeChannel :: a -> Int
  channelMap :: [(Int, a)]
  channelMap = [ (encodeChannel x, x) | x <- [minBound .. maxBound] ]
instance GtrChannel NoteType where
  encodeChannel = fromEnum
instance GtrChannel SlideType where
  encodeChannel = \case
    NormalSlide   -> 0
    ReversedSlide -> 11
    MysterySlide3 -> 3
    MysterySlide2 -> 2
instance GtrChannel StrumArea where
  encodeChannel = \case
    High -> 13
    Mid  -> 14
    Low  -> 15
    MysteryStrum0 -> 0

data GuitarType = TypeGuitar | TypeBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ProGuitarTrack t = ProGuitarTrack
  { pgDifficulties :: Map.Map Difficulty (ProGuitarDifficulty t)
  , pgTrainer      :: RTB.T t (GuitarType, Trainer)
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
  } deriving (Eq, Ord, Show)

instance TraverseTrack ProGuitarTrack where
  traverseTrack fn (ProGuitarTrack a b c d e f g h i j k l m n o p) = ProGuitarTrack
    <$> traverse (traverseTrack fn) a <*> fn b <*> fn c <*> fn d
    <*> fn e <*> fn f <*> fn g <*> fn h <*> fn i <*> fn j
    <*> fn k <*> fn l <*> fn m <*> fn n <*> fn o <*> fn p

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

instance TraverseTrack ProGuitarDifficulty where
  traverseTrack fn (ProGuitarDifficulty a b c d e f g h) = ProGuitarDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d
    <*> fn e <*> fn f <*> fn g <*> fn h

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
      forM trk $ \(c, v) -> do
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

instance ParseTrack ProGuitarTrack where
  parseTrack = do
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
    -- note, we explicitly do TypeGuitar before TypeBass
    -- because we want to try to parse the 6-note version first
    pgBRE          <- (pgBRE =.) $ condenseMap $ eachKey [TypeGuitar, TypeBass] $ edgesBRE . \case
      TypeGuitar -> [120 .. 125]
      TypeBass   -> [120 .. 123]
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
    pgOnyxOctave   <- pgOnyxOctave =. let
      parse = readCommand' >=> \case
        ["onyx", "octave", x] -> readMaybe $ T.unpack x
        _ -> Nothing
      unparse n = showCommand' ["onyx", "octave", T.pack $ show n]
      in single parse unparse
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
      pgChordName    <- pgChordName =. let
        cmd = T.pack $ "chrd" ++ show (fromEnum diff)
        parse = readCommand' >=> \case
          [k] | k == cmd -> Just Nothing
          [k, cname] | k == cmd -> Just $ Just cname
          _ -> Nothing
        unparse cname = showCommand' $ cmd : toList cname
        in single parse unparse
      return ProGuitarDifficulty{..}
    return ProGuitarTrack{..}
