{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module RockBand.Codec.Vocal where

import           Control.Monad                    ((>=>))
import           Control.Monad.Codec
import           Data.DTA.Serialize.Magma         (Percussion (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta

data Pitch
  = Octave36 Key
  | Octave48 Key
  | Octave60 Key
  | Octave72 Key
  | Octave84C
  deriving (Eq, Ord, Show, Read, Generic)
  deriving (Enum, Bounded) via GenericFullEnum Pitch

pitchToKey :: Pitch -> Key
pitchToKey = \case
  Octave36 k -> k
  Octave48 k -> k
  Octave60 k -> k
  Octave72 k -> k
  Octave84C  -> C

parsePercAnimation :: (Monad m, NNC.C t) => TrackEvent m t (Percussion, Bool)
parsePercAnimation = let
  startEnd b = if b then "_start" else "end"
  unparse :: (Percussion, Bool) -> [T.Text]
  unparse (typ, b) = case typ of
    Tambourine -> ["tambourine" <> startEnd b]
    Cowbell    -> ["cowbell"    <> startEnd b]
    Handclap   -> ["clap"       <> startEnd b]
  parse :: [T.Text] -> Maybe (Percussion, Bool)
  parse = reverseLookup ((,) <$> each <*> each) unparse
  in single (readCommand' >=> parse) (showCommand' . unparse)

data VocalTrack t = VocalTrack
  { vocalMood          :: RTB.T t Mood
  , vocalLyrics        :: RTB.T t T.Text
  , vocalPerc          :: RTB.T t () -- ^ playable percussion notes
  , vocalPercSound     :: RTB.T t () -- ^ nonplayable percussion, only triggers sound sample
  , vocalPercAnimation :: RTB.T t (Percussion, Bool)
  , vocalPhrase1       :: RTB.T t Bool -- ^ General phrase marker (RB3) or Player 1 phrases (pre-RB3)
  , vocalPhrase2       :: RTB.T t Bool -- ^ Pre-RB3, used for 2nd player phrases in Tug of War
  , vocalOverdrive     :: RTB.T t Bool
  , vocalLyricShift    :: RTB.T t ()
  , vocalRangeShift    :: RTB.T t Bool
  , vocalNotes         :: RTB.T t (Pitch, Bool)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (VocalTrack t)

nullVox :: VocalTrack t -> Bool
-- we look at lyrics also, so lyrics can be imported from PS/CH into vox tracks
nullVox t = RTB.null (vocalNotes t) && RTB.null (vocalLyrics t)

instance TraverseTrack VocalTrack where
  traverseTrack fn (VocalTrack a b c d e f g h i j k) = VocalTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f
    <*> fn g <*> fn h <*> fn i <*> fn j <*> fn k

instance ParseTrack VocalTrack where
  parseTrack = do
    vocalMood   <- vocalMood   =. command
    vocalLyrics <- vocalLyrics =. let
      withStr s = case readCommand txt :: Maybe [T.Text] of
        -- non-command text events get defaulted to lyrics.
        -- and, commands sometimes accidentally in lyric events
        Nothing -> Just txt
        Just _  -> Nothing
        where txt = T.pack s
      fp = \case
        E.MetaEvent (Meta.Lyric     s) -> withStr s
        E.MetaEvent (Meta.TextEvent s) -> withStr s
        _                              -> Nothing
      fs = E.MetaEvent . Meta.Lyric . T.unpack
      in single fp fs
    vocalPerc          <- vocalPerc          =. fatBlips (1/8) (blip 96)
    vocalPercSound     <- vocalPercSound     =. fatBlips (1/8) (blip 97)
    vocalPercAnimation <- vocalPercAnimation =. parsePercAnimation
    vocalPhrase1       <- vocalPhrase1       =. edges 105
    vocalPhrase2       <- vocalPhrase2       =. edges 106
    vocalOverdrive     <- vocalOverdrive     =. edges 116
    vocalLyricShift    <- vocalLyricShift    =. fatBlips (1/8) (blip 1)
    vocalRangeShift    <- vocalRangeShift    =. edges 0
    vocalNotes         <- (vocalNotes        =.)
      $ condenseMap $ eachKey each $ edges . (+ 36) . fromEnum
    return VocalTrack{..}

asciify :: T.Text -> T.Text
asciify = let
  oneToOne = zip
    "ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõö÷øùúûüýÿ"
    "AAAAAACEEEEIIIIDNOOOOOxOUUUUYaaaaaaceeeeiiiidnooooo/ouuuuyy"
  f 'Æ' = "AE"
  f 'Þ' = "Th"
  f 'ß' = "ss"
  f 'æ' = "ae"
  f 'þ' = "th"
  f c   = T.singleton $ fromMaybe c $ lookup c oneToOne
  in T.concatMap f

-- | Phase Shift doesn't support non-ASCII chars in lyrics.
-- (RB text events are always Latin-1, even if .dta encoding is UTF-8.)
asciiLyrics :: VocalTrack t -> VocalTrack t
asciiLyrics vt = vt { vocalLyrics = fmap asciify $ vocalLyrics vt }

-- | Strips some CH-format tags used in lyric events.
stripTags :: VocalTrack t -> VocalTrack t
stripTags vt = vt
  { vocalLyrics = let
    tags = ["<b>", "</b>", "<i>", "</i>"]
    in flip fmap (vocalLyrics vt) $ \t ->
      foldr (\tag lyric -> T.concat $ T.splitOn tag lyric) t tags
  }
