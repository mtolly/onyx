{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module RockBand.Vocals where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import RockBand.Common
import Data.Char (toLower)
import RockBand.Parse

data Event
  = LyricShift
  | Mood Mood
  | Lyric String
  | Percussion -- ^ playable percussion note
  | PercussionSound -- ^ nonplayable percussion note, only triggers sound sample
  | PercussionAnimation PercussionType Bool
  | Phrase     Bool -- ^ General phrase marker (RB3) or Player 1 phrases (pre-RB3)
  | Phrase2    Bool -- ^ Pre-RB3, used for 2nd player phrases in Tug of War
  | Overdrive  Bool
  | RangeShift Bool
  | Note Pitch Bool
  deriving (Eq, Ord, Show, Read)

data Pitch
  = Octave36 Key
  | Octave48 Key
  | Octave60 Key
  | Octave72 Key
  | Octave84C
  deriving (Eq, Ord, Show, Read)

data PercussionType
  = Tambourine
  | Cowbell
  | Clap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command (PercussionType, Bool) where
  fromCommand (typ, b) = [map toLower (show typ) ++ if b then "_start" else "_end"]
  toCommand = reverseLookup ((,) <$> each <*> each) fromCommand

instanceMIDIEvent [t| Event |]

  [ edge 0 $ applyB [p| RangeShift |]
  , blip 1 [p| LyricShift |]

  , edgeRange [36..47] $ \_i _b -> [p| Note (Octave36 $(keyP $ _i - 36)) $(boolP _b) |]
  , edgeRange [48..59] $ \_i _b -> [p| Note (Octave48 $(keyP $ _i - 48)) $(boolP _b) |]
  , edgeRange [60..71] $ \_i _b -> [p| Note (Octave60 $(keyP $ _i - 60)) $(boolP _b) |]
  , edgeRange [72..83] $ \_i _b -> [p| Note (Octave72 $(keyP $ _i - 72)) $(boolP _b) |]
  , edge      84       $ \   _b -> [p| Note Octave84C                    $(boolP _b) |]

  , blip 96 [p| Percussion |]
  , blip 97 [p| PercussionSound |]
  , edge 105 $ applyB [p| Phrase |]
  , edge 106 $ applyB [p| Phrase2 |]
  , edge 116 $ applyB [p| Overdrive |]

  , ( [e| mapParseOne Mood parseCommand |]
    , [e| \case Mood m -> unparseCommand m |]
    )
  , ( [e| mapParseOne (uncurry PercussionAnimation) parseCommand |]
    , [e| \case PercussionAnimation t b -> unparseCommand (t, b) |]
    )
  , ( [e| firstEventWhich $ \case
        E.MetaEvent (Meta.Lyric s) -> Just $ Lyric s
        E.MetaEvent (Meta.TextEvent s) -> Just $ Lyric s
        -- unrecognized text events are lyrics by default.
        -- but note that this must come after the mood and perc-anim parsers!
        _ -> Nothing
      |]
    , [e| \case Lyric s -> RTB.singleton NNC.zero $ E.MetaEvent $ Meta.Lyric s |]
    )
  ]
