{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Parser.Drums where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Sound.MIDI.Util as U
import Control.Applicative ((<*>), (<$>))

import Parser.Base
import Parser.TH
import Language.Haskell.TH

data ProColor = Yellow | Blue | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ProType = Cymbal | Tom
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Gem a = Kick | Red | Pro ProColor a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- | Constructors are ordered for optimal processing with 'RTB.normalize'.
-- For example, 'Note' comes last so that everything else is processed first.
-- Also, @'ProType' _ 'Cymbal'@ comes before @'ProType' _ 'Tom'@, since the
-- note-on ('Tom') should supercede the simultaneous note-off ('Cymbal').
data Event
  = Mood Mood
  | ProType ProColor ProType
  | SetMix Mix
  | SingleRoll Bool
  | DoubleRoll Bool
  | Overdrive  Bool -- ^ white notes to gain energy
  | Activation Bool -- ^ drum fill to activate Overdrive, or BRE
  | Solo       Bool
  | Player1    Bool
  | Player2    Bool
  | Note Difficulty (Gem ())
  | Animation Animation
  deriving (Eq, Ord, Show, Read)

data Animation
  = Tom1 Hand -- ^ The high tom.
  | Tom2 Hand -- ^ The middle tom.
  | FloorTom Hand -- ^ The low tom.
  | Hihat Hand
  | Snare Hit Hand
  | Ride Hand
  | Crash1 Hit Hand -- ^ The left crash, closer to the hihat.
  | Crash2 Hit Hand -- ^ The right crash, closer to the ride.
  | KickRF
  | Crash1RHChokeLH
  -- NOTE: This is MIDI note 41! The RBN docs incorrectly say 40.
  | Crash2RHChokeLH
  -- NOTE: This is MIDI note 40! The RBN docs incorrectly say 41.
  | PercussionRH
  | HihatOpen Bool
  | RideSide Bool -- ^ Causes slow 'Ride' hits to animate differently.
  deriving (Eq, Ord, Show, Read)

data Hit = SoftHit | HardHit deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Hand = LH | RH deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Mix = Mix Difficulty Audio Disco
  deriving (Eq, Ord, Show, Read)

-- | Controls the audio files used for the drum track.
data Audio
  = D0 -- ^ One stereo mix for the whole kit.
  | D1 -- ^ Mono kick, mono snare, stereo kit.
  | D2 -- ^ Mono kick, stereo snare, stereo kit.
  | D3 -- ^ Stereo kick, stereo snare, stereo kit.
  | D4 -- ^ Mono kick, stereo kit (including snare).
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Special options that can affect drum audio and pad settings.
data Disco
  = NoDisco     -- ^ All pads are normal.
  | Disco       -- ^ Yellow snare, red hihat. Undone by Pro Drums.
  | DiscoNoFlip -- ^ New in RB3: snare beats where accented hits are 'Yellow'.
  | EasyMix     -- ^ Pre-RB3. 'Easy' sections with only 'Red' and 'Kick' notes.
  | EasyNoKick  -- ^ Pre-RB3. 'Easy' sections with no 'Kick' notes.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- TODO: Fly Like an Eagle has [mix n drums2a], what is this?

instance Command Mix where
  fromCommand (Mix diff audio disco) = ["mix", show $ fromEnum diff, showMix audio disco]
  toCommand = reverseLookup (Mix <$> each <*> each <*> each) fromCommand

-- | e.g. turns 'D2' and 'Disco' into @\"drums2d\"@
showMix :: Audio -> Disco -> String
showMix audio disco = "drums" ++ show (fromEnum audio) ++ case disco of
  NoDisco     -> ""
  Disco       -> "d"
  DiscoNoFlip -> "dnoflip"
  EasyMix     -> "easy"
  EasyNoKick  -> "easynokick"

rosetta :: (Q Exp, Q Exp)
rosetta = translation

  [ blip 24  [p| Animation KickRF |]
  , edge 25  $ \b -> if b then [p| Animation (HihatOpen True) |] else [p| Animation (HihatOpen False) |]
  , blip 26  [p| Animation (Snare HardHit LH) |]
  , blip 27  [p| Animation (Snare HardHit RH) |]
  , blip 28  [p| Animation (Snare SoftHit LH) |]
  , blip 29  [p| Animation (Snare SoftHit RH) |]
  , blip 30  [p| Animation (Hihat LH) |]
  , blip 31  [p| Animation (Hihat RH) |]
  , blip 32  [p| Animation PercussionRH |]
  -- 33 unused
  , blip 34  [p| Animation (Crash1 HardHit LH) |]
  , blip 35  [p| Animation (Crash1 SoftHit LH) |]
  , blip 36  [p| Animation (Crash1 HardHit RH) |]
  , blip 37  [p| Animation (Crash1 SoftHit RH) |]
  , blip 38  [p| Animation (Crash2 HardHit RH) |]
  , blip 39  [p| Animation (Crash2 SoftHit RH) |]
  , blip 40  [p| Animation Crash2RHChokeLH |]
  , blip 41  [p| Animation Crash1RHChokeLH |]
  , blip 42  [p| Animation (Ride RH) |]
  , blip 43  [p| Animation (Ride LH) |]
  , blip 44  [p| Animation (Crash2 HardHit LH) |]
  , blip 45  [p| Animation (Crash2 SoftHit LH) |]
  , blip 46  [p| Animation (Tom1 LH) |]
  , blip 47  [p| Animation (Tom1 RH) |]
  , blip 48  [p| Animation (Tom2 LH) |]
  , blip 49  [p| Animation (Tom2 RH) |]
  , blip 50  [p| Animation (FloorTom LH) |]
  , blip 51  [p| Animation (FloorTom RH) |]

  , blip 60  [p| Note Easy Kick |]
  , blip 61  [p| Note Easy Red |]
  , blip 62  [p| Note Easy (Pro Yellow ()) |]
  , blip 63  [p| Note Easy (Pro Blue   ()) |]
  , blip 64  [p| Note Easy (Pro Green  ()) |]

  , blip 72  [p| Note Medium Kick |]
  , blip 73  [p| Note Medium Red |]
  , blip 74  [p| Note Medium (Pro Yellow ()) |]
  , blip 75  [p| Note Medium (Pro Blue   ()) |]
  , blip 76  [p| Note Medium (Pro Green  ()) |]

  , blip 84  [p| Note Hard Kick |]
  , blip 85  [p| Note Hard Red |]
  , blip 86  [p| Note Hard (Pro Yellow ()) |]
  , blip 87  [p| Note Hard (Pro Blue   ()) |]
  , blip 88  [p| Note Hard (Pro Green  ()) |]

  , blip 96  [p| Note Expert Kick |]
  , blip 97  [p| Note Expert Red |]
  , blip 98  [p| Note Expert (Pro Yellow ()) |]
  , blip 99  [p| Note Expert (Pro Blue   ()) |]
  , blip 100 [p| Note Expert (Pro Green  ()) |]

  , edge 103 $ applyB [p| Solo |]
  , edge 105 $ applyB [p| Player1 |]
  , edge 106 $ applyB [p| Player2 |]
  , edge 110 $ \b -> if b then [p| ProType Yellow Tom |] else [p| ProType Yellow Cymbal |]
  , edge 111 $ \b -> if b then [p| ProType Blue   Tom |] else [p| ProType Blue   Cymbal |]
  , edge 112 $ \b -> if b then [p| ProType Green  Tom |] else [p| ProType Green  Cymbal |]
  , edge 116 $ applyB [p| Overdrive |]
  , edges [120 .. 124] $ applyB [p| Activation |]
  , edge 126 $ applyB [p| SingleRoll |]
  , edge 127 $ applyB [p| DoubleRoll |]

  , ( [e| mapParseOne Mood parseCommand |]
    , [e| \case Mood m -> unparseCommand m |]
    )
  , ( [e| mapParseOne SetMix parseCommand |]
    , [e| \case SetMix m -> unparseCommand m |]
    )
  , ( [e| U.extractFirst $ \e -> readCommand' e >>= \case
        ["ride_side_true" ] -> Just $ Animation $ RideSide True
        ["ride_side_false"] -> Just $ Animation $ RideSide False
        _ -> Nothing
      |]
    , [e| \case
        Animation (RideSide True ) -> unparseCommand ["ride_side_true"]
        Animation (RideSide False) -> unparseCommand ["ride_side_false"]
      |]
    )
  ]
