{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Parser.Drums where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Sound.MIDI.Message.Channel.Voice as V
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Relative.TimeBody as RTB
import Control.Applicative ((<|>), (<*>), (<$>))

import Parser.Base

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

readEvent :: E.T -> Maybe [Event]
readEvent e = case e of
  MIDINote p b -> case V.fromPitch p of
    24  -> on $ Animation KickRF
    25  -> long $ Animation $ HihatOpen b
    26  -> on $ Animation $ Snare HardHit LH
    27  -> on $ Animation $ Snare HardHit RH
    28  -> on $ Animation $ Snare SoftHit LH
    29  -> on $ Animation $ Snare SoftHit RH
    30  -> on $ Animation $ Hihat LH
    31  -> on $ Animation $ Hihat RH
    32  -> on $ Animation $ PercussionRH
    -- 33 unused
    34  -> on $ Animation $ Crash1 HardHit LH
    35  -> on $ Animation $ Crash1 SoftHit LH
    36  -> on $ Animation $ Crash1 HardHit RH
    37  -> on $ Animation $ Crash1 SoftHit RH
    38  -> on $ Animation $ Crash2 HardHit RH
    39  -> on $ Animation $ Crash2 SoftHit RH
    40  -> on $ Animation Crash2RHChokeLH
    41  -> on $ Animation Crash1RHChokeLH
    42  -> on $ Animation $ Ride RH
    43  -> on $ Animation $ Ride LH
    44  -> on $ Animation $ Crash2 HardHit LH
    45  -> on $ Animation $ Crash2 SoftHit LH
    46  -> on $ Animation $ Tom1 LH
    47  -> on $ Animation $ Tom1 RH
    48  -> on $ Animation $ Tom2 LH
    49  -> on $ Animation $ Tom2 RH
    50  -> on $ Animation $ FloorTom LH
    51  -> on $ Animation $ FloorTom RH

    60  -> on $ Note Easy Kick
    61  -> on $ Note Easy Red
    62  -> on $ Note Easy $ Pro Yellow ()
    63  -> on $ Note Easy $ Pro Blue   ()
    64  -> on $ Note Easy $ Pro Green  ()

    72  -> on $ Note Medium Kick
    73  -> on $ Note Medium Red
    74  -> on $ Note Medium $ Pro Yellow ()
    75  -> on $ Note Medium $ Pro Blue   ()
    76  -> on $ Note Medium $ Pro Green  ()

    84  -> on $ Note Hard Kick
    85  -> on $ Note Hard Red
    86  -> on $ Note Hard $ Pro Yellow ()
    87  -> on $ Note Hard $ Pro Blue   ()
    88  -> on $ Note Hard $ Pro Green  ()

    96  -> on $ Note Expert Kick
    97  -> on $ Note Expert Red
    98  -> on $ Note Expert $ Pro Yellow ()
    99  -> on $ Note Expert $ Pro Blue   ()
    100 -> on $ Note Expert $ Pro Green  ()

    103 -> long $ Solo b
    105 -> long $ Player1 b
    106 -> long $ Player2 b
    110 -> long $ ProType Yellow $ if b then Tom else Cymbal
    111 -> long $ ProType Blue   $ if b then Tom else Cymbal
    112 -> long $ ProType Green  $ if b then Tom else Cymbal
    116 -> long $ Overdrive b
    -- 120..124 are always together, so we just parse 120 and ignore the rest
    120 -> long $ Activation b
    121 -> Just []
    122 -> Just []
    123 -> Just []
    124 -> Just []
    126 -> long $ SingleRoll b
    127 -> long $ DoubleRoll b
    _   -> Nothing
    where on   x = Just [x | b]
          long x = Just [x]
  E.MetaEvent (Meta.TextEvent s) -> fmap (: [])
    $   fmap SetMix (readCommand s)
    <|> fmap Mood (readCommand s)
    <|> do
      readCommand s >>= \case
        ["ride_side_true" ] -> Just $ Animation $ RideSide True
        ["ride_side_false"] -> Just $ Animation $ RideSide False
        _                   -> Nothing
  _ -> Nothing

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

showEvent :: Event -> RTB.T U.Beats E.T
showEvent = \case
  Mood m -> one $ showCommand' m
  ProType Yellow ptype -> one $ edge' 110 (ptype == Tom)
  ProType Blue   ptype -> one $ edge' 111 (ptype == Tom)
  ProType Green  ptype -> one $ edge' 112 (ptype == Tom)
  SetMix mix -> one $ showCommand' mix
  SingleRoll b -> one $ edge' 126 b
  DoubleRoll b -> one $ edge' 127 b
  Overdrive  b -> one $ edge' 116 b
  Activation b -> foldr (RTB.cons 0) RTB.empty [ edge' p b | p <- [120 .. 124] ]
  Solo       b -> one $ edge' 103 b
  Player1    b -> one $ edge' 105 b
  Player2    b -> one $ edge' 106 b
  Note diff gem -> blip $ V.toPitch $ 60 + 12 * fromEnum diff + case gem of
    Kick          -> 0
    Red           -> 1
    Pro Yellow () -> 2
    Pro Blue   () -> 3
    Pro Green  () -> 4
  Animation anim -> case anim of
    KickRF -> blip $ V.toPitch 24
    HihatOpen b -> one $ edge' 25 b
    Snare HardHit LH -> blip $ V.toPitch 26
    Snare HardHit RH -> blip $ V.toPitch 27
    Snare SoftHit LH -> blip $ V.toPitch 28
    Snare SoftHit RH -> blip $ V.toPitch 29
    Hihat LH -> blip $ V.toPitch 30
    Hihat RH -> blip $ V.toPitch 31
    PercussionRH -> blip $ V.toPitch 32
    -- 33 unused
    Crash1 HardHit LH -> blip $ V.toPitch 34
    Crash1 SoftHit LH -> blip $ V.toPitch 35
    Crash1 HardHit RH -> blip $ V.toPitch 36
    Crash1 SoftHit RH -> blip $ V.toPitch 37
    Crash2 HardHit RH -> blip $ V.toPitch 38
    Crash2 SoftHit RH -> blip $ V.toPitch 39
    Crash2RHChokeLH -> blip $ V.toPitch 40
    Crash1RHChokeLH -> blip $ V.toPitch 41
    Ride RH -> blip $ V.toPitch 42
    Ride LH -> blip $ V.toPitch 43
    Crash2 HardHit LH -> blip $ V.toPitch 44
    Crash2 SoftHit LH -> blip $ V.toPitch 45
    Tom1 LH -> blip $ V.toPitch 46
    Tom1 RH -> blip $ V.toPitch 47
    Tom2 LH -> blip $ V.toPitch 48
    Tom2 RH -> blip $ V.toPitch 49
    FloorTom LH -> blip $ V.toPitch 50
    FloorTom RH -> blip $ V.toPitch 51
    RideSide True -> one $ showCommand' ["ride_side_true"]
    RideSide False -> one $ showCommand' ["ride_side_false"]
  where one x = RTB.singleton 0 x
