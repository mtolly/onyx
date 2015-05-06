{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
module Parser.Drums
( Event(..), Gem(..), ProColor(..), ProType(..), Audio(..), Disco(..)
, readEvent, showEvent
) where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Relative.TimeBody as RTB

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
  = ProType ProColor ProType
  | Mix Difficulty Audio Disco
  | SingleRoll Bool
  | DoubleRoll Bool
  | Overdrive  Bool -- ^ white notes to gain energy
  | Activation Bool -- ^ drum fill to activate Overdrive, or BRE
  | Solo       Bool
  | Note Difficulty (Gem ())
  -- TODO: animations
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

readEvent :: E.T -> Maybe [Event]
readEvent e = let
  one x = Just [x]
  noteOn p = case V.fromPitch p of
    60  -> one $ Note Easy Kick
    61  -> one $ Note Easy Red
    62  -> one $ Note Easy $ Pro Yellow ()
    63  -> one $ Note Easy $ Pro Blue   ()
    64  -> one $ Note Easy $ Pro Green  ()

    72  -> one $ Note Medium Kick
    73  -> one $ Note Medium Red
    74  -> one $ Note Medium $ Pro Yellow ()
    75  -> one $ Note Medium $ Pro Blue   ()
    76  -> one $ Note Medium $ Pro Green  ()

    84  -> one $ Note Hard Kick
    85  -> one $ Note Hard Red
    86  -> one $ Note Hard $ Pro Yellow ()
    87  -> one $ Note Hard $ Pro Blue   ()
    88  -> one $ Note Hard $ Pro Green  ()

    96  -> one $ Note Expert Kick
    97  -> one $ Note Expert Red
    98  -> one $ Note Expert $ Pro Yellow ()
    99  -> one $ Note Expert $ Pro Blue   ()
    100 -> one $ Note Expert $ Pro Green  ()

    103 -> one $ Solo True
    110 -> one $ ProType Yellow Tom
    111 -> one $ ProType Blue   Tom
    112 -> one $ ProType Green  Tom
    116 -> one $ Overdrive True
    120 -> one $ Activation True
    121 -> Just []
    122 -> Just []
    123 -> Just []
    124 -> Just []
    126 -> one $ SingleRoll True
    127 -> one $ DoubleRoll True
    _   -> Nothing
  noteOff p = case V.fromPitch p of
    60  -> Just []
    61  -> Just []
    62  -> Just []
    63  -> Just []
    64  -> Just []

    72  -> Just []
    73  -> Just []
    74  -> Just []
    75  -> Just []
    76  -> Just []

    84  -> Just []
    85  -> Just []
    86  -> Just []
    87  -> Just []
    88  -> Just []

    96  -> Just []
    97  -> Just []
    98  -> Just []
    99  -> Just []
    100 -> Just []
    
    103 -> one $ Solo False
    110 -> one $ ProType Yellow Cymbal
    111 -> one $ ProType Blue   Cymbal
    112 -> one $ ProType Green  Cymbal
    116 -> one $ Overdrive False
    120 -> one $ Activation False
    121 -> Just []
    122 -> Just []
    123 -> Just []
    124 -> Just []
    126 -> one $ SingleRoll False
    127 -> one $ DoubleRoll False
    _   -> Nothing
  in case e of
    E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p vel))) ->
      (if V.fromVelocity vel == 0 then noteOff else noteOn) p
    E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p _))) -> noteOff p
    E.MetaEvent (Meta.TextEvent s) -> fmap (: []) $ readMixEvent s
    _ -> Nothing

-- | e.g. turns 'D2' and 'Disco' into @\"drums2d\"@
showMix :: Audio -> Disco -> String
showMix audio disco = "drums" ++ drop 1 (show audio) ++ case disco of
  NoDisco     -> ""
  Disco       -> "d"
  DiscoNoFlip -> "dnoflip"
  EasyMix     -> "easy"
  EasyNoKick  -> "easynokick"

-- | Tries to interpret a string as an audio mix event.
readMixEvent :: String -> Maybe Event
readMixEvent s = readCommand s >>= \case
  ["mix", x, y]
    | Just diff <- lookup x $ zip (words "0 1 2 3") [Easy .. Expert]
    , Just (audio, disco) <- lookup y $ do
      audio <- [minBound .. maxBound]
      disco <- [minBound .. maxBound]
      return (showMix audio disco, (audio, disco))
    -> Just $ Mix diff audio disco
  _ -> Nothing

showEvent :: Event -> RTB.T U.Beats E.T
showEvent = \case
  ProType Yellow ptype -> one $ note (ptype == Tom) 110
  ProType Blue   ptype -> one $ note (ptype == Tom) 111
  ProType Green  ptype -> one $ note (ptype == Tom) 112
  Mix diff audio disco -> one $ E.MetaEvent $ Meta.TextEvent $
    showCommand ["mix", show $ fromEnum diff, showMix audio disco]
  SingleRoll b -> one $ note b 126
  DoubleRoll b -> one $ note b 127
  Overdrive  b -> one $ note b 116
  Activation b -> foldr (RTB.cons 0) RTB.empty $ map (note b) [120 .. 124]
  Solo       b -> one $ note b 103
  Note diff gem -> let
    p = 60 + 12 * fromEnum diff + case gem of
      Kick          -> 0
      Red           -> 1
      Pro Yellow () -> 2
      Pro Blue   () -> 3
      Pro Green  () -> 4
    in RTB.fromPairList [(0, note True p), (1/32, note False p)]
  where one x = RTB.singleton 0 x
        note True  p = ch0 $ V.NoteOn  (V.toPitch p) (V.toVelocity 96)
        note False p = ch0 $ V.NoteOff (V.toPitch p) (V.toVelocity 0 )
        ch0 = E.MIDIEvent . C.Cons (C.toChannel 0) . C.Voice
