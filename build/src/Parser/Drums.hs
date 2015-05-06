{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Parser.Drums
( Event(..), Gem(..), ProColor(..), ProType(..), Audio(..), Disco(..)
, readEvent, showEvent
) where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
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
readEvent e = case e of
  MIDINote p b -> case V.fromPitch p of
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
  ProType Yellow ptype -> one $ edge' 110 (ptype == Tom)
  ProType Blue   ptype -> one $ edge' 111 (ptype == Tom)
  ProType Green  ptype -> one $ edge' 112 (ptype == Tom)
  Mix diff audio disco -> one $ E.MetaEvent $ Meta.TextEvent $
    showCommand ["mix", show $ fromEnum diff, showMix audio disco]
  SingleRoll b -> one $ edge' 126 b
  DoubleRoll b -> one $ edge' 127 b
  Overdrive  b -> one $ edge' 116 b
  Activation b -> foldr (RTB.cons 0) RTB.empty [ edge' p b | p <- [120 .. 124] ]
  Solo       b -> one $ edge' 103 b
  Note diff gem -> blip $ V.toPitch $ 60 + 12 * fromEnum diff + case gem of
    Kick          -> 0
    Red           -> 1
    Pro Yellow () -> 2
    Pro Blue   () -> 3
    Pro Green  () -> 4
  where one x = RTB.singleton 0 x
