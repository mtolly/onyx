{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
module RockBand.Drums
( Animation(..), Audio(..), Disco(..), Gem(..)
, Hand(..), Hit(..), PSGem(..), ProColor(..), ProType(..)
, Event(..), DiffEvent(..)
, assignToms
, assignPSReal
, psRealToPro
, baseScore
, perfectSoloBonus
, drumsFromLegacy, drumsToLegacy
) where

import qualified Data.EventList.Relative.TimeBody as RTB
import           Guitars                          (applyStatus)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Drums
import           RockBand.Common

-- | Constructors are ordered for optimal processing with 'RTB.normalize'.
-- For example, 'Note' comes last so that everything else is processed first.
-- Also, @'ProType' _ 'Cymbal'@ comes before @'ProType' _ 'Tom'@, since the
-- note-on ('Tom') should supercede the simultaneous note-off ('Cymbal').
data Event
  = Mood       Mood
  | ProType    ProColor ProType
  | SingleRoll Bool
  | DoubleRoll Bool
  | Overdrive  Bool -- ^ white notes to gain energy
  | Activation Bool -- ^ drum fill to activate Overdrive, or BRE
  | Solo       Bool
  | Player1    Bool
  | Player2    Bool
  | DiffEvent  Difficulty DiffEvent
  | Kick2x -- ^ Used as input to the build tool for 2x Bass Pedal notes
  | Animation  Animation
  deriving (Eq, Ord, Show, Read)

data DiffEvent
  = Mix Audio Disco
  | PSHihatOpen    Bool
  | PSHihatPedal   Bool
  | PSSnareRimshot Bool
  | PSHihatSizzle  Bool
  | Note (Gem ())
  deriving (Eq, Ord, Show, Read)

instance HasDiffEvent DiffEvent Event where
  makeDiffEvent = DiffEvent
  unmakeDiffEvent = \case
    DiffEvent d e -> Just (d, e)
    _             -> Nothing

data DrumState = DrumState
  { yellowType  :: ProType
  , blueType    :: ProType
  , greenType   :: ProType
  , easyDisco   :: Bool
  , mediumDisco :: Bool
  , hardDisco   :: Bool
  , expertDisco :: Bool
  } deriving (Eq, Ord, Show, Read)

defDrumState :: DrumState
defDrumState = DrumState Cymbal Cymbal Cymbal False False False False

assignToms :: (NNC.C t) => Bool -> RTB.T t Event -> RTB.T t (Difficulty, Gem ProType)
assignToms expert2x = go defDrumState . RTB.normalize where
  go ds rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, x), rtb') -> case x of
      ProType color ptype -> RTB.delay dt $ case color of
        Yellow -> go ds{ yellowType = ptype } rtb'
        Blue   -> go ds{ blueType   = ptype } rtb'
        Green  -> go ds{ greenType  = ptype } rtb'
      DiffEvent diff (Mix _ dsc) -> RTB.delay dt $ case diff of
        Easy   -> go ds{ easyDisco   = b } rtb'
        Medium -> go ds{ mediumDisco = b } rtb'
        Hard   -> go ds{ hardDisco   = b } rtb'
        Expert -> go ds{ expertDisco = b } rtb'
        where b = dsc == Disco
      DiffEvent diff (Note gem) -> case gem of
        Kick -> RTB.cons dt (diff, Kick) $ go ds rtb'
        Red -> if isDisco
          then RTB.cons dt (diff, Pro Yellow Cymbal) $ go ds rtb'
          else RTB.cons dt (diff, Red) $ go ds rtb'
        Pro color () -> let
          new = case color of
            Yellow -> if isDisco
              then Red
              else Pro Yellow $ yellowType ds
            Blue   -> Pro Blue $ blueType ds
            Green  -> Pro Green $ greenType ds
          in RTB.cons dt (diff, new) $ go ds rtb'
        Orange -> RTB.cons dt (diff, Orange) $ go ds rtb'
        where isDisco = case diff of
                Easy   -> easyDisco ds
                Medium -> mediumDisco ds
                Hard   -> hardDisco ds
                Expert -> expertDisco ds
      Kick2x -> if expert2x
        then RTB.cons dt (Expert, Kick) $ go ds rtb'
        else RTB.delay dt $ go ds rtb'
      _ -> RTB.delay dt $ go ds rtb'

assignPSReal :: (NNC.C t) => Bool -> RTB.T t Event -> RTB.T t (Difficulty, Either PSGem (Gem ProType))
assignPSReal expert2x rtb = let
  pro = assignToms expert2x rtb
  status = flip RTB.mapMaybe rtb $ \case
    DiffEvent d (PSHihatOpen b) -> Just ((d, HHOpen), b)
    DiffEvent d (PSHihatPedal b) -> Just ((d, HHPedal), b)
    DiffEvent d (PSSnareRimshot b) -> Just ((d, Rimshot), b)
    DiffEvent d (PSHihatSizzle b) -> Just ((d, HHSizzle), b)
    _ -> Nothing
  in flip fmap (applyStatus status pro) $ \case
    (mods, (d, Red))
      | elem (d, Rimshot) mods -> (d, Left Rimshot)
    (mods, (d, Pro Yellow Cymbal))
      | elem (d, HHOpen) mods -> (d, Left HHOpen)
      | elem (d, HHPedal) mods -> (d, Left HHPedal)
      | elem (d, HHSizzle) mods -> (d, Left HHSizzle)
    (_, (d, gem)) -> (d, Right gem)

drumsFromLegacy :: (NNC.C t) => RTB.T t Event -> DrumTrack t
drumsFromLegacy = undefined

drumsToLegacy :: (NNC.C t) => DrumTrack t -> RTB.T t Event
drumsToLegacy = undefined
