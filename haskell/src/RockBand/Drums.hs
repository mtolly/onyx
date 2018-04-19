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
import qualified Data.Map                         as Map
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
drumsFromLegacy leg = DrumTrack
  { drumDifficulties = Map.fromList $ do
    d <- each
    let leg' = RTB.mapMaybe (\case DiffEvent d' e | d == d' -> Just e; _ -> Nothing) leg
    return (d, DrumDifficulty
      { drumMix         = RTB.mapMaybe (\case Mix x y -> Just (x, y); _ -> Nothing) leg'
      , drumPSModifiers = flip RTB.mapMaybe leg' $ \case
        PSHihatOpen    b -> Just (HHOpen, b)
        PSHihatPedal   b -> Just (HHPedal, b)
        PSSnareRimshot b -> Just (Rimshot, b)
        PSHihatSizzle  b -> Just (HHSizzle, b)
        _ -> Nothing
      , drumGems        = RTB.mapMaybe (\case Note x -> Just x; _ -> Nothing) leg'
      })
  , drumMood         = RTB.mapMaybe (\case Mood x -> Just x; _ -> Nothing) leg
  , drumToms         = RTB.mapMaybe (\case ProType x y -> Just (x, y); _ -> Nothing) leg
  , drumSingleRoll   = RTB.mapMaybe (\case SingleRoll x -> Just x; _ -> Nothing) leg
  , drumDoubleRoll   = RTB.mapMaybe (\case DoubleRoll x -> Just x; _ -> Nothing) leg
  , drumOverdrive    = RTB.mapMaybe (\case Overdrive x -> Just x; _ -> Nothing) leg
  , drumActivation   = RTB.mapMaybe (\case Activation x -> Just x; _ -> Nothing) leg
  , drumSolo         = RTB.mapMaybe (\case Solo x -> Just x; _ -> Nothing) leg
  , drumPlayer1      = RTB.mapMaybe (\case Player1 x -> Just x; _ -> Nothing) leg
  , drumPlayer2      = RTB.mapMaybe (\case Player2 x -> Just x; _ -> Nothing) leg
  , drumKick2x       = RTB.mapMaybe (\case Kick2x -> Just (); _ -> Nothing) leg
  , drumAnimation    = RTB.mapMaybe (\case Animation x -> Just x; _ -> Nothing) leg
  }

drumsToLegacy :: (NNC.C t) => DrumTrack t -> RTB.T t Event
drumsToLegacy o = foldr RTB.merge RTB.empty
  [ Mood            <$> drumMood o
  , uncurry ProType <$> drumToms o
  , SingleRoll      <$> drumSingleRoll  o
  , DoubleRoll      <$> drumDoubleRoll o
  , Overdrive       <$> drumOverdrive o
  , Activation      <$> drumActivation o
  , Solo            <$> drumSolo o
  , Player1         <$> drumPlayer1 o
  , Player2         <$> drumPlayer2 o
  , const Kick2x    <$> drumKick2x o
  , Animation       <$> drumAnimation o
  , foldr RTB.merge RTB.empty $ do
    (diff, fd) <- Map.toList $ drumDifficulties o
    map (fmap $ DiffEvent diff)
      [ uncurry Mix <$> drumMix fd
      , flip fmap (drumPSModifiers fd) $ \case
        (HHOpen, b) -> PSHihatOpen b
        (HHPedal, b) -> PSHihatPedal b
        (Rimshot, b) -> PSSnareRimshot b
        (HHSizzle, b) -> PSHihatSizzle b
      , Note <$> drumGems fd
      ]
  ]
