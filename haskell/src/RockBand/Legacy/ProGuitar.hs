{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module RockBand.Legacy.ProGuitar
( GtrChannel(..), GtrFret, GtrString(..), NoteType(..), SlideType(..), StrumArea(..)
, Event(..), DiffEvent(..)
, standardGuitar
, standardBass
, lowerOctaves
, pgFromLegacy, pgToLegacy
) where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           Guitars                          (applyStatus)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.ProGuitar
import           RockBand.Common

data Event
  = TrainerGtr   Trainer
  | TrainerBass  Trainer
  | OnyxOctave   GtrFret -- "move these notes 12 frets down if needed" section
  | HandPosition GtrFret
  | ChordRoot    Key
  | NoChordNames Bool
  | SlashChords  Bool
  | Trill        (Maybe LaneDifficulty)
  | Tremolo      (Maybe LaneDifficulty)
  | BREGuitar    Bool
  | BREBass      Bool
  | Overdrive    Bool
  | Solo         Bool
  | SwapAccidental   Bool
  | Mystery45    Bool
  | Mystery69    Bool
  | Mystery93    Bool
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show, Read)

data DiffEvent
  = ChordName (Maybe T.Text)
  | ForceHOPO    Bool
  | Slide        Bool SlideType
  | Arpeggio     Bool
  | PartialChord Bool StrumArea
  | AllFrets     Bool
  | MysteryBFlat Bool
  -- TODO EOF format sysexes
  | Note (LongNote GtrFret (GtrString, NoteType))
  deriving (Eq, Ord, Show, Read)

pgFromLegacy :: (NNC.C t) => RTB.T t Event -> ProGuitarTrack t
pgFromLegacy leg = ProGuitarTrack
  { pgDifficulties = Map.fromList $ do
    d <- each
    let leg' = RTB.mapMaybe (\case DiffEvent d' e | d == d' -> Just e; _ -> Nothing) leg
    return (d, ProGuitarDifficulty
      { pgChordName    = RTB.mapMaybe (\case ChordName x -> Just x; _ -> Nothing) leg'
      , pgForceHOPO    = RTB.mapMaybe (\case ForceHOPO x -> Just x; _ -> Nothing) leg'
      , pgSlide        = RTB.mapMaybe (\case Slide x y -> Just (y, x); _ -> Nothing) leg'
      , pgArpeggio     = RTB.mapMaybe (\case Arpeggio x -> Just x; _ -> Nothing) leg'
      , pgPartialChord = RTB.mapMaybe (\case PartialChord x y -> Just (y, x); _ -> Nothing) leg'
      , pgAllFrets     = RTB.mapMaybe (\case AllFrets x -> Just x; _ -> Nothing) leg'
      , pgMysteryBFlat = RTB.mapMaybe (\case MysteryBFlat x -> Just x; _ -> Nothing) leg'
      , pgNotes        = fmap (\(fret, (str, nt), mlen) -> (str, (nt, fret, mlen))) $ joinEdges $ RTB.mapMaybe (\case Note x -> Just x; _ -> Nothing) leg'
      })
  , pgTrainer      = RTB.mapMaybe (\case TrainerGtr x -> Just (TypeGuitar, x); TrainerBass x -> Just (TypeBass, x); _ -> Nothing) leg
  , pgTremolo      = RTB.mapMaybe (\case Tremolo x -> Just x; _ -> Nothing) leg
  , pgTrill        = RTB.mapMaybe (\case Trill x -> Just x; _ -> Nothing) leg
  , pgOverdrive    = RTB.mapMaybe (\case Overdrive x -> Just x; _ -> Nothing) leg
  , pgBRE          = RTB.mapMaybe (\case BREGuitar x -> Just (TypeGuitar, x); BREBass x -> Just (TypeBass, x); _ -> Nothing) leg
  , pgSolo         = RTB.mapMaybe (\case Solo x -> Just x; _ -> Nothing) leg
  , pgHandPosition = RTB.mapMaybe (\case HandPosition x -> Just x; _ -> Nothing) leg
  , pgChordRoot    = RTB.mapMaybe (\case ChordRoot x -> Just x; _ -> Nothing) leg
  , pgNoChordNames = RTB.mapMaybe (\case NoChordNames x -> Just x; _ -> Nothing) leg
  , pgSlashChords  = RTB.mapMaybe (\case SlashChords x -> Just x; _ -> Nothing) leg
  , pgSwapAccidental   = RTB.mapMaybe (\case SwapAccidental x -> Just x; _ -> Nothing) leg
  , pgOnyxOctave   = RTB.mapMaybe (\case OnyxOctave x -> Just x; _ -> Nothing) leg
  , pgMystery45    = RTB.mapMaybe (\case Mystery45 x -> Just x; _ -> Nothing) leg
  , pgMystery69    = RTB.mapMaybe (\case Mystery69 x -> Just x; _ -> Nothing) leg
  , pgMystery93    = RTB.mapMaybe (\case Mystery93 x -> Just x; _ -> Nothing) leg
  }

pgToLegacy :: (NNC.C t) => ProGuitarTrack t -> RTB.T t Event
pgToLegacy o = foldr RTB.merge RTB.empty
  [ (\case (TypeGuitar, x) -> TrainerGtr x; (TypeBass, x) -> TrainerBass x) <$> pgTrainer o
  , Tremolo <$> pgTremolo o
  , Trill <$> pgTrill o
  , Overdrive <$> pgOverdrive o
  , (\case (TypeGuitar, x) -> BREGuitar x; (TypeBass, x) -> BREBass x) <$> pgBRE o
  , Solo <$> pgSolo o
  , HandPosition <$> pgHandPosition o
  , ChordRoot <$> pgChordRoot o
  , NoChordNames <$> pgNoChordNames o
  , SlashChords <$> pgSlashChords o
  , SwapAccidental <$> pgSwapAccidental o
  , OnyxOctave <$> pgOnyxOctave o
  , Mystery45 <$> pgMystery45 o
  , Mystery69 <$> pgMystery69 o
  , Mystery93 <$> pgMystery93 o
  , foldr RTB.merge RTB.empty $ do
    (diff, fd) <- Map.toList $ pgDifficulties o
    map (fmap $ DiffEvent diff)
      [ ChordName <$> pgChordName fd
      , ForceHOPO <$> pgForceHOPO fd
      , uncurry (flip Slide) <$> pgSlide fd
      , Arpeggio <$> pgArpeggio fd
      , uncurry (flip PartialChord) <$> pgPartialChord fd
      , AllFrets <$> pgAllFrets fd
      , MysteryBFlat <$> pgMysteryBFlat fd
      , fmap Note $ splitEdges $ fmap (\(str, (nt, fret, len)) -> (fret, (str, nt), len)) $ pgNotes fd
      ]
  ]

instance HasDiffEvent DiffEvent Event where
  makeDiffEvent = DiffEvent
  unmakeDiffEvent = \case
    DiffEvent d e -> Just (d, e)
    _             -> Nothing

-- | Ensures that frets do not go above the given maximum,
-- first by lowering 'OnyxOctave' sections and then by simple clamping.
lowerOctaves :: (NNC.C t) => Int -> RTB.T t Event -> RTB.T t Event
lowerOctaves maxFret rtb = let
  (octs, notOcts) = RTB.partitionMaybe (\case OnyxOctave f -> Just f; _ -> Nothing) rtb
  shouldLower = fmap (\f -> ((), f >= maxFret)) octs
  doLower _     0 = 0
  doLower units n = min maxFret $ if null units || n < 12 then n else n - 12
  lowerDiff devts = let
    (notes, notNotes) = RTB.partitionMaybe (\case Note ln -> Just ln; _ -> Nothing) devts
    lowered
      = splitEdges
      $ fmap (\(units, (s, a, mt)) -> (doLower units s, a, mt))
      $ applyStatus shouldLower
      $ joinEdges notes
    in RTB.merge (fmap Note lowered) notNotes
  (hands, notHands) = RTB.partitionMaybe (\case HandPosition f -> Just f; _ -> Nothing) notOcts
  hands'
    = fmap (uncurry doLower)
    $ applyStatus shouldLower hands
  in eachDifficulty lowerDiff $ RTB.merge (fmap HandPosition hands') notHands
