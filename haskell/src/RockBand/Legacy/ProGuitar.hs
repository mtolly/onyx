{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
module RockBand.Legacy.ProGuitar
( GtrChannel(..), GtrFret, GtrString(..), NoteType(..), SlideType(..), StrumArea(..)
, Event(..), DiffEvent(..)
, standardGuitar
, standardBass
, autoHandPosition
, autoChordRoot
, lowerOctaves
, guitarifyHOPO
, makeChordName
, pgFromLegacy, pgToLegacy
) where

import           Control.Monad                    (guard)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Guitars                          (applyStatus, guitarify,
                                                   trackState)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.ProGuitar
import           RockBand.Common
import qualified Sound.MIDI.Util                  as U

data Event
  = TrainerGtr   Trainer
  | TrainerBass  Trainer
  | OnyxOctave   GtrFret -- "move these notes 12 frets down if needed" section
  | HandPosition GtrFret
  | ChordRoot    Key
  | NoChordNames Bool
  | SlashChords  Bool
  | Trill        Bool
  | Tremolo      Bool
  | BREGuitar    Bool
  | BREBass      Bool
  | Overdrive    Bool
  | Solo         Bool
  | FlatChords   Bool
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
  , pgFlatChords   = RTB.mapMaybe (\case FlatChords x -> Just x; _ -> Nothing) leg
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
  , FlatChords <$> pgFlatChords o
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

standardGuitar :: [Int]
standardGuitar = [40, 45, 50, 55, 59, 64]

standardBass :: [Int]
standardBass = [28, 33, 38, 43, 47, 52]
-- last 2 are just gtr one octave down, as observed in game
-- (these aren't super useful, and can't be changed by .dta tuning)

-- | If there are no hand positions, adds one to every note.
autoHandPosition :: (NNC.C t) => RTB.T t Event -> RTB.T t Event
autoHandPosition rtb = let
  mapInstant evts = let
    frets = evts >>= \case
      DiffEvent _ (Note (NoteOn fret _)) -> [fret]
      DiffEvent _ (Note (Blip   fret _)) -> [fret]
      _ -> []
      -- note, we do take ArpeggioForm notes into account because Magma does too
    in case frets of
      [] -> evts
      _  -> case filter (/= 0) frets of
        []     -> HandPosition 0 : evts
        f : fs -> HandPosition (foldr min f fs) : evts
  in if any (\case HandPosition{} -> True; _ -> False) rtb
    then rtb
    else RTB.flatten $ fmap mapInstant $ RTB.collectCoincident rtb

-- | If there are no chord root notes, sets each chord to have its lowest
-- pitch as the root.
autoChordRoot :: (NNC.C t) => [Int] -> RTB.T t Event -> RTB.T t Event
autoChordRoot tuning rtb = let
  getPitch str fret = (tuning !! fromEnum str) + fret
  -- TODO verify that this doesn't do weird things
  -- if there's a different chord in the middle of an arpeggio section
  mapInstant evts = let
    pitches = evts >>= \case
      DiffEvent _ (Note (NoteOn fret (str, _))) -> [getPitch str fret]
      DiffEvent _ (Note (Blip   fret (str, _))) -> [getPitch str fret]
      _ -> []
    in case pitches of
      []     -> evts
      p : ps -> ChordRoot (toEnum $ foldr min p ps `rem` 12) : evts
  -- TODO maybe remove duplicate roots
  in if any (\case ChordRoot{} -> True; _ -> False) rtb
    then rtb
    else RTB.flatten $ fmap mapInstant $ RTB.collectCoincident rtb

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

guitarifyHOPO :: U.Beats -> RTB.T U.Beats DiffEvent
  -> RTB.T U.Beats (StrumHOPOTap, [(GtrString, GtrFret, NoteType)], Maybe U.Beats)
guitarifyHOPO threshold rtb = let
  notes = RTB.mapMaybe (\case Note ln -> Just ln; _ -> Nothing) rtb
  gtr = joinEdges $ guitarify $ splitEdges
    $ (\(fret, (str, ntype), len) -> ((), (str, fret, ntype), len))
    <$> joinEdges notes
  withForce = applyStatus (RTB.mapMaybe (\case ForceHOPO b -> Just (HOPO, b); _ -> Nothing) rtb) gtr
  fn prev dt (forces, ((), gems, len)) = let
    gems' = [ gem | gem@(_, _, nt) <- gems, nt /= ArpeggioForm ]
    ntype = if all (\(_, _, nt) -> nt == Tapped) gems'
      then Tap
      else case forces of
        nt : _ -> nt
        [] -> if dt >= threshold -- TODO: should this be > or >= ?
          then Strum
          else case prev of
            Nothing -> Strum
            Just prevGems -> if null [ () | (_, _, Muted) <- prevGems ]
              then case gems of
                -- note: gems above, not gems'.
                -- if there are arpeggio form notes and one normal note,
                -- we still count it as a chord for auto-hopo purposes.
                -- doesn't make sense, but that's what rb3 does!
                [(str, fret, _)] -> let
                  canHOPOFrom (str', fret', _) = str == str' && fret /= fret'
                  in if any canHOPOFrom prevGems then HOPO else Strum
                _ -> Strum
              else Strum -- after muted note, next note is not auto hopo
    in (Just gems', Just (ntype, gems', len))
  in trackState Nothing fn withForce

-- | Replicates the Pro Guitar chord name algorithm from RB3.
-- This has been verified to match RB3 for each of the 1486 possible chords.
makeChordName :: Key -> Set.Set Key -> Bool -> String
makeChordName root notes flat = let
  s n = toEnum $ (fromEnum root + n) `rem` 12
  only n = Set.toList (Set.delete root notes) == [n]

  dim2 = Set.member (s  1) notes
  nat2 = Set.member (s  2) notes
  min3 = Set.member (s  3) notes
  maj3 = Set.member (s  4) notes
  nat4 = Set.member (s  5) notes
  dim5 = Set.member (s  6) notes
  aug5 = Set.member (s  8) notes
  nat6 = Set.member (s  9) notes
  min7 = Set.member (s 10) notes
  maj7 = Set.member (s 11) notes

  (base, super)

    | only (s  1) = ("(b2)", "")
    | only (s  2) = ("(2)" , "")
    | only (s  5) = ("(4)" , "")
    | only (s  6) = ("(b5)", "")
    | only (s  7) = ("5"   , "")
    | only (s  8) = ("(b6)", "")
    | only (s  9) = ("(6)" , "")
    | only (s 10) = ("(b7)", "")
    | only (s 11) = ("(7)" , "")

    | maj3 || min3 = let

      sharp9 = guard (maj3 && min3) >> "#9"
      four   = guard nat4           >> "4"
      six    = guard nat6           >> "6"

      in if min7 || maj7

        then let
          b = (if maj3 then "" else "m") ++ if
            | maj7 && not min7           -> "M7"
            | not (nat2 || nat4 || nat6) -> "7"
            | maj7                       -> "M7"
            | otherwise                  -> ""
          flat9  = guard dim2 >> "b9"
          nine   = guard nat2 >> "9"
          flat13 = guard aug5 >> "b13"
          fives
            | aug5 && dim5 = "+-5"
            | dim5         = "b5"
            | otherwise    = ""
          in (b, concat [fives, nine, flat9, sharp9, four, six, flat13])

        else let
          (b, start) = if dim5
            then if
              | maj3      -> ("", "#4" )
              | aug5      -> ("", "0#5")
              | otherwise -> ("", "0"  )
            else if
              | maj3 && aug5 -> ("" , "+" )
              | maj3         -> ("" , ""  )
              | aug5         -> ("m", "#5")
              | otherwise    -> ("m", ""  )
          two
            | dim2      = "b2"
            | nat2      = "2"
            | otherwise = ""
          in (b, concat [start, sharp9, two, four, six])

    | otherwise =
      ( if      min7 then "7"
        else if maj7 then "M7"
        else              ""
      , if nat4 && nat6 then "sus4/6"
        else if nat4 then "sus4"
        else if nat6 then "sus6"
        else if dim5 then "sus#4"
        else if nat2 then "sus2"
        else if dim2 then "susb2"
        else ""
      )

  in showKey flat root ++ base ++ case super of
    "" -> ""
    _  -> "<gtr>" ++ super ++ "</gtr>"
