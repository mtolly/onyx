{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module RockBand.ProGuitar where

import           Control.Monad                    (guard)
import           Data.Data
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Maybe                       (isJust)
import           Data.Monoid                      ((<>))
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Guitars                          (applyStatus, guitarify,
                                                   trackState)
import           Language.Haskell.TH
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.FiveButton              (StrumHOPO (..))
import           RockBand.Parse
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

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
  deriving (Eq, Ord, Show, Read, Data, Typeable)

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
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data NoteType
  = NormalNote
  | ArpeggioForm
  | Bent
  | Muted
  | Tapped
  | Harmonic
  | PinchHarmonic
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

data SlideType = NormalSlide | ReversedSlide | MysterySlide3 | MysterySlide2
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

data StrumArea = High | Mid | Low | MysteryStrum0
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

type GtrFret = Int

data GtrString = S6 | S5 | S4 | S3 | S2 | S1
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

class (Enum a, Bounded a) => GtrChannel a where
  encodeChannel :: a -> Int
  channelMap :: [(Int, a)]
  channelMap = [ (encodeChannel x, x) | x <- [minBound .. maxBound] ]
instance GtrChannel NoteType where
  encodeChannel = fromEnum
instance GtrChannel SlideType where
  encodeChannel = \case
    NormalSlide   -> 0
    ReversedSlide -> 11
    MysterySlide3 -> 3
    MysterySlide2 -> 2
instance GtrChannel StrumArea where
  encodeChannel = \case
    High -> 13
    Mid  -> 14
    Low  -> 15
    MysteryStrum0 -> 0

parseNoteBlip :: Int -> Difficulty -> GtrString -> ParseOne U.Beats E.T Event
parseNoteBlip pitch diff str rtb = do
  ((t, (c, p, v)), rtb') <- parseBlipCPVMax (1/3) rtb
  guard $ p == pitch
  guard $ v >= 100
  ntype <- lookup c channelMap
  let e = DiffEvent diff $ Note $ Blip (v - 100) (str, ntype)
  return ((t, e), rtb')

parseLongNoteEdge :: (NNC.C t) => Int -> Difficulty -> GtrString -> ParseOne t E.T Event
parseLongNoteEdge pitch diff str rtb = do
  ((t, (c, p, v)), rtb') <- firstEventWhich isNoteEdgeCPV rtb
  guard $ p == pitch
  guard $ maybe True (>= 100) v
  ntype <- lookup c channelMap
  let e = DiffEvent diff $ Note $ case v of
        Nothing  -> NoteOff (str, ntype)
        Just vel -> NoteOn (vel - 100) (str, ntype)
  return ((t, e), rtb')

parseSlide :: (NNC.C t) => Int -> Difficulty -> ParseOne t E.T Event
parseSlide pitch diff rtb = do
  ((t, (c, p, v)), rtb') <- firstEventWhich isNoteEdgeCPV rtb
  guard $ p == pitch
  stype <- lookup c channelMap
  let e = DiffEvent diff $ Slide (isJust v) stype
  return ((t, e), rtb')

parsePartialChord :: (NNC.C t) => Int -> Difficulty -> ParseOne t E.T Event
parsePartialChord pitch diff rtb = do
  ((t, (c, p, v)), rtb') <- firstEventWhich isNoteEdgeCPV rtb
  guard $ p == pitch
  area <- lookup c channelMap
  let e = DiffEvent diff $ PartialChord (isJust v) area
  return ((t, e), rtb')

parseHandPosition :: (NNC.C t) => ParseOne t E.T Event
parseHandPosition rtb = do
  ((t, (_, p, v)), rtb') <- parseBlipCPV rtb
  guard $ p == 108
  guard $ v >= 100
  return ((t, HandPosition $ v - 100), rtb')

instanceMIDIEvent [t| Event |] Nothing $ let
  note :: Int -> Q Pat -> Q Pat -> [(Q Exp, Q Exp)]
  note pitch diff str =
    [ ( [e| one $ parseNoteBlip pitch $(fmap patToExp diff) $(fmap patToExp str) |]
      , [e| \case
        DiffEvent $diff (Note (Blip fret ($str, ntype))) ->
          unparseBlipCPV (encodeChannel ntype, pitch, fret + 100)
        |]
      )
    , ( [e| one $ parseLongNoteEdge pitch $(fmap patToExp diff) $(fmap patToExp str) |]
      , [e| \case
        DiffEvent $diff (Note (NoteOn fret ($str, ntype))) ->
          RTB.singleton 0 $ makeEdgeCPV (encodeChannel ntype) pitch $ Just $ fret + 100
        DiffEvent $diff (Note (NoteOff ($str, ntype))) ->
          RTB.singleton 0 $ makeEdgeCPV (encodeChannel ntype) pitch Nothing
        |]
      )
    ]
  -- Nemo's MIDI checker complains (incorrectly) if slide notes have velocity < 100.
  slide :: Int -> Q Pat -> (Q Exp, Q Exp)
  slide pitch diff =
    ( [e| one $ parseSlide pitch $(fmap patToExp diff) |]
    , [e| \case
      DiffEvent $diff (Slide b stype) ->
        RTB.singleton 0 $ makeEdgeCPV (encodeChannel stype) pitch $ guard b >> Just 100
      |]
    )
  partialChord :: Int -> Q Pat -> (Q Exp, Q Exp)
  partialChord pitch diff =
    ( [e| one $ parsePartialChord pitch $(fmap patToExp diff) |]
    , [e| \case
      DiffEvent $diff (PartialChord b area) ->
        RTB.singleton 0 $ makeEdgeCPV (encodeChannel area) pitch $ guard b >> Just 100
      |]
    )
  in  [ blip 4  [p| ChordRoot E  |]
      , blip 5  [p| ChordRoot F  |]
      , blip 6  [p| ChordRoot Fs |]
      , blip 7  [p| ChordRoot G  |]
      , blip 8  [p| ChordRoot Gs |]
      , blip 9  [p| ChordRoot A  |]
      , blip 10 [p| ChordRoot As |]
      , blip 11 [p| ChordRoot B  |]
      , blip 12 [p| ChordRoot C  |]
      , blip 13 [p| ChordRoot Cs |]
      , blip 14 [p| ChordRoot D  |]
      , blip 15 [p| ChordRoot Ds |]

      , edge 16 $ \_b -> [p| SlashChords  $(boolP _b) |]
      , edge 17 $ \_b -> [p| NoChordNames $(boolP _b) |]
      , edge 18 $ \_b -> [p| FlatChords   $(boolP _b) |]

      ] ++ note 24  [p| Easy |] [p| S6 |]
        ++ note 25  [p| Easy |] [p| S5 |]
        ++ note 26  [p| Easy |] [p| S4 |]
        ++ note 27  [p| Easy |] [p| S3 |]
        ++ note 28  [p| Easy |] [p| S2 |]
        ++ note 29  [p| Easy |] [p| S1 |] ++
      [ edge 30 $ \_b -> [p| DiffEvent Easy (ForceHOPO $(boolP _b)) |]
      , slide 31 [p| Easy |]
      , edge 32 $ \_b -> [p| DiffEvent Easy (Arpeggio $(boolP _b)) |]
      , partialChord 33 [p| Easy |]
      -- TODO (not actually seen)
      , edge 34 $ \_b -> [p| DiffEvent Easy (MysteryBFlat $(boolP _b)) |]
      , edge 35 $ \_b -> [p| DiffEvent Easy (AllFrets $(boolP _b)) |]

      -- TODO
      , edge 45 $ \_b -> [p| Mystery45 $(boolP _b) |]

      ] ++ note 48  [p| Medium |] [p| S6 |]
        ++ note 49  [p| Medium |] [p| S5 |]
        ++ note 50  [p| Medium |] [p| S4 |]
        ++ note 51  [p| Medium |] [p| S3 |]
        ++ note 52  [p| Medium |] [p| S2 |]
        ++ note 53  [p| Medium |] [p| S1 |] ++
      [ edge 54 $ \_b -> [p| DiffEvent Medium (ForceHOPO $(boolP _b)) |]
      , slide 55 [p| Medium |]
      , edge 56 $ \_b -> [p| DiffEvent Medium (Arpeggio $(boolP _b)) |]
      , partialChord 57 [p| Medium |]
      -- TODO
      , edge 58 $ \_b -> [p| DiffEvent Medium (MysteryBFlat $(boolP _b)) |]
      , edge 59 $ \_b -> [p| DiffEvent Medium (AllFrets $(boolP _b)) |]

      -- TODO
      , edge 69 $ \_b -> [p| Mystery69 $(boolP _b) |]

      ] ++ note 72  [p| Hard |] [p| S6 |]
        ++ note 73  [p| Hard |] [p| S5 |]
        ++ note 74  [p| Hard |] [p| S4 |]
        ++ note 75  [p| Hard |] [p| S3 |]
        ++ note 76  [p| Hard |] [p| S2 |]
        ++ note 77  [p| Hard |] [p| S1 |] ++
      [ edge 78 $ \_b -> [p| DiffEvent Hard (ForceHOPO $(boolP _b)) |]
      , slide 79 [p| Hard |] -- TODO: channel 3
      , edge 80 $ \_b -> [p| DiffEvent Hard (Arpeggio $(boolP _b)) |]
      , partialChord 81 [p| Hard |]
      -- TODO
      , edge 82 $ \_b -> [p| DiffEvent Hard (MysteryBFlat $(boolP _b)) |]
      , edge 83 $ \_b -> [p| DiffEvent Hard (AllFrets $(boolP _b)) |]

      -- TODO
      , edge 93 $ \_b -> [p| Mystery93 $(boolP _b) |]

      ] ++ note 96  [p| Expert |] [p| S6 |]
        ++ note 97  [p| Expert |] [p| S5 |]
        ++ note 98  [p| Expert |] [p| S4 |]
        ++ note 99  [p| Expert |] [p| S3 |]
        ++ note 100 [p| Expert |] [p| S2 |]
        ++ note 101 [p| Expert |] [p| S1 |] ++
      [ edge 102 $ \_b -> [p| DiffEvent Expert (ForceHOPO $(boolP _b)) |]
      , slide 103 [p| Expert |] -- TODO: channel 3
      , edge 104 $ \_b -> [p| DiffEvent Expert (Arpeggio $(boolP _b)) |]
      , partialChord 105 [p| Expert |]
      -- TODO
      , edge 106 $ \_b -> [p| DiffEvent Expert (MysteryBFlat $(boolP _b)) |]
      , edge 107 $ \_b -> [p| DiffEvent Expert (AllFrets $(boolP _b)) |]

      , ( [e| one parseHandPosition |]
        , [e| \case
            HandPosition fret -> unparseBlipCPV (0, 108, fret + 100)
          |]
        )
      , edge 115           $ \_b -> [p| Solo      $(boolP _b) |]
      , edge 116           $ \_b -> [p| Overdrive $(boolP _b) |]
      -- guitar BRE must come before bass
      , edges [120 .. 125] $ \_b -> [p| BREGuitar $(boolP _b) |]
      , edges [120 .. 123] $ \_b -> [p| BREBass   $(boolP _b) |]
      , edge 126           $ \_b -> [p| Tremolo   $(boolP _b) |]
      , edge 127           $ \_b -> [p| Trill     $(boolP _b) |]

      , ( [e| one $ firstEventWhich $ \e -> readCommand' e >>= \case
            (t, s) | s == T.pack "pg" -> Just $ TrainerGtr  t
            (t, s) | s == T.pack "pb" -> Just $ TrainerBass t
            _                         -> Nothing
          |]
        , [e| \case
            TrainerGtr  t -> RTB.singleton NNC.zero $ showCommand' (t, T.pack "pg")
            TrainerBass t -> RTB.singleton NNC.zero $ showCommand' (t, T.pack "pb")
          |]
        )
      -- TODO: "[begin_pb song_trainer_pg_1]"
      -- TODO: "pb_norm song_trainer_pb_1]"
      , ( [e| one $ firstEventWhich $ \e -> readCommand' e >>= \case
            ["chrd0"   ] -> Just $ DiffEvent Easy   $ ChordName Nothing
            ["chrd1"   ] -> Just $ DiffEvent Medium $ ChordName Nothing
            ["chrd2"   ] -> Just $ DiffEvent Hard   $ ChordName Nothing
            ["chrd3"   ] -> Just $ DiffEvent Expert $ ChordName Nothing
            "chrd0" : ws -> Just $ DiffEvent Easy   $ ChordName $ Just $ T.unwords ws
            "chrd1" : ws -> Just $ DiffEvent Medium $ ChordName $ Just $ T.unwords ws
            "chrd2" : ws -> Just $ DiffEvent Hard   $ ChordName $ Just $ T.unwords ws
            "chrd3" : ws -> Just $ DiffEvent Expert $ ChordName $ Just $ T.unwords ws
            _            -> Nothing
          |]
        , [e| \case
            DiffEvent diff (ChordName s) -> RTB.singleton 0 $ showCommand' $ let
              x = "chrd" <> T.pack (show $ fromEnum diff)
              in case s of
                Nothing  -> [x]
                Just str -> [x, str]
          |]
        )
      -- custom onyx event
      , ( [e| one $ firstEventWhich $ \e -> do
            cmd <- readCommand' e
            let _ = cmd :: [T.Text]
            case cmd of
              ["onyx", "octave", n] -> OnyxOctave <$> readMaybe (T.unpack n)
              _                     -> Nothing
          |]
        , [e| \case
            OnyxOctave n -> RTB.singleton 0 $ showCommand'
              ["onyx", "octave", T.pack $ show n]
          |]
        )
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
    frets = do
      (fret, ntype) <- evts >>= \case
        DiffEvent _ (Note (NoteOn fret (_, ntype))) -> [(fret, ntype)]
        DiffEvent _ (Note (Blip   fret (_, ntype))) -> [(fret, ntype)]
        _ -> []
      guard $ ntype /= ArpeggioForm
      return fret
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
  -> RTB.T U.Beats (Maybe StrumHOPO, [(GtrString, GtrFret, NoteType)], Maybe U.Beats)
guitarifyHOPO threshold rtb = let
  notes = RTB.mapMaybe (\case Note ln -> Just ln; _ -> Nothing) rtb
  gtr = joinEdges $ guitarify $ splitEdges
    $ (\(fret, (str, ntype), len) -> ((), (str, fret, ntype), len))
    <$> joinEdges notes
  withForce = applyStatus (RTB.mapMaybe (\case ForceHOPO b -> Just (HOPO, b); _ -> Nothing) rtb) gtr
  fn prev dt (forces, ((), gems, len)) = let
    gems' = [ gem | gem@(_, _, nt) <- gems, nt /= ArpeggioForm ]
    ntype = if all (\(_, _, nt) -> nt == Tapped) gems'
      then Nothing
      else Just $ case forces of
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
