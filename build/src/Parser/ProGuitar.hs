{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser.ProGuitar where

import Parser.Base
import Parser.TH
import qualified Data.EventList.Relative.TimeBody as RTB
import Control.Monad (guard)
import qualified Sound.MIDI.File.Event as E
import qualified Numeric.NonNegative.Class as NNC
import Language.Haskell.TH
import Data.Maybe (isJust)

data Event
  = TrainerGtr Trainer
  | TrainerBass Trainer
  | HandPosition GtrFret
  | ChordRoot Key
  | ChordName Difficulty (Maybe String)
  | NoChordNames Bool
  | SlashChords Bool
  | Trill Bool
  | Tremolo Bool
  | BREGuitar Bool
  | BREBass Bool
  | Overdrive Bool
  | Solo Bool
  | Mystery18 Bool -- ^ I think this switches between sharp and flat chords?
  | Mystery45 Bool
  | Mystery69 Bool
  | Mystery93 Bool
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show)

data DiffEvent
  = ForceHOPO Bool
  | Slide Bool SlideType
  | Arpeggio Bool
  | PartialChord Bool StrumArea
  | AllFrets Bool
  | MysteryBFlat Bool
  | Note GtrString (Maybe GtrFret) NoteType
  deriving (Eq, Ord, Show)

data NoteType
  = NormalNote
  | ArpeggioForm
  | Bent
  | Muted
  | Tapped
  | Harmonic
  | PinchHarmonic
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SlideType = NormalSlide | ReversedSlide | MysterySlide3 | MysterySlide2
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data StrumArea = High | Mid | Low | MysteryStrum0
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type GtrFret = Int

data GtrString = S6 | S5 | S4 | S3 | S2 | S1
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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

parseNote :: (NNC.C t) => Int -> Difficulty -> GtrString -> ParseOne t E.T Event
parseNote pitch diff str rtb = do
  ((t, (c, p, v)), rtb') <- firstEventWhich isNoteEdgeCPV rtb
  guard $ p == pitch
  guard $ maybe True (>= 100) v
  ntype <- lookup c channelMap
  let e = DiffEvent diff $ Note str (fmap (subtract 100) v) ntype
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

instanceMIDIEvent [t| Event |] $ let
  note :: Int -> Q Pat -> Q Pat -> (Q Exp, Q Exp)
  note pitch diff str =
    ( [e| parseNote pitch $(fmap patToExp diff) $(fmap patToExp str) |]
    , [e| \case
      DiffEvent $diff (Note $str fret ntype) ->
        RTB.singleton 0 $ makeEdgeCPV (encodeChannel ntype) pitch $ fmap (+ 100) fret
      |]
    )
  slide :: Int -> Q Pat -> (Q Exp, Q Exp)
  slide pitch diff =
    ( [e| parseSlide pitch $(fmap patToExp diff) |]
    , [e| \case
      DiffEvent $diff (Slide b stype) ->
        RTB.singleton 0 $ makeEdgeCPV (encodeChannel stype) pitch $ guard b >> Just 96
      |]
    )
  partialChord :: Int -> Q Pat -> (Q Exp, Q Exp)
  partialChord pitch diff =
    ( [e| parsePartialChord pitch $(fmap patToExp diff) |]
    , [e| \case
      DiffEvent $diff (PartialChord b area) ->
        RTB.singleton 0 $ makeEdgeCPV (encodeChannel area) pitch $ guard b >> Just 96
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

      , edge 16 $ \_b -> [p| SlashChords $(bool _b) |]
      , edge 17 $ \_b -> [p| NoChordNames $(bool _b) |]
      -- TODO
      , edge 18 $ \_b -> [p| Mystery18 $(bool _b) |]

      , note 24  [p| Easy   |] [p| S6 |]
      , note 25  [p| Easy   |] [p| S5 |]
      , note 26  [p| Easy   |] [p| S4 |]
      , note 27  [p| Easy   |] [p| S3 |]
      , note 28  [p| Easy   |] [p| S2 |]
      , note 29  [p| Easy   |] [p| S1 |]
      , edge 30 $ \_b -> [p| DiffEvent Easy (ForceHOPO $(bool _b)) |]
      , slide 31 [p| Easy |]
      , edge 32 $ \_b -> [p| DiffEvent Easy (Arpeggio $(bool _b)) |]
      , partialChord 33 [p| Easy |]
      -- TODO (not actually seen)
      , edge 34 $ \_b -> [p| DiffEvent Easy (MysteryBFlat $(bool _b)) |]
      , edge 35 $ \_b -> [p| DiffEvent Easy (AllFrets $(bool _b)) |]

      -- TODO
      , edge 45 $ \_b -> [p| Mystery45 $(bool _b) |]

      , note 48  [p| Medium |] [p| S6 |]
      , note 49  [p| Medium |] [p| S5 |]
      , note 50  [p| Medium |] [p| S4 |]
      , note 51  [p| Medium |] [p| S3 |]
      , note 52  [p| Medium |] [p| S2 |]
      , note 53  [p| Medium |] [p| S1 |]
      , edge 54 $ \_b -> [p| DiffEvent Medium (ForceHOPO $(bool _b)) |]
      , slide 55 [p| Medium |]
      , edge 56 $ \_b -> [p| DiffEvent Medium (Arpeggio $(bool _b)) |]
      , partialChord 57 [p| Medium |]
      -- TODO
      , edge 58 $ \_b -> [p| DiffEvent Medium (MysteryBFlat $(bool _b)) |]
      , edge 59 $ \_b -> [p| DiffEvent Medium (AllFrets $(bool _b)) |]

      -- TODO
      , edge 69 $ \_b -> [p| Mystery69 $(bool _b) |]

      , note 72  [p| Hard   |] [p| S6 |]
      , note 73  [p| Hard   |] [p| S5 |]
      , note 74  [p| Hard   |] [p| S4 |]
      , note 75  [p| Hard   |] [p| S3 |]
      , note 76  [p| Hard   |] [p| S2 |]
      , note 77  [p| Hard   |] [p| S1 |]
      , edge 78 $ \_b -> [p| DiffEvent Hard (ForceHOPO $(bool _b)) |]
      , slide 79 [p| Hard |] -- TODO: channel 3
      , edge 80 $ \_b -> [p| DiffEvent Hard (Arpeggio $(bool _b)) |]
      , partialChord 81 [p| Hard |]
      -- TODO
      , edge 82 $ \_b -> [p| DiffEvent Hard (MysteryBFlat $(bool _b)) |]
      , edge 83 $ \_b -> [p| DiffEvent Hard (AllFrets $(bool _b)) |]

      -- TODO
      , edge 93 $ \_b -> [p| Mystery93 $(bool _b) |]

      , note 96  [p| Expert |] [p| S6 |]
      , note 97  [p| Expert |] [p| S5 |]
      , note 98  [p| Expert |] [p| S4 |]
      , note 99  [p| Expert |] [p| S3 |]
      , note 100 [p| Expert |] [p| S2 |]
      , note 101 [p| Expert |] [p| S1 |]
      , edge 102 $ \_b -> [p| DiffEvent Expert (ForceHOPO $(bool _b)) |]
      , slide 103 [p| Expert |] -- TODO: channel 3
      , edge 104 $ \_b -> [p| DiffEvent Expert (Arpeggio $(bool _b)) |]
      , partialChord 105 [p| Expert |]
      -- TODO
      , edge 106 $ \_b -> [p| DiffEvent Expert (MysteryBFlat $(bool _b)) |]
      , edge 107 $ \_b -> [p| DiffEvent Expert (AllFrets $(bool _b)) |]

      , ( [e| parseHandPosition |]
        , [e| \case
            HandPosition fret -> unparseBlipCPV (0, 108, fret + 100)
          |]
        )
      , edge 115           $ \_b -> [p| Solo      $(bool _b) |]
      , edge 116           $ \_b -> [p| Overdrive $(bool _b) |]
      -- guitar BRE must come before bass
      , edges [120 .. 125] $ \_b -> [p| BREGuitar $(bool _b) |]
      , edges [120 .. 123] $ \_b -> [p| BREBass   $(bool _b) |]
      , edge 126           $ \_b -> [p| Tremolo   $(bool _b) |]
      , edge 127           $ \_b -> [p| Trill     $(bool _b) |]

      , ( [e| firstEventWhich $ \e -> readCommand' e >>= \case
            (t, "pg") -> Just $ TrainerGtr  t
            (t, "pb") -> Just $ TrainerBass t
            _          -> Nothing
          |]
        , [e| \case
            TrainerGtr  t -> RTB.singleton NNC.zero $ showCommand' (t, "pg")
            TrainerBass t -> RTB.singleton NNC.zero $ showCommand' (t, "pb")
          |]
        )
      -- TODO: "[begin_pb song_trainer_pg_1]"
      -- TODO: "pb_norm song_trainer_pb_1]"
      , ( [e| firstEventWhich $ \e -> readCommand' e >>= \case
            ["chrd0", s] -> Just $ ChordName Easy   $ Just s
            ["chrd1", s] -> Just $ ChordName Medium $ Just s
            ["chrd2", s] -> Just $ ChordName Hard   $ Just s
            ["chrd3", s] -> Just $ ChordName Expert $ Just s
            ["chrd0"   ] -> Just $ ChordName Easy   Nothing
            ["chrd1"   ] -> Just $ ChordName Medium Nothing
            ["chrd2"   ] -> Just $ ChordName Hard   Nothing
            ["chrd3"   ] -> Just $ ChordName Expert Nothing
            _            -> Nothing
          |]
        , [e| \case
            ChordName diff s -> RTB.singleton 0 $ showCommand' $ let
              x = "chrd" ++ show (fromEnum diff)
              in case s of
                Nothing  -> [x]
                Just str -> [x, str]
          |]
        )
      ]

standardGuitar :: [Int]
standardGuitar = [40, 45, 50, 55, 59, 64]

standardBass :: [Int]
standardBass = [28, 33, 38, 43]

playGuitar :: (NNC.C t) => [Int] -> RTB.T t DiffEvent -> [(GtrString, RTB.T t E.T)]
playGuitar tuning evts = do
  str <- [S6 .. S1]
  let go held rtb = case RTB.viewL rtb of
        Nothing -> case held of
          Nothing -> RTB.empty
          Just _  -> error "playGuitar: unterminated note-on"
        Just ((t, e), rtb') -> case e of
          Note s fret ntype | s == str && ntype /= ArpeggioForm -> case fret of
            Nothing -> case held of
              Nothing -> error "playGuitar: note-off but string is already not played"
              Just p  -> RTB.cons t (makeEdgeCPV (fromEnum str) p Nothing) $ go Nothing rtb'
            Just f -> let
              p = (tuning !! fromEnum str) + f
              in case held of
                Just _  -> error $ "playGuitar: double note-on"
                Nothing -> RTB.cons t (makeEdgeCPV (fromEnum str) p $ Just 96) $ go (Just p) rtb'
          _ -> RTB.delay t $ go held rtb'
  return (str, go Nothing $ RTB.normalize evts)
  -- the normalize puts Nothing (note-off) before Just _ (note-on)

autoHandPosition :: (NNC.C t) => RTB.T t Event -> RTB.T t Event
autoHandPosition = RTB.flatten . fmap f . RTB.collectCoincident where
  f evts = let
    frets = [ fret | DiffEvent _ (Note _ (Just fret) ntype) <- evts, ntype /= ArpeggioForm ]
    evts' = flip filter evts $ \case
      HandPosition _ -> False
      _              -> True
    in case frets of
      [] -> evts'
      _  -> HandPosition (minimum frets) : evts'
