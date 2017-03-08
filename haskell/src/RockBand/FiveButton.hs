-- | Parser used for all the GRYBO instruments (basic guitar, bass, and keys).
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module RockBand.FiveButton where

import           Control.Monad                    (guard)
import           Data.Bifunctor                   (first)
import           Data.Data
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (sort)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.Parse
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U

data Color = Green | Red | Yellow | Blue | Orange
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

data Event
  = Mood                      Mood
  | HandMap                HandMap
  | StrumMap              StrumMap
  | FretPosition FretPosition Bool
  | Tremolo                   Bool
  | Trill                     Bool
  | Overdrive                 Bool
  | BRE                       Bool
  | Solo                      Bool
  | Player1                   Bool
  | Player2                   Bool
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | These don't actually correspond to 20 different frets;
-- see http://i.imgur.com/fRg6Vo9.png by Orange Harrison
data FretPosition
  = Fret40 -- ^ the nut
  | Fret41
  | Fret42
  | Fret43
  | Fret44
  | Fret45
  | Fret46
  | Fret47
  | Fret48
  | Fret49
  | Fret50
  | Fret51
  | Fret52
  | Fret53
  | Fret54
  | Fret55
  | Fret56
  | Fret57
  | Fret58
  | Fret59 -- ^ roughly fret 13
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

data DiffEvent
  = Force StrumHOPO Bool
  | Note (LongNote () Color)
  deriving (Eq, Ord, Show, Read, Typeable, Data)

data StrumHOPO = Strum | HOPO
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

-- | Controls the fretting hand animation of a guitarist/bassist.
data HandMap
  = HandMap_Default
  -- ^ Normal fingering. Single gems = single fingers, gems with duration =
  -- vibrato, chord gems = chords.
  | HandMap_NoChords  -- ^ All single fingers/vibrato.
  | HandMap_AllChords -- ^ All chords.
  | HandMap_Solo      -- ^ D major shape for all chords, vibrato for all chord sustains.
  | HandMap_DropD     -- ^ Open hand for all green gems, all other gems are chords.
  | HandMap_DropD2    -- ^ Open hand for all green gems.
  | HandMap_AllBend   -- ^ All ring finger high vibrato.
  | HandMap_Chord_C   -- ^ All C chord shape.
  | HandMap_Chord_D   -- ^ All D chord shape.
  | HandMap_Chord_A   -- ^ All A minor chord shape.
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

instance Command HandMap where
  fromCommand hm = ["map", T.pack $ show hm]
  toCommand = reverseLookup each fromCommand

-- | Controls the strumming animation for a bassist.
data StrumMap
  = StrumMap_Default
  | StrumMap_Pick
  | StrumMap_SlapBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

instance Command StrumMap where
  fromCommand sm = ["map", T.pack $ show sm]
  toCommand = reverseLookup each fromCommand

instanceMIDIEvent [t| Event |] (Just [e| unparseNice (1/8) |]) $

  [ edge 40 $ applyB [p| FretPosition Fret40 |]
  , edge 41 $ applyB [p| FretPosition Fret41 |]
  , edge 42 $ applyB [p| FretPosition Fret42 |]
  , edge 43 $ applyB [p| FretPosition Fret43 |]
  , edge 44 $ applyB [p| FretPosition Fret44 |]
  , edge 45 $ applyB [p| FretPosition Fret45 |]
  , edge 46 $ applyB [p| FretPosition Fret46 |]
  , edge 47 $ applyB [p| FretPosition Fret47 |]
  , edge 48 $ applyB [p| FretPosition Fret48 |]
  , edge 49 $ applyB [p| FretPosition Fret49 |]
  , edge 50 $ applyB [p| FretPosition Fret50 |]
  , edge 51 $ applyB [p| FretPosition Fret51 |]
  , edge 52 $ applyB [p| FretPosition Fret52 |]
  , edge 53 $ applyB [p| FretPosition Fret53 |]
  , edge 54 $ applyB [p| FretPosition Fret54 |]
  , edge 55 $ applyB [p| FretPosition Fret55 |]
  , edge 56 $ applyB [p| FretPosition Fret56 |]
  , edge 57 $ applyB [p| FretPosition Fret57 |]
  , edge 58 $ applyB [p| FretPosition Fret58 |]
  , edge 59 $ applyB [p| FretPosition Fret59 |]

  ] ++ noteParser 60 [p| Green |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 61 [p| Red |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 62 [p| Yellow |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 63 [p| Blue |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 64 [p| Orange |] (\p -> [p| DiffEvent Easy (Note $p) |]) ++
  [ edge 65 $ \_b -> [p| DiffEvent Easy (Force HOPO  $(boolP _b)) |]
  , edge 66 $ \_b -> [p| DiffEvent Easy (Force Strum $(boolP _b)) |]

  ] ++ noteParser 72 [p| Green |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 73 [p| Red |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 74 [p| Yellow |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 75 [p| Blue |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 76 [p| Orange |] (\p -> [p| DiffEvent Medium (Note $p) |]) ++
  [ edge 77 $ \_b -> [p| DiffEvent Medium (Force HOPO  $(boolP _b)) |]
  , edge 78 $ \_b -> [p| DiffEvent Medium (Force Strum $(boolP _b)) |]

  ] ++ noteParser 84 [p| Green |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 85 [p| Red |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 86 [p| Yellow |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 87 [p| Blue |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 88 [p| Orange |] (\p -> [p| DiffEvent Hard (Note $p) |]) ++
  [ edge 89 $ \_b -> [p| DiffEvent Hard (Force HOPO  $(boolP _b)) |]
  , edge 90 $ \_b -> [p| DiffEvent Hard (Force Strum $(boolP _b)) |]

  ] ++ noteParser 96 [p| Green |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 97 [p| Red |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 98 [p| Yellow |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 99 [p| Blue |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 100 [p| Orange |] (\p -> [p| DiffEvent Expert (Note $p) |]) ++
  [ edge 101 $ \_b -> [p| DiffEvent Expert (Force HOPO  $(boolP _b)) |]
  , edge 102 $ \_b -> [p| DiffEvent Expert (Force Strum $(boolP _b)) |]

  , edge 103 $ applyB [p| Solo |]
  , edge 105 $ applyB [p| Player1 |]
  , edge 106 $ applyB [p| Player2 |]
  , edge 116 $ applyB [p| Overdrive |]
  , edges [120 .. 124] $ applyB [p| BRE |]
  , edge 126 $ applyB [p| Tremolo |]
  , edge 127 $ applyB [p| Trill |]

  , ( [e| mapParseOne Mood parseCommand |]
    , [e| \case Mood m -> unparseCommand m |]
    )
  , ( [e| mapParseOne HandMap parseCommand |]
    , [e| \case HandMap m -> unparseCommand m |]
    )
  -- TODO:
  -- "[map HandMap NoChords]"
  -- "[map HandMap_Drop_D2]"
  -- "[map handMap_DropD2]"
  -- "map HandMap DropD2]"
  , ( [e| mapParseOne StrumMap parseCommand |]
    , [e| \case StrumMap m -> unparseCommand m |]
    )
  -- TODO: "[map HandMap_Pick]"
  ]

copyExpert :: (NNC.C t) => RTB.T t Event -> RTB.T t Event
copyExpert = baseCopyExpert DiffEvent $ \case
  DiffEvent d e -> Just (d, e)
  _             -> Nothing

assignKeys :: (NNC.C t) => RTB.T t DiffEvent -> RTB.T t (LongNote StrumHOPO Color)
assignKeys = RTB.mapMaybe $ \case
  Force _ _ -> Nothing
  Note note -> Just $ first (const Strum) note

trackState :: (NNC.C t) => s -> (s -> t -> a -> (s, Maybe b)) -> RTB.T t a -> RTB.T t b
trackState state step rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> case step state dt x of
    (state', Nothing) -> RTB.delay dt   $ trackState state' step rtb'
    (state', Just y ) -> RTB.cons  dt y $ trackState state' step rtb'

applyStatus :: (NNC.C t, Ord s, Ord a) => RTB.T t (s, Bool) -> RTB.T t a -> RTB.T t ([s], a)
applyStatus status events = let
  fn current _ = \case
    Left  (s, True ) -> (Set.insert s current, Nothing                     )
    Left  (s, False) -> (Set.delete s current, Nothing                     )
    Right x          -> (             current, Just (Set.toList current, x))
  in trackState Set.empty fn $ RTB.merge (fmap Left status) (fmap Right events)

guitarifyHOPO :: U.Beats -> Bool -> RTB.T U.Beats DiffEvent -> RTB.T U.Beats (LongNote StrumHOPO [Color])
guitarifyHOPO threshold keys rtb = let
  gtr = joinEdges $ guitarify $ RTB.mapMaybe (\case Note ln -> Just ln; _ -> Nothing) rtb
  withForce = applyStatus (RTB.mapMaybe (\case Force f b -> Just (f, b); _ -> Nothing) rtb) gtr
  fn prev dt (forces, ((), colors, len)) = let
    ntype = case forces of
      nt : _ -> nt
      [] -> if dt >= threshold -- TODO: should this be > or >= ?
        then Strum
        else case prev of
          Nothing -> Strum
          Just prevColors -> case colors of
            [c] -> if c `elem` prevColors -- TODO: is this also true on keys?
              then Strum
              else HOPO
            _ -> if keys
              then if sort colors /= sort prevColors
                then HOPO
                else Strum
              else Strum
    in (Just colors, Just (ntype, colors, len))
  in splitEdges $ trackState Nothing fn withForce

keysToGuitar :: U.Beats -> RTB.T U.Beats Event -> RTB.T U.Beats Event
keysToGuitar threshold evts = let
  getDiffEvent diff = \case
    DiffEvent d e | d == diff -> Just e
    _                         -> Nothing
  (expert, notExpert) = RTB.partitionMaybe (getDiffEvent Expert) evts
  (hard  , notHard  ) = RTB.partitionMaybe (getDiffEvent Hard  ) notExpert
  (medium, notMedium) = RTB.partitionMaybe (getDiffEvent Medium) notHard
  (easy  , notEasy  ) = RTB.partitionMaybe (getDiffEvent Easy  ) notMedium
  forceNote ntype = RTB.fromPairList
    [ (0   , Force ntype True )
    , (1/32, Force ntype False)
    ]
  longToEvents longs = U.trackJoin $ flip fmap longs $ \case
    Blip   ntype colors -> RTB.merge (forceNote ntype) $ foldr (RTB.cons 0) RTB.empty $ map (Note . Blip   ()) colors
    NoteOn ntype colors -> RTB.merge (forceNote ntype) $ foldr (RTB.cons 0) RTB.empty $ map (Note . NoteOn ()) colors
    NoteOff      colors ->                               foldr (RTB.cons 0) RTB.empty $ map (Note . NoteOff  ) colors
  in foldr RTB.merge RTB.empty
    [ DiffEvent Expert <$> longToEvents (guitarifyHOPO threshold True expert)
    , DiffEvent Hard   <$> longToEvents (guitarifyHOPO threshold True hard  )
    , DiffEvent Medium <$> longToEvents (guitarifyHOPO threshold True medium)
    , DiffEvent Easy   <$> longToEvents (guitarifyHOPO threshold True easy  )
    , notEasy
    ]

unparseNice :: U.Beats -> RTB.T U.Beats Event -> RTB.T U.Beats E.T
unparseNice defLength = U.trackJoin . fmap unparseOne . showBlipsNice defLength

showBlipsNice :: (NNC.C t) => t -> RTB.T t Event -> RTB.T t Event
showBlipsNice defLength evts = let
  getDiffNotes diff = \case
    DiffEvent d (Note ln) | d == diff -> Just ln
    _                                 -> Nothing
  (expert, notExpert) = RTB.partitionMaybe (getDiffNotes Expert) evts
  (hard  , notHard  ) = RTB.partitionMaybe (getDiffNotes Hard  ) notExpert
  (medium, notMedium) = RTB.partitionMaybe (getDiffNotes Medium) notHard
  (easy  , notEasy  ) = RTB.partitionMaybe (getDiffNotes Easy  ) notMedium
  in foldr RTB.merge RTB.empty
    [ DiffEvent Expert . Note <$> showEdgesNice' defLength expert
    , DiffEvent Hard   . Note <$> showEdgesNice' defLength hard
    , DiffEvent Medium . Note <$> showEdgesNice' defLength medium
    , DiffEvent Easy   . Note <$> showEdgesNice' defLength easy
    , notEasy
    ]

-- | Takes a track of individual notes, possibly with overlaps,
-- and returns a guitar-controller-playable sequence where chords are
-- grouped together and overlaps are removed.
guitarify :: (Ord s, Ord a) => RTB.T U.Beats (LongNote s a) -> RTB.T U.Beats (LongNote s [a])
guitarify = splitEdges . go . RTB.collectCoincident . joinEdges where
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, xs), rtb') -> let
      ntype = case head xs of (nt, _, _) -> nt
      colors = map (\(_, c, _) -> c) xs
      len1 = maximum $ map (\(_, _, len) -> len) xs
      len2 = case RTB.viewL rtb' of
        Nothing          -> len1
        Just ((t, _), _) -> min (t NNC.-| (1/8)) <$> len1
      len3 = guard (maybe True (> 1/3) len2) >> len2
      -- anything 1/3 beat or less, make into blip.
      -- RB does not do this step, so it produces 16th note sustains on keytar...
      in RTB.cons dt (ntype, colors, len3) $ go rtb'
