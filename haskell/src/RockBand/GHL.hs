-- | The \"Clone Hero Live\" MIDI format.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}
module RockBand.GHL where

import           Data.Data
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.FiveButton              (StrumHOPO (..))
import           RockBand.Parse
import qualified RockBand.PhaseShiftMessage       as PS
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U

data Fret
  = Black1
  | Black2
  | Black3
  | White1
  | White2
  | White3
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

data Event
  = Overdrive                 Bool
  | Solo                      Bool
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show, Read, Typeable, Data)

data DiffEvent
  = Force StrumHOPO Bool
  | TapNotes Bool
  | Note (LongNote () (Maybe Fret))
  deriving (Eq, Ord, Show, Read, Typeable, Data)

instanceMIDIEvent [t| Event |] (Just [e| unparseNice (1/8) |]) $

       noteParser 58 [p| Nothing |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 59 [p| Just White1 |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 60 [p| Just White2 |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 61 [p| Just White3 |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 62 [p| Just Black1 |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 63 [p| Just Black2 |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 64 [p| Just Black3 |] (\p -> [p| DiffEvent Easy (Note $p) |]) ++
  [ edge 65 $ \_b -> [p| DiffEvent Easy (Force HOPO  $(boolP _b)) |]
  , edge 66 $ \_b -> [p| DiffEvent Easy (Force Strum $(boolP _b)) |]

  ] ++ noteParser 70 [p| Nothing |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 71 [p| Just White1 |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 72 [p| Just White2 |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 73 [p| Just White3 |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 74 [p| Just Black1 |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 75 [p| Just Black2 |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 76 [p| Just Black3 |] (\p -> [p| DiffEvent Medium (Note $p) |]) ++
  [ edge 77 $ \_b -> [p| DiffEvent Medium (Force HOPO  $(boolP _b)) |]
  , edge 78 $ \_b -> [p| DiffEvent Medium (Force Strum $(boolP _b)) |]

  ] ++ noteParser 82 [p| Nothing |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 83 [p| Just White1 |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 84 [p| Just White2 |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 85 [p| Just White3 |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 86 [p| Just Black1 |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 87 [p| Just Black2 |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 88 [p| Just Black3 |] (\p -> [p| DiffEvent Hard (Note $p) |]) ++
  [ edge 89 $ \_b -> [p| DiffEvent Hard (Force HOPO  $(boolP _b)) |]
  , edge 90 $ \_b -> [p| DiffEvent Hard (Force Strum $(boolP _b)) |]

  ] ++ noteParser 94 [p| Nothing |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 95 [p| Just White1 |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 96 [p| Just White2 |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 97 [p| Just White3 |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 98 [p| Just Black1 |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 99 [p| Just Black2 |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 100 [p| Just Black3 |] (\p -> [p| DiffEvent Expert (Note $p) |]) ++
  [ edge 101 $ \_b -> [p| DiffEvent Expert (Force HOPO  $(boolP _b)) |]
  , edge 102 $ \_b -> [p| DiffEvent Expert (Force Strum $(boolP _b)) |]

  , edge 103 $ applyB [p| Solo |]
  , edge 116 $ applyB [p| Overdrive |]

  , ( [e| many $ flip filterParseOne PS.parsePSMessage $ \case
        PS.PSMessage mdiff PS.TapNotes b -> Just $ let
          diffs = maybe [minBound .. maxBound] (: []) mdiff
          in map (\diff -> DiffEvent diff $ TapNotes b) diffs
        _ -> Nothing
      |]
    , [e| \case
        DiffEvent d (TapNotes b) -> unparseOne $ PS.PSMessage (Just d) PS.TapNotes b
      |]
    )

  ]

copyExpert :: (NNC.C t) => RTB.T t Event -> RTB.T t Event
copyExpert = baseCopyExpert DiffEvent $ \case
  DiffEvent d e -> Just (d, e)
  _             -> Nothing

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

eachDifficulty :: (NNC.C t) => (RTB.T t DiffEvent -> RTB.T t DiffEvent) -> RTB.T t Event -> RTB.T t Event
eachDifficulty f evts = let
  getDiffEvent diff = \case
    DiffEvent d e | d == diff -> Just e
    _                         -> Nothing
  (expert, notExpert) = RTB.partitionMaybe (getDiffEvent Expert) evts
  (hard  , notHard  ) = RTB.partitionMaybe (getDiffEvent Hard  ) notExpert
  (medium, notMedium) = RTB.partitionMaybe (getDiffEvent Medium) notHard
  (easy  , notEasy  ) = RTB.partitionMaybe (getDiffEvent Easy  ) notMedium
  in foldr RTB.merge RTB.empty
    [ DiffEvent Expert <$> f expert
    , DiffEvent Hard   <$> f hard
    , DiffEvent Medium <$> f medium
    , DiffEvent Easy   <$> f easy
    , notEasy
    ]
