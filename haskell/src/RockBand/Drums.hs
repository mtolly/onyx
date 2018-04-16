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
) where

import qualified Data.EventList.Relative.TimeBody as RTB
import           Guitars                          (applyStatus)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Drums             (Animation (..), Audio (..),
                                                   Disco (..), Gem (..),
                                                   Hand (..), Hit (..),
                                                   PSGem (..), ProColor (..),
                                                   ProType (..))
import           RockBand.Common
import           RockBand.Parse
import qualified RockBand.PhaseShiftMessage       as PS
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U

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

instanceMIDIEvent [t| Event |] (Just [e| unparseNice (1/8) |])

  -- TODO: unknown notes on pitch 12, 14, 15

  [ blip 24  [p| Animation KickRF |]
  , edge 25  $ \_b -> [p| Animation (HihatOpen $(boolP _b)) |]
  , blip 26  [p| Animation (Snare HardHit LH) |]
  , blip 27  [p| Animation (Snare HardHit RH) |]
  , blip 28  [p| Animation (Snare SoftHit LH) |]
  , blip 29  [p| Animation (Snare SoftHit RH) |]
  , blip 30  [p| Animation (Hihat LH) |]
  , blip 31  [p| Animation (Hihat RH) |]
  , blip 32  [p| Animation PercussionRH |]
  -- 33 unused
  , blip 34  [p| Animation (Crash1 HardHit LH) |]
  , blip 35  [p| Animation (Crash1 SoftHit LH) |]
  , blip 36  [p| Animation (Crash1 HardHit RH) |]
  , blip 37  [p| Animation (Crash1 SoftHit RH) |]
  , blip 38  [p| Animation (Crash2 HardHit RH) |]
  , blip 39  [p| Animation (Crash2 SoftHit RH) |]
  , blip 40  [p| Animation Crash2RHChokeLH |]
  , blip 41  [p| Animation Crash1RHChokeLH |]
  , blip 42  [p| Animation (Ride RH) |]
  , blip 43  [p| Animation (Ride LH) |]
  , blip 44  [p| Animation (Crash2 HardHit LH) |]
  , blip 45  [p| Animation (Crash2 SoftHit LH) |]
  , blip 46  [p| Animation (Tom1 LH) |]
  , blip 47  [p| Animation (Tom1 RH) |]
  , blip 48  [p| Animation (Tom2 LH) |]
  , blip 49  [p| Animation (Tom2 RH) |]
  , blip 50  [p| Animation (FloorTom LH) |]
  , blip 51  [p| Animation (FloorTom RH) |]

  , blip 60  [p| DiffEvent Easy (Note Kick) |]
  , blip 61  [p| DiffEvent Easy (Note Red) |]
  , blip 62  [p| DiffEvent Easy (Note (Pro Yellow ())) |]
  , blip 63  [p| DiffEvent Easy (Note (Pro Blue   ())) |]
  , blip 64  [p| DiffEvent Easy (Note (Pro Green  ())) |]
  , blip 65  [p| DiffEvent Easy (Note Orange) |]

  , blip 72  [p| DiffEvent Medium (Note Kick) |]
  , blip 73  [p| DiffEvent Medium (Note Red) |]
  , blip 74  [p| DiffEvent Medium (Note (Pro Yellow ())) |]
  , blip 75  [p| DiffEvent Medium (Note (Pro Blue   ())) |]
  , blip 76  [p| DiffEvent Medium (Note (Pro Green  ())) |]
  , blip 77  [p| DiffEvent Medium (Note Orange) |]

  , blip 84  [p| DiffEvent Hard (Note Kick) |]
  , blip 85  [p| DiffEvent Hard (Note Red) |]
  , blip 86  [p| DiffEvent Hard (Note (Pro Yellow ())) |]
  , blip 87  [p| DiffEvent Hard (Note (Pro Blue   ())) |]
  , blip 88  [p| DiffEvent Hard (Note (Pro Green  ())) |]
  , blip 89  [p| DiffEvent Hard (Note Orange) |]

  , blip 95  [p| Kick2x |] -- Phase Shift convention, not a Rock Band note
  , blip 96  [p| DiffEvent Expert (Note Kick) |]
  , blip 97  [p| DiffEvent Expert (Note Red) |]
  , blip 98  [p| DiffEvent Expert (Note (Pro Yellow ())) |]
  , blip 99  [p| DiffEvent Expert (Note (Pro Blue   ())) |]
  , blip 100 [p| DiffEvent Expert (Note (Pro Green  ())) |]
  , blip 101 [p| DiffEvent Expert (Note Orange) |]

  , edge 103 $ applyB [p| Solo |]
  , edge 105 $ applyB [p| Player1 |]
  , edge 106 $ applyB [p| Player2 |]
  , edge 110 $ \b -> if b then [p| ProType Yellow Tom |] else [p| ProType Yellow Cymbal |]
  , edge 111 $ \b -> if b then [p| ProType Blue   Tom |] else [p| ProType Blue   Cymbal |]
  , edge 112 $ \b -> if b then [p| ProType Green  Tom |] else [p| ProType Green  Cymbal |]
  , edge 116 $ applyB [p| Overdrive |]
  , edges [120 .. 124] $ applyB [p| Activation |]
  , edge 126 $ applyB [p| SingleRoll |]
  , edge 127 $ applyB [p| DoubleRoll |]

  , ( [e| one $ mapParseOne Mood parseCommand |]
    , [e| \case Mood m -> unparseCommand m |]
    )
  , ( [e| one $ mapParseOne (\(diff, audio, disco) -> DiffEvent diff $ Mix audio disco) parseCommand |]
    , [e| \case DiffEvent diff (Mix audio disco) -> unparseCommand (diff, audio, disco) |]
    )
  -- TODO: "[mix 0 drums2a]", "[mix 1 drums2a]", "[mix 2 drums2a]", "[mix 3 drums2a]" (Fly Like an Eagle)
  , commandPair ["ride_side_true" ] [p| Animation (RideSide True ) |]
  , commandPair ["ride_side_false"] [p| Animation (RideSide False) |]

  , ( [e| many $ flip filterParseOne PS.parsePSMessage $ \case
        PS.PSMessage mdiff PS.HihatOpen b -> Just $ let
          diffs = maybe [minBound .. maxBound] (: []) mdiff
          in map (\diff -> DiffEvent diff $ PSHihatOpen b) diffs
        PS.PSMessage mdiff PS.HihatPedal b -> Just $ let
          diffs = maybe [minBound .. maxBound] (: []) mdiff
          in map (\diff -> DiffEvent diff $ PSHihatPedal b) diffs
        PS.PSMessage mdiff PS.SnareRimshot b -> Just $ let
          diffs = maybe [minBound .. maxBound] (: []) mdiff
          in map (\diff -> DiffEvent diff $ PSSnareRimshot b) diffs
        PS.PSMessage mdiff PS.HihatSizzle b -> Just $ let
          diffs = maybe [minBound .. maxBound] (: []) mdiff
          in map (\diff -> DiffEvent diff $ PSHihatSizzle b) diffs
        _ -> Nothing
      |]
    , [e| \case
        DiffEvent d (PSHihatOpen    b) -> unparseOne $ PS.PSMessage (Just d) PS.HihatOpen    b
        DiffEvent d (PSHihatPedal   b) -> unparseOne $ PS.PSMessage (Just d) PS.HihatPedal   b
        DiffEvent d (PSSnareRimshot b) -> unparseOne $ PS.PSMessage (Just d) PS.SnareRimshot b
        DiffEvent d (PSHihatSizzle  b) -> unparseOne $ PS.PSMessage (Just d) PS.HihatSizzle  b
      |]
    )

  ]

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

psRealToPro :: (NNC.C t) => RTB.T t Event -> RTB.T t Event
psRealToPro rtb = let
  notSysex = \case
    DiffEvent _ PSHihatOpen{}    -> False
    DiffEvent _ PSHihatPedal{}   -> False
    DiffEvent _ PSSnareRimshot{} -> False
    DiffEvent _ PSHihatSizzle{}  -> False
    _                            -> True
  merged = RTB.merge (Left <$> assignPSReal False rtb) (Right <$> RTB.filter notSysex rtb)
  -- this will fail if you use discobeat on the real track, so don't do that
  eachInstant xs = flip filter [ x | Right x <- xs ] $ \case
    DiffEvent d (Note (Pro Yellow ())) -> notElem (Left (d, Left HHPedal)) xs
    _                                  -> True
  in RTB.flatten $ fmap eachInstant $ RTB.collectCoincident merged

-- | Writes drum gems as the given length, or shorter if there is another gem
-- in the same difficulty sooner than that.
unparseNice :: U.Beats -> RTB.T U.Beats Event -> RTB.T U.Beats E.T
unparseNice defLength trk = let
  (notes, notNotes) = flip RTB.partitionMaybe trk $ \case
    Kick2x                 -> Just (Expert, Nothing )
    DiffEvent d (Note gem) -> Just (d     , Just gem)
    _                      -> Nothing
  assignLengths :: RTB.T U.Beats (Difficulty, Maybe (Gem ())) -> RTB.T U.Beats (RTB.T U.Beats E.T)
  assignLengths rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, (diff, gem)), rtb') -> let
      len = case RTB.viewL $ RTB.filter ((diff ==) . fst) $ U.trackTake defLength $ U.trackDropZero rtb' of
        Nothing             -> defLength
        Just ((next, _), _) -> next
      in RTB.cons dt (makeNote len diff gem) $ assignLengths rtb'
  makeNote len diff gem = let
    pitch = let
      a = case diff of
        Easy   -> 60
        Medium -> 72
        Hard   -> 84
        Expert -> 96
      b = case gem of
        Nothing              -> -1
        Just Kick            -> 0
        Just Red             -> 1
        Just (Pro Yellow ()) -> 2
        Just (Pro Blue ())   -> 3
        Just (Pro Green ())  -> 4
        Just Orange          -> 5
      in a + b
    in RTB.cons NNC.zero (makeEdge pitch True) $ RTB.singleton len (makeEdge pitch False)
  in RTB.merge (U.trackJoin $ assignLengths notes) (U.trackJoin $ fmap unparseOne notNotes)

baseScore :: RTB.T U.Beats (Gem ProType) -> Int
baseScore = sum . fmap gemScore where
  gemScore = \case
    Kick         -> 30
    Red          -> 30
    Pro _ Cymbal -> 30
    Pro _ Tom    -> 25
    Orange       -> 30 -- no actual answer

perfectSoloBonus :: (Ord a) => RTB.T U.Beats Bool -> RTB.T U.Beats (Gem a) -> Int
perfectSoloBonus solo gems = sum $ fmap score $ applyStatus (fmap ((),) solo) gems where
  score ([], _) = 0
  score _       = 100
