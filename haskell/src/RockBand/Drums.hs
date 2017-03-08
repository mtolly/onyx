{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
module RockBand.Drums where

import           Data.Data
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.FiveButton              (applyStatus)
import           RockBand.Parse
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U

data ProColor = Yellow | Blue | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

data ProType = Cymbal | Tom
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

data Gem a = Kick | Red | Pro ProColor a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Typeable, Data)

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
  deriving (Eq, Ord, Show, Read, Typeable, Data)

data DiffEvent
  = Mix Audio Disco
  | Note (Gem ())
  deriving (Eq, Ord, Show, Read, Typeable, Data)

data Animation
  = Tom1       Hand -- ^ The high tom.
  | Tom2       Hand -- ^ The middle tom.
  | FloorTom   Hand -- ^ The low tom.
  | Hihat      Hand
  | Snare  Hit Hand
  | Ride       Hand
  | Crash1 Hit Hand -- ^ The left crash, closer to the hihat.
  | Crash2 Hit Hand -- ^ The right crash, closer to the ride.
  | KickRF
  | Crash1RHChokeLH
  -- ^ NOTE: This is MIDI note 41! The RBN docs incorrectly say 40.
  | Crash2RHChokeLH
  -- ^ NOTE: This is MIDI note 40! The RBN docs incorrectly say 41.
  | PercussionRH
  | HihatOpen Bool
  | RideSide Bool -- ^ Causes slow 'Ride' hits to animate differently.
  deriving (Eq, Ord, Show, Read, Typeable, Data)

data Hit = SoftHit | HardHit
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

data Hand = LH | RH
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

-- | Controls the audio files used for the drum track.
data Audio
  = D0 -- ^ One stereo mix for the whole kit.
  | D1 -- ^ Mono kick, mono snare, stereo kit.
  | D2 -- ^ Mono kick, stereo snare, stereo kit.
  | D3 -- ^ Stereo kick, stereo snare, stereo kit.
  | D4 -- ^ Mono kick, stereo kit (including snare).
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

-- | Special options that can affect drum audio and pad settings.
data Disco
  = NoDisco     -- ^ All pads are normal.
  | Disco       -- ^ Yellow snare, red hihat. Undone by Pro Drums.
  | DiscoNoFlip -- ^ New in RB3: snare beats where accented hits are 'Yellow'.
  | EasyMix     -- ^ Pre-RB3. 'Easy' sections with only 'Red' and 'Kick' notes.
  | EasyNoKick  -- ^ Pre-RB3. 'Easy' sections with no 'Kick' notes.
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

instance Command (Difficulty, Audio, Disco) where
  fromCommand (diff, audio, disco) = ["mix", T.pack (show $ fromEnum diff), showMix audio disco]
  toCommand = reverseLookup ((,,) <$> each <*> each <*> each) fromCommand

-- | e.g. turns 'D2' and 'Disco' into @\"drums2d\"@
showMix :: Audio -> Disco -> T.Text
showMix audio disco = "drums" <> T.pack (show $ fromEnum audio) <> case disco of
  NoDisco     -> ""
  Disco       -> "d"
  DiscoNoFlip -> "dnoflip"
  EasyMix     -> "easy"
  EasyNoKick  -> "easynokick"

instanceMIDIEvent [t| Event |] (Just [e| unparseNice (1/8) |])

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

  , blip 72  [p| DiffEvent Medium (Note Kick) |]
  , blip 73  [p| DiffEvent Medium (Note Red) |]
  , blip 74  [p| DiffEvent Medium (Note (Pro Yellow ())) |]
  , blip 75  [p| DiffEvent Medium (Note (Pro Blue   ())) |]
  , blip 76  [p| DiffEvent Medium (Note (Pro Green  ())) |]

  , blip 84  [p| DiffEvent Hard (Note Kick) |]
  , blip 85  [p| DiffEvent Hard (Note Red) |]
  , blip 86  [p| DiffEvent Hard (Note (Pro Yellow ())) |]
  , blip 87  [p| DiffEvent Hard (Note (Pro Blue   ())) |]
  , blip 88  [p| DiffEvent Hard (Note (Pro Green  ())) |]

  , blip 95  [p| Kick2x |] -- Phase Shift convention, not a Rock Band note
  , blip 96  [p| DiffEvent Expert (Note Kick) |]
  , blip 97  [p| DiffEvent Expert (Note Red) |]
  , blip 98  [p| DiffEvent Expert (Note (Pro Yellow ())) |]
  , blip 99  [p| DiffEvent Expert (Note (Pro Blue   ())) |]
  , blip 100 [p| DiffEvent Expert (Note (Pro Green  ())) |]

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

  , ( [e| mapParseOne Mood parseCommand |]
    , [e| \case Mood m -> unparseCommand m |]
    )
  , ( [e| mapParseOne (\(diff, audio, disco) -> DiffEvent diff $ Mix audio disco) parseCommand |]
    , [e| \case DiffEvent diff (Mix audio disco) -> unparseCommand (diff, audio, disco) |]
    )
  -- TODO: "[mix 0 drums2a]", "[mix 1 drums2a]", "[mix 2 drums2a]", "[mix 3 drums2a]" (Fly Like an Eagle)
  , commandPair ["ride_side_true" ] [p| Animation (RideSide True ) |]
  , commandPair ["ride_side_false"] [p| Animation (RideSide False) |]
  ]

copyExpert :: (NNC.C t) => RTB.T t Event -> RTB.T t Event
copyExpert = baseCopyExpert DiffEvent $ \case
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
  } deriving (Eq, Ord, Show, Read, Typeable, Data)

defDrumState :: DrumState
defDrumState = DrumState Cymbal Cymbal Cymbal False False False False

assignToms :: (NNC.C t) => RTB.T t Event -> RTB.T t (Difficulty, Gem ProType)
assignToms = go defDrumState . RTB.normalize where
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
        where isDisco = case diff of
                Easy   -> easyDisco ds
                Medium -> mediumDisco ds
                Hard   -> hardDisco ds
                Expert -> expertDisco ds
      _ -> RTB.delay dt $ go ds rtb'

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
      in a + b
    in RTB.cons NNC.zero (makeEdge pitch True) $ RTB.singleton len (makeEdge pitch False)
  in RTB.merge (U.trackJoin $ assignLengths notes) (unparseAll notNotes)

baseScore :: RTB.T U.Beats (Gem ProType) -> Int
baseScore = sum . fmap gemScore where
  gemScore = \case
    Kick         -> 30
    Red          -> 30
    Pro _ Cymbal -> 30
    Pro _ Tom    -> 25

perfectSoloBonus :: (Ord a) => RTB.T U.Beats Bool -> RTB.T U.Beats (Gem a) -> Int
perfectSoloBonus solo gems = sum $ fmap score $ applyStatus (fmap ((),) solo) gems where
  score ([], _) = 0
  score _       = 100
