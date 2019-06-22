{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module RockBand.Codec.Drums where

import           Control.Monad                    (guard, (>=>))
import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import           Guitars                          (applyStatus)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common
import qualified RockBand.PhaseShiftMessage       as PS

data DrumTrack t = DrumTrack
  { drumDifficulties :: Map.Map Difficulty (DrumDifficulty t)
  , drumMood         :: RTB.T t Mood
  , drumToms         :: RTB.T t (ProColor, ProType)
  , drumSingleRoll   :: RTB.T t (Maybe LaneDifficulty)
  , drumDoubleRoll   :: RTB.T t (Maybe LaneDifficulty)
  , drumOverdrive    :: RTB.T t Bool -- ^ white notes to gain energy
  , drumActivation   :: RTB.T t Bool -- ^ drum fill to activate Overdrive, or BRE
  , drumSolo         :: RTB.T t Bool
  , drumPlayer1      :: RTB.T t Bool
  , drumPlayer2      :: RTB.T t Bool
  , drumKick2x       :: RTB.T t ()
  , drumAnimation    :: RTB.T t Animation
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (DrumTrack t)

nullDrums :: DrumTrack t -> Bool
nullDrums = all (RTB.null . drumGems) . toList . drumDifficulties

instance TraverseTrack DrumTrack where
  traverseTrack fn (DrumTrack a b c d e f g h i j k l) = DrumTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h
    <*> fn i <*> fn j <*> fn k <*> fn l

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
  deriving (Eq, Ord, Show, Read, Generic)
  deriving (Enum, Bounded) via GenericFullEnum Animation

data Hit = SoftHit | HardHit
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Hand = LH | RH
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Controls the audio files used for the drum track.
data Audio
  = D0 -- ^ One stereo mix for the whole kit.
  | D1 -- ^ Mono kick, mono snare, stereo kit.
  | D2 -- ^ Mono kick, stereo snare, stereo kit.
  | D3 -- ^ Stereo kick, stereo snare, stereo kit.
  | D4 -- ^ Mono kick, stereo kit (including snare).
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Special options that can affect drum audio and pad settings.
data Disco
  = NoDisco     -- ^ All pads are normal.
  | Disco       -- ^ Yellow snare, red hihat. Undone by Pro Drums.
  | DiscoNoFlip -- ^ New in RB3: snare beats where accented hits are 'Yellow'.
  | EasyMix     -- ^ Pre-RB3. 'Easy' sections with only 'Red' and 'Kick' notes.
  | EasyNoKick  -- ^ Pre-RB3. 'Easy' sections with no 'Kick' notes.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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

data ProColor = Yellow | Blue | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ProType = Cymbal | Tom
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Gem a = Kick | Red | Pro ProColor a | Orange
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data PSGem = Rimshot | HHOpen | HHSizzle | HHPedal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data DrumDifficulty t = DrumDifficulty
  { drumMix         :: RTB.T t (Audio, Disco)
  , drumPSModifiers :: RTB.T t (PSGem, Bool)
  , drumGems        :: RTB.T t (Gem ())
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (DrumDifficulty t)

instance TraverseTrack DrumDifficulty where
  traverseTrack fn (DrumDifficulty a b c) = DrumDifficulty
    <$> fn a <*> fn b <*> fn c

parseProType :: (Monad m, NNC.C t) => Int -> TrackEvent m t ProType
parseProType
  = dimap
    (fmap $ \case Tom -> True; Cymbal -> False)
    (fmap $ \b -> if b then Tom else Cymbal)
  . edges

instance ParseTrack DrumTrack where
  parseTrack = do
    drumMood <- drumMood =. command
    drumToms <- (drumToms =.) $ condenseMap $ eachKey each $ parseProType . \case
      Yellow -> 110
      Blue   -> 111
      Green  -> 112
    drumSingleRoll <- drumSingleRoll =. edgesLanes 126
    drumDoubleRoll <- drumDoubleRoll =. edgesLanes 127
    drumOverdrive <- drumOverdrive =. edges 116
    drumActivation <- drumActivation =. edgesBRE [120 .. 124]
    drumSolo <- drumSolo =. edges 103
    drumPlayer1 <- drumPlayer1 =. edges 105
    drumPlayer2 <- drumPlayer2 =. edges 106
    drumDifficulties <- (drumDifficulties =.) $ eachKey each $ \diff -> fatBlips (1/8) $ do
      let base = case diff of
            Easy   -> 60
            Medium -> 72
            Hard   -> 84
            Expert -> 96
          allDrums = [Kick, Red, Pro Yellow (), Pro Blue (), Pro Green (), Orange]
      drumGems <- (drumGems =.) $ condenseMap_ $ eachKey allDrums $ \drum -> do
        blip $ base + case drum of
          Kick          -> 0
          Red           -> 1
          Pro Yellow () -> 2
          Pro Blue   () -> 3
          Pro Green  () -> 4
          Orange        -> 5
      drumMix <- drumMix =. let
        parse = readCommand' >=> \(diff', aud, dsc) -> guard (diff == diff') >> Just (aud, dsc)
        unparse (aud, dsc) = showCommand' (diff :: Difficulty, aud :: Audio, dsc :: Disco)
        in single parse unparse
      drumPSModifiers <- (drumPSModifiers =.) $ condenseMap $ eachKey each $ sysexPS diff . \case
        Rimshot  -> PS.SnareRimshot
        HHOpen   -> PS.HihatOpen
        HHSizzle -> PS.HihatSizzle
        HHPedal  -> PS.HihatPedal
      return DrumDifficulty{..}
    drumKick2x <- drumKick2x =. fatBlips (1/8) (blip 95)
    -- TODO 2x kicks should be blip-grouped with expert track
    drumAnimation <- (drumAnimation =.) $ fatBlips (1/8) $ condenseMap_ $ eachKey each $ \case
      KickRF -> blip 24
      HihatOpen b -> edge 25 b
      Snare HardHit LH -> blip 26
      Snare HardHit RH -> blip 27
      Snare SoftHit LH -> blip 28
      Snare SoftHit RH -> blip 29
      Hihat LH -> blip 30
      Hihat RH -> blip 31
      PercussionRH -> blip 32
      -- 33 unused
      Crash1 HardHit LH -> blip 34
      Crash1 SoftHit LH -> blip 35
      Crash1 HardHit RH -> blip 36
      Crash1 SoftHit RH -> blip 37
      Crash2 HardHit RH -> blip 38
      Crash2 SoftHit RH -> blip 39
      Crash2RHChokeLH -> blip 40
      Crash1RHChokeLH -> blip 41
      Ride RH -> blip 42
      Ride LH -> blip 43
      Crash2 HardHit LH -> blip 44
      Crash2 SoftHit LH -> blip 45
      Tom1 LH -> blip 46
      Tom1 RH -> blip 47
      Tom2 LH -> blip 48
      Tom2 RH -> blip 49
      FloorTom LH -> blip 50
      FloorTom RH -> blip 51
      RideSide True -> commandMatch ["ride_side_true"]
      RideSide False -> commandMatch ["ride_side_false"]
    return DrumTrack{..}

computePro :: (NNC.C t) => Maybe Difficulty -> DrumTrack t -> RTB.T t (Gem ProType)
computePro diff trk = let
  toms = fmap (fmap $ \case Tom -> True; Cymbal -> False) $ drumToms trk
  this = fromMaybe mempty $ Map.lookup (fromMaybe Expert diff) $ drumDifficulties trk
  disco = fmap (\(_aud, dsc) -> ((), dsc == Disco)) $ drumMix this
  applied = applyStatus disco $ applyStatus toms $ case diff of
    Nothing -> RTB.merge (fmap (\() -> Kick) $ drumKick2x trk) $ drumGems this
    _       -> drumGems this
  in flip fmap applied $ \case
    (instantDisco, (instantToms, gem)) -> case gem of
      Kick -> Kick
      Red -> if isDisco
        then Pro Yellow Cymbal
        else Red
      Pro Yellow () | isDisco -> Red
      Pro color () -> Pro color $ if elem color instantToms then Tom else Cymbal
      Orange -> Orange -- probably shouldn't happen
      where isDisco = not $ null instantDisco

type RealDrum = Either PSGem (Gem ProType)

computePSReal :: (NNC.C t) => Maybe Difficulty -> DrumTrack t -> RTB.T t RealDrum
computePSReal diff trk = let
  pro = computePro diff trk
  this = fromMaybe mempty $ Map.lookup (fromMaybe Expert diff) $ drumDifficulties trk
  applied = applyStatus (drumPSModifiers this) pro
  in flip fmap applied $ \case
    (mods, Red)
      | elem Rimshot mods -> Left Rimshot
    (mods, Pro Yellow Cymbal)
      | elem HHOpen mods -> Left HHOpen
      | elem HHPedal mods -> Left HHPedal
      | elem HHSizzle mods -> Left HHSizzle
    (_, gem) -> Right gem

psRealToPro :: (NNC.C t) => DrumTrack t -> DrumTrack t
psRealToPro trk = trk
  { drumDifficulties = flip Map.mapWithKey (drumDifficulties trk) $ \diff this -> let
    -- this will fail if you use discobeat on the real track, so don't do that
    merged = RTB.merge (Left <$> computePSReal (Just diff) trk) (Right <$> drumGems this)
    eachInstant xs = flip filter [ x | Right x <- xs ] $ \case
      Pro Yellow () -> notElem (Left $ Left HHPedal) xs
      _             -> True
    in this
      { drumPSModifiers = RTB.empty
      , drumGems = RTB.flatten $ fmap eachInstant $ RTB.collectCoincident merged
      }
  }

baseScore :: RTB.T t (Gem ProType) -> Int
baseScore = sum . fmap gemScore where
  gemScore = \case
    Kick         -> 30
    Red          -> 30
    Pro _ Cymbal -> 30
    Pro _ Tom    -> 25
    Orange       -> 30 -- no actual answer

perfectSoloBonus :: (NNC.C t, Ord a) => RTB.T t Bool -> RTB.T t (Gem a) -> Int
perfectSoloBonus solo gems = sum $ fmap score $ applyStatus (fmap ((),) solo) gems where
  score ([], _) = 0
  score _       = 100

data AnimPad
  = AnimSnare
  | AnimHihat
  | AnimCrash1
  | AnimTom1
  | AnimTom2
  | AnimFloorTom
  | AnimCrash2
  | AnimRide
  deriving (Eq, Ord)

normalDirection :: AnimPad -> AnimPad -> Maybe Hand
normalDirection AnimHihat AnimSnare = Nothing
normalDirection AnimSnare AnimHihat = Nothing
normalDirection AnimSnare AnimTom1  = Nothing
normalDirection AnimTom1  AnimSnare = Nothing
normalDirection x         y         = case compare x y of
  LT -> Just RH
  GT -> Just LH
  EQ -> Nothing

countNothings :: [Maybe a] -> Maybe (Int, a)
countNothings = go 0 where
  go !_ []             = Nothing
  go !n (Nothing : xs) = go (n + 1) xs
  go !n (Just x  : _ ) = Just (n, x)

autoSticking :: Maybe (AnimPad, AnimPad) -> [AnimPad] -> Maybe (AnimPad, AnimPad) -> [Hand]
autoSticking _           []   _         = [] -- shouldn't happen
autoSticking _boundStart pads _boundEnd = let
  flipHand RH = LH
  flipHand LH = RH
  go _    []       = []
  go prev xs@(x : xt) = let
    -- rule: any double strokes, we do at the latest possible time.
    -- so e.g. BRRRRB (ride and snare) would be RLRLLR, not RLLRLR
    hand = case prev of
      Nothing -> case countNothings $ zipWith normalDirection xs xt of
        Nothing     -> RH -- if no movement, just start with RH
        Just (n, h) -> if even n then flipHand h else h
      Just (prevHand, prevPad) -> if x == prevPad
        then case xt of
          y : _ | Just (flipHand prevHand) == normalDirection x y
            -- do a double stroke so we don't have to cross for the next hit
            -> prevHand
          _ -> flipHand prevHand
        else flipHand prevHand -- if we move to a different pad, always switch hands
    in hand : go (Just (hand, x)) xt
  in go Nothing pads

autoDrumAnimation :: (NNC.C t) => t -> RTB.T t (Gem ProType) -> RTB.T t Animation
autoDrumAnimation closeTime pro = let
  hands = flip RTB.mapMaybe (RTB.collectCoincident pro) $ \inst -> let
    anims = if
      | all (`elem` inst) [Pro Yellow Cymbal, Pro Green Cymbal] -> [AnimCrash1, AnimCrash2]
      | all (`elem` inst) [Pro Blue Cymbal, Pro Green Cymbal] -> [AnimCrash1, AnimCrash2]
      | all (`elem` inst) [Pro Yellow Cymbal, Orange] -> [AnimCrash1, AnimCrash2]
      | all (`elem` inst) [Red, Pro Yellow Tom] -> [AnimSnare, AnimSnare]
      | otherwise -> flip mapMaybe inst $ \case
        Red               -> Just AnimSnare
        Pro Yellow Cymbal -> Just AnimHihat
        Pro Blue Cymbal   -> Just AnimRide
        Pro Green Cymbal  -> Just AnimCrash2
        Pro Yellow Tom    -> Just AnimTom1
        Pro Blue Tom      -> Just AnimTom2
        Pro Green Tom     -> Just AnimFloorTom
        Orange            -> Just AnimCrash2
        Kick              -> Nothing
    in case anims of
      x : y : _ -> Just $ Right (min x y, max x y)
      [x]       -> Just $ Left x
      []        -> Nothing
  applySticking prev = \case
    RNil -> RNil
    Wait dt (Right pair) rest -> Wait dt (Right pair) $ applySticking (Just pair) rest
    Wait dt (Left x) rest -> let
      prevBound = guard (dt <= closeTime) >> prev
      (phrase, afterPhrase) = getPhrase rest
      phrase' = (dt, x) : phrase
      nextBound = case afterPhrase of
        Wait dt' (Right pair) _ | dt' <= closeTime -> Just pair
        _                                          -> Nothing
      computed = autoSticking prevBound (map snd phrase') nextBound
      outputPhrase = zipWith (\(dt', pad) hand -> Wait dt' $ Left (pad, hand)) phrase' computed
      in foldr ($) (applySticking Nothing afterPhrase) outputPhrase
  getPhrase = \case
    Wait dt (Left x) rest | dt <= closeTime -> let
      (phrase, afterPhrase) = getPhrase rest
      in ((dt, x) : phrase, afterPhrase)
    afterPhrase -> ([], afterPhrase)
  makeAnimations rtb = RTB.flatten $ flip fmap rtb $ \case
    Right (x, y) -> [makeSingle x LH, makeSingle y RH]
    Left (x, hand) -> [makeSingle x hand]
  makeSingle pad hand = case pad of
    AnimSnare    -> Snare HardHit hand
    AnimHihat    -> Hihat hand
    AnimCrash1   -> Crash1 HardHit hand
    AnimTom1     -> Tom1 hand
    AnimTom2     -> Tom2 hand
    AnimFloorTom -> FloorTom hand
    AnimCrash2   -> Crash2 HardHit hand
    AnimRide     -> Ride hand
  kicks = RTB.mapMaybe (\case Kick -> Just KickRF; _ -> Nothing) pro
  in RTB.merge kicks $ makeAnimations $ applySticking Nothing hands
