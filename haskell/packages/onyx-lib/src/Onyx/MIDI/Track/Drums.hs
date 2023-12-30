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
module Onyx.MIDI.Track.Drums where

import           Control.Monad                    (guard, void, (>=>))
import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DeriveHelpers
import           Onyx.Guitar                      (applyStatus)
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import qualified Onyx.PhaseShift.Message          as PS
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U

data DrumTrack t = DrumTrack
  { drumDifficulties   :: Map.Map Difficulty (DrumDifficulty t)
  , drumMood           :: RTB.T t Mood
  , drumToms           :: RTB.T t (ProColor, ProType)
  , drumSingleRoll     :: RTB.T t (Maybe LaneDifficulty)
  , drumDoubleRoll     :: RTB.T t (Maybe LaneDifficulty)
  , drumOverdrive      :: RTB.T t Bool -- ^ white notes to gain energy
  , drumActivation     :: RTB.T t Bool -- ^ drum fill to activate Overdrive, or BRE
  , drumSolo           :: RTB.T t Bool
  , drumPlayer1        :: RTB.T t Bool
  , drumPlayer2        :: RTB.T t Bool
  , drumKick2x         :: RTB.T t () -- no accent support yet
  , drumAnimation      :: RTB.T t Animation
  , drumEnableDynamics :: RTB.T t ()
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (DrumTrack t)

instance ChopTrack DrumTrack where
  chopTake t dt = DrumTrack
    { drumDifficulties = chopTake t <$> drumDifficulties dt
    , drumMood         = U.trackTake t $ drumMood dt
    , drumToms         = U.trackTake t $ drumToms dt -- TODO
    , drumSingleRoll   = chopTakeMaybe t $ drumSingleRoll dt
    , drumDoubleRoll   = chopTakeMaybe t $ drumDoubleRoll dt
    , drumOverdrive    = chopTakeBool t $ drumOverdrive dt
    , drumActivation   = chopTakeBool t $ drumActivation dt
    , drumSolo         = chopTakeBool t $ drumSolo dt
    , drumPlayer1      = chopTakeBool t $ drumPlayer1 dt
    , drumPlayer2      = chopTakeBool t $ drumPlayer2 dt
    , drumKick2x       = U.trackTake t $ drumKick2x dt
    , drumAnimation    = U.trackTake t $ drumAnimation dt -- TODO
    , drumEnableDynamics = U.trackTake t $ drumEnableDynamics dt
    }
  chopDrop t dt = DrumTrack
    { drumDifficulties = chopDrop t <$> drumDifficulties dt
    , drumMood         = chopDropStatus t $ drumMood dt
    , drumToms         = U.trackDrop t $ drumToms dt -- TODO
    , drumSingleRoll   = chopDropMaybe t $ drumSingleRoll dt
    , drumDoubleRoll   = chopDropMaybe t $ drumDoubleRoll dt
    , drumOverdrive    = chopDropBool t $ drumOverdrive dt
    , drumActivation   = chopDropBool t $ drumActivation dt
    , drumSolo         = chopDropBool t $ drumSolo dt
    , drumPlayer1      = chopDropBool t $ drumPlayer1 dt
    , drumPlayer2      = chopDropBool t $ drumPlayer2 dt
    , drumKick2x       = U.trackDrop t $ drumKick2x dt
    , drumAnimation    = U.trackDrop t $ drumAnimation dt -- TODO
    , drumEnableDynamics = U.trackDrop t $ drumEnableDynamics dt
    }

nullDrums :: DrumTrack t -> Bool
nullDrums = all (RTB.null . drumGems) . toList . drumDifficulties

instance TraverseTrack DrumTrack where
  traverseTrack fn (DrumTrack a b c d e f g h i j k l m) = DrumTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h
    <*> fn i <*> fn j <*> fn k <*> fn l <*> fn m

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
  deriving (Eq, Ord, Show, Generic)
  deriving (Enum, Bounded) via GenericFullEnum Animation

data Hit = SoftHit | HardHit
  deriving (Eq, Ord, Show, Enum, Bounded)

data Hand = LH | RH
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Controls the audio files used for the drum track.
data Audio
  = D0 -- ^ One stereo mix for the whole kit.
  | D1 -- ^ Mono kick, mono snare, stereo kit.
  | D2 -- ^ Mono kick, stereo snare, stereo kit.
  | D3 -- ^ Stereo kick, stereo snare, stereo kit.
  | D4 -- ^ Mono kick, stereo kit (including snare).
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Special options that can affect drum audio and pad settings.
data Disco
  = NoDisco     -- ^ All pads are normal.
  | Disco       -- ^ Yellow snare, red hihat. Undone by Pro Drums.
  | DiscoNoFlip -- ^ New in RB3: snare beats where accented hits are 'Yellow'.
  | EasyMix     -- ^ Pre-RB3. 'Easy' sections with only 'Red' and 'Kick' notes.
  | EasyNoKick  -- ^ Pre-RB3. 'Easy' sections with no 'Kick' notes.
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Command (Difficulty, Audio, Disco) where
  fromCommand (diff, audio, disco) = ["mix", T.pack (show $ fromEnum diff), showMix audio disco]
  toCommand = reverseLookup ((,,) <$> each <*> each <*> each) fromCommand

-- | e.g. turns 'D2' and 'Disco' into @\"drums2d\"@
showMix :: Audio -> Disco -> T.Text
showMix audio disco = "drums" <> T.pack (show $ fromEnum audio) <> case disco of
  NoDisco     -> ""
  Disco       -> "d"
  DiscoNoFlip -> "dnoflip"
  -- make sure we don't emit these for drums0 since they don't exist (or at least Magma errors)
  EasyMix     -> case audio of D0 -> ""; _ -> "easy"
  EasyNoKick  -> case audio of D0 -> ""; _ -> "easynokick"

data ProColor = Yellow | Blue | Green
  deriving (Eq, Ord, Show, Enum, Bounded)

data ProType = Cymbal | Tom
  deriving (Eq, Ord, Show, Enum, Bounded)

data Gem a = Kick | Red | Pro ProColor a | Orange
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PSGem = Rimshot | HHOpen | HHSizzle | HHPedal
  deriving (Eq, Ord, Show, Enum, Bounded)

data DrumVelocity
  = VelocityGhost
  | VelocityNormal
  | VelocityAccent
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data DrumDifficulty t = DrumDifficulty
  { drumMix         :: RTB.T t (Audio, Disco)
  , drumPSModifiers :: RTB.T t (PSGem, Bool)
  , drumGems        :: RTB.T t (Gem (), DrumVelocity)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (DrumDifficulty t)

instance TraverseTrack DrumDifficulty where
  traverseTrack fn (DrumDifficulty a b c) = DrumDifficulty
    <$> fn a <*> fn b <*> fn c

instance ChopTrack DrumDifficulty where
  chopTake t = mapTrack $ U.trackTake t
  chopDrop t dd = DrumDifficulty
    { drumMix         = chopDropStatus t $ drumMix         dd
    , drumPSModifiers = chopDropStatus t $ drumPSModifiers dd
    , drumGems        = U.trackDrop    t $ drumGems        dd
    }

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
          decodeCV (drum, (_c, v)) = let
            vel = case v of
              1   -> VelocityGhost
              127 -> VelocityAccent
              _   -> VelocityNormal
            in (drum, vel)
          encodeCV (drum, vel) = let
            v = case vel of
              VelocityGhost  -> 1
              VelocityNormal -> 96
              VelocityAccent -> 127
            in (drum, (0, v))
          allDrums = [Kick, Red, Pro Yellow (), Pro Blue (), Pro Green (), Orange]
      chordSnap [base - 1 .. base + 5]
      drumGems <- drumGems =. do
        dimap (fmap encodeCV) (fmap decodeCV) $ condenseMap $ eachKey allDrums $ \drum -> do
          blipCV $ base + case drum of
            Kick          -> 0
            Red           -> 1
            Pro Yellow () -> 2
            Pro Blue   () -> 3
            Pro Green  () -> 4
            Orange        -> 5
      drumMix <- drumMix =. let
        parse = toCommand >=> \(diff', aud, dsc) -> guard (diff == diff') >> Just (aud, dsc)
        unparse (aud, dsc) = fromCommand (diff :: Difficulty, aud :: Audio, dsc :: Disco)
        in commandMatch' parse unparse
      drumPSModifiers <- (drumPSModifiers =.) $ condenseMap $ eachKey each $ sysexPS diff . \case
        Rimshot  -> PS.SnareRimshot
        HHOpen   -> PS.HihatOpen
        HHSizzle -> PS.HihatSizzle
        HHPedal  -> PS.HihatPedal
      return DrumDifficulty{..}
    drumKick2x <- drumKick2x =. fatBlips (1/8) (blip 95)
    -- TODO 2x kicks should be blip-grouped with expert track
    drumAnimation <- (drumAnimation =.) $ fatBlips (1/8) $ condenseMap_ $ eachKey each $ \case
      KickRF            -> blip 24
      HihatOpen b       -> edge 25 b
      Snare HardHit LH  -> blip 26
      Snare HardHit RH  -> blip 27
      Snare SoftHit LH  -> blip 28
      Snare SoftHit RH  -> blip 29
      Hihat LH          -> blip 30
      Hihat RH          -> blip 31
      PercussionRH      -> blip 32
      -- 33 unused
      Crash1 HardHit LH -> blip 34
      Crash1 SoftHit LH -> blip 35
      Crash1 HardHit RH -> blip 36
      Crash1 SoftHit RH -> blip 37
      Crash2 HardHit RH -> blip 38
      Crash2 SoftHit RH -> blip 39
      Crash2RHChokeLH   -> blip 40
      Crash1RHChokeLH   -> blip 41
      Ride RH           -> blip 42
      Ride LH           -> blip 43
      Crash2 HardHit LH -> blip 44
      Crash2 SoftHit LH -> blip 45
      Tom1 LH           -> blip 46
      Tom1 RH           -> blip 47
      Tom2 LH           -> blip 48
      Tom2 RH           -> blip 49
      FloorTom LH       -> blip 50
      FloorTom RH       -> blip 51
      RideSide True     -> commandMatch ["ride_side_true"]
      RideSide False    -> commandMatch ["ride_side_false"]
    drumEnableDynamics <- drumEnableDynamics =. Codec
      -- support text event both with and without brackets
      -- (the no-brackets version gets turned into a lyric by preprocessing)
      { codecIn = slurpTrack $ \mt -> let
        (brackets  , cmds'  ) = RTB.partition (== ["ENABLE_CHART_DYNAMICS"]) $ midiCommands mt
        (noBrackets, lyrics') = RTB.partition (== "ENABLE_CHART_DYNAMICS") $ midiLyrics mt
        mt' = mt
          { midiCommands = cmds'
          , midiLyrics   = lyrics'
          }
        in (RTB.merge (void brackets) (void noBrackets), mt')
      -- always emit the no-brackets version, so Magma is ok with it
      , codecOut = makeTrackBuilder $ fmap $ \() -> E.MetaEvent $ Meta.TextEvent "ENABLE_CHART_DYNAMICS"
      }
    return DrumTrack{..}

fiveToPro :: (NNC.C t) => ProColor -> RTB.T t (Gem (), DrumVelocity) -> RTB.T t (Gem ProType, DrumVelocity)
fiveToPro fallback = let
  eachInstant instant = flip map instant $ \case
    (Kick         , vel) -> (Kick             , vel)
    (Red          , vel) -> (Red              , vel)
    (Pro Yellow (), vel) -> (Pro Yellow Cymbal, vel)
    (Pro Blue   (), vel) -> (Pro Blue   Tom   , vel)
    (Pro Green  (), vel) -> (Pro Green  Tom   , vel)
    (Orange       , vel) -> let
      color = if
        | any (\(gem, _vel) -> gem == Pro Blue  ()) instant -> Green
        | any (\(gem, _vel) -> gem == Pro Green ()) instant -> Blue
        | otherwise                                         -> fallback
      in (Pro color Cymbal, vel)
  in RTB.flatten . fmap eachInstant . RTB.collectCoincident

getDrumDifficulty :: (NNC.C t) => Maybe Difficulty -> DrumTrack t -> RTB.T t (Gem (), DrumVelocity)
getDrumDifficulty diff trk = let
  base = drumGems $ fromMaybe mempty $ Map.lookup (fromMaybe Expert diff) $ drumDifficulties trk
  in case diff of
    Nothing -> RTB.merge base $ fmap (\() -> (Kick, VelocityNormal)) $ drumKick2x trk
    _       -> base

computePro :: (NNC.C t) => Maybe Difficulty -> DrumTrack t -> RTB.T t (Gem ProType, DrumVelocity)
computePro diff trk = let
  toms = fmap (fmap $ \case Tom -> True; Cymbal -> False) $ drumToms trk
  this = fromMaybe mempty $ Map.lookup (fromMaybe Expert diff) $ drumDifficulties trk
  disco = fmap (\(_aud, dsc) -> ((), dsc == Disco)) $ drumMix this
  applied = applyStatus disco $ applyStatus toms $ getDrumDifficulty diff trk
  in flip fmap applied $ \case
    (instantDisco, (instantToms, (gem, vel))) -> let
      isDisco = not $ null instantDisco
      gem' = case gem of
        Kick -> Kick
        Red -> if isDisco
          then Pro Yellow Cymbal
          else Red
        Pro Yellow () | isDisco -> Red
        Pro color () -> Pro color $ if elem color instantToms then Tom else Cymbal
        Orange -> Orange -- probably shouldn't happen
      in (gem', vel)

type RealDrum = Either PSGem (Gem ProType)

computePSReal :: (NNC.C t) => Maybe Difficulty -> DrumTrack t -> RTB.T t (RealDrum, DrumVelocity)
computePSReal diff trk = let
  pro = computePro diff trk
  this = fromMaybe mempty $ Map.lookup (fromMaybe Expert diff) $ drumDifficulties trk
  applied = applyStatus (drumPSModifiers this) pro
  in flip fmap applied $ \(mods, (gem, vel)) -> let
    gem' = case gem of
      Red
        | elem Rimshot mods -> Left Rimshot
      Pro Yellow Cymbal
        | elem HHOpen mods -> Left HHOpen
        | elem HHPedal mods -> Left HHPedal
        | elem HHSizzle mods -> Left HHSizzle
      _ -> Right gem
    in (gem', vel)

encodePSReal :: (NNC.C t) => t -> Difficulty -> RTB.T t (RealDrum, DrumVelocity) -> DrumTrack t
encodePSReal blipTime diff real = let
  modifiers = U.trackJoin $ RTB.flatten $ go $ RTB.collectCoincident real
  go (Wait dt xs rest) = let
    thisBlipTime = case rest of
      Wait dt2 _ _ -> min blipTime dt2
      RNil         -> blipTime
    events = flip map xs $ \case
      (Left ps, _)               -> Wait NNC.zero (Left (ps, True)) $ Wait thisBlipTime (Left (ps, False)) RNil
      (Right (Pro color Tom), _) -> Wait NNC.zero (Right (color, Tom)) $ Wait thisBlipTime (Right (color, Cymbal)) RNil
      _                          -> RNil
    in Wait dt events $ go rest
  go RNil = RNil
  psmods = RTB.mapMaybe (\case Left x -> Just x; _ -> Nothing) modifiers
  toms = RTB.mapMaybe (\case Right x -> Just x; _ -> Nothing) modifiers
  gems = flip fmap real $ \(gem, vel) -> let
    gem' = case gem of
      Left ps -> case ps of
        Rimshot  -> Red
        HHOpen   -> Pro Yellow ()
        HHSizzle -> Pro Yellow ()
        HHPedal  -> Pro Yellow ()
      Right rb -> void rb
    in (gem', vel)
  in mempty
    { drumToms = toms
    , drumDifficulties = Map.singleton diff DrumDifficulty
      { drumMix = RNil
      , drumPSModifiers = psmods
      , drumGems = gems
      }
    }

psRealToPro :: (NNC.C t) => DrumTrack t -> DrumTrack t
psRealToPro trk = trk
  { drumDifficulties = flip Map.mapWithKey (drumDifficulties trk) $ \diff this -> this
    -- this will fail if you use discobeat on the real track, so don't do that
    { drumPSModifiers = RTB.empty
    , drumGems = flip RTB.mapMaybe (computePSReal (Just diff) trk) $ \case
      (Left Rimshot, vel) -> Just (Red, vel)
      (Left HHPedal, _)   -> Nothing
      (Left _, vel)       -> Just (Pro Yellow (), vel)
      (Right gem, vel)    -> Just (void gem, vel)
    }
  }

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

autoSticking :: [AnimPad] -> [Hand]
autoSticking pads = let
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

fillDrumAnimation :: U.Seconds -> U.TempoMap -> DrumTrack U.Beats -> DrumTrack U.Beats
fillDrumAnimation closeTime tmap trk = let
  autoAnims
    = U.unapplyTempoTrack tmap
    $ autoDrumAnimation closeTime
    $ fmap fst
    $ U.applyTempoTrack tmap
    $ computePro (Just Expert) trk
  in if RTB.null $ drumAnimation trk
    then trk { drumAnimation = autoAnims }
    else trk

autoDrumAnimation :: (NNC.C t) => t -> RTB.T t (Gem ProType) -> RTB.T t Animation
autoDrumAnimation closeTime pro = let
  hands = RTB.flatten $ flip fmap (RTB.collectCoincident pro) $ \inst -> if
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
  kicks = RTB.mapMaybe (\case Kick -> Just KickRF; _ -> Nothing) pro
  in RTB.merge kicks $ autoDrumHands closeTime
    -- TODO pass along velocity from original track
    $ fmap (\pad -> AnimInput pad VelocityNormal) hands

data AnimInput = AnimInput
  { aiPad      :: AnimPad
  , aiVelocity :: DrumVelocity
  -- TODO support optional sticking input (from full drums)
  } deriving (Eq, Ord)

autoDrumHands :: (NNC.C t) => t -> RTB.T t AnimInput -> RTB.T t Animation
autoDrumHands closeTime pads = let
  groups = flip RTB.mapMaybe (RTB.collectCoincident pads) $ \case
    x : y : _ -> Just $ Right (min x y, max x y)
    [x]       -> Just $ Left x
    []        -> Nothing
  applySticking = \case
    RNil -> RNil
    Wait dt (Right pair) rest -> Wait dt (Right pair) $ applySticking rest
    Wait dt (Left x) rest -> let
      (phrase, afterPhrase) = getPhrase rest
      phrase' = (dt, x) : phrase
      computed = autoSticking $ map (aiPad . snd) phrase'
      outputPhrase = zipWith (\(dt', pad) hand -> Wait dt' $ Left (pad, hand)) phrase' computed
      in foldr ($) (applySticking afterPhrase) outputPhrase
  getPhrase = \case
    Wait dt (Left x) rest | dt <= closeTime -> let
      (phrase, afterPhrase) = getPhrase rest
      in ((dt, x) : phrase, afterPhrase)
    afterPhrase -> ([], afterPhrase)
  makeAnimations rtb = RTB.flatten $ flip fmap rtb $ \case
    Right (x, y)   -> [makeSingle x LH, makeSingle y RH]
    Left (x, hand) -> [makeSingle x hand]
  makeSingle ai hand = case aiPad ai of
    AnimSnare    -> Snare hit hand
    AnimHihat    -> Hihat hand
    AnimCrash1   -> Crash1 hit hand
    AnimTom1     -> Tom1 hand
    AnimTom2     -> Tom2 hand
    AnimFloorTom -> FloorTom hand
    AnimCrash2   -> Crash2 hit hand
    AnimRide     -> Ride hand
    where hit = case aiVelocity ai of
            VelocityGhost -> SoftHit
            _             -> HardHit
  in makeAnimations $ applySticking groups

expertWith2x :: (NNC.C t) => DrumTrack t -> DrumTrack t
expertWith2x dt = let
  add2x = flip Map.adjust Expert $ \dd -> dd
    { drumGems = RTB.merge (drumGems dd) $ (Kick, VelocityNormal) <$ drumKick2x dt
    }
  in dt
    { drumDifficulties = add2x $ drumDifficulties dt
    , drumKick2x = RTB.empty
    }

-- | Changes all existing drum mix events to use the given config (not changing
-- stuff like discobeat), and places ones at the beginning if they don't exist
-- already.
setDrumMix :: (NNC.C t) => Audio -> DrumTrack t -> DrumTrack t
setDrumMix audio trk = let
  f dd = dd
    { drumMix = let
      mixSet = fmap (\(_, disco) -> (audio, disco)) $ drumMix dd
      alreadyMixed = case (RTB.viewL $ drumMix dd, RTB.viewL $ drumGems dd) of
        (Just ((tmix, _), _), Just ((tnote, _), _)) -> tmix <= tnote
        _                                           -> False
      in if alreadyMixed then mixSet else RTB.cons NNC.zero (audio, NoDisco) mixSet
    }
  in trk { drumDifficulties = fmap f $ drumDifficulties trk }
