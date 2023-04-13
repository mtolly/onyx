{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module Onyx.MIDI.Track.Drums.Full where

import           Control.Monad                    (guard, void, when)
import           Control.Monad.Codec
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer       (execWriter, tell)
import           Data.Bifunctor                   (first)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   listToMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.Drums            (DrumVelocity (..))
import qualified Onyx.MIDI.Track.Drums            as D
import qualified Sound.MIDI.Util                  as U

data FullDrumTrack t = FullDrumTrack
  { fdDifficulties :: Map.Map Difficulty (FullDrumDifficulty t)
  , fdKick2        :: RTB.T t () -- TODO (gem type ?) and velocity
  , fdLanes        :: RTB.T t (Edge () FullGem)
  , fdOverdrive    :: RTB.T t Bool
  , fdActivation   :: RTB.T t Bool
  , fdSolo         :: RTB.T t Bool
  , fdSticking     :: RTB.T t D.Hand -- this should probably be per-difficulty?
  -- overrides for DTX sounds
  , fdChipOverride :: RTB.T t (LongNote () T.Text)
  -- overrides for kick assignment in DTX
  , fdFooting      :: RTB.T t D.Hand
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (FullDrumTrack t)

instance TraverseTrack FullDrumTrack where
  traverseTrack fn (FullDrumTrack a b c d e f g h i) = FullDrumTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h <*> fn i

data FullDrumDifficulty t = FullDrumDifficulty
  { fdGems :: RTB.T t (FullGem, FullGemType, DrumVelocity)
  , fdFlam :: RTB.T t ()
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (FullDrumDifficulty t)

instance TraverseTrack FullDrumDifficulty where
  traverseTrack fn (FullDrumDifficulty a b) = FullDrumDifficulty <$> fn a <*> fn b

data FullGem
  = Kick
  | Snare
  | Hihat
  | HihatFoot
  | CrashL
  | Tom1
  | Tom2
  | Tom3
  | CrashR
  | Ride
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data FullGemType
  = GemNormal
  | GemHihatOpen
  | GemHihatClosed
  | GemCymbalChoke
  | GemRim
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ParseTrack FullDrumTrack where
  parseTrack = do
    fdSolo <- fdSolo =. edges 115
    fdOverdrive <- fdOverdrive =. edges 116
    fdActivation <- fdActivation =. edges 120
    fdLanes <- (fdLanes =.) $ translateEdges
      $ condenseMap $ eachKey each $ \drum -> case drum of
        Kick      -> edges $ 84 + 0
        Snare     -> edges $ 84 + 1
        Hihat     -> edges $ 84 + 2
        HihatFoot -> return mempty
        CrashL    -> edges $ 84 + 3
        Tom1      -> edges $ 84 + 4
        Tom2      -> edges $ 84 + 5
        Tom3      -> edges $ 84 + 6
        CrashR    -> edges $ 84 + 7
        Ride      -> edges $ 84 + 8
    fdSticking <- (fdSticking =.) $ condenseMap_ $ eachKey each $ blip . \case
      D.RH -> 112
      D.LH -> 111
    fdFooting <- (fdFooting =.) $ condenseMap_ $ eachKey each $ blip . \case
      D.RH -> 109
      D.LH -> 108
    fdDifficulties <- (fdDifficulties =.) $ eachKey each $ \diff -> fatBlips (1/8) $ do
      let base = case diff of
            Easy   -> 24
            Medium -> 48
            Hard   -> 72
            Expert -> 96
          decodeCV (drum, (c, v)) = let
            gtype = case c of
              1 -> GemHihatOpen
              2 -> GemHihatClosed
              3 -> GemCymbalChoke
              4 -> GemRim
              _ -> GemNormal
            vel = case v of
              1   -> VelocityGhost
              127 -> VelocityAccent
              _   -> VelocityNormal
            in (drum, gtype, vel)
          encodeCV (drum, gtype, vel) = let
            c = case gtype of
              GemNormal      -> 0
              GemHihatOpen   -> 1
              GemHihatClosed -> 2
              GemCymbalChoke -> 3
              GemRim         -> 4
            v = case vel of
              VelocityGhost  -> 1
              VelocityNormal -> 96
              VelocityAccent -> 127
            in (drum, (c, v))
      fdGems <- fdGems =. do
        dimap (fmap encodeCV) (fmap decodeCV) $ condenseMap $ eachKey each $ \drum -> do
          blipCV $ base + case drum of
            HihatFoot -> -2
            Kick      -> 0
            Snare     -> 1
            Hihat     -> 2
            CrashL    -> 3
            Tom1      -> 4
            Tom2      -> 5
            Tom3      -> 6
            CrashR    -> 7
            Ride      -> 8
      fdFlam <- fdFlam =. blip (base + 10)
      return FullDrumDifficulty{..}
    fdKick2 <- fdKick2 =. fatBlips (1/8) (blip 95)
    fdChipOverride <- fdChipOverride =. dimap (fmap ChipOverrideEvent) (fmap fromChipOverrideEvent) command
    return FullDrumTrack{..}

newtype ChipOverrideEvent = ChipOverrideEvent { fromChipOverrideEvent :: LongNote () T.Text }

instance Command ChipOverrideEvent where
  toCommand = fmap ChipOverrideEvent . \case
    ["chip", tag         ] -> Just $ Blip   () tag
    ["chip", tag, "start"] -> Just $ NoteOn () tag
    ["chip", tag, "end"  ] -> Just $ NoteOff   tag
    _                      -> Nothing
  fromCommand (ChipOverrideEvent ln) = case ln of
    Blip   () tag -> ["chip", tag         ]
    NoteOn () tag -> ["chip", tag, "start"]
    NoteOff   tag -> ["chip", tag, "end"  ]

fullDrumNoteNames :: [(Int, T.Text)]
fullDrumNoteNames = execWriter $ do
  o 120 "DRUM FILL"
  o 116 "OVERDRIVE"
  o 115 "SOLO"
  x 114
  o 112 "RIGHT HAND"
  o 111 "LEFT HAND"
  x 110
  o 109 "RIGHT FOOT"
  o 108 "LEFT FOOT"
  let difficulty letter base = do
        x (base + 11)
        o (base + 10) $ letter <> " Flam"
        x (base + 9 )
        o (base + 8 ) $ letter <> " Ride"
        o (base + 7 ) $ letter <> " Crash R"
        o (base + 6 ) $ letter <> " Tom 3"
        o (base + 5 ) $ letter <> " Tom 2"
        o (base + 4 ) $ letter <> " Tom 1"
        o (base + 3 ) $ letter <> " Crash L"
        o (base + 2 ) $ letter <> " Hihat"
        o (base + 1 ) $ letter <> " Snare"
        o base        $ letter <> " Kick"
        o (base - 2 ) $ letter <> " Hihat Pedal"
  difficulty "X" 96
  o 95 "X+ Kick 2"
  x (84 + 9)
  o (84 + 8) $ "Lane Ride"
  o (84 + 7) $ "Lane Crash R"
  o (84 + 6) $ "Lane Tom 3"
  o (84 + 5) $ "Lane Tom 2"
  o (84 + 4) $ "Lane Tom 1"
  o (84 + 3) $ "Lane Crash L"
  o (84 + 2) $ "Lane Hihat"
  o (84 + 1) $ "Lane Snare"
  o 84       $ "Lane Kick"
  difficulty "H" 72
  difficulty "M" 48
  difficulty "E" 24
  where o k v = tell [(k, v)]
        x k = tell [(k, "----")]

data MergedEvent n1 n2
  = MergedNote n1
  | MergedNote2 n2
  | MergedSticking D.Hand
  | MergedFooting D.Hand
  | MergedFlam
  deriving (Eq, Ord)

data FlamStatus = NotFlam | Flam
  deriving (Eq, Ord, Show)

data FullDrumNote a = FullDrumNote
  { fdn_gem      :: FullGem
  , fdn_type     :: FullGemType
  , fdn_velocity :: DrumVelocity
  , fdn_limb     :: Maybe D.Hand -- hand, or foot
  , fdn_extra    :: a
  } deriving (Eq, Ord, Show, Functor)

getDifficulty :: (NNC.C t) => Maybe Difficulty -> FullDrumTrack t -> RTB.T t (FullDrumNote FlamStatus)
getDifficulty diff trk = let
  base = fromMaybe mempty $ Map.lookup (fromMaybe Expert diff) $ fdDifficulties trk
  events = foldr RTB.merge RTB.empty
    $ fmap MergedNote (fdGems base)
    : fmap (const MergedFlam) (fdFlam base)
    : fmap MergedSticking (fdSticking trk)
    : fmap MergedFooting (fdFooting trk)
    : case diff of
      Nothing -> [fmap MergedNote2 $ fdKick2 trk]
      _       -> []
  processSlice evts = let
    notKick = [ trio | MergedNote trio@(gem, _, _) <- evts, gem /= Kick ]
    kick = [ trio | MergedNote trio@(Kick, _, _) <- evts ]
    flam = any (\case MergedFlam -> True; _ -> False) evts
    kick2 = any (\case MergedNote2 () -> True; _ -> False) evts
    foot = listToMaybe [ f | MergedFooting f <- evts ]
    outputNotKick = flip fmap notKick $ \(gem, gtype, vel) -> FullDrumNote
      { fdn_gem      = gem
      , fdn_type     = gtype
      , fdn_velocity = vel
      , fdn_limb     = Nothing -- TODO
      , fdn_extra    = if flam && gem /= HihatFoot then Flam else NotFlam
      }
    outputKick = case (kick, kick2) of
      ([], True) -> return FullDrumNote
        { fdn_gem      = Kick
        , fdn_type     = GemNormal
        , fdn_velocity = VelocityNormal
        , fdn_limb     = Just $ fromMaybe D.LH foot
        , fdn_extra    = NotFlam
        }
      _          -> flip fmap kick $ \(gem, gtype, vel) -> FullDrumNote
        { fdn_gem      = gem
        , fdn_type     = gtype
        , fdn_velocity = vel
        , fdn_limb     = Just $ fromMaybe D.RH foot
        , fdn_extra    = if kick2 then Flam else NotFlam
        }
    in outputNotKick <> outputKick
  in RTB.flatten $ fmap processSlice $ RTB.collectCoincident events

-- full to PS/RB conversion

data CymbalInstant lc rd = CymbalInstant
  -- comments here reflect placeCymbalsFancy
  { instantHH :: Maybe D.RealDrum -- always yellow (with ps real mods)
  , instantLC :: Maybe lc -- default yellow, can be pushed to blue
  , instantRD :: Maybe rd -- default blue, can be pushed to green
  , instantRC :: Bool -- always green
  } deriving (Show)

-- Simple version, hihat = Y, left cymbal = B, right cymbal / ride = G
placeCymbals :: (NNC.C t) => RTB.T t (FullDrumNote ()) -> RTB.T t (D.RealDrum, DrumVelocity)
placeCymbals
  -- TODO actually preserve the velocities
  = fmap (, VelocityNormal)
  . RTB.flatten . fmap (emitCymbals . makeInstant) . RTB.collectCoincident
  . fmap (\fdn -> (fdn_gem fdn, fdn_type fdn))
  where

  makeInstant notes = CymbalInstant
    { instantHH = if
      | elem (Hihat, GemHihatClosed) notes -> Just $ Left D.HHSizzle
      | elem (Hihat, GemHihatOpen) notes  -> Just $ Left D.HHOpen
      | any (\case (Hihat, _) -> True; _ -> False) notes  -> Just $ Right $ D.Pro D.Yellow D.Cymbal
      | any (\case (HihatFoot, _) -> True; _ -> False) notes  -> Just $ Left D.HHPedal
      | otherwise             -> Nothing
    , instantLC = guard (any (\case (CrashL, _) -> True; _ -> False) notes) >> Just ()
    , instantRD = guard (any (\case (Ride, _) -> True; _ -> False) notes) >> Just ()
    , instantRC = any (\case (CrashR, _) -> True; _ -> False) notes
    }

  emitCymbals (CymbalInstant hh lc rd rc) = let
    yellow = toList hh
    blue = [Right $ D.Pro D.Blue D.Cymbal | isJust lc || (isJust rd && rc)]
    green = [Right $ D.Pro D.Green D.Cymbal | isJust rd || rc]
    in yellow <> blue <> green

-- Not used anymore. Might have as an option in the future
placeCymbalsFancy :: (NNC.C t) => RTB.T t (FullGem, FullGemType, DrumVelocity) -> RTB.T t (D.RealDrum, DrumVelocity)
placeCymbalsFancy
  -- TODO actually preserve the velocities
  = fmap (, VelocityNormal)
  . RTB.flatten . fmap emitCymbals . assignFull . RTB.filter hasCymbals . fmap makeInstant . RTB.collectCoincident
  . fmap (\(gem, gtype, _vel) -> (gem, gtype))
  where

  makeInstant notes = CymbalInstant
    { instantHH = if
      | elem (Hihat, GemHihatClosed) notes -> Just $ Left D.HHSizzle
      | elem (Hihat, GemHihatOpen) notes  -> Just $ Left D.HHOpen
      | any (\case (Hihat, _) -> True; _ -> False) notes  -> Just $ Right $ D.Pro D.Yellow D.Cymbal
      | any (\case (HihatFoot, _) -> True; _ -> False) notes  -> Just $ Left D.HHPedal
      | otherwise             -> Nothing
    , instantLC = guard (any (\case (CrashL, _) -> True; _ -> False) notes) >> Just Nothing
    , instantRD = guard (any (\case (Ride, _) -> True; _ -> False) notes) >> Just Nothing
    , instantRC = any (\case (CrashR, _) -> True; _ -> False) notes
    }
  hasCymbals = \case
    CymbalInstant Nothing Nothing Nothing False -> False
    _                                           -> True

  -- first, assign all the cymbals that only have one option due to other simultaneous cymbals.
  -- any LC with HH is blue, any RD with RC is blue (or drop it if also LC)
  assignInstant
    :: RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
    -> RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
  assignInstant = fmap $ \now -> let
    newLC = case (instantHH now, instantLC now) of
      (Just _, Just Nothing) -> Just $ Just D.Blue
      _                      -> instantLC now
    newRD = case (newLC, instantRD now, instantRC now) of
      (Just (Just D.Blue), Just _      , True ) -> Nothing -- already blue and green taken
      (Just (Just D.Blue), Just Nothing, False) -> Just $ Just D.Green -- green because LC is blue
      (_                 , Just Nothing, True ) -> Just $ Just D.Blue -- blue because RC is green
      _                                         -> instantRD now
    in now { instantLC = newLC, instantRD = newRD }

  -- then, propagate info forward to do more cymbal assignments.
  -- this gets run both forward and backward
  propagatePass _    []           = []
  propagatePass prev (now : rest) = let
    newLC = case (prev >>= instantHH, prev >>= instantLC, instantLC now) of
      (_, Just (Just prevLC), Just Nothing) -> Just $ Just prevLC -- match previous LC color
      (Just _, _, Just Nothing)             -> Just $ Just D.Blue -- make LC blue because there's a hihat previously
      _                                     -> instantLC now
    newRD = case (newLC, instantRD now) of
      (Just (Just D.Blue), Just Nothing) -> Just $ Just D.Green -- green because now there's a blue LC
      _ -> case (prev >>= instantLC, prev >>= instantRD, instantRD now) of
        (_, Just (Just prevRD), Just Nothing) -> Just $ Just prevRD -- match previous RD color
        (Just (Just D.Blue), _, Just Nothing) -> Just $ Just D.Green -- green because there's a blue LC previously
        _                                     -> instantRD now
    newNow = now { instantLC = newLC, instantRD = newRD }
    in newNow : propagatePass (Just newNow) rest
  assignPropagate
    :: RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
    -> RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
  assignPropagate rtb = let
    times = RTB.getTimes rtb
    bodies = reverse $ propagatePass Nothing $ reverse $ propagatePass Nothing $ RTB.getBodies rtb
    in RTB.fromPairList $ zip times bodies

  -- finally, default to putting LC on yellow and RD on blue
  assignFinal
    :: RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
    -> RTB.T t (CymbalInstant D.ProColor D.ProColor)
  assignFinal = fmap $ \now -> now
    { instantLC = fromMaybe D.Yellow <$> instantLC now
    , instantRD = fromMaybe D.Blue   <$> instantRD now
    }

  assignFull
    :: RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
    -> RTB.T t (CymbalInstant D.ProColor D.ProColor)
  assignFull = assignFinal . assignPropagate . assignInstant

  emitCymbals now = catMaybes
    [ instantHH now
    , (\color -> Right $ D.Pro color D.Cymbal) <$> instantLC now
    , (\color -> Right $ D.Pro color D.Cymbal) <$> instantRD now
    , guard (instantRC now) >> Just (Right $ D.Pro D.Green D.Cymbal)
    ]

placeToms :: (NNC.C t) => RTB.T t (D.RealDrum, DrumVelocity) -> RTB.T t (FullDrumNote ()) -> RTB.T t (D.RealDrum, DrumVelocity)
placeToms cymbals notes = let
  basicGem = \case
    Left D.Rimshot -> D.Red
    Left _         -> D.Pro D.Yellow ()
    Right rb       -> void rb
  conflict x y = basicGem x == basicGem y
  tomsInstant stuff = let
    thisCymbals = lefts stuff
    thisNotes = rights stuff
    add x choices = when (any (\(y, _) -> x == y) thisNotes) $ modify $ \cur ->
      case filter (\y -> all (not . conflict y) $ thisCymbals <> cur) choices of
        []    -> cur -- no options!
        y : _ -> y : cur
    in flip execState [] $ do
      add Tom1 $ map (\c -> Right $ D.Pro c D.Tom) [D.Yellow, D.Blue, D.Green]
      add Tom2 $ map (\c -> Right $ D.Pro c D.Tom) [D.Blue, D.Yellow, D.Green]
      add Tom3 $ map (\c -> Right $ D.Pro c D.Tom) [D.Green, D.Blue, D.Yellow]
  -- TODO actually preserve the velocities
  in fmap (, VelocityNormal)
    $ RTB.flatten $ fmap tomsInstant $ RTB.collectCoincident $ RTB.merge
      (Left . fst <$> cymbals)
      (Right . (\fdn -> (fdn_gem fdn, fdn_type fdn)) <$> notes)

splitFlams :: U.TempoMap -> RTB.T U.Beats (FullDrumNote FlamStatus) -> RTB.T U.Beats (FullDrumNote ())
splitFlams tmap fdns = let
  defaultFlamOffset :: U.Seconds
  defaultFlamOffset = 0.03
  step1 = ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.collectCoincident fdns
  step2 = zip step1 $ map (Just . fst) (drop 1 step1) <> [Nothing]
  step3 = step2 >>= \((t, notes), nextTime) -> let
    flamTimePerfect, flamTimeAdjusted, flamTime :: U.Beats
    flamTimePerfect = U.unapplyTempoMap tmap $ U.applyTempoMap tmap t + defaultFlamOffset
    -- so we don't make too-high a subdivision in DTX files,
    -- round our desired flam time to the nearest 1/24 of a beat
    flamTimeAdjusted = fromInteger (round $ flamTimePerfect * 24) / 24
    flamTime = case nextTime of
      Nothing   -> flamTimeAdjusted
      -- if next note is close enough, flam should be placed 1/3 between this and next
      Just next -> min flamTimeAdjusted $ t + (next - t) / 3
    mainNotes = map void notes
    flamNotes = map (flipFoot . void) $ filter (\n -> fdn_extra n == Flam) notes
    -- for a kick flam, the second note should be on opposite foot from the first
    flipFoot fdn = case fdn_gem fdn of
      Kick -> case fdn_limb fdn of
        Just D.LH -> fdn { fdn_limb = Just D.RH }
        Just D.RH -> fdn { fdn_limb = Just D.LH }
        Nothing   -> fdn
      _    -> fdn
    in (t, mainNotes) : [ (flamTime, flamNotes) | not $ null flamNotes ]
  in RTB.flatten $ RTB.fromAbsoluteEventList $ ATB.fromPairList step3

fullDrumsToPS :: U.TempoMap -> RTB.T U.Beats (FullDrumNote FlamStatus) -> RTB.T U.Beats (D.RealDrum, DrumVelocity)
fullDrumsToPS tmap input = let
  notes = splitFlams tmap input
  cymbals = placeCymbals notes
  kicksSnares = flip RTB.mapMaybe notes $ \fdn -> case fdn_gem fdn of
    Kick  -> Just (Right D.Kick, fdn_velocity fdn)
    Snare -> Just (Right D.Red , fdn_velocity fdn)
    _     -> Nothing
  toms = placeToms cymbals notes
  in RTB.merge cymbals $ RTB.merge kicksSnares toms

fullDrumsToRB :: U.TempoMap -> RTB.T U.Beats (FullDrumNote FlamStatus) -> RTB.T U.Beats (D.Gem D.ProType, DrumVelocity)
fullDrumsToRB tmap gems = let
  real = fullDrumsToPS tmap $ RTB.filter (\note -> fdn_gem note /= HihatFoot) gems
  in flip fmap real $ \(gem, vel) -> let
    gem' = case gem of
      Left ps -> case ps of
        D.Rimshot  -> D.Red
        D.HHOpen   -> D.Pro D.Yellow D.Cymbal
        D.HHSizzle -> D.Pro D.Yellow D.Cymbal
        D.HHPedal  -> D.Pro D.Yellow D.Cymbal
      Right rb -> rb
    in (gem', vel)

convertFullDrums :: Bool -> U.TempoMap -> FullDrumTrack U.Beats -> D.DrumTrack U.Beats
convertFullDrums isPS tmap trk = let
  expert = getDifficulty (Just Expert) trk
  encoded = D.encodePSReal (1/32) Expert $ if isPS
    then fullDrumsToPS tmap expert
    else fmap (first Right) $ fullDrumsToRB tmap expert
  in encoded
    { D.drumOverdrive  = fdOverdrive trk
    , D.drumActivation = fdActivation trk
    , D.drumSolo       = fdSolo trk
    , D.drumKick2x     = fdKick2 trk
    }

autoFDAnimation :: (NNC.C t) => t -> RTB.T t (FullDrumNote FlamStatus) -> RTB.T t D.Animation
autoFDAnimation closeTime fdns = let
  hands = D.autoDrumHands closeTime $ RTB.flatten $ flip fmap fdns $ \fdn -> do
    pad <- case fdn_gem fdn of
      Kick      -> []
      Snare     -> pure D.AnimSnare
      Hihat     -> pure D.AnimHihat
      HihatFoot -> []
      CrashL    -> pure D.AnimCrash1
      Tom1      -> pure D.AnimTom1
      Tom2      -> pure D.AnimTom2
      Tom3      -> pure D.AnimFloorTom
      CrashR    -> pure D.AnimCrash2
      Ride      -> pure D.AnimRide
    _ <- case fdn_extra fdn of
      Flam    -> [(), ()]
      NotFlam -> [()]
    return D.AnimInput
      { aiPad      = pad
      , aiVelocity = fdn_velocity fdn
      }
  kicks = flip RTB.mapMaybe fdns $ \fdn -> case fdn_gem fdn of
    Kick -> Just D.KickRF
    _    -> Nothing
  -- TODO hihat pedal
  in RTB.merge kicks hands

-- TODO hihat pedal
animationToFD :: (NNC.C t) => RTB.T t D.Animation -> RTB.T t (FullDrumNote (), D.Hand)
animationToFD anims = RTB.flatten $ flip fmap anims $ \case
  D.Tom1       hand -> pure (FullDrumNote Tom1   GemNormal      VelocityNormal Nothing (), hand)
  D.Tom2       hand -> pure (FullDrumNote Tom2   GemNormal      VelocityNormal Nothing (), hand)
  D.FloorTom   hand -> pure (FullDrumNote Tom3   GemNormal      VelocityNormal Nothing (), hand)
  D.Hihat      hand -> pure (FullDrumNote Hihat  GemNormal      VelocityNormal Nothing (), hand)
  D.Snare  hit hand -> pure (FullDrumNote Snare  GemNormal      (fromHit hit)  Nothing (), hand)
  D.Ride       hand -> pure (FullDrumNote Ride   GemNormal      VelocityNormal Nothing (), hand)
  D.Crash1 hit hand -> pure (FullDrumNote CrashL GemNormal      (fromHit hit)  Nothing (), hand)
  D.Crash2 hit hand -> pure (FullDrumNote CrashR GemNormal      (fromHit hit)  Nothing (), hand)
  D.KickRF          -> pure (FullDrumNote Kick   GemNormal      VelocityNormal Nothing (), D.RH)
  D.Crash1RHChokeLH -> pure (FullDrumNote CrashL GemCymbalChoke VelocityNormal Nothing (), D.RH)
  D.Crash2RHChokeLH -> pure (FullDrumNote CrashR GemCymbalChoke VelocityNormal Nothing (), D.RH)
  D.PercussionRH    -> pure (FullDrumNote Tom3   GemNormal      VelocityNormal Nothing (), D.RH)
  _                 -> []
  where fromHit D.SoftHit = VelocityGhost
        fromHit D.HardHit = VelocityNormal
