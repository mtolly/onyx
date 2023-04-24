{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module Onyx.MIDI.Track.Drums.True where

import           Control.Monad                    (guard, void, when)
import           Control.Monad.Codec
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer       (execWriter, tell)
import           Data.Bifunctor                   (first)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   listToMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DeriveHelpers
import           Onyx.Guitar                      (applyStatus)
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.Drums            (DrumVelocity (..))
import qualified Onyx.MIDI.Track.Drums            as D
import qualified Sound.MIDI.Util                  as U

import qualified Data.ByteString                  as B
import qualified Sound.MIDI.File                  as F

data TrueDrumTrack t = TrueDrumTrack
  { tdDifficulties :: Map.Map Difficulty (TrueDrumDifficulty t)
  , tdLanes        :: RTB.T t (Edge () TrueGem)
  , tdOverdrive    :: RTB.T t Bool
  , tdActivation   :: RTB.T t Bool
  , tdSolo         :: RTB.T t Bool
  -- rest is not standardized
  , tdSticking     :: RTB.T t D.Hand -- this should probably be per-difficulty?
  -- overrides for DTX sounds
  , tdChipOverride :: RTB.T t (LongNote () T.Text)
  -- overrides for kick assignment in DTX
  , tdFooting      :: RTB.T t D.Hand
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (TrueDrumTrack t)

instance TraverseTrack TrueDrumTrack where
  traverseTrack fn (TrueDrumTrack a b c d e f g h) = TrueDrumTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h

data TrueDrumDifficulty t = TrueDrumDifficulty
  { tdGems        :: RTB.T t (TrueGem, TrueBasic, DrumVelocity)
  , tdKick2       :: RTB.T t () -- TODO (gem type ?) and velocity
  , tdFlam        :: RTB.T t ()
  , tdHihatOpen   :: RTB.T t Bool
  , tdHihatClosed :: RTB.T t Bool
  , tdDisco       :: RTB.T t Bool
  -- rest is not standardized
  , tdRim         :: RTB.T t Bool
  , tdChoke       :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (TrueDrumDifficulty t)

instance TraverseTrack TrueDrumDifficulty where
  traverseTrack fn (TrueDrumDifficulty a b c d e f g h) = TrueDrumDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h

data TrueGem
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

data TrueGemType
  = GemNormal
  | GemHihatOpen
  | GemHihatClosed
  | GemCymbalChoke
  | GemRim
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data TrueBasic
  = TBDefault
  | TBRed
  | TBYellow
  | TBBlue
  | TBGreen
  deriving (Eq, Ord, Show, Enum, Bounded)

nullTrueDrums :: TrueDrumTrack t -> Bool
nullTrueDrums = all (RTB.null . tdGems) . toList . tdDifficulties

instance ParseTrack TrueDrumTrack where
  parseTrack = do
    tdSolo <- tdSolo =. edges 103
    tdOverdrive <- tdOverdrive =. edges 104
    tdActivation <- tdActivation =. edges 106
    tdLanes <- (tdLanes =.) $ translateEdges
      $ condenseMap $ eachKey each $ \drum -> case drum of
        Kick      -> edges $ 110 + 0
        Snare     -> edges $ 110 + 1
        Hihat     -> edges $ 110 + 2
        HihatFoot -> return mempty
        CrashL    -> edges $ 110 + 3
        Tom1      -> edges $ 110 + 4
        Tom2      -> edges $ 110 + 5
        Tom3      -> edges $ 110 + 6
        Ride      -> edges $ 110 + 7
        CrashR    -> edges $ 110 + 8
    tdSticking <- (tdSticking =.) $ condenseMap_ $ eachKey each $ blip . \case
      D.RH -> 100
      D.LH -> 99
    tdFooting <- (tdFooting =.) $ condenseMap_ $ eachKey each $ blip . \case
      D.RH -> 97
      D.LH -> 96
    tdDifficulties <- (tdDifficulties =.) $ eachKey each $ \diff -> fatBlips (1/8) $ do
      let base = case diff of
            Easy   -> 0
            Medium -> 24
            Hard   -> 48
            Expert -> 72
          decodeCV (drum, (c, v)) = let
            basic = case c of
              1 -> TBRed
              2 -> TBYellow
              3 -> TBBlue
              4 -> TBGreen
              _ -> TBDefault
            vel = case v of
              1   -> VelocityGhost
              127 -> VelocityAccent
              _   -> VelocityNormal
            in (drum, basic, vel)
          encodeCV (drum, basic, vel) = let
            c = case basic of
              TBDefault -> 0
              TBRed     -> 1
              TBYellow  -> 2
              TBBlue    -> 3
              TBGreen   -> 4
            v = case vel of
              VelocityGhost  -> 1
              VelocityNormal -> 96
              VelocityAccent -> 127
            in (drum, (c, v))
      tdGems <- tdGems =. do
        dimap (fmap encodeCV) (fmap decodeCV) $ condenseMap $ eachKey each $ \drum -> do
          blipCV $ base + case drum of
            HihatFoot -> 0
            Kick      -> 2
            Snare     -> 3
            Hihat     -> 4
            CrashL    -> 5
            Tom1      -> 6
            Tom2      -> 7
            Tom3      -> 8
            Ride      -> 9
            CrashR    -> 10
      tdKick2       <- tdKick2       =. blip  (base + 1)
      tdFlam        <- tdFlam        =. blip  (base + 15)
      tdHihatClosed <- tdHihatClosed =. edges (base + 16)
      tdHihatOpen   <- tdHihatOpen   =. edges (base + 17)
      tdDisco       <- tdDisco       =. edges (base + 18)
      tdRim         <- tdRim         =. edges (base + 20)
      tdChoke       <- tdChoke       =. edges (base + 21)
      return TrueDrumDifficulty{..}
    tdChipOverride <- tdChipOverride =. dimap (fmap ChipOverrideEvent) (fmap fromChipOverrideEvent) command
    return TrueDrumTrack{..}

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

trueDrumNoteNames :: [(Int, T.Text)]
trueDrumNoteNames = execWriter $ do
  o 118 "Roll/Swell Crash 2"
  o 117 "Roll/Swell Ride"
  o 116 "Roll/Swell Tom 3"
  o 115 "Roll/Swell Tom 2"
  o 114 "Roll/Swell Tom 1"
  o 113 "Roll/Swell Crash 1"
  o 112 "Roll/Swell Hihat"
  o 111 "Roll/Swell Snare"
  o 110 "Roll/Swell Kick"
  x 107
  o 106 "Activation/BRE"
  o 104 "Overdrive"
  o 103 "Solo"
  x 95
  o 90 "X Discobeat"
  o 89 "X Hihat Open Phrase"
  o 88 "X Hihat Closed Phrase"
  o 87 "X Flam"
  x 83
  o 82 "X Crash 2"
  o 81 "X Ride"
  o 80 "X Tom 3"
  o 79 "X Tom 2"
  o 78 "X Tom 1"
  o 77 "X Crash 1"
  o 76 "X Hihat"
  o 75 "X Snare"
  o 74 "X Kick"
  o 73 "X Kick 2x"
  o 72 "X Hihat Stomp"
  x 71
  o 66 "H Discobeat"
  o 65 "H Hihat Open Phrase"
  o 64 "H Hihat Closed Phrase"
  o 63 "H Flam"
  x 59
  o 58 "H Crash 2"
  o 57 "H Ride"
  o 56 "H Tom 3"
  o 55 "H Tom 2"
  o 54 "H Tom 1"
  o 53 "H Crash 1"
  o 52 "H Hihat"
  o 51 "H Snare"
  o 50 "H Kick"
  o 49 "H Kick 2x"
  o 48 "H Hihat Stomp"
  x 47
  o 42 "M Discobeat"
  o 41 "M Hihat Open Phrase"
  o 40 "M Hihat Closed Phrase"
  o 39 "M Flam"
  x 35
  o 34 "M Crash 2"
  o 33 "M Ride"
  o 32 "M Tom 3"
  o 31 "M Tom 2"
  o 30 "M Tom 1"
  o 29 "M Crash 1"
  o 28 "M Hihat"
  o 27 "M Snare"
  o 26 "M Kick"
  o 25 "M Kick 2x"
  o 24 "M Hihat Stomp"
  x 23
  o 18 "E Discobeat"
  o 17 "E Hihat Open Phrase"
  o 16 "E Hihat Closed Phrase"
  o 15 "E Flam"
  x 11
  o 10 "E Crash 2"
  o 9 "E Ride"
  o 8 "E Tom 3"
  o 7 "E Tom 2"
  o 6 "E Tom 1"
  o 5 "E Crash 1"
  o 4 "E Hihat"
  o 3 "E Snare"
  o 2 "E Kick"
  o 1 "E Kick 2x"
  o 0 "E Hihat Stomp"
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

data TrueDrumNote a = TrueDrumNote
  { tdn_gem      :: TrueGem
  , tdn_type     :: TrueGemType
  , tdn_velocity :: DrumVelocity
  , tdn_limb     :: Maybe D.Hand -- hand, or foot
  , tdn_extra    :: a
  } deriving (Eq, Ord, Show, Functor)

getDifficulty :: (NNC.C t) => Maybe Difficulty -> TrueDrumTrack t -> RTB.T t (TrueDrumNote FlamStatus)
getDifficulty diff trk = let
  base = fromMaybe mempty $ Map.lookup (fromMaybe Expert diff) $ tdDifficulties trk
  events = foldr RTB.merge RTB.empty
    $ fmap MergedNote (tdGems base)
    : fmap (const MergedFlam) (tdFlam base)
    : fmap MergedSticking (tdSticking trk)
    : fmap MergedFooting (tdFooting trk)
    : case diff of
      Nothing -> [fmap MergedNote2 $ tdKick2 base]
      _       -> []
  processSlice (types, evts) = let
    notKick = [ trio | MergedNote trio@(gem, _, _) <- evts, gem /= Kick ]
    kick = [ trio | MergedNote trio@(Kick, _, _) <- evts ]
    flam = any (\case MergedFlam -> True; _ -> False) evts
    kick2 = any (\case MergedNote2 () -> True; _ -> False) evts
    foot = listToMaybe [ f | MergedFooting f <- evts ]
    hihatType  = fromMaybe GemNormal $ listToMaybe $ filter (`elem` [GemHihatOpen, GemHihatClosed]) types
    drumType   = fromMaybe GemNormal $ listToMaybe $ filter (== GemRim) types
    cymbalType = fromMaybe GemNormal $ listToMaybe $ filter (== GemCymbalChoke) types
    outputNotKick = flip fmap notKick $ \(gem, _, vel) -> TrueDrumNote
      { tdn_gem      = gem
      , tdn_type     = case gem of
        Hihat  -> hihatType
        Snare  -> drumType
        Tom1   -> drumType
        Tom2   -> drumType
        Tom3   -> drumType
        CrashL -> cymbalType
        CrashR -> cymbalType
        Ride   -> cymbalType
        _      -> GemNormal
      , tdn_velocity = vel
      , tdn_limb     = Nothing -- TODO
      , tdn_extra    = if flam && gem /= HihatFoot then Flam else NotFlam
      }
    outputKick = case (kick, kick2) of
      ([], True) -> return TrueDrumNote
        { tdn_gem      = Kick
        , tdn_type     = GemNormal
        , tdn_velocity = VelocityNormal
        , tdn_limb     = Just $ fromMaybe D.LH foot
        , tdn_extra    = NotFlam
        }
      _          -> flip fmap kick $ \(gem, _, vel) -> TrueDrumNote
        { tdn_gem      = gem
        , tdn_type     = GemNormal
        , tdn_velocity = vel
        , tdn_limb     = Just $ fromMaybe D.RH foot
        , tdn_extra    = if kick2 then Flam else NotFlam
        }
    in outputNotKick <> outputKick
  statuses = foldr RTB.merge RTB.empty
    [ (GemHihatOpen   ,) <$> tdHihatOpen   base
    , (GemHihatClosed ,) <$> tdHihatClosed base
    , (GemRim         ,) <$> tdRim         base
    , (GemCymbalChoke ,) <$> tdChoke       base
    ]
  in RTB.flatten
    $ fmap processSlice
    $ applyStatus statuses
    $ RTB.collectCoincident events

-- true to PS/RB conversion

data CymbalInstant lc rd = CymbalInstant
  -- comments here reflect placeCymbalsFancy
  { instantHH :: Maybe D.RealDrum -- always yellow (with ps real mods)
  , instantLC :: Maybe lc -- default yellow, can be pushed to blue
  , instantRD :: Maybe rd -- default blue, can be pushed to green
  , instantRC :: Bool -- always green
  } deriving (Show)

-- In cases of an open hihat note followed semi-closely by a closed one,
-- add a foot hihat note. Used for DTX export where it is the norm for readability
addExplicitStomps :: (NNC.C t) => t -> RTB.T t (TrueDrumNote FlamStatus) -> RTB.T t (TrueDrumNote FlamStatus)
addExplicitStomps threshold trk = let
  hihatNotes = flip RTB.mapMaybe (RTB.collectCoincident trk) $ \instant -> let
    open   = any (\tdn -> tdn_gem tdn == Hihat && tdn_type tdn == GemHihatOpen  ) instant
    closed = any (\tdn -> tdn_gem tdn == Hihat && tdn_type tdn == GemHihatClosed) instant
    pedal  = any (\tdn -> tdn_gem tdn == HihatFoot                              ) instant
    in guard (open || closed || pedal) >> Just (open, closed, pedal)
  foot = TrueDrumNote
    { tdn_gem      = HihatFoot
    , tdn_type     = GemNormal
    , tdn_velocity = VelocityNormal
    , tdn_limb     = Nothing
    , tdn_extra    = NotFlam
    }
  go lastOpen = \case
    RNil -> RNil
    Wait dt (open, closed, pedal) rest ->
      if lastOpen && closed && not pedal && dt <= threshold
        then Wait dt foot $ go open rest
        else RTB.delay dt $ go open rest
  in RTB.merge trk $ go False hihatNotes

-- Simple version, hihat = Y, left cymbal = B, right cymbal / ride = G
placeCymbals :: (NNC.C t) => RTB.T t (TrueDrumNote ()) -> RTB.T t (D.RealDrum, DrumVelocity)
placeCymbals
  -- TODO actually preserve the velocities
  = fmap (, VelocityNormal)
  . RTB.flatten . fmap (emitCymbals . makeInstant) . RTB.collectCoincident
  . fmap (\tdn -> (tdn_gem tdn, tdn_type tdn))
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
placeCymbalsFancy :: (NNC.C t) => RTB.T t (TrueGem, TrueGemType, DrumVelocity) -> RTB.T t (D.RealDrum, DrumVelocity)
placeCymbalsFancy
  -- TODO actually preserve the velocities
  = fmap (, VelocityNormal)
  . RTB.flatten . fmap emitCymbals . assignTrue . RTB.filter hasCymbals . fmap makeInstant . RTB.collectCoincident
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

  assignTrue
    :: RTB.T t (CymbalInstant (Maybe D.ProColor) (Maybe D.ProColor))
    -> RTB.T t (CymbalInstant D.ProColor D.ProColor)
  assignTrue = assignFinal . assignPropagate . assignInstant

  emitCymbals now = catMaybes
    [ instantHH now
    , (\color -> Right $ D.Pro color D.Cymbal) <$> instantLC now
    , (\color -> Right $ D.Pro color D.Cymbal) <$> instantRD now
    , guard (instantRC now) >> Just (Right $ D.Pro D.Green D.Cymbal)
    ]

placeToms :: (NNC.C t) => RTB.T t (D.RealDrum, DrumVelocity) -> RTB.T t (TrueDrumNote ()) -> RTB.T t (D.RealDrum, DrumVelocity)
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
      (Right . (\tdn -> (tdn_gem tdn, tdn_type tdn)) <$> notes)

splitFlams :: U.TempoMap -> RTB.T U.Beats (TrueDrumNote FlamStatus) -> RTB.T U.Beats (TrueDrumNote ())
splitFlams tmap tdns = let
  defaultFlamOffset :: U.Seconds
  defaultFlamOffset = 0.03
  step1 = ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.collectCoincident tdns
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
    flamNotes = map (flipFoot . void) $ filter (\n -> tdn_extra n == Flam) notes
    -- for a kick flam, the second note should be on opposite foot from the first
    flipFoot tdn = case tdn_gem tdn of
      Kick -> case tdn_limb tdn of
        Just D.LH -> tdn { tdn_limb = Just D.RH }
        Just D.RH -> tdn { tdn_limb = Just D.LH }
        Nothing   -> tdn
      _    -> tdn
    in (t, mainNotes) : [ (flamTime, flamNotes) | not $ null flamNotes ]
  in RTB.flatten $ RTB.fromAbsoluteEventList $ ATB.fromPairList step3

trueDrumsToPS :: U.TempoMap -> RTB.T U.Beats (TrueDrumNote FlamStatus) -> RTB.T U.Beats (D.RealDrum, DrumVelocity)
trueDrumsToPS tmap input = let
  notes = splitFlams tmap input
  cymbals = placeCymbals notes
  kicksSnares = flip RTB.mapMaybe notes $ \tdn -> case tdn_gem tdn of
    Kick  -> Just (Right D.Kick, tdn_velocity tdn)
    Snare -> Just (Right D.Red , tdn_velocity tdn)
    _     -> Nothing
  toms = placeToms cymbals notes
  in RTB.merge cymbals $ RTB.merge kicksSnares toms

trueDrumsToRB :: U.TempoMap -> RTB.T U.Beats (TrueDrumNote FlamStatus) -> RTB.T U.Beats (D.Gem D.ProType, DrumVelocity)
trueDrumsToRB tmap gems = let
  real = trueDrumsToPS tmap $ RTB.filter (\note -> tdn_gem note /= HihatFoot) gems
  in flip fmap real $ \(gem, vel) -> let
    gem' = case gem of
      Left ps -> case ps of
        D.Rimshot  -> D.Red
        D.HHOpen   -> D.Pro D.Yellow D.Cymbal
        D.HHSizzle -> D.Pro D.Yellow D.Cymbal
        D.HHPedal  -> D.Pro D.Yellow D.Cymbal
      Right rb -> rb
    in (gem', vel)

convertTrueDrums :: Bool -> U.TempoMap -> TrueDrumTrack U.Beats -> D.DrumTrack U.Beats
convertTrueDrums isPS tmap trk = let
  expert = getDifficulty (Just Expert) trk
  encoded = D.encodePSReal (1/32) Expert $ if isPS
    then trueDrumsToPS tmap expert
    else fmap (first Right) $ trueDrumsToRB tmap expert
  in encoded
    { D.drumOverdrive  = tdOverdrive trk
    , D.drumActivation = tdActivation trk
    , D.drumSolo       = tdSolo trk
    , D.drumKick2x     = maybe RTB.empty tdKick2 $ Map.lookup Expert $ tdDifficulties trk
    }

trueDrumsToAnimation :: (NNC.C t) => t -> RTB.T t (TrueDrumNote FlamStatus) -> RTB.T t D.Animation
trueDrumsToAnimation closeTime tdns = let
  hands = D.autoDrumHands closeTime $ RTB.flatten $ flip fmap tdns $ \tdn -> do
    pad <- case tdn_gem tdn of
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
    _ <- case tdn_extra tdn of
      Flam    -> [(), ()]
      NotFlam -> [()]
    return D.AnimInput
      { aiPad      = pad
      , aiVelocity = tdn_velocity tdn
      }
  kicks = flip RTB.mapMaybe tdns $ \tdn -> case tdn_gem tdn of
    Kick -> Just D.KickRF
    _    -> Nothing
  -- TODO hihat pedal
  in RTB.merge kicks hands

-- TODO hihat pedal
animationToTrueDrums :: (NNC.C t) => RTB.T t D.Animation -> RTB.T t (TrueDrumNote (), D.Hand)
animationToTrueDrums anims = RTB.flatten $ flip fmap anims $ \case
  -- TODO need to handle two hits on same drum and turn into flam
  D.Tom1       hand -> pure (TrueDrumNote Tom1   GemNormal      VelocityNormal Nothing (), hand)
  D.Tom2       hand -> pure (TrueDrumNote Tom2   GemNormal      VelocityNormal Nothing (), hand)
  D.FloorTom   hand -> pure (TrueDrumNote Tom3   GemNormal      VelocityNormal Nothing (), hand)
  D.Hihat      hand -> pure (TrueDrumNote Hihat  GemNormal      VelocityNormal Nothing (), hand)
  D.Snare  hit hand -> pure (TrueDrumNote Snare  GemNormal      (fromHit hit)  Nothing (), hand)
  D.Ride       hand -> pure (TrueDrumNote Ride   GemNormal      VelocityNormal Nothing (), hand)
  D.Crash1 hit hand -> pure (TrueDrumNote CrashL GemNormal      (fromHit hit)  Nothing (), hand)
  D.Crash2 hit hand -> pure (TrueDrumNote CrashR GemNormal      (fromHit hit)  Nothing (), hand)
  D.KickRF          -> pure (TrueDrumNote Kick   GemNormal      VelocityNormal Nothing (), D.RH)
  D.Crash1RHChokeLH -> pure (TrueDrumNote CrashL GemCymbalChoke VelocityNormal Nothing (), D.RH)
  D.Crash2RHChokeLH -> pure (TrueDrumNote CrashR GemCymbalChoke VelocityNormal Nothing (), D.RH)
  -- TODO this should probably go on Tom1/Tom2 as needed, I think it's more behind those in the RB anim kit
  D.PercussionRH    -> pure (TrueDrumNote Tom3   GemNormal      VelocityNormal Nothing (), D.RH)
  _                 -> []
  where fromHit D.SoftHit = VelocityGhost
        fromHit D.HardHit = VelocityNormal

swapActivation :: F.T B.ByteString -> Maybe (F.T B.ByteString)
swapActivation (F.Cons typ dvn trks) = let
  results = flip map trks $ \trk -> case U.trackName trk of
    Just "PART TRUE_DRUMS" -> (True,) $ RTB.flatten $ flip fmap trk $ \e ->
      case isNoteEdgeCPV e of
        Nothing -> [e]
        Just (c, p, mv) -> let
          newPitches
            | p == 105 = [106]
            | otherwise = [p]
          in map (\p' -> makeEdgeCPV c p' mv) newPitches
    _ -> (False, trk)
  in if any fst results
    then Just $ F.Cons typ dvn $ map snd results
    else Nothing

makeTrueDifficulty :: RTB.T U.Beats (TrueGem, TrueGemType, DrumVelocity) -> TrueDrumDifficulty U.Beats
makeTrueDifficulty gems = let
  types = U.trackJoin $ go $ RTB.collectCoincident gems
  go = \case
    Wait dt1 gems1 rest -> let
      mods = nubOrd $ filter (/= GemNormal) [ typ | (_, typ, _) <- gems1 ]
      len = case rest of
        RNil         -> 1/8
        Wait dt2 _ _ -> min (1/8) dt2
      modEdges = RTB.flatten $ Wait 0 (map (, True) mods) $ Wait len (map (, False) mods) RNil
      in Wait dt1 modEdges $ go rest
    RNil -> RNil
  getModifier m = RTB.mapMaybe
    (\(m', b) -> guard (m == m') >> Just b)
    types
  in TrueDrumDifficulty
    { tdGems        = (\(gem, _, vel) -> (gem, TBDefault, vel)) <$> gems
    , tdKick2       = RTB.empty
    , tdFlam        = RTB.empty
    , tdHihatOpen   = getModifier GemHihatOpen
    , tdHihatClosed = getModifier GemHihatClosed
    , tdDisco       = RTB.empty
    , tdRim         = getModifier GemRim
    , tdChoke       = getModifier GemCymbalChoke
    }
