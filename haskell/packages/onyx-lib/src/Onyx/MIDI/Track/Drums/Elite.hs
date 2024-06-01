{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
module Onyx.MIDI.Track.Drums.Elite where

import           Control.Monad                    (guard, void)
import           Control.Monad.Codec
import           Control.Monad.Trans.Writer       (execWriter, tell)
import           Data.Bifunctor                   (first)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List.Extra                  (nubOrd, sortOn)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Set                         as Set
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

data EliteDrumTrack t = EliteDrumTrack
  { tdDifficulties :: Map.Map Difficulty (EliteDrumDifficulty t)
  , tdLanes        :: RTB.T t (Edge () (EliteGem ()))
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
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EliteDrumTrack t)

instance TraverseTrack EliteDrumTrack where
  traverseTrack fn (EliteDrumTrack a b c d e f g h) = EliteDrumTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h

data EliteDrumDifficulty t = EliteDrumDifficulty
  { tdGems        :: RTB.T t (EliteGem D.Hand, EliteBasic, DrumVelocity)
  , tdFlam        :: RTB.T t ()
  , tdHihatOpen   :: RTB.T t Bool
  , tdHihatClosed :: RTB.T t Bool
  , tdDisco       :: RTB.T t Bool
  -- rest is not standardized
  , tdRim         :: RTB.T t Bool
  , tdChoke       :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EliteDrumDifficulty t)

instance TraverseTrack EliteDrumDifficulty where
  traverseTrack fn (EliteDrumDifficulty a b c d e f g) = EliteDrumDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g

data EliteGem a
  = Kick a
  | Snare
  | Hihat
  | HihatFoot
  | CrashL
  | Tom1
  | Tom2
  | Tom3
  | CrashR
  | Ride
  deriving (Eq, Ord, Show, Read, Generic, Functor)
  deriving (Enum, Bounded) via GenericFullEnum (EliteGem a)

data EliteGemType
  = GemNormal
  | GemHihatOpen
  | GemHihatClosed
  | GemCymbalChoke
  | GemRim
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data EliteBasic
  = TBDefault
  | TBRed
  | TBYellow
  | TBBlue
  | TBGreen
  deriving (Eq, Ord, Show, Enum, Bounded)

nullEliteDrums :: EliteDrumTrack t -> Bool
nullEliteDrums = all (RTB.null . tdGems) . toList . tdDifficulties

instance ParseTrack EliteDrumTrack where
  parseTrack = do
    tdSolo <- tdSolo =. edges 103
    tdOverdrive <- tdOverdrive =. edges 104
    tdActivation <- tdActivation =. edges 106
    tdLanes <- (tdLanes =.) $ translateEdges
      $ condenseMap $ eachKey each $ \drum -> case drum of
        Kick ()   -> edges $ 110 + 0
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
              10 -> TBRed
              11 -> TBYellow
              12 -> TBBlue
              13 -> TBGreen
              _  -> TBDefault
            vel = case v of
              1   -> VelocityGhost
              127 -> VelocityAccent
              _   -> VelocityNormal
            in (drum, basic, vel)
          encodeCV (drum, basic, vel) = let
            c = case basic of
              TBDefault -> 0
              TBRed     -> 10
              TBYellow  -> 11
              TBBlue    -> 12
              TBGreen   -> 13
            v = case vel of
              VelocityGhost  -> 1
              VelocityNormal -> 96
              VelocityAccent -> 127
            in (drum, (c, v))
      tdGems <- tdGems =. do
        dimap (fmap encodeCV) (fmap decodeCV) $ condenseMap $ eachKey each $ \drum -> do
          blipCV $ base + case drum of
            HihatFoot -> 0
            Kick D.LH -> 1
            Kick D.RH -> 2
            Snare     -> 3
            Hihat     -> 4
            CrashL    -> 5
            Tom1      -> 6
            Tom2      -> 7
            Tom3      -> 8
            Ride      -> 9
            CrashR    -> 10
      tdFlam        <- tdFlam        =. blip  (base + 15)
      tdHihatClosed <- tdHihatClosed =. edges (base + 16)
      tdHihatOpen   <- tdHihatOpen   =. edges (base + 17)
      tdDisco       <- tdDisco       =. edges (base + 18)
      tdRim         <- tdRim         =. edges (base + 20)
      tdChoke       <- tdChoke       =. edges (base + 21)
      return EliteDrumDifficulty{..}
    tdChipOverride <- tdChipOverride =. dimap (fmap ChipOverrideEvent) (fmap fromChipOverrideEvent) command
    return EliteDrumTrack{..}

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

eliteDrumNoteNames :: [(Int, T.Text)]
eliteDrumNoteNames = execWriter $ do
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
  o 88 "X Hihat Indifferent"
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
  o 64 "H Hihat Indifferent"
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
  o 40 "M Hihat Indifferent"
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
  o 16 "E Hihat Indifferent"
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

data MergedEvent n
  = MergedNote n
  | MergedSticking D.Hand
  | MergedFooting D.Hand
  | MergedFlam
  deriving (Eq, Ord)

data FlamStatus = NotFlam | Flam
  deriving (Eq, Ord, Show)

data EliteDrumNote a = EliteDrumNote
  { tdn_gem      :: EliteGem ()
  , tdn_type     :: EliteGemType
  , tdn_velocity :: DrumVelocity
  , tdn_limb     :: Maybe D.Hand -- hand, or foot
  , tdn_basic    :: EliteBasic
  , tdn_extra    :: a
  } deriving (Eq, Ord, Show, Functor)

getDifficulty :: (NNC.C t) => Maybe Difficulty -> EliteDrumTrack t -> RTB.T t (EliteDrumNote FlamStatus)
getDifficulty diff trk = let
  base = fromMaybe mempty $ Map.lookup (fromMaybe Expert diff) $ tdDifficulties trk
  events = foldr RTB.merge RTB.empty
    $ fmap MergedNote (adjustKicks $ tdGems base)
    : fmap (const MergedFlam) (tdFlam base)
    : fmap MergedSticking (tdSticking trk)
    : fmap MergedFooting (tdFooting trk)
    : []
  adjustKicks = case diff of
    Nothing -> id
    _       -> RTB.filter $ \case
      (Kick D.LH, _, _) -> False
      _                 -> True
  processSlice (types, evts) = let
    notKick = do
      MergedNote (void -> gem, basic, vel) <- evts
      guard $ gem /= Kick ()
      return (gem, basic, vel)
    kick1 = [ (basic, vel) | MergedNote (Kick D.RH, basic, vel) <- evts ]
    kick2 = [ (basic, vel) | MergedNote (Kick D.LH, basic, vel) <- evts ]
    flam = any (\case MergedFlam -> True; _ -> False) evts
    foot = listToMaybe [ f | MergedFooting f <- evts ]
    hihatType  = fromMaybe GemNormal $ listToMaybe $ filter (`elem` [GemHihatOpen, GemHihatClosed]) types
    drumType   = fromMaybe GemNormal $ listToMaybe $ filter (== GemRim) types
    cymbalType = fromMaybe GemNormal $ listToMaybe $ filter (== GemCymbalChoke) types
    outputNotKick = flip fmap notKick $ \(gem, basic, vel) -> EliteDrumNote
      { tdn_gem      = gem
      , tdn_type     = case gem of
        Hihat     -> hihatType
        HihatFoot -> case hihatType of
          GemHihatOpen -> GemHihatOpen
          _            -> GemHihatClosed
        Snare     -> drumType
        Tom1      -> drumType
        Tom2      -> drumType
        Tom3      -> drumType
        CrashL    -> cymbalType
        CrashR    -> cymbalType
        Ride      -> cymbalType
        Kick ()   -> GemNormal
      , tdn_velocity = vel
      , tdn_limb     = Nothing -- TODO
      , tdn_basic = basic
      , tdn_extra    = if flam && gem /= HihatFoot then Flam else NotFlam
      }
    outputKick = case (kick1, kick2) of
      ([], (basic, vel) : _) -> return EliteDrumNote
        { tdn_gem      = Kick ()
        , tdn_type     = GemNormal
        , tdn_velocity = vel
        , tdn_limb     = Just $ fromMaybe D.LH foot
        , tdn_basic    = basic
        , tdn_extra    = NotFlam
        }
      _          -> flip fmap kick1 $ \(basic, vel) -> EliteDrumNote
        { tdn_gem      = Kick ()
        , tdn_type     = GemNormal
        , tdn_velocity = vel
        , tdn_limb     = Just $ fromMaybe D.RH foot
        , tdn_basic    = basic
        , tdn_extra    = if null kick2 then NotFlam else Flam
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

-- elite to PS/RB conversion

data CymbalInstant lc rd = CymbalInstant
  -- comments here reflect placeCymbalsFancy
  { instantHH :: Maybe D.RealDrum -- always yellow (with ps real mods)
  , instantLC :: Maybe lc -- default yellow, can be pushed to blue
  , instantRD :: Maybe rd -- default blue, can be pushed to green
  , instantRC :: Bool -- always green
  } deriving (Show)

-- In cases of an open hihat note followed semi-closely by a closed one,
-- add a foot hihat note. Used for DTX export where it is the norm for readability
addExplicitStomps :: (NNC.C t) => t -> RTB.T t (EliteDrumNote FlamStatus) -> RTB.T t (EliteDrumNote FlamStatus)
addExplicitStomps threshold trk = let
  hihatNotes = flip RTB.mapMaybe (RTB.collectCoincident trk) $ \instant -> let
    open   = any (\tdn -> tdn_gem tdn == Hihat && tdn_type tdn == GemHihatOpen  ) instant
    closed = any (\tdn -> tdn_gem tdn == Hihat && tdn_type tdn == GemHihatClosed) instant
    pedal  = any (\tdn -> tdn_gem tdn == HihatFoot                              ) instant
    in guard (open || closed || pedal) >> Just (open, closed, pedal)
  foot = EliteDrumNote
    { tdn_gem      = HihatFoot
    , tdn_type     = GemNormal
    , tdn_velocity = VelocityNormal
    , tdn_limb     = Nothing
    , tdn_basic    = TBDefault
    , tdn_extra    = NotFlam
    }
  go lastOpen = \case
    RNil -> RNil
    Wait dt (open, closed, pedal) rest ->
      if lastOpen && closed && not pedal && dt <= threshold
        then Wait dt foot $ go open rest
        else RTB.delay dt $ go open rest
  in RTB.merge trk $ go False hihatNotes

-- TODO this does not handle kick flams right!
-- TODO this does not translate or warn about roll/swell lanes!
eliteDrumsToRBWithWarnings
  :: U.TempoMap
  -> RTB.T U.Beats (EliteDrumNote FlamStatus)
  -> (RTB.T U.Beats T.Text, RTB.T U.Beats (D.Gem D.ProType, DrumVelocity))
eliteDrumsToRBWithWarnings tmap input = let
  noFlams = splitFlams tmap $ snareFlamRY input
  allResults = RTB.flatten $ fmap eachInstant $ RTB.collectCoincident noFlams
  warnings = RTB.mapMaybe (either Just (const Nothing)) allResults
  results = RTB.mapMaybe (either (const Nothing) Just) allResults
  eachInstant :: [EliteDrumNote ()] -> [Either T.Text (D.Gem D.ProType, DrumVelocity)]
  eachInstant tdns = let
    handsBeforeMoving = flip mapMaybe tdns $ \tdn -> let
      defaultTo maybeYBG pro = Just $ let
        maybeYBG' = case tdn.tdn_basic of
          TBDefault -> maybeYBG
          TBRed     -> Nothing
          TBYellow  -> Just D.Yellow
          TBBlue    -> Just D.Blue
          TBGreen   -> Just D.Green
        gem = case maybeYBG' of
          Nothing       -> D.Red
          Just D.Yellow -> D.Pro D.Yellow pro
          Just D.Blue   -> D.Pro D.Blue   pro
          Just D.Green  -> D.Pro D.Green  pro
        in (gem, tdn.tdn_velocity)
      in case tdn.tdn_gem of
        Snare     -> defaultTo Nothing         D.Tom
        Hihat     -> defaultTo (Just D.Yellow) D.Cymbal
        CrashL    -> defaultTo (Just D.Yellow) D.Cymbal
        Tom1      -> defaultTo (Just D.Yellow) D.Tom
        Tom2      -> defaultTo (Just D.Blue  ) D.Tom
        Tom3      -> defaultTo (Just D.Green ) D.Tom
        Ride      -> defaultTo (Just D.Blue  ) D.Cymbal
        CrashR    -> defaultTo (Just D.Green ) D.Cymbal
        Kick ()   -> Nothing
        HihatFoot -> Nothing
    kicks = flip mapMaybe tdns $ \tdn -> case tdn.tdn_gem of
      Kick () -> Just (D.Kick, tdn.tdn_velocity)
      _       -> Nothing
    warnTooMany = guard (not $ null $ drop 2 handsBeforeMoving) >> ["More than 2 hand-played gems at this position!"]
    sortedHands = flip sortOn handsBeforeMoving $ \(gem, _vel) -> case gem of
      D.Red            -> 0 :: Int
      D.Pro _ D.Cymbal -> 1
      D.Pro _ D.Tom    -> 2
      _                -> 3
    hands = case sortedHands of
      firstHand : secondHand : _ -> Right firstHand : case (firstHand, secondHand) of
        ((D.Red, _), (D.Red, v2)) ->
          [ Right (D.Pro D.Yellow D.Tom, v2)
          ]
        ((D.Pro D.Yellow _, _), (D.Pro D.Yellow typ2, v2)) ->
          [ Right (D.Pro D.Blue typ2, v2)
          , Left "Conflict on yellow, moving one to blue"
          ]
        ((D.Pro D.Blue _, _), (D.Pro D.Blue typ2, v2)) ->
          [ Right (D.Pro D.Yellow typ2, v2)
          , Left "Conflict on blue, moving one to yellow"
          ]
        ((D.Pro D.Green _, _), (D.Pro D.Green typ2, v2)) ->
          [ Right (D.Pro D.Blue typ2, v2)
          , Left "Conflict on green, moving one to blue"
          ]
        _ -> [Right secondHand]
      _ -> map Right sortedHands
    in concat
      [ hands
      , map Right kicks
      , map Left warnTooMany
      ]
  flamWarnings = flip RTB.mapMaybe input $ \tdn -> case tdn.tdn_extra of
    Flam    -> Just "Flam note split into two notes"
    NotFlam -> Nothing
  leftCrashWarnings = let
    conflictWindow = 4 :: U.Beats
    absTimes = Set.fromList . ATB.getTimes . RTB.toAbsoluteEventList 0
    hihatPositions = absTimes $ RTB.filter (\tdn -> tdn.tdn_gem == Hihat) noFlams
    crashLPositions = absTimes $ RTB.filter (\tdn -> tdn.tdn_gem == CrashL) noFlams
    hihatCrashLConflicts = flip Set.filter crashLPositions $ \posn ->
      case Set.lookupLT (posn + conflictWindow) hihatPositions of
        Nothing -> False
        Just t  -> (posn NNC.-| conflictWindow) < t
    warningText = "Left crash near hihats, check color"
    in RTB.fromAbsoluteEventList $ ATB.fromPairList $ map (, warningText) $ Set.toList hihatCrashLConflicts
  in (RTB.merge warnings $ RTB.merge flamWarnings leftCrashWarnings, results)

snareFlamRY :: RTB.T U.Beats (EliteDrumNote FlamStatus) -> RTB.T U.Beats (EliteDrumNote FlamStatus)
snareFlamRY = let
  eachNote tdn = case (tdn.tdn_gem, tdn.tdn_extra) of
    (Snare, Flam) ->
      [ tdn { tdn_extra = NotFlam }
      , tdn { tdn_extra = NotFlam, tdn_gem = Tom1 }
      ]
    _ -> [tdn]
  in RTB.flatten . fmap eachNote

splitFlams :: U.TempoMap -> RTB.T U.Beats (EliteDrumNote FlamStatus) -> RTB.T U.Beats (EliteDrumNote ())
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
      Kick () -> case tdn_limb tdn of
        Just D.LH -> tdn { tdn_limb = Just D.RH }
        Just D.RH -> tdn { tdn_limb = Just D.LH }
        Nothing   -> tdn
      _    -> tdn
    in (t, mainNotes) : [ (flamTime, flamNotes) | not $ null flamNotes ]
  in RTB.flatten $ RTB.fromAbsoluteEventList $ ATB.fromPairList step3

convertEliteDrums :: U.TempoMap -> EliteDrumTrack U.Beats -> (RTB.T U.Beats T.Text, D.DrumTrack U.Beats)
convertEliteDrums tmap trk = let
  expert = getDifficulty (Just Expert) trk
  (warnings, ps) = case eliteDrumsToRBWithWarnings tmap expert of
    (ws, rb) -> (ws, fmap (first Right) rb)
  encoded = D.encodePSReal (1/8) Expert ps
  in (warnings, encoded
    { D.drumOverdrive  = tdOverdrive trk
    , D.drumActivation = tdActivation trk
    , D.drumSolo       = tdSolo trk
    , D.drumKick2x     = case Map.lookup Expert $ tdDifficulties trk of
      Nothing   -> RTB.empty
      Just diff -> flip RTB.mapMaybe (tdGems diff) $ \case
        (Kick D.LH, _, _) -> Just ()
        _                 -> Nothing
    })

eliteDrumsToAnimation :: (NNC.C t) => t -> RTB.T t (EliteDrumNote FlamStatus) -> RTB.T t D.Animation
eliteDrumsToAnimation closeTime tdns = let
  hands = D.autoDrumHands closeTime $ RTB.flatten $ flip fmap tdns $ \tdn -> do
    pad <- case tdn_gem tdn of
      Kick ()   -> []
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
    Kick () -> Just D.KickRF
    _       -> Nothing
  -- TODO hihat pedal
  in RTB.merge kicks hands

-- TODO hihat pedal
animationToEliteDrums :: (NNC.C t) => RTB.T t D.Animation -> RTB.T t (EliteDrumNote (), D.Hand)
animationToEliteDrums anims = RTB.flatten $ flip fmap anims $ \case
  -- TODO need to handle two hits on same drum and turn into flam
  D.Tom1       hand -> pure (note Tom1      GemNormal      VelocityNormal, hand)
  D.Tom2       hand -> pure (note Tom2      GemNormal      VelocityNormal, hand)
  D.FloorTom   hand -> pure (note Tom3      GemNormal      VelocityNormal, hand)
  D.Hihat      hand -> pure (note Hihat     GemNormal      VelocityNormal, hand)
  D.Snare  hit hand -> pure (note Snare     GemNormal      (fromHit hit) , hand)
  D.Ride       hand -> pure (note Ride      GemNormal      VelocityNormal, hand)
  D.Crash1 hit hand -> pure (note CrashL    GemNormal      (fromHit hit) , hand)
  D.Crash2 hit hand -> pure (note CrashR    GemNormal      (fromHit hit) , hand)
  D.KickRF          -> pure (note (Kick ()) GemNormal      VelocityNormal, D.RH)
  D.Crash1RHChokeLH -> pure (note CrashL    GemCymbalChoke VelocityNormal, D.RH)
  D.Crash2RHChokeLH -> pure (note CrashR    GemCymbalChoke VelocityNormal, D.RH)
  -- TODO this should probably go on Tom1/Tom2 as needed, I think it's more behind those in the RB anim kit
  D.PercussionRH    -> pure (note Tom3   GemNormal      VelocityNormal, D.RH)
  _                 -> []
  where fromHit D.SoftHit = VelocityGhost
        fromHit D.HardHit = VelocityNormal
        note gem typ vel = EliteDrumNote
          { tdn_gem = gem
          , tdn_type = typ
          , tdn_velocity = vel
          , tdn_limb = Nothing
          , tdn_basic = TBDefault
          , tdn_extra = ()
          }

makeEliteDifficulty' :: (EliteGem D.Hand -> EliteBasic) -> RTB.T U.Beats (EliteGem D.Hand, EliteGemType, DrumVelocity) -> EliteDrumDifficulty U.Beats
makeEliteDifficulty' basicMapping gems = let
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
  in EliteDrumDifficulty
    { tdGems        = (\(gem, _, vel) -> (gem, basicMapping gem, vel)) <$> gems
    , tdFlam        = RTB.empty
    , tdHihatOpen   = getModifier GemHihatOpen
    , tdHihatClosed = getModifier GemHihatClosed
    , tdDisco       = RTB.empty
    , tdRim         = getModifier GemRim
    , tdChoke       = getModifier GemCymbalChoke
    }

makeEliteDifficulty, makeEliteDifficultyDTX :: RTB.T U.Beats (EliteGem D.Hand, EliteGemType, DrumVelocity) -> EliteDrumDifficulty U.Beats
makeEliteDifficulty    = makeEliteDifficulty' $ const TBDefault
makeEliteDifficultyDTX = makeEliteDifficulty' $ \case
  Hihat  -> TBYellow
  CrashL -> TBBlue
  CrashR -> TBGreen
  Ride   -> TBGreen
  _      -> TBDefault

edKicks2 :: (NNC.C t) => EliteDrumDifficulty t -> RTB.T t ()
edKicks2 diff = flip RTB.mapMaybe (tdGems diff) $ \case
  (Kick D.LH, _, _) -> Just ()
  _                 -> Nothing
