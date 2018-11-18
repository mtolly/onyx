{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
module RockBand.Codec where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (forM, guard, void, (>=>))
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State.Strict (State, StateT, execState,
                                                   get, modify', put)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor.Identity            (Identity (..))
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import           Data.Tuple                       (swap)
import           Numeric.NonNegative.Class        ((-|))
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import qualified RockBand.PhaseShiftMessage       as PS
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U

-- like the json/dta one but uses StateT so it can modify the source as it goes
type TrackParser m t = StackTraceT (StateT (RTB.T t E.T) m)
type TrackBuilder t = State (RTB.T t E.T)
type TrackCodec m t a = Codec (TrackParser m t) (TrackBuilder t) a

type TrackEvent m t a = TrackCodec m t (RTB.T t a)

makeTrackBuilder :: (NNC.C t) => (c -> RTB.T t E.T) -> (c -> TrackBuilder t c)
makeTrackBuilder f = fmapArg $ \xs -> modify' $ \ys -> let
  ys' = RTB.merge ys $ f xs
  in seq (forceTrackSpine ys') ys'

runTrackBuilder :: (c -> TrackBuilder t c) -> (c -> RTB.T t E.T)
runTrackBuilder f = (`execState` RTB.empty) . f

-- | Forces the spine of the event list, and WHNFs the times and events.
forceTrackSpine :: RTB.T t a -> ()
forceTrackSpine rtb = case RTB.viewL rtb of
  Nothing              -> ()
  Just ((dt, x), rtb') -> seq dt $ seq x $ forceTrackSpine rtb'

slurpTrack :: (Monad m) => (RTB.T t a -> (RTB.T t b, RTB.T t a)) -> StackTraceT (StateT (RTB.T t a) m) (RTB.T t b)
slurpTrack f = lift $ do
  (slurp, leave) <- f <$> get
  seq (forceTrackSpine slurp) $ seq (forceTrackSpine leave) $ return ()
  put leave
  return slurp

-- Types of MIDI events:
-- * status text
--   e.g. mood/crowd, discobeat, handmap/strummap, PS tap/open, [onyx close],
--        [onyx octave]
-- * other text e.g. trainers, venue text events
-- * status notes e.g. pro keys ranges, protar chord roots and neck position
--   can be written as long notes up to next status or [end]
-- * blips e.g. drum notes, drum animation minus hihat pedal
--   should be written 16th/32nd notes or up to next note-on
-- * sustain notes
--   e.g. vocal notes, tom markers, lanes, OD, activation/BRE, solos,
--        hihat open animation, player 1/2, guitar animation neck position,
--        force hopo/strum, protar NoChordNames/SlashChords/FlatChords,
--        protar Arpeggio, probably protar Slide/PartialChord/AllFrets but
--        should test
--   should probably be stored as matched on/off pairs or notes with length
-- * blip-sustain e.g. 5fret/6fret/protar/prokeys notes
--   if length below a certain value, is a blip. otherwise sustain

blipCV :: (Monad m) => Int -> TrackEvent m U.Beats (Int, Int)
blipCV p = Codec
  { codecIn = do
    trk <- lift get
    let (slurp, leave) = flip RTB.partitionMaybe trk $ \x -> case isNoteEdgeCPV x of
          Just (c, p', v) | p == p' -> Just $ (c ,) <$> v
          _                         -> Nothing
    lift $ put leave
    return $ RTB.catMaybes slurp
  , codecOut = makeTrackBuilder $ U.trackJoin . fmap (\(c, v) -> unparseBlipCPV (c, p, v))
  }

-- | A blip is an event which is serialized as a MIDI note of unimportant length.
-- In Rock Band, these can always have their length set to 1\/32 of a beat,
-- which is the smallest allowed distance between events.
blip :: (Monad m) => Int -> TrackEvent m U.Beats ()
blip = dimap (fmap $ \() -> (0, 96)) void . blipCV

edge :: (Monad m) => Int -> Bool -> TrackEvent m U.Beats ()
edge p b = single
  (\x -> case isNoteEdge x of
    Just pb' | (p, b) == pb' -> Just ()
    _                        -> Nothing
  ) (\() -> makeEdge p b)

edges :: (Monad m, NNC.C t) => Int -> TrackEvent m t Bool
edges p = single
  (\x -> case isNoteEdge x of
    Just (p', b) | p == p' -> Just b
    _                      -> Nothing
  ) (makeEdge p)

edgesLanes :: (Monad m, NNC.C t) => Int -> TrackEvent m t (Maybe LaneDifficulty)
edgesLanes p = single
  (\x -> case isNoteEdgeCPV x of
    Just (_, p', mv) | p == p' -> Just $ (\v -> if v <= 64 then LaneHard else LaneExpert) <$> mv
    _                          -> Nothing
  ) (makeEdgeCPV 0 p . fmap (\case LaneHard -> 40; LaneExpert -> 96))

splitTrack :: (a -> (b, c)) -> RTB.T t a -> (RTB.T t b, RTB.T t c)
splitTrack f trk = let
  trk' = fmap f trk
  in (fmap fst trk', fmap snd trk')

removeEdgeGroup :: [Int] -> Bool -> [E.T] -> Maybe [E.T]
removeEdgeGroup [] _ evts = Just evts
removeEdgeGroup (p : ps) b evts =
  case break (maybe False (== (p, b)) . isNoteEdge) evts of
    (xs, _ : ys) -> removeEdgeGroup ps b $ xs ++ ys
    (_ , []    ) -> Nothing

edgesBRE :: (Monad m, NNC.C t) => [Int] -> TrackEvent m t Bool
edgesBRE ps = Codec
  { codecIn = slurpTrack $ \trk -> let
    f evts = let
      (hasOn, evts') = case removeEdgeGroup ps True evts of
        Nothing  -> (False, evts)
        Just new -> (True, new)
      (hasOff, evts'') = case removeEdgeGroup ps False evts' of
        Nothing  -> (False, evts')
        Just new -> (True, new)
      in ([ True | hasOn ] ++ [ False | hasOff ], evts'')
    (slurp, leave) = splitTrack f $ RTB.collectCoincident trk
    slurp' = RTB.flatten slurp
    leave' = RTB.flatten leave
    in if RTB.null slurp' then (RTB.empty, trk) else (slurp', leave')
  , codecOut = makeTrackBuilder $ RTB.flatten . fmap (\b -> [makeEdge p b | p <- ps])
  }

edgesCV :: (Monad m, NNC.C t) => Int -> TrackEvent m t (Int, Maybe Int)
edgesCV p = single
  (\x -> case isNoteEdgeCPV x of
    Just (c, p', v) | p == p' -> Just (c, v)
    _                         -> Nothing
  ) (\(c, v) -> makeEdgeCPV c p v)

single :: (Monad m, NNC.C t) => (E.T -> Maybe a) -> (a -> E.T) -> TrackEvent m t a
single fp fs = Codec
  { codecIn = slurpTrack $ RTB.partitionMaybe fp
  , codecOut = makeTrackBuilder $ fmap fs
  }

command :: (Monad m, NNC.C t, Command a) => TrackEvent m t a
command = single readCommand' showCommand'

commandMatch :: (Monad m, NNC.C t) => [T.Text] -> TrackEvent m t ()
commandMatch c = single
  (readCommand' >=> \c' -> guard (c == c') >> Just ())
  (\() -> showCommand' c)

joinEdges' :: (NNC.C t, Eq a) => RTB.T t (a, Maybe s) -> RTB.T t (a, s, t)
joinEdges' = fmap (\(s, a, t) -> (a, s, t)) . joinEdgesSimple . fmap swap

matchEdges :: (Monad m, NNC.C t) => TrackEvent m t Bool -> TrackEvent m t t
matchEdges = dimap
  (U.trackJoin . fmap (\len -> RTB.fromPairList [(NNC.zero, True), (len, False)]))
  (fmap (\((), (), t) -> t) . joinEdges' . fmap (\b -> ((), guard b >> Just ())))

matchEdgesCV :: (Ord a, Monad m, NNC.C t) => TrackEvent m t (a, Maybe Int) -> TrackEvent m t (a, Int, t)
matchEdgesCV = dimap
  (U.trackJoin . fmap (\(c, v, len) -> RTB.fromPairList [(NNC.zero, (c, Just v)), (len, (c, Nothing))]))
  joinEdges'

-- | Extends output notes within this group to be at least a 32nd note,
-- or up to the next note.
fatBlips :: (NNC.C t, Monad m) => t -> Codec (TrackParser m t) (TrackBuilder t) a -> Codec (TrackParser m t) (TrackBuilder t) a
fatBlips len cdc = cdc
  { codecOut = let
    extend trk = let
      (origEdges, notNotes) = RTB.partitionMaybe isNoteEdgeCPV trk
      notes = joinEdgesSimple $ fmap (\(c, p, mv) -> (mv, (c, p))) origEdges
      notes' = RTB.flatten $ go $ RTB.collectCoincident notes
      go rtb = case RTB.viewL rtb of
        Nothing -> RTB.empty
        Just ((dt, evs), rtb') -> let
          blipLen = case RTB.viewL rtb' of
            Nothing            -> len
            Just ((dt', _), _) -> min len dt'
          adjustLen (v, c, origLen) = (v, c, max origLen blipLen)
          in RTB.cons dt (map adjustLen evs) $ go rtb'
      edgeEvents = fmap (\(mv, (c, p)) -> makeEdgeCPV c p mv) $ splitEdgesSimple notes'
      in RTB.merge edgeEvents notNotes
    in makeTrackBuilder $ extend . runTrackBuilder (codecOut cdc)
  }

-- | Extends output notes within this group to go up to the next note on,
-- or to the last current MIDI event written so far.
statusBlips :: (NNC.C t, Monad m) => Codec (TrackParser m t) (TrackBuilder t) a -> Codec (TrackParser m t) (TrackBuilder t) a
statusBlips cdc = cdc
  { codecOut = \x -> do
    lastTime <- mconcat . RTB.getTimes <$> get
    let extend trk = let
          (origEdges, notNotes) = RTB.partitionMaybe isNoteEdgeCPV trk
          notes = joinEdgesSimple $ fmap (\(c, p, mv) -> (mv, (c, p))) origEdges
          notes' = RTB.flatten $ go NNC.zero $ RTB.collectCoincident notes
          go elapsed rtb = case RTB.viewL rtb of
            Nothing -> RTB.empty
            Just ((dt, evs), rtb') -> let
              elapsed' = NNC.add elapsed dt
              statusLen = case RTB.viewL rtb' of
                Nothing            -> lastTime -| elapsed'
                Just ((dt', _), _) -> dt'
              adjustLen (v, c, origLen) = (v, c, max origLen statusLen)
              in RTB.cons dt (map adjustLen evs) $ go elapsed' rtb'
          edgeEvents = fmap (\(mv, (c, p)) -> makeEdgeCPV c p mv) $ splitEdgesSimple notes'
          in RTB.merge edgeEvents notNotes
    makeTrackBuilder (extend . runTrackBuilder (codecOut cdc)) x
  }

eachKey
  :: (Monad m, NNC.C t, Ord k)
  => [k]
  -> (k -> TrackCodec m t a)
  -> TrackCodec m t (Map.Map k a)
eachKey keys f = Codec
  { codecIn = fmap Map.fromList $ forM keys $ \k -> do
    trk <- codecIn $ f k
    return (k, trk)
  , codecOut = makeTrackBuilder $ \m -> foldr RTB.merge RTB.empty $ do
    k <- keys
    trk <- toList $ Map.lookup k m
    return $ runTrackBuilder (codecOut $ f k) trk
  }

condenseMap
  :: (Monad m, NNC.C t, Ord k, Ord a)
  => TrackCodec m t (Map.Map k (RTB.T t a))
  -> TrackCodec m t (RTB.T t (k, a))
condenseMap = let
  fs trk = Map.fromList $ do
    k <- nubOrd $ map fst $ toList trk
    return (k, RTB.mapMaybe (\(k', v) -> guard (k == k') >> Just v) trk)
  fp m = foldr RTB.merge RTB.empty [fmap (k,) trk | (k, trk) <- Map.toList m]
  in dimap fs fp

condenseMap_
  :: (Monad m, NNC.C t, Ord k)
  => TrackCodec m t (Map.Map k (RTB.T t ()))
  -> TrackCodec m t (RTB.T t k)
condenseMap_ = dimap (fmap (, ())) (fmap fst) . condenseMap

blipSustainRB
  :: (Monad m, Ord a)
  => TrackCodec m U.Beats (RTB.T U.Beats (a, U.Beats))
  -> TrackCodec m U.Beats (RTB.T U.Beats (a, Maybe U.Beats))
blipSustainRB = let
  fs = fmap $ \(a, mt) -> (a, fromMaybe (1/32) mt)
  fp = fmap $ \(a, t) -> (a, guard (t > (1/3)) >> Just t)
  in dimap fs fp

sysexPS :: (Monad m, NNC.C t) => Difficulty -> PS.PhraseID -> TrackEvent m t Bool
sysexPS diff pid = Codec
  { codecIn = slurpTrack $ \trk -> let
    otherDiffs = filter (/= diff) each
    (slurp, leave) = flip RTB.partitionMaybe trk $ \x -> case PS.parsePSSysEx x <|> readCommand' x of
      Just (PS.PSMessage Nothing      pid' b) | pid == pid' -> Just (b, otherDiffs)
      Just (PS.PSMessage (Just diff') pid' b) | (diff, pid) == (diff', pid') -> Just (b, [])
      _                                       -> Nothing
    unslurp = RTB.flatten $ flip fmap slurp $ \(b, diffs) ->
      [ PS.unparsePSSysEx $ PS.PSMessage (Just d) pid b | d <- diffs ]
    in if RTB.null slurp
      then (RTB.empty, trk)
      else (fmap fst slurp, RTB.merge unslurp leave)
  , codecOut = makeTrackBuilder $ fmap (PS.unparsePSSysEx . PS.PSMessage (Just diff) pid)
  }

class ParseTrack trk where
  parseTrack :: (SendMessage m) => TrackCodec m U.Beats (trk U.Beats)

class TraverseTrack trk where
  traverseTrack :: (NNC.C t, NNC.C u, Applicative f) => (forall a. RTB.T t a -> f (RTB.T u a)) -> trk t -> f (trk u)

mapTrack :: (NNC.C t, NNC.C u, TraverseTrack trk) => (forall a. RTB.T t a -> RTB.T u a) -> trk t -> trk u
mapTrack f = runIdentity . traverseTrack (Identity . f)

traverseBlipSustain :: (NNC.C t, NNC.C u, Ord b, Applicative f) => (forall a. RTB.T t a -> f (RTB.T u a)) -> RTB.T t (b, Maybe t) -> f (RTB.T u (b, Maybe u))
traverseBlipSustain f
  = fmap (fmap (\((), a, mt) -> (a, mt)) . joinEdges)
  . f
  . splitEdges
  . fmap (\(a, mt) -> ((), a, mt))
