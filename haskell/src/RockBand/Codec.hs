{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
module RockBand.Codec where

import           Control.Monad                    (forM, guard)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State        (StateT, get, put)
import           Control.Monad.Trans.Writer       (Writer, execWriter, tell)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Profunctor                  (dimap)
import           Data.Semigroup                   (Semigroup (..))
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.Parse                   (isNoteEdge, isNoteEdgeCPV,
                                                   makeEdge, makeEdgeCPV,
                                                   unparseBlip)
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U

newtype MergeTrack t a = MergeTrack { runMergeTrack :: RTB.T t a }
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

instance (NNC.C t, Ord a) => Semigroup (MergeTrack t a) where
  MergeTrack x <> MergeTrack y = MergeTrack $ RTB.merge x y

instance (NNC.C t, Ord a) => Monoid (MergeTrack t a) where
  mappend = (<>)
  mempty = MergeTrack RTB.empty

-- like the json/dta one but uses StateT so it can modify the source as it goes
type TrackParser m t = StackTraceT (StateT (RTB.T t E.T) m)
type TrackBuilder t = Writer (MergeTrack t E.T)
type TrackCodec m t a = Codec (TrackParser m t) (TrackBuilder t) a

type TrackEvent m t a = TrackCodec m t (RTB.T t a)

simpleShow :: (c -> RTB.T t E.T) -> (c -> TrackBuilder t c)
simpleShow f = fmapArg $ tell . MergeTrack . f

simpleShown :: (c -> TrackBuilder t c) -> (c -> RTB.T t E.T)
simpleShown f = runMergeTrack . execWriter . f

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

blip :: (Monad m) => Int -> TrackEvent m U.Beats ()
blip n = Codec
  { codecIn = do
    trk <- lift get
    let (slurp, leave) = flip RTB.partitionMaybe trk $ \x -> case isNoteEdge x of
          Just (n', b) | n == n' -> Just $ guard b >> Just ()
          _            -> Nothing
    lift $ put leave
    return $ RTB.catMaybes slurp
  , codecOut = simpleShow $ U.trackJoin . fmap (\() -> unparseBlip n)
  }

edges :: (Monad m, NNC.C t) => Int -> TrackEvent m t Bool
edges p = Codec
  { codecIn = do
    trk <- lift get
    let (slurp, leave) = flip RTB.partitionMaybe trk $ \x -> case isNoteEdge x of
          Just (p', b) | p == p' -> Just b
          _            -> Nothing
    lift $ put leave
    return slurp
  , codecOut = simpleShow $ fmap $ makeEdge p
  }

edgesBRE :: (Monad m, NNC.C t) => [Int] -> TrackEvent m t Bool
edgesBRE = undefined -- TODO

edgesCV :: (Monad m, NNC.C t) => Int -> TrackEvent m t (Int, Maybe Int)
edgesCV p = Codec
  { codecIn = do
    trk <- lift get
    let (slurp, leave) = flip RTB.partitionMaybe trk $ \x -> case isNoteEdgeCPV x of
          Just (c, p', v) | p == p' -> Just (c, v)
          _               -> Nothing
    lift $ put leave
    return slurp
  , codecOut = simpleShow $ fmap $ \(c, v) -> makeEdgeCPV c p v
  }

single :: (Monad m, NNC.C t) => (E.T -> Maybe a) -> (a -> E.T) -> TrackEvent m t a
single fp fs = Codec
  { codecIn = do
    trk <- lift get
    let (slurp, leave) = RTB.partitionMaybe fp trk
    lift $ put leave
    return slurp
  , codecOut = simpleShow $ fmap fs
  }

command :: (Monad m, NNC.C t, Command a) => TrackEvent m t a
command = single readCommand' showCommand'

joinEdges' :: (NNC.C t, Eq a) => RTB.T t (a, Maybe s) -> RTB.T t (a, s, t)
joinEdges' rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> case x of
    (a, Just s) -> let
      isNoteOff (a', Nothing) = guard (a == a') >> Just ()
      isNoteOff _             = Nothing
      in case U.extractFirst isNoteOff rtb' of
        Nothing                 -> RTB.delay dt $ joinEdges' rtb' -- unmatched note on
        Just ((len, ()), rtb'') -> RTB.cons dt (a, s, len) $ joinEdges' rtb''
    (_, Nothing) -> RTB.delay dt $ joinEdges' rtb' -- unmatched note off

matchEdges :: (Monad m, NNC.C t) => TrackEvent m t Bool -> TrackEvent m t t
matchEdges = dimap
  (U.trackJoin . fmap (\len -> RTB.fromPairList [(NNC.zero, True), (len, False)]))
  (fmap (\((), (), t) -> t) . joinEdges' . fmap (\b -> ((), guard b >> Just ())))

matchEdgesCV :: (Monad m, NNC.C t) => TrackEvent m t (Int, Maybe Int) -> TrackEvent m t (Int, Int, t)
matchEdgesCV = dimap
  (U.trackJoin . fmap (\(c, v, len) -> RTB.fromPairList [(NNC.zero, (c, Just v)), (len, (c, Nothing))]))
  joinEdges'

-- | Extends output notes within this group to be at least a 32nd note,
-- or up to the next note.
fatBlips :: (NNC.C t, Monad m) => t -> TrackEvent m t a -> TrackEvent m t a
fatBlips _ = id -- TODO

-- | Extends output notes within this group to go up to the next note on.
statusBlips :: (NNC.C t, Monad m) => TrackEvent m t a -> TrackEvent m t a
statusBlips = id -- TODO

eachKey
  :: (Monad m, NNC.C t, Ord k)
  => [k]
  -> (k -> TrackCodec m t a)
  -> TrackCodec m t (Map.Map k a)
eachKey keys f = Codec
  { codecIn = fmap Map.fromList $ forM keys $ \k -> do
    trk <- codecIn $ f k
    return (k, trk)
  , codecOut = simpleShow $ \m -> foldr RTB.merge RTB.empty $ do
    k <- keys
    trk <- toList $ Map.lookup k m
    return $ simpleShown (codecOut $ f k) trk
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
