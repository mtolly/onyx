{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
module RockBand.Codec where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (forM, guard)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State        (StateT, get, put)
import           Control.Monad.Trans.Writer       (Writer, execWriter, tell)
import           Data.Either                      (partitionEithers)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Profunctor                  (dimap)
import           Data.Semigroup                   (Semigroup (..))
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.Parse                   (isNoteEdge, isNoteEdgeCPV,
                                                   makeEdge, makeEdgeCPV,
                                                   unparseBlip)
import qualified RockBand.PhaseShiftMessage       as PS
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

slurpTrack :: (Monad m) => (RTB.T t a -> (RTB.T t b, RTB.T t a)) -> StackTraceT (StateT (RTB.T t a) m) (RTB.T t b)
slurpTrack f = lift $ do
  (slurp, leave) <- f <$> get
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

edge :: (Monad m) => Int -> Bool -> TrackEvent m U.Beats ()
edge p b = single
  (\x -> case isNoteEdge x of
    Just pb' | (p, b) == pb' -> Just ()
    _        -> Nothing
  ) (\() -> makeEdge p b)

edges :: (Monad m, NNC.C t) => Int -> TrackEvent m t Bool
edges p = single
  (\x -> case isNoteEdge x of
    Just (p', b) | p == p' -> Just b
    _            -> Nothing
  ) (makeEdge p)

splitTrack :: (a -> (b, c)) -> RTB.T t a -> (RTB.T t b, RTB.T t c)
splitTrack f trk = let
  trk' = fmap f trk
  in (fmap fst trk', fmap snd trk')

edgesBRE :: (Monad m, NNC.C t) => [Int] -> TrackEvent m t Bool
edgesBRE ps = Codec
  { codecIn = slurpTrack $ \trk -> let
    f evts = let
      -- TODO this should require all the pitches, due to protar gtr vs bass BREs
      (match, left) = partitionEithers $ flip map evts $ \x -> case isNoteEdge x of
        Just (p, b) | elem p ps -> Left b
        _           -> Right x
      in (nubOrd match, left)
    (slurp, leave) = splitTrack f $ RTB.collectCoincident trk
    slurp' = RTB.flatten slurp
    leave' = RTB.flatten leave
    in if RTB.null slurp' then (RTB.empty, trk) else (slurp', leave')
  , codecOut = simpleShow $ RTB.flatten . fmap (\b -> [makeEdge p b | p <- ps])
  }

edgesCV :: (Monad m, NNC.C t) => Int -> TrackEvent m t (Int, Maybe Int)
edgesCV p = single
  (\x -> case isNoteEdgeCPV x of
    Just (c, p', v) | p == p' -> Just (c, v)
    _               -> Nothing
  ) (\(c, v) -> makeEdgeCPV c p v)

single :: (Monad m, NNC.C t) => (E.T -> Maybe a) -> (a -> E.T) -> TrackEvent m t a
single fp fs = Codec
  { codecIn = slurpTrack $ RTB.partitionMaybe fp
  , codecOut = simpleShow $ fmap fs
  }

command :: (Monad m, NNC.C t, Command a) => TrackEvent m t a
command = single readCommand' showCommand'

commandMatch :: (Monad m, NNC.C t) => [T.Text] -> TrackEvent m t ()
commandMatch c = single
  (\e -> readCommand' e >>= \c' -> guard (c == c') >> Just ())
  (\() -> showCommand' c)

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
  , codecOut = simpleShow $ fmap (PS.unparsePSSysEx . PS.PSMessage (Just diff) pid)
  }
