{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
module RockBand.Codec where

import           Control.Monad                    (forM, guard, void)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State.Strict (State, StateT, execState,
                                                   get, modify', put)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor.Identity            (Identity (..))
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust)
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import           Numeric.NonNegative.Class        ((-|))
import qualified Numeric.NonNegative.Class        as NNC
import           PhaseShift.Message
import qualified PhaseShift.Message               as PS
import           RockBand.Common
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Message.Channel       as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util                  as U

data MIDITrack f = MIDITrack
  { midiNotes      :: Map.Map V.Pitch (f (Edge V.Velocity C.Channel))
  , midiCommands   :: f [T.Text]
  , midiLyrics     :: f T.Text -- also any text events that aren't commands or comments
  , midiComments   :: f T.Text
  , midiPhaseShift :: f PSMessage
  , midiUnknown    :: f E.T
  }

mapMIDITrack :: (forall a. f a -> g a) -> MIDITrack f -> MIDITrack g
mapMIDITrack f mt = mt
  { midiNotes = fmap f $ midiNotes mt
  , midiCommands = f $ midiCommands mt
  , midiLyrics = f $ midiLyrics mt
  , midiComments = f $ midiComments mt
  , midiPhaseShift = f $ midiPhaseShift mt
  , midiUnknown = f $ midiUnknown mt
  }

getMIDITrack :: (NNC.C t) => ATB.T t E.T -> MIDITrack (ATB.T t)
getMIDITrack = let
  go cur [] = cur
  go cur ((t, x) : rest) = let
    new = case x of
      (isNoteEdgeCPV -> Just (c, p, mv)) -> cur
        { midiNotes = let
          f = Just . At t noteEdge . fromMaybe ANil
          noteEdge = case mv of
            Nothing -> EdgeOff                 $ C.toChannel c
            Just v  -> EdgeOn (V.toVelocity v) $ C.toChannel c
          in Map.alter f (V.toPitch p) $ midiNotes cur
        }
      -- note, we do PS before commands so our PS command parser goes first
      (parsePS -> Just msgs) -> cur
        { midiPhaseShift = foldr (At t) (midiPhaseShift cur) msgs
        }
      (readCommandList -> Just cmd) -> cur
        { midiCommands = At t cmd $ midiCommands cur
        }
      E.MetaEvent (Meta.TextEvent ('#' : cmt)) -> cur
        { midiComments = At t (T.pack cmt) $ midiComments cur
        }
      E.MetaEvent (Meta.Lyric lyric) -> cur
        { midiLyrics = At t (T.pack lyric) $ midiLyrics cur
        }
      E.MetaEvent (Meta.TextEvent lyric) -> cur
        { midiLyrics = At t (T.pack lyric) $ midiLyrics cur
        }
      _ -> cur
        { midiUnknown = At t x $ midiUnknown cur
        }
    in go new rest
  empty = MIDITrack
    { midiNotes = Map.empty
    , midiCommands = ATB.empty
    , midiLyrics = ATB.empty
    , midiComments = ATB.empty
    , midiPhaseShift = ATB.empty
    , midiUnknown = ATB.empty
    }
  in go empty . reverse . ATB.toPairList

putMIDITrack :: (Functor f) => (f E.T -> f E.T -> f E.T) -> MIDITrack f -> f E.T
putMIDITrack merge mt = foldr merge (midiUnknown mt) $ let
  notes = flip map (Map.toList $ midiNotes mt) $ \(p, lane) -> let
    f (EdgeOff  c) = makeEdgeCPV (C.fromChannel c) (V.fromPitch p) Nothing
    f (EdgeOn v c) = makeEdgeCPV (C.fromChannel c) (V.fromPitch p) (Just $ V.fromVelocity v)
    in f <$> lane
  in notes ++
    [ showCommand' <$> midiCommands mt
    , (\cmt -> E.MetaEvent $ Meta.TextEvent $ '#' : T.unpack cmt) <$> midiComments mt
    , unparsePSSysEx <$> midiPhaseShift mt
    ]

-- like the json/dta one but uses StateT so it can modify the source as it goes
type TrackParser m t = StackTraceT (StateT (MIDITrack (RTB.T t)) m)
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

slurpTrack :: (Monad m) => (trk -> (RTB.T t b, trk)) -> StackTraceT (StateT trk m) (RTB.T t b)
slurpTrack f = lift $ do
  (slurp, leave) <- f <$> get
  -- TODO do we need to re-add a force for `leave`
  seq (forceTrackSpine slurp) $ return ()
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
  { codecIn = slurpTrack $ \mt -> case Map.lookup (V.toPitch p) (midiNotes mt) of
    Nothing        -> (RTB.empty, mt)
    Just noteEdges -> let
      cvs = flip RTB.mapMaybe noteEdges $ \case
        EdgeOn v c -> Just (C.fromChannel c, V.fromVelocity v)
        _          -> Nothing
      mt' = mt { midiNotes = Map.delete (V.toPitch p) $ midiNotes mt }
      in (cvs, mt')
  , codecOut = makeTrackBuilder $ U.trackJoin . fmap (\(c, v) -> unparseBlipCPV (c, p, v))
  }

-- | A blip is an event which is serialized as a MIDI note of unimportant length.
-- In Rock Band, these can always have their length set to 1\/32 of a beat,
-- which is the smallest allowed distance between events.
blip :: (Monad m) => Int -> TrackEvent m U.Beats ()
blip = dimap (fmap $ \() -> (0, 96)) void . blipCV

edge :: (Monad m) => Int -> Bool -> TrackEvent m U.Beats ()
edge p b = Codec
  { codecIn = slurpTrack $ \mt -> case Map.lookup (V.toPitch p) (midiNotes mt) of
    Nothing -> (RTB.empty, mt)
    Just noteEdges -> let
      (matches, rest) = flip RTB.partitionMaybe noteEdges $ \case
        EdgeOn _ _ | b     -> Just ()
        EdgeOff  _ | not b -> Just ()
        _                  -> Nothing
      mt' = mt { midiNotes = Map.insert (V.toPitch p) rest $ midiNotes mt }
      in (matches, mt')
  , codecOut = makeTrackBuilder $ fmap (\() -> makeEdge p b)
  }

edges :: (Monad m, NNC.C t) => Int -> TrackEvent m t Bool
edges = let
  fs b = (0, guard b >> Just 96)
  fp (_c, mv) = isJust mv
  in dimap (fmap fs) (fmap fp) . edgesCV

edgesLanes :: (Monad m, NNC.C t) => Int -> TrackEvent m t (Maybe LaneDifficulty)
edgesLanes = let
  fs mld = (0, (\case LaneHard -> 40; LaneExpert -> 96) <$> mld)
  fp (_c, mv) = (\v -> if v <= 50 then LaneHard else LaneExpert) <$> mv
  in dimap (fmap fs) (fmap fp) . edgesCV

splitTrack :: (a -> (b, c)) -> RTB.T t a -> (RTB.T t b, RTB.T t c)
splitTrack f trk = let
  trk' = fmap f trk
  in (fmap fst trk', fmap snd trk')

removeEdgeGroup :: [Int] -> Bool
  -> [(V.Pitch, Edge V.Velocity C.Channel)]
  -> Maybe [(V.Pitch, Edge V.Velocity C.Channel)]
removeEdgeGroup [] _ evts = Just evts
removeEdgeGroup (p : ps) b evts = let
  matcher (p', EdgeOff{}) = p == V.fromPitch p' && not b
  matcher (p', EdgeOn{})  = p == V.fromPitch p' && b
  in case break matcher evts of
    (xs, _ : ys) -> removeEdgeGroup ps b $ xs ++ ys
    (_ , []    ) -> Nothing

edgesBRE :: (Monad m, NNC.C t) => [Int] -> TrackEvent m t Bool
edgesBRE ps = Codec
  { codecIn = slurpTrack $ \mt -> let
    ourPitches
      = foldr RTB.merge RTB.empty
      $ map (\(p, xs) -> fmap (p,) xs)
      $ filter (\(p, _) -> elem (V.fromPitch p) ps)
      $ Map.toList $ midiNotes mt
    f evts = let
      (hasOn, evts') = case removeEdgeGroup ps True evts of
        Nothing  -> (False, evts)
        Just new -> (True, new)
      (hasOff, evts'') = case removeEdgeGroup ps False evts' of
        Nothing  -> (False, evts')
        Just new -> (True, new)
      in ([ True | hasOn ] ++ [ False | hasOff ], evts'')
    (slurp, leave) = splitTrack f $ RTB.collectCoincident ourPitches
    slurp' = RTB.flatten slurp
    leave' = RTB.flatten leave
    getLeavePitch p = fmap snd $ RTB.filter (\(p', _) -> p == p') leave'
    mt' = mt
      { midiNotes = Map.union
        (Map.fromList $ flip map ps $ (\p -> (p, getLeavePitch p)) . V.toPitch)
        (midiNotes mt)
      }
    in if RTB.null slurp' then (RTB.empty, mt) else (slurp', mt')
  , codecOut = makeTrackBuilder $ RTB.flatten . fmap (\b -> [makeEdge p b | p <- ps])
  }

edgesCV :: (Monad m, NNC.C t) => Int -> TrackEvent m t (Int, Maybe Int)
edgesCV p = Codec
  { codecIn = slurpTrack $ \mt -> case Map.lookup (V.toPitch p) (midiNotes mt) of
    Nothing -> (RTB.empty, mt)
    Just noteEdges -> let
      cvs = flip fmap noteEdges $ \case
        EdgeOn v c -> (C.fromChannel c, Just $ V.fromVelocity v)
        EdgeOff c -> (C.fromChannel c, Nothing)
      mt' = mt { midiNotes = Map.delete (V.toPitch p) $ midiNotes mt }
      in (cvs, mt')
  , codecOut = makeTrackBuilder $ fmap (\(c, mv) -> makeEdgeCPV c p mv)
  }

command :: (Monad m, NNC.C t, Command a) => TrackEvent m t a
command = Codec
  { codecIn = slurpTrack $ \mt -> case RTB.partitionMaybe toCommand (midiCommands mt) of
    (matches, rest) -> (matches, mt { midiCommands = rest })
  , codecOut = makeTrackBuilder $ fmap showCommand'
  }

commandMatch' :: (Monad m, NNC.C t) => ([T.Text] -> Maybe a) -> (a -> [T.Text]) -> TrackEvent m t a
commandMatch' fp fs = Codec
  { codecIn = slurpTrack $ \mt -> case RTB.partitionMaybe fp (midiCommands mt) of
    (matches, rest) -> (matches, mt { midiCommands = rest })
  , codecOut = makeTrackBuilder $ fmap $ showCommand' . fs
  }

commandMatch :: (Monad m, NNC.C t) => [T.Text] -> TrackEvent m t ()
commandMatch c = commandMatch'
  (\c' -> guard (c == c') >> Just ())
  (const c)

lyrics :: (Monad m, NNC.C t) => TrackEvent m t T.Text
lyrics = Codec
  { codecIn = slurpTrack $ \mt -> (midiLyrics mt, mt { midiLyrics = RTB.empty })
  , codecOut = makeTrackBuilder $ fmap $ E.MetaEvent . Meta.Lyric . T.unpack
  }

joinEdges' :: (NNC.C t, Eq a) => RTB.T t (a, Maybe s) -> RTB.T t (a, s, t)
joinEdges' = fmap (\(s, a, t) -> (a, s, t)) . joinEdgesSimple . fmap (\(a, ms) -> maybe (EdgeOff a) (`EdgeOn` a) ms)

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
      (origEdges, notNotes) = RTB.partitionMaybe isNoteEdge' trk
      notes = joinEdgesSimple origEdges
      notes' = RTB.flatten $ go $ RTB.collectCoincident notes
      go rtb = case RTB.viewL rtb of
        Nothing -> RTB.empty
        Just ((dt, evs), rtb') -> let
          blipLen = case RTB.viewL rtb' of
            Nothing            -> len
            Just ((dt', _), _) -> min len dt'
          adjustLen (v, c, origLen) = (v, c, max origLen blipLen)
          in RTB.cons dt (map adjustLen evs) $ go rtb'
      edgeEvents = fmap makeEdge' $ splitEdgesSimple notes'
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
          (origEdges, notNotes) = RTB.partitionMaybe isNoteEdge' trk
          notes = joinEdgesSimple origEdges
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
          edgeEvents = fmap makeEdge' $ splitEdgesSimple notes'
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
  { codecIn = slurpTrack $ \mt -> let
    otherDiffs = filter (/= diff) each
    matcher (PS.PSMessage (Just diff') pid' b) = do
      guard $ (diff, pid) == (diff', pid')
      Just (b, [])
    matcher (PS.PSMessage Nothing pid' b) = do
      guard $ pid == pid'
      Just (b, otherDiffs)
    in case RTB.partitionMaybe matcher (midiPhaseShift mt) of
      (matches, rest) -> let
        leftover = RTB.flatten $ flip fmap matches $ \(b, diffs) ->
          map (\diff' -> PS.PSMessage (Just diff') pid b) diffs
        in (fmap fst matches, mt { midiPhaseShift = RTB.merge leftover rest })
  , codecOut = makeTrackBuilder $ fmap $ PS.unparsePSSysEx . PS.PSMessage (Just diff) pid
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

reprPrefix :: (Show a) => T.Text -> a -> T.Text
reprPrefix pre x = let
  s = T.pack $ show x
  in case T.stripPrefix pre s of
    Just s' -> s'
    Nothing -> error $ "panic! couldn't strip prefix " <> show pre <> " from enum " <> show s

class (Enum a, Bounded a) => ChannelType a where
  encodeChannel :: a -> Int
  channelMap :: [(Int, a)]
  channelMap = [ (encodeChannel x, x) | x <- [minBound .. maxBound] ]

channelEdges
  :: (Show a, ChannelType a, SendMessage m, NNC.C t)
  => Int -> TrackEvent m t (a, Maybe Int)
channelEdges p = let
  src = edgesCV p
  in Codec
    { codecIn = do
      trk <- codecIn src
      forM trk $ \(c, v) -> do
        c' <- case lookup c channelMap of
          Just c' -> return c'
          Nothing  -> do
            -- TODO we should put the timestamp in here as a context layer
            let c' = minBound
            warn $ "Unrecognized channel " ++ show c ++ "; using default value of " ++ show c'
            return c'
        return (c', v)
    , codecOut = \x -> do
      _ <- codecOut src $ fmap (\(c', v) -> (encodeChannel c', v)) x
      return x
    }

channelEdges_
  :: (Show a, ChannelType a, SendMessage m, NNC.C t)
  => Int -> TrackEvent m t (a, Bool)
channelEdges_ = let
  -- velocity has to be at least 100 because Nemo's MIDI checker
  -- complains (incorrectly) if Pro Guitar slide notes have velocity < 100.
  fs (c, b) = (c, guard b >> Just 100)
  fp (c, v) = (c, isJust v)
  in dimap (fmap fs) (fmap fp) . channelEdges
