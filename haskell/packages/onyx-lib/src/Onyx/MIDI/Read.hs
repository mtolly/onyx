{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
module Onyx.MIDI.Read where

import           Control.Monad                    (forM, guard, void)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (State, StateT, execState,
                                                   get, modify', put, runState)
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
import           Onyx.MIDI.Common
import           Onyx.PhaseShift.Message
import qualified Onyx.PhaseShift.Message          as PS
import           Onyx.StackTrace
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Message.Channel       as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util                  as U

data MIDITrack f = MIDITrack
  { midiNotes       :: Map.Map V.Pitch (f (Edge V.Velocity C.Channel))
  , midiControllers :: Map.Map C.Controller (f (C.Channel, V.ControllerValue))
  , midiCommands    :: f [T.Text]
  , midiLyrics      :: f T.Text -- also any text events that aren't commands or comments
  , midiComments    :: f T.Text
  , midiPhaseShift  :: f PSMessage
  , midiPowerGig    :: f T.Text -- vocal text events for powergig
  , midiUnknown     :: f E.T
  }

mapMIDITrack :: (forall a. f a -> g a) -> MIDITrack f -> MIDITrack g
mapMIDITrack f mt = mt
  { midiNotes       = fmap f $ midiNotes       mt
  , midiCommands    = f      $ midiCommands    mt
  , midiLyrics      = f      $ midiLyrics      mt
  , midiComments    = f      $ midiComments    mt
  , midiPhaseShift  = f      $ midiPhaseShift  mt
  , midiPowerGig    = f      $ midiPowerGig    mt
  , midiUnknown     = f      $ midiUnknown     mt
  , midiControllers = fmap f $ midiControllers mt
  }

-- Note, this does not yet process different channel notes separately
fixZeroLengthNotes :: (NNC.C t) => t -> MIDITrack (RTB.T t) -> MIDITrack (RTB.T t)
fixZeroLengthNotes oneTick mt = mt { midiNotes = fmap fixRow $ midiNotes mt } where
  fixRow = go False . RTB.normalize
  go False (Wait t1 x@EdgeOff{} (Wait t2 y@EdgeOn{} rest)) | t2 == mempty
    = case rest of
      RNil -> Wait t1 y $ Wait oneTick x RNil
      Wait t3 z rest' -> let
        (m, (b, d)) = NNC.split oneTick t3
        newNoteTime = m {- min oneTick t3 -}
        restTime = if b {- oneTick <= t3 -} then d else NNC.zero
        in Wait t1 y $ Wait newNoteTime x $ go False $ Wait restTime z rest'
  go _ (Wait t x@EdgeOff{} rest) = Wait t x $ go False rest
  go _ (Wait t x@EdgeOn{}  rest) = Wait t x $ go True  rest
  go _ RNil                      = RNil

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
      E.MetaEvent (Meta.TextEvent lyric)
        | lyric == "\\n" || lyric == "\\r" -> cur
          { midiPowerGig = At t (T.pack lyric) $ midiPowerGig cur
          }
        | otherwise -> cur
          { midiLyrics = At t (T.pack lyric) $ midiLyrics cur
          }
      E.MIDIEvent (C.Cons chan (C.Voice (V.Control cont v))) -> cur
        { midiControllers = let
          f = Just . At t (chan, v) . fromMaybe ANil
          in Map.alter f cont $ midiControllers cur
        }
      _ -> cur
        { midiUnknown = At t x $ midiUnknown cur
        }
    in go new rest
  empty = MIDITrack
    { midiNotes       = Map.empty
    , midiCommands    = ATB.empty
    , midiLyrics      = ATB.empty
    , midiComments    = ATB.empty
    , midiPhaseShift  = ATB.empty
    , midiPowerGig    = ATB.empty
    , midiUnknown     = ATB.empty
    , midiControllers = Map.empty
    }
  in go empty . reverse . ATB.toPairList

putMIDITrack :: (Functor f) => (f E.T -> f E.T -> f E.T) -> MIDITrack f -> f E.T
putMIDITrack merge mt = foldr merge (midiUnknown mt) $ let
  notes = flip map (Map.toList $ midiNotes mt) $ \(p, lane) -> let
    f (EdgeOff  c) = makeEdgeCPV (C.fromChannel c) (V.fromPitch p) Nothing
    f (EdgeOn v c) = makeEdgeCPV (C.fromChannel c) (V.fromPitch p) (Just $ V.fromVelocity v)
    in f <$> lane
  controllers = flip fmap (Map.toList $ midiControllers mt) $ \(cont, lane) -> let
    f (chan, v) = E.MIDIEvent (C.Cons chan (C.Voice (V.Control cont v)))
    in f <$> lane
  in notes ++ controllers ++
    [ showCommand' <$> midiCommands mt
    , (\cmt -> E.MetaEvent $ Meta.TextEvent $ '#' : T.unpack cmt) <$> midiComments mt
    , (\t -> E.MetaEvent $ Meta.TextEvent $ T.unpack t) <$> midiPowerGig mt
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
        EdgeOff c  -> (C.fromChannel c, Nothing)
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

powerGigText :: (Monad m, NNC.C t) => T.Text -> TrackEvent m t ()
powerGigText t = Codec
  { codecIn = slurpTrack $ \mt -> case RTB.partitionMaybe (\x -> guard (x == t) >> Just ()) (midiPowerGig mt) of
    (matches, rest) -> (matches, mt { midiPowerGig = rest })
  , codecOut = let
    s = T.unpack t
    in makeTrackBuilder $ fmap $ E.MetaEvent . Meta.TextEvent . const s
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

-- | Tries to emulate the Harmonix MIDI chord snapping behavior.
-- If a note in the given pitch set begins, and then overlaps some other notes
-- that start up to 10 ticks (at 480 resolution) later, those notes will instead
-- start simultaneous with the earlier note.
chordSnap :: (Monad m) => [Int] -> CodecFor (TrackParser m U.Beats) (TrackBuilder U.Beats) a ()
chordSnap pitches = Codec
  { codecIn = lift $ modify' $ \mt -> let
    pitches' = map V.toPitch pitches
    (target, other) = Map.partitionWithKey (\p _ -> elem p pitches') $ midiNotes mt

    -- note, this is 1 more tick than the max distance we will snap
    maxSnapAnchorLength :: U.Beats
    maxSnapAnchorLength = 11 / 480

    -- first match up edges and just get all note lengths
    allNotes :: RTB.T U.Beats U.Beats
    allNotes = fmap (\(_vel, _ch, len) -> len)
      $ foldr RTB.merge RTB.empty $ map joinEdgesSimple $ Map.elems target

    -- then process them so we only have the notes that other notes could snap to
    snapAnchors = Left <$> notesToSnapAnchors allNotes
    notesToSnapAnchors :: RTB.T U.Beats U.Beats -> RTB.T U.Beats U.Beats
    notesToSnapAnchors = thinAnchors . fmap (min maxSnapAnchorLength . maximum) . RTB.collectCoincident
    thinAnchors = \case
      RNil             -> RNil
      Wait dt len rest -> Wait dt len $ thinAnchors $ RTB.delay len $ U.trackDrop len rest

    -- finally, use the anchors to adjust edges in each pitch
    eachSnap :: RTB.T U.Beats (Edge V.Velocity C.Channel) -> RTB.T U.Beats (Edge V.Velocity C.Channel)
    eachSnap = go . RTB.merge snapAnchors . fmap Right
    -- (merge puts Lefts before Rights)
    go :: RTB.T U.Beats (Either U.Beats (Edge V.Velocity C.Channel)) -> RTB.T U.Beats (Edge V.Velocity C.Channel)
    go = \case
      RNil -> RNil
      Wait dt (Right e) rest -> Wait dt e $ go rest
      Wait dt (Left len) rest -> case rest of
        -- pull back note on to the snap anchor
        Wait dt2 (Right on@EdgeOn{}) rest2 | dt2 < len -> Wait dt on $ go $ RTB.delay dt2 rest2
        -- pull back a note off and a note on to the snap anchor
        Wait dt2 (Right off@EdgeOff{}) (Wait dt3 (Right on@EdgeOn{}) rest2)
          | dt2 <> dt3 < len
          -> Wait dt off $ Wait 0 on $ go $ RTB.delay (dt2 <> dt3) rest2
        _ -> RTB.delay dt $ go rest

    target' = Map.fromList $ zip (Map.keys target) (map eachSnap $ Map.elems target)
    in mt
      { midiNotes = Map.union target' other
      }
  , codecOut = \_ -> return ()
  }

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
statusBlips :: (NNC.C t, Monad m) => CodecFor (TrackParser m t) (TrackBuilder t) c a -> CodecFor (TrackParser m t) (TrackBuilder t) c a
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
        (a, output) = runState (codecOut cdc x) RTB.empty
    _ <- makeTrackBuilder (\_ -> extend output) x
    return a
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

translateEdges :: (Functor m) => TrackCodec m t (RTB.T t (k, Bool)) -> TrackCodec m t (RTB.T t (Edge () k))
translateEdges = dimap
  (fmap $ \case EdgeOn () k -> (k, True); EdgeOff k -> (k, False))
  (fmap $ \(k, b) -> (if b then EdgeOn () else EdgeOff) k)

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
  fs = fmap $ \(a, mt) -> (a, fromMaybe (1/480) mt)
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

lookupCV :: (Show a, ChannelType a, SendMessage m) => (Int, velocity) -> StackTraceT m (a, velocity)
lookupCV (c, v) = do
  c' <- case lookup c channelMap of
    Just c' -> return c'
    Nothing  -> do
      -- TODO we should put the timestamp in here as a context layer
      let c' = minBound
      warn $ "Unrecognized channel " ++ show c ++ "; using default value of " ++ show c'
      return c'
  return (c', v)

channelEdges
  :: (Show a, ChannelType a, SendMessage m, NNC.C t)
  => Int -> TrackEvent m t (a, Maybe Int)
channelEdges p = let
  src = edgesCV p
  in Codec
    { codecIn = do
      trk <- codecIn src
      mapM lookupCV trk
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

channelBlip
  :: (Show a, ChannelType a, SendMessage m)
  => Int -> TrackEvent m U.Beats (a, Int)
channelBlip p = let
  src = blipCV p
  in Codec
    { codecIn = do
      trk <- codecIn src
      mapM lookupCV trk
    , codecOut = \x -> do
      _ <- codecOut src $ fmap (\(c', v) -> (encodeChannel c', v)) x
      return x
    }

channelBlip_
  :: (Show a, ChannelType a, SendMessage m)
  => Int -> TrackEvent m U.Beats a
channelBlip_ = dimap (fmap (, 100)) (fmap fst) . channelBlip

-- (channel, value)
controller :: (Monad m, NNC.C t) => Int -> TrackEvent m t (Int, Int)
controller cont = Codec
  { codecIn = slurpTrack $ \mt -> case Map.lookup (V.toController cont) (midiControllers mt) of
    Nothing -> (RTB.empty, mt)
    Just events -> let
      cvs = flip fmap events $ \(chan, v) -> (C.fromChannel chan, v)
      mt' = mt { midiControllers = Map.delete (V.toController cont) $ midiControllers mt }
      in (cvs, mt')
  , codecOut = makeTrackBuilder $ fmap
    (\(chan, v) -> E.MIDIEvent (C.Cons (C.toChannel chan) (C.Voice (V.Control (V.toController cont) v))))
  }

-- Only value
controller_ :: (Monad m, NNC.C t) => Int -> TrackEvent m t Int
controller_ = let
  fs v      = (0, v)
  fp (_, v) = v
  in dimap (fmap fs) (fmap fp) . controller

controllerBool :: (Monad m, NNC.C t) => Int -> TrackEvent m t Bool
controllerBool = dimap (fmap $ \b -> if b then 127 else 0) (fmap (/= 0)) . controller_
