{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module WebPlayer
( makeDisplay
, showTimestamp
) where

import qualified Amplitude.File                   as Amp
import qualified Amplitude.Track                  as Amp
import qualified Config                           as C
import           Control.Applicative              ((<|>))
import           Control.Arrow                    (first)
import           Control.Monad                    (guard)
import qualified Data.Aeson                       as A
import qualified Data.Aeson.Types                 as A
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (toLower)
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Milli)
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd, sort)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   listToMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Guitars
import qualified Numeric.NonNegative.Class        as NNC
import qualified Numeric.NonNegative.Wrapper      as NN
import qualified RockBand.Codec.Beat              as Beat
import qualified RockBand.Codec.Drums             as D
import           RockBand.Codec.Events
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as Five
import qualified RockBand.Codec.ProGuitar         as PG
import           RockBand.Codec.ProKeys           as PK
import qualified RockBand.Codec.Six               as GHL
import           RockBand.Common                  (Difficulty (..),
                                                   LaneDifficulty (..),
                                                   LongNote (..), SongKey (..),
                                                   StrumHOPOTap (..), joinEdges,
                                                   songKeyUsesFlats, splitEdges)
import qualified RockBand.Legacy.Vocal            as Vox
import           Scripts                          (songLengthBeats)
import qualified Sound.MIDI.Util                  as U

data Five t = Five
  { fiveNotes  :: Map.Map (Maybe Five.Color) (RTB.T t (LongNote StrumHOPOTap ()))
  , fiveSolo   :: RTB.T t Bool
  , fiveEnergy :: RTB.T t Bool
  , fiveLanes  :: Map.Map (Maybe Five.Color) (RTB.T t Bool)
  , fiveBRE    :: RTB.T t Bool
  } deriving (Eq, Ord, Show)

eventList :: RTB.T U.Seconds a -> (a -> A.Value) -> A.Value
eventList evts f
  = A.toJSON
  $ concatMap g
  $ RTB.toPairList
  $ RTB.discretize
  $ RTB.mapTime (* 1000) evts
  where g (secs, evt) = let
          secs' = A.Number $ realToFrac (secs :: NN.Int)
          evt' = f evt
          in [secs', evt']

showHSTNote :: LongNote StrumHOPOTap () -> A.Value
showHSTNote = \case
  NoteOff () -> "e"
  Blip Tap   () -> "t"
  Blip Strum () -> "s"
  Blip HOPO  () -> "h"
  NoteOn Tap   () -> "T"
  NoteOn Strum () -> "S"
  NoteOn HOPO  () -> "H"

instance A.ToJSON (Five U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ fiveNotes x) $ \(color, notes) ->
      (,) (maybe "open" (T.pack . map toLower . show) color) $ eventList notes showHSTNote
    , (,) "solo" $ eventList (fiveSolo x) A.toJSON
    , (,) "energy" $ eventList (fiveEnergy x) A.toJSON
    , (,) "lanes" $ A.object $ flip map (Map.toList $ fiveLanes x) $ \(color, lanes) ->
      (,) (maybe "open" (T.pack . map toLower . show) color) $ eventList lanes A.toJSON
    , (,) "bre" $ eventList (fiveBRE x) A.toJSON
    ]

data Drums t = Drums
  { drumNotes  :: RTB.T t [D.Gem D.ProType]
  , drumSolo   :: RTB.T t Bool
  , drumEnergy :: RTB.T t Bool
  , drumLanes  :: Map.Map (D.Gem D.ProType) (RTB.T t Bool)
  , drumBRE    :: RTB.T t Bool
  , drumMode5  :: Bool
  } deriving (Eq, Ord, Show)

drumGemChar :: D.Gem D.ProType -> Char
drumGemChar = \case
  D.Kick                  -> 'k'
  D.Red                   -> 'r'
  D.Pro D.Yellow D.Cymbal -> 'Y'
  D.Pro D.Yellow D.Tom    -> 'y'
  D.Pro D.Blue   D.Cymbal -> 'B'
  D.Pro D.Blue   D.Tom    -> 'b'
  D.Pro D.Green  D.Cymbal -> 'G'
  D.Pro D.Green  D.Tom    -> 'g'
  D.Orange                -> 'O'

instance A.ToJSON (Drums U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ eventList (drumNotes x) $ A.toJSON . map drumGemChar
    , (,) "solo" $ eventList (drumSolo x) A.toJSON
    , (,) "energy" $ eventList (drumEnergy x) A.toJSON
    , (,) "lanes" $ A.object $ flip map (Map.toList $ drumLanes x) $ \(gem, lanes) ->
      (,) (T.singleton $ drumGemChar gem) $ eventList lanes A.toJSON
    , (,) "bre" $ eventList (drumBRE x) A.toJSON
    , (,) "mode-5" $ A.toJSON (drumMode5 x)
    ]

data ProKeys t = ProKeys
  { proKeysNotes     :: Map.Map PK.Pitch (RTB.T t (LongNote () ()))
  , proKeysRanges    :: RTB.T t PK.LaneRange
  , proKeysSolo      :: RTB.T t Bool
  , proKeysEnergy    :: RTB.T t Bool
  , proKeysLanes     :: Map.Map PK.Pitch (RTB.T t Bool)
  , proKeysGlissando :: RTB.T t Bool
  , proKeysBRE       :: RTB.T t Bool
  } deriving (Eq, Ord, Show)

showPitch :: PK.Pitch -> T.Text
showPitch = \case
  PK.RedYellow k -> "ry-" <> showKey k
  PK.BlueGreen k -> "bg-" <> showKey k
  PK.OrangeC -> "o-c"
  where showKey = T.pack . map toLower . show

_pitchMap :: [(T.Text, PK.Pitch)]
_pitchMap = do
  p <- [minBound .. maxBound]
  return (showPitch p, p)

_readPitch :: T.Text -> A.Parser PK.Pitch
_readPitch t = case lookup t _pitchMap of
  Just p  -> return p
  Nothing -> fail "invalid pro keys pitch name"

instance A.ToJSON (ProKeys U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ proKeysNotes x) $ \(p, notes) ->
      (,) (showPitch p) $ eventList notes $ \case
        NoteOff () -> "e"
        Blip () () -> "n"
        NoteOn () () -> "N"
    , (,) "ranges" $ eventList (proKeysRanges x) $ A.toJSON . map toLower . drop 5 . show
    , (,) "solo" $ eventList (proKeysSolo x) A.toJSON
    , (,) "energy" $ eventList (proKeysEnergy x) A.toJSON
    , (,) "lanes" $ A.object $ flip map (Map.toList $ proKeysLanes x) $ \(pitch, lanes) ->
      (,) (showPitch pitch) $ eventList lanes A.toJSON
    , (,) "gliss" $ eventList (proKeysGlissando x) A.toJSON
    , (,) "bre" $ eventList (proKeysBRE x) A.toJSON
    ]

data Protar t = Protar
  { protarNotes   :: Map.Map PG.GtrString (RTB.T t (LongNote (StrumHOPOTap, Maybe PG.GtrFret) ()))
  , protarSolo    :: RTB.T t Bool
  , protarEnergy  :: RTB.T t Bool
  , protarLanes   :: Map.Map PG.GtrString (RTB.T t Bool)
  , protarBRE     :: RTB.T t Bool
  , protarChords  :: RTB.T t (LongNote T.Text ())
  , protarStrings :: Int
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Protar U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ protarNotes x) $ \(string, notes) ->
      (,) (T.pack $ map toLower $ show string) $ eventList notes $ A.String . \case
        NoteOff () -> "e"
        Blip (Strum, fret) () -> "s" <> showFret fret
        Blip (HOPO, fret) () -> "h" <> showFret fret
        Blip (Tap, fret) () -> "t" <> showFret fret
        NoteOn (Strum, fret) () -> "S" <> showFret fret
        NoteOn (HOPO, fret) () -> "H" <> showFret fret
        NoteOn (Tap, fret) () -> "T" <> showFret fret
    , (,) "solo" $ eventList (protarSolo x) A.toJSON
    , (,) "energy" $ eventList (protarEnergy x) A.toJSON
    , (,) "lanes" $ A.object $ flip map (Map.toList $ protarLanes x) $ \(string, lanes) ->
      (,) (T.pack $ map toLower $ show string) $ eventList lanes A.toJSON
    , (,) "bre" $ eventList (protarBRE x) A.toJSON
    , (,) "chords" $ eventList (protarChords x) $ A.String . \case
      NoteOff     () -> "e"
      Blip   name () -> "c:" <> name
      NoteOn name () -> "C:" <> name
    , (,) "strings" $ A.toJSON $ protarStrings x
    ] where showFret Nothing  = "x"
            showFret (Just i) = T.pack (show i)

data GHLLane
  = GHLSingle GHL.Fret
  | GHLBoth1
  | GHLBoth2
  | GHLBoth3
  | GHLOpen
  deriving (Eq, Ord, Show, Read)

data Six t = Six
  { sixNotes  :: Map.Map GHLLane (RTB.T t (LongNote StrumHOPOTap ()))
  , sixSolo   :: RTB.T t Bool
  , sixEnergy :: RTB.T t Bool
  -- TODO lanes (not actually in CH)
  , sixBRE    :: RTB.T t Bool
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Six U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ sixNotes x) $ \(lane, notes) ->
      let showLane = \case
            GHLSingle GHL.Black1 -> "b1"
            GHLSingle GHL.Black2 -> "b2"
            GHLSingle GHL.Black3 -> "b3"
            GHLSingle GHL.White1 -> "w1"
            GHLSingle GHL.White2 -> "w2"
            GHLSingle GHL.White3 -> "w3"
            GHLBoth1 -> "bw1"
            GHLBoth2 -> "bw2"
            GHLBoth3 -> "bw3"
            GHLOpen -> "open"
      in (,) (showLane lane) $ eventList notes showHSTNote
    , (,) "solo" $ eventList (sixSolo x) A.toJSON
    , (,) "energy" $ eventList (sixEnergy x) A.toJSON
    , (,) "bre" $ eventList (sixBRE x) A.toJSON
    ]

realTrack :: (Ord a) => U.TempoMap -> RTB.T U.Beats a -> RTB.T U.Seconds a
realTrack tmap = U.applyTempoTrack tmap . RTB.normalize

filterKey :: (NNC.C t, Eq a) => a -> RTB.T t (LongNote s a) -> RTB.T t (LongNote s ())
filterKey k = RTB.mapMaybe $ mapM $ \x -> guard (k == x) >> return ()

findTremolos' :: (NNC.C t) => Map.Map t [a] -> Map.Map t ((), (), Maybe t) -> Map.Map t (Bool, [a])
findTremolos' ons trems = Map.fromList $ flip concatMap (Map.toAscList trems) $ \(start, (_, _, mlen)) -> case mlen of
  Nothing -> [] -- shouldn't happen (no blips)
  Just len -> case Map.lookupGE start ons of
    Nothing -> [] -- shouldn't happen (no notes under or after tremolo)
    Just (_, colors) -> [(start, (True, colors)), (start <> len, (False, colors))]

findTrills' :: (NNC.C t) => Map.Map t [a] -> Map.Map t ((), (), Maybe t) -> Map.Map t (Bool, [a])
findTrills' ons trills = Map.fromList $ flip concatMap (Map.toAscList trills) $ \(start, (_, _, mlen)) -> case mlen of
  Nothing -> [] -- shouldn't happen (no blips)
  Just len -> case Map.lookupGE start ons of
    Nothing -> [] -- shouldn't happen (no notes under or after trill)
    Just (t1, cols1) -> case Map.lookupGT t1 ons of
      Nothing -> [] -- shouldn't happen (only one note under or after trill)
      Just (_, cols2) -> let
        colors = cols1 ++ cols2
        in [(start, (True, colors)), (start <> len, (False, colors))]

findTremolos :: (Num t, NNC.C t) => RTB.T t [a] -> RTB.T t ((), (), Maybe t) -> RTB.T t (Bool, [a])
findTremolos ons trems = RTB.fromAbsoluteEventList $ ATB.fromPairList $ Map.toAscList
  $ findTremolos'
    (Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 ons)
    (Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 trems)

findTrills :: (Num t, NNC.C t) => RTB.T t [a] -> RTB.T t ((), (), Maybe t) -> RTB.T t (Bool, [a])
findTrills ons trills = RTB.fromAbsoluteEventList $ ATB.fromPairList $ Map.toAscList
  $ findTrills'
    (Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 ons)
    (Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 trills)

makeDifficulties :: (Difficulty -> Maybe (a t)) -> Difficulties a t
makeDifficulties f = Difficulties $ catMaybes $ do
  (d, name) <- [(Expert, "X"), (Hard, "H"), (Medium, "M"), (Easy, "E")]
  return $ (name,) <$> f d

makeDrumDifficulties :: (Maybe Difficulty -> Maybe (a t)) -> Difficulties a t
makeDrumDifficulties f = Difficulties $ catMaybes $ do
  (d, name) <- [(Nothing, "X+"), (Just Expert, "X"), (Just Hard, "H"), (Just Medium, "M"), (Just Easy, "E")]
  return $ (name,) <$> f d

laneDifficulty :: (NNC.C t) => Difficulty -> RTB.T t (Maybe LaneDifficulty) -> RTB.T t ((), (), Maybe t)
laneDifficulty Expert lanes = joinEdges $ (\case Just _ -> NoteOn () (); Nothing -> NoteOff ()) <$> lanes
laneDifficulty Hard   lanes = let
  joined = joinEdges $ (\case Just d -> NoteOn d (); Nothing -> NoteOff ()) <$> lanes
  in RTB.mapMaybe (\(d, (), len) -> guard (d == LaneHard) >> Just ((), (), len)) joined
laneDifficulty _      _     = RTB.empty

processFive :: Maybe U.Beats -> U.TempoMap -> Five.FiveTrack U.Beats -> Difficulties Five U.Seconds
processFive hopoThreshold tmap trk = makeDifficulties $ \diff -> let
  thisDiff = fromMaybe mempty $ Map.lookup diff $ Five.fiveDifficulties trk
  assigned
    = case hopoThreshold of
      Nothing -> fmap (\(col, len) -> ((col, Strum), len))
      Just ht -> applyForces (getForces5 thisDiff) . strumHOPOTap' HOPOsRBGuitar ht
    $ openNotes' thisDiff
  assigned' = U.trackJoin $ flip fmap assigned $ \((color, sht), mlen) -> case mlen of
    Nothing -> RTB.singleton 0 $ Blip sht color
    Just len -> RTB.fromPairList [(0, NoteOn sht color), (len, NoteOff color)]
  getColor color = realTrack tmap $ filterKey color assigned'
  notes = Map.fromList $ do
    color <- Nothing : map Just [minBound .. maxBound]
    return (color, getColor color)
  solo   = realTrack tmap $ Five.fiveSolo      trk
  energy = realTrack tmap $ Five.fiveOverdrive trk
  bre    = realTrack tmap $ Five.fiveBRE       trk
  ons    = RTB.normalize $ RTB.collectCoincident $ fmap (fst . fst) assigned
  trems  = findTremolos ons $ RTB.normalize $ laneDifficulty diff $ Five.fiveTremolo trk
  trills = findTrills ons $ RTB.normalize $ laneDifficulty diff $ Five.fiveTrill trk
  lanesAll = U.applyTempoTrack tmap $ RTB.merge trems trills
  lanes = Map.fromList $ do
    color <- Nothing : fmap Just [Five.Green .. Five.Orange]
    return $ (,) color $ flip RTB.mapMaybe lanesAll $ \(b, colors) -> do
      guard $ elem color colors
      return b
  in guard (not $ RTB.null $ Five.fiveGems thisDiff) >> Just (Five notes solo energy lanes bre)

processSix :: U.Beats -> U.TempoMap -> GHL.SixTrack U.Beats -> Difficulties Six U.Seconds
processSix hopoThreshold tmap trk = makeDifficulties $ \diff -> let
  thisDiff = fromMaybe mempty $ Map.lookup diff $ GHL.sixDifficulties trk
  assigned
    = applyForces (getForces6 thisDiff)
    $ strumHOPOTap' HOPOsRBGuitar hopoThreshold $ GHL.sixGems thisDiff
  assigned' = U.trackJoin $ flip fmap assigned $ \((color, sht), mlen) -> case mlen of
    Nothing -> RTB.singleton 0 $ Blip sht color
    Just len -> RTB.fromPairList [(0, NoteOn sht color), (len, NoteOff color)]
  oneTwoBoth x y = let
    dual = RTB.collectCoincident $ RTB.merge (fmap (const False) <$> x) (fmap (const True) <$> y)
    (both, notBoth) = flip RTB.partitionMaybe dual $ \case
      [a, b] | (() <$ a) == (() <$ b) -> Just $ () <$ a
      _                               -> Nothing
    notBoth' = RTB.flatten notBoth
    one = filterKey False notBoth'
    two = filterKey True  notBoth'
    in (one, two, both)
  (b1, w1, bw1) = oneTwoBoth (filterKey (Just GHL.Black1) assigned') (filterKey (Just GHL.White1) assigned')
  (b2, w2, bw2) = oneTwoBoth (filterKey (Just GHL.Black2) assigned') (filterKey (Just GHL.White2) assigned')
  (b3, w3, bw3) = oneTwoBoth (filterKey (Just GHL.Black3) assigned') (filterKey (Just GHL.White3) assigned')
  getLane = realTrack tmap . \case
    GHLSingle GHL.Black1 -> b1
    GHLSingle GHL.Black2 -> b2
    GHLSingle GHL.Black3 -> b3
    GHLSingle GHL.White1 -> w1
    GHLSingle GHL.White2 -> w2
    GHLSingle GHL.White3 -> w3
    GHLBoth1 -> bw1
    GHLBoth2 -> bw2
    GHLBoth3 -> bw3
    GHLOpen -> filterKey Nothing assigned'
  notes = Map.fromList $ do
    lane <- map GHLSingle [minBound .. maxBound] ++ [GHLBoth1, GHLBoth2, GHLBoth3, GHLOpen]
    return (lane, getLane lane)
  solo   = realTrack tmap $ GHL.sixSolo      trk
  energy = realTrack tmap $ GHL.sixOverdrive trk
  bre    = RTB.empty
  in guard (not $ RTB.null $ GHL.sixGems thisDiff) >> Just (Six notes solo energy bre)

processDrums :: C.DrumMode -> U.TempoMap -> Maybe U.Beats -> D.DrumTrack U.Beats -> D.DrumTrack U.Beats -> Difficulties Drums U.Seconds
processDrums mode tmap coda trk1x trk2x = makeDrumDifficulties $ \diff -> let
  has2x = all (not . D.nullDrums) [trk1x, trk2x] || any (not . RTB.null . D.drumKick2x) [trk1x, trk2x]
  trk = case (D.nullDrums trk1x, D.nullDrums trk2x, diff) of
    (True , _    , _      ) -> trk2x
    (_    , True , _      ) -> trk1x
    (False, False, Nothing) -> trk2x
    (False, False, Just _ ) -> trk1x
  nonPro is5 = (if diff == Nothing then RTB.merge $ const D.Kick <$> D.drumKick2x trk else id)
    $ flip fmap (maybe RTB.empty D.drumGems $ Map.lookup (fromMaybe Expert diff) $ D.drumDifficulties trk) $ \case
      D.Kick            -> D.Kick
      D.Red             -> D.Red
      D.Pro D.Yellow () -> D.Pro D.Yellow $ if is5 then D.Cymbal else D.Tom
      D.Pro D.Blue   () -> D.Pro D.Blue D.Tom
      D.Orange          -> D.Orange
      D.Pro D.Green  () -> D.Pro D.Green D.Tom
  notes = fmap sort $ RTB.collectCoincident $ case mode of
    C.Drums4   -> nonPro False
    C.Drums5   -> nonPro True
    C.DrumsPro -> D.computePro diff trk
  notesS = realTrack tmap notes
  notesB = RTB.normalize notes
  solo   = realTrack tmap $ D.drumSolo trk
  energy = realTrack tmap $ D.drumOverdrive trk
  fills  = realTrack tmap $ D.drumActivation trk
  bre    = case U.applyTempoMap tmap <$> coda of
    Nothing -> RTB.empty
    Just c  -> RTB.delay c $ U.trackDrop c fills
  hands  = RTB.filter (not . null) $ fmap (filter (/= D.Kick)) notesB
  singles = findTremolos hands $ RTB.normalize $ laneDifficulty (fromMaybe Expert diff) $ D.drumSingleRoll trk
  doubles = findTrills   hands $ RTB.normalize $ laneDifficulty (fromMaybe Expert diff) $ D.drumDoubleRoll trk
  lanesAll = U.applyTempoTrack tmap $ RTB.merge singles doubles
  lanes = Map.fromList $ do
    gem <-
      [ D.Kick, D.Red
      , D.Pro D.Yellow D.Cymbal, D.Pro D.Yellow D.Tom
      , D.Pro D.Blue   D.Cymbal, D.Pro D.Blue   D.Tom
      , D.Pro D.Green  D.Cymbal, D.Pro D.Green  D.Tom
      , D.Orange
      ]
    return $ (,) gem $ flip RTB.mapMaybe lanesAll $ \(b, gems) -> do
      guard $ elem gem gems
      return b
  in do
    guard $ not $ RTB.null notes
    guard $ diff /= Nothing || has2x
    Just $ Drums notesS solo energy lanes bre $ mode == C.Drums5

processProKeys :: U.TempoMap -> ProKeysTrack U.Beats -> Maybe (ProKeys U.Seconds)
processProKeys tmap trk = let
  assigned' = U.trackJoin $ flip fmap (pkNotes trk) $ \(p, mlen) -> case mlen of
    Nothing  -> RTB.singleton 0 $ Blip () p
    Just len -> RTB.fromPairList [(0, NoteOn () p), (len, NoteOff p)]
  notesForPitch p = realTrack tmap $ filterKey p assigned'
  notes = Map.fromList [ (p, notesForPitch p) | p <- [minBound .. maxBound] ]
  ons = RTB.normalize $ RTB.collectCoincident $ fmap fst $ pkNotes trk
  ranges = realTrack tmap $ pkLanes trk
  solo   = realTrack tmap $ pkSolo trk
  energy = realTrack tmap $ pkOverdrive trk
  trills = findTrills ons $ RTB.normalize $ joinEdges
    $ fmap (\b -> if b then NoteOn () () else NoteOff ()) $ pkTrill trk
  lanesAll = U.applyTempoTrack tmap trills
  lanes = Map.fromList $ do
    key <- [minBound .. maxBound]
    return $ (,) key $ flip RTB.mapMaybe lanesAll $ \(b, keys) -> do
      guard $ elem key keys
      return b
  gliss  = realTrack tmap $ pkGlissando trk
  bre    = realTrack tmap $ pkBRE trk
  in guard (not $ RTB.null $ pkNotes trk) >> Just (ProKeys notes ranges solo energy lanes gliss bre)

processProtar :: U.Beats -> [Int] -> Bool -> U.TempoMap -> PG.ProGuitarTrack U.Beats -> Difficulties Protar U.Seconds
processProtar hopoThreshold relTuning defaultFlat tmap pg = makeDifficulties $ \diff -> let
  thisDiff = fromMaybe mempty $ Map.lookup diff $ PG.pgDifficulties pg
  assigned = expandColors $ PG.guitarifyHOPO hopoThreshold thisDiff
  expandColors = splitEdges . RTB.flatten . fmap expandChord
  expandChord (shopo, gems, len) = do
    (str, fret, ntype) <- gems
    return ((shopo, guard (ntype /= PG.Muted) >> Just fret), str, len)
  getString string = realTrack tmap $ flip RTB.mapMaybe assigned $ \case
    Blip   ntype s -> guard (s == string) >> Just (Blip   ntype ())
    NoteOn ntype s -> guard (s == string) >> Just (NoteOn ntype ())
    NoteOff      s -> guard (s == string) >> Just (NoteOff      ())
  notes = Map.fromList $ do
    string <- [minBound .. maxBound]
    return (string, getString string)
  solo   = realTrack tmap $ PG.pgSolo pg
  energy = realTrack tmap $ PG.pgOverdrive pg
  -- for protar we can treat tremolo/trill identically;
  -- in both cases it's just "get the strings the first note/chord is on"
  onStrings = RTB.normalize $ RTB.collectCoincident $ fmap fst $ PG.pgNotes thisDiff
  lanesAll = U.applyTempoTrack tmap $ findTremolos onStrings $ RTB.normalize
    $ laneDifficulty diff $ RTB.merge (PG.pgTremolo pg) (PG.pgTrill pg)
  lanes = Map.fromList $ do
    str <- [minBound .. maxBound]
    return $ (,) str $ flip RTB.mapMaybe lanesAll $ \(b, strs) -> do
      guard $ elem str strs
      return b
  bre = realTrack tmap $ fmap snd $ PG.pgBRE pg
  usedStrings = nubOrd $ toList (PG.pgDifficulties pg) >>= map fst . toList . PG.pgNotes
  stringCount
    | not (null $ drop 5 relTuning) || elem PG.S1 usedStrings = 6
    | not (null $ drop 4 relTuning) || elem PG.S2 usedStrings = 5
    | not (null $ drop 3 relTuning) || elem PG.S3 usedStrings = 4
    | otherwise                                               = 3
  tuning = zipWith (+) PG.standardGuitar $ relTuning ++ repeat 0
  chords = realTrack tmap $ PG.computeChordNames diff tuning defaultFlat pg
  -- TODO include correct strings number
  in guard (not $ RTB.null $ PG.pgNotes thisDiff) >> Just (Protar notes solo energy lanes bre chords stringCount)

newtype Beats t = Beats
  { beatLines :: RTB.T t Beat
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Beats U.Seconds) where
  toJSON x = A.object
    [ (,) "lines" $ eventList (beatLines x) $ A.toJSON . \case
      Bar      -> 0 :: Int
      Beat     -> 1
      HalfBeat -> 2
    ]

data Beat
  = Bar
  | Beat
  | HalfBeat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

processBeat :: U.TempoMap -> RTB.T U.Beats Beat.BeatEvent -> Beats U.Seconds
processBeat tmap rtb = Beats $ U.applyTempoTrack tmap $ flip fmap rtb $ \case
    Beat.Bar -> Bar
    Beat.Beat -> Beat
    -- TODO: add half-beats

data Vocal t = Vocal
  { harm1Notes      :: RTB.T t VocalNote
  , harm2Notes      :: RTB.T t VocalNote
  , harm3Notes      :: RTB.T t VocalNote
  , vocalPercussion :: RTB.T t ()
  , vocalPhraseEnds :: RTB.T t ()
  , vocalRanges     :: RTB.T t VocalRange
  , vocalEnergy     :: RTB.T t Bool
  , vocalTonic      :: Maybe Int
  } deriving (Eq, Ord, Show)

data VocalRange
  = VocalRangeShift    -- ^ Start of a range shift
  | VocalRange Int Int -- ^ The starting range, or the end of a range shift
  deriving (Eq, Ord, Show)

data VocalNote
  = VocalStart T.Text (Maybe Int)
  | VocalEnd
  deriving (Eq, Ord, Show)

instance A.ToJSON (Vocal U.Seconds) where
  toJSON x = A.object
    [ (,) "harm1" $ eventList (harm1Notes x) voxEvent
    , (,) "harm2" $ eventList (harm2Notes x) voxEvent
    , (,) "harm3" $ eventList (harm3Notes x) voxEvent
    , (,) "percussion" $ eventList (vocalPercussion x) $ \() -> A.Null
    , (,) "phrases" $ eventList (vocalPhraseEnds x) $ \() -> A.Null
    , (,) "ranges" $ eventList (vocalRanges x) $ \case
      VocalRange pmin pmax -> A.toJSON [pmin, pmax]
      VocalRangeShift      -> A.Null
    , (,) "energy" $ eventList (vocalEnergy x) A.toJSON
    , (,) "tonic" $ maybe A.Null A.toJSON $ vocalTonic x
    ] where voxEvent VocalEnd                 = A.Null
            voxEvent (VocalStart lyric pitch) = A.toJSON [A.toJSON lyric, maybe A.Null A.toJSON pitch]

rtbMapMaybeWithAbsoluteTime :: (Num t) => (t -> a -> Maybe b) -> RTB.T t a -> RTB.T t b
rtbMapMaybeWithAbsoluteTime f = RTB.fromAbsoluteEventList . ATB.foldrPair g ATB.empty . RTB.toAbsoluteEventList 0 where
  g absTime body = case f absTime body of
    Nothing    -> id
    Just body' -> ATB.cons absTime body'

showTimestamp :: U.Seconds -> String
showTimestamp secs = let
  minutes = floor $ secs / 60 :: Int
  seconds = secs - realToFrac minutes * 60
  milli = realToFrac seconds :: Milli
  pad = if milli < 10 then "0" else ""
  in show minutes ++ ":" ++ pad ++ show milli

processVocal
  :: U.TempoMap
  -> RTB.T U.Beats Vox.Event
  -> RTB.T U.Beats Vox.Event
  -> RTB.T U.Beats Vox.Event
  -> Maybe Int
  -> Vocal U.Seconds
processVocal tmap h1 h2 h3 tonic = let
  perc = realTrack tmap $ flip RTB.mapMaybe h1 $ \case
    Vox.Percussion -> Just ()
    _              -> Nothing
  ends = realTrack tmap $ flip RTB.mapMaybe h1 $ \case
    Vox.Phrase False  -> Just ()
    Vox.Phrase2 False -> Just ()
    _                 -> Nothing
  pitchToInt p = fromEnum p + 36
  makeVoxPart trk = realTrack tmap $ flip rtbMapMaybeWithAbsoluteTime (RTB.collectCoincident trk) $ \bts evts -> let
    lyric = listToMaybe [ s | Vox.Lyric s <- evts ]
    note = listToMaybe [ p | Vox.Note True p <- evts ]
    end = listToMaybe [ () | Vox.Note False _ <- evts ]
    in case (lyric, note, end) of
      -- Note: the _ in the first pattern below should be Nothing,
      -- but we allow Just () for sloppy vox charts with no gap between notes
      (Just l, Just p, _) -> Just $ case T.stripSuffix "#" l <|> T.stripSuffix "^" l of
        Nothing -> case T.stripSuffix "#$" l <|> T.stripSuffix "^$" l of
          Nothing -> VocalStart l $ Just $ pitchToInt p -- non-talky
          Just l' -> VocalStart (l' <> "$") Nothing     -- hidden lyric talky
        Just l' -> VocalStart l' Nothing                -- talky
      (Nothing, Nothing, Just ()) -> Just VocalEnd
      (Nothing, Nothing, Nothing) -> Nothing
      lne -> error $
        "processVocal: invalid set of vocal events at " ++ showTimestamp (U.applyTempoMap tmap bts) ++ "! " ++ show lne
  harm1 = makeVoxPart h1
  harm2 = makeVoxPart h2
  harm3 = makeVoxPart h3
  -- TODO: handle range changes
  ranges = RTB.singleton 0 $ VocalRange (foldr min 84 allPitches) (foldr max 36 allPitches)
  allPitches = [ p | VocalStart _ (Just p) <- concatMap RTB.getBodies [harm1, harm2, harm3] ]
  in Vocal
    { vocalPercussion = perc
    , vocalPhraseEnds = ends
    , vocalTonic = tonic
    , harm1Notes = harm1
    , harm2Notes = harm2
    , harm3Notes = harm3
    , vocalEnergy = realTrack tmap $ flip RTB.mapMaybe h1 $ \case
      Vox.Overdrive b -> Just b
      _               -> Nothing
    , vocalRanges = ranges
    }

data Amplitude t = Amplitude
  { ampNotes      :: RTB.T t Amp.Gem
  , ampInstrument :: Amp.Instrument
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Amplitude U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ eventList (ampNotes x) $ \case
      Amp.L -> A.Number 1
      Amp.M -> A.Number 2
      Amp.R -> A.Number 3
    , (,) "instrument" $ A.toJSON $ show $ ampInstrument x
    ]

newtype Difficulties a t = Difficulties [(T.Text, a t)]
  deriving (Eq, Ord, Show)

instance (A.ToJSON (a t)) => A.ToJSON (Difficulties a t) where
  toJSON (Difficulties xs) = A.toJSON [ [A.toJSON d, A.toJSON x] | (d, x) <- xs ]

data Flex t = Flex
  { flexFive    :: Maybe (Difficulties Five      t)
  , flexSix     :: Maybe (Difficulties Six       t)
  , flexDrums   :: Maybe (Difficulties Drums     t)
  , flexProKeys :: Maybe (Difficulties ProKeys   t)
  , flexProtar  :: Maybe (Difficulties Protar    t)
  , flexCatch   :: Maybe (Difficulties Amplitude t)
  , flexVocal   :: Maybe (Difficulties Vocal     t)
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Flex U.Seconds) where
  toJSON flex = A.object $ concat
    [ case flexFive    flex of Nothing -> []; Just x -> [("five"   , A.toJSON x)]
    , case flexSix     flex of Nothing -> []; Just x -> [("six"    , A.toJSON x)]
    , case flexDrums   flex of Nothing -> []; Just x -> [("drums"  , A.toJSON x)]
    , case flexProKeys flex of Nothing -> []; Just x -> [("prokeys", A.toJSON x)]
    , case flexProtar  flex of Nothing -> []; Just x -> [("protar" , A.toJSON x)]
    , case flexCatch   flex of Nothing -> []; Just x -> [("catch"  , A.toJSON x)]
    , case flexVocal   flex of Nothing -> []; Just x -> [("vocal"  , A.toJSON x)]
    ]

data Processed t = Processed
  { processedTitle  :: T.Text
  , processedArtist :: T.Text
  , processedAuthor :: T.Text
  , processedBeats  :: Beats t
  , processedEnd    :: t
  , processedParts  :: [(T.Text, Flex t)]
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Processed U.Seconds) where
  toJSON proc = A.object $ concat
    [ [("title" , A.toJSON $ processedTitle  proc)]
    , [("artist", A.toJSON $ processedArtist proc)]
    , [("author", A.toJSON $ processedAuthor proc)]
    , [("beats" , A.toJSON $ processedBeats  proc)]
    , [("end"   , A.Number $ realToFrac (realToFrac $ processedEnd proc :: Milli))]
    , [("parts" , A.toJSON $ processedParts  proc)]
    ]

makeDisplay :: C.SongYaml -> RBFile.Song (RBFile.OnyxFile U.Beats) -> BL.ByteString
makeDisplay songYaml song = let
  ht n = fromIntegral n / 480
  coda = fmap (fst . fst) $ RTB.viewL $ eventsCoda $ RBFile.onyxEvents $ RBFile.s_tracks song
  defaultFlat = maybe False songKeyUsesFlats $ C._key $ C._metadata songYaml
  -- the above gets imported from first song_key then vocal_tonic_note
  makePart name fpart = Flex
    { flexFive = flip fmap (C.partGRYBO fpart) $ \grybo -> let
      gtr = RBFile.onyxPartGuitar tracks
      keys = RBFile.onyxPartKeys tracks
      in if Five.nullFive gtr
        then processFive Nothing (RBFile.s_tempos song) keys
        else processFive (Just $ ht $ C.gryboHopoThreshold grybo) (RBFile.s_tempos song) gtr
    , flexSix = flip fmap (C.partGHL fpart) $ \ghl -> processSix (ht $ C.ghlHopoThreshold ghl) (RBFile.s_tempos song) (RBFile.onyxPartSix tracks)
    , flexDrums = flip fmap (C.partDrums fpart) $ \pd -> processDrums (C.drumsMode pd) (RBFile.s_tempos song) coda (RBFile.onyxPartDrums tracks) (RBFile.onyxPartDrums2x tracks)
    , flexProKeys = flip fmap (C.partProKeys fpart) $ \_ -> makeDifficulties $ \diff ->
      processProKeys (RBFile.s_tempos song) $ case diff of
        Easy   -> RBFile.onyxPartRealKeysE tracks
        Medium -> RBFile.onyxPartRealKeysM tracks
        Hard   -> RBFile.onyxPartRealKeysH tracks
        Expert -> RBFile.onyxPartRealKeysX tracks
    , flexProtar = flip fmap (C.partProGuitar fpart) $ \pg -> processProtar
      (ht $ C.pgHopoThreshold pg)
      (case C.pgTuning pg of
        [] -> case name of
          RBFile.FlexBass -> replicate 4 0
          _               -> replicate 6 0
        offs -> offs
      )
      defaultFlat
      (RBFile.s_tempos song)
      $ let mustang = RBFile.onyxPartRealGuitar tracks
            squier  = RBFile.onyxPartRealGuitar22 tracks
        in if PG.nullPG squier then mustang else squier
    , flexVocal = flip fmap (C.partVocal fpart) $ \pvox -> let
      harm = case C.vocalCount pvox of
        C.Vocal3 -> [("H", makeVox pvox
          (RBFile.onyxHarm1 tracks)
          (RBFile.onyxHarm2 tracks)
          (RBFile.onyxHarm3 tracks))]
        C.Vocal2 -> [("H", makeVox pvox
          (RBFile.onyxHarm1 tracks)
          (RBFile.onyxHarm2 tracks)
          mempty)]
        C.Vocal1 -> []
      solo = ("1", makeVox pvox (RBFile.onyxPartVocals tracks) mempty mempty)
      in Difficulties $ reverse $ solo : harm
    , flexCatch = flip fmap (C.partAmplitude fpart) $ \amp -> let
      ampDiffNames (Difficulties pairs) = Difficulties $ flip map pairs $ first $ \case
        "X" -> "S/X"
        "H" -> "A"
        "M" -> "I"
        "E" -> "B"
        d   -> d
      in ampDiffNames $ makeDifficulties $ \diff -> do
        notes <- fmap Amp.catchGems $ Map.lookup diff $ Amp.catchDifficulties $ RBFile.onyxCatch tracks
        guard $ not $ RTB.null notes
        return Amplitude
          { ampNotes = U.applyTempoTrack (RBFile.s_tempos song) notes
          , ampInstrument = C.ampInstrument amp
          }
    } where
      tracks = RBFile.getFlexPart name $ RBFile.s_tracks song
  parts = do
    (name, fpart) <- sort $ HM.toList $ HM.filter (/= def) $ C.getParts $ C._parts songYaml
    return (RBFile.getPartName name, makePart name fpart)
  makeVox pvox h1 h2 h3 = processVocal (RBFile.s_tempos song)
    (Vox.vocalToLegacy h1) (Vox.vocalToLegacy h2) (Vox.vocalToLegacy h3)
    $ fmap fromEnum (C.vocalKey pvox)
    <|> fmap (fromEnum . songKey) (C._key $ C._metadata songYaml)
  beat = processBeat (RBFile.s_tempos song)
    $ Beat.beatLines $ RBFile.onyxBeat $ RBFile.s_tracks song
  end = U.applyTempoMap (RBFile.s_tempos song) $ songLengthBeats song
  title  = fromMaybe "" $ C._title  $ C._metadata songYaml
  artist = fromMaybe "" $ C._artist $ C._metadata songYaml
  author = fromMaybe "" $ C._author $ C._metadata songYaml
  in A.encode $ Processed title artist author beat end parts
