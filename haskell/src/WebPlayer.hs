{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module WebPlayer
( makeDisplay
, showTimestamp
) where

import qualified Config                           as C
import           Control.Applicative              ((<|>))
import           Control.Monad                    (forM, guard)
import qualified Data.Aeson                       as A
import qualified Data.Aeson.Types                 as A
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (toLower)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Milli)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (sort)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe, listToMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Guitars
import qualified Numeric.NonNegative.Class        as NNC
import qualified RockBand.Beat                    as Beat
import           RockBand.Common                  (Difficulty (..),
                                                   LongNote (..), joinEdges,
                                                   splitEdges)
import qualified RockBand.Drums                   as Drums
import qualified RockBand.Events                  as Ev
import qualified RockBand.File                    as RBFile
import qualified RockBand.FiveButton              as Five
import qualified RockBand.GHL                     as GHL
import qualified RockBand.ProGuitar               as PG
import qualified RockBand.ProKeys                 as PK
import qualified RockBand.Vocals                  as Vox
import           Scripts                          (songLengthBeats_precodec)
import qualified Sound.MIDI.Util                  as U

class TimeFunctor f where
  mapTime :: (Real u) => (t -> u) -> f t -> f u

data Five t = Five
  { fiveNotes  :: Map.Map (Maybe Five.Color) (Map.Map t (LongNote Five.StrumHOPOTap ()))
  , fiveSolo   :: Map.Map t Bool
  , fiveEnergy :: Map.Map t Bool
  , fiveLanes  :: Map.Map (Maybe Five.Color) (Map.Map t Bool)
  , fiveBRE    :: Map.Map t Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor Five where
  mapTime f (Five n s e l b) = let
    g = Map.mapKeys f
    in Five (fmap g n) (g s) (g e) (fmap g l) (g b)

eventList :: (Real t) => Map.Map t a -> (a -> A.Value) -> A.Value
eventList evts f = A.toJSON $ map g $ Map.toAscList evts where
  g (secs, evt) = let
    secs' = A.Number $ realToFrac secs
    evt' = f evt
    in A.toJSON [secs', evt']

showHSTNote :: LongNote Five.StrumHOPOTap () -> A.Value
showHSTNote = \case
  NoteOff () -> "end"
  Blip Five.Tap () -> "tap"
  Blip Five.Strum () -> "strum"
  Blip Five.HOPO () -> "hopo"
  NoteOn Five.Tap () -> "tap-sust"
  NoteOn Five.Strum () -> "strum-sust"
  NoteOn Five.HOPO () -> "hopo-sust"

instance (Real t) => A.ToJSON (Five t) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ fiveNotes x) $ \(color, notes) ->
      (,) (maybe "open" (T.pack . map toLower . show) color) $ eventList notes showHSTNote
    , (,) "solo" $ eventList (fiveSolo x) A.toJSON
    , (,) "energy" $ eventList (fiveEnergy x) A.toJSON
    , (,) "lanes" $ A.object $ flip map (Map.toList $ fiveLanes x) $ \(color, lanes) ->
      (,) (maybe "open" (T.pack . map toLower . show) color) $ eventList lanes A.toJSON
    , (,) "bre" $ eventList (fiveBRE x) A.toJSON
    ]

_readEventList :: (Ord t, Fractional t) => (A.Value -> A.Parser a) -> A.Value -> A.Parser (Map.Map t a)
_readEventList f v = do
  dblValPairs <- A.parseJSON v
  fmap Map.fromList $ forM dblValPairs $ \(dbl, val) -> do
    let _ = dbl :: Double
    x <- f val
    return (realToFrac dbl, x)

_readKeyMapping :: (Ord k) => (T.Text -> A.Parser k) -> (A.Value -> A.Parser a) -> A.Value -> A.Parser (Map.Map k a)
_readKeyMapping readKey readVal = A.withObject "object with key->notes mapping" $ \obj -> do
  fmap Map.fromList $ forM (HM.toList obj) $ \(k, v) -> do
    key <- readKey k
    val <- readVal v
    return (key, val)

data Drums t = Drums
  { drumNotes  :: Map.Map t [Drums.Gem Drums.ProType]
  , drumSolo   :: Map.Map t Bool
  , drumEnergy :: Map.Map t Bool
  , drumLanes  :: Map.Map (Drums.Gem Drums.ProType) (Map.Map t Bool)
  , drumBRE    :: Map.Map t Bool
  , drumMode5  :: Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor Drums where
  mapTime f (Drums n s e l b m) = let
    g = Map.mapKeys f
    in Drums (g n) (g s) (g e) (fmap g l) (g b) m

drumGemKey :: Drums.Gem Drums.ProType -> T.Text
drumGemKey = \case
  Drums.Kick                          -> "kick"
  Drums.Red                           -> "red"
  Drums.Pro Drums.Yellow Drums.Cymbal -> "y-cym"
  Drums.Pro Drums.Yellow Drums.Tom    -> "y-tom"
  Drums.Pro Drums.Blue   Drums.Cymbal -> "b-cym"
  Drums.Pro Drums.Blue   Drums.Tom    -> "b-tom"
  Drums.Pro Drums.Green  Drums.Cymbal -> "g-cym"
  Drums.Pro Drums.Green  Drums.Tom    -> "g-tom"
  Drums.Orange                        -> "o-cym"

instance (Real t) => A.ToJSON (Drums t) where
  toJSON x = A.object
    [ (,) "notes" $ eventList (drumNotes x) $ A.toJSON . map (A.String . drumGemKey)
    , (,) "solo" $ eventList (drumSolo x) A.toJSON
    , (,) "energy" $ eventList (drumEnergy x) A.toJSON
    , (,) "lanes" $ A.object $ flip map (Map.toList $ drumLanes x) $ \(gem, lanes) ->
      (,) (drumGemKey gem) $ eventList lanes A.toJSON
    , (,) "bre" $ eventList (drumBRE x) A.toJSON
    , (,) "mode-5" $ A.toJSON (drumMode5 x)
    ]

data ProKeys t = ProKeys
  { proKeysNotes     :: Map.Map PK.Pitch (Map.Map t (LongNote () ()))
  , proKeysRanges    :: Map.Map t PK.LaneRange
  , proKeysSolo      :: Map.Map t Bool
  , proKeysEnergy    :: Map.Map t Bool
  , proKeysLanes     :: Map.Map PK.Pitch (Map.Map t Bool)
  , proKeysGlissando :: Map.Map t Bool
  , proKeysBRE       :: Map.Map t Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor ProKeys where
  mapTime f (ProKeys n r s e l gl b) = let
    g = Map.mapKeys f
    in ProKeys (fmap g n) (g r) (g s) (g e) (fmap g l) (g gl) (g b)

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

instance (Real t) => A.ToJSON (ProKeys t) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ proKeysNotes x) $ \(p, notes) ->
      (,) (showPitch p) $ eventList notes $ \case
        NoteOff () -> "end"
        Blip () () -> "note"
        NoteOn () () -> "sust"
    , (,) "ranges" $ eventList (proKeysRanges x) $ A.toJSON . map toLower . drop 5 . show
    , (,) "solo" $ eventList (proKeysSolo x) A.toJSON
    , (,) "energy" $ eventList (proKeysEnergy x) A.toJSON
    , (,) "lanes" $ A.object $ flip map (Map.toList $ proKeysLanes x) $ \(pitch, lanes) ->
      (,) (showPitch pitch) $ eventList lanes A.toJSON
    , (,) "gliss" $ eventList (proKeysGlissando x) A.toJSON
    , (,) "bre" $ eventList (proKeysBRE x) A.toJSON
    ]

data Protar t = Protar
  { protarNotes  :: Map.Map PG.GtrString (Map.Map t (LongNote (Five.StrumHOPOTap, Maybe PG.GtrFret) ()))
  , protarSolo   :: Map.Map t Bool
  , protarEnergy :: Map.Map t Bool
  , protarLanes  :: Map.Map PG.GtrString (Map.Map t Bool)
  , protarBRE    :: Map.Map t Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor Protar where
  mapTime f (Protar n s e l b) = let
    g = Map.mapKeys f
    in Protar (fmap g n) (g s) (g e) (fmap g l) (g b)

instance (Real t) => A.ToJSON (Protar t) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ protarNotes x) $ \(string, notes) ->
      (,) (T.pack $ map toLower $ show string) $ eventList notes $ A.String . \case
        NoteOff () -> "end"
        Blip (Five.Strum, fret) () -> "strum" <> showFret fret
        Blip (Five.HOPO, fret) () -> "hopo" <> showFret fret
        Blip (Five.Tap, fret) () -> "tap" <> showFret fret
        NoteOn (Five.Strum, fret) () -> "strum-sust" <> showFret fret
        NoteOn (Five.HOPO, fret) () -> "hopo-sust" <> showFret fret
        NoteOn (Five.Tap, fret) () -> "tap-sust" <> showFret fret
    , (,) "solo" $ eventList (protarSolo x) A.toJSON
    , (,) "energy" $ eventList (protarEnergy x) A.toJSON
    , (,) "lanes" $ A.object $ flip map (Map.toList $ protarLanes x) $ \(string, lanes) ->
      (,) (T.pack $ map toLower $ show string) $ eventList lanes A.toJSON
    , (,) "bre" $ eventList (protarBRE x) A.toJSON
    ] where showFret Nothing  = "-x"
            showFret (Just i) = "-" <> T.pack (show i)

data GHLLane
  = GHLSingle GHL.Fret
  | GHLBoth1
  | GHLBoth2
  | GHLBoth3
  | GHLOpen
  deriving (Eq, Ord, Show, Read)

data Six t = Six
  { sixNotes  :: Map.Map GHLLane (Map.Map t (LongNote Five.StrumHOPOTap ()))
  , sixSolo   :: Map.Map t Bool
  , sixEnergy :: Map.Map t Bool
  -- TODO lanes (not actually in CH)
  , sixBRE    :: Map.Map t Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor Six where
  mapTime f (Six n s e b) = let
    g = Map.mapKeys f
    in Six (fmap g n) (g s) (g e) (g b)

instance (Real t) => A.ToJSON (Six t) where
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

trackToMap :: (Ord a) => U.TempoMap -> RTB.T U.Beats a -> Map.Map U.Seconds a
trackToMap tmap = Map.fromList . ATB.toPairList . RTB.toAbsoluteEventList 0 . U.applyTempoTrack tmap . RTB.normalize

trackToBeatMap :: (Ord a) => RTB.T U.Beats a -> Map.Map U.Beats a
trackToBeatMap = Map.fromList . ATB.toPairList . RTB.toAbsoluteEventList 0 . RTB.normalize

filterKey :: (NNC.C t, Eq a) => a -> RTB.T t (LongNote s a) -> RTB.T t (LongNote s ())
filterKey k = RTB.mapMaybe $ mapM $ \x -> guard (k == x) >> return ()

findTremolos :: (Num t, Ord t) => Map.Map t [a] -> Map.Map t ((), (), Maybe t) -> Map.Map t (Bool, [a])
findTremolos ons trems = Map.fromList $ flip concatMap (Map.toAscList trems) $ \(start, (_, _, mlen)) -> case mlen of
  Nothing -> [] -- shouldn't happen (no blips)
  Just len -> case Map.lookupGE start ons of
    Nothing -> [] -- shouldn't happen (no notes under or after tremolo)
    Just (_, colors) -> [(start, (True, colors)), (start + len, (False, colors))]

findTrills :: (Num t, Ord t) => Map.Map t [a] -> Map.Map t ((), (), Maybe t) -> Map.Map t (Bool, [a])
findTrills ons trills = Map.fromList $ flip concatMap (Map.toAscList trills) $ \(start, (_, _, mlen)) -> case mlen of
  Nothing -> [] -- shouldn't happen (no blips)
  Just len -> case Map.lookupGE start ons of
    Nothing -> [] -- shouldn't happen (no notes under or after trill)
    Just (t1, cols1) -> case Map.lookupGT t1 ons of
      Nothing -> [] -- shouldn't happen (only one note under or after trill)
      Just (_, cols2) -> let
        colors = cols1 ++ cols2
        in [(start, (True, colors)), (start + len, (False, colors))]

processFive :: Maybe U.Beats -> U.TempoMap -> RTB.T U.Beats Five.Event -> Five U.Seconds
processFive hopoThreshold tmap trk = let
  expert = flip RTB.mapMaybe trk $ \case Five.DiffEvent Expert e -> Just e; _ -> Nothing
  assigned
    = case hopoThreshold of
      Nothing -> allStrums
      Just ht -> strumHOPOTap HOPOsRBGuitar ht
    $ openNotes expert
  getColor color = trackToMap tmap $ filterKey color assigned
  notes = Map.fromList $ do
    color <- Nothing : map Just [minBound .. maxBound]
    return (color, getColor color)
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Five.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Five.Overdrive b -> Just b; _ -> Nothing
  bre    = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Five.BRE       b -> Just b; _ -> Nothing
  ons    = trackToBeatMap $ RTB.collectCoincident $ flip RTB.mapMaybe assigned $ \case
    NoteOn _ col -> Just col
    Blip   _ col -> Just col
    _            -> Nothing
  trems  = findTremolos ons $ trackToBeatMap $ joinEdges $ flip RTB.mapMaybe trk $ \case
    Five.Tremolo b -> Just $ if b then NoteOn () () else NoteOff ()
    _              -> Nothing
  trills = findTrills ons $ trackToBeatMap $ joinEdges $ flip RTB.mapMaybe trk $ \case
    Five.Trill b -> Just $ if b then NoteOn () () else NoteOff ()
    _            -> Nothing
  lanesAll = Map.mapKeys (U.applyTempoMap tmap) $ Map.union trems trills
  lanes = Map.fromList $ do
    color <- Nothing : fmap Just [Five.Green .. Five.Orange]
    return $ (,) color $ flip Map.mapMaybe lanesAll $ \(b, colors) -> do
      guard $ elem color colors
      return b
  in Five notes solo energy lanes bre

processSix :: U.Beats -> U.TempoMap -> RTB.T U.Beats GHL.Event -> Six U.Seconds
processSix hopoThreshold tmap trk = let
  expert = flip RTB.mapMaybe trk $ \case GHL.DiffEvent Expert e -> Just e; _ -> Nothing
  assigned = strumHOPOTap HOPOsRBGuitar hopoThreshold $ ghlNotes expert
  oneTwoBoth x y = let
    dual = RTB.collectCoincident $ RTB.merge (fmap (const False) <$> x) (fmap (const True) <$> y)
    (both, notBoth) = flip RTB.partitionMaybe dual $ \case
      [a, b] | (() <$ a) == (() <$ b) -> Just $ () <$ a
      _                               -> Nothing
    notBoth' = RTB.flatten notBoth
    one = filterKey False notBoth'
    two = filterKey True  notBoth'
    in (one, two, both)
  (b1, w1, bw1) = oneTwoBoth (filterKey (Just GHL.Black1) assigned) (filterKey (Just GHL.White1) assigned)
  (b2, w2, bw2) = oneTwoBoth (filterKey (Just GHL.Black2) assigned) (filterKey (Just GHL.White2) assigned)
  (b3, w3, bw3) = oneTwoBoth (filterKey (Just GHL.Black3) assigned) (filterKey (Just GHL.White3) assigned)
  getLane = trackToMap tmap . \case
    GHLSingle GHL.Black1 -> b1
    GHLSingle GHL.Black2 -> b2
    GHLSingle GHL.Black3 -> b3
    GHLSingle GHL.White1 -> w1
    GHLSingle GHL.White2 -> w2
    GHLSingle GHL.White3 -> w3
    GHLBoth1 -> bw1
    GHLBoth2 -> bw2
    GHLBoth3 -> bw3
    GHLOpen -> filterKey Nothing assigned
  notes = Map.fromList $ do
    lane <- map GHLSingle [minBound .. maxBound] ++ [GHLBoth1, GHLBoth2, GHLBoth3, GHLOpen]
    return (lane, getLane lane)
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case GHL.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case GHL.Overdrive b -> Just b; _ -> Nothing
  bre    = Map.empty
  in Six notes solo energy bre

processDrums :: C.DrumMode -> U.TempoMap -> Maybe U.Beats -> RTB.T U.Beats Drums.Event -> Drums U.Seconds
processDrums mode tmap coda trk = let
  nonPro is5 = flip RTB.mapMaybe trk $ \case
    Drums.DiffEvent Expert (Drums.Note x) -> Just $ case x of
      Drums.Kick                -> Drums.Kick
      Drums.Red                 -> Drums.Red
      Drums.Pro Drums.Yellow () -> Drums.Pro Drums.Yellow $ if is5 then Drums.Cymbal else Drums.Tom
      Drums.Pro Drums.Blue   () -> Drums.Pro Drums.Blue Drums.Tom
      Drums.Orange              -> Drums.Orange
      Drums.Pro Drums.Green  () -> Drums.Pro Drums.Green Drums.Tom
    Drums.Kick2x                          -> Just Drums.Kick
    _                                     -> Nothing
  notes = fmap sort $ RTB.collectCoincident $ case mode of
    C.Drums4 -> nonPro False
    C.Drums5 -> nonPro True
    C.DrumsPro -> flip RTB.mapMaybe (Drums.assignToms True trk) $ \case
      (Expert, gem) -> Just gem
      _             -> Nothing
  notesS = trackToMap tmap notes
  notesB = trackToBeatMap notes
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Drums.Solo       b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Drums.Overdrive  b -> Just b; _ -> Nothing
  fills  = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Drums.Activation b -> Just b; _ -> Nothing
  bre    = case U.applyTempoMap tmap <$> coda of
    Nothing -> Map.empty
    Just c  -> Map.filterWithKey (\k _ -> k >= c) fills
  hands  = Map.filter (not . null) $ fmap (filter (/= Drums.Kick)) notesB
  singles = findTremolos hands $ trackToBeatMap $ joinEdges $ flip RTB.mapMaybe trk $ \case
    Drums.SingleRoll b -> Just $ if b then NoteOn () () else NoteOff ()
    _              -> Nothing
  doubles = findTrills hands $ trackToBeatMap $ joinEdges $ flip RTB.mapMaybe trk $ \case
    Drums.DoubleRoll b -> Just $ if b then NoteOn () () else NoteOff ()
    _            -> Nothing
  lanesAll = Map.mapKeys (U.applyTempoMap tmap) $ Map.union singles doubles
  lanes = Map.fromList $ do
    gem <-
      [ Drums.Kick, Drums.Red
      , Drums.Pro Drums.Yellow Drums.Cymbal, Drums.Pro Drums.Yellow Drums.Tom
      , Drums.Pro Drums.Blue   Drums.Cymbal, Drums.Pro Drums.Blue   Drums.Tom
      , Drums.Pro Drums.Green  Drums.Cymbal, Drums.Pro Drums.Green  Drums.Tom
      , Drums.Orange
      ]
    return $ (,) gem $ flip Map.mapMaybe lanesAll $ \(b, gems) -> do
      guard $ elem gem gems
      return b
  in Drums notesS solo energy lanes bre $ mode == C.Drums5

processProKeys :: U.TempoMap -> RTB.T U.Beats PK.Event -> ProKeys U.Seconds
processProKeys tmap trk = let
  notesForPitch p = trackToMap tmap $ flip RTB.mapMaybe trk $ \case
    PK.Note (NoteOff    p') -> guard (p == p') >> Just (NoteOff    ())
    PK.Note (Blip    () p') -> guard (p == p') >> Just (Blip    () ())
    PK.Note (NoteOn  () p') -> guard (p == p') >> Just (NoteOn  () ())
    _                    -> Nothing
  notes = Map.fromList [ (p, notesForPitch p) | p <- [minBound .. maxBound] ]
  ons = trackToBeatMap $ RTB.collectCoincident $ flip RTB.mapMaybe trk $ \case
    PK.Note (Blip   () p) -> Just p
    PK.Note (NoteOn () p) -> Just p
    _                     -> Nothing
  ranges = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.LaneShift r -> Just r; _ -> Nothing
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.Overdrive b -> Just b; _ -> Nothing
  trills = findTrills ons $ trackToBeatMap $ joinEdges $ flip RTB.mapMaybe trk $ \case
    PK.Trill b -> Just $ if b then NoteOn () () else NoteOff ()
    _          -> Nothing
  lanesAll = Map.mapKeys (U.applyTempoMap tmap) trills
  lanes = Map.fromList $ do
    key <- [minBound .. maxBound]
    return $ (,) key $ flip Map.mapMaybe lanesAll $ \(b, keys) -> do
      guard $ elem key keys
      return b
  gliss  = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.Glissando b -> Just b; _ -> Nothing
  bre    = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.BRE       b -> Just b; _ -> Nothing
  in ProKeys notes ranges solo energy lanes gliss bre

processProtar :: U.Beats -> U.TempoMap -> RTB.T U.Beats PG.Event -> Protar U.Seconds
processProtar hopoThreshold tmap trk = let
  expert = flip RTB.mapMaybe trk $ \case PG.DiffEvent Expert e -> Just e; _ -> Nothing
  assigned = expandColors $ PG.guitarifyHOPO hopoThreshold expert
  expandColors = splitEdges . RTB.flatten . fmap expandChord
  expandChord (shopo, gems, len) = do
    (str, fret, ntype) <- gems
    return ((shopo, guard (ntype /= PG.Muted) >> Just fret), str, len)
  getString string = trackToMap tmap $ flip RTB.mapMaybe assigned $ \case
    Blip   ntype s -> guard (s == string) >> Just (Blip   ntype ())
    NoteOn ntype s -> guard (s == string) >> Just (NoteOn ntype ())
    NoteOff      s -> guard (s == string) >> Just (NoteOff      ())
  notes = Map.fromList $ do
    string <- [minBound .. maxBound]
    return (string, getString string)
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PG.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PG.Overdrive b -> Just b; _ -> Nothing
  -- for protar we can treat tremolo/trill identically;
  -- in both cases it's just "get the strings the first note/chord is on"
  onStrings = trackToBeatMap $ RTB.collectCoincident $ flip RTB.mapMaybe trk $ \case
    PG.DiffEvent Expert (PG.Note (NoteOn _ (s, _))) -> Just s
    PG.DiffEvent Expert (PG.Note (Blip   _ (s, _))) -> Just s
    _                                               -> Nothing
  lanesAll = Map.mapKeys (U.applyTempoMap tmap) $ findTremolos onStrings $ trackToBeatMap $ joinEdges $ flip RTB.mapMaybe trk $ \case
    PG.Tremolo b -> Just $ if b then NoteOn () () else NoteOff ()
    PG.Trill   b -> Just $ if b then NoteOn () () else NoteOff ()
    _            -> Nothing
  lanes = Map.fromList $ do
    str <- [minBound .. maxBound]
    return $ (,) str $ flip Map.mapMaybe lanesAll $ \(b, strs) -> do
      guard $ elem str strs
      return b
  bre    = trackToMap tmap $ flip RTB.mapMaybe trk $ \case
    PG.BREGuitar b -> Just b
    PG.BREBass   b -> Just b
    _              -> Nothing
  in Protar notes solo energy lanes bre

newtype Beats t = Beats
  { beatLines :: Map.Map t Beat
  } deriving (Eq, Ord, Show)

instance TimeFunctor Beats where
  mapTime f (Beats x) = Beats $ Map.mapKeys f x

instance (Real t) => A.ToJSON (Beats t) where
  toJSON x = A.object
    [ (,) "lines" $ eventList (beatLines x) $ A.toJSON . map toLower . show
    ]

data Beat
  = Bar
  | Beat
  | HalfBeat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

processBeat :: U.TempoMap -> RTB.T U.Beats Beat.Event -> Beats U.Seconds
processBeat tmap rtb = Beats $ Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0
  $ U.applyTempoTrack tmap $ flip fmap rtb $ \case
    Beat.Bar -> Bar
    Beat.Beat -> Beat
    -- TODO: add half-beats

data Vocal t = Vocal
  { harm1Notes      :: Map.Map t VocalNote
  , harm2Notes      :: Map.Map t VocalNote
  , harm3Notes      :: Map.Map t VocalNote
  , vocalPercussion :: Map.Map t ()
  , vocalPhraseEnds :: Map.Map t ()
  , vocalRanges     :: Map.Map t VocalRange
  , vocalEnergy     :: Map.Map t Bool
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

instance (Real t) => A.ToJSON (Vocal t) where
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

instance TimeFunctor Vocal where
  mapTime f (Vocal h1 h2 h3 perc ends ranges energy tonic) = Vocal
    (Map.mapKeys f h1)
    (Map.mapKeys f h2)
    (Map.mapKeys f h3)
    (Map.mapKeys f perc)
    (Map.mapKeys f ends)
    (Map.mapKeys f ranges)
    (Map.mapKeys f energy)
    tonic

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
  perc = trackToMap tmap $ flip RTB.mapMaybe h1 $ \case
    Vox.Percussion -> Just ()
    _              -> Nothing
  ends = trackToMap tmap $ flip RTB.mapMaybe h1 $ \case
    Vox.Phrase False  -> Just ()
    Vox.Phrase2 False -> Just ()
    _                 -> Nothing
  pitchToInt p = fromEnum p + 36
  makeVoxPart trk = trackToMap tmap $ flip rtbMapMaybeWithAbsoluteTime (RTB.collectCoincident trk) $ \bts evts -> let
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
  ranges = Map.singleton 0 $ VocalRange (foldr min 84 allPitches) (foldr max 36 allPitches)
  allPitches = [ p | VocalStart _ (Just p) <- concatMap Map.elems [harm1, harm2, harm3] ]
  in Vocal
    { vocalPercussion = perc
    , vocalPhraseEnds = ends
    , vocalTonic = tonic
    , harm1Notes = harm1
    , harm2Notes = harm2
    , harm3Notes = harm3
    , vocalEnergy = trackToMap tmap $ flip RTB.mapMaybe h1 $ \case
      Vox.Overdrive b -> Just b
      _               -> Nothing
    , vocalRanges = ranges
    }

data Flex t = Flex
  { flexFive    :: Maybe (Five    t)
  , flexSix     :: Maybe (Six     t)
  , flexDrums   :: Maybe (Drums   t)
  , flexProKeys :: Maybe (ProKeys t)
  , flexProtar  :: Maybe (Protar  t)
  , flexVocal   :: Maybe (Vocal   t)
  } deriving (Eq, Ord, Show)

instance TimeFunctor Flex where
  mapTime f (Flex f5 f6 fd fpk fpt fv) = Flex
    (fmap (mapTime f) f5)
    (fmap (mapTime f) f6)
    (fmap (mapTime f) fd)
    (fmap (mapTime f) fpk)
    (fmap (mapTime f) fpt)
    (fmap (mapTime f) fv)

instance (Real t) => A.ToJSON (Flex t) where
  toJSON flex = A.object $ concat
    [ case flexFive    flex of Nothing -> []; Just x -> [("five"   , A.toJSON x)]
    , case flexSix     flex of Nothing -> []; Just x -> [("six"    , A.toJSON x)]
    , case flexDrums   flex of Nothing -> []; Just x -> [("drums"  , A.toJSON x)]
    , case flexProKeys flex of Nothing -> []; Just x -> [("prokeys", A.toJSON x)]
    , case flexProtar  flex of Nothing -> []; Just x -> [("protar" , A.toJSON x)]
    , case flexVocal   flex of Nothing -> []; Just x -> [("vocal"  , A.toJSON x)]
    ]

data Processed t = Processed
  { processedTitle  :: T.Text
  , processedArtist :: T.Text
  , processedBeats  :: Beats t
  , processedEnd    :: t
  , processedParts  :: [(T.Text, Flex t)]
  } deriving (Eq, Ord, Show)

instance TimeFunctor Processed where
  mapTime f (Processed title artist bts end parts) = Processed title artist
    (mapTime f bts)
    (f end)
    (map (fmap $ mapTime f) parts)

instance (Real t) => A.ToJSON (Processed t) where
  toJSON proc = A.object $ concat
    [ [("title" , A.toJSON $              processedTitle  proc)]
    , [("artist", A.toJSON $              processedArtist proc)]
    , [("beats" , A.toJSON $              processedBeats  proc)]
    , [("end"   , A.Number $ realToFrac $ processedEnd    proc)]
    , [("parts" , A.toJSON $              processedParts  proc)]
    ]

makeDisplay :: C.SongYaml -> RBFile.Song (RBFile.OnyxFile U.Beats) -> BL.ByteString
makeDisplay songYaml song = let
  ht n = fromIntegral n / 480
  coda = fmap (fst . fst) $ RTB.viewL $ RTB.filter (== Ev.Coda) $ RBFile.onyxEvents $ RBFile.s_tracks song
  makePart name fpart = Flex
    { flexFive = flip fmap (C.partGRYBO fpart) $ \grybo -> processFive
      (guard (not $ RBFile.flexFiveIsKeys tracks) >> Just (ht $ C.gryboHopoThreshold grybo))
      (RBFile.s_tempos song)
      (RBFile.flexFiveButton tracks)
    , flexSix = flip fmap (C.partGHL fpart) $ \ghl -> processSix (ht $ C.ghlHopoThreshold ghl) (RBFile.s_tempos song) (RBFile.flexGHL tracks)
    , flexDrums = flip fmap (C.partDrums fpart) $ \pd -> processDrums (C.drumsMode pd) (RBFile.s_tempos song) coda (RBFile.flexPartDrums tracks)
    , flexProKeys = flip fmap (C.partProKeys fpart) $ \_ -> processProKeys (RBFile.s_tempos song) (RBFile.flexPartRealKeysX tracks)
    , flexProtar = flip fmap (C.partProGuitar fpart) $ \pg -> processProtar (ht $ C.pgHopoThreshold pg) (RBFile.s_tempos song)
      $ let mustang = RBFile.flexPartRealGuitar tracks
            squier  = RBFile.flexPartRealGuitar22 tracks
        in if RTB.null squier then mustang else squier
    , flexVocal = flip fmap (C.partVocal fpart) $ \pvox -> case C.vocalCount pvox of
      C.Vocal3 -> makeVox
        (RBFile.flexHarm1 tracks)
        (RBFile.flexHarm2 tracks)
        (RBFile.flexHarm3 tracks)
      C.Vocal2 -> makeVox
        (RBFile.flexHarm1 tracks)
        (RBFile.flexHarm2 tracks)
        RTB.empty
      C.Vocal1 -> makeVox
        (RBFile.flexPartVocals $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song)
        RTB.empty
        RTB.empty
    } where
      tracks = RBFile.getFlexPart name $ RBFile.s_tracks song
  parts = do
    (name, fpart) <- sort $ HM.toList $ C.getParts $ C._parts songYaml
    return (RBFile.getPartName name, makePart name fpart)
  makeVox h1 h2 h3 = processVocal (RBFile.s_tempos song) h1 h2 h3 (fmap (fromEnum . C.songKey) $ C._key $ C._metadata songYaml)
  beat = processBeat (RBFile.s_tempos song)
    $ RBFile.onyxBeat $ RBFile.s_tracks song
  end = U.applyTempoMap (RBFile.s_tempos song) $ songLengthBeats_precodec song
  title  = fromMaybe "" $ C._title  $ C._metadata songYaml
  artist = fromMaybe "" $ C._artist $ C._metadata songYaml
  in A.encode $ mapTime (realToFrac :: U.Seconds -> Milli) $ Processed title artist beat end parts
