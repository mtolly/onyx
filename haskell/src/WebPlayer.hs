{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module WebPlayer
( makeDisplay
, showTimestamp
, findTremolos, findTrills, laneDifficulty
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
import qualified Data.Text                        as T
import           Guitars
import qualified Numeric.NonNegative.Class        as NNC
import qualified Numeric.NonNegative.Wrapper      as NN
import qualified PhaseShift.Dance                 as Dance
import qualified RockBand.Codec.Beat              as Beat
import qualified RockBand.Codec.Drums             as D
import           RockBand.Codec.Events
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as Five
import qualified RockBand.Codec.ProGuitar         as PG
import           RockBand.Codec.ProKeys           as PK
import qualified RockBand.Codec.Six               as GHL
import           RockBand.Common                  (Difficulty (..), Edge (..),
                                                   LaneDifficulty (..),
                                                   LongNote (..), SongKey (..),
                                                   StrumHOPOTap (..),
                                                   edgeBlipsRB_,
                                                   joinEdgesSimple,
                                                   songKeyUsesFlats, splitEdges)
import qualified RockBand.Legacy.Vocal            as Vox
import           RockBand.Sections                (makePSSection)
import qualified Sound.MIDI.Util                  as U
import           Text.Transform                   (showTimestamp)

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
  { drumNotes  :: RTB.T t [D.RealDrum]
  , drumSolo   :: RTB.T t Bool
  , drumEnergy :: RTB.T t Bool
  , drumLanes  :: Map.Map D.RealDrum (RTB.T t Bool)
  , drumBRE    :: RTB.T t Bool
  , drumMode   :: C.DrumMode
  , drumDisco  :: RTB.T t Bool
  } deriving (Eq, Ord, Show)

drumGemChar :: D.RealDrum -> Char
drumGemChar = \case
  Right rb -> case rb of
    D.Kick                  -> 'k'
    D.Red                   -> 'r'
    D.Pro D.Yellow D.Cymbal -> 'Y'
    D.Pro D.Yellow D.Tom    -> 'y'
    D.Pro D.Blue   D.Cymbal -> 'B'
    D.Pro D.Blue   D.Tom    -> 'b'
    D.Pro D.Green  D.Cymbal -> 'G'
    D.Pro D.Green  D.Tom    -> 'g'
    D.Orange                -> 'O'
  Left ps -> case ps of
    D.Rimshot  -> 'R'
    D.HHOpen   -> 'H'
    D.HHSizzle -> 'h'
    D.HHPedal  -> 'p'

instance A.ToJSON (Drums U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ eventList (drumNotes x) $ A.toJSON . map drumGemChar
    , (,) "solo" $ eventList (drumSolo x) A.toJSON
    , (,) "energy" $ eventList (drumEnergy x) A.toJSON
    , (,) "lanes" $ A.object $ flip map (Map.toList $ drumLanes x) $ \(gem, lanes) ->
      (,) (T.singleton $ drumGemChar gem) $ eventList lanes A.toJSON
    , (,) "bre" $ eventList (drumBRE x) A.toJSON
    , (,) "mode" $ case drumMode x of
      C.Drums4    -> "4"
      C.Drums5    -> "5"
      C.DrumsPro  -> "pro"
      C.DrumsReal -> "real"
    , (,) "disco" $ eventList (drumDisco x) A.toJSON
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
  { protarNotes    :: Map.Map PG.GtrString (RTB.T t (LongNote (StrumHOPOTap, Maybe PG.GtrFret, Bool, Maybe PG.Slide) ()))
  , protarSolo     :: RTB.T t Bool
  , protarEnergy   :: RTB.T t Bool
  , protarLanes    :: Map.Map PG.GtrString (RTB.T t Bool)
  , protarBRE      :: RTB.T t Bool
  , protarChords   :: RTB.T t (LongNote T.Text ())
  , protarArpeggio :: RTB.T t Bool
  , protarStrings  :: Int
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Protar U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ protarNotes x) $ \(string, notes) ->
      (,) (T.pack $ map toLower $ show string) $ eventList notes $ A.String . \case
        NoteOff () -> "e"
        Blip (sht, fret, phantom, _) () -> T.concat
          [ case sht of Strum -> "s"; HOPO -> "h"; Tap -> "t"
          , if phantom then "p" else ""
          , showFret fret
          ]
        NoteOn (sht, fret, phantom, slide) () -> T.concat
          [ case sht of Strum -> "S"; HOPO -> "H"; Tap -> "T"
          , case slide of Nothing -> ""; Just PG.SlideUp -> "u"; Just PG.SlideDown -> "d"
          , if phantom then "p" else ""
          , showFret fret
          ]
    , (,) "solo" $ eventList (protarSolo x) A.toJSON
    , (,) "energy" $ eventList (protarEnergy x) A.toJSON
    , (,) "lanes" $ A.object $ flip map (Map.toList $ protarLanes x) $ \(string, lanes) ->
      (,) (T.pack $ map toLower $ show string) $ eventList lanes A.toJSON
    , (,) "bre" $ eventList (protarBRE x) A.toJSON
    , (,) "chords" $ eventList (protarChords x) $ A.String . \case
      NoteOff     () -> "e"
      Blip   name () -> "c:" <> name
      NoteOn name () -> "C:" <> name
    , (,) "arpeggio" $ eventList (protarArpeggio x) A.toJSON
    , (,) "strings" $ A.toJSON $ protarStrings x
    ] where showFret Nothing  = "x"
            showFret (Just i) = T.pack (show i)

data GHLLane
  = GHLSingle GHL.Fret
  | GHLBoth1
  | GHLBoth2
  | GHLBoth3
  | GHLOpen
  deriving (Eq, Ord, Show)

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
filterKey k = RTB.mapMaybe $ mapM $ \x -> guard $ k == x

findTremolos :: (Num t, NNC.C t) => RTB.T t a -> RTB.T t t -> RTB.T t (a, t)
findTremolos ons = let
  ons' = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList NNC.zero $ RTB.collectCoincident ons
  in  RTB.fromAbsoluteEventList
    . ATB.fromPairList
    . concatMap (\(time, len) -> map (\fret -> (time, (fret, len))) $ maybe [] snd $ Map.lookupGE time ons')
    . ATB.toPairList
    . RTB.toAbsoluteEventList NNC.zero

findTrills :: (Num t, NNC.C t) => RTB.T t a -> RTB.T t t -> RTB.T t (a, t)
findTrills ons = let
  ons' = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList NNC.zero $ RTB.collectCoincident ons
  in  RTB.fromAbsoluteEventList
    . ATB.fromPairList
    . concatMap (\(time, len) -> case Map.lookupGE time ons' of
      Nothing -> [] -- no notes under or after trill
      Just (t1, cols1) -> case Map.lookupGT t1 ons' of
        Nothing -> [] -- only one note under or after trill
        Just (_, cols2) -> map (\fret -> (time, (fret, len))) $ cols1 ++ cols2
      )
    . ATB.toPairList
    . RTB.toAbsoluteEventList NNC.zero

makeDifficulties :: (Difficulty -> Maybe (a t)) -> Difficulties a t
makeDifficulties f = Difficulties $ catMaybes $ do
  (d, name) <- [(Expert, "X"), (Hard, "H"), (Medium, "M"), (Easy, "E")]
  return $ (name,) <$> f d

makeDrumDifficulties :: (Maybe Difficulty -> Maybe (a t)) -> Difficulties a t
makeDrumDifficulties f = Difficulties $ catMaybes $ do
  (d, name) <- [(Nothing, "X+"), (Just Expert, "X"), (Just Hard, "H"), (Just Medium, "M"), (Just Easy, "E")]
  return $ (name,) <$> f d

makeDanceDifficulties :: (Dance.SMDifficulty -> Maybe (a t)) -> Difficulties a t
makeDanceDifficulties f = Difficulties $ catMaybes $ do
  (d, name) <-
    [ (Dance.SMChallenge, "C")
    , (Dance.SMHard, "H")
    , (Dance.SMMedium, "M")
    , (Dance.SMEasy, "E")
    , (Dance.SMBeginner, "B")
    ]
  return $ (name,) <$> f d

laneDifficulty :: (NNC.C t) => Difficulty -> RTB.T t (Maybe LaneDifficulty) -> RTB.T t t
laneDifficulty Expert lanes = fmap (\(_, _, len) -> len) $ joinEdgesSimple $ maybe (EdgeOff ()) (`EdgeOn` ()) <$> lanes
laneDifficulty Hard   lanes
  = RTB.mapMaybe (\(d, (), len) -> guard (d == LaneHard) >> Just len)
  $ joinEdgesSimple $ maybe (EdgeOff ()) (`EdgeOn` ()) <$> lanes
laneDifficulty _      _     = RTB.empty

splitEdgesBool :: (NNC.C t) => RTB.T t t -> RTB.T t Bool
splitEdgesBool = U.trackJoin . fmap (\len -> RTB.fromPairList [(NNC.zero, True), (len, False)])

processFive :: HOPOsAlgorithm -> U.Beats -> U.TempoMap -> Five.FiveTrack U.Beats -> Difficulties Five U.Seconds
processFive algo hopoThreshold tmap trk = makeDifficulties $ \diff -> let
  thisDiff = fromMaybe mempty $ Map.lookup diff $ Five.fiveDifficulties trk
  assigned
    = applyForces (getForces5 thisDiff)
    $ strumHOPOTap' algo hopoThreshold
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
  ons    = RTB.normalize $ fmap (fst . fst) assigned
  trems  = findTremolos ons $ RTB.normalize $ laneDifficulty diff $ Five.fiveTremolo trk
  trills = findTrills ons $ RTB.normalize $ laneDifficulty diff $ Five.fiveTrill trk
  lanesAll = RTB.merge trems trills
  lanes = Map.fromList $ do
    color <- Nothing : fmap Just [Five.Green .. Five.Orange]
    return $ (,) color $ U.applyTempoTrack tmap $ splitEdgesBool $ flip RTB.mapMaybe lanesAll $ \(color', len) -> do
      guard $ color == color'
      return len
  in guard (not $ RTB.null $ Five.fiveGems thisDiff) >> Just (Five notes solo energy lanes bre)

processSix :: U.Beats -> U.TempoMap -> GHL.SixTrack U.Beats -> Difficulties Six U.Seconds
processSix hopoThreshold tmap trk = makeDifficulties $ \diff -> let
  thisDiff = fromMaybe mempty $ Map.lookup diff $ GHL.sixDifficulties trk
  assigned :: RTB.T U.Beats ((Maybe GHL.Fret, StrumHOPOTap), Maybe U.Beats)
  assigned
    = applyForces (getForces6 thisDiff)
    $ strumHOPOTap' HOPOsRBGuitar hopoThreshold $ edgeBlipsRB_ $ GHL.sixGems thisDiff
  onlyKey :: (Eq a) => a -> RTB.T U.Beats ((a, b), c) -> RTB.T U.Beats (((), b), c)
  onlyKey fret trips = flip RTB.mapMaybe trips $ \case
    ((fret', sht), mlen) -> guard (fret' == fret) >> Just (((), sht), mlen)
  oneTwoBoth x y = let
    replaceFret b ((_, sht), mlen) = ((b, sht), mlen)
    dual = RTB.collectCoincident $ RTB.merge (replaceFret False <$> x) (replaceFret True <$> y)
    (both, notBoth) = flip RTB.partitionMaybe dual $ \case
      [a, b] | replaceFret () a == replaceFret () b -> Just $ replaceFret () a
      _                                             -> Nothing
    notBoth' = RTB.flatten notBoth
    one = onlyKey False notBoth'
    two = onlyKey True  notBoth'
    in (one, two, both)
  (b1, w1, bw1) = oneTwoBoth (onlyKey (Just GHL.Black1) assigned) (onlyKey (Just GHL.White1) assigned)
  (b2, w2, bw2) = oneTwoBoth (onlyKey (Just GHL.Black2) assigned) (onlyKey (Just GHL.White2) assigned)
  (b3, w3, bw3) = oneTwoBoth (onlyKey (Just GHL.Black3) assigned) (onlyKey (Just GHL.White3) assigned)
  toEdges = splitEdges . fmap (\((fret, sht), mlen) -> (sht, fret, mlen))
  getLane = realTrack tmap . toEdges . \case
    GHLSingle GHL.Black1 -> b1
    GHLSingle GHL.Black2 -> b2
    GHLSingle GHL.Black3 -> b3
    GHLSingle GHL.White1 -> w1
    GHLSingle GHL.White2 -> w2
    GHLSingle GHL.White3 -> w3
    GHLBoth1 -> bw1
    GHLBoth2 -> bw2
    GHLBoth3 -> bw3
    GHLOpen -> onlyKey Nothing assigned
  notes = Map.fromList $ do
    lane <- map GHLSingle [minBound .. maxBound] ++ [GHLBoth1, GHLBoth2, GHLBoth3, GHLOpen]
    return (lane, getLane lane)
  solo   = realTrack tmap $ GHL.sixSolo      trk
  energy = realTrack tmap $ GHL.sixOverdrive trk
  bre    = RTB.empty
  in guard (not $ RTB.null $ GHL.sixGems thisDiff) >> Just (Six notes solo energy bre)

processDrums :: C.DrumMode -> U.TempoMap -> Maybe U.Beats -> D.DrumTrack U.Beats -> D.DrumTrack U.Beats -> Difficulties Drums U.Seconds
processDrums mode tmap coda trk1x trk2x = makeDrumDifficulties $ \diff -> let
  -- TODO if only 2x kicks charted, label difficulty as X+ instead of X
  has2x = all (not . D.nullDrums) [trk1x, trk2x] || any (not . RTB.null . D.drumKick2x) [trk1x, trk2x]
  trk = case (D.nullDrums trk1x, D.nullDrums trk2x, diff) of
    (True , _    , _      ) -> trk2x
    (_    , True , _      ) -> trk1x
    (False, False, Nothing) -> trk2x
    (False, False, Just _ ) -> trk1x
  drumDiff = Map.lookup (fromMaybe Expert diff) $ D.drumDifficulties trk
  nonPro is5 = (if diff == Nothing then RTB.merge $ const D.Kick <$> D.drumKick2x trk else id)
    $ flip fmap (maybe RTB.empty D.drumGems drumDiff) $ \case
      D.Kick            -> D.Kick
      D.Red             -> D.Red
      D.Pro D.Yellow () -> D.Pro D.Yellow $ if is5 then D.Cymbal else D.Tom
      D.Pro D.Blue   () -> D.Pro D.Blue D.Tom
      D.Orange          -> D.Orange
      D.Pro D.Green  () -> D.Pro D.Green D.Tom
  notes = fmap sort $ RTB.collectCoincident $ case mode of
    C.Drums4    -> fmap Right $ nonPro False
    C.Drums5    -> fmap Right $ nonPro True
    C.DrumsPro  -> fmap Right $ D.computePro diff trk
    C.DrumsReal -> D.computePSReal diff trk
  notesS = realTrack tmap notes
  notesB = RTB.normalize notes
  solo   = realTrack tmap $ D.drumSolo trk
  energy = realTrack tmap $ D.drumOverdrive trk
  fills  = realTrack tmap $ D.drumActivation trk
  bre    = case U.applyTempoMap tmap <$> coda of
    Nothing -> RTB.empty
    Just c  -> RTB.delay c $ U.trackDrop c fills
  hands  = RTB.filter (`notElem` [Right D.Kick, Left D.HHPedal]) $ RTB.flatten notesB
  singles = findTremolos hands $ RTB.normalize $ laneDifficulty (fromMaybe Expert diff) $ D.drumSingleRoll trk
  doubles = findTrills   hands $ RTB.normalize $ laneDifficulty (fromMaybe Expert diff) $ D.drumDoubleRoll trk
  lanesAll = RTB.merge singles doubles
  lanes = Map.fromList $ do
    gem <- map Right
      [ D.Kick, D.Red
      , D.Pro D.Yellow D.Cymbal, D.Pro D.Yellow D.Tom
      , D.Pro D.Blue   D.Cymbal, D.Pro D.Blue   D.Tom
      , D.Pro D.Green  D.Cymbal, D.Pro D.Green  D.Tom
      , D.Orange
      ] ++ map Left [D.Rimshot, D.HHOpen, D.HHSizzle, D.HHPedal]
    return $ (,) gem $ U.applyTempoTrack tmap $ splitEdgesBool $ flip RTB.mapMaybe lanesAll $ \(gem', len) -> do
      guard $ gem == gem'
      return len
  disco = realTrack tmap $ maybe RTB.empty (fmap ((== D.Disco) . snd) . D.drumMix) drumDiff
  in do
    guard $ not $ RTB.null notes
    guard $ diff /= Nothing || has2x
    Just $ Drums notesS solo energy lanes bre mode disco

processProKeys :: U.TempoMap -> ProKeysTrack U.Beats -> Maybe (ProKeys U.Seconds)
processProKeys tmap trk = let
  joined = edgeBlipsRB_ $ pkNotes trk
  assigned' = U.trackJoin $ flip fmap joined $ \(p, mlen) -> case mlen of
    Nothing  -> RTB.singleton 0 $ Blip () p
    Just len -> RTB.fromPairList [(0, NoteOn () p), (len, NoteOff p)]
  notesForPitch p = realTrack tmap $ filterKey p assigned'
  notes = Map.fromList [ (p, notesForPitch p) | p <- [minBound .. maxBound] ]
  ons = RTB.normalize $ fmap fst joined
  ranges = realTrack tmap $ pkLanes trk
  solo   = realTrack tmap $ pkSolo trk
  energy = realTrack tmap $ pkOverdrive trk
  trills = findTrills ons $ RTB.normalize $ laneDifficulty Expert
    $ (\b -> guard b >> Just LaneExpert) <$> pkTrill trk
  lanes = Map.fromList $ do
    key <- [minBound .. maxBound]
    return $ (,) key $ U.applyTempoTrack tmap $ splitEdgesBool $ flip RTB.mapMaybe trills $ \(key', len) -> do
      guard $ key == key'
      return len
  gliss  = realTrack tmap $ pkGlissando trk
  bre    = realTrack tmap $ pkBRE trk
  in guard (not $ RTB.null $ pkNotes trk) >> Just (ProKeys notes ranges solo energy lanes gliss bre)

processProtar :: U.Beats -> PG.GtrTuning -> Bool -> U.TempoMap -> PG.ProGuitarTrack U.Beats -> Difficulties Protar U.Seconds
processProtar hopoThreshold tuning defaultFlat tmap pg = makeDifficulties $ \diff -> let
  thisDiff = fromMaybe mempty $ Map.lookup diff $ PG.pgDifficulties pg
  assigned = expandColors $ PG.guitarifyFull hopoThreshold thisDiff
  expandColors = splitEdges . RTB.flatten . fmap expandChord
  expandChord (shopo, gems, len) = do
    (str, fret, ntype) <- gems
    return ((shopo, guard (ntype /= PG.Muted) >> Just fret, ntype == PG.ArpeggioForm, len >>= snd), str, fmap fst len)
  getString string = realTrack tmap $ flip RTB.mapMaybe assigned $ \case
    Blip   ntype s -> guard (s == string) >> Just (Blip   ntype ())
    NoteOn ntype s -> guard (s == string) >> Just (NoteOn ntype ())
    NoteOff      s -> guard (s == string) >> Just (NoteOff      ())
  protarNotes = Map.fromList $ do
    string <- [minBound .. maxBound]
    return (string, getString string)
  protarSolo = realTrack tmap $ PG.pgSolo pg
  protarEnergy = realTrack tmap $ PG.pgOverdrive pg
  -- for protar we can treat tremolo/trill identically;
  -- in both cases it's just "get the strings the first note/chord is on"
  onStrings = RTB.normalize $ flip RTB.mapMaybe (PG.pgNotes thisDiff) $ \case
    EdgeOn _ (str, _) -> Just str
    EdgeOff _         -> Nothing
  lanesAll = findTremolos onStrings $ RTB.normalize
    $ laneDifficulty diff $ RTB.merge (PG.pgTremolo pg) (PG.pgTrill pg)
  protarLanes = Map.fromList $ do
    str <- [minBound .. maxBound]
    return $ (,) str $ U.applyTempoTrack tmap $ splitEdgesBool $ flip RTB.mapMaybe lanesAll $ \(str', len) -> do
      guard $ str == str'
      return len
  protarBRE = realTrack tmap $ fmap snd $ PG.pgBRE pg
  usedStrings = nubOrd $ toList (PG.pgDifficulties pg) >>= toList . PG.pgNotes >>= map fst . toList
  pitches = PG.tuningPitches tuning { PG.gtrGlobal = 0 }
  protarStrings
    | not (null $ drop 7 pitches) || elem PG.S8 usedStrings = 8
    | not (null $ drop 6 pitches) || elem PG.S7 usedStrings = 7
    | not (null $ drop 5 pitches) || elem PG.S1 usedStrings = 6
    | not (null $ drop 4 pitches) || elem PG.S2 usedStrings = 5
    | not (null $ drop 3 pitches) || elem PG.S3 usedStrings = 4
    | otherwise                                             = 3
  protarChords = realTrack tmap $ PG.computeChordNames diff pitches defaultFlat pg
  protarArpeggio = realTrack tmap $ PG.pgArpeggio thisDiff
  in guard (not $ RTB.null $ PG.pgNotes thisDiff) >> Just Protar{..}

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
  deriving (Eq, Ord, Show, Enum, Bounded)

processBeat :: U.TempoMap -> RTB.T U.Beats Beat.BeatEvent -> Beats U.Seconds
processBeat tmap rtb = Beats $ U.applyTempoTrack tmap $ flip fmap rtb $ \case
  Beat.Bar -> Bar
  Beat.Beat -> Beat
  -- TODO: add half-beats

data Vocal t = Vocal
  { harm1Notes       :: RTB.T t VocalNote
  , harm2Notes       :: RTB.T t VocalNote
  , harm3Notes       :: RTB.T t VocalNote
  , vocalPercussion  :: RTB.T t ()
  , vocalPhraseEnds1 :: RTB.T t ()
  , vocalPhraseEnds2 :: RTB.T t ()
  , vocalRanges      :: RTB.T t VocalRange
  , vocalEnergy      :: RTB.T t Bool
  , vocalTonic       :: Maybe Int
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
    , (,) "phrases1" $ eventList (vocalPhraseEnds1 x) $ \() -> A.Null
    , (,) "phrases2" $ eventList (vocalPhraseEnds2 x) $ \() -> A.Null
    , (,) "ranges" $ eventList (vocalRanges x) $ \case
      VocalRange pmin pmax -> A.toJSON [pmin, pmax]
      VocalRangeShift      -> A.Null
    , (,) "energy" $ eventList (vocalEnergy x) A.toJSON
    , (,) "tonic" $ maybe A.Null A.toJSON $ vocalTonic x
    ] where voxEvent VocalEnd                 = A.Null
            voxEvent (VocalStart lyric pitch) = A.toJSON [A.toJSON lyric, maybe A.Null A.toJSON pitch]

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
  getPhraseEnds = RTB.mapMaybe $ \case
    Vox.Phrase False  -> Just ()
    Vox.Phrase2 False -> Just ()
    _                 -> Nothing
  pitchToInt p = fromEnum p + 36
  makeVoxPart trk = realTrack tmap $ flip RTB.mapMaybe (RTB.collectCoincident trk) $ \evts -> let
    lyric = listToMaybe [ s | Vox.Lyric s <- evts ]
    note = listToMaybe [ p | Vox.Note True p <- evts ]
    end = listToMaybe [ () | Vox.Note False _ <- evts ]
    in case (lyric, note, end) of
      -- Note: the _ in the first pattern below should be Nothing,
      -- but we allow Just () for sloppy vox charts with no gap between notes
      (ml, Just p, _) -> let
        l = fromMaybe "" ml
        in Just $ case T.stripSuffix "#" l <|> T.stripSuffix "^" l of
          Nothing -> case T.stripSuffix "#$" l <|> T.stripSuffix "^$" l of
            Nothing -> VocalStart l $ Just $ pitchToInt p -- non-talky
            Just l' -> VocalStart (l' <> "$") Nothing     -- hidden lyric talky
          Just l' -> VocalStart l' Nothing                -- talky
      (Nothing, Nothing, Just ()) -> Just VocalEnd
      (Nothing, Nothing, Nothing) -> Nothing
      (Just l, Nothing, _) -> Just $ VocalStart l Nothing -- shouldn't happen
  harm1 = makeVoxPart h1
  harm2 = makeVoxPart h2
  harm3 = makeVoxPart h3
  -- TODO: handle range changes
  ranges = RTB.singleton 0 $ VocalRange (foldr min 84 allPitches) (foldr max 36 allPitches)
  allPitches = [ p | VocalStart _ (Just p) <- concatMap RTB.getBodies [harm1, harm2, harm3] ]
  in Vocal
    { vocalPercussion = perc
    , vocalPhraseEnds1 = realTrack tmap $ getPhraseEnds h1
    , vocalPhraseEnds2 = realTrack tmap $ getPhraseEnds h2
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

data Dance t = Dance
  { danceNotes  :: Map.Map Dance.Arrow (RTB.T t (LongNote Dance.NoteType ()))
  , danceEnergy :: RTB.T t Bool
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Dance U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ danceNotes x) $ \(lane, notes) ->
      let showLane = \case
            Dance.ArrowL -> "L"
            Dance.ArrowD -> "D"
            Dance.ArrowU -> "U"
            Dance.ArrowR -> "R"
      in (,) (showLane lane) $ eventList notes $ \case
        NoteOff                 () -> "e"
        Blip   Dance.NoteNormal () -> "n"
        Blip   Dance.NoteMine   () -> "m"
        Blip   Dance.NoteLift   () -> "l"
        Blip   Dance.NoteRoll   () -> "r" -- unexpected
        NoteOn Dance.NoteNormal () -> "N"
        NoteOn Dance.NoteMine   () -> "M" -- unexpected
        NoteOn Dance.NoteLift   () -> "L" -- unexpected
        NoteOn Dance.NoteRoll   () -> "R"
    , (,) "energy" $ eventList (danceEnergy x) A.toJSON
    ]

processDance :: U.TempoMap -> Dance.DanceTrack U.Beats -> Difficulties Dance U.Seconds
processDance tmap trk = makeDanceDifficulties $ \diff -> let
  thisDiff = fromMaybe mempty $ Map.lookup diff $ Dance.danceDifficulties trk
  edges
    = splitEdges
    $ fmap (\((arr, ntype), mlen) -> (ntype, arr, mlen))
    $ edgeBlipsRB_
    $ Dance.danceNotes thisDiff
  getArrow arrow = realTrack tmap $ filterKey arrow edges
  notes = Map.fromList $ do
    arrow <- [minBound .. maxBound]
    return (arrow, getArrow arrow)
  in do
    guard $ not $ RTB.null $ Dance.danceNotes thisDiff
    return Dance
      { danceNotes = notes
      , danceEnergy = realTrack tmap $ Dance.danceOverdrive trk
      }

newtype Difficulties a t = Difficulties [(T.Text, a t)]
  deriving (Eq, Ord, Show)

instance (A.ToJSON (a t)) => A.ToJSON (Difficulties a t) where
  toJSON (Difficulties xs) = A.toJSON [ [A.toJSON d, A.toJSON x] | (d, x) <- xs ]

data Flex t = Flex
  { flexFive    :: Maybe (Difficulties Five      t)
  , flexSix     :: Maybe (Difficulties Six       t)
  , flexDrums   :: [Difficulties Drums t]
  , flexProKeys :: Maybe (Difficulties ProKeys   t)
  , flexProtar  :: Maybe (Difficulties Protar    t)
  , flexCatch   :: Maybe (Difficulties Amplitude t)
  , flexVocal   :: Maybe (Difficulties Vocal     t)
  , flexDance   :: Maybe (Difficulties Dance     t)
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Flex U.Seconds) where
  toJSON flex = A.object $ concat
    [ case flexFive    flex of Nothing -> []; Just x -> [("five"   , A.toJSON x)]
    , case flexSix     flex of Nothing -> []; Just x -> [("six"    , A.toJSON x)]
    , case flexDrums   flex of []      -> []; xs     -> [("drums"  , A.toJSON xs)]
    , case flexProKeys flex of Nothing -> []; Just x -> [("prokeys", A.toJSON x)]
    , case flexProtar  flex of Nothing -> []; Just x -> [("protar" , A.toJSON x)]
    , case flexCatch   flex of Nothing -> []; Just x -> [("catch"  , A.toJSON x)]
    , case flexVocal   flex of Nothing -> []; Just x -> [("vocal"  , A.toJSON x)]
    , case flexDance   flex of Nothing -> []; Just x -> [("dance"  , A.toJSON x)]
    ]

data Processed t = Processed
  { processedTitle    :: T.Text
  , processedArtist   :: T.Text
  , processedAuthor   :: T.Text
  , processedBeats    :: Beats t
  , processedEnd      :: t
  , processedSections :: RTB.T t T.Text
  , processedParts    :: [(T.Text, Flex t)]
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Processed U.Seconds) where
  toJSON proc = A.object $ concat
    [ [("title"   , A.toJSON $ processedTitle  proc)]
    , [("artist"  , A.toJSON $ processedArtist proc)]
    , [("author"  , A.toJSON $ processedAuthor proc)]
    , [("beats"   , A.toJSON $ processedBeats  proc)]
    , [("end"     , A.Number $ realToFrac (realToFrac $ processedEnd proc :: Milli))]
    , [("sections", eventList (processedSections proc) A.String)]
    , [("parts"   , A.toJSON $ processedParts  proc)]
    ]

makeDisplay :: C.SongYaml FilePath -> RBFile.Song (RBFile.OnyxFile U.Beats) -> BL.ByteString
makeDisplay songYaml song = let
  ht n = fromIntegral n / 480
  coda = fmap (fst . fst) $ RTB.viewL $ eventsCoda $ RBFile.onyxEvents $ RBFile.s_tracks song
  defaultFlat = maybe False songKeyUsesFlats $ C._key $ C._metadata songYaml
  -- the above gets imported from first song_key then vocal_tonic_note
  makePart name fpart = Flex
    { flexFive = flip fmap (C.partGRYBO fpart) $ \grybo ->
      case RBFile.selectGuitarTrack RBFile.FiveTypeGuitarExt tracks of
        (trk, algo) -> processFive
          algo
          (ht $ C.gryboHopoThreshold grybo)
          (RBFile.s_tempos song)
          trk
    , flexSix = flip fmap (C.partGHL fpart) $ \ghl -> processSix (ht $ C.ghlHopoThreshold ghl) (RBFile.s_tempos song) (RBFile.onyxPartSix tracks)
    , flexDrums = case C.partDrums fpart of
      Nothing -> []
      Just pd -> case C.drumsMode pd of
        C.DrumsReal -> let
          realDrumTrack = if D.nullDrums $ RBFile.onyxPartRealDrumsPS tracks
            then RBFile.onyxPartDrums tracks
            else RBFile.onyxPartRealDrumsPS tracks
          real = processDrums C.DrumsReal (RBFile.s_tempos song) coda realDrumTrack mempty
          proDrumTrack = if D.nullDrums $ RBFile.onyxPartDrums tracks
            then D.psRealToPro $ RBFile.onyxPartRealDrumsPS tracks
            else RBFile.onyxPartDrums tracks
          pro = processDrums C.DrumsPro (RBFile.s_tempos song) coda proDrumTrack mempty
          in [real, pro]
        mode -> (: []) $ processDrums mode (RBFile.s_tempos song) coda
          (RBFile.onyxPartDrums tracks)
          (RBFile.onyxPartDrums2x tracks)
    , flexProKeys = flip fmap (C.partProKeys fpart) $ \_ -> makeDifficulties $ \diff ->
      processProKeys (RBFile.s_tempos song) $ let
        solos = pkSolo $ RBFile.onyxPartRealKeysX tracks
        in case diff of
          Easy   -> (RBFile.onyxPartRealKeysE tracks) { pkSolo = solos }
          Medium -> (RBFile.onyxPartRealKeysM tracks) { pkSolo = solos }
          Hard   -> (RBFile.onyxPartRealKeysH tracks) { pkSolo = solos }
          Expert ->  RBFile.onyxPartRealKeysX tracks
    , flexProtar = flip fmap (C.partProGuitar fpart) $ \pg -> processProtar
      (ht $ C.pgHopoThreshold pg)
      (C.pgTuning pg)
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
    , flexDance = flip fmap (C.partDance fpart) $ \_dance -> processDance
      (RBFile.s_tempos song)
      (RBFile.onyxPartDance tracks)
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
  end = U.applyTempoMap (RBFile.s_tempos song) $ RBFile.songLengthBeats song
  title  = fromMaybe "" $ C._title  $ C._metadata songYaml
  artist = fromMaybe "" $ C._artist $ C._metadata songYaml
  author = fromMaybe "" $ C._author $ C._metadata songYaml
  sections = U.applyTempoTrack (RBFile.s_tempos song)
    $ fmap (snd . makePSSection . snd)
    $ eventsSections $ RBFile.onyxEvents $ RBFile.s_tracks song
  in A.encode $ Processed title artist author beat end sections parts
