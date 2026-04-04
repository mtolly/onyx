{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.WebPlayer
( makeDisplay
, showTimestamp
, findTremolos, findTrills, laneDifficulty
) where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (guard)
import qualified Data.Aeson                       as A
import qualified Data.Aeson.Key                   as K
import qualified Data.Aeson.Types                 as A
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (toLower)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Milli)
import           Data.Foldable                    (toList)
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd, sort)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   listToMaybe)
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import qualified Numeric.NonNegative.Wrapper      as NN
import           Onyx.Amplitude.PS2.MIDI          (AmpInstrument)
import qualified Onyx.Amplitude.Track             as Amp
import           Onyx.Guitar
import           Onyx.MIDI.Common                 (Difficulty (..), Edge (..),
                                                   LaneDifficulty (..),
                                                   LongNote (..), SongKey (..),
                                                   StrumHOPOTap (..),
                                                   edgeBlips_, joinEdgesSimple,
                                                   minSustainLengthRB,
                                                   songKeyUsesFlats, splitEdges,
                                                   splitEdgesBool)
import qualified Onyx.MIDI.Track.Beat             as Beat
import qualified Onyx.MIDI.Track.Drums            as D
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import qualified Onyx.MIDI.Track.ProGuitar        as PG
import           Onyx.MIDI.Track.ProKeys          as PK
import qualified Onyx.MIDI.Track.SixFret          as GHL
import qualified Onyx.MIDI.Track.Vocal.Legacy     as Vox
import           Onyx.Mode
import qualified Onyx.PhaseShift.Dance            as Dance
import qualified Onyx.Project                     as C
import           Onyx.Sections                    (makeDisplaySection,
                                                   sectionBody)
import           Onyx.Util.Text.Transform         (showTimestamp)
import           Optics                           ((.~))
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
  NoteOff ()      -> "e"
  Blip Tap   ()   -> "t"
  Blip Strum ()   -> "s"
  Blip HOPO  ()   -> "h"
  NoteOn Tap   () -> "T"
  NoteOn Strum () -> "S"
  NoteOn HOPO  () -> "H"

instance A.ToJSON (Five U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ Map.toList x.fiveNotes <&> \(color, notes) ->
      (,) (maybe "open" (K.fromString . map toLower . show) color) $ eventList notes showHSTNote
    , (,) "solo" $ eventList x.fiveSolo A.toJSON
    , (,) "energy" $ eventList x.fiveEnergy A.toJSON
    , (,) "lanes" $ A.object $ Map.toList x.fiveLanes <&> \(color, lanes) ->
      (,) (maybe "open" (K.fromString . map toLower . show) color) $ eventList lanes A.toJSON
    , (,) "bre" $ eventList x.fiveBRE A.toJSON
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
    [ (,) "notes" $ eventList x.drumNotes $ A.toJSON . map drumGemChar
    , (,) "solo" $ eventList x.drumSolo A.toJSON
    , (,) "energy" $ eventList x.drumEnergy A.toJSON
    , (,) "lanes" $ A.object $ Map.toList x.drumLanes <&> \(gem, lanes) ->
      (,) (K.fromString [drumGemChar gem]) $ eventList lanes A.toJSON
    , (,) "bre" $ eventList x.drumBRE A.toJSON
    , (,) "mode" case x.drumMode of
      C.Drums4     -> "4"
      C.Drums5     -> "5"
      C.DrumsPro   -> "pro"
      C.DrumsReal  -> "real"
      C.DrumsElite -> "true"
    , (,) "disco" $ eventList x.drumDisco A.toJSON
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
  PK.OrangeC     -> "o-c"
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
    [ (,) "notes" $ A.object $ Map.toList x.proKeysNotes <&> \(p, notes) ->
      (,) (K.fromText $ showPitch p) $ eventList notes \case
        NoteOff ()   -> "e"
        Blip () ()   -> "n"
        NoteOn () () -> "N"
    , (,) "ranges" $ eventList x.proKeysRanges $ A.toJSON . map toLower . drop 5 . show
    , (,) "solo" $ eventList x.proKeysSolo A.toJSON
    , (,) "energy" $ eventList x.proKeysEnergy A.toJSON
    , (,) "lanes" $ A.object $ Map.toList x.proKeysLanes <&> \(pitch, lanes) ->
      (,) (K.fromText $ showPitch pitch) $ eventList lanes A.toJSON
    , (,) "gliss" $ eventList x.proKeysGlissando A.toJSON
    , (,) "bre" $ eventList x.proKeysBRE A.toJSON
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
    [ (,) "notes" $ A.object $ Map.toList x.protarNotes <&> \(string, notes) ->
      (,) (K.fromString $ map toLower $ show string) $ eventList notes $ A.String . \case
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
    , (,) "solo" $ eventList x.protarSolo A.toJSON
    , (,) "energy" $ eventList x.protarEnergy A.toJSON
    , (,) "lanes" $ A.object $ Map.toList x.protarLanes <&> \(string, lanes) ->
      (,) (K.fromString $ map toLower $ show string) $ eventList lanes A.toJSON
    , (,) "bre" $ eventList x.protarBRE A.toJSON
    , (,) "chords" $ eventList x.protarChords $ A.String . \case
      NoteOff     () -> "e"
      Blip   name () -> "c:" <> name
      NoteOn name () -> "C:" <> name
    , (,) "arpeggio" $ eventList x.protarArpeggio A.toJSON
    , (,) "strings" $ A.toJSON x.protarStrings
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
    [ (,) "notes" $ A.object $ Map.toList x.sixNotes <&> \(lane, notes) ->
      let showLane = \case
            GHLSingle GHL.Black1 -> "b1"
            GHLSingle GHL.Black2 -> "b2"
            GHLSingle GHL.Black3 -> "b3"
            GHLSingle GHL.White1 -> "w1"
            GHLSingle GHL.White2 -> "w2"
            GHLSingle GHL.White3 -> "w3"
            GHLBoth1             -> "bw1"
            GHLBoth2             -> "bw2"
            GHLBoth3             -> "bw3"
            GHLOpen              -> "open"
      in (,) (showLane lane) $ eventList notes showHSTNote
    , (,) "solo" $ eventList x.sixSolo A.toJSON
    , (,) "energy" $ eventList x.sixEnergy A.toJSON
    , (,) "bre" $ eventList x.sixBRE A.toJSON
    ]

realTrack :: (Ord a) => U.TempoMap -> RTB.T U.Beats a -> RTB.T U.Seconds a
realTrack tmap = U.applyTempoTrack tmap . RTB.normalize

filterKey :: (NNC.C t, Eq a) => a -> RTB.T t (LongNote s a) -> RTB.T t (LongNote s ())
filterKey k = RTB.mapMaybe $ mapM \x -> guard $ k == x

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
        Nothing         -> [] -- only one note under or after trill
        Just (_, cols2) -> map (\fret -> (time, (fret, len))) $ cols1 ++ cols2
      )
    . ATB.toPairList
    . RTB.toAbsoluteEventList NNC.zero

makeDifficulties :: (Difficulty -> Maybe (a t)) -> Difficulties a t
makeDifficulties f = Difficulties $ catMaybes do
  (d, name) <- [(Expert, "X"), (Hard, "H"), (Medium, "M"), (Easy, "E")]
  return $ (name,) <$> f d

makeDrumDifficulties :: (Maybe Difficulty -> Maybe (a t)) -> Difficulties a t
makeDrumDifficulties f = Difficulties $ catMaybes do
  (d, name) <- [(Nothing, "X+"), (Just Expert, "X"), (Just Hard, "H"), (Just Medium, "M"), (Just Easy, "E")]
  return $ (name,) <$> f d

laneDifficulty :: (NNC.C t) => Difficulty -> RTB.T t (Maybe LaneDifficulty) -> RTB.T t t
laneDifficulty Expert lanes = fmap (\(_, _, len) -> len) $ joinEdgesSimple $ maybe (EdgeOff ()) (`EdgeOn` ()) <$> lanes
laneDifficulty Hard   lanes
  = RTB.mapMaybe (\(d, (), len) -> guard (d == LaneHard) >> Just len)
  $ joinEdgesSimple $ maybe (EdgeOff ()) (`EdgeOn` ()) <$> lanes
laneDifficulty _      _     = RTB.empty

processFive :: U.TempoMap -> FiveResult -> Difficulties Five U.Seconds
processFive tmap result = makeDifficulties \diff -> let
  assigned = fromMaybe mempty $ Map.lookup diff result.notes
  assigned' = U.trackJoin $ assigned <&> \((color, sht), mlen) -> case mlen of
    Nothing  -> RTB.singleton 0 $ Blip sht color
    Just len -> RTB.fromPairList [(0, NoteOn sht color), (len, NoteOff color)]
  getColor color = realTrack tmap $ filterKey color assigned'
  notes = Map.fromList do
    color <- Nothing : map Just [minBound .. maxBound]
    return (color, getColor color)
  solo   = realTrack tmap result.other.solo
  energy = realTrack tmap result.other.overdrive
  bre    = realTrack tmap result.other.bre
  ons    = RTB.normalize $ fmap (fst . fst) assigned
  trems  = findTremolos ons $ RTB.normalize $ laneDifficulty diff result.other.tremolo
  trills = findTrills   ons $ RTB.normalize $ laneDifficulty diff result.other.trill
  lanesAll = RTB.merge trems trills
  lanes = Map.fromList do
    color <- Nothing : fmap Just [Five.Green .. Five.Orange]
    return $ (,) color $ U.applyTempoTrack tmap $ splitEdgesBool $ flip RTB.mapMaybe lanesAll \(color', len) -> do
      guard $ color == color'
      return len
  in guard (not $ RTB.null assigned) >> Just (Five notes solo energy lanes bre)

processSix :: U.Beats -> U.TempoMap -> GHL.SixTrack U.Beats -> Difficulties Six U.Seconds
processSix hopoThreshold tmap trk = makeDifficulties \diff -> let
  thisDiff = fromMaybe mempty $ Map.lookup diff trk.difficulties
  assigned :: RTB.T U.Beats ((Maybe GHL.Fret, StrumHOPOTap), Maybe U.Beats)
  assigned
    = applyForces (getForces6 thisDiff)
    $ strumHOPOTap HOPOsRBGuitar hopoThreshold $ edgeBlips_ minSustainLengthRB thisDiff.gems
  onlyKey :: (Eq a) => a -> RTB.T U.Beats ((a, b), c) -> RTB.T U.Beats (((), b), c)
  onlyKey fret trips = flip RTB.mapMaybe trips \case
    ((fret', sht), mlen) -> guard (fret' == fret) >> Just (((), sht), mlen)
  oneTwoBoth x y = let
    replaceFret b ((_, sht), mlen) = ((b, sht), mlen)
    dual = RTB.collectCoincident $ RTB.merge (replaceFret False <$> x) (replaceFret True <$> y)
    (both, notBoth) = flip RTB.partitionMaybe dual \case
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
    GHLBoth1             -> bw1
    GHLBoth2             -> bw2
    GHLBoth3             -> bw3
    GHLOpen              -> onlyKey Nothing assigned
  notes = Map.fromList do
    lane <- map GHLSingle [minBound .. maxBound] ++ [GHLBoth1, GHLBoth2, GHLBoth3, GHLOpen]
    return (lane, getLane lane)
  solo   = realTrack tmap trk.solo
  energy = realTrack tmap trk.overdrive
  bre    = RTB.empty
  in guard (not $ RTB.null thisDiff.gems) >> Just (Six notes solo energy bre)

processDrums :: C.DrumMode -> U.TempoMap -> Maybe U.Beats -> D.DrumTrack U.Beats -> D.DrumTrack U.Beats -> Difficulties Drums U.Seconds
processDrums mode tmap coda trk1x trk2x = makeDrumDifficulties \diff -> let
  -- TODO if only 2x kicks charted, label difficulty as X+ instead of X
  has2x = all (not . D.nullDrums) [trk1x, trk2x] || any (not . RTB.null . (.kick2x)) [trk1x, trk2x]
  trk = case (D.nullDrums trk1x, D.nullDrums trk2x, diff) of
    (True , _    , _      ) -> trk2x
    (_    , True , _      ) -> trk1x
    (False, False, Nothing) -> trk2x
    (False, False, Just _ ) -> trk1x
  drumDiff = Map.lookup (fromMaybe Expert diff) trk.difficulties
  nonPro is5 = (if diff == Nothing then RTB.merge $ D.Kick <$ trk.kick2x else id)
    $ maybe RTB.empty (.gems) drumDiff <&> \(gem, _) -> case gem of
      D.Kick            -> D.Kick
      D.Red             -> D.Red
      D.Pro D.Yellow () -> D.Pro D.Yellow if is5 then D.Cymbal else D.Tom
      D.Pro D.Blue   () -> D.Pro D.Blue D.Tom
      D.Orange          -> D.Orange
      D.Pro D.Green  () -> D.Pro D.Green D.Tom
  notes = fmap sort $ RTB.collectCoincident case mode of
    C.Drums4     -> fmap Right $ nonPro False
    C.Drums5     -> fmap Right $ nonPro True
    C.DrumsPro   -> fmap (Right . fst) $ D.computePro diff trk
    C.DrumsReal  -> fmap fst $ D.computePSReal diff trk
    C.DrumsElite -> fmap (Right . fst) $ D.computePro diff trk -- TODO generate pro if needed
  notesS = realTrack tmap notes
  notesB = RTB.normalize notes
  solo   = realTrack tmap trk.solo
  energy = realTrack tmap trk.overdrive
  fills  = realTrack tmap trk.activation
  bre    = case U.applyTempoMap tmap <$> coda of
    Nothing -> RTB.empty
    Just c  -> RTB.delay c $ U.trackDrop c fills
  hands  = RTB.filter (`notElem` [Right D.Kick, Left D.HHPedal]) $ RTB.flatten notesB
  singles = findTremolos hands $ RTB.normalize $ laneDifficulty (fromMaybe Expert diff) trk.singleRoll
  doubles = findTrills   hands $ RTB.normalize $ laneDifficulty (fromMaybe Expert diff) trk.doubleRoll
  lanesAll = RTB.merge singles doubles
  lanes = Map.fromList do
    gem <- map Right
      [ D.Kick, D.Red
      , D.Pro D.Yellow D.Cymbal, D.Pro D.Yellow D.Tom
      , D.Pro D.Blue   D.Cymbal, D.Pro D.Blue   D.Tom
      , D.Pro D.Green  D.Cymbal, D.Pro D.Green  D.Tom
      , D.Orange
      ] ++ map Left [D.Rimshot, D.HHOpen, D.HHSizzle, D.HHPedal]
    return $ (,) gem $ U.applyTempoTrack tmap $ splitEdgesBool $ flip RTB.mapMaybe lanesAll \(gem', len) -> do
      guard $ gem == gem'
      return len
  disco = realTrack tmap $ maybe RTB.empty (fmap ((== D.Disco) . snd) . (.mix)) drumDiff
  in do
    guard $ not $ RTB.null notes
    guard $ diff /= Nothing || has2x
    let mode' = case mode of
          C.DrumsElite -> C.DrumsPro
          _            -> mode
    Just $ Drums notesS solo energy lanes bre mode' disco

processProKeys :: U.TempoMap -> ProKeysTrack U.Beats -> Maybe (ProKeys U.Seconds)
processProKeys tmap trk = let
  joined = edgeBlips_ minSustainLengthRB trk.notes
  assigned' = U.trackJoin $ joined <&> \(p, mlen) -> case mlen of
    Nothing  -> RTB.singleton 0 $ Blip () p
    Just len -> RTB.fromPairList [(0, NoteOn () p), (len, NoteOff p)]
  notesForPitch p = realTrack tmap $ filterKey p assigned'
  notes = Map.fromList [ (p, notesForPitch p) | p <- [minBound .. maxBound] ]
  ons = RTB.normalize $ fmap fst joined
  ranges = realTrack tmap trk.lanes
  solo   = realTrack tmap trk.solo
  energy = realTrack tmap trk.overdrive
  trills = findTrills ons $ RTB.normalize $ laneDifficulty Expert
    $ (\b -> guard b >> Just LaneExpert) <$> trk.trill
  lanes = Map.fromList do
    key <- [minBound .. maxBound]
    return $ (,) key $ U.applyTempoTrack tmap $ splitEdgesBool $ flip RTB.mapMaybe trills \(key', len) -> do
      guard $ key == key'
      return len
  gliss  = realTrack tmap trk.glissando
  bre    = realTrack tmap trk.bre
  in guard (not $ RTB.null trk.notes) >> Just (ProKeys notes ranges solo energy lanes gliss bre)

processProtar :: U.Beats -> PG.GtrTuning -> Bool -> U.TempoMap -> PG.ProGuitarTrack U.Beats -> Difficulties Protar U.Seconds
processProtar hopoThreshold tuning defaultFlat tmap pg = makeDifficulties \diff -> let
  thisDiff = fromMaybe mempty $ Map.lookup diff pg.difficulties
  assigned = expandColors $ PG.guitarifyFull hopoThreshold thisDiff
  expandColors = splitEdges . RTB.flatten . fmap expandChord
  expandChord (shopo, gems, len) = do
    (str, fret, ntype) <- gems
    return ((shopo, guard (ntype /= PG.Muted) >> Just fret, ntype == PG.ArpeggioForm, len >>= snd), str, fmap fst len)
  getString string = realTrack tmap $ flip RTB.mapMaybe assigned \case
    Blip   ntype s -> guard (s == string) >> Just (Blip   ntype ())
    NoteOn ntype s -> guard (s == string) >> Just (NoteOn ntype ())
    NoteOff      s -> guard (s == string) >> Just (NoteOff      ())
  protarNotes = Map.fromList do
    string <- [minBound .. maxBound]
    return (string, getString string)
  protarSolo = realTrack tmap pg.solo
  protarEnergy = realTrack tmap pg.overdrive
  -- for protar we can treat tremolo/trill identically;
  -- in both cases it's just "get the strings the first note/chord is on"
  onStrings = RTB.normalize $ flip RTB.mapMaybe thisDiff.notes \case
    EdgeOn _ (str, _) -> Just str
    EdgeOff _         -> Nothing
  lanesAll = findTremolos onStrings $ RTB.normalize
    $ laneDifficulty diff $ RTB.merge pg.tremolo pg.trill
  protarLanes = Map.fromList do
    str <- [minBound .. maxBound]
    return $ (,) str $ U.applyTempoTrack tmap $ splitEdgesBool $ flip RTB.mapMaybe lanesAll \(str', len) -> do
      guard $ str == str'
      return len
  protarBRE = realTrack tmap $ fmap snd pg.bre
  usedStrings = nubOrd $ toList pg.difficulties >>= toList . (.notes) >>= map fst . toList
  pitches = PG.tuningPitches tuning { PG.gtrGlobal = 0 }
  protarStrings
    | not (null $ drop 7 pitches) || elem PG.S8 usedStrings = 8
    | not (null $ drop 6 pitches) || elem PG.S7 usedStrings = 7
    | not (null $ drop 5 pitches) || elem PG.S1 usedStrings = 6
    | not (null $ drop 4 pitches) || elem PG.S2 usedStrings = 5
    | not (null $ drop 3 pitches) || elem PG.S3 usedStrings = 4
    | otherwise                                             = 3
  protarChords = realTrack tmap $ PG.computeChordNames diff pitches defaultFlat pg
  protarArpeggio = realTrack tmap thisDiff.arpeggio
  in guard (not $ RTB.null thisDiff.notes) >> Just Protar{..}

newtype Beats t = Beats
  { beatLines :: RTB.T t Beat
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Beats U.Seconds) where
  toJSON x = A.object
    [ (,) "lines" $ eventList x.beatLines $ A.toJSON . \case
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
processBeat tmap rtb = Beats $ U.applyTempoTrack tmap $ rtb <&> \case
  Beat.Bar  -> Bar
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
    [ (,) "harm1" $ eventList x.harm1Notes voxEvent
    , (,) "harm2" $ eventList x.harm2Notes voxEvent
    , (,) "harm3" $ eventList x.harm3Notes voxEvent
    , (,) "percussion" $ eventList x.vocalPercussion \() -> A.Null
    , (,) "phrases1" $ eventList x.vocalPhraseEnds1 \() -> A.Null
    , (,) "phrases2" $ eventList x.vocalPhraseEnds2 \() -> A.Null
    , (,) "ranges" $ eventList x.vocalRanges \case
      VocalRange pmin pmax -> A.toJSON [pmin, pmax]
      VocalRangeShift      -> A.Null
    , (,) "energy" $ eventList x.vocalEnergy A.toJSON
    , (,) "tonic" $ maybe A.Null A.toJSON x.vocalTonic
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
  perc = realTrack tmap $ flip RTB.mapMaybe h1 \case
    Vox.Percussion -> Just ()
    _              -> Nothing
  getPhraseEnds = RTB.mapMaybe \case
    Vox.Phrase False  -> Just ()
    Vox.Phrase2 False -> Just ()
    _                 -> Nothing
  pitchToInt p = fromEnum p + 36
  makeVoxPart trk = realTrack tmap $ flip RTB.mapMaybe (RTB.collectCoincident trk) \evts -> let
    lyric = listToMaybe [ s | Vox.Lyric s <- evts ]
    note = listToMaybe [ p | Vox.Note True p <- evts ]
    end = listToMaybe [ () | Vox.Note False _ <- evts ]
    in case (lyric, note, end) of
      -- Note: the _ in the first pattern below should be Nothing,
      -- but we allow Just () for sloppy vox charts with no gap between notes
      (ml, Just p, _) -> let
        l = fromMaybe "" ml
        in Just case T.stripSuffix "#" l <|> T.stripSuffix "^" l of
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
    , vocalEnergy = realTrack tmap $ flip RTB.mapMaybe h1 \case
      Vox.Overdrive b -> Just b
      _               -> Nothing
    , vocalRanges = ranges
    }

data Amplitude t = Amplitude
  { ampNotes      :: RTB.T t Amp.Gem
  , ampInstrument :: AmpInstrument
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Amplitude U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ eventList x.ampNotes \case
      Amp.L -> A.Number 1
      Amp.M -> A.Number 2
      Amp.R -> A.Number 3
    -- hack drop "Amp" from constructor name
    , (,) "instrument" $ A.toJSON $ drop 3 $ show x.ampInstrument
    ]

data Dance t = Dance
  { danceNotes  :: Map.Map Dance.Arrow (RTB.T t (LongNote Dance.NoteType ()))
  , danceEnergy :: RTB.T t Bool
  } deriving (Eq, Ord, Show)

instance A.ToJSON (Dance U.Seconds) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ Map.toList x.danceNotes <&> \(lane, notes) ->
      let showLane = \case
            Dance.ArrowL -> "L"
            Dance.ArrowD -> "D"
            Dance.ArrowU -> "U"
            Dance.ArrowR -> "R"
      in (,) (showLane lane) $ eventList notes \case
        NoteOff                 () -> "e"
        Blip   Dance.NoteNormal () -> "n"
        Blip   Dance.NoteMine   () -> "m"
        Blip   Dance.NoteLift   () -> "l"
        Blip   Dance.NoteRoll   () -> "r" -- unexpected
        NoteOn Dance.NoteNormal () -> "N"
        NoteOn Dance.NoteMine   () -> "M" -- unexpected
        NoteOn Dance.NoteLift   () -> "L" -- unexpected
        NoteOn Dance.NoteRoll   () -> "R"
    , (,) "energy" $ eventList x.danceEnergy A.toJSON
    ]

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
    [ case flex.flexFive    of Nothing -> []; Just x -> [("five"   , A.toJSON x)]
    , case flex.flexSix     of Nothing -> []; Just x -> [("six"    , A.toJSON x)]
    , case flex.flexDrums   of []      -> []; xs     -> [("drums"  , A.toJSON xs)]
    , case flex.flexProKeys of Nothing -> []; Just x -> [("prokeys", A.toJSON x)]
    , case flex.flexProtar  of Nothing -> []; Just x -> [("protar" , A.toJSON x)]
    , case flex.flexCatch   of Nothing -> []; Just x -> [("catch"  , A.toJSON x)]
    , case flex.flexVocal   of Nothing -> []; Just x -> [("vocal"  , A.toJSON x)]
    , case flex.flexDance   of Nothing -> []; Just x -> [("dance"  , A.toJSON x)]
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
    [ [("title"   , A.toJSON proc.processedTitle)]
    , [("artist"  , A.toJSON proc.processedArtist)]
    , [("author"  , A.toJSON proc.processedAuthor)]
    , [("beats"   , A.toJSON proc.processedBeats)]
    , [("end"     , A.Number $ realToFrac (realToFrac $ proc.processedEnd :: Milli))]
    , [("sections", eventList proc.processedSections A.String)]
    , [("parts"   , A.toJSON proc.processedParts)]
    ]

makeDisplay :: C.SongYaml FilePath -> F.Song (F.OnyxFile U.Beats) -> BL.ByteString
makeDisplay songYaml song = let
  ht n = fromIntegral n / 480
  coda = fmap (fst . fst) $ RTB.viewL song.tracks.onyxEvents.coda
  defaultFlat = maybe False songKeyUsesFlats songYaml.metadata.key
  -- the above gets imported from first song_key then vocal_tonic_note
  makePart :: F.PartName -> C.Part FilePath -> Flex U.Seconds
  makePart name fpart = Flex
    { flexFive = nativeFiveFret fpart <&> \builder -> let
      result = builder FiveTypeGuitarExt ModeInput
        { tempo  = song.tempos
        , events = song.tracks.onyxEvents
        , part   = tracks
        }
      in processFive song.tempos result
    , flexSix = fpart.ghl <&> \ghl -> processSix (ht ghl.hopoThreshold) song.tempos tracks.onyxPartSix
    , flexDrums = case fpart.drums of
      Nothing -> []
      Just pd -> case pd.mode of
        C.DrumsReal -> let
          realDrumTrack = if D.nullDrums tracks.onyxPartRealDrumsPS
            then tracks.onyxPartDrums
            else tracks.onyxPartRealDrumsPS
          real = processDrums C.DrumsReal song.tempos coda realDrumTrack mempty
          proDrumTrack = if D.nullDrums tracks.onyxPartDrums
            then D.psRealToPro tracks.onyxPartRealDrumsPS
            else tracks.onyxPartDrums
          pro = processDrums C.DrumsPro song.tempos coda proDrumTrack mempty
          in [real, pro]
        mode -> (: []) $ processDrums mode song.tempos coda
          tracks.onyxPartDrums
          tracks.onyxPartDrums2x
    , flexProKeys = fpart.proKeys <&> \_ -> makeDifficulties \diff ->
      processProKeys song.tempos let
        solos = tracks.onyxPartRealKeysX.solo
        in case diff of
          Easy   -> tracks.onyxPartRealKeysE & #solo .~ solos
          Medium -> tracks.onyxPartRealKeysM & #solo .~ solos
          Hard   -> tracks.onyxPartRealKeysH & #solo .~ solos
          Expert -> tracks.onyxPartRealKeysX
    , flexProtar = fpart.proGuitar <&> \pg -> processProtar
      (ht pg.hopoThreshold)
      pg.tuning
      defaultFlat
      song.tempos
      $ let mustang = tracks.onyxPartRealGuitar
            squier  = tracks.onyxPartRealGuitar22
        in if PG.nullPG squier then mustang else squier
    , flexVocal = fpart.vocal <&> \pvox -> let
      harm = case pvox.count of
        C.Vocal3 -> [("H", makeVox pvox
          tracks.onyxHarm1
          tracks.onyxHarm2
          tracks.onyxHarm3)]
        C.Vocal2 -> [("H", makeVox pvox
          tracks.onyxHarm1
          tracks.onyxHarm2
          mempty)]
        C.Vocal1 -> []
      solo = ("1", makeVox pvox tracks.onyxPartVocals mempty mempty)
      in Difficulties $ reverse $ solo : harm
    , flexCatch = Nothing -- removed, moved to mania
    , flexDance = Nothing -- removed, moved to mania
    } where
      tracks = F.getFlexPart name song.tracks
  parts = do
    (name, fpart) <- sort $ HM.toList $ HM.filter (/= C.emptyPart) songYaml.parts.getParts
    return (F.getPartName name, makePart name fpart)
  makeVox pvox h1 h2 h3 = processVocal song.tempos
    (Vox.vocalToLegacy h1) (Vox.vocalToLegacy h2) (Vox.vocalToLegacy h3)
    $ fmap fromEnum pvox.key
    <|> fmap (fromEnum . (.songKey)) songYaml.metadata.key
  beat = processBeat song.tempos song.tracks.onyxBeat.beatLines
  end = U.applyTempoMap song.tempos $ F.songLengthBeats song
  title  = fromMaybe "" songYaml.metadata.title
  artist = fromMaybe "" songYaml.metadata.artist
  author = fromMaybe "" songYaml.metadata.author
  sections = U.applyTempoTrack song.tempos
    $ fmap (sectionBody . makeDisplaySection . makeDisplaySection)
      song.tracks.onyxEvents.sections
  in A.encode $ Processed title artist author beat end sections parts
