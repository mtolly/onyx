{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module OnyxiteDisplay.Process where

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
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (stripPrefix)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (listToMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified RockBand.Beat                    as Beat
import           RockBand.Common                  (Difficulty (..),
                                                   LongNote (..), splitEdges)
import qualified RockBand.Drums                   as Drums
import qualified RockBand.File                    as RBFile
import qualified RockBand.FiveButton              as Five
import           RockBand.PhaseShiftMessage       (discardPS)
import qualified RockBand.ProGuitar               as PG
import qualified RockBand.ProKeys                 as PK
import qualified RockBand.Vocals                  as Vox
import           Scripts                          (songLengthBeats)
import qualified Sound.MIDI.Util                  as U

class TimeFunctor f where
  mapTime :: (Real u) => (t -> u) -> f t -> f u

data Five t = Five
  { fiveNotes  :: Map.Map Five.Color (Map.Map t (LongNote Five.StrumHOPO ()))
  , fiveSolo   :: Map.Map t Bool
  , fiveEnergy :: Map.Map t Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor Five where
  mapTime f (Five x y z) = Five (Map.map (Map.mapKeys f) x) (Map.mapKeys f y) (Map.mapKeys f z)

eventList :: (Real t) => Map.Map t a -> (a -> A.Value) -> A.Value
eventList evts f = A.toJSON $ map g $ Map.toAscList evts where
  g (secs, evt) = let
    secs' = A.Number $ realToFrac secs
    evt' = f evt
    in A.toJSON [secs', evt']

instance (Real t) => A.ToJSON (Five t) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ fiveNotes x) $ \(color, notes) ->
      (,) (T.pack $ map toLower $ show color) $ eventList notes $ \case
        NoteOff () -> "end"
        Blip Five.Strum () -> "strum"
        Blip Five.HOPO () -> "hopo"
        NoteOn Five.Strum () -> "strum-sust"
        NoteOn Five.HOPO () -> "hopo-sust"
    , (,) "solo" $ eventList (fiveSolo x) A.toJSON
    , (,) "energy" $ eventList (fiveEnergy x) A.toJSON
    ]

readEventList :: (Ord t, Fractional t) => (A.Value -> A.Parser a) -> A.Value -> A.Parser (Map.Map t a)
readEventList f v = do
  dblValPairs <- A.parseJSON v
  fmap Map.fromList $ forM dblValPairs $ \(dbl, val) -> do
    let _ = dbl :: Double
    x <- f val
    return (realToFrac dbl, x)

readKeyMapping :: (Ord k) => (T.Text -> A.Parser k) -> (A.Value -> A.Parser a) -> A.Value -> A.Parser (Map.Map k a)
readKeyMapping readKey readVal = A.withObject "object with key->notes mapping" $ \obj -> do
  fmap Map.fromList $ forM (HM.toList obj) $ \(k, v) -> do
    key <- readKey k
    val <- readVal v
    return (key, val)

data Drums t = Drums
  { drumNotes  :: Map.Map t [Drums.Gem Drums.ProType]
  , drumSolo   :: Map.Map t Bool
  , drumEnergy :: Map.Map t Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor Drums where
  mapTime f (Drums x y z) = Drums (Map.mapKeys f x) (Map.mapKeys f y) (Map.mapKeys f z)

instance (Real t) => A.ToJSON (Drums t) where
  toJSON x = A.object
    [ (,) "notes" $ eventList (drumNotes x) $ let
      gem = A.String . \case
        Drums.Kick                          -> "kick"
        Drums.Red                           -> "red"
        Drums.Pro Drums.Yellow Drums.Cymbal -> "y-cym"
        Drums.Pro Drums.Yellow Drums.Tom    -> "y-tom"
        Drums.Pro Drums.Blue   Drums.Cymbal -> "b-cym"
        Drums.Pro Drums.Blue   Drums.Tom    -> "b-tom"
        Drums.Pro Drums.Green  Drums.Cymbal -> "g-cym"
        Drums.Pro Drums.Green  Drums.Tom    -> "g-tom"
      in A.toJSON . map gem
    , (,) "solo" $ eventList (drumSolo x) A.toJSON
    , (,) "energy" $ eventList (drumEnergy x) A.toJSON
    ]

data ProKeys t = ProKeys
  { proKeysNotes  :: Map.Map PK.Pitch (Map.Map t (LongNote () ()))
  , proKeysRanges :: Map.Map t PK.LaneRange
  , proKeysSolo   :: Map.Map t Bool
  , proKeysEnergy :: Map.Map t Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor ProKeys where
  mapTime f (ProKeys w x y z) = ProKeys (Map.map (Map.mapKeys f) w) (Map.mapKeys f x) (Map.mapKeys f y) (Map.mapKeys f z)

showPitch :: PK.Pitch -> T.Text
showPitch = \case
  PK.RedYellow k -> "ry-" <> showKey k
  PK.BlueGreen k -> "bg-" <> showKey k
  PK.OrangeC -> "o-c"
  where showKey = T.pack . map toLower . show

pitchMap :: [(T.Text, PK.Pitch)]
pitchMap = do
  p <- [minBound .. maxBound]
  return (showPitch p, p)

readPitch :: T.Text -> A.Parser PK.Pitch
readPitch t = case lookup t pitchMap of
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
    ]

data Protar t = Protar
  { protarNotes  :: Map.Map PG.GtrString (Map.Map t (LongNote (Five.StrumHOPO, Maybe PG.GtrFret) ()))
  , protarSolo   :: Map.Map t Bool
  , protarEnergy :: Map.Map t Bool
  } deriving (Eq, Ord, Show)

instance TimeFunctor Protar where
  mapTime f (Protar x y z) = Protar (Map.map (Map.mapKeys f) x) (Map.mapKeys f y) (Map.mapKeys f z)

instance (Real t) => A.ToJSON (Protar t) where
  toJSON x = A.object
    [ (,) "notes" $ A.object $ flip map (Map.toList $ protarNotes x) $ \(string, notes) ->
      (,) (T.pack $ map toLower $ show string) $ eventList notes $ A.String . \case
        NoteOff () -> "end"
        Blip (Five.Strum, fret) () -> "strum" <> showFret fret
        Blip (Five.HOPO, fret) () -> "hopo" <> showFret fret
        NoteOn (Five.Strum, fret) () -> "strum-sust" <> showFret fret
        NoteOn (Five.HOPO, fret) () -> "hopo-sust" <> showFret fret
    , (,) "solo" $ eventList (protarSolo x) A.toJSON
    , (,) "energy" $ eventList (protarEnergy x) A.toJSON
    ] where showFret Nothing  = "-x"
            showFret (Just i) = "-" <> T.pack (show i)

trackToMap :: (Ord a) => U.TempoMap -> RTB.T U.Beats a -> Map.Map U.Seconds a
trackToMap tmap = Map.fromList . ATB.toPairList . RTB.toAbsoluteEventList 0 . U.applyTempoTrack tmap . RTB.normalize

processFive :: Maybe U.Beats -> U.TempoMap -> RTB.T U.Beats Five.Event -> Five U.Seconds
processFive hopoThreshold tmap trk = let
  expert = flip RTB.mapMaybe trk $ \case Five.DiffEvent Expert e -> Just e; _ -> Nothing
  assigned = case hopoThreshold of
    Just threshold -> expandColors $ Five.guitarifyHOPO threshold False expert
    Nothing -> Five.assignKeys expert
  expandColors = RTB.flatten . fmap (traverse toList)
  getColor color = trackToMap tmap $ flip RTB.mapMaybe assigned $ \case
    Blip   ntype c -> guard (c == color) >> Just (Blip   ntype ())
    NoteOn ntype c -> guard (c == color) >> Just (NoteOn ntype ())
    NoteOff      c -> guard (c == color) >> Just (NoteOff      ())
  notes = Map.fromList $ do
    color <- [minBound .. maxBound]
    return (color, getColor color)
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Five.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Five.Overdrive b -> Just b; _ -> Nothing
  in Five notes solo energy

processDrums :: U.TempoMap -> RTB.T U.Beats Drums.Event -> Drums U.Seconds
processDrums tmap trk = let
  notes = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $
    U.applyTempoTrack tmap $ RTB.collectCoincident $ flip RTB.mapMaybe (Drums.assignToms trk) $ \case
      (Expert, gem) -> Just gem
      _             -> Nothing
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Drums.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case Drums.Overdrive b -> Just b; _ -> Nothing
  in Drums notes solo energy

processProKeys :: U.TempoMap -> RTB.T U.Beats PK.Event -> ProKeys U.Seconds
processProKeys tmap trk = let
  notesForPitch p = trackToMap tmap $ flip RTB.mapMaybe trk $ \case
    PK.Note (NoteOff    p') -> guard (p == p') >> Just (NoteOff    ())
    PK.Note (Blip    () p') -> guard (p == p') >> Just (Blip    () ())
    PK.Note (NoteOn  () p') -> guard (p == p') >> Just (NoteOn  () ())
    _                    -> Nothing
  notes = Map.fromList [ (p, notesForPitch p) | p <- [minBound .. maxBound] ]
  ranges = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.LaneShift r -> Just r; _ -> Nothing
  solo   = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.Solo      b -> Just b; _ -> Nothing
  energy = trackToMap tmap $ flip RTB.mapMaybe trk $ \case PK.Overdrive b -> Just b; _ -> Nothing
  in ProKeys notes ranges solo energy

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
  in Protar notes solo energy

data Beats t = Beats
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

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix sfx str = fmap reverse $ stripPrefix (reverse sfx) (reverse str)

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

data Processed t = Processed
  { processedGuitar    :: Maybe (Five    t)
  , processedBass      :: Maybe (Five    t)
  , processedKeys      :: Maybe (Five    t)
  , processedDrums     :: Maybe (Drums   t)
  , processedProKeys   :: Maybe (ProKeys t)
  , processedProGuitar :: Maybe (Protar  t)
  , processedProBass   :: Maybe (Protar  t)
  , processedVocal     :: Maybe (Vocal   t)
  , processedBeats     ::        Beats   t
  , processedEnd       :: t
  } deriving (Eq, Ord, Show)

instance TimeFunctor Processed where
  mapTime f (Processed g b k d pk pg pb v bts end) = Processed
    (fmap (mapTime f) g)
    (fmap (mapTime f) b)
    (fmap (mapTime f) k)
    (fmap (mapTime f) d)
    (fmap (mapTime f) pk)
    (fmap (mapTime f) pg)
    (fmap (mapTime f) pb)
    (fmap (mapTime f) v)
    (mapTime f bts)
    (f end)

instance (Real t) => A.ToJSON (Processed t) where
  toJSON proc = A.object $ concat
    [ case processedGuitar    proc of Nothing -> []; Just x -> [("guitar"   , A.toJSON x)]
    , case processedBass      proc of Nothing -> []; Just x -> [("bass"     , A.toJSON x)]
    , case processedKeys      proc of Nothing -> []; Just x -> [("keys"     , A.toJSON x)]
    , case processedDrums     proc of Nothing -> []; Just x -> [("drums"    , A.toJSON x)]
    , case processedProKeys   proc of Nothing -> []; Just x -> [("prokeys"  , A.toJSON x)]
    , case processedProGuitar proc of Nothing -> []; Just x -> [("proguitar", A.toJSON x)]
    , case processedProBass   proc of Nothing -> []; Just x -> [("probass"  , A.toJSON x)]
    , case processedVocal     proc of Nothing -> []; Just x -> [("vocal"    , A.toJSON x)]
    , [("beats", A.toJSON $ processedBeats proc)]
    , [("end", A.Number $ realToFrac $ processedEnd proc)]
    ]

makeDisplay :: C.SongYaml -> RBFile.Song (RBFile.OnyxFile U.Beats) -> BL.ByteString
makeDisplay songYaml song = let
  ht n = fromIntegral n / 480
  gtr = flip fmap (C.getPart RBFile.FlexGuitar songYaml >>= C.partGRYBO) $ \grybo -> processFive (Just $ ht $ C.gryboHopoThreshold grybo) (RBFile.s_tempos song)
    $ discardPS $ RBFile.flexFiveButton $ RBFile.getFlexPart RBFile.FlexGuitar $ RBFile.s_tracks song
  bass = flip fmap (C.getPart RBFile.FlexBass songYaml >>= C.partGRYBO) $ \grybo -> processFive (Just $ ht $ C.gryboHopoThreshold grybo) (RBFile.s_tempos song)
    $ discardPS $ RBFile.flexFiveButton $ RBFile.getFlexPart RBFile.FlexBass $ RBFile.s_tracks song
  keys = flip fmap (C.getPart RBFile.FlexKeys songYaml >>= C.partGRYBO) $ \_ -> processFive Nothing (RBFile.s_tempos song)
    $ discardPS $ RBFile.flexFiveButton $ RBFile.getFlexPart RBFile.FlexKeys $ RBFile.s_tracks song
  drums = flip fmap (C.getPart RBFile.FlexDrums songYaml >>= C.partDrums) $ \_ -> processDrums (RBFile.s_tempos song)
    $ discardPS $ RBFile.flexPartDrums $ RBFile.getFlexPart RBFile.FlexDrums $ RBFile.s_tracks song
  prokeys = flip fmap (C.getPart RBFile.FlexKeys songYaml >>= C.partProKeys) $ \_ -> processProKeys (RBFile.s_tempos song)
    $ discardPS $ RBFile.flexPartRealKeysX $ RBFile.getFlexPart RBFile.FlexKeys $ RBFile.s_tracks song
  proguitar = flip fmap (C.getPart RBFile.FlexGuitar songYaml >>= C.partProGuitar) $ \pg -> processProtar (ht $ C.pgHopoThreshold pg) (RBFile.s_tempos song)
    $ let mustang = discardPS $ RBFile.flexPartRealGuitar   $ RBFile.getFlexPart RBFile.FlexGuitar $ RBFile.s_tracks song
          squier  = discardPS $ RBFile.flexPartRealGuitar22 $ RBFile.getFlexPart RBFile.FlexGuitar $ RBFile.s_tracks song
      in if RTB.null squier then mustang else squier
  probass = flip fmap (C.getPart RBFile.FlexBass songYaml >>= C.partProGuitar) $ \pg -> processProtar (ht $ C.pgHopoThreshold pg) (RBFile.s_tempos song)
    $ let mustang = discardPS $ RBFile.flexPartRealGuitar   $ RBFile.getFlexPart RBFile.FlexBass $ RBFile.s_tracks song
          squier  = discardPS $ RBFile.flexPartRealGuitar22 $ RBFile.getFlexPart RBFile.FlexBass $ RBFile.s_tracks song
      in if RTB.null squier then mustang else squier
  vox = flip fmap (C.getPart RBFile.FlexVocal songYaml >>= C.partVocal) $ \pvox -> case C.vocalCount pvox of
    C.Vocal3 -> makeVox
      (discardPS $ RBFile.flexHarm1 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song)
      (discardPS $ RBFile.flexHarm2 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song)
      (discardPS $ RBFile.flexHarm3 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song)
    C.Vocal2 -> makeVox
      (discardPS $ RBFile.flexHarm1 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song)
      (discardPS $ RBFile.flexHarm2 $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song)
      RTB.empty
    C.Vocal1 -> makeVox
      (discardPS $ RBFile.flexPartVocals $ RBFile.getFlexPart RBFile.FlexVocal $ RBFile.s_tracks song)
      RTB.empty
      RTB.empty
  makeVox h1 h2 h3 = processVocal (RBFile.s_tempos song) h1 h2 h3 (fmap fromEnum $ C._key $ C._metadata songYaml)
  beat = processBeat (RBFile.s_tempos song)
    $ discardPS $ RBFile.onyxBeat $ RBFile.s_tracks song
  end = U.applyTempoMap (RBFile.s_tempos song) $ songLengthBeats song
  in A.encode $ mapTime (realToFrac :: U.Seconds -> Milli)
    $ Processed gtr bass keys drums prokeys proguitar probass vox beat end
