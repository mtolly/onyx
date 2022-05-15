{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module RhythmGame.Track where

import           Config
import           Control.Applicative              ((<|>))
import           Control.Arrow                    (first)
import           Control.Monad                    (forM, guard)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                  as B
import           Data.Char                        (toLower)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Functor                     (($>))
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (sort, sortOn)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes, fromMaybe)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified FeedBack.Load                    as FB
import           Guitars
import           Numeric.NonNegative.Class        ((-|))
import qualified Reaper.Extract                   as RPP
import qualified Reaper.Parse                     as RPP
import qualified Reaper.Scan                      as RPP
import qualified RhythmGame.PNF                   as PNF
import qualified RockBand.Codec.Beat              as Beat
import qualified RockBand.Codec.Drums             as D
import           RockBand.Codec.Events            (eventsCoda, eventsSections)
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as F
import qualified RockBand.Codec.FullDrums         as FD
import qualified RockBand.Codec.ProGuitar         as PG
import           RockBand.Common
import           RockBand.Sections                (makePSSection)
import           RockBand3                        (BasicTiming (..),
                                                   basicTiming)
import           Rocksmith.ArrangementXML
import           Rocksmith.MIDI
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeExtension)
import           Text.Decode                      (decodeGeneral)
import           WebPlayer                        (findTremolos, findTrills,
                                                   laneDifficulty)

data PreviewTrack
  = PreviewDrums (Map.Map Double (PNF.CommonState (PNF.DrumState (D.Gem D.ProType, D.DrumVelocity) (D.Gem D.ProType))))
  | PreviewDrumsFull FullDrumLayout (Map.Map Double (PNF.CommonState (PNF.DrumState FD.FullDrumNote FD.FullGem)))
  | PreviewFive (Map.Map Double (PNF.CommonState (PNF.GuitarState (Maybe F.Color))))
  | PreviewPG PG.GtrTuning (Map.Map Double (PNF.CommonState (PNF.PGState Double)))
  deriving (Show)

data PreviewBG
  = PreviewBGVideo (VideoInfo FilePath)
  | PreviewBGImage FilePath
  -- TODO PreviewBGVenueRB
  deriving (Eq, Show)

data PreviewSong = PreviewSong
  { previewTempo    :: U.TempoMap
  , previewMeasures :: U.MeasureMap
  , previewTiming   :: BasicTiming
  , previewTracks   :: [[(T.Text, PreviewTrack)]]
  , previewBG       :: [(T.Text, PreviewBG)]
  , previewSections :: Map.Map Double T.Text
  } deriving (Show)

computeTracks
  :: (SendMessage m)
  => SongYaml FilePath
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> StackTraceT m PreviewSong
computeTracks songYaml song = basicTiming song (return 0) >>= \timing -> let

  parts = _parts songYaml
  rtbToMap
    = rtbToMapSecs
    . U.applyTempoTrack tempos
  rtbToMapSecs
    = Map.fromList
    . map (first realToFrac)
    . ATB.toPairList
    . RTB.toAbsoluteEventList 0
  tempos = RBFile.s_tempos song
  toggle
    = PNF.makeToggle
    . rtbToMap
    . RTB.normalize
  diffPairs = [(Expert, "X"), (Hard, "H"), (Medium, "M"), (Easy, "E")]
  drumDiffPairs = (Nothing, "X+") : [ (Just diff, letter) | (diff, letter) <- diffPairs ]

  makeLanes colors lanes = let
    gemToggle gem = U.trackJoin $ flip fmap lanes $ \(gem', t) -> if gem == gem'
      then Wait 0 True $ Wait t False RNil
      else RNil
    gemSingletons = do
      gem <- colors
      return $ fmap (Map.singleton gem) $ toggle $ gemToggle gem
    in foldr (\x y -> fmap (uncurry Map.union) $ PNF.zipStateMaps x y) Map.empty gemSingletons

  drumTrack fpart pdrums diff = let
    -- TODO support PS real
    -- TODO if kicks = 2, don't emit an X track, only X+
    drumSrc   = maybe mempty RBFile.onyxPartDrums   $ Map.lookup fpart $ RBFile.onyxParts $ RBFile.s_tracks song
    drumSrc2x = maybe mempty RBFile.onyxPartDrums2x $ Map.lookup fpart $ RBFile.onyxParts $ RBFile.s_tracks song
    thisSrc = if drumsMode pdrums == DrumsFull && D.nullDrums drumSrc && D.nullDrums drumSrc2x
      then maybe mempty (FD.convertFullDrums False . RBFile.onyxPartFullDrums)
        $ Map.lookup fpart $ RBFile.onyxParts $ RBFile.s_tracks song
      else case diff of
        Nothing -> if D.nullDrums drumSrc2x then drumSrc else drumSrc2x
        Just _  -> drumSrc
    drumMap :: Map.Map Double [(D.Gem D.ProType, D.DrumVelocity)]
    drumMap = rtbToMap $ RTB.collectCoincident drumPro
    drumPro = let
      -- quick 5 lane to 4 hack, eventually should actually support drawing 5-lane drums
      ddiff = D.getDrumDifficulty diff thisSrc
      in case drumsMode pdrums of
        Drums4    -> (\(gem, vel) -> (gem $> D.Tom, vel)) <$> ddiff
        Drums5    -> (\(gem, vel) -> (gem $> D.Tom, vel)) <$> D.fiveToFour
          (case drumsFallback pdrums of FallbackBlue -> D.Blue; FallbackGreen -> D.Green)
          ddiff
        DrumsPro  -> D.computePro diff thisSrc
        DrumsReal -> D.computePro diff $ D.psRealToPro thisSrc
        DrumsFull -> D.computePro diff thisSrc -- TODO support convert from full track
    hands = RTB.filter (/= D.Kick) $ fmap fst drumPro
    (acts, bres) = case fmap (fst . fst) $ RTB.viewL $ eventsCoda $ RBFile.onyxEvents $ RBFile.s_tracks song of
      Nothing   -> (D.drumActivation thisSrc, RTB.empty)
      Just coda ->
        ( U.trackTake coda $ D.drumActivation thisSrc
        , RTB.delay coda $ U.trackDrop coda $ D.drumActivation thisSrc
        )
    drumStates = (\((a, b), c) -> PNF.DrumState a b c) <$> do
      (Set.fromList <$> drumMap)
        `PNF.zipStateMaps` (let
          single = findTremolos hands $ laneDifficulty (fromMaybe Expert diff) $ D.drumSingleRoll thisSrc
          double = findTrills   hands $ laneDifficulty (fromMaybe Expert diff) $ D.drumDoubleRoll thisSrc
          in makeLanes (D.Red : (D.Pro <$> each <*> each)) (RTB.merge single double)
          )
        `PNF.zipStateMaps` toggle acts
    in do
      guard $ not $ Map.null drumMap
      guard $ not $ diff == Nothing && drumsKicks pdrums == Kicks1x
      Just $ (\((((a, b), c), d), e) -> PNF.CommonState a b c d e) <$> do
        drumStates
          `PNF.zipStateMaps` toggle (D.drumOverdrive thisSrc)
          `PNF.zipStateMaps` toggle bres
          `PNF.zipStateMaps` toggle (D.drumSolo thisSrc)
          `PNF.zipStateMaps` fmap Just beats

  drumTrackFull fpart pdrums diff = let
    -- TODO if kicks = 2, don't emit an X track, only X+
    thisSrc = maybe mempty RBFile.onyxPartFullDrums $ Map.lookup fpart $ RBFile.onyxParts $ RBFile.s_tracks song
    drumMap :: Map.Map Double [FD.FullDrumNote]
    drumMap = rtbToMap $ RTB.collectCoincident $ FD.getDifficulty diff thisSrc
    (acts, bres) = case fmap (fst . fst) $ RTB.viewL $ eventsCoda $ RBFile.onyxEvents $ RBFile.s_tracks song of
      Nothing   -> (FD.fdActivation thisSrc, RTB.empty)
      Just coda ->
        ( U.trackTake coda $ FD.fdActivation thisSrc
        , RTB.delay coda $ U.trackDrop coda $ FD.fdActivation thisSrc
        )
    drumStates = (\((a, b), c) -> PNF.DrumState a b c) <$> do
      (Set.fromList <$> drumMap)
        `PNF.zipStateMaps` (makeLanes each $ fmap (\((), gem, len) -> (gem, len)) $ joinEdgesSimple $ FD.fdLanes thisSrc)
        `PNF.zipStateMaps` toggle acts
    in do
      guard $ not $ Map.null drumMap
      guard $ not $ diff == Nothing && drumsKicks pdrums == Kicks1x
      Just $ (\((((a, b), c), d), e) -> PNF.CommonState a b c d e) <$> do
        drumStates
          `PNF.zipStateMaps` toggle (FD.fdOverdrive thisSrc)
          `PNF.zipStateMaps` toggle bres
          `PNF.zipStateMaps` toggle (FD.fdSolo thisSrc)
          `PNF.zipStateMaps` fmap Just beats

  fiveTrack fpart pgrybo diff = let
    -- TODO support multiple tracks (show both opens and no-opens versions, including auto no-opens conversion)
    (src, isKeys) = case Map.lookup fpart $ RBFile.onyxParts $ RBFile.s_tracks song of
      Nothing -> (mempty, False)
      Just part
        | not $ F.nullFive $ RBFile.onyxPartGuitarExt part -> (RBFile.onyxPartGuitarExt part, False)
        | not $ F.nullFive $ RBFile.onyxPartKeys      part -> (RBFile.onyxPartKeys      part, True )
        | otherwise                                        -> (RBFile.onyxPartGuitar    part, False)
    thisDiff = fromMaybe mempty $ Map.lookup diff $ F.fiveDifficulties src
    withOpens = openNotes' thisDiff
    ons = fmap fst withOpens
    assigned :: RTB.T U.Beats (LongNote Bool (Maybe F.Color, StrumHOPOTap))
    assigned
      = splitEdges
      $ fmap (\(a, (b, c)) -> (a, b, c))
      $ applyStatus1 False (RTB.normalize $ F.fiveOverdrive src)
      $ applyForces (getForces5 thisDiff)
      $ strumHOPOTap' (if isKeys then HOPOsRBKeys else HOPOsRBGuitar) hopoThreshold
      $ withOpens
    assignedMap :: Map.Map Double (Map.Map (Maybe F.Color) (PNF.PNF PNF.IsOverdrive StrumHOPOTap))
    assignedMap
      = rtbToMap
      $ buildFiveStatus PNF.empty
      $ RTB.collectCoincident
      $ assigned
    buildFiveStatus _    RNil                 = RNil
    buildFiveStatus prev (Wait dt edges rest) = let
      applyEdge edge = case edge of
        Blip _ (color, sht) -> Map.alter (\case
          Nothing             -> Just $ PNF.N sht
          Just PNF.Empty      -> Just $ PNF.N sht
          Just (PNF.P  wasOD) -> Just $ PNF.PN wasOD sht
          Just (PNF.PF wasOD) -> Just $ PNF.PN wasOD sht
          x                   -> x -- shouldn't happen
          ) color
        NoteOn od (color, sht) -> Map.alter (\case
          Nothing             -> Just $ PNF.NF sht od
          Just PNF.Empty      -> Just $ PNF.NF sht od
          Just (PNF.P  wasOD) -> Just $ PNF.PNF wasOD sht od
          Just (PNF.PF wasOD) -> Just $ PNF.PNF wasOD sht od
          x                   -> x -- shouldn't happen
          ) color
        NoteOff (color, _) -> Map.update (\case
          PNF.PF wasOD -> Just $ PNF.P wasOD
          x            -> Just x -- could happen if Blip or NoteOn was applied first
          ) color
      this = foldr applyEdge (PNF.after prev) edges
      in Wait dt this $ buildFiveStatus this rest
    hopoThreshold = fromIntegral (gryboHopoThreshold pgrybo) / 480 :: U.Beats
    fiveStates = (\((a, b), c) -> PNF.GuitarState a b c) <$> do
      assignedMap
        `PNF.zipStateMaps` (makeLanes (Nothing : map Just each) $ findTremolos ons $ laneDifficulty diff $ F.fiveTremolo src)
        `PNF.zipStateMaps` (makeLanes (Nothing : map Just each) $ findTrills   ons $ laneDifficulty diff $ F.fiveTrill   src)
    in do
      guard $ not $ RTB.null $ F.fiveGems thisDiff
      Just $ (\((((a, b), c), d), e) -> PNF.CommonState a b c d e) <$> do
        fiveStates
          `PNF.zipStateMaps` toggle (F.fiveOverdrive src)
          `PNF.zipStateMaps` toggle (F.fiveBRE src)
          `PNF.zipStateMaps` toggle (F.fiveSolo src)
          `PNF.zipStateMaps` fmap Just beats

  pgRocksmith rso = let
    notes = let
      eachString str = let
        stringNotes :: RTB.T U.Seconds (PNF.IsOverdrive, (StrumHOPOTap, (PG.GtrFret, PG.NoteType), Maybe (U.Seconds, Maybe PG.Slide)))
        stringNotes = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort $ do
          note <- V.toList (lvl_notes $ rso_level rso) <> do
            V.toList (lvl_chords $ rso_level rso) >>= V.toList . chd_chordNotes
          guard $ PG.getStringIndex 6 str == n_string note
          let time = realToFrac $ n_time note
              sht = if n_tap note then Tap
                else if n_hammerOn note || n_pullOff note then HOPO
                  else Strum
              fret = n_fret note
              ntype = PG.NormalNote -- TODO
              sust = flip fmap (n_sustain note) $ \len -> let
                slide = case n_slideTo note <|> n_slideUnpitchTo note of
                  Nothing -> Nothing
                  Just slideToFret -> case compare fret slideToFret of
                    LT -> Just PG.SlideUp
                    GT -> Just PG.SlideDown
                    EQ -> Nothing
                in (len, slide)
          return (time, (False, (sht, (fret, ntype), sust)))
        in rtbToMapSecs
          $ PNF.buildPNF
          $ RTB.fromAbsoluteEventList
          $ ATB.fromPairList
          $ fmap (\(t, (od, (sht, (fret, ntype), msust))) -> let
            now = PNF.PGNote fret ntype sht
            sust = case msust of
              Just (len, Nothing) -> Just (PNF.PGSustain Nothing od, len)
              Just (len, Just slide) -> let
                secsStart = realToFrac t
                secsEnd = realToFrac $ t <> len
                in Just (PNF.PGSustain (Just (slide, secsStart, secsEnd)) od, len)
              Nothing -> Nothing
            in (t, (now, sust))
            )
          $ ATB.toPairList
          $ RTB.toAbsoluteEventList 0 stringNotes
      strSingletons = do
        str <- [minBound .. maxBound]
        return $ Map.singleton str <$> eachString str
      in foldr (\x y -> fmap (uncurry Map.union) $ PNF.zipStateMaps x y) Map.empty strSingletons
    pgStates = (\(((a, b), c), d) -> PNF.PGState a b c d) <$> do
      notes
        `PNF.zipStateMaps` Map.empty -- TODO pgArea :: Maybe PG.StrumArea
        `PNF.zipStateMaps` Map.empty -- TODO pgChords :: PNF T.Text T.Text
        `PNF.zipStateMaps` Map.empty -- TODO pgArpeggio :: PNF (Map.Map PG.GtrString PG.GtrFret) ()
    in (\(((((a, b), c), d), e)) -> PNF.CommonState a b c d e) <$> do
      pgStates
        `PNF.zipStateMaps` Map.empty
        `PNF.zipStateMaps` Map.empty
        `PNF.zipStateMaps` Map.empty
        `PNF.zipStateMaps` fmap Just beats

  rsTracks fpart ppg = sequence $ let
    (srcG, srcB) = case Map.lookup fpart $ RBFile.onyxParts $ RBFile.s_tracks song of
      Nothing   -> (mempty, mempty)
      Just part -> (RBFile.onyxPartRSGuitar part, RBFile.onyxPartRSBass part)
    in catMaybes
      [ do
        guard $ not $ RTB.null $ rsNotes srcG
        let name = case fpart of
              RBFile.FlexGuitar               -> "RS Lead"
              RBFile.FlexExtra "rhythm"       -> "RS Rhythm"
              RBFile.FlexExtra "bonus-lead"   -> "RS Bonus Lead"
              RBFile.FlexExtra "bonus-rhythm" -> "RS Bonus Rhythm"
              _                               -> T.pack $ show fpart <> " [RS Guitar]"
        return $ (\rso -> (name, PreviewPG (pgTuning ppg) $ pgRocksmith rso)) <$> buildRS tempos 0 srcG
      , do
        guard $ not $ RTB.null $ rsNotes srcB
        let name = case fpart of
              RBFile.FlexBass               -> "RS Bass"
              RBFile.FlexExtra "bonus-bass" -> "RS Bonus Bass"
              _                             -> T.pack $ show fpart <> " [RS Bass]"
        return $ (\rso -> (name, PreviewPG (pgTuning ppg) $ pgRocksmith rso)) <$> buildRS tempos 0 srcB
      ]

  pgTrack fpart ppg diff = let
    src = case Map.lookup fpart $ RBFile.onyxParts $ RBFile.s_tracks song of
      Nothing -> mempty
      Just part
        | not $ PG.nullPG $ RBFile.onyxPartRealGuitar22 part -> RBFile.onyxPartRealGuitar22 part
        | otherwise                                          -> RBFile.onyxPartRealGuitar   part
    thisDiff = fromMaybe mempty $ Map.lookup diff $ PG.pgDifficulties src
    tuning = PG.tuningPitches (pgTuning ppg) { PG.gtrGlobal = 0 }
    flatDefault = maybe False songKeyUsesFlats $ _key $ _metadata songYaml
    chordNames = PG.computeChordNames diff tuning flatDefault src
    computed :: RTB.T U.Beats (StrumHOPOTap, [(PG.GtrString, PG.GtrFret, PG.NoteType)], Maybe (U.Beats, Maybe PG.Slide))
    computed = PG.guitarifyFull
      (fromIntegral (pgHopoThreshold ppg) / 480 :: U.Beats)
      thisDiff
    notes = let
      eachString str = let
        stringNotes :: RTB.T U.Beats (PNF.IsOverdrive, (StrumHOPOTap, (PG.GtrFret, PG.NoteType), Maybe (U.Beats, Maybe PG.Slide)))
        stringNotes = applyStatus1 False (RTB.normalize $ PG.pgOverdrive src) $
          flip RTB.mapMaybe computed $ \(sht, trips, sust) ->
            case [ (fret, ntype) | (str', fret, ntype) <- trips, str == str' ] of
              []       -> Nothing
              pair : _ -> Just (sht, pair, sust)
        in rtbToMap
          $ PNF.buildPNF
          $ RTB.fromAbsoluteEventList
          $ ATB.fromPairList
          $ fmap (\(t, (od, (sht, (fret, ntype), msust))) -> let
            now = PNF.PGNote fret ntype sht
            sust = case msust of
              Just (len, Nothing) -> Just (PNF.PGSustain Nothing od, len)
              Just (len, Just slide) -> let
                secsStart = realToFrac $ U.applyTempoMap tempos t
                secsEnd = realToFrac $ U.applyTempoMap tempos $ t <> len
                in Just (PNF.PGSustain (Just (slide, secsStart, secsEnd)) od, len)
              Nothing -> Nothing
            in (t, (now, sust))
            )
          $ ATB.toPairList
          $ RTB.toAbsoluteEventList 0 stringNotes
      strSingletons = do
        str <- [minBound .. maxBound]
        return $ Map.singleton str <$> eachString str
      in foldr (\x y -> fmap (uncurry Map.union) $ PNF.zipStateMaps x y) Map.empty strSingletons
    pgStates = (\(((a, b), c), d) -> PNF.PGState a b c d) <$> do
      notes
        `PNF.zipStateMaps` Map.empty -- TODO pgArea :: Maybe PG.StrumArea
        `PNF.zipStateMaps` Map.empty -- TODO pgChords :: PNF T.Text T.Text
        `PNF.zipStateMaps` Map.empty -- TODO pgArpeggio :: PNF (Map.Map PG.GtrString PG.GtrFret) ()
    in do
      guard $ not $ RTB.null $ PG.pgNotes thisDiff
      Just $ (\(((((a, b), c), d), e)) -> PNF.CommonState a b c d e) <$> do
        pgStates
          `PNF.zipStateMaps` toggle (PG.pgOverdrive src)
          `PNF.zipStateMaps` toggle (snd <$> PG.pgBRE src)
          `PNF.zipStateMaps` toggle (PG.pgSolo src)
          `PNF.zipStateMaps` fmap Just beats

  beats :: Map.Map Double (Maybe Beat.BeatEvent)
  beats = let
    sourceMidi = Beat.beatLines $ RBFile.onyxBeat $ RBFile.s_tracks song
    source = if RTB.null sourceMidi
      then Beat.beatLines $ timingBeat timing
      else sourceMidi
    makeBeats _         RNil            = RNil
    makeBeats _         (Wait 0 e rest) = Wait 0 (Just e) $ makeBeats False rest
    makeBeats firstLine (Wait t e rest) = let
      t' = t -| 0.5
      in (if firstLine then Wait 0 Nothing else id)
        $ RTB.delay (t - t')
        $ makeBeats True
        $ Wait (t -| 0.5) e rest
    in Map.fromList
      $ map (first $ realToFrac . U.applyTempoMap tempos)
      $ ATB.toPairList
      $ RTB.toAbsoluteEventList 0
      $ makeBeats True source

  tracks = fmap (filter $ not . null) $ forM (sortOn fst $ HM.toList $ getParts parts) $ \(fpart, part) -> let
    five = case partGRYBO part of
      Nothing     -> []
      Just pgrybo -> let
        name = case fpart of
          RBFile.FlexGuitar              -> "Guitar"
          RBFile.FlexBass                -> "Bass"
          RBFile.FlexKeys                -> "Keys"
          RBFile.FlexExtra "rhythm"      -> "Rhythm"
          RBFile.FlexExtra "guitar-coop" -> "Guitar Coop"
          _                              -> T.pack $ show fpart <> " [5]"
        in diffPairs >>= \(diff, letter) -> case fiveTrack fpart pgrybo diff of
          Nothing  -> []
          Just trk -> [(name <> " (" <> letter <> ")", PreviewFive trk)]
    drums = case partDrums part of
      Nothing     -> []
      Just pdrums -> let
        name = case fpart of
          RBFile.FlexDrums -> case drumsMode pdrums of
            Drums4    -> "Drums"
            Drums5    -> "Drums"
            DrumsPro  -> "Pro Drums"
            DrumsReal -> "Pro Drums"
            DrumsFull -> "Pro Drums"
          _                -> T.pack $ show fpart <> " [D]"
        in drumDiffPairs >>= \(diff, letter) -> case drumTrack fpart pdrums diff of
          Nothing  -> []
          Just trk -> [(name <> " (" <> letter <> ")", PreviewDrums trk)] ++ case drumsMode pdrums of
            DrumsFull -> case drumTrackFull fpart pdrums diff of
              Nothing -> []
              Just trkFull ->
                [ ( case fpart of
                    RBFile.FlexDrums -> "DTXMania Drums (" <> letter <> ")"
                    _                -> T.pack (show fpart) <> " Full Drums (" <> letter <> ")"
                  , PreviewDrumsFull (drumsFullLayout pdrums) trkFull
                  )
                ]
            _         -> []
    pg = case partProGuitar part of
      Nothing     -> []
      Just ppg -> let
        name = case fpart of
          RBFile.FlexGuitar -> "Pro Guitar"
          RBFile.FlexBass   -> "Pro Bass"
          _                 -> T.pack $ show fpart <> " [PG]"
        in diffPairs >>= \(diff, letter) -> case pgTrack fpart ppg diff of
          Nothing  -> []
          Just trk -> [(name <> " (" <> letter <> ")", PreviewPG (pgTuning ppg) trk)]
    rs = case partProGuitar part of
      Nothing  -> return []
      Just ppg -> rsTracks fpart ppg
    in ((five ++ drums ++ pg) ++) <$> rs

  bgs = concat
    [ case _backgroundVideo $ _global songYaml of
      Nothing -> []
      Just vi -> [("Background Video", PreviewBGVideo vi)]
    , case _fileBackgroundImage $ _global songYaml of
      Nothing -> []
      Just f  -> [("Background Image", PreviewBGImage f)]
    ]

  in tracks >>= \trk -> return $ PreviewSong
    { previewTempo    = RBFile.s_tempos song
    , previewMeasures = RBFile.s_signatures song
    , previewTiming   = timing
    , previewTracks   = trk
    , previewBG       = bgs
    , previewSections = rtbToMap
      $ fmap (snd . makePSSection . snd)
      $ eventsSections $ RBFile.onyxEvents $ RBFile.s_tracks song
    }

loadTracks
  :: (SendMessage m, MonadIO m)
  => SongYaml FilePath
  -> FilePath
  -> StackTraceT m PreviewSong
loadTracks songYaml f = do
  song <- case map toLower $ takeExtension f of
    ".rpp" -> do
      txt <- stackIO $ decodeGeneral <$> B.readFile f
      song <- RPP.scanStack txt >>= RPP.parseStack >>= RPP.getMIDI
      RBFile.interpretMIDIFile song
    ".chart" -> do
      chart <- FB.chartToBeats <$> FB.loadChartFile f
      FB.chartToMIDI chart >>= RBFile.readMIDIFile' . RBFile.showMIDIFile' -- TODO just convert fixed to onyx simply
    _ -> RBFile.loadMIDI f
  computeTracks songYaml song
