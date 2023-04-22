{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Onyx.Neversoft.GH3 where

import           Control.Monad                    (forM, void)
import           Control.Monad.Trans.Writer       (execWriter, tell)
import           Data.Bits                        (bit, testBit, (.|.))
import qualified Data.ByteString                  as B
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (sort)
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
import qualified Data.Text                        as T
import           Data.Word                        (Word32)
import           Onyx.FeedBack.Load               (TrackEvent (..), emitTrack)
import           Onyx.Guitar                      (HOPOsAlgorithm (..), emit5',
                                                   guitarify', strumHOPOTap)
import           Onyx.MIDI.Common                 (Difficulty (..),
                                                   StrumHOPOTap (..))
import qualified Onyx.MIDI.Track.Drums            as D
import           Onyx.MIDI.Track.Drums.True
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.Neversoft.CRC               (qbKeyCRC)
import           Onyx.Neversoft.Metadata          (SongInfoGH3 (..))
import           Onyx.Neversoft.QB
import           Onyx.StackTrace
import qualified Sound.MIDI.Util                  as U

data GH3Track = GH3Track
  { gh3Notes       :: [(Word32, Word32, Word32)]
  , gh3StarPower   :: [(Word32, Word32, Word32)]
  , gh3BattleStars :: [(Word32, Word32, Word32)]
  } deriving (Show)

data GH3Part = GH3Part
  { gh3Easy   :: GH3Track
  , gh3Medium :: GH3Track
  , gh3Hard   :: GH3Track
  , gh3Expert :: GH3Track
  } deriving (Show)

data GH3Background a = GH3Background
  { gh3Scripts     :: [a]
  , gh3Anim        :: [a]
  , gh3Triggers    :: [a]
  , gh3Cameras     :: [a]
  , gh3LightShow   :: [a]
  , gh3Crowd       :: [a]
  , gh3Drums       :: [a]
  , gh3Performance :: [a]
  } deriving (Show)

data GH3AnimEvent = GH3AnimEvent
  { gh3AnimTime   :: Word32
  , gh3AnimScr    :: Word32
  , gh3AnimParams :: [QBStructItem Word32 Word32]
  } deriving (Show)

data GH3MidQB = GH3MidQB
  { gh3Guitar          :: GH3Part
  , gh3Rhythm          :: GH3Part
  , gh3CoopGuitar      :: GH3Part
  , gh3CoopRhythm      :: GH3Part

  , gh3P1FaceOff       :: [(Word32, Word32)]
  , gh3P2FaceOff       :: [(Word32, Word32)]
  , gh3P1BossBattle    :: [(Word32, Word32)] -- is this right? TODO check
  , gh3P2BossBattle    :: [(Word32, Word32)]

  , gh3TimeSignatures  :: [(Word32, Word32, Word32)]
  , gh3FretBars        :: [Word32]
  , gh3Markers         :: [(Word32, Either Word32 T.Text)] -- time, marker

  , gh3BackgroundNotes :: GH3Background [Word32] -- usually 3 ints, but some dlcX_drums_notes are 4 ints?
  , gh3Background      :: GH3Background GH3AnimEvent
  } deriving (Show)

emptyMidQB :: GH3MidQB
emptyMidQB = let
  emptyPart = GH3Part emptyTrack emptyTrack emptyTrack emptyTrack
  emptyTrack = GH3Track [] [] []
  emptyBackground = GH3Background [] [] [] [] [] [] [] []
  in GH3MidQB
    emptyPart emptyPart emptyPart emptyPart
    [] [] [] []
    [] [] []
    emptyBackground emptyBackground

findSection
  :: (Monad m)
  => [QBSection Word32 Word32]
  -> B.ByteString
  -> (QBArray Word32 Word32 -> StackTraceT m a)
  -> StackTraceT m a
findSection qb key go = inside ("QB section: " <> show key) $ do
  case [ary | QBSectionArray k _ ary <- qb, k == qbKeyCRC key] of
    [ary] -> go ary
    []    -> fatal "Section not found"
    _     -> fatal "Multiple sections with same key?"

groupBy3 :: (Monad m) => [w] -> StackTraceT m [(w, w, w)]
groupBy3 = go [] where
  go trips []                 = return $ reverse trips
  go trips (x : y : z : rest) = go ((x, y, z) : trips) rest
  go _     _                  = fatal "Expected a list whose length is a multiple of 3"

listOfPairs :: (Monad m) => QBArray Word32 Word32 -> StackTraceT m [(Word32, Word32)]
listOfPairs = \case
  QBArrayOfFloatRaw [] -> return []
  QBArrayOfArray arys -> forM arys $ \case
    QBArrayOfInteger [x, y] -> return (x, y)
    _                       -> fatal "Expected an array of 2 integers"
  _                   -> fatal "Expected array of arrays"

listOfTriples :: (Monad m) => QBArray Word32 Word32 -> StackTraceT m [(Word32, Word32, Word32)]
listOfTriples = \case
  QBArrayOfFloatRaw [] -> return []
  QBArrayOfArray arys -> forM arys $ \case
    QBArrayOfInteger [x, y, z] -> return (x, y, z)
    _                          -> fatal "Expected an array of 3 integers"
  _                   -> fatal "Expected array of arrays"

parsePart :: (Monad m) => B.ByteString -> [QBSection Word32 Word32] -> B.ByteString -> StackTraceT m GH3Part
parsePart dlc qb part = let
  parseTrack diff = do
    gh3Notes       <- findSection qb (dlc <> "_song_" <> part <> diff) $ \case
      QBArrayOfFloatRaw [] -> return []
      QBArrayOfInteger ns  -> groupBy3 ns
      _                    -> fatal "Expected array of integers for notes"
    gh3StarPower   <- findSection qb (dlc <> "_" <> part <> diff <> "_star") listOfTriples
    gh3BattleStars <- findSection qb (dlc <> "_" <> part <> diff <> "_starbattlemode") listOfTriples
    return GH3Track{..}
  in do
    gh3Easy   <- parseTrack "easy"
    gh3Medium <- parseTrack "medium"
    gh3Hard   <- parseTrack "hard"
    gh3Expert <- parseTrack "expert"
    return GH3Part{..}

parseAnimEvent :: (Monad m) => [QBStructItem Word32 Word32] -> StackTraceT m GH3AnimEvent
parseAnimEvent = \case
  QBStructHeader : struct -> do
    gh3AnimTime <- case [v | QBStructItemInteger810000 k v <- struct, k == qbKeyCRC "time"] of
      [t] -> return t
      _   -> fatal "Couldn't get time of anim event"
    gh3AnimScr <- case [v | QBStructItemQbKey8D0000 k v <- struct, k == qbKeyCRC "scr"] of
      [scr] -> return scr
      _     -> fatal "Couldn't get scr of anim event"
    gh3AnimParams <- case [v | QBStructItemStruct8A0000 k v <- struct, k == qbKeyCRC "params"] of
      [QBStructHeader : params] -> return params
      []                        -> return []
      _                         -> fatal "Couldn't get params of anim event"
    return GH3AnimEvent{..}
  _ -> fatal "Expected struct header in anim event"

parseBackground
  :: (Monad m)
  => B.ByteString
  -> [QBSection Word32 Word32]
  -> B.ByteString
  -> (QBArray Word32 Word32 -> StackTraceT m [a])
  -> StackTraceT m (GH3Background a)
parseBackground dlc qb sfx inner = do
  gh3Scripts     <- findSection qb (dlc <> "_scripts" <> sfx) inner
  gh3Anim        <- findSection qb (dlc <> "_anim" <> sfx) inner
  gh3Triggers    <- findSection qb (dlc <> "_triggers" <> sfx) inner
  gh3Cameras     <- findSection qb (dlc <> "_cameras" <> sfx) inner
  gh3LightShow   <- findSection qb (dlc <> "_lightshow" <> sfx) inner
  gh3Crowd       <- findSection qb (dlc <> "_crowd" <> sfx) inner
  gh3Drums       <- findSection qb (dlc <> "_drums" <> sfx) inner
  gh3Performance <- findSection qb (dlc <> "_performance" <> sfx) inner
  return GH3Background{..}

parseMidQB :: (Monad m) => B.ByteString -> [QBSection Word32 Word32] -> StackTraceT m GH3MidQB
parseMidQB dlc qb = do

  gh3Guitar     <- parsePart dlc qb ""
  gh3Rhythm     <- parsePart dlc qb "rhythm_"
  gh3CoopGuitar <- parsePart dlc qb "guitarcoop_"
  gh3CoopRhythm <- parsePart dlc qb "rhythmcoop_"

  gh3P1FaceOff    <- findSection qb (dlc <> "_faceoffp1") listOfPairs
  gh3P2FaceOff    <- findSection qb (dlc <> "_faceoffp2") listOfPairs
  gh3P1BossBattle <- findSection qb (dlc <> "_bossbattlep1") listOfPairs -- TODO check
  gh3P2BossBattle <- findSection qb (dlc <> "_bossbattlep2") listOfPairs -- TODO check

  gh3TimeSignatures <- findSection qb (dlc <> "_timesig") listOfTriples
  gh3FretBars       <- findSection qb (dlc <> "_fretbars") $ \case
    QBArrayOfFloatRaw [] -> return []
    QBArrayOfInteger ns  -> return ns
    _                    -> fatal "Expected array of integers for fretbars"
  gh3Markers        <- findSection qb (dlc <> "_markers") $ \case
    QBArrayOfFloatRaw [] -> return []
    QBArrayOfStruct marks -> forM marks $ \case
      QBStructHeader : items -> let
        time      = [v | QBStructItemInteger810000     k v <- items, k == qbKeyCRC "time"  ]
        markerKey = [v | QBStructItemQbKeyString9A0000 k v <- items, k == qbKeyCRC "marker"]
        -- seen in SanicStudios custom. do these work?
        markerStr = [v | QBStructItemStringW           k v <- items, k == qbKeyCRC "marker"]
        in case (time, markerKey, markerStr) of
          ([t], [m], []) -> return (t, Left m)
          ([t], [], [m]) -> return (t, Right m)
          _              -> fatal $ "Unexpected contents of marker: " <> show items
      _ -> fatal "No struct header in marker"
    _ -> fatal "Expected array of structs for markers"

  gh3BackgroundNotes <- parseBackground dlc qb "_notes" $ \case
    QBArrayOfFloatRaw [] -> return []
    QBArrayOfArray arys -> forM arys $ \case
      QBArrayOfInteger xs -> return xs
      _                   -> fatal "Expected array of integers"
    _                   -> fatal "Expected array of arrays"
  gh3Background      <- parseBackground dlc qb "" $ \case
    QBArrayOfFloatRaw [] -> return []
    QBArrayOfStruct entries -> mapM parseAnimEvent entries
    _ -> fatal "Expected array of structs for background events"

  return GH3MidQB{..}

makeMidQB :: Word32 -> B.ByteString -> GH3MidQB -> [QBSection Word32 Word32]
makeMidQB file dlc GH3MidQB{..} = execWriter $ do

  let makePart str GH3Part{..} = do
        makeDifficulty (str <> "easy"  ) gh3Easy
        makeDifficulty (str <> "medium") gh3Medium
        makeDifficulty (str <> "hard"  ) gh3Hard
        makeDifficulty (str <> "expert") gh3Expert
      makeDifficulty str GH3Track{..} = do
        makeSection ("_song_" <> str) $ QBArrayOfInteger $
          gh3Notes >>= \(x, y, z) -> [x, y, z]
        makeTriples ("_" <> str <> "_star") gh3StarPower
        makeTriples ("_" <> str <> "_starbattlemode") gh3BattleStars
      makePairs str pairs = makeSection str $ QBArrayOfArray $
        pairs >>= \(x, y) -> [QBArrayOfInteger [x, y]]
      makeTriples str triples = makeSection str $ QBArrayOfArray $
        triples >>= \(x, y, z) -> [QBArrayOfInteger [x, y, z]]
      makeBackground sfx GH3Background{..} inner = do
        () <- inner ("_scripts"     <> sfx) gh3Scripts
        inner       ("_anim"        <> sfx) gh3Anim
        inner       ("_triggers"    <> sfx) gh3Triggers
        inner       ("_cameras"     <> sfx) gh3Cameras
        inner       ("_lightshow"   <> sfx) gh3LightShow
        inner       ("_crowd"       <> sfx) gh3Crowd
        inner       ("_drums"       <> sfx) gh3Drums
        inner       ("_performance" <> sfx) gh3Performance
      makeSection str contents = tell [QBSectionArray (qbKeyCRC $ dlc <> str) file contents]

  makePart ""            gh3Guitar
  makePart "rhythm_"     gh3Rhythm
  makePart "guitarcoop_" gh3CoopGuitar
  makePart "rhythmcoop_" gh3CoopRhythm

  makePairs "_faceoffp1"    gh3P1FaceOff
  makePairs "_faceoffp2"    gh3P2FaceOff
  makePairs "_bossbattlep1" gh3P1BossBattle
  makePairs "_bossbattlep2" gh3P2BossBattle

  makeTriples "_timesig" gh3TimeSignatures
  makeSection "_fretbars" $ QBArrayOfInteger gh3FretBars
  makeSection "_markers" $ QBArrayOfStruct $ do
    (time, marker) <- gh3Markers
    return
      [ QBStructHeader
      , QBStructItemInteger810000 (qbKeyCRC "time") time
      , case marker of
        Left  k -> QBStructItemQbKeyString9A0000 (qbKeyCRC "marker") k
        Right s -> QBStructItemStringW (qbKeyCRC "marker") s
      ]

  makeBackground "_notes" gh3BackgroundNotes $ \str notes -> makeSection str $
    QBArrayOfArray $ map QBArrayOfInteger notes
  makeBackground "" gh3Background $ \str evts -> makeSection str $ QBArrayOfStruct $ do
    GH3AnimEvent{..} <- evts
    return
      [ QBStructHeader
      , QBStructItemInteger810000 (qbKeyCRC "time") gh3AnimTime
      , QBStructItemQbKey8D0000 (qbKeyCRC "scr") gh3AnimScr
      , QBStructItemStruct8A0000 (qbKeyCRC "params") $ QBStructHeader : gh3AnimParams
      ]

toSeconds :: Word32 -> U.Seconds
toSeconds = (/ 1000) . fromIntegral

readGH3TempoMap :: [(Word32, Word32, Word32)] -> [Word32] -> U.TempoMap
readGH3TempoMap sigs bars = let
  sigMap = Map.fromList [ (time, (num, den)) | (time, num, den) <- sigs ]
  barSigs = [ (t, maybe 4 (snd . snd) $ Map.lookupLE t sigMap) | t <- bars ]
  makeTempo (t1, denom) (t2, _) = let
    secs = toSeconds t2 - toSeconds t1
    beats = 4 / fromIntegral denom
    in (U.makeTempo beats secs, beats)
  temposGaps = zipWith makeTempo barSigs (drop 1 barSigs)
  in U.tempoMapFromBPS $ RTB.fromPairList
    $ zip (0 : map snd temposGaps) (map fst temposGaps)

gh3ToMidi :: SongInfoGH3 -> Bool -> Bool -> HM.HashMap Word32 T.Text -> GH3MidQB -> F.Song (F.OnyxFile U.Beats)
gh3ToMidi songInfo coopTracks coopRhythm bank gh3 = let
  tempos = readGH3TempoMap (gh3TimeSignatures gh3) (gh3FretBars gh3)
  toBeats :: Word32 -> U.Beats
  toBeats = U.unapplyTempoMap tempos . toSeconds
  fromPairs ps = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort ps
  -- AFAIK we can use the .chart HOPO algorithm.
  -- The threshold is in "hammer_on_measure_scale" - even though this says measure I think it means beat,
  -- where the threshold in beats is the reciprocal of this number.
  -- Default value is 2.95, as indicated by
  -- https://github.com/Nanook/TheGHOST/blob/27f143205d/TheGHOSTCore/QbHelpers/SongQb.cs#L188
  -- This is overridden by some songs to 1.95. These correspond to roughly the 66/192 and 100/192 mentioned here
  -- https://github.com/raynebc/editor-on-fire/blob/8d081c6cc2f/src/gh_import.c#L33
  hopoThreshold :: U.Beats
  hopoThreshold = realToFrac $ 1 / fromMaybe 2.95 (gh3HammerOnMeasureScale songInfo)
  -- Weird sustain threshold algorithm, see
  -- https://github.com/raynebc/editor-on-fire/blob/8d081c6cc2f/src/gh_import.c#L1537-L1546
  -- https://github.com/raynebc/editor-on-fire/blob/8d081c6cc2f/src/gh_import.c#L5814-L5832
  -- Note, sustains may also be dropped by the converted midi if they are too short!
  sustainThreshold :: Word32
  sustainThreshold = floor $ (U.applyTempoMap tempos 1 / 2) * 1000
  sustainTrim :: Word32
  sustainTrim = quot sustainThreshold 2
  getTrack trk = emit5' $ emitTrack hopoThreshold $ fromPairs $ do
    (time, len, bits) <- gh3Notes trk
    let pos = toBeats time
        lenTrimmed = if len > sustainThreshold
          then len - sustainTrim
          else 0
        lenBeats = toBeats (time + lenTrimmed) - pos
    concat
      [ [(pos, TrackNote (Just Five.Green ) lenBeats) | bits `testBit` 0]
      , [(pos, TrackNote (Just Five.Red   ) lenBeats) | bits `testBit` 1]
      , [(pos, TrackNote (Just Five.Yellow) lenBeats) | bits `testBit` 2]
      , [(pos, TrackNote (Just Five.Blue  ) lenBeats) | bits `testBit` 3]
      , [(pos, TrackNote (Just Five.Orange) lenBeats) | bits `testBit` 4]
      , [(pos, TrackForce                lenBeats) | bits `testBit` 5]
      ]
  getPart part = mempty
    { Five.fiveDifficulties = Map.fromList
      [ (Expert, getTrack $ gh3Expert part)
      , (Hard  , getTrack $ gh3Hard   part)
      , (Medium, getTrack $ gh3Medium part)
      , (Easy  , getTrack $ gh3Easy   part)
      ]
    , Five.fiveOverdrive = makeSpan $ fmap (\(time, len, _notecount) -> (time, len)) $ gh3StarPower $ gh3Expert part
    , Five.fivePlayer1 = makeSpan $ gh3P1FaceOff gh3
    , Five.fivePlayer2 = makeSpan $ gh3P2FaceOff gh3
    }
  makeSpan spans = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort $ do
    (time, len) <- spans
    [(toBeats time, True), (toBeats $ time + len, False)]
  trackLead = getPart $ if coopTracks then gh3CoopGuitar gh3 else gh3Guitar gh3
  trackCoop = getPart $ if coopTracks then gh3CoopRhythm gh3 else gh3Rhythm gh3
  events = mempty
    { eventsSections = fromPairs $ do
      (t, marker) <- gh3Markers gh3
      let str = case marker of
            Right s -> s
            Left  n -> case HM.lookup n bank of
              Just s  -> s
              Nothing -> T.pack $ show n
      return (toBeats t, (SectionRB2, str))
    }
  drums = gh3DrumsToFull toBeats $ gh3Drums $ gh3BackgroundNotes gh3
  fixed = mempty
    { F.onyxParts = Map.fromList
      [ ( F.FlexGuitar
        , mempty { F.onyxPartGuitar = trackLead }
        )
      , ( if coopRhythm then F.FlexExtra "rhythm" else F.FlexBass
        , mempty { F.onyxPartGuitar = trackCoop }
        )
      , ( F.FlexDrums
        , mempty { F.onyxPartTrueDrums = drums }
        )
      ]
    , F.onyxEvents = events
    }
  in F.Song
    { F.s_tempos = tempos
    , F.s_signatures = U.measureMapFromTimeSigs U.Truncate $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ do
      (time, num, den) <- gh3TimeSignatures gh3
      let unit = 4 / fromIntegral den
          len = fromIntegral num * unit
      return (toBeats time, U.TimeSig len unit)
    , F.s_tracks = fixed
    }

gh3DrumMapping :: [(Word32, (TrueGem, D.Hand))]
gh3DrumMapping =
  [ (36, (Kick  , D.LH)) -- second kick drum
  , (37, (Tom3  , D.LH))
  , (38, (Tom2  , D.LH))
  , (39, (Tom1  , D.LH))
  , (40, (Snare , D.LH))
  , (41, (Hihat , D.LH))
  , (42, (Hihat , D.LH)) -- duplicate?
  , (43, (Ride  , D.LH))
  , (44, (CrashL, D.LH))
  , (45, (CrashR, D.LH))
  --
  , (48, (Kick  , D.RH)) -- normal kick
  , (49, (Tom3  , D.RH))
  , (50, (Tom2  , D.RH))
  , (51, (Tom1  , D.RH))
  , (52, (Snare , D.RH))
  , (53, (Hihat , D.RH))
  , (54, (Hihat , D.RH))
  , (55, (Ride  , D.RH))
  , (56, (CrashL, D.RH))
  , (57, (CrashR, D.RH))
  ]

gh3DrumsToFull :: (Word32 -> U.Beats) -> [[Word32]] -> TrueDrumTrack U.Beats
gh3DrumsToFull toBeats notes = let
  fromPairs ps = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort ps
  allGems = flip mapMaybe notes $ \case
    time : pitch : _ -> (toBeats time,) <$> lookup pitch gh3DrumMapping
    _                -> Nothing
  kicks = fromPairs $ flip mapMaybe allGems $ \case
    (time, (Kick, hand)) -> Just (time, hand == D.RH)
    _                    -> Nothing
  hands = RTB.collectCoincident $ fromPairs $ filter
    (\(_time, (pad, _hand)) -> pad /= Kick)
    allGems
  handsNoSticking = fmap (map fst) hands
  in mempty
    { tdSticking = RTB.empty -- TODO
    , tdDifficulties = Map.singleton Expert mempty
      { tdGems
        = fmap (\fgem -> (fgem, TBDefault, D.VelocityNormal))
        $ RTB.merge (Kick <$ RTB.filter id kicks)
        $ RTB.flatten
        $ fmap nubOrd handsNoSticking
      , tdKick2 = void $ RTB.filter not kicks
      , tdFlam = flip RTB.mapMaybe handsNoSticking $ \case
        [x, y] | x == y -> Just ()
        _               -> Nothing
      }
    }

makeGH3TrackNotes
  :: U.TempoMap
  -> [(Word32, Word32, Word32)] -- time signatures in new gh3 mid
  -> [Word32] -- fretbars in new gh3 mid
  -> RTB.T U.Beats ((Five.Color, StrumHOPOTap), Maybe U.Beats)
  -> [(Word32, Word32, Word32)]
makeGH3TrackNotes tmap newSigs newFretbars notes = let
  newTempos = readGH3TempoMap newSigs newFretbars
  -- we translate from the original midi tempo map to the way gh3 will see it,
  -- for purposes of computing the default hopos (so we know when to force).
  -- sustain lengths aren't right here but they don't matter.
  defHOPOs = strumHOPOTap HOPOsGH3 hopoThreshold
    $ fmap (\((color, _), len) -> (color, len))
    $ U.unapplyTempoTrack newTempos $ U.applyTempoTrack tmap notes
  hopoThreshold :: U.Beats
  hopoThreshold = 1 / 2.95
  withForces = RTB.fromPairList $ zipWith findForce (RTB.toPairList notes) (RTB.toPairList defHOPOs)
  findForce (dt, ((color, sht), len)) (_, ((_, shtDefault), _)) = let
    force = (sht == Strum) /= (shtDefault == Strum)
    in (dt, ((color, force), len))
  sustainThreshold :: Word32
  sustainThreshold = floor $ (U.applyTempoMap tmap 1 / 2) * 1000
  sustainTrim :: Word32
  sustainTrim = quot sustainThreshold 2
  toMilli :: U.Beats -> Word32
  toMilli b = floor $ U.applyTempoMap tmap b * 1000
  eachNotes :: (U.Beats, ([(Five.Color, Bool)], Maybe U.Beats)) -> (Word32, Word32, Word32)
  eachNotes (pos, (gems, len)) = let
    posMS = toMilli pos
    lenMS = case len of
      Nothing      -> 1
      Just sustain -> toMilli (pos + sustain) - posMS + sustainTrim
    bits = foldr (.|.) 0 $ flip map gems $ \(gem, force) -> let
      force' = force && null (drop 1 gems) -- can't force chords
      in (if force' then bit 5 else 0) .|. case gem of
        Five.Green  -> bit 0
        Five.Red    -> bit 1
        Five.Yellow -> bit 2
        Five.Blue   -> bit 3
        Five.Orange -> bit 4
    in (posMS, lenMS, bits)
  in map eachNotes $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ guitarify' withForces
