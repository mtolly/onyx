{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Reductions (pkReduce, drumsComplete, protarComplete, simpleReduce, completeFiveResult, completeDrumResult, FretLimit(..), loadFretLimit, limitFretsDifficulty) where

import           Control.Monad                     (forM, guard, void, (<=<))
import           Control.Monad.Codec               (codecIn, codecOut)
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Trans.State.Strict  (runStateT)
import           Control.Monad.Trans.Writer.Strict (execWriter)
import           Data.Bifunctor                    (first)
import qualified Data.EventList.Absolute.TimeBody  as ATB
import qualified Data.EventList.Relative.TimeBody  as RTB
import           Data.Function                     ((&))
import           Data.Functor                      ((<&>))
import           Data.List.Extra                   (nubOrd, sort, sortOn)
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                as NE
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe, isNothing,
                                                    mapMaybe)
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import           Numeric.NonNegative.Class         ((-|))
import qualified Numeric.NonNegative.Class         as NNC
import           Onyx.AutoChart                    (autoChart)
import           Onyx.FeedBack.Load                (loadRawMIDIOrChart)
import           Onyx.Guitar
import           Onyx.Keys.Ranges                  (completeRanges)
import           Onyx.MIDI.Common                  (Difficulty (..), Edge (..),
                                                    Key (..), StrumHOPOTap (..),
                                                    blipEdgesRB_, edgeBlips_,
                                                    minSustainLengthRB,
                                                    trackGlue)
import           Onyx.MIDI.Read                    (ParseTrack)
import qualified Onyx.MIDI.Track.Drums             as D
import           Onyx.MIDI.Track.Events            (EventsTrack, sections)
import qualified Onyx.MIDI.Track.File              as F
import           Onyx.MIDI.Track.FiveFret          as Five
import           Onyx.MIDI.Track.ProGuitar
import           Onyx.MIDI.Track.ProKeys           as PK
import           Onyx.Mode
import           Onyx.Preferences                  (Preferences (..),
                                                    readPreferences)
import           Onyx.Sections                     (Section)
import           Onyx.StackTrace
import           Optics                            ((.~))
import qualified Sound.MIDI.File.Event             as E
import qualified Sound.MIDI.File.Save              as Save
import qualified Sound.MIDI.Util                   as U

data FretLimit = AnyFrets | LimitedFrets

applyLimit :: FretLimit -> (a -> a) -> a -> a
applyLimit AnyFrets     _ x = x
applyLimit LimitedFrets f x = f x

loadFretLimit :: (SendMessage m, MonadIO m) => StackTraceT m FretLimit
loadFretLimit = readPreferences <&> \prefs ->
  if prefs.prefReduceFrets then LimitedFrets else AnyFrets

completeFiveResult :: FretLimit -> Bool -> U.MeasureMap -> FiveResult -> FiveResult
completeFiveResult limit isKeys mmap result = let
  od        = result.other.overdrive
  getDiff d = fromMaybe RTB.empty $ Map.lookup d result.notes
  trk1 `orIfNull` trk2 = if length (RTB.collectCoincident trk1) <= 5 then trk2 else trk1
  expert    = getDiff Expert
  hard      = getDiff Hard   `orIfNull` gryboReduce Hard isKeys mmap od expert
  medium    = getDiff Medium `orIfNull` applyLimit limit (limitFrets 4) (gryboReduce Medium isKeys mmap od hard  )
  easy      = getDiff Easy   `orIfNull` applyLimit limit (limitFrets 3) (gryboReduce Easy   isKeys mmap od medium)
  in result & #notes .~ Map.fromList
    [ (Easy  , easy  )
    , (Medium, medium)
    , (Hard  , hard  )
    , (Expert, expert)
    ]

limitFretsMaybe
  :: Int
  -> RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)
  -> Maybe (RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats))
limitFretsMaybe maxFrets input = let
  autoInput = input <&> \((mc, _), _) -> maybe 0 fromEnum mc
  maxUsedFret = foldr max 0 autoInput
  autoOutput
    = autoChart maxFrets
    $ map (first realToFrac)
    $ ATB.toPairList
    $ RTB.toAbsoluteEventList 0 autoInput
  autoMap = foldr (Map.unionWith (<>)) Map.empty $ map
    (\(pos, fret) -> Map.singleton (realToFrac pos) [fret])
    autoOutput
  outputGems
    = RTB.fromAbsoluteEventList
    $ ATB.fromPairList
    $ concatMap (\(posn, (sht, len)) -> let
      frets = fromMaybe [0] $ Map.lookup posn autoMap
      in [ (posn, ((Just $ toEnum fret, sht), len)) | fret <- frets ]
      )
    $ ATB.toPairList
    $ RTB.toAbsoluteEventList 0
    $ fmap (\notes ->
      ( maximum [ sht | ((_, sht), _  ) <- notes ]
      , minimum [ len | ((_, _  ), len) <- notes ]
      ))
    $ RTB.collectCoincident input
  in if maxUsedFret < maxFrets
    then Nothing
    else Just outputGems

limitFrets
  :: Int
  -> RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)
  -> RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)
limitFrets maxFrets input = fromMaybe input $ limitFretsMaybe maxFrets input

limitFretsDifficulty :: Int -> Maybe U.Beats -> FiveDifficulty U.Beats -> FiveDifficulty U.Beats
limitFretsDifficulty maxFrets hopoThres fd = let
  maybeLimited
    = limitFretsMaybe maxFrets
    $ noExtendedSustains' standardBlipThreshold standardSustainGap
    $ (case hopoThres of
      Nothing -> fmap (\(col, len) -> ((col, Strum), len))
      Just ht -> applyForces (getForces5 fd) . strumHOPOTap HOPOsRBGuitar ht
      )
    $ computeFiveFretNotes fd
  in case maybeLimited of
    Nothing  -> fd
    Just new -> (emitGuitar5 new)
      { forceStrum = RTB.empty, forceHOPO = RTB.empty, tap = RTB.empty }

-- | Fills out a GRYBO chart by generating difficulties if needed.
gryboComplete :: FretLimit -> Maybe U.Beats -> U.MeasureMap -> FiveTrack U.Beats -> FiveTrack U.Beats
gryboComplete limit hopoThres mmap trk = let
  od        = trk.overdrive
  getDiff d = fromMaybe mempty $ Map.lookup d trk.difficulties
  orIfNull :: FiveDifficulty U.Beats -> FiveDifficulty U.Beats -> FiveDifficulty U.Beats
  trk1 `orIfNull` trk2 = if length (RTB.collectCoincident trk1.gems) <= 5 then trk2 else trk1
  expert    = getDiff Expert
  hard      = getDiff Hard   `orIfNull` gryboReduceDifficulty Hard   hopoThres mmap od expert
  medium    = getDiff Medium `orIfNull` applyLimit limit (limitFretsDifficulty 4 hopoThres) (gryboReduceDifficulty Medium hopoThres mmap od hard  )
  easy      = getDiff Easy   `orIfNull` applyLimit limit (limitFretsDifficulty 3 hopoThres) (gryboReduceDifficulty Easy   hopoThres mmap od medium)
  in trk & #difficulties .~ Map.fromList
    [ (Easy, easy)
    , (Medium, medium)
    , (Hard, hard)
    , (Expert, expert)
    ]

gryboReduceDifficulty
  :: Difficulty
  -> Maybe U.Beats          -- ^ HOPO threshold if guitar or bass, Nothing if keys
  -> U.MeasureMap
  -> RTB.T U.Beats Bool     -- ^ Overdrive phrases
  -> FiveDifficulty U.Beats -- ^ The source difficulty, one level up
  -> FiveDifficulty U.Beats -- ^ The target difficulty
gryboReduceDifficulty diff hopoThres mmap od fd
  = (\result -> if isNothing hopoThres || elem diff [Easy, Medium]
    then result { forceStrum = RTB.empty, forceHOPO = RTB.empty, tap = RTB.empty }
    else result
    )
  $ emitGuitar5
  $ gryboReduce diff (isNothing hopoThres) mmap od
  $ (case hopoThres of
    Nothing -> fmap (\(col, len) -> ((col, Strum), len))
    Just ht -> applyForces (getForces5 fd) . strumHOPOTap HOPOsRBGuitar ht
    )
  $ computeFiveFretNotes fd

gryboReduce
  :: Difficulty
  -> Bool                                                            -- ^ is keys?
  -> U.MeasureMap
  -> RTB.T U.Beats Bool                                              -- ^ Overdrive phrases
  -> RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats) -- ^ The source difficulty, one level up
  -> RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats) -- ^ The target difficulty

gryboReduce Expert _      _    _  diffNotes = diffNotes
gryboReduce diff   isKeys mmap od diffNotes = let
  odMap = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.normalize od
  gnotes1 = readGuitarNotes diffNotes
  isOD bts = maybe False snd $ Map.lookupLE bts odMap
  -- TODO: replace the next 2 with BEAT track
  isMeasure bts = snd (U.applyMeasureMap mmap bts) == 0
  isAligned divn bts = let
    beatsWithinMeasure = snd $ U.applyMeasureMap mmap bts
    (_, frac) = properFraction $ beatsWithinMeasure / divn :: (Int, U.Beats)
    in frac == 0
  -- Step: simplify grace notes
  gnotes2 = RTB.fromAbsoluteEventList $ ATB.fromPairList $ disgrace $ ATB.toPairList $ RTB.toAbsoluteEventList 0 gnotes1
  disgrace = \case
    [] -> []
    (t1, GuitarNote _ Strum Nothing) : (t2, GuitarNote cols HOPO (Just len)) : rest
      | t2 - t1 <= 0.25 && isAligned 0.5 t1 && not (not (isOD t1) && isOD t2)
      -> (t1, GuitarNote cols Strum $ Just $ len + t2 - t1) : disgrace rest
    x : xs -> x : disgrace xs
  -- Step: simplify 3-note (or more) chords and GO chords (or GB and RO chords on medium)
  gnotes3 = gnotes2 <&> \(GuitarNote cols ntype len) -> let
    cols' = case diff of
      Hard   -> case cols of
        _ : _ : _ : _ -> case (minimum cols, maximum cols) of
          (Five.Green, Five.Orange) -> [Five.Green, Five.Blue]
          (mincol, maxcol)          -> [mincol, maxcol]
        [Five.Green, Five.Orange] -> [Five.Green, Five.Blue]
        [Five.Orange, Five.Green] -> [Five.Green, Five.Blue]
        _ -> cols
      Medium -> case cols of
        [x] -> [x]
        _ -> case (minimum cols, maximum cols) of
          (Five.Green , c2) -> [Five.Green, min c2 Five.Yellow]
          (Five.Red   , c2) -> [Five.Red, min c2 Five.Blue]
          (c1         , c2) -> [c1, c2]
      Easy -> [minimum cols]
    in GuitarNote cols' ntype len
  -- Step: start marking notes as kept according to these (in order of importance):
  -- 1. Look at OD phrases first
  -- 2. Keep sustains first
  -- 3. Keep notes on a measure line first
  -- 4. Keep notes aligned to an 8th-note within a measure first
  -- 5. Finally, look at notes in chronological order
  -- TODO: maybe also prioritize strums over hopos, and chords over single notes?
  -- TODO: this order causes some oddness when OD phrases make adjacent notes disappear
  sortedPositions = map snd $ sort do
    (bts, gnote) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 gnotes3
    let isSustain = case gnote of
          GuitarNote _ _ (Just _) -> True
          _                       -> False
        priority :: Int
        priority = sum
          [ if isOD           bts then 0 else 8
          , if isSustain          then 0 else 4
          , if isMeasure      bts then 0 else 2
          , if isAligned divn bts then 0 else 1
          ]
        divn = case diff of
          Hard   -> 0.5
          Medium -> 1
          Easy   -> 2
    return (priority, bts)
  handlePosns kept []             = kept
  handlePosns kept (posn : posns) = let
    padding = case diff of
      Hard   -> 0.5
      Medium -> 1
      Easy   -> 2
    slice = fst $ Set.split (posn + padding) $ snd $ Set.split (posn -| padding) kept
    in if Set.null slice
      then handlePosns (Set.insert posn kept) posns
      else handlePosns kept posns
  keptPosns = handlePosns Set.empty sortedPositions
  gnotes4
    = RTB.fromAbsoluteEventList $ ATB.fromPairList
    $ filter (\(t, _) -> Set.member t keptPosns)
    $ ATB.toPairList $ RTB.toAbsoluteEventList 0 gnotes3
  -- Step: for hard, only hopo chords and sustained notes
  -- for easy/medium, all strum
  gnotes5 = case diff of
    Hard -> gnotes4 <&> \case
      GuitarNote [col] HOPO Nothing -> GuitarNote [col] Strum Nothing
      gnote                         -> gnote
    _ -> (\(GuitarNote cols _ len) -> GuitarNote cols Strum len) <$> gnotes4
  -- Step: chord->hopochord becomes note->hopochord
  gnotes6 = RTB.fromPairList $ fixDoubleHopoChord $ RTB.toPairList gnotes5
  fixDoubleHopoChord = \case
    [] -> []
    (t1, GuitarNote cols@(_ : _ : _) ntype len) : rest@((_, GuitarNote (_ : _ : _) HOPO _) : _)
      -> (t1, GuitarNote [minimum cols] ntype len) : fixDoubleHopoChord rest
    x : xs -> x : fixDoubleHopoChord xs
  -- Step: fix quick jumps (GO for hard, GB/RO for medium/easy)
  gnotes7 = if isKeys
    then gnotes6
    else RTB.fromPairList $ fixJumps $ RTB.toPairList gnotes6
  jumpDiff = case diff of
    Hard   -> 3
    Medium -> 2
    Easy   -> 2
  jumpTime = case diff of
    Hard   -> 0.5
    Medium -> 1
    Easy   -> 2
  fixJumps = \case
    [] -> []
    tnote1@(_, GuitarNote cols1 _ len1) : (t2, GuitarNote cols2 ntype2 len2) : rest
      | fromEnum (maximum cols2) - fromEnum (minimum cols1) > jumpDiff
      && (t2 <= jumpTime || case len1 of Nothing -> False; Just l1 -> t2 -| l1 <= jumpTime)
      -> tnote1 : fixJumps ((t2, GuitarNote (map pred cols2) ntype2 len2) : rest)
      | fromEnum (maximum cols1) - fromEnum (minimum cols2) > jumpDiff
      && (t2 <= jumpTime || case len1 of Nothing -> False; Just l1 -> t2 -| l1 <= jumpTime)
      -> tnote1 : fixJumps ((t2, GuitarNote (map succ cols2) ntype2 len2) : rest)
    x : xs -> x : fixJumps xs
  -- Step: fix HOPO notes with the same high fret as the note before them
  gnotes8 = RTB.fromPairList $ fixHOPOs $ RTB.toPairList gnotes7
  fixHOPOs = \case
    [] -> []
    tnote1@(_, GuitarNote cols1 _ _) : (t2, GuitarNote cols2 HOPO len2) : rest
      | maximum cols1 == maximum cols2
      -> tnote1 : fixHOPOs ((t2, GuitarNote cols2 Strum len2) : rest)
    x : xs -> x : fixHOPOs xs
  -- Step: bring back sustains for quarter note gap on medium/easy
  gnotes9 = case diff of
    Hard -> gnotes8
    _    -> RTB.fromPairList $ pullBackSustains $ RTB.toPairList gnotes8
  pullBackSustains = \case
    [] -> []
    (t1, GuitarNote cols1 ntype1 (Just l1)) : rest@((t2, _) : _) -> let
      l1' = min l1 $ t2 -| 1
      len1' = guard (l1' >= 1) >> Just l1'
      in (t1, GuitarNote cols1 ntype1 len1') : pullBackSustains rest
    x : xs -> x : pullBackSustains xs
  in showGuitarNotes gnotes9

data GuitarNote t = GuitarNote [Five.Color] StrumHOPOTap (Maybe t)
  deriving (Eq, Ord, Show)

readGuitarNotes :: RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats) -> RTB.T U.Beats (GuitarNote U.Beats)
readGuitarNotes
  = fmap (\(notes, len) -> GuitarNote (map fst notes) (snd $ head notes) len)
  . guitarify'
  . noOpenNotes True -- doesn't really matter if we do mute-strums detection

showGuitarNotes :: RTB.T U.Beats (GuitarNote U.Beats) -> RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)
showGuitarNotes trk = RTB.flatten $ trk <&> \case
  GuitarNote colors sht mlen -> map (\color -> ((Just color, sht), mlen)) colors

data PKNote t = PKNote [PK.Pitch] (Maybe t)
  deriving (Eq, Ord, Show)

readPKNotes :: ProKeysTrack U.Beats -> RTB.T U.Beats (PKNote U.Beats)
readPKNotes = fmap (uncurry PKNote) . guitarify' . edgeBlips_ minSustainLengthRB . (.notes)

showPKNotes :: RTB.T U.Beats (PKNote U.Beats) -> ProKeysTrack U.Beats
showPKNotes pk = mempty & #notes .~ blipEdgesRB_ (RTB.flatten $ fmap f pk) where
  f (PKNote ps len) = map (, len) ps

pkReduce
  :: Difficulty
  -> U.MeasureMap
  -> RTB.T U.Beats Bool   -- ^ Overdrive phrases
  -> ProKeysTrack U.Beats -- ^ The source difficulty, one level up
  -> ProKeysTrack U.Beats -- ^ The target difficulty

pkReduce Expert _    _  diffEvents = diffEvents
pkReduce diff   mmap od diffEvents = let
  odMap = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.normalize od
  pknotes1 = readPKNotes diffEvents
  isOD bts = maybe False snd $ Map.lookupLE bts odMap
  -- TODO: replace the next 2 with BEAT track
  isMeasure bts = snd (U.applyMeasureMap mmap bts) == 0
  isAligned divn bts = let
    beatsWithinMeasure = snd $ U.applyMeasureMap mmap bts
    (_, frac) = properFraction $ beatsWithinMeasure / divn :: (Int, U.Beats)
    in frac == 0
  -- Step: simplify grace notes
  pknotes2 = RTB.fromAbsoluteEventList $ ATB.fromPairList $ disgrace $ ATB.toPairList $ RTB.toAbsoluteEventList 0 pknotes1
  disgrace = \case
    [] -> []
    (t1, PKNote _ Nothing) : (t2, PKNote ps (Just len)) : rest
      | t2 - t1 <= 0.25 && isAligned 0.5 t1 && not (not (isOD t1) && isOD t2)
      -> (t1, PKNote ps $ Just $ len + t2 - t1) : disgrace rest
    x : xs -> x : disgrace xs
  -- Step: simplify chords
  pknotes3 = pknotes2 <&> \(PKNote ps len) -> let
    ps' = case diff of
      Hard   -> case ps of
        [x] -> [x]
        [x, y] -> [x, y]
        _    -> let
          (p1, p3) = (minimum ps, maximum ps)
          p2 = minimum $ filter (`notElem` [p1, p3]) ps
          in [p1, p2, p3]
      Medium -> case ps of
        [x] -> [x]
        _   -> [minimum ps, maximum ps]
      Easy   -> [maximum ps]
    in PKNote ps' len
  -- Step: start marking notes as kept according to these (in order of importance):
  -- 1. Look at OD phrases first
  -- 2. Keep sustains first
  -- 3. Keep notes on a measure line first
  -- 4. Keep notes aligned to an 8th-note within a measure first
  -- 5. Finally, look at notes in chronological order
  -- TODO: maybe also prioritize chords over single notes?
  -- TODO: this order causes some oddness when OD phrases make adjacent notes disappear
  sortedPositions = map snd $ sort do
    (bts, pknote) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 pknotes3
    let isSustain = case pknote of
          PKNote _ (Just _) -> True
          _                 -> False
        priority :: Int
        priority = sum
          [ if isOD           bts then 0 else 8
          , if isSustain          then 0 else 4
          , if isMeasure      bts then 0 else 2
          , if isAligned divn bts then 0 else 1
          ]
        divn = case diff of
          Hard   -> 0.5
          Medium -> 1
          Easy   -> 2
    return (priority, bts)
  handlePosns kept []             = kept
  handlePosns kept (posn : posns) = let
    padding = case diff of
      Hard   -> 0.5
      Medium -> 1
      Easy   -> 2
    slice = fst $ Set.split (posn + padding) $ snd $ Set.split (posn -| padding) kept
    in if Set.null slice
      then handlePosns (Set.insert posn kept) posns
      else handlePosns kept posns
  keptPosns = handlePosns Set.empty sortedPositions
  pknotes4
    = RTB.fromAbsoluteEventList $ ATB.fromPairList
    $ filter (\(t, _) -> Set.member t keptPosns)
    $ ATB.toPairList $ RTB.toAbsoluteEventList 0 pknotes3
  -- Step: fix quick jumps (TODO) and out-of-range notes on easy/medium
  pknotes5 = if elem diff [Expert, Hard]
    then pknotes4
    else pknotes4 <&> \(PKNote ps len) -> let
      -- we'll use range F-A for easy/medium
      ps' = nubOrd $ ps <&> \case
        PK.RedYellow k -> if k < F
          then PK.BlueGreen k
          else PK.RedYellow k
        PK.BlueGreen k -> if A < k
          then PK.RedYellow k
          else PK.BlueGreen k
        PK.OrangeC -> PK.BlueGreen C
      in PKNote ps' len
  -- Step: bring back sustains for quarter note gap on medium/easy
  pknotes6 = case diff of
    Hard -> pknotes5
    _    -> RTB.fromPairList $ pullBackSustains $ RTB.toPairList pknotes5
  pullBackSustains = \case
    [] -> []
    (t1, PKNote ps1 (Just l1)) : rest@((t2, _) : _) -> let
      l1' = min l1 $ t2 -| 1
      len1' = guard (l1' >= 1) >> Just l1'
      in (t1, PKNote ps1 len1') : pullBackSustains rest
    x : xs -> x : pullBackSustains xs
  -- Step: redo range shifts
  in completeRanges (showPKNotes pknotes6) { lanes = RTB.empty }

completeDrumResult :: U.MeasureMap -> RTB.T U.Beats Section -> DrumResult -> DrumResult
completeDrumResult mmap sections result = let
  od        = result.other.overdrive
  getDiff d = fromMaybe RTB.empty $ Map.lookup d result.notes
  trk1 `orIfNull` trk2 = if length (RTB.collectCoincident trk1) <= 5 then trk2 else trk1
  expert    = getDiff Expert
  hard      = getDiff Hard   `orIfNull` defVelocity (drumsReduce Hard   mmap od sections (noVelocity expert))
  medium    = getDiff Medium `orIfNull` defVelocity (drumsReduce Medium mmap od sections (noVelocity hard  ))
  easy      = getDiff Easy   `orIfNull` defVelocity (drumsReduce Easy   mmap od sections (noVelocity medium))
  defVelocity = fmap \gem -> (gem, D.VelocityNormal)
  noVelocity = fmap fst
  in result & #notes .~ Map.fromList
    [ (Easy  , easy  )
    , (Medium, medium)
    , (Hard  , hard  )
    , (Expert, expert)
    ]

drumsComplete
  :: U.MeasureMap
  -> RTB.T U.Beats Section
  -> D.DrumTrack U.Beats
  -> D.DrumTrack U.Beats
drumsComplete mmap sections trk = let
  od       = trk.overdrive
  getPro d = fmap fst $ D.computePro (Just d) trk
  getRaw d = fromMaybe mempty $ Map.lookup d trk.difficulties
  reduceStep diff source = let
    raw = getRaw diff
    in if length raw.gems <= 5
      then let
        auto = drumsReduce diff mmap od sections source
        in (proToDiff auto, auto)
      else (raw, getPro diff)
  (expertRaw, expert) = (getRaw Expert, getPro Expert)
  (hardRaw  , hard  ) = reduceStep Hard   expert
  (mediumRaw, _     ) = reduceStep Medium hard
  (easyRaw  , _     ) = reduceStep Easy   hard -- we want kicks that medium might've removed
  proToDiff pro = mempty { D.gems = fmap (\gem -> (void gem, D.VelocityNormal)) pro }
  in trk
    { D.difficulties = Map.fromList
      [ (Easy  , easyRaw  )
      , (Medium, mediumRaw)
      , (Hard  , hardRaw  )
      , (Expert, expertRaw)
      ]
    }

ensureODNotes
  :: (NNC.C t, Ord a)
  => RTB.T t Bool -- ^ Overdrive phrases
  -> RTB.T t a    -- ^ Original notes
  -> RTB.T t a    -- ^ Reduced notes
  -> RTB.T t a
ensureODNotes = go . RTB.normalize where
  go od original reduced = case RTB.viewL od of
    Just ((dt, True), od') -> let
      odLength = case RTB.viewL od' of
        Just ((len, _), _) -> U.trackTake len
        Nothing            -> id
      original' = U.trackDrop dt original
      reduced' = U.trackDrop dt reduced
      in trackGlue dt reduced if RTB.null $ odLength reduced'
        then go od' original' case RTB.viewL original' of
          Just ((originalTime, originalEvent), _) -> RTB.insert originalTime originalEvent reduced'
          Nothing -> reduced'
        else go od' original' reduced'
    Just ((dt, False), od') -> trackGlue dt reduced $ go od' (U.trackDrop dt original) (U.trackDrop dt reduced)
    Nothing -> reduced

drumsReduce
  :: Difficulty
  -> U.MeasureMap
  -> RTB.T U.Beats Bool              -- ^ Overdrive phrases
  -> RTB.T U.Beats Section
  -> RTB.T U.Beats (D.Gem D.ProType) -- ^ The source difficulty, one level up
  -> RTB.T U.Beats (D.Gem D.ProType) -- ^ The target difficulty
drumsReduce Expert _    _  _        trk = trk
drumsReduce diff   mmap od sections trk = let
  -- odMap = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.normalize od
  -- isOD bts = fromMaybe False $ fmap snd $ Map.lookupLE bts odMap
  isMeasure bts = snd (U.applyMeasureMap mmap bts) == 0
  isAligned divn bts = let
    beatsWithinMeasure = snd $ U.applyMeasureMap mmap bts
    (_, frac) = properFraction $ beatsWithinMeasure / divn :: (Int, U.Beats)
    in frac == 0
  priority :: U.Beats -> Int
  priority bts = sum
    [ if isMeasure     bts then 0 else 1
    , if isAligned 2   bts then 0 else 1
    , if isAligned 1   bts then 0 else 1
    , if isAligned 0.5 bts then 0 else 1
    ]
  snares = ATB.getTimes $ RTB.toAbsoluteEventList 0 $ RTB.filter (== D.Red) trk
  keepSnares kept [] = kept
  keepSnares kept (posn : rest) = let
    padding = if diff == Hard then 0.5 else 1
    slice = fst $ Map.split (posn + padding) $ snd $ Map.split (posn -| padding) kept
    in if Map.null slice
      then keepSnares (Map.insert posn [D.Red] kept) rest
      else keepSnares kept rest
  keptSnares = keepSnares Map.empty $ sortOn (\bts -> (priority bts, bts)) snares
  kit = ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.collectCoincident $ RTB.filter (`notElem` [D.Red, D.Kick]) trk
  keepKit kept [] = kept
  keepKit kept ((posn, gems) : rest) = let
    gems' = case sort gems of
      [D.Pro _ D.Cymbal, green@(D.Pro D.Green D.Cymbal)]      -> [green]
      [D.Pro D.Yellow D.Cymbal, blue@(D.Pro D.Blue D.Cymbal)] -> [blue]
      [tom1@(D.Pro _ D.Tom), D.Pro _ D.Tom] | diff <= Medium  -> [tom1]
      _                                                       -> gems
    padding = if diff == Hard then 0.5 else 1
    slice = fst $ Map.split (posn + padding) $ snd $ Map.split (posn -| padding) kept
    in if case Map.toList slice of [(p, _)] | p == posn -> True; [] -> True; _ -> False
      then keepKit (Map.alter (Just . (gems' ++) . fromMaybe []) posn kept) rest
      else keepKit kept rest
  keptHands = keepKit keptSnares $ sortOn (\(bts, _) -> (priority bts, bts)) kit
  kicks = ATB.getTimes $ RTB.toAbsoluteEventList 0 $ RTB.filter (== D.Kick) trk
  keepKicks kept [] = kept
  keepKicks kept (posn : rest) = let
    padding = if diff == Hard then 1 else 2
    slice = fst $ Map.split (posn + padding) $ snd $ Map.split (posn -| padding) kept
    hasKick = any (D.Kick `elem`) $ Map.elems slice
    hasOneHandGem = case Map.lookup posn slice of
      Just [_] -> True
      _        -> False
    in if not hasKick && (diff /= Medium || Map.null slice || hasOneHandGem)
      then keepKicks (Map.alter (Just . (D.Kick :) . fromMaybe []) posn kept) rest
      -- keep inter-hand kicks for Easy even though they are dropped in Medium
      else keepKicks kept rest
  keptAll = keepKicks keptHands $ sortOn (\bts -> (priority bts, bts)) kicks
  nullNothing [] = Nothing
  nullNothing xs = Just xs
  makeEasy, makeSnareKick, makeNoKick
    :: U.Beats -> Maybe U.Beats
    -> Map.Map U.Beats [D.Gem D.ProType]
    -> Map.Map U.Beats [D.Gem D.ProType]
  makeEasy start maybeEnd progress = let
    (_, startNote, sliceStart) = Map.splitLookup start progress
    slice = concat $ maybe id (:) startNote $ Map.elems case maybeEnd of
      Nothing  -> sliceStart
      Just end -> fst $ Map.split end sliceStart
    sliceKicks = length $ filter (== D.Kick) slice
    sliceHihats = length $ filter (== D.Pro D.Yellow D.Cymbal) slice
    sliceOtherKit = length $ filter (`notElem` [D.Kick, D.Red, D.Pro D.Yellow D.Cymbal]) slice
    fn  | sliceKicks == 0                          = makeNoKick
        | sliceKicks > sliceHihats + sliceOtherKit = makeSnareKick
        | sliceHihats > sliceOtherKit              = makeSnareKick
        | otherwise                                = makeNoKick
    in fn start maybeEnd progress
  makeSnareKick start maybeEnd = Map.mapMaybeWithKey \bts gems ->
    if start <= bts && case maybeEnd of Just end -> bts < end; Nothing -> True
      then nullNothing if start == bts && elem (D.Pro D.Green D.Cymbal) gems
        then filter (/= D.Kick) gems
        else filter (`elem` [D.Kick, D.Red]) gems
      else Just gems
  makeNoKick start maybeEnd = Map.mapMaybeWithKey \bts gems ->
    if start <= bts && case maybeEnd of Just end -> bts < end; Nothing -> True
      then nullNothing $ filter (/= D.Kick) gems
      else Just gems
  sectionStarts = ATB.toPairList $ RTB.toAbsoluteEventList 0 sections
  sectionBounds = zip sectionStarts (map (Just . fst) (drop 1 sectionStarts) ++ [Nothing])
  sectioned = if diff /= Easy
    then keptAll
    else let
      f ((start, _), maybeEnd) = makeEasy start maybeEnd
      in foldr f keptAll sectionBounds
  in ensureODNotes od trk $ RTB.flatten $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ Map.toAscList sectioned

-- | Parses a single named track out of the file-parser state (removing it from
-- the leftover tracks), applies a reduction, and returns the re-emitted MIDI
-- events. If the track is absent, nothing is consumed and nothing is emitted.
reduceOneTrack
  :: (SendMessage m, ParseTrack trk)
  => NonEmpty T.Text                                       -- ^ track name (and aliases) to match
  -> (trk U.Beats -> trk U.Beats)                          -- ^ reduction to apply
  -> F.FileParser m U.Beats [RTB.T U.Beats (E.T T.Text)]
reduceOneTrack names f = do
  trk <- codecIn $ F.fileTrack names
  return $ execWriter $ codecOut (F.fileId $ F.fileTrack names) $ f trk

-- (mostly as-is Claude output)
-- Adds lower difficulties (Easy/Medium/Hard) to the note charts in an Onyx
-- MIDI that don't already have them. Only the specific instrument tracks we
-- reduce are parsed and rewritten; every other track (comments, vocals, venue,
-- unrecognized tracks, etc.) is passed through untouched.
simpleReduce :: (SendMessage m, MonadIO m) => FretLimit -> FilePath -> FilePath -> StackTraceT m ()
simpleReduce fretLimit fin fout = do
  F.Song tempos mmap rawTrks <- loadRawMIDIOrChart fin >>= F.readMIDIFile
  -- Sections drive the Easy drum reduction; parse EVENTS without consuming it,
  -- since we want to leave that track in the output verbatim.
  events <- F.parseTracks rawTrks "EVENTS"
  let sections = (events :: EventsTrack U.Beats).sections
      -- Which flex parts are present in the file (guitar, bass, keys, ...).
      partNames = nubOrd $ mapMaybe (F.identifyFlexTrack <=< U.trackName) rawTrks
      -- Resolve the raw + part-adorned track names the way Onyx's file parser
      -- does, so we match whatever naming the input happens to use.
      trackNames partName pairs = let
        raws = fmap snd $ NE.filter ((== partName) . fst) pairs
        adorn (_, n) = "[" <> F.getPartName partName <> "] " <> n
        in foldr NE.cons (fmap adorn pairs) raws
      emitPK names v = execWriter $ codecOut (F.fileId $ F.fileTrack names) v
      reduceParts = fmap concat $ forM partNames \partName -> do
        let guitarNames = trackNames partName
              $ (F.PartGuitar            , "PART GUITAR"     ) :|
              [ (F.PartBass              , "PART BASS"       )
              , (F.PartName "rhythm"     , "PART RHYTHM"     )
              , (F.PartName "guitar-coop", "PART GUITAR COOP")
              ]
            keysNames  = trackNames partName $ pure (F.PartKeys , "PART KEYS"           )
            drumsNames = trackNames partName $ pure (F.PartDrums, "PART DRUMS"          )
            rkNames d  = trackNames partName $ pure (F.PartKeys , "PART REAL_KEYS_" <> d)
        emGuitar <- reduceOneTrack guitarNames $ gryboComplete fretLimit (Just (170/480)) mmap
        emKeys   <- reduceOneTrack keysNames   $ gryboComplete fretLimit Nothing          mmap
        emDrums  <- reduceOneTrack drumsNames
          $ D.fillDrumAnimation (0.25 :: U.Seconds) tempos . drumsComplete mmap sections
        -- Pro Keys difficulties live in four separate tracks that reduce off
        -- each other, so parse all four together before rewriting them.
        pkX <- codecIn $ F.fileTrack $ rkNames "X"
        pkH <- codecIn $ F.fileTrack $ rkNames "H"
        pkM <- codecIn $ F.fileTrack $ rkNames "M"
        pkE <- codecIn $ F.fileTrack $ rkNames "E"
        let od = pkX.overdrive
            trkX `pkOr` trkY
              | nullPK pkX  = mempty
              | nullPK trkX = trkY
              | otherwise   = trkX
            pkH' = pkH `pkOr` pkReduce Hard   mmap od pkX
            pkM' = pkM `pkOr` pkReduce Medium mmap od pkH'
            pkE' = pkE `pkOr` pkReduce Easy   mmap od pkM'
        return $ concat
          [ emGuitar, emKeys, emDrums
          , emitPK (rkNames "X") pkX
          , emitPK (rkNames "H") pkH'
          , emitPK (rkNames "M") pkM'
          , emitPK (rkNames "E") pkE'
          ]
  -- Run the reduction over the raw track list, keeping whatever tracks weren't
  -- consumed so we can pass them through unchanged.
  (emitted, leftover) <- flip mapStackTraceT reduceParts \f ->
    runStateT f rawTrks <&> \case
      (Left  err  , _   ) -> Left err
      (Right added, rest) -> Right (added, rest)
  stackIO $ Save.toFile fout $ fmap TE.encodeUtf8
    $ F.showMIDIFile $ F.Song tempos mmap $ leftover ++ emitted

-- | Currently just fills empty lower difficulties with dummy data
protarComplete :: ProGuitarTrack U.Beats -> ProGuitarTrack U.Beats
protarComplete pg = let
  getDiff d = fromMaybe mempty $ Map.lookup d pg.difficulties
  fill :: ProGuitarDifficulty U.Beats -> ProGuitarDifficulty U.Beats -> ProGuitarDifficulty U.Beats
  fill upper this = if RTB.null this.notes then upper else this
  x = getDiff Expert
  dummy = ProGuitarDifficulty
    { chordName    = RTB.empty
    , force        = RTB.empty
    , slide        = RTB.empty
    , arpeggio     = RTB.empty
    , partialChord = RTB.empty
    , allFrets     = RTB.empty
    , mysteryBFlat = RTB.empty
    , notes        = let
      openLow EdgeOn {} = EdgeOn 0 (S6, NormalNote)
      openLow EdgeOff{} = EdgeOff  (S6, NormalNote)
      in RTB.flatten $ fmap (nubOrd . map openLow) $ RTB.collectCoincident x.notes
    }
  h = fill dummy $ getDiff Hard
  m = fill dummy $ getDiff Medium
  e = fill dummy $ getDiff Easy
  in pg & #difficulties .~ Map.fromList [(Expert, x), (Hard, h), (Medium, m), (Easy, e)]
