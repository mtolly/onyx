{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Overdrive where

import           Control.Monad                    (forM, guard, unless)
import           Control.Monad.Trans.StackTrace
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (nub)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Drums
import           RockBand.Codec.File
import           RockBand.Codec.Five
import           RockBand.Codec.ProGuitar
import           RockBand.Codec.ProKeys
import           Scripts                          (trackGlue)
import qualified Sound.MIDI.Util                  as U

class HasOverdrive file where
  getOverdrive :: (SendMessage m, NNC.C t) => file t -> StackTraceT m (RTB.T t (FlexPartName, Bool))
  putOverdrive :: (NNC.C t) => file t -> RTB.T t (FlexPartName, Bool) -> file t

instance HasOverdrive FixedFile where
  getOverdrive rb3 = do
    let combineOD trks = case filter (not . RTB.null . snd) trks of
          []                            -> return RTB.empty
          hasOD@((firstTrk, od) : rest) -> if all (== od) $ map snd rest
            then return od
            else do
              warn $ unwords
                [ "These tracks should have identical overdrive but they don't:"
                , show $ map fst hasOD
                , "so I'm arbitrarily using the overdrive from"
                , firstTrk
                ]
              return od
        part fpart = fmap $ fmap (fpart ,)
    foldr RTB.merge RTB.empty <$> sequence
      [ part FlexDrums $ return $ drumOverdrive $ fixedPartDrums rb3
      , part FlexGuitar $ combineOD
        [ ("PART GUITAR"        , fiveOverdrive $ fixedPartGuitar       rb3)
        , ("PART REAL_GUITAR"   , pgOverdrive   $ fixedPartRealGuitar   rb3)
        , ("PART REAL_GUITAR_22", pgOverdrive   $ fixedPartRealGuitar22 rb3)
        ]
      , part FlexBass $ combineOD
        [ ("PART BASS"        , fiveOverdrive $ fixedPartBass       rb3)
        , ("PART REAL_BASS"   , pgOverdrive   $ fixedPartRealBass   rb3)
        , ("PART REAL_BASS_22", pgOverdrive   $ fixedPartRealBass22 rb3)
        ]
      , part FlexKeys $ combineOD
        [ ("PART KEYS"        , fiveOverdrive $ fixedPartKeys      rb3)
        , ("PART REAL_KEYS"   , pkOverdrive   $ fixedPartRealKeysX rb3)
        ]
      ]
  putOverdrive rb3 od = rb3
    { fixedPartDrums        = fn fixedPartDrums        $ \x -> x { drumOverdrive = drums }
    , fixedPartGuitar       = fn fixedPartGuitar       $ \x -> x { fiveOverdrive = gtr }
    , fixedPartRealGuitar   = fn fixedPartRealGuitar   $ \x -> x { pgOverdrive = gtr }
    , fixedPartRealGuitar22 = fn fixedPartRealGuitar22 $ \x -> x { pgOverdrive = gtr }
    , fixedPartBass         = fn fixedPartBass         $ \x -> x { fiveOverdrive = bass }
    , fixedPartRealBass     = fn fixedPartRealBass     $ \x -> x { pgOverdrive = bass }
    , fixedPartRealBass22   = fn fixedPartRealBass22   $ \x -> x { pgOverdrive = bass }
    , fixedPartKeys         = fn fixedPartKeys         $ \x -> x { fiveOverdrive = keys }
    , fixedPartRealKeysX    = fn fixedPartRealKeysX    $ \x -> x { pkOverdrive = keys }
    } where
      fn getTrk addOD = let
        trk = getTrk rb3
        in if trk /= mempty then addOD trk else trk
      drums = bools FlexDrums
      gtr = bools FlexGuitar
      bass = bools FlexBass
      keys = bools FlexKeys
      bools fpart = flip RTB.mapMaybe od $ \case
        (fpart', b) | fpart == fpart' -> Just b
        _ -> Nothing

{-

How unisons work:

1. If an instrument has an OD phrase start, and another instrument has an OD
   phrase start within a beat (less than 480 ticks) from that, they form a
   unison, which starts where the earlier phrase starts. However the unison
   indicator in-game will appear at the later phrase start.

2. The above rule applies from left-to-right and gobbles up phrases within that
   beat. For example, if keys starts a phrase, then 0.75 beats later guitar
   starts a phrase, then 0.75 beats later bass starts a phrase, keys and guitar
   will form a unison, and the bass phrase will not be in a unison. (Even if you
   are playing solo bass.)

3. If you have a unison, then the distance from the first phrase end to the last
   phrase end must be less than 1 beat (480 ticks). Otherwise "unison phrases
   don't quite coincide".

-}

-- | Removes overdrive phrases to prevent \"unison phrases don't quite coincide\" in Magma v2.
removeBrokenUnisons
  :: (SendMessage m)
  => U.MeasureMap
  -> RTB.T U.Beats (FlexPartName, Bool)
  -> StackTraceT m (RTB.T U.Beats (FlexPartName, Bool))
removeBrokenUnisons mmap = go 0 where
  go time rtb = case RTB.viewL rtb of
    Nothing -> return RTB.empty
    Just ((dt, off@(_, False)), rtb') -> RTB.cons dt off <$> go (time + dt) rtb'
    Just ((dt, on@(fpart, True)), rtb') -> let
      starts = nub $ fpart : [ fp | (fp, True) <- RTB.getBodies $ U.trackTake 1 rtb' ]
      in case starts of
        unison@(_ : _ : _) -> do
          unison' <- forM unison $ \inst -> case RTB.viewL $ RTB.filter (== (inst, False)) rtb' of
            Nothing                -> fatal "removeBrokenUnisons: overdrive phrase has no endpoint"
            Just ((posnOff, _), _) -> return (inst, posnOff)
          let firstEnd = minimum $ map snd unison'
              removing = [ inst | (inst, end) <- unison', end >= firstEnd + 1 ]
              removePhrase inst od = case U.extractFirst (\x -> guard (x == (inst, True)) >> Just ()) od of
                Nothing -> od -- shouldn't happen
                Just (_, od') -> case U.extractFirst (\x -> guard (x == (inst, False)) >> Just ()) od' of
                  Nothing        -> od' -- shouldn't happen
                  Just (_, od'') -> od''
              removed = foldr removePhrase rtb removing
              unisonLocation = showPosition $ U.applyMeasureMap mmap $ time + dt
          unless (null removing) $ inside unisonLocation $ warn $ unwords
            [ "Removing overdrive phrases on the following instruments"
            , "to fix an invalid unison phrase:"
            , unwords $ flip map removing $ \inst -> case show inst of
              'F':'l':'e':'x':str -> str
              str                 -> str
            ]
          case U.trackSplit (dt + 1) removed of
            (x, y) -> trackGlue (dt + 1) x <$> go (time + dt + 1) y
        _ -> RTB.cons dt on <$> go (time + dt) rtb'

fixBrokenUnisons
  :: (SendMessage m, HasOverdrive f)
  => Song (f U.Beats)
  -> StackTraceT m (Song (f U.Beats))
fixBrokenUnisons (Song tmap mmap rb3) = do
  od <- getOverdrive rb3
  od' <- removeBrokenUnisons mmap od
  return $ Song tmap mmap $ putOverdrive rb3 od'

-- | Removes overdrive phrases to prevent partial unisons that cause errors in Magma v1.
removePartialUnisons
  :: (SendMessage m)
  => [FlexPartName]
  -> U.MeasureMap
  -> RTB.T U.Beats (FlexPartName, Bool)
  -> StackTraceT m (RTB.T U.Beats (FlexPartName, Bool))
removePartialUnisons = undefined

fixPartialUnisons
  :: (SendMessage m, HasOverdrive f)
  => [FlexPartName]
  -> Song (f U.Beats)
  -> StackTraceT m (Song (f U.Beats))
fixPartialUnisons parts (Song tmap mmap rb3) = do
  od <- getOverdrive rb3
  od' <- removePartialUnisons parts mmap od
  return $ Song tmap mmap $ putOverdrive rb3 od'

-- Functions from RB2 module:

{-

-- | Removes OD phrases to ensures that no phrases overlap on different tracks,
-- except for precisely matching unison phrases on all tracks.
fixOverdrive :: (NNC.C t) => [RTB.T t Bool] -> [RTB.T t Bool]
fixOverdrive [] = []
fixOverdrive tracks = let
  go trks = case sort $ mapMaybe (fmap fst . RTB.viewL) trks of
    [] -> trks -- all tracks are empty
    (_, ((), (), Nothing)) : _ -> panic "blip in joined phrase stream"
    firstPhrase@(dt, ((), (), Just len)) : _ -> let
      hasThisPhrase trk = case RTB.viewL trk of
        Nothing             -> Nothing
        Just (phrase, trk') -> guard (phrase == firstPhrase) >> Just trk'
      in case mapM hasThisPhrase trks of
        Just trks' -> map (uncurry RTB.cons firstPhrase) $ go trks' -- full unison
        Nothing    -> let
          ix = length $ takeWhile (isNothing . hasThisPhrase) trks
          trksNext = map (RTB.delay len . U.trackDrop (NNC.add dt len)) trks
          repackage i = if i == ix
            then uncurry RTB.cons firstPhrase
            else RTB.delay dt
          in zipWith repackage [0..] $ go trksNext
  boolToLong b = if b then NoteOn () () else NoteOff ()
  longToBool (NoteOn  () ()) = True
  longToBool (Blip    () ()) = panic "blip in LongNote stream"
  longToBool (NoteOff    ()) = False
  panic s = error $ "RockBand2.fixOverdrive: panic! this shouldn't happen: " ++ s
  in map (fmap longToBool . splitEdges) $ go $ map (joinEdges . fmap boolToLong) tracks

-- the complicated dance to extract OD phrases, fix partial unisons,
-- and put the phrases back
fixUnisons trks = let
  gtr  = F.fixedPartGuitar trks
  bass = F.fixedPartBass   trks
  drum = F.fixedPartDrums  trks
  gtrOD  = RTB.mapMaybe getFiveOD gtr
  bassOD = RTB.mapMaybe getFiveOD bass
  drumOD = RTB.mapMaybe getDrumOD drum
  getFiveOD = \case Five.Overdrive  b -> Just b; _ -> Nothing
  getDrumOD = \case Drums.Overdrive b -> Just b; _ -> Nothing
  replaceFiveOD od trk = RTB.merge (fmap Five.Overdrive od)
    $ RTB.filter (\case Five.Overdrive _ -> False; _ -> True) trk
  replaceDrumsOD od trk = RTB.merge (fmap Drums.Overdrive od)
    $ RTB.filter (\case Drums.Overdrive _ -> False; _ -> True) trk
  in case (not $ RTB.null gtr, not $ RTB.null bass, not $ RTB.null drum) of
    (False, False, False) -> trks
    ( True, False, False) -> trks
    (False,  True, False) -> trks
    (False, False,  True) -> trks
    ( True,  True, False) -> let
      [gtrOD', bassOD'] = fixOverdrive [gtrOD, bassOD]
      in trks
        { F.fixedPartGuitar = replaceFiveOD gtrOD'  $ F.fixedPartGuitar trks
        , F.fixedPartBass   = replaceFiveOD bassOD' $ F.fixedPartBass   trks
        }
    ( True, False,  True) -> let
      [drumOD', gtrOD'] = fixOverdrive [drumOD, gtrOD]
      in trks
        { F.fixedPartGuitar = replaceFiveOD  gtrOD'  $ F.fixedPartGuitar trks
        , F.fixedPartDrums  = replaceDrumsOD drumOD' $ F.fixedPartDrums  trks
        }
    (False,  True,  True) -> let
      [drumOD', bassOD'] = fixOverdrive [drumOD, bassOD]
      in trks
        { F.fixedPartBass   = replaceFiveOD  bassOD' $ F.fixedPartBass   trks
        , F.fixedPartDrums  = replaceDrumsOD drumOD' $ F.fixedPartDrums  trks
        }
    ( True,  True,  True) -> let
      [drumOD', gtrOD', bassOD'] = fixOverdrive [drumOD, gtrOD, bassOD]
      in trks
        { F.fixedPartGuitar = replaceFiveOD  gtrOD'  $ F.fixedPartGuitar trks
        , F.fixedPartBass   = replaceFiveOD  bassOD' $ F.fixedPartBass   trks
        , F.fixedPartDrums  = replaceDrumsOD drumOD' $ F.fixedPartDrums  trks
        }

-}
