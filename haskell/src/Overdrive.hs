{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Overdrive where

import           Control.Monad                    (forM, guard, unless)
import           Control.Monad.Trans.StackTrace
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (nub)
import qualified Numeric.NonNegative.Class        as NNC
import qualified RockBand.Drums                   as RBDrums
import           RockBand.File
import qualified RockBand.FiveButton              as RBFive
import qualified RockBand.ProGuitar               as PG
import qualified RockBand.ProKeys                 as PK
import           Scripts                          (trackGlue)
import qualified Sound.MIDI.Util                  as U

class HasOverdrive file where
  getOverdrive :: (SendMessage m, NNC.C t) => file t -> StackTraceT m (RTB.T t (FlexPartName, Bool))
  putOverdrive :: (NNC.C t) => file t -> RTB.T t (FlexPartName, Bool) -> file t

instance HasOverdrive RB3File where
  getOverdrive rb3 = do
    let fnDrums = \case RBDrums.Overdrive b -> Just b; _ -> Nothing
        fnFive  = \case RBFive.Overdrive  b -> Just b; _ -> Nothing
        fnPK    = \case PK.Overdrive      b -> Just b; _ -> Nothing
        fnPG    = \case PG.Overdrive      b -> Just b; _ -> Nothing
        combineOD trks = case filter (not . RTB.null . snd) trks of
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
      [ part FlexDrums $ return $ RTB.mapMaybe fnDrums $ rb3PartDrums rb3
      , part FlexGuitar $ combineOD
        [ ("PART GUITAR"        , RTB.mapMaybe fnFive $ rb3PartGuitar       rb3)
        , ("PART REAL_GUITAR"   , RTB.mapMaybe fnPG   $ rb3PartRealGuitar   rb3)
        , ("PART REAL_GUITAR_22", RTB.mapMaybe fnPG   $ rb3PartRealGuitar22 rb3)
        ]
      , part FlexBass $ combineOD
        [ ("PART BASS"        , RTB.mapMaybe fnFive $ rb3PartBass       rb3)
        , ("PART REAL_BASS"   , RTB.mapMaybe fnPG   $ rb3PartRealBass   rb3)
        , ("PART REAL_BASS_22", RTB.mapMaybe fnPG   $ rb3PartRealBass22 rb3)
        ]
      , part FlexKeys $ combineOD
        [ ("PART KEYS"        , RTB.mapMaybe fnFive $ rb3PartKeys      rb3)
        , ("PART REAL_KEYS"   , RTB.mapMaybe fnPK   $ rb3PartRealKeysX rb3)
        ]
      ]
  putOverdrive rb3 od = rb3
    { rb3PartDrums        = fnDrums drums $ rb3PartDrums        rb3
    , rb3PartGuitar       = fnFive  gtr   $ rb3PartGuitar       rb3
    , rb3PartRealGuitar   = fnPG    gtr   $ rb3PartRealGuitar   rb3
    , rb3PartRealGuitar22 = fnPG    gtr   $ rb3PartRealGuitar22 rb3
    , rb3PartBass         = fnFive  bass  $ rb3PartBass         rb3
    , rb3PartRealBass     = fnPG    bass  $ rb3PartRealBass     rb3
    , rb3PartRealBass22   = fnPG    bass  $ rb3PartRealBass22   rb3
    , rb3PartKeys         = fnFive  keys  $ rb3PartKeys         rb3
    , rb3PartRealKeysX    = fnPK    keys  $ rb3PartRealKeysX    rb3
    } where
      drums = bools FlexDrums
      gtr = bools FlexGuitar
      bass = bools FlexBass
      keys = bools FlexKeys
      bools fpart = flip RTB.mapMaybe od $ \case
        (fpart', b) | fpart == fpart' -> Just b
        _ -> Nothing
      fn make clean newOD trk = if RTB.null trk
        then RTB.empty
        else RTB.merge (fmap make newOD) $ RTB.filter clean trk
      fnDrums = fn RBDrums.Overdrive $ \case RBDrums.Overdrive{} -> False; _ -> True
      fnFive  = fn RBFive.Overdrive  $ \case RBFive.Overdrive{}  -> False; _ -> True
      fnPG    = fn PG.Overdrive      $ \case PG.Overdrive{}      -> False; _ -> True
      fnPK    = fn PK.Overdrive      $ \case PK.Overdrive{}      -> False; _ -> True

instance HasOverdrive RB2File where
  getOverdrive rb2 = do
    let fnDrums = \case RBDrums.Overdrive b -> Just b; _ -> Nothing
        fnFive  = \case RBFive.Overdrive  b -> Just b; _ -> Nothing
        part fpart = fmap (fpart ,)
    return $ foldr RTB.merge RTB.empty
      [ part FlexDrums  $ RTB.mapMaybe fnDrums $ rb2PartDrums  rb2
      , part FlexGuitar $ RTB.mapMaybe fnFive  $ rb2PartGuitar rb2
      , part FlexBass   $ RTB.mapMaybe fnFive  $ rb2PartBass   rb2
      ]
  putOverdrive rb2 od = rb2
    { rb2PartDrums  = fnDrums (bools FlexDrums ) $ rb2PartDrums        rb2
    , rb2PartGuitar = fnFive  (bools FlexGuitar) $ rb2PartGuitar       rb2
    , rb2PartBass   = fnFive  (bools FlexBass  ) $ rb2PartBass         rb2
    } where
      bools fpart = flip RTB.mapMaybe od $ \case
        (fpart', b) | fpart == fpart' -> Just b
        _ -> Nothing
      fn make clean newOD trk = if RTB.null trk
        then RTB.empty
        else RTB.merge (fmap make newOD) $ RTB.filter clean trk
      fnDrums = fn RBDrums.Overdrive $ \case RBDrums.Overdrive{} -> False; _ -> True
      fnFive  = fn RBFive.Overdrive  $ \case RBFive.Overdrive{}  -> False; _ -> True

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
  :: (SendMessage m)
  => Song (RB3File U.Beats)
  -> StackTraceT m (Song (RB3File U.Beats))
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
