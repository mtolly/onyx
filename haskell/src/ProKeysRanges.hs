{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
module ProKeysRanges (completeFile, completeRanges, closeShifts, closeShiftsFile) where

import           Control.Monad                    (guard)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.StackTrace
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Milli)
import           Data.List                        (sortOn)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec                   (mapTrack)
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.ProKeys
import           RockBand.Common
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           WebPlayer                        (showTimestamp)

completeFile :: (SendMessage m, MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
completeFile fin fout = do
  RBFile.Song tempos mmap trks <- RBFile.loadMIDI fin
  liftIO $ Save.toFile fout $ RBFile.showMIDIFile' $ RBFile.Song tempos mmap trks
    { RBFile.onyxParts = flip fmap (RBFile.onyxParts trks) $ \flex -> flex
      { RBFile.onyxPartRealKeysE = completeRanges $ RBFile.onyxPartRealKeysE flex
      , RBFile.onyxPartRealKeysM = completeRanges $ RBFile.onyxPartRealKeysM flex
      , RBFile.onyxPartRealKeysH = completeRanges $ RBFile.onyxPartRealKeysH flex
      , RBFile.onyxPartRealKeysX = completeRanges $ RBFile.onyxPartRealKeysX flex
      }
    }

-- | Adds ranges if there are none.
completeRanges :: ProKeysTrack U.Beats -> ProKeysTrack U.Beats
completeRanges trk = if RTB.null $ pkLanes trk
  then let
    held = heldNotes $ U.trackJoin $ flip fmap (pkNotes trk)
      $ \(p, mlen) -> RTB.fromPairList
        [ (0                   , (True , p))
        , (fromMaybe (1/4) mlen, (False, p)) -- give all blips a 16th note of room
        ]
    in trk { pkLanes = pullBackRanges held $ createRanges held }
  else trk

heldNotes :: (NNC.C t) => RTB.T t (Bool, Pitch) -> RTB.T t (Set.Set Pitch)
heldNotes = go Set.empty . RTB.collectCoincident where
  go held rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, ends), rtb') -> let
      ons  = Set.fromList [p | (True , p) <- ends]
      offs = Set.fromList [p | (False, p) <- ends]
      held' = Set.difference held offs `Set.union` ons
      in RTB.cons dt held' $ go held' rtb'

-- | Chooses valid ranges to accommodate the given notes.
createRanges :: (NNC.C t) => RTB.T t (Set.Set Pitch) -> RTB.T t LaneRange
createRanges = go Nothing where
  go currentRange rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, held), rtb') -> if maybe False (\r -> all (keyInRange r) held) currentRange
      then RTB.delay dt $ go currentRange rtb'
      else case bestRange currentRange held rtb' of
        Nothing -> error $ "Couldn't make a Pro Keys range, because all of these notes are held simultaneously: " ++ show held
        Just newRange -> RTB.cons dt newRange $ go (Just newRange) rtb'

pullBackRanges :: (NNC.C t) => RTB.T t (Set.Set Pitch) -> RTB.T t LaneRange -> RTB.T t LaneRange
pullBackRanges rtb rngs = case RTB.viewL rngs of
  Nothing -> RTB.empty
  Just ((trng, rng), rngs') -> case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((theld, held), rtb') -> case NNC.split theld trng of
      (_, (theldLEQtrng, tdiff)) -> if theldLEQtrng
        then if all (all $ keyInRange rng) $ U.trackTake trng rtb
          then RTB.cons theld rng $ pullBackRanges rtb' $ RTB.delay tdiff rngs'
          else RTB.delay theld $ pullBackRanges rtb' $ RTB.cons tdiff rng rngs'
        else RTB.cons trng rng $ pullBackRanges (RTB.cons tdiff held rtb') rngs'

data Lifetime t
  = Mortal t
  | Immortal
  deriving (Eq, Ord, Show, Functor)

bestRange :: (NNC.C t) => Maybe LaneRange -> Set.Set Pitch -> RTB.T t (Set.Set Pitch) -> Maybe LaneRange
bestRange currentRange held rtb = let
  ranges = filter (\rng -> all (keyInRange rng) held) [minBound .. maxBound]
  isLegible rng = rng `notElem` [RangeD, RangeE]
  distance rng = case currentRange of
    Nothing -> 0
    Just cr -> abs $ fromEnum cr - fromEnum rng
  -- higher of each score value is better
  score rng = (rangeLifetime rng rtb, isLegible rng, negate $ distance rng)
  -- First, we want to pick the longest-lasting range.
  -- Second, I personally think D and E ranges are harder to read.
  -- Third, we want to pick a range that is closest to the last one.
  in listToMaybe $ reverse $ sortOn score ranges

rangeLifetime :: (NNC.C t) => LaneRange -> RTB.T t (Set.Set Pitch) -> Lifetime t
rangeLifetime rng rtb = case RTB.viewL rtb of
  Nothing -> Immortal -- range lasts till end of song
  Just ((dt, held), rtb') -> if all (keyInRange rng) held
    then NNC.add dt <$> rangeLifetime rng rtb'
    else Mortal dt

keyInRange :: LaneRange -> Pitch -> Bool
keyInRange RangeC p = RedYellow C <= p && p <= BlueGreen E
keyInRange RangeD p = RedYellow D <= p && p <= BlueGreen F
keyInRange RangeE p = RedYellow E <= p && p <= BlueGreen G
keyInRange RangeF p = RedYellow F <= p && p <= BlueGreen A
keyInRange RangeG p = RedYellow G <= p && p <= BlueGreen B
keyInRange RangeA p = RedYellow A <= p && p <= OrangeC

keyInPreRange :: LaneRange -> Pitch -> Bool
keyInPreRange RangeC p = RedYellow C  <= p && p <= BlueGreen E
keyInPreRange RangeD p = RedYellow Cs <= p && p <= BlueGreen Fs
keyInPreRange RangeE p = RedYellow Ds <= p && p <= BlueGreen Gs
keyInPreRange RangeF p = RedYellow F  <= p && p <= BlueGreen As
keyInPreRange RangeG p = RedYellow Fs <= p && p <= BlueGreen B
keyInPreRange RangeA p = RedYellow Gs <= p && p <= OrangeC

showPitch :: Pitch -> String
showPitch = \case
  RedYellow k -> if k < F
    then "Red "    <> showKey False k
    else "Yellow " <> showKey False k
  BlueGreen k -> if k < F
    then "Blue "  <> showKey False k
    else "Green " <> showKey False k
  OrangeC -> "Orange C"

closeShiftsFile :: RBFile.Song (RBFile.OnyxFile U.Beats) -> String
closeShiftsFile song = unlines $ do
  (partName, part) <- Map.toAscList $ RBFile.onyxParts $ RBFile.s_tracks song
  let xpk = RBFile.onyxPartRealKeysX part
  guard $ not $ nullPK xpk
  let close = U.unapplyTempoTrack (RBFile.s_tempos song) $ closeShifts 1 $ mapTrack (U.applyTempoTrack $ RBFile.s_tempos song) xpk
      showSeconds secs = show (realToFrac secs :: Milli) ++ "s"
      showClose (t, (rng1, rng2, dt, p)) = unwords
        [ showTimestamp (U.applyTempoMap (RBFile.s_tempos song) t) ++ ":"
        , "expert pro keys shift to"
        , show rng2
        , "is"
        , showSeconds dt
        , "before"
        , showPitch p ++ ","
        , "which is outside previous range"
        , show rng1
        ]
      surround x = ["[" ++ T.unpack (RBFile.getPartName partName) ++ "]"] ++ x
  surround $ case ATB.toPairList $ RTB.toAbsoluteEventList 0 close of
    []    -> ["No close shifts found."]
    pairs -> map showClose pairs

closeShifts :: U.Seconds -> ProKeysTrack U.Seconds -> RTB.T U.Seconds (LaneRange, LaneRange, U.Seconds, Pitch)
closeShifts threshold trk = let
  lanes = ATB.toPairList $ RTB.toAbsoluteEventList 0 $ pkLanes trk
  shifts = zip lanes $ drop 1 lanes
  notes = fst <$> pkNotes trk
  closeNotes ((_, rng1), (t, rng2)) = do
    ((dt, p), _) <- RTB.viewL $ RTB.filter (not . keyInPreRange rng1) $ U.trackTake threshold $ U.trackDrop t notes
    return (t, (rng1, rng2, dt, p))
  in RTB.fromAbsoluteEventList $ ATB.fromPairList $ mapMaybe closeNotes shifts
