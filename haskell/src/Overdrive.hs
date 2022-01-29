{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Overdrive
( HasOverdrive(..)
, calculateUnisons
, renderUnisons
, fixBrokenUnisons
, fixPartialUnisons
, printFlexParts
, notesFromRB3
, fixNotelessOD
, removeNotelessOD
, voidEdgeOn
) where

import           Control.Arrow                    ((>>>))
import           Control.Monad                    (forM, guard, unless, void)
import           Control.Monad.Trans.StackTrace
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List.Extra                  (intercalate, nubOrd, sort,
                                                   stripPrefix)
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe)
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Drums
import           RockBand.Codec.File
import           RockBand.Codec.Five
import           RockBand.Codec.ProGuitar
import           RockBand.Codec.ProKeys
import           RockBand.Codec.Six
import           RockBand.Common
import qualified Sound.MIDI.Util                  as U

class HasOverdrive file where
  getOverdrive :: (SendMessage m, NNC.C t) => file t -> StackTraceT m (RTB.T t (FlexPartName, Bool))
  putOverdrive :: (NNC.C t) => file t -> RTB.T t (FlexPartName, Bool) -> file t

combineOD :: (Eq t, SendMessage m) => FlexPartName -> [(String, RTB.T t Bool)] -> StackTraceT m (RTB.T t (FlexPartName, Bool))
combineOD part trks = case filter (not . RTB.null . snd) trks of
  []                            -> return RTB.empty
  hasOD@((firstTrk, od) : rest) -> fmap (part,) <$> if all (== od) $ map snd rest
    then return od
    else do
      warn $ unwords
        [ "Part"
        , show $ getPartName part
        , "tracks"
        , show $ map fst hasOD
        , "should have identical overdrive but they don't,"
        , "so I'm arbitrarily using the overdrive from"
        , show firstTrk
        ]
      return od

instance HasOverdrive OnyxFile where
  getOverdrive onyx = fmap (foldr RTB.merge RTB.empty) $ do
    forM (Map.toList $ onyxParts onyx) $ \(inst, opart) -> do
      combineOD inst
        [ ("drums"          , drumOverdrive $ onyxPartDrums        opart)
        , ("drums (2x)"     , drumOverdrive $ onyxPartDrums2x      opart)
        , ("drums (PS)"     , drumOverdrive $ onyxPartRealDrumsPS  opart)
        , ("5-fret (guitar)", fiveOverdrive $ onyxPartGuitar       opart)
        , ("5-fret (keys)"  , fiveOverdrive $ onyxPartKeys         opart)
        , ("5-fret (CH gtr)", fiveOverdrive $ onyxPartGuitarExt    opart)
        , ("6-fret"         , sixOverdrive  $ onyxPartSix          opart)
        , ("pro guitar"     , pgOverdrive   $ onyxPartRealGuitar   opart)
        , ("pro guitar (22)", pgOverdrive   $ onyxPartRealGuitar22 opart)
        , ("pro keys"       , pkOverdrive   $ onyxPartRealKeysX    opart)
        ]
  putOverdrive onyx od = onyx
    { onyxParts = flip Map.mapWithKey (onyxParts onyx) $ \inst opart -> let
      fn isEmpty getTrk addOD = let
        trk = getTrk opart
        in if isEmpty trk then trk else addOD trk
      bools = flip RTB.mapMaybe od $ \case
        (inst', b) | inst == inst' -> Just b
        _                          -> Nothing
      in opart
        { onyxPartDrums        = fn nullDrums onyxPartDrums        $ \x -> x { drumOverdrive = bools }
        , onyxPartDrums2x      = fn nullDrums onyxPartDrums2x      $ \x -> x { drumOverdrive = bools }
        , onyxPartRealDrumsPS  = fn nullDrums onyxPartRealDrumsPS  $ \x -> x { drumOverdrive = bools }
        , onyxPartGuitar       = fn nullFive  onyxPartGuitar       $ \x -> x { fiveOverdrive = bools }
        , onyxPartKeys         = fn nullFive  onyxPartKeys         $ \x -> x { fiveOverdrive = bools }
        , onyxPartGuitarExt    = fn nullFive  onyxPartGuitarExt    $ \x -> x { fiveOverdrive = bools }
        , onyxPartSix          = fn nullSix   onyxPartSix          $ \x -> x { sixOverdrive  = bools }
        , onyxPartRealGuitar   = fn nullPG    onyxPartRealGuitar   $ \x -> x { pgOverdrive   = bools }
        , onyxPartRealGuitar22 = fn nullPG    onyxPartRealGuitar22 $ \x -> x { pgOverdrive   = bools }
        , onyxPartRealKeysX    = fn nullPK    onyxPartRealKeysX    $ \x -> x { pkOverdrive   = bools }
        }
    }

instance HasOverdrive FixedFile where
  getOverdrive rb3 = do
    foldr RTB.merge RTB.empty <$> sequence
      [ combineOD FlexDrums
        [ ("PART DRUMS"         , drumOverdrive $ fixedPartDrums        rb3)
        ]
      , combineOD FlexGuitar
        [ ("PART GUITAR"        , fiveOverdrive $ fixedPartGuitar       rb3)
        , ("PART GUITAR GHL"    , sixOverdrive  $ fixedPartGuitarGHL    rb3)
        , ("PART REAL_GUITAR"   , pgOverdrive   $ fixedPartRealGuitar   rb3)
        , ("PART REAL_GUITAR_22", pgOverdrive   $ fixedPartRealGuitar22 rb3)
        ]
      , combineOD FlexBass
        [ ("PART BASS"          , fiveOverdrive $ fixedPartBass         rb3)
        , ("PART BASS GHL"      , sixOverdrive  $ fixedPartBassGHL      rb3)
        , ("PART REAL_BASS"     , pgOverdrive   $ fixedPartRealBass     rb3)
        , ("PART REAL_BASS_22"  , pgOverdrive   $ fixedPartRealBass22   rb3)
        ]
      , combineOD FlexKeys
        [ ("PART KEYS"          , fiveOverdrive $ fixedPartKeys         rb3)
        , ("PART REAL_KEYS"     , pkOverdrive   $ fixedPartRealKeysX    rb3)
        ]
      , combineOD (FlexExtra "rhythm")
        [ ("PART RHYTHM"        , fiveOverdrive $ fixedPartRhythm       rb3)
        ]
      , combineOD (FlexExtra "guitar-coop")
        [ ("PART GUITAR COOP"   , fiveOverdrive $ fixedPartGuitarCoop   rb3)
        ]
      ]
  putOverdrive rb3 od = rb3
    { fixedPartDrums        = fn nullDrums fixedPartDrums        $ \x -> x { drumOverdrive = drums }
    , fixedPartGuitar       = fn nullFive  fixedPartGuitar       $ \x -> x { fiveOverdrive = gtr }
    , fixedPartGuitarGHL    = fn nullSix   fixedPartGuitarGHL    $ \x -> x { sixOverdrive = gtr }
    , fixedPartRealGuitar   = fn nullPG    fixedPartRealGuitar   $ \x -> x { pgOverdrive = gtr }
    , fixedPartRealGuitar22 = fn nullPG    fixedPartRealGuitar22 $ \x -> x { pgOverdrive = gtr }
    , fixedPartBass         = fn nullFive  fixedPartBass         $ \x -> x { fiveOverdrive = bass }
    , fixedPartBassGHL      = fn nullSix   fixedPartBassGHL      $ \x -> x { sixOverdrive = bass }
    , fixedPartRealBass     = fn nullPG    fixedPartRealBass     $ \x -> x { pgOverdrive = bass }
    , fixedPartRealBass22   = fn nullPG    fixedPartRealBass22   $ \x -> x { pgOverdrive = bass }
    , fixedPartKeys         = fn nullFive  fixedPartKeys         $ \x -> x { fiveOverdrive = keys }
    , fixedPartRealKeysX    = fn nullPK    fixedPartRealKeysX    $ \x -> x { pkOverdrive = keys }
    , fixedPartRhythm       = fn nullFive  fixedPartRhythm       $ \x -> x { fiveOverdrive = rhythm }
    , fixedPartGuitarCoop   = fn nullFive  fixedPartGuitarCoop   $ \x -> x { fiveOverdrive = coop }
    } where
      fn isEmpty getTrk addOD = let
        trk = getTrk rb3
        in if isEmpty trk then trk else addOD trk
      drums = bools FlexDrums
      gtr = bools FlexGuitar
      bass = bools FlexBass
      keys = bools FlexKeys
      rhythm = bools (FlexExtra "rhythm")
      coop = bools (FlexExtra "guitar-coop")
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

-- | Overdrive phrases organized into unisons.
-- Each list entry is (time from unison start to inst phrase start, inst, inst phrase length)
type Unisons t = RTB.T t (NE.NonEmpty (t, FlexPartName, t))

calculateUnisons :: RTB.T U.Beats (FlexPartName, Bool) -> Unisons U.Beats
calculateUnisons = let
  joinOD = joinEdgesSimple . fmap (\(inst, b) -> if b then EdgeOn () inst else EdgeOff inst)
  findUnisons = RTB.viewL >>> \case
    Nothing -> RTB.empty
    Just ((dt, x), rest) -> let
      (unison, after) = U.trackSplit 1 rest
      unisonList = do
        (t, ((), inst, len)) <- (0, x) :| ATB.toPairList (RTB.toAbsoluteEventList 0 unison)
        return (t, inst, len)
      in RTB.cons dt unisonList $ RTB.delay 1 $ findUnisons after
  in findUnisons . joinOD

renderUnisons :: (NNC.C t) => Unisons t -> RTB.T t (FlexPartName, Bool)
renderUnisons = let
  f (t, part, len) = RTB.cons t (part, True) $ RTB.singleton len (part, False)
  in U.trackJoin . fmap (foldr RTB.merge RTB.empty . fmap f)

-- | Removes overdrive phrases to prevent \"unison phrases don't quite coincide\" in Magma v2.
removeBrokenUnisons
  :: (SendMessage m)
  => U.MeasureMap
  -> RTB.T U.Beats (FlexPartName, Bool)
  -> StackTraceT m (RTB.T U.Beats (FlexPartName, Bool))
removeBrokenUnisons mmap = fmap renderUnisons . go 0 . calculateUnisons where
  go :: (SendMessage m) => U.Beats -> Unisons U.Beats -> StackTraceT m (Unisons U.Beats)
  go !time = RTB.viewL >>> \case
    Nothing -> return RTB.empty
    Just ((dt, uni), rest) -> let
      firstEnd = minimum $ fmap (\(t, _, len) -> t + len) uni
      (keep, remove) = NE.partition (\(t, _, len) -> t + len < firstEnd + 1) uni
      readdUnison = case keep of
        []    -> RTB.delay dt -- shouldn't happen
        h : t -> RTB.cons dt $ h :| t
      unisonLocation = showPosition mmap $ time + dt
      in do
        unless (null remove) $ inside unisonLocation $ warn $ unwords
          [ "Removing overdrive phrases on the following instruments"
          , "to fix an invalid unison phrase:"
          , printFlexParts [ inst | (_, inst, _) <- remove ]
          ]
        readdUnison <$> go (time + dt) rest

fixBrokenUnisons
  :: (SendMessage m, HasOverdrive f)
  => Song (f U.Beats)
  -> StackTraceT m (Song (f U.Beats))
fixBrokenUnisons (Song tmap mmap rb3) = do
  od <- getOverdrive rb3
  od' <- removeBrokenUnisons mmap od
  return $ Song tmap mmap $ putOverdrive rb3 od'

printFlexParts :: [FlexPartName] -> String
printFlexParts = let
  part inst = let
    s = show inst
    in fromMaybe s $ stripPrefix "Flex" s
  in intercalate ", " . map part

-- | Removes overdrive phrases to prevent partial unisons that cause errors in Magma v1.
removePartialUnisons
  :: (SendMessage m)
  => [FlexPartName]
  -> U.MeasureMap
  -> RTB.T U.Beats (FlexPartName, Bool)
  -> StackTraceT m (RTB.T U.Beats (FlexPartName, Bool))
removePartialUnisons parts mmap = fmap renderUnisons . go 0 . calculateUnisons where
  parts' = sort parts
  go :: (SendMessage m) => U.Beats -> Unisons U.Beats -> StackTraceT m (Unisons U.Beats)
  go !time = RTB.viewL >>> \case
    Nothing -> return RTB.empty
    Just ((dt, uni), rest) -> case uni of
      _ :| []        -> RTB.cons dt uni <$> go (time + dt) rest -- single instrument, not a unison
      h :| t@(_ : _) -> if sort [ inst | (_, inst, _) <- NE.toList uni ] == parts'
        then RTB.cons dt uni <$> go (time + dt) rest -- full unison
        else do
          let unisonLocation = showPosition mmap $ time + dt
          inside unisonLocation $ warn $ unwords
            [ "Removing overdrive phrases on the following instruments"
            , "to fix a partial unison phrase:"
            , printFlexParts [ inst | (_, inst, _) <- t ]
            ]
          RTB.cons dt (h :| []) <$> go (time + dt) rest

fixPartialUnisons
  :: (SendMessage m, HasOverdrive f)
  => [FlexPartName]
  -> Song (f U.Beats)
  -> StackTraceT m (Song (f U.Beats))
fixPartialUnisons parts (Song tmap mmap rb3) = do
  od <- getOverdrive rb3
  od' <- removePartialUnisons parts mmap od
  return $ Song tmap mmap $ putOverdrive rb3 od'

voidEdgeOn :: (NNC.C t) => RTB.T t (Edge s a) -> RTB.T t ()
voidEdgeOn = RTB.mapMaybe $ \case EdgeOn{} -> Just (); EdgeOff{} -> Nothing

notesFromRB3
  :: (NNC.C t)
  => FixedFile t
  -> [(FlexPartName, [(String, RTB.T t ())])]
notesFromRB3 FixedFile{..} = let
  five name part = do
    (diff, fd) <- Map.toAscList $ fiveDifficulties part
    return (show diff ++ " " ++ name, voidEdgeOn $ fiveGems fd)
  protar name part = do
    (diff, pgd) <- Map.toAscList $ pgDifficulties part
    return (show diff ++ " Pro " ++ name, voidEdgeOn $ pgNotes pgd)
  in do
    (fpart, notes) <-
      [ (FlexDrums,) $ do
        (diff, dd) <- Map.toAscList $ drumDifficulties fixedPartDrums
        return (show diff ++ " Drums", void $ drumGems dd)
      , (FlexGuitar,) $ five "Guitar" fixedPartGuitar ++ protar "Guitar" fixedPartRealGuitar ++ protar "Guitar (22)" fixedPartRealGuitar22
      , (FlexBass,) $ five "Bass" fixedPartBass ++ protar "Bass" fixedPartRealBass ++ protar "Bass (22)" fixedPartRealBass22
      , (FlexKeys,) $ five "Keys" fixedPartKeys ++
        [ ("Easy Pro Keys"  , voidEdgeOn $ pkNotes fixedPartRealKeysE)
        , ("Medium Pro Keys", voidEdgeOn $ pkNotes fixedPartRealKeysM)
        , ("Hard Pro Keys"  , voidEdgeOn $ pkNotes fixedPartRealKeysH)
        , ("Expert Pro Keys", voidEdgeOn $ pkNotes fixedPartRealKeysX)
        ]
      ]
    case filter (not . RTB.null . snd) notes of
      []       -> []
      nonempty -> return (fpart, nonempty)

-- | Remove OD phrases that are either missing a note in some difficulty,
-- or don't have any notes between it and the previous phrase.
removeNotelessOD
  :: (SendMessage m)
  => U.MeasureMap
  -> [(FlexPartName, [(String, RTB.T U.Beats ())])]
  -> RTB.T U.Beats (FlexPartName, Bool)
  -> StackTraceT m (RTB.T U.Beats (FlexPartName, Bool))
removeNotelessOD mmap notes allOD = foldr RTB.merge RTB.empty <$> do
  forM (nubOrd $ map fst $ toList allOD) $ \fpart -> do
    let thisODEdges = fmap snd $ RTB.filter ((== fpart) . fst) allOD
        joinOD = fmap (\((), (), len) -> len) . joinEdgesSimple . fmap (\b -> if b then EdgeOn () () else EdgeOff ())
        splitOD = U.trackJoin . fmap (\len -> Wait 0 (fpart, True) $ Wait len (fpart, False) RNil)
        removeNoteless = let
          -- a map-form of each difficulty that needs notes for each phrase
          needNotes = do
            (diff, units) <- fromMaybe [] $ lookup fpart notes
            return (diff, Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList NNC.zero units)
          -- returns a difficulty that is missing notes in the given time span, if one exists
          findNoNotes includeT1 t1 t2 = listToMaybe $ do
            (diff, units) <- needNotes
            guard $ maybe True (\(noteTime, ()) -> t2 <= noteTime)
              $ (if includeT1 then Map.lookupGE else Map.lookupGT) t1 units
            return diff
          go _ ANil = return ANil
          go mprev (At t len rest) = case mprev >>= \prev -> findNoNotes False prev t of
            Just diff -> do
              -- note: "between" means *after* (not simultaneous with) the previous phrase end.
              -- this really shouldn't be a problem but Magma will fail
              inside (showPosition mmap t) $ warn $ unwords
                [ "Removing an OD phrase on"
                , T.unpack $ getPartName fpart
                , "because there are no notes between it and the previous phrase on"
                , diff
                ]
              go mprev rest
            Nothing -> case findNoNotes True t $ t <> len of
              Just diff -> do
                inside (showPosition mmap t) $ warn $ unwords
                  [ "Removing an OD phrase on"
                  , T.unpack $ getPartName fpart
                  , "because it has no notes on"
                  , diff
                  ]
                go mprev rest
              Nothing -> At t len <$> go (Just $ t <> len) rest
          in fmap RTB.fromAbsoluteEventList . go Nothing . RTB.toAbsoluteEventList NNC.zero
    splitOD <$> removeNoteless (joinOD thisODEdges)

fixNotelessOD
  :: (SendMessage m)
  => Song (FixedFile U.Beats)
  -> StackTraceT m (Song (FixedFile U.Beats))
fixNotelessOD (Song tmap mmap rb3) = do
  od <- getOverdrive rb3
  od' <- removeNotelessOD mmap (notesFromRB3 rb3) od
  return $ Song tmap mmap $ putOverdrive rb3 od'
