{-# LANGUAGE LambdaCase #-}
module Scripts where

import           Data.Maybe                       (listToMaybe, mapMaybe)

import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class        as NNC
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U

import           Control.Monad.Trans.StackTrace
import qualified RockBand.Beat                    as Beat
import           RockBand.Common
import qualified RockBand.Drums                   as Drums
import qualified RockBand.Events                  as Events
import           RockBand.File
import qualified RockBand.FiveButton              as Five
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Vocals                  as Vocals

import           Config                           (Instrument (..), SongYaml,
                                                   _metadata, _previewEnd,
                                                   _previewStart)
import           Development.Shake

-- | Changes all existing drum mix events to use the given config (not changing
-- stuff like discobeat), and places ones at the beginning if they don't exist
-- already.
drumMix :: Drums.Audio -> RTB.T U.Beats Drums.Event -> RTB.T U.Beats Drums.Event
drumMix audio' trk = let
  (mixes, notMixes) = flip RTB.partitionMaybe trk $ \case
    Drums.DiffEvent diff (Drums.Mix audio disco) -> Just (diff, audio, disco)
    _                                            -> Nothing
  mixes' = fmap (\(diff, _, disco) -> (diff, audio', disco)) mixes
  alreadyMixed = [ diff | (diff, _, _) <- U.trackTakeZero mixes' ]
  addedMixes =
    [ (diff, audio', Drums.NoDisco)
    | diff <- [Easy .. Expert]
    , diff `notElem` alreadyMixed
    ]
  setMix (diff, audio, disco) = Drums.DiffEvent diff $ Drums.Mix audio disco
  in RTB.merge notMixes $ setMix <$> foldr addZero mixes' addedMixes

-- | Adds an event at position zero *after* all the other events there.
addZero :: (NNC.C t) => a -> RTB.T t a -> RTB.T t a
addZero x rtb = case U.trackSplitZero rtb of
  (zero, rest) -> U.trackGlueZero (zero ++ [x]) rest

loadMIDI :: FilePath -> Action (Song U.Beats)
loadMIDI fp = need [fp] >> liftIO (loadMIDI_IO fp)

loadMIDI_IO :: FilePath -> IO (Song U.Beats)
loadMIDI_IO fp = Load.fromFile fp >>= printStackTraceIO . readMIDIFile

loadTempos :: FilePath -> Action U.TempoMap
loadTempos fp = do
  need [fp]
  mid <- liftIO $ Load.fromFile fp
  case U.decodeFile mid of
    Left []               -> return $ U.makeTempoMap RTB.empty
    Left (tempoTrack : _) -> return $ U.makeTempoMap tempoTrack
    Right _               -> error "Scripts.loadTempos: SMPTE midi not supported"

saveMIDI :: FilePath -> Song U.Beats -> Action ()
saveMIDI fp song = liftIO $ Save.toFile fp $ showMIDIFile song

allEvents :: (NNC.C t) => Song t -> RTB.T t Events.Event
allEvents = foldr RTB.merge RTB.empty . mapMaybe getEvents . s_tracks where
  getEvents (Events trk) = Just trk
  getEvents _            = Nothing

-- | Returns the start and end of the preview audio in milliseconds.
previewBounds :: SongYaml -> Song U.Beats -> (Int, Int)
previewBounds syaml song = let
  starts = map Events.PracticeSection ["chorus", "chorus_1", "chorus_1a", "verse", "verse_1"]
  events = allEvents song
  find s = fmap (fst . fst) $ RTB.viewL $ RTB.filter (== s) events
  start = case mapMaybe find starts of
    []      -> 0
    bts : _ -> max 0 $ U.applyTempoMap (s_tempos song) bts - 0.6
  end = start + 30
  ms n = floor $ n * 1000
  in case (_previewStart $ _metadata syaml, _previewEnd $ _metadata syaml) of
    (Nothing, Nothing) -> (ms start, ms end)
    (Just ps, Just pe) -> (ms ps, ms pe)
    (Just ps, Nothing) -> (ms ps, ms $ ps + 30)
    (Nothing, Just pe) -> (ms $ pe - 30, ms pe)

songLengthBeats :: Song U.Beats -> U.Beats
songLengthBeats s = case RTB.getTimes $ RTB.filter (== Events.End) $ allEvents s of
  [bts] -> bts
  _     -> 0 -- eh

-- | Returns the time of the [end] event in milliseconds.
songLengthMS :: Song U.Beats -> Int
songLengthMS song = floor $ U.applyTempoMap (s_tempos song) (songLengthBeats song) * 1000

-- | Given a measure map, produces an infinite BEAT track.
makeBeatTrack :: U.MeasureMap -> RTB.T U.Beats Beat.Event
makeBeatTrack mmap = fixDoubleDownbeat $ go 0 where
  go i = let
    len = U.unapplyMeasureMap mmap (i + 1, 0) - U.unapplyMeasureMap mmap (i, 0)
    -- the rounding below ensures that
    -- e.g. the sig must be at least 3.5 to get bar-beat-beat-beat.
    -- if it's 3.25, then you would get a beat 0.25 before the next bar,
    -- which Magma doesn't like...
    thisMeasure = U.trackTake (fromInteger $ simpleRound len) infiniteMeasure
    -- simpleRound always rounds 0.5 up,
    -- unlike round which rounds to the nearest even number.
    simpleRound frac = case properFraction frac :: (Integer, U.Beats) of
      (_, 0.5) -> ceiling frac
      _        -> round frac
    in trackGlue len thisMeasure $ go $ i + 1
  infiniteMeasure, infiniteBeats :: RTB.T U.Beats Beat.Event
  infiniteMeasure = RTB.cons 0 Beat.Bar  $ RTB.delay 1 infiniteBeats
  infiniteBeats   = RTB.cons 0 Beat.Beat $ RTB.delay 1 infiniteBeats
  -- If you have a small measure (less than 1.5 beats) you get no upbeats in a measure.
  -- You can't have two downbeats in a row or Magma says:
  -- ERROR: MIDI Compiler: (BEAT): Two downbeats occur back to back at [45:1:000] and [46:1:000]
  -- So, make the first one not a downbeat.
  -- This works unless the very first measure of the song is small, but that should pretty much never happen.
  fixDoubleDownbeat :: RTB.T U.Beats Beat.Event -> RTB.T U.Beats Beat.Event
  fixDoubleDownbeat = RTB.fromPairList . fixDoubleDownbeat' . RTB.toPairList
  fixDoubleDownbeat' = \case
    (t1, Beat.Bar) : rest@((_, Beat.Bar) : _)
      -> (t1, Beat.Beat) : fixDoubleDownbeat' rest
    (t, x) : rest -> (t, x) : fixDoubleDownbeat' rest
    [] -> []

trackGlue :: (NNC.C t) => t -> RTB.T t a -> RTB.T t a -> RTB.T t a
trackGlue t xs ys = let
  xs' = U.trackTake t xs
  gap = t NNC.-| NNC.sum (RTB.getTimes xs')
  in RTB.append xs' $ RTB.delay gap ys

-- | Adjusts instrument tracks so rolls on notes 126/127 end just a tick after
--- their last gem note-on.
fixRolls :: Track U.Beats -> Track U.Beats
fixRolls = let
  drumsSingle = fixFreeform (== Drums.SingleRoll True) (== Drums.SingleRoll False) isHand
  drumsDouble = fixFreeform (== Drums.DoubleRoll True) (== Drums.DoubleRoll False) isHand
  isHand (Drums.DiffEvent Expert (Drums.Note gem)) = gem /= Drums.Kick
  isHand _                       = False
  fiveTremolo = fixFreeform (== Five.Tremolo True) (== Five.Tremolo False) isGem
  fiveTrill   = fixFreeform (== Five.Trill   True) (== Five.Trill   False) isGem
  isGem (Five.DiffEvent Expert (Five.Note (NoteOn () _))) = True
  isGem (Five.DiffEvent Expert (Five.Note (Blip   () _))) = True
  isGem _                                                 = False
  pkGlissando = fixFreeform (== ProKeys.Glissando True) (== ProKeys.Glissando False) isPKNote
  pkTrill     = fixFreeform (== ProKeys.Trill     True) (== ProKeys.Trill     False) isPKNote
  isPKNote (ProKeys.Note (NoteOn _ _)) = True
  isPKNote (ProKeys.Note (Blip   _ _)) = True
  isPKNote _                           = False
  in \case
    PartDrums         t -> PartDrums         $ drumsSingle $ drumsDouble t
    PartGuitar        t -> PartGuitar        $ fiveTremolo $ fiveTrill   t
    PartBass          t -> PartBass          $ fiveTremolo $ fiveTrill   t
    PartKeys          t -> PartKeys          $               fiveTrill   t
    PartRealKeys diff t -> PartRealKeys diff $ pkGlissando $ pkTrill     t
    trk                 -> trk

fixFreeform
  :: (Ord a)
  => (a -> Bool) -- ^ start of a freeform section
  -> (a -> Bool) -- ^ end of a freeform section
  -> (a -> Bool) -- ^ events which are covered by the freeform section
  -> RTB.T U.Beats a
  -> RTB.T U.Beats a
fixFreeform isStart isEnd isCovered = RTB.flatten . go . RTB.collectCoincident where
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, evts), rtb') -> RTB.cons dt evts $ if any isStart evts
      then case U.extractFirst (\x -> if isEnd x then Just x else Nothing) $ RTB.flatten rtb' of
        Nothing -> RTB.cons dt evts $ go rtb' -- probably an error
        Just ((oldLength, theEnd), rtb'noEnd) -> let
          coveredEvents = U.trackTake oldLength $ RTB.filter (any isCovered) rtb'
          newLength = case reverse $ ATB.getTimes $ RTB.toAbsoluteEventList 0 coveredEvents of
            pos : _ -> pos + 1/32
            _       -> oldLength
          in RTB.insert newLength [theEnd] $ go $ RTB.collectCoincident rtb'noEnd
      else go rtb'

-- This doesn't currently handle
-- [note start, phrase boundary, phrase boundary, note end]
-- i.e. a note that spans 3 phrases. But you shouldn't be doing that anyways!
harm1ToPartVocals :: (NNC.C t) => RTB.T t Vocals.Event -> RTB.T t Vocals.Event
harm1ToPartVocals = go . RTB.normalize where
  go rtb = case RTB.viewL rtb of
    Just ((dt, phstart@(Vocals.Phrase True)), rtb') -> case U.extractFirst isPhraseEnd rtb' of
      Nothing -> error "harm1ToPartVocals: found a HARM1 phrase with no end"
      Just ((phlen, phend), rtb'') -> if any isNote $ RTB.getBodies $ U.trackTake phlen rtb''
        then RTB.merge (RTB.fromPairList [(dt, phstart), (phlen, phend)])
          $ RTB.delay dt $ go rtb''
        else RTB.delay dt $ go rtb''
    Just ((dt, evt), rtb') -> RTB.cons dt evt $ go rtb'
    Nothing -> RTB.empty
  isPhraseEnd e = case e of
    Vocals.Phrase False -> Just e
    _                   -> Nothing
  isNote = \case Vocals.Note _ _ -> True; _ -> False

getPercType :: Song U.Beats -> Maybe Vocals.PercussionType
getPercType song = let
  vox = foldr RTB.merge RTB.empty $ mapMaybe getVox $ s_tracks song
  getVox (PartVocals t) = Just t
  getVox (Harm1      t) = Just t
  getVox (Harm2      t) = Just t
  getVox (Harm3      t) = Just t
  getVox _                     = Nothing
  isPercType (Vocals.PercussionAnimation ptype _) = Just ptype
  isPercType _                                   = Nothing
  in listToMaybe $ mapMaybe isPercType $ RTB.getBodies vox

-- | Makes a dummy Basic Keys track, for songs with only Pro Keys charted.
expertProKeysToKeys :: RTB.T U.Beats ProKeys.Event -> RTB.T U.Beats Five.Event
expertProKeysToKeys = let
  pkToBasic :: [ProKeys.Event] -> RTB.T U.Beats Five.Event
  pkToBasic pk = let
    hasNote     = flip any pk $ \case
      ProKeys.Note (Blip   () _) -> True
      ProKeys.Note (NoteOn () _) -> True
      _                          -> False
    hasODTrue   = elem (ProKeys.Overdrive True ) pk
    hasODFalse  = elem (ProKeys.Overdrive False) pk
    hasBRETrue  = elem (ProKeys.BRE       True ) pk
    hasBREFalse = elem (ProKeys.BRE       False) pk
    blip diff = RTB.singleton 0 $ Five.DiffEvent diff $ Five.Note $ Blip () Five.Green
    in foldr RTB.merge RTB.empty $ concat
      [ [ blip d | d <- [minBound .. maxBound], hasNote ]
      , [ RTB.singleton 0 $ Five.Overdrive True  | hasODTrue   ]
      , [ RTB.singleton 0 $ Five.Overdrive False | hasODFalse  ]
      , [ RTB.singleton 0 $ Five.BRE       True  | hasBRETrue  ]
      , [ RTB.singleton 0 $ Five.BRE       False | hasBREFalse ]
      ]
  in U.trackJoin . fmap pkToBasic . RTB.collectCoincident

-- | Makes a Pro Keys track, for songs with only Basic Keys charted.
keysToProKeys :: Difficulty -> RTB.T U.Beats Five.Event -> RTB.T U.Beats ProKeys.Event
keysToProKeys d = let
  basicToPK = \case
    Five.DiffEvent d' (Five.Note long) | d == d' ->
      Just $ ProKeys.Note $ flip fmap long $ \c -> ProKeys.BlueGreen $ case c of
        Five.Green  -> C
        Five.Red    -> D
        Five.Yellow -> E
        Five.Blue   -> F
        Five.Orange -> G
    Five.Overdrive b | d == Expert -> Just $ ProKeys.Overdrive b
    Five.Solo      b | d == Expert -> Just $ ProKeys.Solo      b
    Five.BRE       b | d == Expert -> Just $ ProKeys.BRE       b
    Five.Trill     b               -> Just $ ProKeys.Trill     b
    _                                -> Nothing
  in RTB.cons 0 (ProKeys.LaneShift ProKeys.RangeA) . RTB.mapMaybe basicToPK

hasSolo :: Instrument -> Song t -> Bool
hasSolo Guitar song = not $ null $ do
  PartGuitar t <- s_tracks song
  Five.Solo _ <- RTB.getBodies t
  return ()
hasSolo Bass song = not $ null $ do
  PartBass t <- s_tracks song
  Five.Solo _ <- RTB.getBodies t
  return ()
hasSolo Drums song = not $ null $ do
  PartDrums t <- s_tracks song
  Drums.Solo _ <- RTB.getBodies t
  return ()
hasSolo Keys song = not $ null
  $ do
    PartKeys t <- s_tracks song
    Five.Solo _ <- RTB.getBodies t
    return ()
  ++ do
    PartRealKeys Expert t <- s_tracks song
    ProKeys.Solo _ <- RTB.getBodies t
    return ()
hasSolo Vocal song = not $ null
  $ do
    PartVocals t <- s_tracks song
    Vocals.Percussion <- RTB.getBodies t
    return ()
  ++ do
    Harm1 t <- s_tracks song
    Vocals.Percussion <- RTB.getBodies t
    return ()

-- | Gives any note with no lyric a note name lyric.
windLyrics :: (NNC.C t) => RTB.T t Vocals.Event -> RTB.T t Vocals.Event
windLyrics = RTB.flatten . fmap f . RTB.collectCoincident where
  f evts = let
    ps = [ p | Vocals.Note True p <- evts ]
    lyrics = [ t | Vocals.Lyric t <- evts ]
    notlyrics = flip filter evts $ \case Vocals.Lyric _ -> False; _ -> True
    in case (ps, lyrics) of
      ([p], [   ]) -> Vocals.Lyric (noteName p       ) : evts
      ([p], ["$"]) -> Vocals.Lyric (noteName p ++ "$") : notlyrics
      _            -> evts

noteName :: Vocals.Pitch -> String
noteName = concatMap (\case 's' -> "# "; c -> [c]) . show . Vocals.pitchToKey
