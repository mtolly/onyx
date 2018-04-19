{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Scripts where

import           Config                           (Instrument (..),
                                                   PreviewTime (..), SongYaml,
                                                   _metadata, _previewEnd,
                                                   _previewStart)
import           Control.Monad                    (forM, guard)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.StackTrace
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import           Data.Monoid                      ((<>))
import           Development.Shake
import qualified FretsOnFire                      as FoF
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Beat
import           RockBand.Codec.Drums
import           RockBand.Codec.Events
import           RockBand.Codec.File              (Song (..))
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.Five
import           RockBand.Codec.ProGuitar
import           RockBand.Codec.ProKeys
import           RockBand.Codec.Vocal
import           RockBand.Common
import qualified RockBand.Drums                   as Drums
import qualified RockBand.FiveButton              as Five
import           RockBand.Parse                   (isNoteEdgeCPV, makeEdgeCPV)
import qualified RockBand.ProGuitar               as ProGuitar
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Vocals                  as Vocals
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U

-- | Changes all existing drum mix events to use the given config (not changing
-- stuff like discobeat), and places ones at the beginning if they don't exist
-- already.
drumMix_precodec :: Drums.Audio -> RTB.T U.Beats Drums.Event -> RTB.T U.Beats Drums.Event
drumMix_precodec audio' trk = let
  (mixes, notMixes) = flip RTB.partitionMaybe trk $ \case
    Drums.DiffEvent diff (Drums.Mix audio disco) -> Just (diff, audio, disco)
    _                                            -> Nothing
  mixes' = fmap (\(diff, _, disco) -> (diff, audio', disco)) mixes
  getDiff diff = flip RTB.mapMaybe trk $ \case
    Drums.DiffEvent d evt | d == diff -> Just evt
    _                                 -> Nothing
  alreadyMixed = go . RTB.getBodies . RTB.collectCoincident . getDiff
  go [] = False
  go (now : later)
    | not $ null [ () | Drums.Mix _ _ <- now ] = True
    | not $ null [ () | Drums.Note _  <- now ] = False
    | otherwise                                = go later
  addedMixes =
    [ (diff, audio', Drums.NoDisco)
    | diff <- [Easy .. Expert]
    , not $ alreadyMixed diff
    ]
  setMix (diff, audio, disco) = Drums.DiffEvent diff $ Drums.Mix audio disco
  in RTB.merge notMixes $ setMix <$> foldr addZero mixes' addedMixes

-- | Adds an event at position zero *after* all the other events there.
addZero :: (NNC.C t) => a -> RTB.T t a -> RTB.T t a
addZero x rtb = case U.trackSplitZero rtb of
  (zero, rest) -> U.trackGlueZero (zero ++ [x]) rest

loadMIDI :: (SendMessage m, MonadIO m, RBFile.ParseFile f) => FilePath -> StackTraceT m (Song (f U.Beats))
loadMIDI fp = stackIO (Load.fromFile fp) >>= RBFile.readMIDIFile'

-- | Moves star power from the GH 1/2 format to the RB format, either if it is
-- specified in the song.ini, or automatically detected from the MIDI.
loadFoFMIDI :: (SendMessage m, MonadIO m, RBFile.ParseFile f) => FoF.Song -> FilePath -> StackTraceT m (Song (f U.Beats))
loadFoFMIDI ini fp = do
  mid <- stackIO $ Load.fromFile fp
  let isGtrTrack trk = U.trackName trk `elem` map Just ["PART GUITAR", "PART BASS", "PART RHYTHM", "T1 GEMS"]
      midGH = case mid of
        F.Cons typ dvn trks -> F.Cons typ dvn $ flip map trks $ \trk -> if isGtrTrack trk
          then flip RTB.mapMaybe trk $ \e -> case isNoteEdgeCPV e of
            Just (c, 103, v) -> Just $ makeEdgeCPV c 116 v
            Just (_, 116, _) -> Nothing
            _                -> Just e
          else trk
      -- look for and remove fake OD notes used in lieu of star_power_note;
      -- this was seen in Bocaj Hero V
      midRB = case mid of
        F.Cons typ dvn trks -> fmap (F.Cons typ dvn) $ forM trks $ \trk -> if isGtrTrack trk
          then let
            od = flip RTB.mapMaybe trk $ \e -> case isNoteEdgeCPV e of
              Just (_, 116, Just _) -> Just ()
              _                     -> Nothing
            odless = flip RTB.filter trk $ \e -> case isNoteEdgeCPV e of
              Just (_, 116, _) -> False
              _                -> True
            in case RTB.toPairList od of
              -- look for 2 tiny OD phrases right next to each other
              [(_, ()), (x, ())] | x < (480 * 5) -> do
                lg "Removing thebocaj-style fake OD notes"
                return odless
              _                          -> return trk
          else return trk
      hasPitch n = not $ null $ do
        trk <- case mid of F.Cons _ _ trks -> trks
        guard $ isGtrTrack trk
        e <- toList trk
        case isNoteEdgeCPV e of
          Just (_, n', _) | n == n' -> [()]
          _               -> []
  mid' <- case FoF.starPowerNote ini of
    Just 103 -> do
      lg "Star Power note specified in song.ini to be 103 (old GH format), converting to RB"
      return midGH
    Just 116 -> do
      lg "Star Power note specified in song.ini to be 116 (RB format)"
      midRB
    Nothing -> if hasPitch 103 && not (hasPitch 116)
      then do
        lg "MIDI auto-detected as old GH Star Power format, converting to RB"
        return midGH
      else do
        lg "MIDI auto-detected as RB Overdrive format, passing through unmodified"
        midRB
    Just n -> do
      warn $ "song.ini has unsupported Star Power pitch of " <> show n <> ", assuming RB format"
      midRB
  RBFile.readMIDIFile' mid'

shakeMIDI :: (RBFile.ParseFile f) => FilePath -> StackTraceT (QueueLog Action) (Song (f U.Beats))
shakeMIDI fp = lift (lift $ need [fp]) >> loadMIDI fp

loadTemposIO :: FilePath -> IO U.TempoMap
loadTemposIO fp = do
  mid <- Load.fromFile fp
  case U.decodeFile mid of
    Left []               -> return $ U.makeTempoMap RTB.empty
    Left (tempoTrack : _) -> return $ U.makeTempoMap tempoTrack
    Right _               -> error "Scripts.loadTemposIO: SMPTE midi not supported"

saveMIDI :: (MonadIO m, RBFile.ParseFile f) => FilePath -> Song (f U.Beats) -> m ()
saveMIDI fp song = liftIO $ Save.toFile fp $ RBFile.showMIDIFile' song

-- | Returns the start and end of the preview audio in milliseconds.
previewBounds :: SongYaml -> Song (RBFile.OnyxFile U.Beats) -> (Int, Int)
previewBounds syaml song = let
  len = songLengthMS song
  secsToMS s = floor $ s * 1000
  leadIn = max 0 . subtract 600
  evalTime = \case
    PreviewSeconds secs -> Just $ secsToMS secs
    PreviewMIDI mb -> Just $ leadIn $ secsToMS $ U.applyTempoMap (s_tempos song) $ U.unapplyMeasureMap (s_signatures song) mb
    PreviewSection str -> case findSection str of
      Nothing  -> Nothing
      Just bts -> Just $ leadIn $ secsToMS $ U.applyTempoMap (s_tempos song) bts
  evalTime' pt = fromMaybe (error $ "Couldn't evaluate preview bound: " ++ show pt) $ evalTime pt
  defStartTime = case mapMaybe (evalTime . PreviewSection) ["chorus", "chorus_1", "chorus_1a", "verse", "verse_1"] of
    []    -> max 0 $ quot len 2 - 15000
    t : _ -> min (len - 30000) t
  findSection sect = fmap (fst . fst) $ RTB.viewL $ RTB.filter ((== sect) . snd)
    $ eventsSections $ RBFile.onyxEvents $ s_tracks song
  in case (_previewStart $ _metadata syaml, _previewEnd $ _metadata syaml) of
    (Nothing, Nothing) -> (defStartTime, defStartTime + 30000)
    (Just ps, Just pe) -> (evalTime' ps, evalTime' pe)
    (Just ps, Nothing) -> let start = evalTime' ps in (start, start + 30000)
    (Nothing, Just pe) -> let end = evalTime' pe in (end - 30000, end)

songLengthBeats :: Song (RBFile.OnyxFile U.Beats) -> U.Beats
songLengthBeats s = case RTB.getTimes $ eventsEnd $ RBFile.onyxEvents $ s_tracks s of
  [bts] -> bts
  _     -> 0 -- eh

-- | Returns the time of the [end] event in milliseconds.
songLengthMS :: Song (RBFile.OnyxFile U.Beats) -> Int
songLengthMS song = floor $ U.applyTempoMap (s_tempos song) (songLengthBeats song) * 1000

-- | Given a measure map, produces an infinite BEAT track.
makeBeatTrack :: U.MeasureMap -> RTB.T U.Beats BeatEvent
makeBeatTrack mmap = go 0 where
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
  infiniteMeasure, infiniteBeats :: RTB.T U.Beats BeatEvent
  infiniteMeasure = RTB.cons 0 Bar  $ RTB.delay 1 infiniteBeats
  infiniteBeats   = RTB.cons 0 Beat $ RTB.delay 1 infiniteBeats

trackGlue :: (NNC.C t) => t -> RTB.T t a -> RTB.T t a -> RTB.T t a
trackGlue t xs ys = let
  xs' = U.trackTake t xs
  gap = t NNC.-| NNC.sum (RTB.getTimes xs')
  in RTB.append xs' $ RTB.delay gap ys

fixFreeformDrums_precodec :: RTB.T U.Beats Drums.Event -> RTB.T U.Beats Drums.Event
fixFreeformDrums_precodec = let
  drumsSingle' = fixFreeform_precodec (== Drums.SingleRoll True) (== Drums.SingleRoll False) isHand
  drumsDouble' = fixFreeform_precodec (== Drums.DoubleRoll True) (== Drums.DoubleRoll False) isHand
  isHand (Drums.DiffEvent Expert (Drums.Note gem)) = gem /= Drums.Kick
  isHand _                                         = False
  in drumsSingle' . drumsDouble'

fixFreeformFive_precodec :: RTB.T U.Beats Five.Event -> RTB.T U.Beats Five.Event
fixFreeformFive_precodec = let
  fiveTremolo' = fixFreeform_precodec (== Five.Tremolo True) (== Five.Tremolo False) isGem
  fiveTrill'   = fixFreeform_precodec (== Five.Trill   True) (== Five.Trill   False) isGem
  isGem (Five.DiffEvent Expert (Five.Note (NoteOn () _))) = True
  isGem (Five.DiffEvent Expert (Five.Note (Blip   () _))) = True
  isGem _                                                 = False
  in fiveTremolo' . fiveTrill'

fixFreeformPK_precodec :: RTB.T U.Beats ProKeys.Event -> RTB.T U.Beats ProKeys.Event
fixFreeformPK_precodec = let
  pkGlissando' = fixFreeform_precodec (== ProKeys.Glissando True) (== ProKeys.Glissando False) isPKNote
  pkTrill'     = fixFreeform_precodec (== ProKeys.Trill     True) (== ProKeys.Trill     False) isPKNote
  isPKNote (ProKeys.Note (NoteOn _ _)) = True
  isPKNote (ProKeys.Note (Blip   _ _)) = True
  isPKNote _                           = False
  in pkGlissando' . pkTrill'

fixFreeformPG_precodec :: RTB.T U.Beats ProGuitar.Event -> RTB.T U.Beats ProGuitar.Event
fixFreeformPG_precodec = let
  pgTremolo' = fixFreeform_precodec (== ProGuitar.Tremolo True) (== ProGuitar.Tremolo False) isGem
  pgTrill'   = fixFreeform_precodec (== ProGuitar.Trill   True) (== ProGuitar.Trill   False) isGem
  isGem (ProGuitar.DiffEvent Expert (ProGuitar.Note (NoteOn _ (_, ntype))))
    = ntype /= ProGuitar.ArpeggioForm
  isGem (ProGuitar.DiffEvent Expert (ProGuitar.Note (Blip   _ (_, ntype))))
    = ntype /= ProGuitar.ArpeggioForm
  isGem _ = False
  in pgTremolo' . pgTrill'

-- | Adjusts instrument tracks so rolls on notes 126/127 end just a tick after
--- their last gem note-on.
fixFreeform_precodec
  :: (Ord a)
  => (a -> Bool) -- ^ start of a freeform section
  -> (a -> Bool) -- ^ end of a freeform section
  -> (a -> Bool) -- ^ events which are covered by the freeform section
  -> RTB.T U.Beats a
  -> RTB.T U.Beats a
fixFreeform_precodec isStart isEnd isCovered = RTB.flatten . go . RTB.collectCoincident where
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

-- | Adjusts instrument tracks so rolls on notes 126/127 end just a tick after
--- their last gem note-on.
fixFreeform :: RTB.T U.Beats () -> RTB.T U.Beats Bool -> RTB.T U.Beats Bool
fixFreeform initGems = go initGems . RTB.normalize where
  go gems lanes = case RTB.viewL lanes of
    Just ((dt, True), lanes') -> case RTB.viewL lanes' of
      Just ((len, False), lanes'') -> let
        covered = U.trackTake len $ U.trackDrop dt gems
        len' = case sum $ RTB.getTimes covered of
          0 -> len -- no gems, shouldn't happen
          s -> s + 1/32
        in RTB.cons dt True $ RTB.insert len' False $
          go (U.trackDrop dt gems) (RTB.delay len lanes'')
      _ -> lanes -- on not followed by off, abort
    Just ((_, False), _) -> lanes -- off not preceded by on, abort
    Nothing -> RTB.empty -- done

-- This doesn't currently handle
-- [note start, phrase boundary, phrase boundary, note end]
-- i.e. a note that spans 3 phrases. But you shouldn't be doing that anyways!
harm1ToPartVocals_precodec :: (NNC.C t) => RTB.T t Vocals.Event -> RTB.T t Vocals.Event
harm1ToPartVocals_precodec = go . RTB.normalize where
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

getPercType :: (NNC.C t) => Song (RBFile.FixedFile t) -> Maybe Vocals.PercussionType
getPercType song = listToMaybe $ do
  trk <-
    [ RBFile.fixedPartVocals $ RBFile.s_tracks song
    , RBFile.fixedHarm1      $ RBFile.s_tracks song
    , RBFile.fixedHarm2      $ RBFile.s_tracks song
    , RBFile.fixedHarm3      $ RBFile.s_tracks song
    ]
  (perc, _) <- RTB.getBodies $ vocalPercAnimation trk
  return perc

-- | Makes a dummy Basic Guitar/Bass track, for parts with only Pro Guitar/Bass charted.
protarToGrybo_precodec :: RTB.T U.Beats ProGuitar.Event -> RTB.T U.Beats Five.Event
protarToGrybo_precodec = let
  pgToBasic :: [ProGuitar.Event] -> RTB.T U.Beats Five.Event
  pgToBasic pg = let
    hasNote diff = flip any pg $ \case
      ProGuitar.DiffEvent d (ProGuitar.Note (Blip   _ _)) | d == diff -> True
      ProGuitar.DiffEvent d (ProGuitar.Note (NoteOn _ _)) | d == diff -> True
      _ -> False
    hasODTrue   = elem (ProGuitar.Overdrive True ) pg
    hasODFalse  = elem (ProGuitar.Overdrive False) pg
    hasBRETrue  = any (`elem` pg) [ProGuitar.BREGuitar True , ProGuitar.BREBass True ]
    hasBREFalse = any (`elem` pg) [ProGuitar.BREGuitar False, ProGuitar.BREBass False]
    blip diff = RTB.singleton 0 $ Five.DiffEvent diff $ Five.Note $ Blip () Five.Green
    in foldr RTB.merge RTB.empty $ concat
      [ [ blip d | d <- [minBound .. maxBound], hasNote d ]
      , [ RTB.singleton 0 $ Five.Overdrive True  | hasODTrue   ]
      , [ RTB.singleton 0 $ Five.Overdrive False | hasODFalse  ]
      , [ RTB.singleton 0 $ Five.BRE       True  | hasBRETrue  ]
      , [ RTB.singleton 0 $ Five.BRE       False | hasBREFalse ]
      ]
  in U.trackJoin . fmap pgToBasic . RTB.collectCoincident

-- | Makes a dummy Basic Keys track, for parts with only Pro Keys charted.
expertProKeysToKeys_precodec :: RTB.T U.Beats ProKeys.Event -> RTB.T U.Beats Five.Event
expertProKeysToKeys_precodec = let
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

-- | Makes a Pro Keys track, for parts with only Basic Keys charted.
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

hasSolo :: (NNC.C t) => Instrument -> Song (RBFile.FixedFile t) -> Bool
hasSolo Guitar song = any (not . null)
  [ fiveSolo $ RBFile.fixedPartGuitar $ RBFile.s_tracks song
  , pgSolo $ RBFile.fixedPartRealGuitar $ RBFile.s_tracks song
  , pgSolo $ RBFile.fixedPartRealGuitar22 $ RBFile.s_tracks song
  ]
hasSolo Bass song = any (not . null)
  [ fiveSolo $ RBFile.fixedPartBass $ RBFile.s_tracks song
  , pgSolo $ RBFile.fixedPartRealBass $ RBFile.s_tracks song
  , pgSolo $ RBFile.fixedPartRealBass22 $ RBFile.s_tracks song
  ]
hasSolo Drums song = any (not . null)
  [ drumSolo $ RBFile.fixedPartDrums $ RBFile.s_tracks song
  ]
hasSolo Keys song = any (not . null)
  [ fiveSolo $ RBFile.fixedPartKeys $ RBFile.s_tracks song
  , pkSolo $ RBFile.fixedPartRealKeysX $ RBFile.s_tracks song
  ]
hasSolo Vocal song = any (not . null)
  [ vocalPerc $ RBFile.fixedPartVocals $ RBFile.s_tracks song
  , vocalPerc $ RBFile.fixedHarm1 $ RBFile.s_tracks song
  ]

{-

-- | Gives any note with no lyric a note name lyric.
windLyrics_precodec :: (NNC.C t) => RTB.T t Vocals.Event -> RTB.T t Vocals.Event
windLyrics_precodec = RTB.flatten . fmap f . RTB.collectCoincident where
  f evts = let
    ps = [ p | Vocals.Note True p <- evts ]
    lyrics = [ t | Vocals.Lyric t <- evts ]
    notlyrics = flip filter evts $ \case Vocals.Lyric _ -> False; _ -> True
    in case (ps, lyrics) of
      ([p], [   ]) -> Vocals.Lyric (noteName p       ) : evts
      ([p], ["$"]) -> Vocals.Lyric (noteName p <> "$") : notlyrics
      _            -> evts

noteName :: Vocals.Pitch -> T.Text
noteName = T.concatMap (\case 's' -> "# "; c -> T.singleton c) . T.pack . show . Vocals.pitchToKey

-}
