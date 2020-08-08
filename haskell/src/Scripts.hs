{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Scripts where

import           Config                           (PreviewTime (..), SongYaml,
                                                   _metadata, _previewEnd,
                                                   _previewStart)
import           Control.Monad                    (forM, guard, void)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.StackTrace
import           Data.DTA.Serialize.Magma         (Percussion)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust,
                                                   listToMaybe, mapMaybe)
import           Development.Shake
import qualified FretsOnFire                      as FoF
import           Guitars
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Beat
import           RockBand.Codec.Drums
import           RockBand.Codec.Events
import           RockBand.Codec.File              (HasEvents (..), Song (..))
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.Five              as Five
import           RockBand.Codec.ProGuitar
import           RockBand.Codec.ProKeys
import           RockBand.Codec.Vocal
import           RockBand.Common
import qualified RockBand.Legacy.Vocal            as Vocals
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U

-- | Changes all existing drum mix events to use the given config (not changing
-- stuff like discobeat), and places ones at the beginning if they don't exist
-- already.
setDrumMix :: (NNC.C t) => Audio -> DrumTrack t -> DrumTrack t
setDrumMix audio trk = let
  f dd = dd
    { drumMix = let
      mixSet = fmap (\(_, disco) -> (audio, disco)) $ drumMix dd
      alreadyMixed = case (RTB.viewL $ drumMix dd, RTB.viewL $ drumGems dd) of
        (Just ((tmix, _), _), Just ((tnote, _), _)) -> tmix <= tnote
        _                                           -> False
      in if alreadyMixed then mixSet else RTB.cons NNC.zero (audio, NoDisco) mixSet
    }
  in trk { drumDifficulties = fmap f $ drumDifficulties trk }

-- | Adds an event at position zero *after* all the other events there.
addZero :: (NNC.C t) => a -> RTB.T t a -> RTB.T t a
addZero x rtb = case U.trackSplitZero rtb of
  (zero, rest) -> U.trackGlueZero (zero ++ [x]) rest

-- | Moves star power from the GH 1/2 format to the RB format, either if it is
-- specified in the song.ini, or automatically detected from the MIDI.
loadFoFMIDI :: (SendMessage m, MonadIO m, RBFile.ParseFile f) => FoF.Song -> FilePath -> StackTraceT m (Song (f U.Beats))
loadFoFMIDI ini fp = do
  mid <- RBFile.loadRawMIDI fp
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
          _                         -> []
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
shakeMIDI fp = lift (lift $ need [fp]) >> RBFile.loadMIDI fp

getTempos :: (Monad m) => F.T -> StackTraceT m U.TempoMap
getTempos mid = case U.decodeFile mid of
  Left []               -> return $ U.makeTempoMap RTB.empty
  Left (tempoTrack : _) -> return $ U.makeTempoMap tempoTrack
  Right _               -> fatal "SMPTE midi not supported"

saveMIDI :: (MonadIO m, RBFile.ParseFile f) => FilePath -> Song (f U.Beats) -> m ()
saveMIDI fp song = liftIO $ Save.toFile fp $ RBFile.showMIDIFile' song

evalPreviewTime :: Bool -> Maybe (f -> EventsTrack U.Beats) -> Song f -> PreviewTime -> Maybe U.Seconds
evalPreviewTime leadin getEvents song = \case
  PreviewSeconds secs -> Just secs
  PreviewMIDI mb -> Just $ addLeadin $ U.applyTempoMap (s_tempos song) $ U.unapplyMeasureMap (s_signatures song) mb
  PreviewSection str -> addLeadin . U.applyTempoMap (s_tempos song) <$> findSection str
  where addLeadin = if leadin then max 0 . subtract 0.6 else id
        findSection sect = getEvents >>= \f ->
          fmap (fst . fst) $ RTB.viewL $ RTB.filter ((== sect) . snd)
            $ eventsSections $ f $ s_tracks song

-- | Returns the start and end of the preview audio in milliseconds.
previewBounds :: (HasEvents f) => SongYaml file -> Song (f U.Beats) -> (Int, Int)
previewBounds syaml song = let
  len = songLengthMS song
  secsToMS s = floor $ s * 1000
  evalTime t = secsToMS <$> evalPreviewTime True (Just getEventsTrack) song t
  evalTime' pt = fromMaybe (error $ "Couldn't evaluate preview bound: " ++ show pt) $ evalTime pt
  defStartTime = case mapMaybe (evalTime . PreviewSection) ["chorus", "chorus_1", "chorus_1a", "verse", "verse_1"] of
    []    -> max 0 $ quot len 2 - 15000
    t : _ -> min (len - 30000) t
  in case (_previewStart $ _metadata syaml, _previewEnd $ _metadata syaml) of
    (Nothing, Nothing) -> (defStartTime, defStartTime + 30000)
    (Just ps, Just pe) -> (evalTime' ps, evalTime' pe)
    (Just ps, Nothing) -> let start = evalTime' ps in (start, start + 30000)
    (Nothing, Just pe) -> let end = evalTime' pe in (end - 30000, end)

songLengthBeats :: (HasEvents f) => Song (f U.Beats) -> U.Beats
songLengthBeats s = case RTB.getTimes $ eventsEnd $ getEventsTrack $ s_tracks s of
  [bts] -> bts
  _     -> 0 -- eh

-- | Returns the time of the [end] event in milliseconds.
songLengthMS :: (HasEvents f) => Song (f U.Beats) -> Int
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

fixFreeformDrums :: DrumTrack U.Beats -> DrumTrack U.Beats
fixFreeformDrums ft = ft
  { drumSingleRoll = fixFreeform' gems $ drumSingleRoll ft
  , drumDoubleRoll = fixFreeform' gems $ drumDoubleRoll ft
  } where gems = maybe RTB.empty (void . drumGems) $ Map.lookup Expert $ drumDifficulties ft

fixFreeformFive :: FiveTrack U.Beats -> FiveTrack U.Beats
fixFreeformFive ft = ft
  { fiveTremolo = fixFreeform' gems $ fiveTremolo ft
  , fiveTrill   = fixFreeform' gems $ fiveTrill   ft
  } where gems = maybe RTB.empty (void . fiveGems) $ Map.lookup Expert $ fiveDifficulties ft

fixFreeformPK :: ProKeysTrack U.Beats -> ProKeysTrack U.Beats
fixFreeformPK ft = ft
  { pkGlissando = fixFreeform gems $ pkGlissando ft
  , pkTrill     = fixFreeform gems $ pkTrill     ft
  } where gems = void $ pkNotes ft

fixFreeformPG :: ProGuitarTrack U.Beats -> ProGuitarTrack U.Beats
fixFreeformPG ft = ft
  { pgTremolo = fixFreeform' gems $ pgTremolo ft
  , pgTrill   = fixFreeform' gems $ pgTrill   ft
  } where gems = maybe RTB.empty (void . pgNotes) $ Map.lookup Expert $ pgDifficulties ft

-- | Adjusts instrument tracks so rolls on notes 126/127 end just a tick after
--- their last gem note-on.
fixFreeform :: RTB.T U.Beats () -> RTB.T U.Beats Bool -> RTB.T U.Beats Bool
fixFreeform initGems = fmap isJust . fixFreeform' initGems . fmap (\b -> guard b >> Just ())

fixFreeform' :: (Ord a) => RTB.T U.Beats () -> RTB.T U.Beats (Maybe a) -> RTB.T U.Beats (Maybe a)
fixFreeform' initGems = go initGems . RTB.normalize where
  go gems lanes = case RTB.viewL lanes of
    Just ((dt, Just x), lanes') -> case RTB.viewL lanes' of
      Just ((len, Nothing), lanes'') -> let
        covered = U.trackTake len $ U.trackDrop dt gems
        len' = case sum $ RTB.getTimes covered of
          0 -> len -- no gems, shouldn't happen
          s -> s + 1/32
        in RTB.cons dt (Just x) $ RTB.insert len' Nothing $
          go (U.trackDrop dt gems) (RTB.delay len lanes'')
      _ -> lanes -- on not followed by off, abort
    Just ((_, Nothing), _) -> lanes -- off not preceded by on, abort
    Nothing -> RTB.empty -- done

-- This doesn't currently handle
-- [note start, phrase boundary, phrase boundary, note end]
-- i.e. a note that spans 3 phrases. But you shouldn't be doing that anyways!
harm1ToPartVocals :: (NNC.C t) => VocalTrack t -> VocalTrack t
harm1ToPartVocals = Vocals.vocalFromLegacy . go . RTB.normalize . Vocals.vocalToLegacy where
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

getPercType :: (NNC.C t) => Song (RBFile.FixedFile t) -> Maybe Percussion
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
protarToGrybo :: ProGuitarTrack U.Beats -> FiveTrack U.Beats
protarToGrybo pg = mempty
  { fiveDifficulties = flip fmap (pgDifficulties pg) $ \pgd -> mempty
    { fiveGems
      = blipEdgesRB_
      $ fmap head
      $ RTB.collectCoincident
      $ noExtendedSustains' standardBlipThreshold standardSustainGap
      $ fmap (\(_, _, len) -> (Five.Green, len))
      $ edgeBlipsRB
      $ pgNotes pgd
    }
  , fiveOverdrive    = pgOverdrive pg
  , fiveBRE          = fmap snd $ pgBRE pg
  , fiveSolo         = pgSolo pg
  }

-- | Makes a dummy Basic Keys track, for parts with only Pro Keys charted.
expertProKeysToKeys :: ProKeysTrack U.Beats -> FiveTrack U.Beats
expertProKeysToKeys pk = mempty
  { fiveDifficulties = let
    fd = mempty
      { fiveGems
        = blipEdgesRB_
        $ fmap head
        $ RTB.collectCoincident
        $ noExtendedSustains' standardBlipThreshold standardSustainGap
        $ fmap (\(_, len) -> (Five.Green, len))
        $ edgeBlipsRB_
        $ pkNotes pk
      }
    in Map.fromList [ (diff, fd) | diff <- [minBound .. maxBound] ]
  , fiveOverdrive    = pkOverdrive pk
  , fiveBRE          = pkBRE pk
  , fiveSolo         = pkSolo pk
  }

-- | Makes a Pro Keys track, for parts with only Basic Keys charted.
keysToProKeys :: (NNC.C t) => Difficulty -> FiveTrack t -> ProKeysTrack t
keysToProKeys d ft = ProKeysTrack
  { pkLanes     = RTB.singleton NNC.zero RangeA
  , pkTrainer   = RTB.empty
  , pkMood      = RTB.empty
  , pkSolo      = if d == Expert then fiveSolo ft else RTB.empty
  , pkGlissando = RTB.empty
  , pkTrill     = case d of
    Expert -> isJust <$> fiveTrill ft
    -- TODO add Hard trills
    _      -> RTB.empty
  , pkOverdrive = if d == Expert then fiveOverdrive ft else RTB.empty
  , pkBRE       = if d == Expert then fiveBRE ft else RTB.empty
  , pkNotes     = case Map.lookup d $ fiveDifficulties ft of
    Nothing -> RTB.empty
    Just fd -> let
      colorToKey = BlueGreen . \case
        Five.Green  -> C
        Five.Red    -> D
        Five.Yellow -> E
        Five.Blue   -> F
        Five.Orange -> G
      in fmap colorToKey <$> fiveGems fd
  }

hasSolo :: (NNC.C t) => RB3Instrument -> Song (RBFile.FixedFile t) -> Bool
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
