{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Build.GuitarHero5.Logic where

import           Control.Monad                    (forM, guard)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Bifunctor                   (first)
import           Data.Bits
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (toLower, toUpper)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (sort)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   isNothing)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Word
import           Numeric                          (showHex)
import           Numeric.NonNegative.Class        ((-|))
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.Build.RB3CH                 (BasicTiming (..),
                                                   basicTiming)
import           Onyx.Guitar                      (emit5', noLowerExtSustains,
                                                   standardBlipThreshold,
                                                   standardSustainGap)
import           Onyx.MIDI.Common                 (Difficulty (..), Edge (..),
                                                   StrumHOPOTap (..),
                                                   joinEdgesSimple,
                                                   noRedundantStatus,
                                                   pattern RNil, pattern Wait,
                                                   trackGlue)
import           Onyx.MIDI.Read                   (mapTrack)
import           Onyx.MIDI.Track.Beat
import qualified Onyx.MIDI.Track.Drums            as D
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.MIDI.Track.Vocal
import           Onyx.MIDI.Track.Vocal.Legacy     (harm1ToPartVocals)
import           Onyx.Mode
import           Onyx.Neversoft.CRC
import           Onyx.Neversoft.GH5.Metadata
import           Onyx.Neversoft.GH5.Note
import           Onyx.Neversoft.Pak
import           Onyx.Neversoft.QB
import           Onyx.PlayStation.NPData          (ghworCustomMidEdatConfig,
                                                   npdContentID, packNPData)
import           Onyx.PlayStation.PKG             (loadPKG, makePKG, pkgFolder,
                                                   tryDecryptEDAT)
import           Onyx.Project
import           Onyx.Reductions                  (completeFiveResult)
import           Onyx.Resources                   (getResourcesPath,
                                                   ghWoRThumbnail)
import           Onyx.Sections                    (makeDisplaySection,
                                                   sectionBody)
import           Onyx.StackTrace                  (SendMessage, StackTraceT,
                                                   errorToWarning, stackIO)
import           Onyx.Util.Handle
import           Onyx.Xbox.STFS
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeExtension, takeFileName,
                                                   (</>))
import           System.IO.Temp                   (withSystemTempDirectory)

worGuitarEdits
  :: RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)
  -> RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)
worGuitarEdits
  = RTB.flatten
  . (let
    -- Repeated hopo chords can't be hit without strumming, so make them strums.
    -- (No tap chords due to below)
    -- Also making chords immediately after tap notes strums, because they are
    -- weirdly hard to hit when hopos (have to play late, even if you strum)
    go = \case
      Wait t1 c1 rest1@(Wait t2 c2 rest2) -> Wait t1 c1 $ go $ let
        isRepeatedChord = not (null $ drop 1 c1)
          && sort (map (fst . fst) c1) == sort (map (fst . fst) c2)
          && any (\((_color, sht), _len) -> sht == HOPO) c2
        isChordAfterTap = not (null $ drop 1 c2)
          && any (\((_color, sht), _len) -> sht == Tap) c1
          && any (\((_color, sht), _len) -> sht == HOPO) c2
        in if isRepeatedChord || isChordAfterTap
          then Wait t2 [ ((color, Strum), len) | ((color, _), len) <- c2 ] rest2
          else rest1
      oneOrEmpty -> oneOrEmpty
    in go
    )
  . fmap (\instant ->
    -- Tapping sections have limitations due to compatibility with the "slider strip".
    -- Tap chords produce notes that look right but can't be hit, so we make them HOPO.
    -- Tap opens also aren't supported (they "work" but make a bizarre "dust cloud" visual),
    -- so we make those HOPO too, but they will actually come out as strums on Guitar
    -- (but correct HOPOs on Bass).
    if any (\((_color, sht), _len) -> sht == Tap) instant
      then if not (null $ drop 1 instant) || any (\((color, _sht), _len) -> isNothing color) instant
        then [ ((color, HOPO), len) | ((color, _), len) <- instant ]
        else instant
      else instant
    )
  . RTB.collectCoincident
  -- Holding higher notes and playing lower ones doesn't work
  . noLowerExtSustains standardBlipThreshold standardSustainGap

data PhrasePoint
  = PhraseNotes Bool
  | PhraseBarLine
  deriving (Eq, Ord)

-- Turn RB vocal phrases into GH phrase boundaries.
-- Empty space is also split into phrases using barlines as potential splits.
makePhrasePoints :: BeatTrack U.Seconds -> RTB.T U.Seconds Bool -> [U.Seconds]
makePhrasePoints beat edges = let
  closeStartTime = 1 :: U.Seconds
  dropCloseStarts = \case
    Wait t1 False (Wait t2 True rest) | t2 < closeStartTime
      -> Wait t1 (PhraseNotes True) $ dropCloseStarts $ RTB.delay t2 rest
    Wait t b rest -> Wait t (PhraseNotes b) $ dropCloseStarts rest
    RNil -> RNil
  minEmptyPhrase = 3 :: U.Seconds
  barLines = RTB.mapMaybe (\case Bar -> Just PhraseBarLine; _ -> Nothing) $ beatLines beat
  filterBarLines inNotes = \case
    Wait t PhraseBarLine rest -> let
      canSplit = not inNotes && t >= minEmptyPhrase && case RTB.filter (/= PhraseBarLine) rest of
        Wait t2 _ _ -> t2 >= minEmptyPhrase
        RNil        -> True
      in if canSplit
        then Wait t PhraseBarLine $ filterBarLines inNotes rest
        else filterBarLines inNotes $ RTB.delay t rest
    Wait t p@(PhraseNotes b) rest -> Wait t p $ filterBarLines b rest
    RNil -> RNil
  in (0 :) $ ATB.getTimes $ RTB.toAbsoluteEventList 0
    $ filterBarLines False
    $ RTB.merge barLines $ dropCloseStarts $ RTB.normalize edges

-- Puts drum freestyle sections when there are long enough gaps of no notes.
-- TODO fills aren't ending?? disabling for now
makeDrumFill :: RTB.T U.Seconds gem -> U.Seconds -> [(U.Seconds, U.Seconds)]
makeDrumFill notes end = let
  notePosns = ATB.getTimes $ RTB.toAbsoluteEventList 0 $ RTB.collectCoincident notes
  gaps = zip (0 : notePosns) (notePosns <> [end])
  potentialFills = flip map gaps $ \(gapStart, gapEnd) ->
    ( if gapStart == 0 then 0 else gapStart + 0.15 -- NS actually appear to use a gap of "32nd note and then 100ms" but this should work
    , if gapEnd == end then end else gapEnd -| 0.1
    )
  minimumFill = 5 :: U.Seconds -- just guessing at a good value
  in filter (\(gapStart, gapEnd) -> gapEnd -| gapStart >= minimumFill) potentialFills

-- GH doesn't support chords of different lengths, so we can join all simultaneous notes together
joinChords
  :: (NNC.C t)
  => RTB.T t ((Maybe Five.Color, StrumHOPOTap), Maybe t)
  -> RTB.T t ([Maybe Five.Color], StrumHOPOTap, Maybe t)
joinChords = fmap joinChord . RTB.collectCoincident where
  joinChord notes = let
    -- these should all be safe due to collectCoincident
    colors = map (fst . fst) notes
    sht = snd $ fst $ head notes
    len = minimum $ map snd notes
    in (colors, sht, len)

annotateOverlaps
  :: (NNC.C t)
  => RTB.T t ([Maybe Five.Color], StrumHOPOTap, Maybe t)
  -> RTB.T t ([Maybe Five.Color], StrumHOPOTap, Maybe t, [Maybe Five.Color])
annotateOverlaps = go . fmap (, Set.empty) where
  go = \case
    RNil -> RNil
    Wait t ((colors, sht, mlen), preheld) rest -> let
      postheld = Set.fromList $ case mlen of
        Nothing  -> []
        Just len -> do
          ((colors', _, _), _) <- RTB.getBodies $ U.trackTake len rest
          colors'
      held = Set.difference (Set.union preheld postheld) $ Set.fromList colors
      rest' = case mlen of
        Nothing -> rest
        Just len -> let
          (duringThis, afterThis) = U.trackSplit len rest
          duringThis' = (\(triple, otherPreheld) -> (triple, Set.union otherPreheld $ Set.fromList colors))
            <$> duringThis
          in trackGlue len duringThis' afterThis
      in Wait t (colors, sht, mlen, Set.toList held) $ go rest'
  -- TODO might need more overlaps marked? weird dropped sustains on Chorus 1 of Informal Gluttony on guitar

data TapFixEvent
  = NoteOff
  | TapEnd
  | TapStart
  | NoteBlip
  | NoteOn
  deriving (Eq, Ord)

-- This was an attempt to fix hopo chords after tap being hard to hit,
-- by pulling back the tap length a bit. It doesn't appear to work, but it's
-- still probably a good idea
pullBackTapEnds :: (NNC.C t, Fractional t) => RTB.T t TapFixEvent -> RTB.T t TapFixEvent
pullBackTapEnds = go . RTB.normalize where
  go rtb = case rtb of
    Wait t1 NoteBlip (Wait t2 TapEnd rest) -> case rest of
      RNil -> rtb
      _    -> Wait t1 NoteBlip $ Wait (t2 / 2) TapEnd $ pullBackTapEnds $ RTB.delay (t2 / 2) rest
    Wait t1 NoteOff (Wait t2 TapEnd rest) -> Wait t1 NoteOff $ Wait 0 TapEnd $ pullBackTapEnds $ RTB.delay t2 rest
    Wait t x rest -> Wait t x $ go rest
    RNil -> RNil

pullBackTapEnds' :: (NNC.C t, Fractional t) => RTB.T t (note, Maybe t) -> RTB.T t Bool -> RTB.T t Bool
pullBackTapEnds' notes taps = let
  result = pullBackTapEnds $ RTB.merge
    (U.trackJoin $ flip fmap notes $ \case
      (_, Just len) -> Wait 0 NoteOn $ Wait len NoteOff RNil
      (_, Nothing ) -> Wait 0 NoteBlip RNil
    )
    (fmap (\b -> if b then TapStart else TapEnd) taps)
  in flip RTB.mapMaybe result $ \case
    TapStart -> Just True
    TapEnd   -> Just False
    _        -> Nothing

makeGHWoRNote
  :: (SendMessage m)
  => SongYaml f
  -> TargetGH5 f
  -> F.Song (F.OnyxFile U.Beats)
  -> StackTraceT m U.Seconds -- ^ get longest audio length
  -> StackTraceT m (GHNoteFile, HM.HashMap Word32 T.Text, [Word32])
makeGHWoRNote songYaml target song@(F.Song tmap mmap ofile) getAudioLength = let
  secondsToMS :: U.Seconds -> Word32
  secondsToMS = floor . (* 1000)
  beatsToMS :: U.Beats -> Word32
  beatsToMS = secondsToMS . U.applyTempoMap tmap
  makeStarPower od = do
    (t, ((), (), len)) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 $ joinEdgesSimple $ flip fmap od $ \case
      True  -> EdgeOn () ()
      False -> EdgeOff ()
    let start = beatsToMS t
        end   = beatsToMS $ t + len
    return $ Single start (fromIntegral $ end - start)
  resultGB fpart = getPart fpart songYaml >>= anyFiveFret >>= \builder ->
    return $ completeFiveResult False mmap $ builder FiveTypeGuitarExt ModeInput
      { tempo  = tmap
      , events = F.onyxEvents ofile
      , part   = F.getFlexPart fpart ofile
      }
  resultGuitar = resultGB target.guitar
  resultBass   = resultGB target.bass
  makeGB :: Maybe FiveResult -> Onyx.MIDI.Common.Difficulty -> GuitarBass
  makeGB Nothing       _    = GuitarBass [] [] []
  makeGB (Just result) diff = let
    notes = worGuitarEdits $ fromMaybe mempty $ Map.lookup diff result.notes
    taps = pullBackTapEnds' notes (emit5' notes).fiveTap
    colorToBit = \case
      Just Five.Green  -> 0
      Just Five.Red    -> 1
      Just Five.Yellow -> 2
      Just Five.Blue   -> 3
      Just Five.Orange -> 4
      Nothing          -> 5
    in GuitarBass
      { gb_instrument = do
        (t, (colors, sht, maybeLen, overlaps)) <- ATB.toPairList $ RTB.toAbsoluteEventList 0
          $ annotateOverlaps $ joinChords notes
        let start = beatsToMS t
            end = maybe (start + 1) (\bts -> beatsToMS $ t + bts) maybeLen
        return Note
          { noteTimeOffset = beatsToMS t
          , noteDuration = fromIntegral $ end - start
          , noteBits = foldr (.|.) 0 $ do
            -- Open pulloff notes only work on bass for some reason.
            -- For now we just leave them in on guitar to become strums;
            -- could also consider modifying the no-opens algorithm to only
            -- adjust pulloffs and not open strums.
            mcolor <- colors
            let shtBit = if sht /= Strum then bit 6 else 0
                colorBit = bit $ colorToBit mcolor
            return $ shtBit .|. colorBit
          -- noteAccent is required for extended sustains to work.
          -- Clear bits for colors with notes that overlap this one
          , noteAccent = foldr (\mcolor n -> n `clearBit` colorToBit mcolor) 31 overlaps
          }
      , gb_tapping = makeStarPower taps
      , gb_starpower = makeStarPower result.other.fiveOverdrive
      }
  drumResult = flip fmap (getPart target.drums songYaml >>= anyDrums) $ \builder ->
    builder DrumTargetGH ModeInput
      { tempo  = tmap
      , events = F.getEventsTrack ofile
      , part   = fromMaybe mempty $ Map.lookup target.drums $ F.onyxParts ofile
      }
  makeDrums diff = case drumResult of
    Just result -> let
      trk = drumResultToTrack result
      dd = fromMaybe mempty $ Map.lookup diff trk.drumDifficulties
      add2x xs = case diff of
        Expert -> RTB.merge (fmap Just xs) $ Nothing <$ trk.drumKick2x
        _      -> fmap Just xs
      fiveLane = case result.settings.mode of
        Drums4 -> add2x dd.drumGems
        Drums5 -> add2x dd.drumGems
        _ | target.proTo4 -> add2x dd.drumGems
        _ -> let
          -- TODO this logic should be moved to Onyx.Mode DrumResult
          pro = add2x $ D.computePro (Just diff) trk
          eachGroup evts = concat
            [ [Just (D.Kick, vel) | Just (D.Kick, vel) <- evts]
            , [Nothing | Nothing <- evts]
            , [Just (D.Red, vel) | Just (D.Red, vel) <- evts]
            -- Y and G notes in input go to the left and right cymbals/toms
            -- B notes go to right cymbal/tom, or left if there's already a right one
            , let
              ycym = [Just (D.Pro D.Yellow (), vel) | Just (D.Pro D.Yellow D.Cymbal, vel) <- evts]
              gcym = [Just (D.Orange, vel) | Just (D.Pro D.Green D.Cymbal, vel) <- evts]
              bcym = do
                Just (D.Pro D.Blue D.Cymbal, vel) <- evts
                return $ Just (if null gcym then D.Orange else D.Pro D.Yellow (), vel)
              in concat [ycym, gcym, bcym]
            , let
              ytom = [Just (D.Pro D.Blue (), vel) | Just (D.Pro D.Yellow D.Tom, vel) <- evts]
              gtom = [Just (D.Pro D.Green (), vel) | Just (D.Pro D.Green D.Tom, vel) <- evts]
              btom = do
                Just (D.Pro D.Blue D.Tom, vel) <- evts
                return $ Just (D.Pro (if null gtom then D.Green else D.Blue) (), vel)
              in concat [ytom, gtom, btom]
            ]
          in RTB.flatten $ fmap eachGroup $ RTB.collectCoincident pro
      fills = [] -- makeDrumFill (U.applyTempoTrack tmap fiveLane) (U.applyTempoMap tmap $ timingEnd timing)
      drumBit = bit . \case
        Just (D.Pro D.Green () , _) -> 0
        Just (D.Red            , _) -> 1
        Just (D.Pro D.Yellow (), _) -> 2
        Just (D.Pro D.Blue ()  , _) -> 3
        Just (D.Orange         , _) -> 4
        Just (D.Kick           , _) -> 5
        Nothing                     -> 6
      drumBitNoGhost = \case
        Just (_, D.VelocityGhost) -> 0
        drum                      -> drumBit drum
      accentBit drum = case drum of
        Just (D.Kick, _)           -> 0
        Just (_, D.VelocityAccent) -> drumBit drum
        _                          -> 0
      ghostBit drum = case drum of
        Just (D.Kick, _)          -> 0
        Just (_, D.VelocityGhost) -> drumBit drum
        _                         -> 0
      withGhost = do
        (t, evts) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.collectCoincident fiveLane
        return NoteExpertDrums
          { xdNote = Note
            { noteTimeOffset = beatsToMS t
            , noteDuration = 1 -- should be fine?
            , noteBits = foldr (.|.) 0 $ map drumBitNoGhost evts
            , noteAccent = foldr (.|.) 0 $ map accentBit evts
            }
          , xdGhost = foldr (.|.) 0 $ map ghostBit evts
          }
      in Drums
        { drums_instrument = case diff of
          Expert -> Right withGhost
          _      -> Left $ map (.xdNote) withGhost
        , drums_starpower = makeStarPower trk.drumOverdrive
        , drums_drumfill = flip map fills $ \(gapStart, gapEnd) ->
          Single (secondsToMS gapStart) (secondsToMS gapEnd)
        }
    Nothing -> Drums (case diff of Expert -> Right []; _ -> Left []) [] []
  in do
    timing <- basicTiming False song getAudioLength
    (voxNotes, voxLyrics, voxSP, voxPhrases, voxMarkers) <- case getPart target.vocal songYaml >>= (.vocal) of
      Just _pv -> do
        let opart = fromMaybe mempty $ Map.lookup target.vocal $ F.onyxParts ofile
            trk = if nullVox $ F.onyxPartVocals opart
              then harm1ToPartVocals $ F.onyxHarm1 opart
              else F.onyxPartVocals opart
            sp = makeStarPower $ vocalOverdrive trk
            phrases = map secondsToMS $ makePhrasePoints
              (mapTrack (U.applyTempoTrack tmap) $ timingBeat timing)
              (U.applyTempoTrack tmap $ vocalPhraseAll trk)
        lyricNotes <- getLyricNotes mmap trk
        let lyrics
              = map (\(t, txt) -> Single (beatsToMS t) txt)
              $ ATB.toPairList
              $ RTB.toAbsoluteEventList 0
              $ makeLyrics False $ fmap fst lyricNotes
            makeLyrics continuing = let
              emitLyric lyric = if continuing
                then "=" <> lyricText lyric
                else lyricText lyric
              in \case
                Wait t (Pitched _ lyric) rest -> Wait t (emitLyric lyric)
                  $ makeLyrics (lyricContinues lyric) rest
                Wait t (Talky _ lyric) rest -> Wait t (emitLyric lyric)
                  $ makeLyrics (lyricContinues lyric) rest
                Wait t (SlideTo _) rest -> RTB.delay t $ makeLyrics continuing rest
                RNil -> RNil
            notes
              = map (\(t, (p, len)) -> VocalNote
                { vnTimeOffset = beatsToMS t
                , vnDuration = fromIntegral $ beatsToMS (t <> len) - beatsToMS t
                , vnPitch = p
                })
              $ ATB.toPairList
              $ RTB.toAbsoluteEventList 0
              $ makeNotes lyricNotes
            makeNotes = let
              makeNote t mpitch len rest = let
                ghPitch = maybe 26 (\p -> fromIntegral $ fromEnum p + 36) (mpitch :: Maybe Pitch)
                in Wait t (ghPitch, len) $ case rest of
                  Wait t2 next@(SlideTo _, _) rest2 -> Wait len (2, t2 - len)
                    $ makeNotes $ Wait (t2 - len) next rest2
                  _ -> makeNotes rest
              in \case
                Wait t (Pitched p _, len) rest -> makeNote t (Just p) len rest
                Wait t (Talky   _ _, len) rest -> makeNote t Nothing  len rest
                Wait t (SlideTo p  , len) rest -> makeNote t (Just p) len rest
                RNil                           -> RNil
            markers
              = map (\(t, label) -> Single (beatsToMS t) label)
              $ ATB.toPairList $ RTB.toAbsoluteEventList 0
              $ getPhraseLabels (vocalPhrase1 trk) lyricNotes
        return (notes, lyrics, sp, phrases, markers)
      Nothing -> return ([], [], [], [], [])
    let fretbars = map beatsToMS $ ATB.getTimes $ RTB.toAbsoluteEventList 0 $ beatLines $ timingBeat timing
        beatToTimesigs = \case
          Wait t _bar rest -> case RTB.span (== Beat) rest of
            (beats, after) -> let
              num = 1 + length beats
              in Wait t num $ beatToTimesigs $ RTB.delay (mconcat $ RTB.getTimes beats) after
          RNil -> RNil
        timesig = do
          (t, num) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 $ noRedundantStatus $ beatToTimesigs $ beatLines $ timingBeat timing
          return TimeSig
            { tsTimestamp = beatsToMS t
            , tsNumerator = fromIntegral num
            , tsDenominator = 4
            }
        sections = flip fmap (eventsSections $ F.onyxEvents ofile) $ \section -> let
          sectionPrint = sectionBody $ makeDisplaySection section
          sectionGH = "\\u[m]" <> sectionPrint
          in (qsKey sectionGH, sectionGH)
        sections' = RTB.merge sections $ let
          -- Not all songs have this event, but it lets you place the song end precisely.
          -- Otherwise it just ends right after the last playable note
          sectionGH = "\\L_ENDOFSONG"
          in RTB.singleton (timingEnd timing) (qsKey sectionGH, sectionGH)
        sectionMarkers
          = map (\(t, (qsid, _)) -> Single (beatsToMS t) qsid)
          $ ATB.toPairList
          $ RTB.toAbsoluteEventList 0 sections'
        sectionBank = HM.fromList $ RTB.getBodies sections'
        drumAnims = case drumResult of
          Nothing -> []
          Just dr -> makeDrumAnimations tmap dr.animations
    return (GHNoteFile
      { gh_guitareasy            = makeGB resultGuitar Easy
      , gh_guitarmedium          = makeGB resultGuitar Medium
      , gh_guitarhard            = makeGB resultGuitar Hard
      , gh_guitarexpert          = makeGB resultGuitar Expert

      , gh_basseasy              = makeGB resultBass Easy
      , gh_bassmedium            = makeGB resultBass Medium
      , gh_basshard              = makeGB resultBass Hard
      , gh_bassexpert            = makeGB resultBass Expert

      , gh_drumseasy             = makeDrums Easy
      , gh_drumsmedium           = makeDrums Medium
      , gh_drumshard             = makeDrums Hard
      , gh_drumsexpert           = makeDrums Expert

      , gh_vocals                = voxNotes
      , gh_vocallyrics           = voxLyrics
      , gh_vocalstarpower        = voxSP
      , gh_vocalphrase           = voxPhrases
      , gh_vocalfreeform         = [] -- TODO

      , gh_backup_vocalphrase    = Nothing
      , gh_backup_vocals         = Nothing
      , gh_backup_vocalfreeform  = Nothing
      , gh_backup_vocallyrics    = Nothing
      , gh_backup_vocalstarpower = Nothing

      , gh_fretbar               = fretbars
      , gh_timesig               = timesig
      , gh_bandmoment            = [] -- TODO

      , gh_markers               = sectionMarkers
      , gh_vocalmarkers          = voxMarkers
      , gh_backup_vocalmarkers   = Nothing

      }, sectionBank, drumAnims)

-- very basic, no velocity, no hihat control
makeDrumAnimations :: U.TempoMap -> RTB.T U.Beats D.Animation -> [Word32]
makeDrumAnimations tmap anims = do
  (secs, anim) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 $ U.applyTempoTrack tmap anims
  pitch <- case anim of
    D.Tom1     D.LH   -> [4]
    D.Tom1     D.RH   -> [34]
    D.Tom2     D.LH   -> [3]
    D.Tom2     D.RH   -> [33]
    D.FloorTom D.LH   -> [2]
    D.FloorTom D.RH   -> [32]
    D.Hihat    D.LH   -> [11]
    D.Hihat    D.RH   -> [41]
    D.Snare  _ D.LH   -> [5]
    D.Snare  _ D.RH   -> [35]
    D.Ride     D.LH   -> [12]
    D.Ride     D.RH   -> [42]
    D.Crash1 _ D.LH   -> [17]
    D.Crash1 _ D.RH   -> [47]
    D.Crash2 _ D.LH   -> [18]
    D.Crash2 _ D.RH   -> [48]
    D.KickRF          -> [30]
    D.Crash1RHChokeLH -> [47]
    D.Crash2RHChokeLH -> [48]
    D.PercussionRH    -> [33]
    _                 -> []
  [round $ secs * 1000, 0x77000000 + pitch * 0x10000 + 0x60]

getAllMetadata :: (SendMessage m, MonadIO m) => [FilePath] -> StackTraceT m [TextPakSongStruct]
getAllMetadata inputs = fmap (combineTextPakQBs . concat) $ forM inputs $ \input -> do
  texts <- case map toLower $ takeExtension input of
    ".pkg" -> do
      folder <- pkgFolder <$> stackIO (loadPKG input)
      fmap catMaybes $ mapM (uncurry $ tryDecryptEDAT "") $ do
        (_, usrdir) <- folderSubfolders folder
        (_, songdir) <- folderSubfolders usrdir
        pair@(name, _) <- folderFiles songdir
        guard $ "_TEXT.PAK.PS3.EDAT" `B.isSuffixOf` name
        return pair
    ".edat" -> toList <$> tryDecryptEDAT "" (B8.pack $ takeFileName input) (fileReadable input)
    _ -> do
      folder <- stackIO $ getSTFSFolder input
      return [ r | (name, r) <- folderFiles folder, "_text.pak.xen" `T.isSuffixOf` name ]
  fmap catMaybes $ forM texts $ \r -> do
    bs <- stackIO $ useHandle r handleToByteString
    errorToWarning $ readTextPakQB bs Nothing Nothing

makeMetadataLIVE :: (SendMessage m, MonadIO m) => [FilePath] -> FilePath -> StackTraceT m ()
makeMetadataLIVE inputs fout = do
  library <- getAllMetadata inputs
  stackIO $ saveMetadataLIVE library fout

makeMetadataPKG :: (SendMessage m, MonadIO m) => [FilePath] -> FilePath -> StackTraceT m ()
makeMetadataPKG inputs fout = do
  library <- getAllMetadata inputs
  stackIO $ saveMetadataPKG library fout

worFileManifest :: QBKey -> T.Text -> QBKey -> [Word32] -> BL.ByteString
worFileManifest titleHashHex cdl manifestQBFilenameKey songIDs = buildPak
  [ ( Node
      { nodeFileType = ".qb"
      , nodeOffset = 0
      , nodeSize = 0
      , nodeFilenamePakKey = 0
      , nodeFilenameKey = manifestQBFilenameKey
      , nodeFilenameCRC = 2997346177 -- same across packs
      , nodeUnknown = 0
      , nodeFlags = 0
      , nodeName = Nothing
      }
    , putQB
      -- first number in each of these sections is same across packs
      [ QBSectionInteger 3850140781 manifestQBFilenameKey 2
      , QBSectionInteger 1403615505 manifestQBFilenameKey 0
      , QBSectionArray 2706631909 manifestQBFilenameKey $ QBArrayOfStruct
        [ [ QBStructHeader
          , QBStructItemArray
            "package_name_checksums"
            (QBArrayOfQbKey [titleHashHex])
          , QBStructItemQbKey
            "format"
            654834362 -- all WoR dlc has this, gh5 might be different
          , QBStructItemString
            "song_pak_stem"
            (TE.encodeUtf8 cdl)
          , QBStructItemArray
            "songs"
            (QBArrayOfInteger songIDs)
          ]
        ]
      ]
    )
  , ( Node
      { nodeFileType = ".last"
      , nodeOffset = 1
      , nodeSize = 0
      , nodeFilenamePakKey = 0
      , nodeFilenameKey = "chunk.last"
      , nodeFilenameCRC = "chunk"
      , nodeUnknown = 0
      , nodeFlags = 0
      , nodeName = Nothing
      }
    , BL.replicate 4 0xAB
    )
  ]

worFileBarePak :: BL.ByteString
worFileBarePak = buildPak
  -- all of this is constant across packs
  [ ( Node
      { nodeFileType = ".qb"
      , nodeOffset = 0
      , nodeSize = 0
      , nodeFilenamePakKey = 0
      , nodeFilenameKey = 2973311508
      , nodeFilenameCRC = 24767173
      , nodeUnknown = 0
      , nodeFlags = 0
      , nodeName = Nothing
      }
    , putQB []
    )
  , ( Node
      { nodeFileType = ".last"
      , nodeOffset = 1
      , nodeSize = 0
      , nodeFilenamePakKey = 0
      , nodeFilenameKey = "chunk.last"
      , nodeFilenameCRC = "chunk"
      , nodeUnknown = 0
      , nodeFlags = 0
      , nodeName = Nothing
      }
    , BL.replicate 4 0xAB
    )
  ]

-- BDLC*_SONG_VRAM.PAK. Takes QB key of e.g. "dlc771"
worFilePS3SongVRAMPak :: QBKey -> BL.ByteString
worFilePS3SongVRAMPak songKeyQB = buildPak
  [ ( Node
      { nodeFileType = ".last"
      , nodeOffset = 0
      , nodeSize = 0
      , nodeFilenamePakKey = songKeyQB
      , nodeFilenameKey = "chunk.last"
      , nodeFilenameCRC = "chunk"
      , nodeUnknown = 0
      , nodeFlags = 0
      , nodeName = Nothing
      }
    , BL.replicate 4 0xAB
    )
  ]

-- Same file for CDL*_TEXT_VRAM.PAK, CDL*_VRAM.PAK, and CMANIFEST_*_VRAM.PAK
worFilePS3EmptyVRAMPak :: BL.ByteString
worFilePS3EmptyVRAMPak = worFilePS3SongVRAMPak 0

worFileTextPak
  :: (QBKey, BL.ByteString) -- QB file
  -> (QBKey, QBKey, QBKey, QBKey, QBKey, BL.ByteString) -- QS file
  -> BL.ByteString
worFileTextPak (qbKey, qb) (qsKey1, qsKey2, qsKey3, qsKey4, qsKey5, qs) = buildPak
  -- all these nodeFilenameCRC are same across packs.
  -- the nodeFilenameKey are different for the first 6 files, and the .stat file (not included). rest are same
  [ ( Node {nodeFileType = ".qb", nodeOffset = 0, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = qbKey, nodeFilenameCRC = 1379803300, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , qb
    )
  , ( Node {nodeFileType = ".qs.en", nodeOffset = 1, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = qsKey1, nodeFilenameCRC = 1379803300, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , qs
    )
  , ( Node {nodeFileType = ".qs.fr", nodeOffset = 2, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = qsKey2, nodeFilenameCRC = 1379803300, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , qs
    )
  , ( Node {nodeFileType = ".qs.it", nodeOffset = 3, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = qsKey3, nodeFilenameCRC = 1379803300, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , qs
    )
  , ( Node {nodeFileType = ".qs.de", nodeOffset = 4, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = qsKey4, nodeFilenameCRC = 1379803300, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , qs
    )
  , ( Node {nodeFileType = ".qs.es", nodeOffset = 5, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = qsKey5, nodeFilenameCRC = 1379803300, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , qs
    )
  , ( Node {nodeFileType = ".qs.en", nodeOffset = 6, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = 2339261848, nodeFilenameCRC = 24767173, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , makeQS []
    )
  , ( Node {nodeFileType = ".qs.fr", nodeOffset = 7, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = 3024241172, nodeFilenameCRC = 24767173, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , makeQS []
    )
  , ( Node {nodeFileType = ".qs.it", nodeOffset = 8, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = 3669621742, nodeFilenameCRC = 24767173, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , makeQS []
    )
  , ( Node {nodeFileType = ".qs.de", nodeOffset = 9, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = 94872913, nodeFilenameCRC = 24767173, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , makeQS []
    )
  , ( Node {nodeFileType = ".qs.es", nodeOffset = 10, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = 3899138369, nodeFilenameCRC = 24767173, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , makeQS []
    )
  , ( Node {nodeFileType = ".last", nodeOffset = 12, nodeSize = 0, nodeFilenamePakKey = 0, nodeFilenameKey = "chunk.last", nodeFilenameCRC = "chunk", nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
    , BL.replicate 4 0xAB
    )
  ]

saveMetadataLIVE :: [TextPakSongStruct] -> FilePath -> IO ()
saveMetadataLIVE library fout = let

  packageTitle = "GH:WoR Customs Database"
  packageTitles = [packageTitle, "", packageTitle, packageTitle, packageTitle, packageTitle]
  packageDescs = let
    s = "Onyx cache file containing metadata for " <> case length library of
      1 -> "1 song"
      n -> T.pack (show n) <> " songs"
    in [s, "", s, s, s, s]
  (titleHashHex, titleHash) = packageNameHash packageTitle
  cdl = "cdl2000000000"
  -- I don't actually think these matter or need to be unique
  manifestQBFilenameKey = 2000000001
  textQBFilenameKey     = 2000000002
  textQS1FilenameKey    = 2000000003
  textQS2FilenameKey    = 2000000004
  textQS3FilenameKey    = 2000000005
  textQS4FilenameKey    = 2000000006
  textQS5FilenameKey    = 2000000007

  (qb, qs) = showTextPakQBQS $ TextPakQB textQBFilenameKey library
  textPak = worFileTextPak
    (textQBFilenameKey, qb)
    (textQS1FilenameKey, textQS2FilenameKey, textQS3FilenameKey, textQS4FilenameKey, textQS5FilenameKey, qs)

  files =
    [ ( "cmanifest_" <> titleHash <> ".pak.xen"
      , worFileManifest titleHashHex (T.pack cdl) manifestQBFilenameKey []
      )
    , ( cdl <> ".pak.xen"
      , worFileBarePak
      )
    , ( cdl <> "_text.pak.xen"
      , textPak
      )
    ]
  folder = Folder
    { folderSubfolders = []
    , folderFiles = map (\(name, bs) -> (T.pack name, makeHandle name $ byteStringSimpleHandle bs)) files
    }

  in do
    thumb <- ghWoRThumbnail >>= B.readFile
    makeCONReadable CreateOptions
      { createNames = packageTitles
      , createDescriptions = packageDescs
      , createTitleID = 0x41560883
      , createTitleName = "Guitar Hero : Warriors of Rock"
      , createThumb = thumb
      , createTitleThumb = thumb
      , createLicenses = [LicenseEntry (-1) 1 1, LicenseEntry (-1) 1 0]
      , createMediaID       = 0
      , createVersion       = 0
      , createBaseVersion   = 0
      , createTransferFlags = 0xC0
      , createLIVE = True
      } folder fout

saveMetadataPKG :: [TextPakSongStruct] -> FilePath -> IO ()
saveMetadataPKG library fout = let

  label = "CUSTOMS_DATABASE" :: T.Text
  (titleHashHex, titleHash) = packageNameHash label
  cdl = "cdl2000000000"
  -- I don't actually think these matter or need to be unique
  manifestQBFilenameKey = 2000000001
  textQBFilenameKey     = 2000000002
  textQS1FilenameKey    = 2000000003
  textQS2FilenameKey    = 2000000004
  textQS3FilenameKey    = 2000000005
  textQS4FilenameKey    = 2000000006
  textQS5FilenameKey    = 2000000007

  (qb, qs) = showTextPakQBQS $ TextPakQB textQBFilenameKey library
  textPak = worFileTextPak
    (textQBFilenameKey, qb)
    (textQS1FilenameKey, textQS2FilenameKey, textQS3FilenameKey, textQS4FilenameKey, textQS5FilenameKey, qs)

  ps3EDATConfig = ghworCustomMidEdatConfig $ TE.encodeUtf8 label
  ps3ContentID  = npdContentID ps3EDATConfig

  files =
    [ ( "cmanifest_" <> titleHash <> ".pak.ps3"
      , worFileManifest titleHashHex (T.pack cdl) manifestQBFilenameKey []
      )
    , ( "cmanifest_" <> titleHash <> "_vram.pak.ps3"
      , worFilePS3EmptyVRAMPak
      )
    , ( cdl <> ".pak.ps3"
      , worFileBarePak
      )
    , ( cdl <> "_vram.pak.ps3"
      , worFilePS3EmptyVRAMPak
      )
    , ( cdl <> "_text.pak.ps3"
      , textPak
      )
    , ( cdl <> "_text_vram.pak.ps3"
      , worFilePS3EmptyVRAMPak
      )
    ]

  in do
    edats <- withSystemTempDirectory "onyx-ps3-wor-cache" $ \tmp -> do
      let edatIn  = tmp </> "in.bin"
          edatOut = tmp </> "out.bin"
      forM files $ \(name, bs) -> do
        let edatName = TE.encodeUtf8 $ T.toUpper (T.pack name) <> ".EDAT"
        BL.writeFile edatIn bs
        packNPData ps3EDATConfig edatIn edatOut edatName
        edat <- BL.fromStrict <$> B.readFile edatOut
        return (edatName, makeHandle (B8.unpack edatName) $ byteStringSimpleHandle edat)
    let main = Folder
          { folderSubfolders = return $ (,) "USRDIR" Folder
            { folderSubfolders = return $ (,) (TE.encodeUtf8 label) Folder
              { folderSubfolders = []
              , folderFiles = edats
              }
            , folderFiles = []
            }
          , folderFiles = []
          }
    extra <- getResourcesPath "pkg-contents/ghwor" >>= fmap (first TE.encodeUtf8) . crawlFolder
    makePKG ps3ContentID (main <> extra) fout

packageNameHashFormat :: Bool -> T.Text -> B.ByteString
packageNameHashFormat caps t = B8.pack $ do
  c <- take 42 $ T.unpack t -- not sure if cutoff is 42 chars, 96 bytes, 42 non-zero bytes, or something else
  let inRange x y = x <= c && c <= y
  if inRange 'a' 'z' || inRange 'A' 'Z' || inRange '0' '9'
    then [if caps then toUpper c else toLower c]
    else "_"

-- The STFS package display name is used to make the hash in the manifest file's name.
-- TODO does not work for (I think) characters beyond U+00FF. Seen with U+2019 (right single quote)
packageNameHash :: T.Text -> (QBKey, String)
packageNameHash t = let
  titleHashHex = qbKeyCRC $ packageNameHashFormat False t
  -- Previously I thought this needed to be padded with 0 in front, but apparently not
  titleHashStr = showHex (fromQBKey titleHashHex) ""
  in (titleHashHex, titleHashStr)
