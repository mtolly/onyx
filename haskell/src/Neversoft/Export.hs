{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Neversoft.Export where

import           Config
import           Control.Monad                    (forM, forM_)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.StackTrace   (SendMessage, StackTraceT,
                                                   stackIO, warn)
import           Data.Bits
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Hashable                    (hash)
import qualified Data.HashMap.Strict              as HM
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   isNothing)
import           Data.SimpleHandle
import qualified Data.Text                        as T
import           Data.Word
import           Guitars                          (applyForces, emit5',
                                                   fixSloppyNotes, getForces5,
                                                   openNotes', strumHOPOTap')
import           Neversoft.Metadata
import           Neversoft.Note
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Beat
import qualified RockBand.Codec.Drums             as D
import           RockBand.Codec.Events
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as F
import           RockBand.Codec.Vocal
import           RockBand.Common                  (Difficulty (..), Edge (..),
                                                   pattern RNil,
                                                   StrumHOPOTap (..),
                                                   pattern Wait,
                                                   joinEdgesSimple,
                                                   noRedundantStatus)
import           RockBand.Legacy.Vocal            (harm1ToPartVocals)
import           RockBand.Sections                (makePSSection)
import           RockBand3                        (BasicTiming (..),
                                                   basicTiming)
import qualified Sound.MIDI.Util                  as U
import           STFS.Package
import qualified System.Directory                 as Dir
import           System.FilePath                  ((<.>))

worGuitarEdits
  :: (NNC.C t)
  => RTB.T t ((Maybe F.Color, StrumHOPOTap), Maybe U.Beats)
  -> RTB.T t ((Maybe F.Color, StrumHOPOTap), Maybe U.Beats)
worGuitarEdits
  = RTB.flatten
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

makeGHWoRNote
  :: (SendMessage m)
  => SongYaml f
  -> TargetGH5
  -> RBFile.Song (RBFile.OnyxFile U.Beats)
  -> StackTraceT m U.Seconds -- ^ get longest audio length
  -> StackTraceT m (GHNoteFile, HM.HashMap Word32 T.Text)
makeGHWoRNote songYaml target song@(RBFile.Song tmap mmap ofile) getAudioLength = let
  beatsToMS :: U.Beats -> Word32
  beatsToMS = floor . (* 1000) . U.applyTempoMap tmap
  makeStarPower od = do
    (t, ((), (), len)) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 $ joinEdgesSimple $ flip fmap od $ \case
      True  -> EdgeOn () ()
      False -> EdgeOff ()
    let start = beatsToMS t
        end   = beatsToMS $ t + len
    return $ Single start (fromIntegral $ end - start)
  makeGB fpart diff = case getPart fpart songYaml >>= partGRYBO of
    Just grybo -> let
      opart = fromMaybe mempty $ Map.lookup fpart $ RBFile.onyxParts ofile
      (trk, algo) = RBFile.selectGuitarTrack RBFile.FiveTypeGuitarExt opart
      fd = fromMaybe mempty $ Map.lookup diff $ F.fiveDifficulties trk
      notes
        = worGuitarEdits
        . applyForces (getForces5 fd)
        . strumHOPOTap' algo (fromIntegral (gryboHopoThreshold grybo) / 480)
        . fixSloppyNotes (10 / 480)
        . openNotes'
        $ fd
        :: RTB.T U.Beats ((Maybe F.Color, StrumHOPOTap), Maybe U.Beats)
      taps = F.fiveTap $ emit5' notes
      in GuitarBass
        { gb_instrument = do
          (t, evts) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.collectCoincident notes
          let start = beatsToMS t
              maybeLen = NE.nonEmpty (map snd evts) >>= minimum
              end = maybe (start + 1) (\bts -> beatsToMS $ t + bts) maybeLen
          return Note
            { noteTimeOffset = beatsToMS t
            , noteDuration = fromIntegral $ end - start
            , noteBits = foldr (.|.) 0 $ do
              -- Open pulloff notes only work on bass for some reason.
              -- For now we just leave them in on guitar to become strums;
              -- could also consider modifying the no-opens algorithm to only
              -- adjust pulloffs and not open strums.
              ((mcolor, sht), _len) <- evts
              let shtBit = if sht /= Strum then bit 6 else 0
                  colorBit = bit $ case mcolor of
                    Just F.Green  -> 0
                    Just F.Red    -> 1
                    Just F.Yellow -> 2
                    Just F.Blue   -> 3
                    Just F.Orange -> 4
                    Nothing       -> 5
              return $ shtBit .|. colorBit
            , noteAccent = 31
            -- TODO noteAccent is required for extended sustains to work.
            -- Clear bits for colors with notes that overlap this one
            }
        , gb_tapping = makeStarPower taps
        , gb_starpower = makeStarPower $ F.fiveOverdrive trk
        }
    Nothing -> GuitarBass [] [] []
  makeDrums fpart diff = case getPart fpart songYaml >>= partDrums of
    Just pd -> let
      opart = fromMaybe mempty $ Map.lookup fpart $ RBFile.onyxParts ofile
      trk = RBFile.onyxPartDrums opart -- TODO use other tracks potentially
      dd = fromMaybe mempty $ Map.lookup diff $ D.drumDifficulties trk
      add2x xs = case diff of
        Expert -> RTB.merge xs $ Nothing <$ D.drumKick2x trk
        _      -> xs
      fiveLane = case drumsMode pd of
        Drums5 -> add2x $ fmap Just $ D.drumGems dd
        DrumsPro -> let
          pro = add2x $ fmap Just $ D.computePro (Just diff) trk
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
        _ -> RTB.empty -- TODO handle basic, real, full
      drumBit = bit . \case
        Just (D.Pro D.Green () , _) -> 0
        Just (D.Red            , _) -> 1
        Just (D.Pro D.Yellow (), _) -> 2
        Just (D.Pro D.Blue ()  , _) -> 3
        Just (D.Orange         , _) -> 4
        Just (D.Kick           , _) -> 5
        Nothing                     -> 6
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
            , noteBits = foldr (.|.) 0 $ map drumBit evts
            , noteAccent = foldr (.|.) 0 $ map accentBit evts
            }
          , xdGhost = foldr (.|.) 0 $ map ghostBit evts
          }
      in Drums
        { drums_instrument = case diff of
          Expert -> Right withGhost
          _      -> Left $ map xdNote withGhost
        , drums_starpower = makeStarPower $ D.drumOverdrive trk
        , drums_drumfill = [] -- TODO
        }
    Nothing -> Drums (case diff of Expert -> Right []; _ -> Left []) [] []
  in do
    (voxNotes, voxLyrics, voxSP, voxPhrases) <- case getPart (gh5_Vocal target) songYaml >>= partVocal of
      Just _pv -> do
        let opart = fromMaybe mempty $ Map.lookup (gh5_Vocal target) $ RBFile.onyxParts ofile
            trk = if nullVox $ RBFile.onyxPartVocals opart
              then harm1ToPartVocals $ RBFile.onyxHarm1 opart
              else RBFile.onyxPartVocals opart
            sp = makeStarPower $ vocalOverdrive trk
            -- TODO include vocalPhrase2
            phrases = map beatsToMS $ ATB.getTimes $ RTB.toAbsoluteEventList 0
              $ RTB.collectCoincident $ vocalPhrase1 trk
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
                ghPitch = maybe 26 (fromIntegral . fromEnum) (mpitch :: Maybe Pitch)
                in Wait t (ghPitch, len) $ case rest of
                  Wait t2 next@(SlideTo _, _) rest2 -> Wait len (2, t2 - len)
                    $ makeNotes $ Wait (t2 - len) next rest2
                  _ -> makeNotes rest
              in \case
                Wait t (Pitched p _, len) rest -> makeNote t (Just p) len rest
                Wait t (Talky   _ _, len) rest -> makeNote t Nothing  len rest
                Wait t (SlideTo p  , len) rest -> makeNote t (Just p) len rest
                RNil -> RNil
        return (notes, lyrics, sp, phrases)
      Nothing -> return ([], [], [], [])
    timing <- basicTiming song getAudioLength
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
        sections = flip fmap (eventsSections $ RBFile.onyxEvents ofile) $ \(_, section) -> let
          (_, sectionPrint) = makePSSection section
          sectionGH = "\\u[m]" <> sectionPrint
          in (fromIntegral $ hash sectionGH, sectionGH)
        sectionMarkers
          = map (\(t, (qsid, _)) -> Single (beatsToMS t) qsid)
          $ ATB.toPairList
          $ RTB.toAbsoluteEventList 0 sections
        sectionBank = HM.fromList $ RTB.getBodies sections
    return (GHNoteFile
      { gh_guitareasy            = makeGB (gh5_Guitar target) Easy
      , gh_guitarmedium          = makeGB (gh5_Guitar target) Medium
      , gh_guitarhard            = makeGB (gh5_Guitar target) Hard
      , gh_guitarexpert          = makeGB (gh5_Guitar target) Expert

      , gh_basseasy              = makeGB (gh5_Bass target) Easy
      , gh_bassmedium            = makeGB (gh5_Bass target) Medium
      , gh_basshard              = makeGB (gh5_Bass target) Hard
      , gh_bassexpert            = makeGB (gh5_Bass target) Expert

      , gh_drumseasy             = makeDrums (gh5_Drums target) Easy
      , gh_drumsmedium           = makeDrums (gh5_Drums target) Medium
      , gh_drumshard             = makeDrums (gh5_Drums target) Hard
      , gh_drumsexpert           = makeDrums (gh5_Drums target) Expert

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
      , gh_vocalmarkers          = [] -- TODO
      , gh_backup_vocalmarkers   = Nothing

      }, sectionBank)

shareMetadata :: (SendMessage m, MonadIO m) => [FilePath] -> StackTraceT m ()
shareMetadata lives = do
  library <- fmap (combineTextPakQBs . concat) $ forM lives $ \live -> do
    folder <- stackIO $ getSTFSFolder live
    let texts = [ r | (name, r) <- folderFiles folder, "_text.pak.xen" `T.isSuffixOf` name ]
    fmap catMaybes $ forM texts $ \r -> do
      bs <- stackIO $ useHandle r handleToByteString
      case readTextPakQB bs of
        Left  err      -> warn err >> return Nothing
        Right contents -> return $ Just contents
  forM_ lives $ \live -> do
    folder <- stackIO $ getSTFSFolder live
    repack <- stackIO $ withSTFSPackage live $ return . repackOptions
    files' <- forM (folderFiles folder) $ \pair@(name, r) ->
      if "_text.pak.xen" `T.isSuffixOf` name
        then do
          bs <- stackIO $ useHandle r handleToByteString
          let bs' = updateTextPakQB library bs
          return (name, makeHandle ("New contents for " <> T.unpack name) $ byteStringSimpleHandle bs')
        else return pair
    let folder' = folder { folderFiles = files' }
    stackIO $ makeCONReadable repack folder' (live <.> "tmp")
    stackIO $ Dir.renameFile live (live <.> "bak")
    stackIO $ Dir.renameFile (live <.> "tmp") live
