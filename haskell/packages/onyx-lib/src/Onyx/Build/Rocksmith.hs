{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
module Onyx.Build.Rocksmith (rsRules) where

import           Control.Monad.Extra
import qualified Data.Aeson                       as A
import           Data.Conduit.Audio
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Fixed (..), Milli)
import           Data.Foldable                    (toList)
import           Data.Int                         (Int32)
import           Data.List.Extra                  (nubOrd)
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   isNothing, listToMaybe)
import qualified Data.Text                        as T
import           Data.Time                        (defaultTimeLocale,
                                                   formatTime, getZonedTime)
import qualified Data.UUID                        as UUID
import qualified Data.UUID.V4                     as UUID
import qualified Data.Vector                      as V
import           Data.Version                     (showVersion)
import           Development.Shake                hiding (phony, (%>), (&%>))
import           Development.Shake.FilePath
import           Onyx.Audio
import           Onyx.Build.Common
import           Onyx.Codec.JSON                  (toJSON)
import           Onyx.FFMPEG                      (audioIntegratedVolume)
import           Onyx.MIDI.Common
import           Onyx.MIDI.Track.Beat
import           Onyx.MIDI.Track.File             (saveMIDI, shakeMIDI)
import qualified Onyx.MIDI.Track.File             as RBFile
import           Onyx.MIDI.Track.ProGuitar
import           Onyx.MIDI.Track.Rocksmith
import           Onyx.MIDI.Track.Vocal            (nullVox)
import           Onyx.Project                     hiding (Difficulty)
import qualified Onyx.Rocksmith.ArrangementXML    as Arr
import qualified Onyx.Rocksmith.CST               as CST
import qualified Onyx.Rocksmith.DLCBuilder        as DLC
import           Onyx.StackTrace
import           Paths_onyx_lib                   (version)
import qualified Sound.MIDI.Util                  as U
import           System.Random                    (randomRIO)

rsRules :: BuildInfo -> FilePath -> TargetRS -> QueueLog Rules ()
rsRules buildInfo dir rs = do

  let songYaml = biSongYaml buildInfo
      rel = biRelative buildInfo

  (planName, plan) <- case getPlan rs.rs_Common.tgt_Plan songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show rs
    Just pair -> return pair
  let planDir = rel $ "gen/plan" </> T.unpack planName

  let isBass = \case
        RSArrSlot _ RSBass -> True
        _                  -> False
      presentPlayable = do
        (arrSlot, fpart) <- rs.rs_Arrangements
        -- TODO warn if arrangement does not have pro guitar mode
        pg <- maybe [] (toList . (.partProGuitar)) $ getPart fpart songYaml
        return (fpart, RSPlayable arrSlot pg)
      presentParts = presentPlayable <> do
        let fpart = rs.rs_Vocal
        pv <- maybe [] (toList . (.partVocal)) $ getPart fpart songYaml
        return (fpart, RSVocal pv)
      rsPadding = dir </> "padding.txt"
      rsAnchors = dir </> "anchors.mid"
      rsBuilder = dir </> "cst/project.rs2dlc"
      rsProject = dir </> "cst/project.dlc.xml"
      rsAudio   = dir </> "cst/audio.wav"
      rsPreview = dir </> "cst/audio_preview.wav" -- this has to be named same as audio + "_preview" for CST to load it
      rsArt     = dir </> "cst/cover.png"
      rsArr p arrSlot = let
        suffix = case arrSlot of
          RSVocal _         -> "vocals" -- NOTE this is required! CST breaks if the filename does not have "vocals"
          -- see https://github.com/rscustom/rocksmith-custom-song-toolkit/blob/fa63cc4e0075/RocksmithToolkitLib/XML/Song2014.cs#L347
          -- similarly, we'll need "showlights" when that is implemented
          RSPlayable slot _ -> if isBass slot then "b" else "g"
        in dir </> "cst/" <> T.unpack (RBFile.getPartName p) <> "." <> suffix <> ".arr.xml"

  phony (dir </> "cst") $ shk $ need $
    [rsBuilder, rsProject, rsAudio, rsPreview, rsArt] ++ [ rsArr p arrSlot | (p, arrSlot) <- presentParts ]

  rsPadding %> \out -> do
    mid <- shakeMIDI $ planDir </> "processed.mid"
    let firstNoteBeats = do
          (fpart, RSPlayable _ _) <- presentParts
          let opart = RBFile.getFlexPart fpart $ RBFile.s_tracks mid
          trk <- [RBFile.onyxPartRSBass opart, RBFile.onyxPartRSGuitar opart]
          Wait dt _ _ <- [rsNotes trk]
          return dt
        targetTime = 10 :: U.Seconds
        firstNoteTime = case NE.nonEmpty firstNoteBeats of
          Nothing -> targetTime
          Just ts -> U.applyTempoMap (RBFile.s_tempos mid) $ minimum ts
    stackIO $ writeFile out $ show $ if firstNoteTime >= targetTime
      then 0
      else realToFrac $ targetTime - firstNoteTime :: Milli

  rsAnchors %> \out -> do
    mid <- shakeMIDI $ planDir </> "processed.mid"
    let eachTrack trk = if RTB.null $ rsNotes trk
          then return trk
          else do
            rso <- buildRS (RBFile.s_tempos mid) 0 trk
            return $ backportAnchors (RBFile.s_tempos mid) trk rso
    newParts <- forM (RBFile.onyxParts $ RBFile.s_tracks mid) $ \opart -> do
      gtr  <- eachTrack $ RBFile.onyxPartRSGuitar opart
      bass <- eachTrack $ RBFile.onyxPartRSBass   opart
      return opart
        { RBFile.onyxPartRSGuitar = gtr
        , RBFile.onyxPartRSBass   = bass
        }
    saveMIDI out $ mid
      { RBFile.s_tracks = (RBFile.s_tracks mid)
        { RBFile.onyxParts = newParts
        }
      }

  forM_ presentParts $ \(fpart, arrSlot) -> do
    rsArr fpart arrSlot %> \out -> do
      mid <- shakeMIDI $ planDir </> "processed.mid"
      pad <- shk $ (realToFrac :: Milli -> U.Seconds) . read <$> readFile' rsPadding
      case arrSlot of
        RSVocal _pv -> do
          let opart = RBFile.getFlexPart fpart $ RBFile.s_tracks mid
              trk = if nullVox $ RBFile.onyxPartVocals opart
                then RBFile.onyxHarm1 opart
                else RBFile.onyxPartVocals opart
              vox = buildRSVocals (RBFile.s_tempos mid) trk
          Arr.writePart out $ Arr.addPadding pad $ Arr.PartVocals vox
        RSPlayable slot pg -> do
          mapM_ (shk . need . toList) pg.pgTones
          toneKeys <- forM pg.pgTones $ mapM $ fmap CST.t14_Key . CST.parseTone
          -- TODO the first beat event needs to be a barline,
          -- otherwise DDC fails to run!
          -- also, notes can't go past the last beat event, or they disappear.
          let ebeats = V.fromList $ numberBars 1 $ ATB.toPairList
                $ RTB.toAbsoluteEventList 0
                $ U.applyTempoTrack (RBFile.s_tempos mid)
                $ beatLines $ RBFile.onyxBeat $ RBFile.s_tracks mid
              numberBars _       [] = []
              numberBars measure ((t, Beat) : rest)
                = Arr.Ebeat t Nothing : numberBars measure rest
              numberBars measure ((t, Bar) : rest)
                = Arr.Ebeat t (Just measure) : numberBars (measure + 1) rest
              tuning0 = case (isBass slot, pg.pgTuningRSBass) of
                (True, Just tun) -> tun
                _                -> pg.pgTuning
              tuning1 = map (+ gtrGlobal tuning0)
                $ encodeTuningOffsets tuning0 (if isBass slot then TypeBass else TypeGuitar)
              tuning2 = tuning1 <> repeat (last tuning1) -- so 5-string bass has a consistent dummy top string
              octaveDown = head tuning2 < (if isBass slot then -4 else -7)
              -- NOTE: the cutoff is -4 for bass because for some reason CST fails
              -- when trying to apply the low bass fix
              tuning3 = map (+ if octaveDown then 12 else 0) tuning2
              lengthBeats = RBFile.songLengthBeats mid
              lengthSeconds = U.applyTempoMap (RBFile.s_tempos mid) lengthBeats
          rso <- let
            opart = RBFile.getFlexPart fpart $ RBFile.s_tracks mid
            trk = if isBass slot
              then RBFile.onyxPartRSBass   opart
              else RBFile.onyxPartRSGuitar opart
              -- TODO maybe support using bass track for a guitar slot
            in buildRS (RBFile.s_tempos mid) (gtrCapo tuning0) trk
          let allNotes = Arr.lvl_notes $ rso_level rso
          time <- stackIO getZonedTime
          Arr.writePart out $ Arr.addPadding pad $ Arr.PartArrangement Arr.Arrangement
            { Arr.arr_version                = 7 -- this is what EOF has currently
            , Arr.arr_title                  = targetTitle songYaml $ RS rs
            , Arr.arr_arrangement            = case slot of
              RSArrSlot _ RSLead        -> "Lead"
              RSArrSlot _ RSRhythm      -> "Rhythm"
              RSArrSlot _ RSComboLead   -> "Combo"
              RSArrSlot _ RSComboRhythm -> "Combo"
              RSArrSlot _ RSBass        -> "Bass"
            , Arr.arr_part                   = 1 -- TODO what is this?
            , Arr.arr_offset                 = 0
            , Arr.arr_centOffset             = getTuningCents plan + if octaveDown then -1200 else 0
            , Arr.arr_songLength             = lengthSeconds
            , Arr.arr_lastConversionDateTime = T.pack $ formatTime defaultTimeLocale
              "%-m-%d-%y %-H:%M"
              time
            , Arr.arr_startBeat              = 0
            , Arr.arr_averageTempo           = U.makeTempo lengthBeats lengthSeconds
            , Arr.arr_tuning                 = Arr.Tuning
              { Arr.tuning_string0 = fromMaybe 0 $ listToMaybe tuning3
              , Arr.tuning_string1 = fromMaybe 0 $ listToMaybe $ drop 1 tuning3
              , Arr.tuning_string2 = fromMaybe 0 $ listToMaybe $ drop 2 tuning3
              , Arr.tuning_string3 = fromMaybe 0 $ listToMaybe $ drop 3 tuning3
              , Arr.tuning_string4 = fromMaybe 0 $ listToMaybe $ drop 4 tuning3
              , Arr.tuning_string5 = fromMaybe 0 $ listToMaybe $ drop 5 tuning3
              }
            , Arr.arr_capo                   = gtrCapo tuning0
            , Arr.arr_artistName             = getArtist songYaml.metadata
            , Arr.arr_artistNameSort         = getArtist songYaml.metadata -- TODO
            , Arr.arr_albumName              = getAlbum songYaml.metadata
            , Arr.arr_albumYear              = songYaml.metadata.year
            , Arr.arr_crowdSpeed             = 1
            , Arr.arr_arrangementProperties  = Arr.ArrangementProperties
              { Arr.ap_represent         = True -- this is always true in arrangement xmls, but false for bonus (+ vocal/lights) in the project xml?
              , Arr.ap_bonusArr          = case slot of
                RSArrSlot RSDefault   _ -> False
                RSArrSlot RSBonus     _ -> True
                RSArrSlot RSAlternate _ -> True
              , Arr.ap_standardTuning    = all (== 0) tuning1
              , Arr.ap_nonStandardChords = False -- TODO :: Bool
              , Arr.ap_barreChords       = False -- TODO :: Bool
              , Arr.ap_powerChords       = False -- TODO :: Bool
              , Arr.ap_dropDPower        = False -- TODO :: Bool
              , Arr.ap_openChords        = False -- TODO :: Bool
              , Arr.ap_fingerPicking     = False -- TODO :: Bool
              , Arr.ap_pickDirection     = any (isJust . Arr.n_pickDirection) allNotes
              , Arr.ap_doubleStops       = False -- TODO :: Bool
              , Arr.ap_palmMutes         = any Arr.n_palmMute allNotes
              , Arr.ap_harmonics         = any Arr.n_harmonic allNotes
              , Arr.ap_pinchHarmonics    = any Arr.n_harmonicPinch allNotes
              , Arr.ap_hopo              = any Arr.n_hopo allNotes
              , Arr.ap_tremolo           = any Arr.n_tremolo allNotes
              , Arr.ap_slides            = any (isJust . Arr.n_slideTo) allNotes
              , Arr.ap_unpitchedSlides   = any (isJust . Arr.n_slideUnpitchTo) allNotes
              , Arr.ap_bends             = any (\n -> isJust (Arr.n_bend n) || not (V.null $ Arr.n_bendValues n)) allNotes
              , Arr.ap_tapping           = any Arr.n_tap allNotes
              , Arr.ap_vibrato           = any (isJust . Arr.n_vibrato) allNotes
              , Arr.ap_fretHandMutes     = any Arr.n_mute allNotes
              , Arr.ap_slapPop           = any (\n -> any isJust [Arr.n_slap n, Arr.n_pluck n]) allNotes
              , Arr.ap_twoFingerPicking  = False -- TODO :: Bool
              , Arr.ap_fifthsAndOctaves  = False -- TODO :: Bool
              , Arr.ap_syncopation       = False -- TODO :: Bool
              , Arr.ap_bassPick          = pg.pgPickedBass
              , Arr.ap_sustain           = any (isJust . Arr.n_sustain) allNotes
              -- TODO what does Combo select for these?
              , Arr.ap_pathLead          = case slot of
                RSArrSlot _ RSLead -> True
                _                  -> False
              , Arr.ap_pathRhythm        = case slot of
                RSArrSlot _ RSRhythm -> True
                _                    -> False
              , Arr.ap_pathBass          = case slot of
                RSArrSlot _ RSBass -> True
                _                  -> False
              , Arr.ap_routeMask         = Nothing
              }
            , Arr.arr_phrases                = rso_phrases rso
            , Arr.arr_phraseIterations       = rso_phraseIterations rso
            , Arr.arr_chordTemplates         = rso_chordTemplates rso
            , Arr.arr_tonebase               = (.rsFileToneBase) <$> toneKeys
            , Arr.arr_tonea                  = toneKeys >>= (.rsFileToneA)
            , Arr.arr_toneb                  = toneKeys >>= (.rsFileToneB)
            , Arr.arr_tonec                  = toneKeys >>= (.rsFileToneC)
            , Arr.arr_toned                  = toneKeys >>= (.rsFileToneD)
            , Arr.arr_tones
              = V.fromList
              $ map (\(t, letter) -> let
                in Arr.Tone
                  { tone_time = t
                  , tone_id   = Just $ fromEnum letter
                  , tone_name = fromMaybe "" $ toneKeys >>= case letter of
                    ToneA -> (.rsFileToneA)
                    ToneB -> (.rsFileToneB)
                    ToneC -> (.rsFileToneC)
                    ToneD -> (.rsFileToneD)
                  }
                )
              $ ATB.toPairList
              $ RTB.toAbsoluteEventList 0
              $ rso_tones rso
            , Arr.arr_ebeats                 = ebeats
            , Arr.arr_sections               = rso_sections rso
            , Arr.arr_events                 = mempty -- TODO :: V.Vector Event
            , Arr.arr_transcriptionTrack     = Arr.Level
              { Arr.lvl_difficulty    = -1
              , Arr.lvl_notes         = mempty
              , Arr.lvl_chords        = mempty
              , Arr.lvl_fretHandMutes = mempty
              , Arr.lvl_anchors       = mempty
              , Arr.lvl_handShapes    = mempty
              }
            , Arr.arr_levels                 = V.singleton $ rso_level rso
            }

  rsAudio %> \out -> do
    pad <- shk $ (realToFrac :: Milli -> Seconds) . read <$> readFile' rsPadding
    let wav = planDir </> "everything.wav"
    case pad of
      0 -> shk $ copyFile' wav out
      _ -> buildAudio (Pad Start (Seconds pad) $ Input wav) out
  rsPreview %> \out -> do
    mid <- shakeMIDI $ planDir </> "processed.mid"
    let (pstart, pend) = previewBounds songYaml (mid :: RBFile.Song (RBFile.OnyxFile U.Beats)) 0 False
        fromMS ms = Seconds $ fromIntegral (ms :: Int) / 1000
        previewExpr
          = Fade End (Seconds 5)
          $ Fade Start (Seconds 2)
          $ Take Start (fromMS $ pend - pstart)
          $ Drop Start (fromMS pstart)
          $ Input (planDir </> "everything.wav")
    buildAudio previewExpr out
  rsArt %> shk . copyFile' (rel "gen/cover-full.png")
  let getRSKey = case rs.rs_SongKey of
        Nothing -> stackIO $ T.pack . ("OnyxCST" <>) . show <$> randomRIO (0, maxBound :: Int32)
        -- TODO maybe autogenerate a CST-like key, e.g. OnyASAMACrimsonRoseandaGinToni
        Just k  -> if T.length k <= 30
          then return k
          else do
            warn $ "RS song key of " <> show k <> " truncated, longer than max length of 30 characters"
            return $ T.take 30 k
      {-
        Info from CST source on song key (+ tone key):

        Limited to a maximum length of 30 charactures, minimum of 6 charactures for uniqueness
        Only Ascii Alpha and Numeric may be used
        No spaces, no special characters, no puncuation
        All alpha lower, upper, or mixed case are allowed
        All numeric is allowed
      -}
  rsBuilder %> \out -> do
    let allTonePaths = nubOrd $ do
          (_, RSPlayable _ pg) <- presentParts
          tones <- toList pg.pgTones
          toList tones
    shk $ need $ [ rsArr fpart arrSlot | (fpart, arrSlot) <- presentParts ] ++ allTonePaths
    allTones <- forM allTonePaths $ \f -> do
      tone <- CST.parseTone f
      return (f, tone)
    parsedArrFiles <- forM presentParts $ \(fpart, arrSlot) -> do
      contents <- Arr.parseFile $ rsArr fpart arrSlot
      return (fpart, arrSlot, contents)
    key <- getRSKey
    shk $ need [rsAudio]
    volume <- stackIO $ audioIntegratedVolume rsAudio
    when (isNothing volume) $ warn "Unable to calculate integrated volume of audio file"
    prevVolume <- stackIO $ audioIntegratedVolume rsPreview
    when (isNothing prevVolume) $ warn "Unable to calculate integrated volume of preview audio file"
    arrangements <- forM (zip [0..] parsedArrFiles) $ \(i, (fpart, arrSlot, contents)) -> do
      -- TODO check if the following still applies with DLC Builder:
      -- ArrangementSort doesn't appear to work for multiples of the same arrangement label.
      -- Instead it sorts by the arrangement ID UUID!
      -- So, we could just use the first character or so to encode `i`.
      persistentID <- stackIO UUID.nextRandom
      arrMasterID <- stackIO $ randomRIO (0, maxBound :: Int32) -- this matches the range CST uses (C# Random.Next method)
      return $ case contents of
        Arr.PartVocals _ -> DLC.ArrVocals DLC.Vocals
          { xml          = takeFileName $ rsArr fpart arrSlot
          , japanese     = False -- TODO
          , customFont   = Nothing
          , masterID     = fromIntegral arrMasterID
          , persistentID = UUID.toText persistentID
          }
        Arr.PartArrangement arr -> DLC.ArrInstrumental DLC.Instrumental
          { xml          = takeFileName $ rsArr fpart arrSlot
          , name         = case arrSlot of
            RSPlayable (RSArrSlot _ RSLead       ) _ -> 0
            RSPlayable (RSArrSlot _ RSRhythm     ) _ -> 2
            RSPlayable (RSArrSlot _ RSComboLead  ) _ -> 1
            RSPlayable (RSArrSlot _ RSComboRhythm) _ -> 1
            RSPlayable (RSArrSlot _ RSBass       ) _ -> 3
            RSVocal    _                             -> 0 -- shouldn't happen
          , routeMask    = case arrSlot of
            RSPlayable (RSArrSlot _ RSLead       ) _ -> 1
            RSPlayable (RSArrSlot _ RSRhythm     ) _ -> 2
            RSPlayable (RSArrSlot _ RSComboLead  ) _ -> 1
            RSPlayable (RSArrSlot _ RSComboRhythm) _ -> 2
            RSPlayable (RSArrSlot _ RSBass       ) _ -> 4
            RSVocal    _                             -> 0 -- shouldn't happen
          , priority     = i
          , scrollSpeed  = 1.3
          , bassPicked   = Arr.ap_bassPick $ Arr.arr_arrangementProperties arr
          , tuning       =
            [ Arr.tuning_string0 $ Arr.arr_tuning arr
            , Arr.tuning_string1 $ Arr.arr_tuning arr
            , Arr.tuning_string2 $ Arr.arr_tuning arr
            , Arr.tuning_string3 $ Arr.arr_tuning arr
            , Arr.tuning_string4 $ Arr.arr_tuning arr
            , Arr.tuning_string5 $ Arr.arr_tuning arr
            ]
          , tuningPitch  = 440 * (2 ** (1 / 12)) ** (fromIntegral (Arr.arr_centOffset arr) / 100)
          -- Builder errors if tone is "", but not if it's a key with no matching tone
          , baseTone     = fromMaybe "no-tone" $ Arr.arr_tonebase arr
          , tones        = catMaybes [Arr.arr_tonea arr, Arr.arr_toneb arr, Arr.arr_tonec arr, Arr.arr_toned arr]
          , masterID     = fromIntegral arrMasterID
          , persistentID = UUID.toText persistentID
          }
    let dlc = DLC.RS2DLC
          { version            = fromMaybe "1.0" rs.rs_Version
          , author             = fromMaybe "" songYaml.metadata.author
          , dlcKey             = key
          , artistName         = DLC.Sortable
            { value     = getArtist songYaml.metadata
            , sortValue = getArtist songYaml.metadata -- TODO
            }
          , japaneseArtistName = songYaml.metadata.artistJP
          , japaneseTitle      = songYaml.metadata.titleJP
          , title              = DLC.Sortable
            { value     = targetTitle songYaml $ RS rs
            , sortValue = targetTitle songYaml $ RS rs -- TODO
            }
          , albumName          = DLC.Sortable
            { value     = fromMaybe "" songYaml.metadata.album
            , sortValue = fromMaybe "" songYaml.metadata.album -- TODO
            }
          , year               = getYear songYaml.metadata
          , albumArtFile       = "cover.png"
          , audioFile          = DLC.AudioFile
            { path   = "audio.wav"
            , volume = case volume of
              Nothing -> -7 -- default in CST
              Just v  -> realToFrac $ (-16) - v -- -16 is the target ODLC uses
            }
          , audioPreviewFile   = DLC.AudioFile
            { path   = "audio_preview.wav"
            , volume = case prevVolume of
              Nothing -> -7
              Just v  -> realToFrac $ (-16) - v
            }
          , ignoredIssues      = []
          , arrangements       = arrangements
          , tones              = map snd allTones
          }
    stackIO $ A.encodeFile out $ toJSON dlc
  rsProject %> \out -> do
    let allTonePaths = nubOrd $ do
          (_, RSPlayable _ pg) <- presentParts
          tones <- toList pg.pgTones
          toList tones
    shk $ need $ [ rsArr fpart arrSlot | (fpart, arrSlot) <- presentParts ] ++ allTonePaths
    allTones <- forM allTonePaths $ \f -> do
      tone <- CST.parseTone f
      return (f, tone)
    parsedArrFiles <- forM presentParts $ \(fpart, arrSlot) -> do
      contents <- Arr.parseFile $ rsArr fpart arrSlot
      return (fpart, arrSlot, contents)
    let averageTempo = listToMaybe $ do
          (_, _, Arr.PartArrangement arr) <- parsedArrFiles
          return $ Arr.arr_averageTempo arr
    arrangements <- forM (zip [0..] parsedArrFiles) $ \(i, (fpart, arrSlot, contents)) -> do
      -- TODO ArrangementSort doesn't appear to work for multiples of the same arrangement label.
      -- Instead it sorts by the arrangement ID UUID!
      -- So, we could just use the first character or so to encode `i`.
      arrID <- stackIO UUID.nextRandom
      songFileID <- stackIO UUID.nextRandom
      songXmlID <- stackIO UUID.nextRandom
      arrMasterID <- stackIO $ randomRIO (0, maxBound :: Int32) -- this matches the range CST uses (C# Random.Next method)
      return $ case contents of
        Arr.PartVocals _ -> CST.Arrangement
          { CST.arr_ArrangementName      = "Vocals"
          , CST.arr_ArrangementPropeties = Nothing
          , CST.arr_ArrangementSort      = i
          , CST.arr_ArrangementType      = "Vocal"
          , CST.arr_BonusArr             = False
          , CST.arr_CapoFret             = 0
          , CST.arr_GlyphsXmlPath        = Nothing
          , CST.arr_Id                   = UUID.toText arrID
          , CST.arr_LyricsArtPath        = Nothing
          , CST.arr_MasterId             = arrMasterID
          , CST.arr_Metronome            = "None"
          , CST.arr_PluckedType          = "NotPicked"
          , CST.arr_Represent            = False
          , CST.arr_RouteMask            = "None"
          , CST.arr_ScrollSpeed          = 13
          , CST.arr_Sng2014              = Nothing
          , CST.arr_SongFile             = CST.AggregateGraph
            { CST.ag_UUID    = UUID.toText songFileID
            , CST.ag_File    = ""
            , CST.ag_Version = Nothing
            }
          , CST.arr_SongXml              = CST.AggregateGraph
            { CST.ag_UUID    = UUID.toText songXmlID
            , CST.ag_File    = T.pack $ takeFileName $ rsArr fpart arrSlot
            , CST.ag_Version = Nothing
            }
          , CST.arr_ToneA                = ""
          , CST.arr_ToneB                = ""
          , CST.arr_ToneBase             = "" -- is this fine?
          , CST.arr_ToneC                = ""
          , CST.arr_ToneD                = ""
          , CST.arr_ToneMultiplayer      = Nothing
          , CST.arr_Tuning               = "E Standard"
          , CST.arr_TuningPitch          = 440
          , CST.arr_TuningStrings        = CST.TuningStrings
            { CST.ts_String0 = 0
            , CST.ts_String1 = 0
            , CST.ts_String2 = 0
            , CST.ts_String3 = 0
            , CST.ts_String4 = 0
            , CST.ts_String5 = 0
            }
          }
        Arr.PartArrangement arr -> CST.Arrangement
          { CST.arr_ArrangementName      = Arr.arr_arrangement arr
          , CST.arr_ArrangementPropeties = Just CST.ArrangementPropeties
            { CST.ap_BarreChords       = Arr.ap_barreChords       $ Arr.arr_arrangementProperties arr
            , CST.ap_BassPick          = Arr.ap_bassPick          $ Arr.arr_arrangementProperties arr
            , CST.ap_Bends             = Arr.ap_bends             $ Arr.arr_arrangementProperties arr
            , CST.ap_DoubleStops       = Arr.ap_doubleStops       $ Arr.arr_arrangementProperties arr
            , CST.ap_DropDPower        = Arr.ap_dropDPower        $ Arr.arr_arrangementProperties arr
            , CST.ap_FifthsAndOctaves  = Arr.ap_fifthsAndOctaves  $ Arr.arr_arrangementProperties arr
            , CST.ap_FingerPicking     = Arr.ap_fingerPicking     $ Arr.arr_arrangementProperties arr
            , CST.ap_FretHandMutes     = Arr.ap_fretHandMutes     $ Arr.arr_arrangementProperties arr
            , CST.ap_Harmonics         = Arr.ap_harmonics         $ Arr.arr_arrangementProperties arr
            , CST.ap_Hopo              = Arr.ap_hopo              $ Arr.arr_arrangementProperties arr
            , CST.ap_NonStandardChords = Arr.ap_nonStandardChords $ Arr.arr_arrangementProperties arr
            , CST.ap_OpenChords        = Arr.ap_openChords        $ Arr.arr_arrangementProperties arr
            , CST.ap_PalmMutes         = Arr.ap_palmMutes         $ Arr.arr_arrangementProperties arr
            , CST.ap_PickDirection     = Arr.ap_pickDirection     $ Arr.arr_arrangementProperties arr
            , CST.ap_PinchHarmonics    = Arr.ap_pinchHarmonics    $ Arr.arr_arrangementProperties arr
            , CST.ap_PowerChords       = Arr.ap_powerChords       $ Arr.arr_arrangementProperties arr
            , CST.ap_Represent         = case arrSlot of
              RSPlayable (RSArrSlot RSDefault _) _ -> True
              _                                    -> False -- vocals, bonus/alt arrangements
            , CST.ap_SlapPop           = Arr.ap_slapPop           $ Arr.arr_arrangementProperties arr
            , CST.ap_Slides            = Arr.ap_slides            $ Arr.arr_arrangementProperties arr
            , CST.ap_StandardTuning    = Arr.ap_standardTuning    $ Arr.arr_arrangementProperties arr
            , CST.ap_Sustain           = Arr.ap_sustain           $ Arr.arr_arrangementProperties arr
            , CST.ap_Syncopation       = Arr.ap_syncopation       $ Arr.arr_arrangementProperties arr
            , CST.ap_Tapping           = Arr.ap_tapping           $ Arr.arr_arrangementProperties arr
            , CST.ap_Tremolo           = Arr.ap_tremolo           $ Arr.arr_arrangementProperties arr
            , CST.ap_TwoFingerPicking  = Arr.ap_twoFingerPicking  $ Arr.arr_arrangementProperties arr
            , CST.ap_UnpitchedSlides   = Arr.ap_unpitchedSlides   $ Arr.arr_arrangementProperties arr
            , CST.ap_Vibrato           = Arr.ap_vibrato           $ Arr.arr_arrangementProperties arr
            , CST.ap_BonusArr          = Arr.ap_bonusArr          $ Arr.arr_arrangementProperties arr
            , CST.ap_PathBass          = Arr.ap_pathBass          $ Arr.arr_arrangementProperties arr
            , CST.ap_PathLead          = Arr.ap_pathLead          $ Arr.arr_arrangementProperties arr
            , CST.ap_PathRhythm        = Arr.ap_pathRhythm        $ Arr.arr_arrangementProperties arr
            , CST.ap_Metronome         = False
            , CST.ap_RouteMask         = case arrSlot of
              RSPlayable (RSArrSlot _ RSLead       ) _ -> 1
              RSPlayable (RSArrSlot _ RSRhythm     ) _ -> 2
              RSPlayable (RSArrSlot _ RSComboLead  ) _ -> 1
              RSPlayable (RSArrSlot _ RSComboRhythm) _ -> 2
              RSPlayable (RSArrSlot _ RSBass       ) _ -> 4
              RSVocal    _                             -> 0 -- shouldn't happen (vocal/showlight have a nil ArrangementPropeties)
            }
          , CST.arr_ArrangementSort      = i
          , CST.arr_ArrangementType      = case arrSlot of
            RSPlayable (RSArrSlot _ RSBass) _ -> "Bass"
            RSPlayable (RSArrSlot _ _     ) _ -> "Guitar"
            RSVocal                         _ -> "Vocal"
            -- last one is "ShowLight"
          , CST.arr_BonusArr             = case arrSlot of
            RSPlayable (RSArrSlot RSBonus     _) _ -> True
            RSPlayable (RSArrSlot RSAlternate _) _ -> False -- alt: both represent and bonus set to false
            _                                      -> False
          , CST.arr_CapoFret             = Arr.arr_capo arr
          , CST.arr_GlyphsXmlPath        = Nothing
          , CST.arr_Id                   = UUID.toText arrID
          , CST.arr_LyricsArtPath        = Nothing
          , CST.arr_MasterId             = arrMasterID
          , CST.arr_Metronome            = "None"
          , CST.arr_PluckedType          = if Arr.ap_bassPick $ Arr.arr_arrangementProperties arr
            then "Picked"
            else "NotPicked"
          , CST.arr_Represent            = case arrSlot of
            RSPlayable (RSArrSlot RSDefault _) _ -> True
            _                                    -> False -- vocals, bonus/alt arrangements
          , CST.arr_RouteMask            = case arrSlot of
            RSPlayable (RSArrSlot _ RSLead       ) _ -> "Lead"
            RSPlayable (RSArrSlot _ RSRhythm     ) _ -> "Rhythm"
            RSPlayable (RSArrSlot _ RSBass       ) _ -> "Bass"
            RSPlayable (RSArrSlot _ RSComboLead  ) _ -> "Lead"
            RSPlayable (RSArrSlot _ RSComboRhythm) _ -> "Rhythm"
            RSVocal _                                -> "None"
            -- showlights are also "None"
          , CST.arr_ScrollSpeed          = 13 -- default?
          , CST.arr_Sng2014              = Nothing
          , CST.arr_SongFile             = CST.AggregateGraph
            { CST.ag_UUID    = UUID.toText songFileID
            , CST.ag_File    = ""
            , CST.ag_Version = Nothing
            }
          , CST.arr_SongXml              = CST.AggregateGraph
            { CST.ag_UUID    = UUID.toText songXmlID
            , CST.ag_File    = T.pack $ takeFileName $ rsArr fpart arrSlot
            , CST.ag_Version = Nothing
            }
          , CST.arr_ToneA                = fromMaybe "" $ Arr.arr_tonea arr
          , CST.arr_ToneB                = fromMaybe "" $ Arr.arr_toneb arr
          , CST.arr_ToneBase             = fromMaybe "" $ Arr.arr_tonebase arr
          , CST.arr_ToneC                = fromMaybe "" $ Arr.arr_tonec arr
          , CST.arr_ToneD                = fromMaybe "" $ Arr.arr_toned arr
          , CST.arr_ToneMultiplayer      = Nothing
          , CST.arr_Tuning               = let
            rs2014Tunings =
              [ ([0, 0, 0, 0, 0, 0], "E Standard")
              , ([-2, 0, 0, 0, 0, 0], "Drop D")
              , ([-2, 0, 0, -1, -2, -2], "Open D")
              , ([0, 0, 2, 2, 2, 0], "Open A")
              , ([-2, -2, 0, 0, 0, -2], "Open G")
              , ([0, 2, 2, 1, 0, 0], "Open E")
              , ([-1, -1, -1, -1, -1, -1], "Eb Standard")
              , ([-3, -1, -1, -1, -1, -1], "Eb Drop Db")
              , ([-2, -2, -2, -2, -2, -2], "D Standard")
              , ([-4, -2, -2, -2, -2, -2], "D Drop C")
              , ([-3, -3, -3, -3, -3, -3], "C# Standard")
              , ([-4, -4, -4, -4, -4, -4], "C Standard")
              ]
            thisTuning =
              [ Arr.tuning_string0 $ Arr.arr_tuning arr
              , Arr.tuning_string1 $ Arr.arr_tuning arr
              , Arr.tuning_string2 $ Arr.arr_tuning arr
              , Arr.tuning_string3 $ Arr.arr_tuning arr
              , Arr.tuning_string4 $ Arr.arr_tuning arr
              , Arr.tuning_string5 $ Arr.arr_tuning arr
              ]
            in fromMaybe "Custom Tuning" $ lookup thisTuning rs2014Tunings
          , CST.arr_TuningPitch          = let
            -- we use round instead of realToFrac to make sure that e.g. -1200 cents becomes 220 Hz and not 219.999
            hertzDouble = 440 * (2 ** (1 / 12)) ** (fromIntegral (Arr.arr_centOffset arr) / 100) :: Double
            in MkFixed $ round $ 1000 * hertzDouble :: Milli
          , CST.arr_TuningStrings        = CST.TuningStrings
            { CST.ts_String0 = Arr.tuning_string0 $ Arr.arr_tuning arr
            , CST.ts_String1 = Arr.tuning_string1 $ Arr.arr_tuning arr
            , CST.ts_String2 = Arr.tuning_string2 $ Arr.arr_tuning arr
            , CST.ts_String3 = Arr.tuning_string3 $ Arr.arr_tuning arr
            , CST.ts_String4 = Arr.tuning_string4 $ Arr.arr_tuning arr
            , CST.ts_String5 = Arr.tuning_string5 $ Arr.arr_tuning arr
            }
          }
    key <- getRSKey
    shk $ need [rsAudio]
    volume <- stackIO $ audioIntegratedVolume rsAudio
    when (isNothing volume) $ warn "Unable to calculate integrated volume of audio file"
    CST.writeProject out CST.DLCPackageData
      { CST.dlc_AlbumArtPath      = "cover.png"
      , CST.dlc_AppId             = 248750 -- Cherub Rock ID
      , CST.dlc_Arrangements      = V.fromList arrangements
      , CST.dlc_ArtFiles          = Nothing
      , CST.dlc_DefaultShowlights = False
      , CST.dlc_GameVersion       = "RS2014"
      , CST.dlc_Inlay             = Nothing
      , CST.dlc_Mac               = False
      , CST.dlc_Name              = key
      , CST.dlc_OggPath           = "audio.wav"
      , CST.dlc_OggPreviewPath    = "audio_preview.wav"
      , CST.dlc_OggQuality        = 5
      , CST.dlc_PS3               = False
      , CST.dlc_Pc                = True
      , CST.dlc_PreviewVolume     = -5 -- default?
      , CST.dlc_SignatureType     = "CON"
      , CST.dlc_SongInfo          = let
        -- not sure why brackets aren't allowed, CST removes them on compile
        textReplace = T.replace "[" "(" . T.replace "]" ")"
        in CST.SongInfo
          { CST.si_Album               = textReplace $ getAlbum songYaml.metadata
          , CST.si_AlbumSort           = textReplace $ getAlbum songYaml.metadata -- TODO
          , CST.si_Artist              = textReplace $ getArtist songYaml.metadata
          , CST.si_ArtistSort          = textReplace $ getArtist songYaml.metadata -- TODO
          , CST.si_AverageTempo        = maybe 120 {- shouldn't happen -} ((round :: U.BPS -> Int) . (* 60)) averageTempo
          , CST.si_JapaneseArtistName  = case songYaml.metadata.artistJP of
            Nothing -> ""
            Just s  -> textReplace s
          , CST.si_JapaneseSongName    = maybe "" textReplace $ targetTitleJP songYaml $ RS rs
          , CST.si_SongDisplayName     = textReplace $ targetTitle songYaml $ RS rs
          , CST.si_SongDisplayNameSort = textReplace $ targetTitle songYaml $ RS rs
          , CST.si_SongYear            = fromMaybe 1960 songYaml.metadata.year -- TODO see if this can be empty
          }
          {-
            Info from CST source on sortable text (might not need to do this though, CST appears to edit them itself):

            ( ) are always stripped
            / is replaced with a space
            - usage is inconsistent (so for consistency remove it)
            , is stripped (in titles)
            ' is not stripped
            . and ? usage are inconsistent (so for consistency leave these)
            Abbreviations/symbols like 'Mr.' and '&' are replaced with words
            Diacritics are replaced with their ASCII approximations if available
          -}
      , CST.dlc_Tones             = mempty
      , CST.dlc_TonesRS2014       = V.fromList $ map snd allTones
      , CST.dlc_ToolkitInfo       = CST.ToolkitInfo
        { CST.tk_PackageAuthor  = Nothing -- TODO can this be filled in to override the author entered in the toolkit?
        , CST.tk_PackageComment = Just $ "(Project generated by Onyx v" <> T.pack (showVersion version) <> ")"
        , CST.tk_PackageRating  = Nothing
        , CST.tk_PackageVersion = Just $ fromMaybe "1.0" rs.rs_Version
        , CST.tk_ToolkitVersion = Nothing
        }
      , CST.dlc_Version           = "" -- TODO this should be e.g. "Toolkit Version 2.9.2.1-5a8cb74e"
      , CST.dlc_Volume            = case volume of
        Nothing -> -7 -- default in CST
        Just v  -> realToFrac $ (-16) - v -- -16 is the target ODLC uses
      , CST.dlc_XBox360           = False
      , CST.dlc_XBox360Licenses   = mempty
      }

data RSArrangementType g v
  = RSPlayable RSArrSlot g
  | RSVocal v
  -- TODO showlight
