{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Build.CloneHero (psRules) where

import qualified Codec.Archive.Zip                as Zip
import           Codec.Picture                    (encodeJpegAtQuality)
import           Codec.Picture.Types              (convertImage)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy             as BL
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe                       (isJust)
import qualified Data.Text                        as T
import           Development.Shake                hiding (phony, (%>), (&%>))
import           Development.Shake.FilePath
import           Onyx.Audio
import           Onyx.Audio.Render
import           Onyx.Build.Common
import qualified Onyx.Build.RB3CH                 as RB3
import           Onyx.Difficulty
import qualified Onyx.FretsOnFire                 as FoF
import           Onyx.Genre
import qualified Onyx.MIDI.Track.Drums            as RBDrums
import           Onyx.MIDI.Track.File             (saveMIDI, shakeMIDI)
import qualified Onyx.MIDI.Track.File             as RBFile
import           Onyx.MIDI.Track.FiveFret
import           Onyx.Project                     hiding (Difficulty)
import           Onyx.StackTrace
import qualified Sound.MIDI.Util                  as U

psRules :: BuildInfo -> FilePath -> TargetPS -> QueueLog Rules ()
psRules buildInfo dir ps = do

  let songYaml = biSongYaml buildInfo
      rel = biRelative buildInfo

  (planName, plan) <- case getPlan (tgt_Plan $ ps_Common ps) songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show ps
    Just pair -> return pair
  let planDir = rel $ "gen/plan" </> T.unpack planName
      pathPSEditedParts = dir </> "edited-parts.txt"
      loadEditedParts :: Staction (DifficultyPS, Maybe VocalCount)
      loadEditedParts = shk $ read <$> readFile' pathPSEditedParts

  [dir </> "ps/notes.mid", pathPSEditedParts] &%> \[out, parts] -> do
    input <- shakeMIDI $ planDir </> "raw.mid"
    (_, mixMode) <- computeDrumsPart (ps_Drums ps) plan songYaml
    (output, diffs, vc) <- RB3.processPS
      ps
      songYaml
      (applyTargetMIDI (ps_Common ps) input)
      mixMode
      (applyTargetLength (ps_Common ps) input <$> getAudioLength buildInfo planName plan)
    saveMIDI out output
    liftIO $ writeFile parts $ show (diffs, vc)

  dir </> "ps/expert+.mid" %> \out -> do
    song <- shakeMIDI $ dir </> "ps/notes.mid"
    saveMIDI out song
      { RBFile.s_tracks = (RBFile.s_tracks song)
        { RBFile.fixedPartDrums = RBDrums.expertWith2x
          $ RBFile.fixedPartDrums $ RBFile.s_tracks song
        }
      }

  useJPEG <- case _fileAlbumArt $ _metadata songYaml of
    Just img | elem (takeExtension img) [".jpg", ".jpeg"] -> do
      dir </> "ps/album.jpg" %> \out -> do
        (imageData, pathJpeg) <- loadSquareArtOrJPEG songYaml
        case pathJpeg of
          Just jpg -> shk $ copyFile' jpg out
          -- we could just use png in this case. but this is fine
          Nothing  -> stackIO $ BL.writeFile out $ encodeJpegAtQuality 85 $ convertImage imageData
      return True
    _ -> return False
  dir </> "ps/album.png"   %> shk . copyFile' (rel "gen/cover-full.png")
  bgimg <- forM (_fileBackgroundImage $ _global songYaml) $ \f -> do
    let psImage = "background" <> takeExtension f
    dir </> "ps" </> psImage %> shk . copyFile' (rel f)
    return psImage

  dir </> "ps/song.ini" %> \out -> do
    raw <- shakeMIDI $ planDir </> "raw.mid"
    song <- shakeMIDI $ dir </> "ps/notes.mid"
    (DifficultyPS{..}, _) <- loadEditedParts
    let (pstart, _) = previewBounds songYaml (raw :: RBFile.Song (RBFile.OnyxFile U.Beats)) 0 False
        len = RBFile.songLengthMS song
        pd = getPart (ps_Drums ps) songYaml >>= partDrums
        dmode = fmap drumsMode pd
        DifficultyRB3{..} = psDifficultyRB3
        allFives =
          [ RBFile.fixedPartGuitar     $ RBFile.s_tracks song
          , RBFile.fixedPartBass       $ RBFile.s_tracks song
          , RBFile.fixedPartKeys       $ RBFile.s_tracks song
          , RBFile.fixedPartRhythm     $ RBFile.s_tracks song
          , RBFile.fixedPartGuitarCoop $ RBFile.s_tracks song
          ]
    FoF.saveSong out FoF.Song
      { FoF.artist           = _artist $ _metadata songYaml
      , FoF.name             = Just $ targetTitle songYaml $ PS ps
      , FoF.album            = _album $ _metadata songYaml
      , FoF.charter          = _author $ _metadata songYaml
      , FoF.year             = _year $ _metadata songYaml
      , FoF.genre            = Just $ fofGenre $ fullGenre songYaml
      , FoF.proDrums         = flip fmap dmode $ \case
        DrumsPro  -> True
        DrumsReal -> True
        DrumsFull -> True
        Drums4    -> False
        Drums5    -> False
      , FoF.fiveLaneDrums    = Nothing
      -- for consistency we will just use the flipped midi layout,
      -- where 100 is green and 101 is orange
      , FoF.drumFallbackBlue = pd >>= \case
        PartDrums{ drumsMode = Drums5, drumsFallback = FallbackBlue } -> Just True
        _                                                             -> Nothing
      , FoF.songLength       = Just len
      , FoF.previewStartTime = Just pstart
      -- difficulty tiers go from 0 to 6, or -1 for no part
      , FoF.diffBand         = Just $ fromIntegral $ rb3BandTier      - 1
      , FoF.diffGuitar       = Just $ fromIntegral $ rb3GuitarTier    - 1
      , FoF.diffGuitarGHL    = Just $ fromIntegral $ chGuitarGHLTier  - 1
      , FoF.diffBass         = Just $ fromIntegral $ rb3BassTier      - 1
      , FoF.diffBassGHL      = Just $ fromIntegral $ chBassGHLTier    - 1
      , FoF.diffDrums        = Just $ fromIntegral $ rb3DrumsTier     - 1
      , FoF.diffDrumsReal    = Just $ case dmode of
        Just DrumsPro  -> fromIntegral $ rb3DrumsTier - 1
        Just DrumsReal -> fromIntegral $ rb3DrumsTier - 1
        Just DrumsFull -> fromIntegral $ rb3DrumsTier - 1
        _              -> -1
      , FoF.diffKeys         = Just $ fromIntegral $ rb3KeysTier      - 1
      , FoF.diffKeysReal     = Just $ fromIntegral $ rb3ProKeysTier   - 1
      , FoF.diffVocals       = Just $ fromIntegral $ rb3VocalTier     - 1
      , FoF.diffVocalsHarm   = Just $ fromIntegral $ rb3VocalTier     - 1
      , FoF.diffDance        = Just $ fromIntegral $ psDanceTier      - 1
      , FoF.diffBassReal     = Just $ fromIntegral $ rb3ProBassTier   - 1
      , FoF.diffGuitarReal   = Just $ fromIntegral $ rb3ProGuitarTier - 1
      -- TODO: are the 22-fret difficulties needed?
      , FoF.diffBassReal22   = Just $ fromIntegral $ rb3ProBassTier   - 1
      , FoF.diffGuitarReal22 = Just $ fromIntegral $ rb3ProGuitarTier - 1
      , FoF.diffGuitarCoop   = Just $ fromIntegral $ psGuitarCoopTier - 1
      , FoF.diffRhythm       = Just $ fromIntegral $ psRhythmTier     - 1
      , FoF.diffDrumsRealPS  = Just (-1)
      , FoF.diffKeysRealPS   = Just (-1)
      , FoF.delay            = Nothing
      , FoF.starPowerNote    = Just 116
      , FoF.eighthNoteHOPO   = Nothing
      , FoF.hopoFrequency    = Nothing
      , FoF.track            = _trackNumber $ _metadata songYaml
      , FoF.sysexSlider      = Just $ or $ do
        five <- allFives
        fd <- toList $ fiveDifficulties five
        return $ not $ RTB.null $ fiveTap fd
      , FoF.sysexOpenBass    = Just $ or $ do
        five <- allFives
        fd <- toList $ fiveDifficulties five
        return $ not $ RTB.null $ fiveOpen fd
      , FoF.loadingPhrase    = ps_LoadingPhrase ps
      , FoF.cassetteColor    = Nothing
      , FoF.tags             = guard (_cover $ _metadata songYaml) >> Just "cover"
      , FoF.background       = bgimg
       -- TODO fill these in if we have a video
      , FoF.video            = Nothing
      , FoF.videoStartTime   = Nothing
      , FoF.videoEndTime     = Nothing
      , FoF.videoLoop        = Nothing
      }

  let psParts = map ($ ps) [ps_Drums, ps_Guitar, ps_Bass, ps_Keys, ps_Vocal, ps_Rhythm, ps_GuitarCoop]
      eitherDiff x y = if x == 0 then y else x
      loadPSMidi :: Staction (RBFile.Song (RBFile.OnyxFile U.Beats), DifficultyPS, DifficultyRB3, U.Seconds)
      loadPSMidi = do
        (diffs, _) <- loadEditedParts
        mid <- shakeMIDI $ planDir </> "processed.mid"
        -- should just retrieve the events already there
        timing <- RB3.basicTiming mid $ getAudioLength buildInfo planName plan
        let endSecs = U.applyTempoMap (RBFile.s_tempos mid) $ RB3.timingEnd timing
        return (mid, diffs, psDifficultyRB3 diffs, endSecs)
  -- TODO for mix mode 4 (kick + kit), we should create only
  --   drums_1 (kick) and drums_2 (kit). currently we create
  --   drums_1 (kick) drums_2 (snare, empty) drums_3 (kit)
  let setInstLength secs s = stackIO $ runResourceT $ setAudioLengthOrEmpty secs s
  dir </> "audio/drums.ogg"   %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceStereoParts buildInfo psParts (ps_Common ps) mid 0 planName plan [(ps_Drums  ps, rb3DrumsRank)]
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> "audio/drums_1.ogg" %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceKick  buildInfo psParts (ps_Common ps) mid 0 False planName plan  (ps_Drums  ps) rb3DrumsRank
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> "audio/drums_2.ogg" %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceSnare buildInfo psParts (ps_Common ps) mid 0 False planName plan  (ps_Drums  ps) rb3DrumsRank
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> "audio/drums_3.ogg" %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceKit   buildInfo psParts (ps_Common ps) mid 0 False planName plan  (ps_Drums  ps) rb3DrumsRank
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> "audio/guitar.ogg"  %> \out -> do
    (mid, DifficultyPS{..}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceStereoParts buildInfo psParts (ps_Common ps) mid 0 planName plan
      [(ps_Guitar ps, eitherDiff rb3GuitarRank chGuitarGHLTier), (ps_GuitarCoop ps, psGuitarCoopTier)]
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> "audio/keys.ogg"    %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceStereoParts buildInfo psParts (ps_Common ps) mid 0 planName plan
      [(ps_Keys ps, rb3KeysRank)]
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> "audio/rhythm.ogg"  %> \out -> do
    (mid, DifficultyPS{..}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceStereoParts buildInfo psParts (ps_Common ps) mid 0 planName plan
      [(ps_Bass ps, eitherDiff rb3BassRank chBassGHLTier), (ps_Rhythm ps, psRhythmTier)]
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> "audio/vocals.ogg"  %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceStereoParts buildInfo psParts (ps_Common ps) mid 0 planName plan
      [(ps_Vocal  ps, rb3VocalRank)]
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> "audio/crowd.ogg"   %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{}, endSecs) <- loadPSMidi
    s <- sourceCrowd       buildInfo (ps_Common ps) mid 0 planName plan
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> "ps/song.ogg"    %> \out -> do
    (mid, DifficultyPS{..}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceSongCountin buildInfo (ps_Common ps) mid 0 True planName plan
      [ (ps_Drums      ps, rb3DrumsTier    )
      , (ps_Guitar     ps, eitherDiff rb3GuitarRank chGuitarGHLTier)
      , (ps_GuitarCoop ps, psGuitarCoopTier)
      , (ps_Bass       ps, eitherDiff rb3BassRank chBassGHLTier)
      , (ps_Rhythm     ps, psRhythmTier    )
      , (ps_Keys       ps, rb3KeysTier     )
      , (ps_Vocal      ps, rb3VocalTier    )
      ]
    runAudio (setAudioLength endSecs s) out

  -- Only copy instrument audio over if it's not silent
  forM_ ["drums", "drums_1", "drums_2", "drums_3", "guitar", "keys", "rhythm", "vocals", "crowd"] $ \inst -> do
    phony (dir </> "ps/try-" <> inst <.> "ogg") $ do
      let fin = dir </> "audio" </> inst <.> "ogg"
      shk $ need [fin]
      -- above, we clamped to 0 frames if silent
      audioLength fin >>= \case
        Just 0 -> return ()
        _      -> shk $ copyFile' fin $ dir </> "ps" </> inst <.> "ogg"

  phony (dir </> "ps") $ do
    (_, mixMode) <- computeDrumsPart (ps_Drums ps) plan songYaml
    let needsPartAudio f = maybe False (/= def) (getPart (f ps) songYaml) && case plan of
          Plan{..} -> HM.member (f ps) $ getParts _planParts
          _        -> True
    shk $ need $ map (\f -> dir </> "ps" </> f) $ concat
      -- TODO replace (/= def), should actually check whether the right PS play mode is present
      [ ["song.ini", "notes.mid", "song.ogg", if useJPEG then "album.jpg" else "album.png"]
      , ["expert+.mid"
        | maybe False ((/= Kicks1x) . drumsKicks)
        $ getPart (ps_Drums ps) songYaml >>= partDrums
        ]
      , ["try-drums.ogg"   | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode == RBDrums.D0 && case plan of
          Plan{..} -> HM.member (ps_Drums ps) $ getParts _planParts
          _        -> True
        ]
      , ["try-drums_1.ogg" | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode /= RBDrums.D0]
      , ["try-drums_2.ogg" | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode /= RBDrums.D0]
      , ["try-drums_3.ogg" | maybe False (/= def) (getPart (ps_Drums ps) songYaml) && mixMode /= RBDrums.D0]
      , ["try-guitar.ogg"  | needsPartAudio ps_Guitar || needsPartAudio ps_GuitarCoop]
      , ["try-keys.ogg"    | needsPartAudio ps_Keys                                  ]
      , ["try-rhythm.ogg"  | needsPartAudio ps_Bass || needsPartAudio ps_Rhythm      ]
      , ["try-vocals.ogg"  | needsPartAudio ps_Vocal                                 ]
      , ["try-crowd.ogg"   | case plan of
          Plan{..}     -> isJust _crowd
          MoggPlan{..} -> not $ null _moggCrowd
        ]
      , toList bgimg
      ]
  dir </> "ps.zip" %> \out -> do
    let d = dir </> "ps"
    shk $ need [d]
    files <- shk $ getDirectoryContents d
    let folderInZip = T.unpack $ validFileNamePiece NameRulePC
          $ getArtist (_metadata songYaml) <> " - " <> targetTitle songYaml (PS ps)
    Zip.createArchive out $ do
      forM_ files $ \file -> do
        sel <- Zip.mkEntrySelector $ folderInZip </> file
        Zip.loadEntry Zip.Deflate sel $ d </> file
