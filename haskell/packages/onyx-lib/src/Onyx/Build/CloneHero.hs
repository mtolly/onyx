{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Build.CloneHero (psRules, makeYARGTuning) where

import           Codec.Picture                    (encodeJpegAtQuality)
import           Codec.Picture.Types              (convertImage)
import           Control.Applicative              ((<|>))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe                       (isJust)
import qualified Data.Text                        as T
import           Development.Shake                hiding (phony, (%>))
import           Development.Shake.FilePath
import           Onyx.Audio
import           Onyx.Audio.Render
import           Onyx.Build.Common
import qualified Onyx.Build.RB3CH                 as RB3
import           Onyx.CloneHero.SNG               (makeSNG)
import           Onyx.Difficulty
import           Onyx.FretsOnFire                 (PSTuning (..))
import qualified Onyx.FretsOnFire                 as FoF
import           Onyx.Genre
import qualified Onyx.MIDI.Track.Drums            as Drums
import           Onyx.MIDI.Track.File             (saveMIDIUtf8, shakeMIDI)
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.MIDI.Track.FiveFret
import           Onyx.MIDI.Track.ProGuitar        (GtrBase (..), GtrTuning (..),
                                                   GuitarType (..),
                                                   encodeTuningOffsets, nullPG)
import           Onyx.Mode
import           Onyx.Project                     hiding (Difficulty)
import           Onyx.StackTrace
import           Onyx.Util.Handle                 (Folder (..), fileReadable,
                                                   saveReadables)
import           Onyx.Zip.Load                    (makeZipFile)
import qualified Sound.MIDI.Util                  as U

psRules :: BuildInfo -> FilePath -> TargetPS FilePath -> QueueLog Rules ()
psRules buildInfo dir ps = do

  let songYaml = biSongYaml buildInfo
      rel = biRelative buildInfo
      gen = biGen buildInfo
      metadata = getTargetMetadata songYaml $ PS ps

  (planName, plan) <- case getPlan ps.common.plan songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show ps
    Just pair -> return pair
  let planDir = gen $ "plan" </> T.unpack planName
      pathPSEditedParts = dir </> "edited-parts.txt"
      loadEditedParts :: Staction (DifficultyPS, Maybe VocalCount)
      loadEditedParts = shk $ read <$> readFile' pathPSEditedParts

  (dir </> "ps/notes.mid", pathPSEditedParts) %> \(out, parts) -> do
    input <- shakeMIDI $ planDir </> "raw.mid"
    (_, mixMode, _) <- computeDrumsPart ps.drums plan songYaml
    (output, diffs, vc) <- RB3.processPS
      ps
      songYaml
      (applyTargetMIDI ps.common input)
      mixMode
      (applyTargetLength ps.common input <$> getAudioLength buildInfo planName plan)
    saveMIDIUtf8 out output
    liftIO $ writeFile parts $ show (diffs, vc)

  dir </> "ps/expert+.mid" %> \out -> do
    song <- shakeMIDI $ dir </> "ps/notes.mid"
    saveMIDIUtf8 out song
      { F.tracks = song.tracks
        { F.fixedPartDrums = Drums.expertWith2x song.tracks.fixedPartDrums
        }
      }

  useJPEG <- case metadata.fileAlbumArt of
    Just img | elem (takeExtension img) [".jpg", ".jpeg"] -> do
      dir </> "ps/album.jpg" %> \out -> do
        (imageData, pathJpeg) <- loadSquareArtOrJPEG songYaml
        case pathJpeg of
          Just jpg -> shk $ copyFile' jpg out
          -- we could just use png in this case. but this is fine
          Nothing  -> stackIO $ BL.writeFile out $ encodeJpegAtQuality 85 $ convertImage imageData
      return True
    _ -> return False
  -- TODO support target-specific album art
  dir </> "ps/album.png"   %> shk . copyFile' (gen "cover-full.png")
  bgimg <- forM songYaml.global.fileBackgroundImage $ \f -> do
    let psImage = "background" <> takeExtension f
    dir </> "ps" </> psImage %> shk . copyFile' (rel f)
    return psImage

  let makeSongIni = do
        midEvents <- shakeMIDI $ planDir </> "events.mid"
        song <- shakeMIDI $ dir </> "ps/notes.mid"
        let _ = song :: F.Song (F.FixedFile U.Beats)
        (DifficultyPS{..}, vocalCount) <- loadEditedParts
        let (pstart, pend) = previewBoundsTarget
              metadata
              midEvents
              ps.common
              0 -- padding
            len = F.songLengthMS song
            pd = getPart ps.drums songYaml >>= (.drums)
            dmode = (.mode) <$> pd
            DifficultyRB3{..} = psDifficultyRB3
            allFives =
              [ song.tracks.fixedPartGuitar
              , song.tracks.fixedPartBass
              , song.tracks.fixedPartKeys
              , song.tracks.fixedPartRhythm
              , song.tracks.fixedPartGuitarCoop
              ]
            emptyModeInput = ModeInput
              { tempo = midEvents.tempos
              , events = mempty
              , part = mempty
              }
            isFiveAutochart builder = (builder FiveTypeGuitarExt emptyModeInput).autochart
            isDrumAutochart builder = (builder DrumTargetCH emptyModeInput).autochart
            usesAutochart = or
              [ maybe False isFiveAutochart $ getPart ps.guitar     songYaml >>= anyFiveFret
              , maybe False isFiveAutochart $ getPart ps.bass       songYaml >>= anyFiveFret
              , maybe False isFiveAutochart $ getPart ps.rhythm     songYaml >>= anyFiveFret
              , maybe False isFiveAutochart $ getPart ps.guitarCoop songYaml >>= anyFiveFret
              , maybe False isFiveAutochart $ getPart ps.keys       songYaml >>= anyFiveFret
              , maybe False isDrumAutochart $ getPart ps.drums      songYaml >>= anyDrums
              ]
            tuningGtr = do
              pg <- getPart ps.guitar songYaml >>= (.proGuitar)
              return $ makeYARGTuning pg.tuning
            tuningBass = do
              pb <- getPart ps.bass songYaml >>= (.proGuitar)
              return $ makeYARGTuning pb.tuning
            hasGuitar22 = not $ nullPG song.tracks.fixedPartRealGuitar22
            hasBass22   = not $ nullPG song.tracks.fixedPartRealBass22
        return FoF.Song
          { artist           = metadata.artist
          , name             = Just $ targetTitle songYaml $ PS ps
          , album            = metadata.album
          , charter          = metadata.author
          , year             = T.pack . show <$> metadata.year
          , genre            = Just $ fofGenre $ fullGenre metadata
          , proDrums         = flip fmap dmode $ \case
            DrumsPro   -> True
            DrumsReal  -> True
            DrumsElite -> True
            Drums4     -> False
            Drums5     -> False
          , fiveLaneDrums    = case dmode of
            Just Drums5 -> Just True
            _           -> Nothing
          -- we use to not use five_lane_drums for easier MIDI output.
          -- but now we set it to true for 5-lane output for better compatibility
          -- (Moonscraper, YARG) and a nicer looking MIDI output (RYBOG in order)
          , drumFallbackBlue = pd >>= \case
            ModeDrums{ mode = Drums5, fallback = FallbackBlue } -> Just True
            _                                                   -> Nothing
          , songLength       = Just len
          , previewStartTime = Just pstart
          , previewEndTime   = Just pend
          -- difficulty tiers go from 0 to 6, or -1 for no part
          , diffBand         = Just $ fromIntegral $ rb3BandTier      - 1
          , diffGuitar       = Just $ fromIntegral $ rb3GuitarTier    - 1
          , diffGuitarGHL    = Just $ fromIntegral $ chGuitarGHLTier  - 1
          , diffBass         = Just $ fromIntegral $ rb3BassTier      - 1
          , diffBassGHL      = Just $ fromIntegral $ chBassGHLTier    - 1
          , diffDrums        = Just $ fromIntegral $ rb3DrumsTier     - 1
          , diffDrumsReal    = Just $ case dmode of
            Just DrumsPro   -> fromIntegral $ rb3DrumsTier - 1
            Just DrumsReal  -> fromIntegral $ rb3DrumsTier - 1
            Just DrumsElite -> fromIntegral $ rb3DrumsTier - 1
            _               -> -1
          , diffKeys         = Just $ fromIntegral $ rb3KeysTier      - 1
          , diffKeysReal     = Just $ fromIntegral $ rb3ProKeysTier   - 1
          , diffVocals       = Just $ fromIntegral $ rb3VocalTier     - 1
          , diffVocalsHarm   = Just $ case vocalCount of
            Nothing     -> -1
            Just Vocal1 -> -1
            Just Vocal2 -> fromIntegral $ rb3VocalTier - 1
            Just Vocal3 -> fromIntegral $ rb3VocalTier - 1
          , diffDance        = Just $ fromIntegral $ psDanceTier      - 1
          , diffBassReal     = Just $ fromIntegral $ rb3ProBassTier   - 1
          , diffGuitarReal   = Just $ fromIntegral $ rb3ProGuitarTier - 1
          , diffBassReal22   = guard hasBass22   >> Just (fromIntegral $ rb3ProBassTier   - 1)
          , diffGuitarReal22 = guard hasGuitar22 >> Just (fromIntegral $ rb3ProGuitarTier - 1)
          , diffGuitarCoop   = Just $ fromIntegral $ psGuitarCoopTier - 1
          , diffRhythm       = Just $ fromIntegral $ psRhythmTier     - 1
          , diffDrumsRealPS  = Just (-1)
          , diffKeysRealPS   = Just (-1)
          , delay            = Nothing
          , starPowerNote    = Just 116
          , eighthNoteHOPO   = Nothing
          , hopoFrequency    = Nothing
          , track            = metadata.trackNumber
          , sysexSlider      = Just $ or $ do
            five <- allFives
            fd <- toList five.fiveDifficulties
            return $ not $ RTB.null fd.fiveTap
          , sysexOpenBass    = Just $ or $ do
            five <- allFives
            fd <- toList five.fiveDifficulties
            return $ not $ RTB.null fd.fiveOpen
          , loadingPhrase    = metadata.loadingPhrase <|> do
            guard usesAutochart
            Just "Chart generated by Onyx with logic from Edward's midi-CH auto charter: https://efhiii.github.io/midi-ch/"
          , cassetteColor    = Nothing
          , tags             = guard metadata.cover >> Just "cover"
          , background       = bgimg
           -- TODO fill these in if we have a video
          , video            = Nothing
          , videoStartTime   = Nothing
          , videoEndTime     = Nothing
          , videoLoop        = Nothing
          , realGuitarTuning   = tuningGtr
          , realGuitar22Tuning = guard hasGuitar22 >> tuningGtr
          , realBassTuning     = tuningBass
          , realBass22Tuning   = guard hasBass22 >> tuningBass
          }

  dir </> "ps/song.ini" %> \out -> do
    ini <- makeSongIni
    FoF.saveSong out ini

  let psParts = [ps.drums, ps.guitar, ps.bass, ps.keys, ps.vocal, ps.rhythm, ps.guitarCoop]
      audioExt s = s <> "." <> T.unpack ps.audioFormat
      eitherDiff x y = if x == 0 then y else x
      loadPSMidi :: Staction (F.Song (F.OnyxFile U.Beats), DifficultyPS, DifficultyRB3, U.Seconds)
      loadPSMidi = do
        (diffs, _) <- loadEditedParts
        -- we need to do applyTargetMIDI on the raw midi, not processed,
        -- because otherwise if we are zooming in on a segment, the basic timing
        -- events will be placed according to the whole song and not the segment
        midRaw <- shakeMIDI $ planDir </> "raw.mid"
        let midSegment = applyTargetMIDI ps.common midRaw
        timing <- RB3.basicTiming False midSegment $
          applyTargetLength ps.common midRaw <$> getAudioLength buildInfo planName plan
        let endSecs = U.applyTempoMap midSegment.tempos timing.timingEnd
        return (midRaw, diffs, psDifficultyRB3 diffs, endSecs)
  let setInstLength secs s = stackIO $ runResourceT $ setAudioLengthOrEmpty secs s
  dir </> audioExt "audio/drums"   %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceStereoParts buildInfo psParts ps.common mid 0 planName plan [(ps.drums, rb3DrumsRank)]
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> audioExt "audio/drums_1" %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceKick  buildInfo psParts ps.common mid 0 SpecNoPannedMono planName plan   ps.drums  rb3DrumsRank
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> audioExt "audio/drums_2" %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    (_, mixMode, _) <- computeDrumsPart ps.drums plan songYaml
    s <- (case mixMode of Drums.D4 -> sourceKit; _ -> sourceSnare)
      buildInfo psParts ps.common mid 0 SpecNoPannedMono planName plan ps.drums rb3DrumsRank
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> audioExt "audio/drums_3" %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    (_, _, ghDrumsAudio) <- computeDrumsPart ps.drums plan songYaml
    s <- (if ghDrumsAudio then sourceCymbals else sourceKit)
      buildInfo psParts ps.common mid 0 SpecNoPannedMono planName plan ps.drums rb3DrumsRank
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> audioExt "audio/drums_4" %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceToms buildInfo psParts ps.common mid 0 SpecNoPannedMono planName plan ps.drums rb3DrumsRank
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> audioExt "audio/guitar"  %> \out -> do
    (mid, DifficultyPS{..}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceStereoParts buildInfo psParts ps.common mid 0 planName plan
      [(ps.guitar, eitherDiff rb3GuitarRank chGuitarGHLTier), (ps.guitarCoop, psGuitarCoopTier)]
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> audioExt "audio/keys"    %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceStereoParts buildInfo psParts ps.common mid 0 planName plan
      [(ps.keys, rb3KeysRank)]
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> audioExt "audio/rhythm"  %> \out -> do
    (mid, DifficultyPS{..}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceStereoParts buildInfo psParts ps.common mid 0 planName plan
      [(ps.bass, eitherDiff rb3BassRank chBassGHLTier), (ps.rhythm, psRhythmTier)]
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> audioExt "audio/vocals"  %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceStereoParts buildInfo psParts ps.common mid 0 planName plan
      [(ps.vocal, rb3VocalRank)]
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> audioExt "audio/crowd"   %> \out -> do
    (mid, DifficultyPS{}, DifficultyRB3{}, endSecs) <- loadPSMidi
    s <- sourceCrowd       buildInfo ps.common mid 0 planName plan
    setInstLength endSecs s >>= \s' -> runAudio s' out
  dir </> audioExt "ps/song"    %> \out -> do
    (mid, DifficultyPS{..}, DifficultyRB3{..}, endSecs) <- loadPSMidi
    s <- sourceBacking buildInfo ps.common mid 0 planName plan
      [ (ps.drums     , rb3DrumsTier    )
      , (ps.guitar    , eitherDiff rb3GuitarRank chGuitarGHLTier)
      , (ps.guitarCoop, psGuitarCoopTier)
      , (ps.bass      , eitherDiff rb3BassRank chBassGHLTier)
      , (ps.rhythm    , psRhythmTier    )
      , (ps.keys      , rb3KeysTier     )
      , (ps.vocal     , rb3VocalTier    )
      ]
    runAudio (setAudioLength endSecs s) out

  -- Only copy instrument audio over if it's not silent
  forM_ ["drums", "drums_1", "drums_2", "drums_3", "drums_4", "guitar", "keys", "rhythm", "vocals", "crowd"] $ \inst -> do
    phony (audioExt $ dir </> "ps/try-" <> inst) $ do
      let fin = dir </> "audio" </> audioExt inst
      shk $ need [fin]
      -- above, we clamped to 0 frames if silent
      audioLength fin >>= \case
        Just 0 -> return ()
        _      -> shk $ copyFile' fin $ dir </> "ps" </> audioExt inst

  phony (dir </> "ps") $ do
    (_, mixMode, ghDrumsAudio) <- computeDrumsPart ps.drums plan songYaml
    let needsPartAudio f = maybe False (/= emptyPart) (getPart (f ps) songYaml) && case plan of
          StandardPlan x -> HM.member (f ps) x.parts.getParts
          MoggPlan     _ -> True
    shk $ need $ map (\f -> dir </> "ps" </> f) $ concat
      -- TODO replace (/= emptyPart), should actually check whether the right PS play mode is present
      [ ["song.ini", "notes.mid", audioExt "song", if useJPEG then "album.jpg" else "album.png"]
      {-
      , ["expert+.mid"
        | maybe False ((/= Kicks1x) . (.kicks))
        $ getPart ps.drums songYaml >>= (.drums)
        ]
      -}
      , [audioExt "try-drums"   | maybe False (/= emptyPart) (getPart ps.drums songYaml) && mixMode == Drums.D0 && case plan of
          StandardPlan x -> HM.member ps.drums x.parts.getParts
          MoggPlan     _ -> True
        ]
      , [audioExt "try-drums_1" | maybe False (/= emptyPart) (getPart ps.drums songYaml) && mixMode /= Drums.D0]
      , [audioExt "try-drums_2" | maybe False (/= emptyPart) (getPart ps.drums songYaml) && mixMode /= Drums.D0]
      , [audioExt "try-drums_3" | maybe False (/= emptyPart) (getPart ps.drums songYaml) && mixMode /= Drums.D0 && mixMode /= Drums.D4]
      , [audioExt "try-drums_4" | maybe False (/= emptyPart) (getPart ps.drums songYaml) && ghDrumsAudio       ]
      , [audioExt "try-guitar"  | needsPartAudio (.guitar) || needsPartAudio (.guitarCoop)]
      , [audioExt "try-keys"    | needsPartAudio (.keys  )                                ]
      , [audioExt "try-rhythm"  | needsPartAudio (.bass  ) || needsPartAudio (.rhythm    )]
      , [audioExt "try-vocals"  | needsPartAudio (.vocal )                                ]
      , [audioExt "try-crowd"   | case plan of
          StandardPlan x -> isJust x.crowd
          MoggPlan     x -> not $ null x.crowd
        ]
      , toList bgimg
      ]
  dir </> "ps.zip" %> \out -> do
    let d = dir </> "ps"
    shk $ need [d]
    files <- shk $ getDirectoryContents d
    let folderInZip = validFileNamePiece NameRulePC
          $ getArtist metadata <> " - " <> targetTitle songYaml (PS ps)
        songFolder = Folder
          { folderSubfolders = []
          , folderFiles = [ (T.pack name, d </> name) | name <- files ]
          }
    stackIO $ makeZipFile out Folder
      { folderFiles = []
      , folderSubfolders = [(folderInZip, songFolder)]
      }
  dir </> "ps.sng" %> \out -> do
    let d = dir </> "ps"
    shk $ need [d]
    files <- shk $ getDirectoryContents d
    ini <- makeSongIni
    let filesForSNG = do
          name <- files
          guard $ name /= "song.ini"
          return (T.pack name, fileReadable $ dir </> "ps" </> name)
    sngParts <- stackIO $ makeSNG (FoF.songToIniContents ini) filesForSNG
    stackIO $ saveReadables sngParts out

makeYARGTuning :: GtrTuning -> PSTuning
makeYARGTuning t = PSTuning
  { offsets = let
    gtrOffsets = encodeTuningOffsets t TypeGuitar
    in if elem t.gtrBase [Bass4, Bass5, Bass6]
      then map (+ 12) gtrOffsets
      else gtrOffsets
  , name = t.gtrName
  }
