{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Build.GuitarHero2 (gh2Rules) where

import           Audio
import           Build.Common
import           Config                           hiding (Difficulty)
import           Control.Monad.Codec.Onyx         (makeValue, valueId)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import           Data.Binary.Put                  (runPut)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Conduit.Audio
import           Data.Conduit.Audio.SampleRate
import qualified Data.DTA                         as D
import qualified Data.DTA.Serialize               as D
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor.Identity            (Identity (..))
import           Data.Hashable                    (Hashable, hash)
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe                       (catMaybes, fromMaybe)
import qualified Data.Text                        as T
import           Data.Version                     (showVersion)
import           Development.Shake                hiding (phony, (%>), (&%>))
import           Development.Shake.FilePath
import           GuitarHeroII.Audio               (writeVGS, writeVGSMultiRate)
import           GuitarHeroII.Convert
import qualified GuitarHeroII.Events              as GH2
import           GuitarHeroII.File
import           Image
import           MoggDecrypt
import           Paths_onyxite_customs_lib        (version)
import           RockBand.Codec                   (mapTrack)
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.File              (saveMIDI, shakeMIDI)
import           RockBand.Common
import           RockBand.Milo                    (englishSyllables, gh2Lipsync,
                                                   putVocFile)
import           RockBand.Score                   (gh2Base)
import qualified Sound.MIDI.Util                  as U
import           STFS.Package                     (gh2pkg)

hashGH2 :: (Hashable f) => SongYaml f -> TargetGH2 -> Int
hashGH2 songYaml gh2 = let
  hashed =
    ( gh2
    , _title $ _metadata songYaml
    , _artist $ _metadata songYaml
    )
  in 1000000000 + (hash hashed `mod` 1000000000)

gh2Rules :: BuildInfo -> FilePath -> TargetGH2 -> QueueLog Rules ()
gh2Rules buildInfo dir gh2 = do

  let songYaml = biSongYaml buildInfo
      rel = biRelative buildInfo

  (planName, plan) <- case getPlan (tgt_Plan $ gh2_Common gh2) songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show gh2
    Just pair -> return pair
  let planDir = rel $ "gen/plan" </> T.unpack planName
      defaultID = hashGH2 songYaml gh2
      defaultLBP = defaultID + 1
      defaultLBW = defaultID + 2
      key = fromMaybe (makeShortName defaultID songYaml) $ gh2_Key gh2
      pkg = T.unpack key

  let loadPartAudioCheck = case plan of
        Plan{..}     -> return $ \part -> HM.member part $ getParts _planParts
        MoggPlan{..} -> do
          silentChans <- shk $ read <$> readFile' (planDir </> "silent-channels.txt")
          return $ \part -> case HM.lookup part $ getParts _moggParts of
            Nothing    -> False
            Just chans -> any (`notElem` (silentChans :: [Int])) $ concat $ toList chans

  [dir </> "gh2/notes.mid", dir </> "gh2/coop_max_scores.dta", dir </> "gh2/pad.txt"] &%> \[out, coop, pad] -> do
    input <- shakeMIDI $ planDir </> "processed.mid"
    hasAudio <- loadPartAudioCheck
    audio <- computeGH2Audio songYaml gh2 hasAudio
    (mid, padSeconds) <- midiRB3toGH2 songYaml gh2 audio
      (applyTargetMIDI (gh2_Common gh2) input)
      (getAudioLength buildInfo planName plan)
    saveMIDI out mid
    let p1 = gh2PartGuitar $ RBFile.s_tracks mid
        p2 = case gh2_Coop gh2 of
          GH2Bass   -> gh2PartBass   $ RBFile.s_tracks mid
          GH2Rhythm -> gh2PartRhythm $ RBFile.s_tracks mid
        scores = map (\diff -> gh2Base diff p1 + gh2Base diff p2) [Easy .. Expert]
        dta = "(" <> T.unpack key <> " (" <> unwords (map show scores) <> "))"
    stackIO $ writeFile coop dta
    stackIO $ writeFile pad $ show padSeconds

  let loadGH2Midi = shakeMIDI $ dir </> "gh2/notes.mid" :: Staction (RBFile.Song (GH2File U.Beats))
      correctAudioLength mid = do
        endTime <- case RTB.filter (== GH2.End) $ GH2.eventsOther $ gh2Events $ RBFile.s_tracks mid of
          RNil       -> fatal "panic! couldn't find [end] event in GH2 output midi"
          Wait t _ _ -> return $ U.applyTempoMap (RBFile.s_tempos mid) t
        return $ endTime + 5
        -- previously we went 0.5s past [end], but that still had issues,
        -- particularly in practice mode when playing the last section
      gh2SourceGeneral lowRateSilence withSources = do
        hasAudio <- loadPartAudioCheck
        audio <- computeGH2Audio songYaml gh2 hasAudio
        mid <- loadGH2Midi
        srcs <- forM (gh2AudioSections audio) $ \case
          GH2PartStereo part -> getPartSource buildInfo [(-1, 0), (1, 0)] planName plan part 1
          -- This halves the volume, so we set vols in .dta to compensate
          GH2PartMono part -> applyVolsMono [0] <$> getPartSource buildInfo [(-1, 0), (1, 0)] planName plan part 1
          GH2Band -> sourceSongCountin buildInfo (gh2_Common gh2) mid 0 True planName plan $ concat
            [ [(gh2LeadTrack audio, 1)]
            , [(gh2CoopTrack audio, 1)]
            , maybe [] (\t -> [(t, 1)]) $ gh2DrumTrack audio
            ]
          GH2Silent -> return $ silent (Seconds 0) (if lowRateSilence then 11025 else 44100) 1
        pad <- shk $ read <$> readFile' (dir </> "gh2/pad.txt")
        audioLen <- correctAudioLength mid
        let applyOffset = case compare (gh2_Offset gh2) 0 of
              EQ -> id
              GT -> dropStart $ Seconds          $ gh2_Offset gh2
              LT -> padStart  $ Seconds $ negate $ gh2_Offset gh2
            toEachSource
              = setAudioLength audioLen
              . applyOffset
              . padAudio pad
              . applyTargetAudio (gh2_Common gh2) mid
        return $ fmap toEachSource $ withSources srcs
      -- for vgs, separate sources so silence can be encoded at low sample rate
      gh2SourcesVGS = gh2SourceGeneral True id
      -- for mogg, single source
      gh2Source = fmap runIdentity $ gh2SourceGeneral False $ Identity . foldr1 merge

  dir </> "gh2/audio.vgs" %> \out -> do
    srcs <- gh2SourcesVGS
    stackIO $ runResourceT $ writeVGSMultiRate out $ map (mapSamples integralSample) srcs

  dir </> "gh2/audio_empty.vgs" %> \out -> do
    audioLen <- loadGH2Midi >>= correctAudioLength
    stackIO $ runResourceT $ writeVGS out
      $ silent (Seconds $ realToFrac audioLen) 1024 1
  forM_ ([90, 75, 60] :: [Int]) $ \speed -> do
    dir </> ("gh2/audio_p" ++ show speed ++ ".vgs") %> \out -> do
      hasAudio <- loadPartAudioCheck
      audio <- computeGH2Audio songYaml gh2 hasAudio
      mid <- loadGH2Midi
      (src, dupe) <- case gh2Practice audio of
        [Nothing, Nothing] -> do
          -- just compute it once and duplicate later
          src <- applyVolsMono [0] <$> shk (buildSource $ Input $ planDir </> "everything.wav")
          return (src, True)
        _ -> do
          srcs <- forM (gh2Practice audio) $ \case
            Nothing   -> applyVolsMono [0] <$> shk (buildSource $ Input $ planDir </> "everything.wav")
            Just part -> applyVolsMono [0] <$> getPartSource buildInfo [(-1, 0), (1, 0)] planName plan part 1
          return (foldr1 merge srcs, False)
      pad <- shk $ read <$> readFile' (dir </> "gh2/pad.txt")
      rate <- case speed of
        60 -> return 19875
        75 -> return 16125
        90 -> return 13500
        50 -> return 24000
        65 -> return 18375
        85 -> return 14250
        _  -> fatal $ "No known rate for GH2 practice speed: " ++ show speed ++ "%"
      lg $ "Writing GH2 practice audio for " ++ show speed ++ "% speed"
      stackIO $ runResourceT $ writeVGS out
        $ (if dupe then remapChannels [Just 0, Just 0] else id)
        $ mapSamples integralSample
        $ resampleTo rate SincMediumQuality
        $ stretchFull 1 (100 / fromIntegral speed)
        $ padAudio pad
        $ applyTargetAudio (gh2_Common gh2) mid src
      lg $ "Finished writing GH2 practice audio for " ++ show speed ++ "% speed"

  [dir </> "gh2/songs.dta", dir </> "gh2/songs-dx2.dta", dir </> "gh2/songs-inner.dta", dir </> "gh2/songs-inner-dx2.dta"] &%> \[out, outDX2, outInner, outInnerDX2] -> do
    input <- shakeMIDI $ planDir </> "processed.mid"
    hasAudio <- loadPartAudioCheck
    audio <- computeGH2Audio songYaml gh2 hasAudio
    pad <- shk $ read <$> readFile' (dir </> "gh2/pad.txt")
    let padSeconds = fromIntegral (pad :: Int) :: U.Seconds
        inner isDX2 = D.serialize (valueId D.stackChunks) $ makeGH2DTA
          songYaml
          key
          (previewBounds songYaml (input :: RBFile.Song (RBFile.OnyxFile U.Beats)) padSeconds False)
          gh2
          audio
          (targetTitle songYaml $ GH2 gh2)
          isDX2
        innerStandard = inner False
        innerDX2      = inner True
    stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
      [ D.Parens $ D.Tree 0 $ D.Sym key : D.treeChunks (D.topTree innerStandard) ]
    stackIO $ D.writeFileDTA_latin1 outDX2 $ D.DTA 0 $ D.Tree 0
      [ D.Parens $ D.Tree 0 $ D.Sym key : D.treeChunks (D.topTree innerDX2) ]
    stackIO $ D.writeFileDTA_latin1 outInner innerStandard
    stackIO $ D.writeFileDTA_latin1 outInnerDX2 innerDX2

  dir </> "gh2/lipsync.voc" %> \out -> do
    midi <- shakeMIDI $ planDir </> "raw.mid"
    let vox = RBFile.getFlexPart (gh2_Vocal gh2) $ RBFile.s_tracks midi
        auto = gh2Lipsync englishSyllables . mapTrack (U.applyTempoTrack $ RBFile.s_tempos midi)
    stackIO $ BL.writeFile out $ runPut $ putVocFile
      $ auto $ RBFile.onyxPartVocals vox

  dir </> "gh2/symbol" %> \out -> do
    stackIO $ B.writeFile out $ B8.pack pkg

  -- TODO give this the "distressed photo" look like the other bonus songs
  dir </> "gh2/cover.png_ps2" %> \out -> do
    img <- loadRGB8 songYaml
    stackIO $ BL.writeFile out $ toHMXPS2 img

  phony (dir </> "gh2") $ shk $ need $
    [ dir </> "gh2/notes.mid"
    , dir </> "gh2/audio.vgs"
    , dir </> "gh2/songs.dta"
    , dir </> "gh2/songs-dx2.dta"
    , dir </> "gh2/songs-inner.dta"
    , dir </> "gh2/songs-inner-dx2.dta"
    , dir </> "gh2/lipsync.voc"
    , dir </> "gh2/coop_max_scores.dta"
    , dir </> "gh2/symbol"
    , dir </> "gh2/cover.png_ps2"
    ] <> if gh2_PracticeAudio gh2
      then
        [ dir </> "gh2/audio_p90.vgs"
        , dir </> "gh2/audio_p75.vgs"
        , dir </> "gh2/audio_p60.vgs"
        ]
      else [dir </> "gh2/audio_empty.vgs"]

  dir </> "stfs/config/contexts.dta" %> \out -> do
    let ctx = fromMaybe defaultID $ gh2_Context gh2
    stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
      [ D.Parens $ D.Tree 0 $ catMaybes
        [ Just $ D.Sym key
        -- include author for gh2dx
        , flip fmap (_author $ _metadata songYaml) $ \author -> D.Braces $ D.Tree 0
          [ D.Sym "set"
          , D.Var "author"
          , D.String author
          ]
        , Just $ D.Int $ fromIntegral ctx
        ]
      ]
  dir </> "stfs/config/coop_max_scores.dta" %> \out -> do
    shk $ copyFile' (dir </> "gh2/coop_max_scores.dta") out
  dir </> "stfs/config/leaderboards.dta" %> \out -> do
    let (lbp, lbw) = fromMaybe (defaultLBP, defaultLBW) $ gh2_Leaderboard gh2
    stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
      [ D.Parens $ D.Tree 0
        [ D.Sym key
        , D.Parens $ D.Tree 0
          [ D.Int $ fromIntegral lbp
          , D.Int $ fromIntegral lbw
          ]
        ]
      ]
  dir </> "stfs/config/songs.dta" %> \out -> do
    input <- shakeMIDI $ planDir </> "raw.mid"
    hasAudio <- loadPartAudioCheck
    audio <- computeGH2Audio songYaml gh2 hasAudio
    pad <- shk $ read <$> readFile' (dir </> "gh2/pad.txt")
    let padSeconds = fromIntegral (pad :: Int) :: U.Seconds
        songPackage = makeGH2DTA360
          songYaml
          key
          (previewBounds songYaml (input :: RBFile.Song (RBFile.OnyxFile U.Beats)) padSeconds False)
          gh2
          audio
          (targetTitle songYaml (GH2 gh2))
    stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
      [ D.Parens $ D.Tree 0
        $ D.Sym key
        : makeValue (valueId D.stackChunks) songPackage
      ]
  dir </> "stfs/songs" </> pkg </> pkg <.> "mid" %> \out -> do
    shk $ copyFile' (dir </> "gh2/notes.mid") out
  dir </> "audio.ogg" %> \out -> do
    src <- gh2Source
    runAudio src out
  dir </> "stfs/songs" </> pkg </> pkg <.> "mogg" %> \out -> do
    shk $ need [dir </> "audio.ogg"]
    oggToMogg (dir </> "audio.ogg") out
  dir </> "stfs/songs" </> pkg </> pkg <.> "voc" %> \out -> do
    shk $ copyFile' (dir </> "gh2/lipsync.voc") out
  dir </> "gh2live" %> \out -> do
    shk $ need
      [ dir </> "stfs/config/contexts.dta"
      , dir </> "stfs/config/coop_max_scores.dta"
      , dir </> "stfs/config/leaderboards.dta"
      , dir </> "stfs/config/songs.dta"
      , dir </> "stfs/songs" </> pkg </> pkg <.> "mid"
      , dir </> "stfs/songs" </> pkg </> pkg <.> "mogg"
      , dir </> "stfs/songs" </> pkg </> pkg <.> "voc"
      ]
    lg "# Producing GH2 LIVE file"
    mapStackTraceT (mapQueueLog $ liftIO . runResourceT) $ gh2pkg
      (getArtist (_metadata songYaml) <> " - " <> targetTitle songYaml (GH2 gh2))
      (T.pack $ "Compiled by Onyx Music Game Toolkit version " <> showVersion version)
      (dir </> "stfs")
      out
