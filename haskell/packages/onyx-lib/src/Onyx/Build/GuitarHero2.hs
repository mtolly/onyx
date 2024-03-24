{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Build.GuitarHero2 (gh2Rules) where

import           Control.Monad.Extra
import           Control.Monad.Trans.Resource
import           Data.Binary.Put                  (runPut)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Conduit.Audio
import           Data.Conduit.Audio.SampleRate
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor.Identity            (Identity (..))
import           Data.Hashable                    (Hashable, hash)
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe                       (catMaybes, fromMaybe)
import qualified Data.Text                        as T
import           Data.Version                     (showVersion)
import           Development.Shake                hiding (phony, (%>))
import           Development.Shake.FilePath
import           Onyx.Audio
import           Onyx.Audio.VGS                   (writeVGS, writeVGSMultiRate)
import           Onyx.Build.Common
import           Onyx.Build.GuitarHero2.Logic
import           Onyx.Codec.Common                (makeValue, valueId)
import           Onyx.Difficulty
import           Onyx.Harmonix.Ark.GH2            (GH2DXExtra (..))
import qualified Onyx.Harmonix.DTA                as D
import qualified Onyx.Harmonix.DTA.Serialize      as D
import qualified Onyx.Harmonix.GH2.Events         as GH2
import           Onyx.Harmonix.GH2.File
import           Onyx.Harmonix.MOGG
import           Onyx.Harmonix.RockBand.Milo      (englishSyllables, gh2Lipsync,
                                                   putVocFile)
import           Onyx.Harmonix.RockBand.Score     (gh2Base)
import           Onyx.Image.DXT
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read                   (mapTrack)
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.Project                     hiding (Difficulty)
import           Onyx.StackTrace
import           Onyx.Xbox.STFS                   (gh2pkg)
import           Paths_onyx_lib                   (version)
import qualified Sound.MIDI.Util                  as U

hashGH2 :: (Hashable f) => SongYaml f -> TargetGH2 f -> Int
hashGH2 songYaml gh2 = let
  hashed =
    ( gh2
    , songYaml.metadata.title
    , songYaml.metadata.artist
    )
  in 1000000000 + (hash hashed `mod` 1000000000)

gh2Rules :: BuildInfo -> FilePath -> TargetGH2 FilePath -> QueueLog Rules ()
gh2Rules buildInfo dir gh2 = do

  let songYaml = biSongYaml buildInfo

  (planName, plan) <- case getPlan gh2.common.plan songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show gh2
    Just pair -> return pair
  let planDir = biGen buildInfo $ "plan" </> T.unpack planName
      defaultID = hashGH2 songYaml gh2
      defaultLBP = defaultID + 1
      defaultLBW = defaultID + 2
      key = fromMaybe (makeShortName defaultID songYaml) gh2.key
      pkg = T.unpack key

  let loadPartAudioCheck = case plan of
        StandardPlan x -> return $ \part -> HM.member part x.parts.getParts
        MoggPlan     x -> do
          silentChans <- shk $ read <$> readFile' (planDir </> "silent-channels.txt")
          return $ \part -> case HM.lookup part x.parts.getParts of
            Nothing    -> False
            Just chans -> any (`notElem` (silentChans :: [Int])) $ concat $ toList chans

  (dir </> "gh2/notes.mid", dir </> "gh2/coop_max_scores.dta", dir </> "gh2/pad.txt") %> \(out, coop, pad) -> do
    input <- F.shakeMIDI $ planDir </> "processed.mid"
    hasAudio <- loadPartAudioCheck
    audio <- computeGH2Audio songYaml gh2 True hasAudio -- "is 360" parameter here doesn't matter
    (mid, padSeconds) <- midiRB3toGH2 songYaml gh2 audio
      (applyTargetMIDI gh2.common input)
      (getAudioLength buildInfo planName plan)
    F.saveMIDILatin1 out mid
    let p1 = gh2PartGuitar $ F.s_tracks mid
        p2 = case gh2.coop of
          GH2Bass   -> gh2PartBass   $ F.s_tracks mid
          GH2Rhythm -> gh2PartRhythm $ F.s_tracks mid
        scores = map (\diff -> gh2Base diff p1 + gh2Base diff p2) [Easy .. Expert]
        dta = "(" <> T.unpack key <> " (" <> unwords (map show scores) <> "))"
    stackIO $ writeFile coop dta
    stackIO $ writeFile pad $ show padSeconds

  let loadGH2Midi = F.shakeMIDI $ dir </> "gh2/notes.mid" :: Staction (F.Song (GH2File U.Beats))
      correctAudioLength mid = do
        endTime <- case RTB.filter (== GH2.End) $ GH2.eventsOther $ gh2Events $ F.s_tracks mid of
          RNil       -> fatal "panic! couldn't find [end] event in GH2 output midi"
          Wait t _ _ -> return $ U.applyTempoMap (F.s_tempos mid) t
        return $ endTime + 5
        -- previously we went 0.5s past [end], but that still had issues,
        -- particularly in practice mode when playing the last section
      gh2SourceGeneral is360 withSources = do
        hasAudio <- loadPartAudioCheck
        audio <- computeGH2Audio songYaml gh2 is360 hasAudio
        mid <- loadGH2Midi
        srcs <- forM audio.audioSections $ \case
          GH2PartStereo part -> getPartSource buildInfo [(-1, 0), (1, 0)] planName plan part 1
          -- This halves the volume, so we set vols in .dta to compensate
          GH2PartMono part -> applyVolsMono [0] <$> getPartSource buildInfo [(-1, 0), (1, 0)] planName plan part 1
          GH2Band -> sourceBacking buildInfo def mid 0 planName plan $ concat
            [ [(audio.leadTrack, 1)]
            , [(audio.coopTrack, 1)]
            , maybe [] (\t -> [(t, 1)]) audio.drumTrack
            ]
          -- for ps2 only we encode the silent vgs channel at a lower rate
          GH2Silent -> return $ silent (Seconds 0) (if is360 then 44100 else 11025) 1
        pad <- shk $ read <$> readFile' (dir </> "gh2/pad.txt")
        audioLen <- correctAudioLength mid
        let applyOffset = case compare gh2.offset 0 of
              EQ -> id
              GT -> dropStart $ Seconds          gh2.offset
              LT -> padStart  $ Seconds $ negate gh2.offset
            toEachSource
              = setAudioLength audioLen
              . applyOffset
              . padAudio pad
              . applyTargetAudio gh2.common mid
        return $ fmap toEachSource $ withSources srcs
      -- for vgs, separate sources so silence can be encoded at low sample rate
      gh2SourcesVGS = gh2SourceGeneral False id
      -- for mogg, single source
      gh2SourceMOGG = fmap runIdentity $ gh2SourceGeneral True $ Identity . foldr1 merge . fmap standardRate

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
      audio <- computeGH2Audio songYaml gh2 False hasAudio
      mid <- loadGH2Midi
      (src, dupe) <- case audio.practice of
        [Nothing, Nothing] -> do
          -- just compute it once and duplicate later
          src <- applyVolsMono [0] <$> shk (buildSource $ Input $ planDir </> "everything.wav")
          return (src, True)
        _ -> do
          srcs <- forM audio.practice $ \case
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
        $ applyTargetAudio gh2.common mid src
      lg $ "Finished writing GH2 practice audio for " ++ show speed ++ "% speed"

  let addExtraIfDX :: F.Song (F.OnyxFile U.Beats) -> GH2Audio -> [D.Chunk T.Text] -> [D.Chunk T.Text]
      addExtraIfDX input audio = let
        difficulty = difficultyPS (def :: TargetPS FilePath)
          { guitar = audio.leadTrack
          , bass   = case gh2.coop of GH2Bass -> audio.coopTrack; GH2Rhythm -> F.FlexExtra "undefined"
          , rhythm = case gh2.coop of GH2Rhythm -> audio.coopTrack; GH2Bass -> F.FlexExtra "undefined"
          , drums  = fromMaybe (F.FlexExtra "undefined") audio.drumTrack
          } songYaml
        translateDiff = \case
          0 -> -1 -- in gh2dx, 0 means "present but unknown difficulty" which we might want to support later
          n -> fromIntegral n
        metadata = getTargetMetadata songYaml $ GH2 gh2
        in if gh2.gh2Deluxe
          then addDXExtra GH2DXExtra
            { songalbum      = metadata.album
            , author         = metadata.author
            , songyear       = T.pack . show <$> metadata.year
            , songgenre      = metadata.genre
            , songorigin     = Nothing
            , songduration   = Just $ fromIntegral $ F.songLengthMS input
            , songguitarrank = Just $ translateDiff $ rb3GuitarTier $ psDifficultyRB3 difficulty
            , songbassrank   = Just $ translateDiff $ rb3BassTier $ psDifficultyRB3 difficulty
            , songrhythmrank = Just $ translateDiff $ psRhythmTier difficulty
            , songdrumrank   = Just $ translateDiff $ rb3DrumsTier $ psDifficultyRB3 difficulty
            , songartist     = Nothing -- not needed
            }
          else id

  (dir </> "gh2/songs.dta", dir </> "gh2/songs-inner.dta") %> \(out, outInner) -> do
    input <- F.shakeMIDI $ planDir </> "processed.mid"
    hasAudio <- loadPartAudioCheck
    audio <- computeGH2Audio songYaml gh2 False hasAudio
    pad <- shk $ read <$> readFile' (dir </> "gh2/pad.txt")
    let padSeconds = fromIntegral (pad :: Int) :: U.Seconds
        inner = addExtraIfDX input audio $ makeValue (valueId D.stackChunks) $ makeGH2DTA
          songYaml
          key
          (previewBounds songYaml.metadata (input :: F.Song (F.OnyxFile U.Beats)) padSeconds False)
          gh2
          audio
          (targetTitle songYaml $ GH2 gh2)
    stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
      [ D.Parens $ D.Tree 0 $ D.Sym key : inner ]
    stackIO $ D.writeFileDTA_latin1 outInner $ D.DTA 0 $ D.Tree 0 inner

  dir </> "gh2/lipsync.voc" %> \out -> do
    midi <- F.shakeMIDI $ planDir </> "raw.mid"
    let vox = F.getFlexPart gh2.vocal $ F.s_tracks midi
        auto = gh2Lipsync englishSyllables . mapTrack (U.applyTempoTrack $ F.s_tempos midi)
    stackIO $ BL.writeFile out $ runPut $ putVocFile
      $ auto $ F.onyxPartVocals vox

  dir </> "gh2/symbol" %> \out -> do
    stackIO $ B.writeFile out $ B8.pack pkg

  -- TODO for the store, give this the "distressed photo" look like the other bonus songs.
  -- but not for GH2DX songlist
  dir </> "gh2/cover.png_ps2" %> \out -> do
    img <- loadRGB8 songYaml
    stackIO $ BL.writeFile out $ toHMXPS2 img

  phony (dir </> "gh2") $ shk $ need $
    [ dir </> "gh2/notes.mid"
    , dir </> "gh2/audio.vgs"
    , dir </> "gh2/songs.dta"
    , dir </> "gh2/songs-inner.dta"
    , dir </> "gh2/lipsync.voc"
    , dir </> "gh2/coop_max_scores.dta"
    , dir </> "gh2/symbol"
    , dir </> "gh2/cover.png_ps2"
    ] <> if gh2.practiceAudio
      then
        [ dir </> "gh2/audio_p90.vgs"
        , dir </> "gh2/audio_p75.vgs"
        , dir </> "gh2/audio_p60.vgs"
        ]
      else [dir </> "gh2/audio_empty.vgs"]

  dir </> "stfs/config/contexts.dta" %> \out -> do
    let ctx = fromMaybe defaultID gh2.context
    stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
      [ D.Parens $ D.Tree 0 $ catMaybes
        [ Just $ D.Sym key
        -- include author for gh2dx
        , flip fmap (guard gh2.gh2Deluxe >> songYaml.metadata.author) $ \author -> D.Braces $ D.Tree 0
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
    let (lbp, lbw) = fromMaybe (defaultLBP, defaultLBW) gh2.leaderboard
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
    input <- F.shakeMIDI $ planDir </> "processed.mid"
    hasAudio <- loadPartAudioCheck
    audio <- computeGH2Audio songYaml gh2 True hasAudio
    pad <- shk $ read <$> readFile' (dir </> "gh2/pad.txt")
    let padSeconds = fromIntegral (pad :: Int) :: U.Seconds
        songPackage = makeGH2DTA360
          songYaml
          key
          (previewBounds songYaml.metadata (input :: F.Song (F.OnyxFile U.Beats)) padSeconds False)
          gh2
          audio
          (targetTitle songYaml (GH2 gh2))
    stackIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0
      [ D.Parens $ D.Tree 0
        $ D.Sym key
        : addExtraIfDX input audio (makeValue (valueId D.stackChunks) songPackage)
      ]
  dir </> "stfs/songs" </> pkg </> pkg <.> "mid" %> \out -> do
    shk $ copyFile' (dir </> "gh2/notes.mid") out
  dir </> "audio.ogg" %> \out -> do
    src <- gh2SourceMOGG
    runAudio src out
  dir </> "stfs/songs" </> pkg </> pkg <.> "mogg" %> \out -> do
    shk $ need [dir </> "audio.ogg"]
    oggToMoggFiles (dir </> "audio.ogg") out
  dir </> "stfs/songs" </> pkg </> pkg <.> "voc" %> \out -> do
    shk $ copyFile' (dir </> "gh2/lipsync.voc") out
  dir </> "stfs/songs" </> pkg </> "gen" </> pkg <.> "bmp_xbox" %> \out -> do
    loadRGB8 songYaml >>= stackIO . BL.writeFile out . toDXT1File PNGXbox
  dir </> "gh2live" %> \out -> do
    shk $ need $
      [ dir </> "stfs/config/contexts.dta"
      , dir </> "stfs/config/coop_max_scores.dta"
      , dir </> "stfs/config/leaderboards.dta"
      , dir </> "stfs/config/songs.dta"
      , dir </> "stfs/songs" </> pkg </> pkg <.> "mid"
      , dir </> "stfs/songs" </> pkg </> pkg <.> "mogg"
      , dir </> "stfs/songs" </> pkg </> pkg <.> "voc"
      ] <> [ dir </> "stfs/songs" </> pkg </> "gen" </> pkg <.> "bmp_xbox" | gh2.gh2Deluxe ]
    lg "# Producing GH2 LIVE file"
    gh2pkg
      (getArtist songYaml.metadata <> " - " <> targetTitle songYaml (GH2 gh2))
      (T.pack $ "Compiled by Onyx Music Game Toolkit version " <> showVersion version)
      (dir </> "stfs")
      out
