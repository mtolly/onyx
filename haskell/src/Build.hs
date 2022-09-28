{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Build (shakeBuildFiles, shakeBuild, targetTitle, validFileName, validFileNamePiece, NameRule(..), hashRB3) where

import           Audio
import           AudioSearch
import           Build.CloneHero
import           Build.Common
import           Build.GuitarHero1
import           Build.GuitarHero2
import           Build.GuitarHero3
import           Build.GuitarHero5
import           Build.PowerGig
import           Build.RockBand
import           Build.Rocksmith
import           Codec.Picture
import qualified Codec.Picture.STBIR              as STBIR
import           Config                           hiding (Difficulty)
import           Control.Monad.Codec.Onyx.JSON    (loadYaml)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base64.Lazy      as B64
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isSpace)
import           Data.Conduit                     (runConduit)
import           Data.Conduit.Audio
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Centi, Milli)
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (intercalate, sort)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Development.Shake                hiding (phony, (%>), (&%>))
import           Development.Shake.FilePath
import qualified DTXMania.DTX                     as DTX
import           Guitars                          (guitarify', openNotes')
import           Image
import qualified MelodysEscape                    as Melody
import           MoggDecrypt
import           OSFiles                          (copyDirRecursive)
import           Overdrive                        (calculateUnisons,
                                                   getOverdrive, printFlexParts)
import           Path                             (parseAbsDir, toFilePath)
import           ProKeysRanges
import           Reaper.Build                     (TuningInfo (..),
                                                   makeReaperShake)
import           RenderAudio
import           Resources                        (webDisplay)
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.File              (saveMIDI, shakeMIDI)
import           RockBand.Codec.Five
import qualified RockBand.Codec.FullDrums         as FD
import           RockBand.Common
import qualified RockBand3                        as RB3
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.Jammit.Base                as J
import qualified Sound.Jammit.Export              as J
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.Environment.Executable    (getExecutablePath)
import           Text.Decode                      (decodeGeneral)
import           Text.Read                        (readMaybe)
import           WebPlayer                        (makeDisplay)

forceRW :: (MonadIO m) => FilePath -> StackTraceT m ()
forceRW f = stackIO $ do
  p <- Dir.getPermissions f
  Dir.setPermissions f $ Dir.setOwnerReadable True $ Dir.setOwnerWritable True p

printOverdrive :: FilePath -> StackTraceT (QueueLog Action) ()
printOverdrive mid = do
  song <- shakeMIDI mid
  let _ = song :: RBFile.Song (RBFile.OnyxFile U.Beats)
  od <- calculateUnisons <$> getOverdrive (RBFile.s_tracks song)
  forM_ (ATB.toPairList $ RTB.toAbsoluteEventList 0 od) $ \(posn, unison) -> do
    let posn' = showPosition (RBFile.s_signatures song) posn
    if all (== 0) [ t | (t, _, _) <- NE.toList unison ]
      then lg $ posn' <> ": " <> printFlexParts [ inst | (_, inst, _) <- NE.toList unison ]
      else lg $ intercalate "\n" $ (posn' <> ":") : do
        (t, inst, _) <- NE.toList unison
        return $ "  (" <> show (realToFrac t :: Milli) <> ") " <> printFlexParts [inst]

------------------------------------------------------------------------------

dtxRules :: BuildInfo -> FilePath -> TargetDTX -> QueueLog Rules ()
dtxRules buildInfo dir dtx = do

  let songYaml = biSongYaml buildInfo
      rel = biRelative buildInfo

  (planName, _plan) <- case getPlan (tgt_Plan $ dtx_Common dtx) songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show dtx
    Just pair -> return pair
  let planDir = rel $ "gen/plan" </> T.unpack planName

  let dtxPartDrums  = case getPart (dtx_Drums dtx) songYaml >>= partDrums of
        Just pd -> Just (dtx_Drums  dtx, pd)
        Nothing -> Nothing
      dtxPartGuitar = case getPart (dtx_Guitar dtx) songYaml >>= partGRYBO of
        Just pg -> Just (dtx_Guitar dtx, pg)
        Nothing -> Nothing
      dtxPartBass   = case getPart (dtx_Bass dtx) songYaml >>= partGRYBO of
        Just pg -> Just (dtx_Bass dtx, pg)
        Nothing -> Nothing

  dir </> "dtx/empty.wav" %> \out -> do
    buildAudio (Silence 1 $ Seconds 0) out

  dir </> "dtx/bgm.ogg" %> \out -> do
    let wav = planDir </> "everything.wav"
    buildAudio (Input wav) out

  dir </> "dtx/preview.ogg" %> \out -> do
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

  artPath <- case _fileAlbumArt $ _metadata songYaml of
    Just img | elem (takeExtension img) [".jpg", ".jpeg"] -> do
      dir </> "dtx/cover.jpg" %> shk . copyFile' img
      return "cover.jpg"
    _ -> return "cover.png"
  dir </> "dtx/cover.png" %> shk . copyFile' (rel "gen/cover-full.png")

  mapping <- forM (dtxPartDrums >>= \(_, pd) -> drumsFileDTXKit pd) $ \f -> do
    bs <- liftIO $ B.readFile f
    case readMaybe $ T.unpack $ decodeGeneral bs of
      Nothing -> fail $ "Couldn't parse mapping of full drums to DTX template from: " <> f
      Just m  -> return (f, m)
  template <- forM mapping $ \(f, DTX.DTXMapping templateRel _) -> liftIO $ do
    let templateFixed = takeDirectory f </> templateRel
    templateDTX <- DTX.readDTXLines DTX.FormatDTX <$> DTX.loadDTXLines templateFixed
    return (templateFixed, templateDTX)

  dir </> "dtx/mstr.dtx" %> \out -> do
    mid <- shakeMIDI $ planDir </> "processed.mid"
    let bgmChip   = "0X" -- TODO read this from BGMWAV in mapping
        emptyChip = "ZZ"
        makeGuitarBass = \case
          Nothing          -> RTB.empty
          Just (part, _pg) -> let
            notes
              = maybe RTB.empty (guitarify' . openNotes')
              $ Map.lookup Expert
              $ fiveDifficulties
              $ maybe mempty (fst . RBFile.selectGuitarTrack RBFile.FiveTypeGuitarExt)
              $ Map.lookup part
              $ RBFile.onyxParts
              $ RBFile.s_tracks mid
            toDTXNotes = fmap $ \(mcolors, _len) -> (sort $ catMaybes mcolors, emptyChip)
            in toDTXNotes notes
    liftIO $ B.writeFile out $ TE.encodeUtf16LE $ T.cons '\xFEFF' $ DTX.makeDTX DTX.DTX
      { DTX.dtx_TITLE         = Just $ targetTitle songYaml $ DTX dtx
      , DTX.dtx_ARTIST        = Just $ getArtist $ _metadata songYaml
      , DTX.dtx_PREIMAGE      = Just artPath
      , DTX.dtx_COMMENT       = Nothing
      , DTX.dtx_GENRE         = _genre $ _metadata songYaml
      , DTX.dtx_PREVIEW       = Just "preview.ogg"
      , DTX.dtx_STAGEFILE     = Nothing
      , DTX.dtx_DLEVEL        = Nothing
      , DTX.dtx_GLEVEL        = Nothing
      , DTX.dtx_BLEVEL        = Nothing
      , DTX.dtx_DLVDEC        = Nothing
      , DTX.dtx_GLVDEC        = Nothing
      , DTX.dtx_BLVDEC        = Nothing
      , DTX.dtx_WAV           = let
        initWAV = HM.fromList
          [ (emptyChip, "empty.wav")
          , (bgmChip  , "bgm.ogg"  )
          ]
        -- TODO maybe make sure all template WAV paths are local
        in maybe initWAV (HM.union initWAV . DTX.dtx_WAV . snd) template
      , DTX.dtx_VOLUME        = maybe HM.empty (DTX.dtx_VOLUME . snd) template
      , DTX.dtx_PAN           = maybe HM.empty (DTX.dtx_PAN    . snd) template
      , DTX.dtx_AVI           = HM.empty
      , DTX.dtx_MeasureMap    = RBFile.s_signatures mid
      , DTX.dtx_TempoMap      = RBFile.s_tempos mid
      , DTX.dtx_Drums         = case dtxPartDrums of
        Nothing          -> RTB.empty
        Just (part, _pd) -> let
          -- TODO split flams
          -- TODO figure out what to do for Left Bass
          fullNotes
            = FD.getDifficulty Nothing
            $ maybe mempty RBFile.onyxPartFullDrums
            $ Map.lookup part
            $ RBFile.onyxParts
            $ RBFile.s_tracks mid
          toDTXNotes = fmap $ \fdn -> let
            lane = case FD.fdn_gem fdn of
              FD.Kick      -> DTX.BassDrum
              FD.Snare     -> DTX.Snare
              FD.Hihat     -> case FD.fdn_type fdn of
                FD.GemHihatOpen -> DTX.HihatOpen
                _               -> DTX.HihatClose
              FD.HihatFoot -> DTX.LeftPedal
              FD.CrashL    -> DTX.LeftCymbal
              FD.Tom1      -> DTX.HighTom
              FD.Tom2      -> DTX.LowTom
              FD.Tom3      -> DTX.FloorTom
              FD.CrashR    -> DTX.Cymbal
              FD.Ride      -> DTX.RideCymbal
            chip = fromMaybe emptyChip $ mapping >>= \(_, m) -> DTX.lookupDTXMapping m fdn
            in (lane, chip)
          in toDTXNotes fullNotes
      , DTX.dtx_DrumsDummy    = RTB.empty
      , DTX.dtx_Guitar        = makeGuitarBass dtxPartGuitar
      , DTX.dtx_GuitarWailing = RTB.empty
      , DTX.dtx_Bass          = makeGuitarBass dtxPartBass
      , DTX.dtx_BassWailing   = RTB.empty
      , DTX.dtx_BGM           = RTB.singleton 0 bgmChip
      , DTX.dtx_BGMExtra      = HM.empty
      , DTX.dtx_Video         = RTB.empty
      }

  phony (dir </> "dtx") $ do
    shk $ need
      [ dir </> "dtx/empty.wav"
      , dir </> "dtx/bgm.ogg"
      , dir </> "dtx/preview.ogg"
      , dir </> "dtx" </> artPath
      , dir </> "dtx/mstr.dtx"
      ]
    forM_ template $ \(templatePath, templateDTX) -> do
      forM_ (HM.toList $ DTX.dtx_WAV templateDTX) $ \(_, path) -> do
        -- again, maybe make sure path is local
        when (path /= "bgm.ogg") $ do
          shk $ copyFile'
            (takeDirectory templatePath </> path)
            (rel $ dir </> "dtx" </> path)

------------------------------------------------------------------------------

melodyRules :: BuildInfo -> FilePath -> TargetPart -> QueueLog Rules ()
melodyRules buildInfo dir tgt = do

  let songYaml = biSongYaml buildInfo
      rel = biRelative buildInfo

  (planName, _) <- case getPlan (tgt_Plan $ tgt_Common tgt) songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show tgt
    Just pair -> return pair
  let planDir = rel $ "gen/plan" </> T.unpack planName
      midraw = planDir </> "raw.mid"

  -- Melody's Escape customs
  let melodyAudio = dir </> "melody/audio.ogg"
      melodyChart = dir </> "melody/song.track"
  -- TODO support audio speed
  melodyAudio %> shk . copyFile' (planDir </> "everything.ogg")
  melodyChart %> \out -> do
    shk $ need [midraw, melodyAudio]
    mid <- shakeMIDI midraw
    melody <- liftIO
      $ Melody.randomNotes
      $ maybe mempty RBFile.onyxMelody
      $ Map.lookup (tgt_Part tgt)
      $ RBFile.onyxParts
      $ RBFile.s_tracks mid
    info <- liftIO $ Snd.getFileInfo melodyAudio
    let secs = realToFrac (Snd.frames info) / realToFrac (Snd.samplerate info) :: U.Seconds
        str = unlines
          [ "1.02"
          , intercalate ";"
            [ show (Melody.secondsToTicks secs)
            , show (realToFrac secs :: Centi)
            , "420"
            ]
            , "4"
          , Melody.writeTransitions (RBFile.s_tempos mid) melody
          , Melody.writeNotes (RBFile.s_tempos mid) melody
          ]
    liftIO $ writeFile out str
  phony (dir </> "melody") $ shk $ need [melodyAudio, melodyChart]

------------------------------------------------------------------------------

shakeBuildFiles :: (MonadIO m) => [FilePath] -> FilePath -> [FilePath] -> StackTraceT (QueueLog m) ()
shakeBuildFiles audioDirs yamlPath = shakeBuild audioDirs yamlPath []

shakeBuild :: (MonadIO m) => [FilePath] -> FilePath -> [(T.Text, Target FilePath)] -> [FilePath] -> StackTraceT (QueueLog m) ()
shakeBuild audioDirs yamlPathRel extraTargets buildables = do

  yamlPath <- stackIO $ Dir.canonicalizePath yamlPathRel
  songYaml <- loadYaml yamlPath

  checkDefined songYaml

  exeTime <- stackIO $ getExecutablePath >>= Dir.getModificationTime
  yamlTime <- stackIO $ Dir.getModificationTime yamlPath
  let projVersion = show exeTime ++ "," ++ show yamlTime

  audioLib <- newAudioLibrary
  forM_ audioDirs $ \dir -> do
    p <- parseAbsDir dir
    addAudioDir audioLib p

  writeMsg <- lift $ QueueLog ask
  let yamlDir = takeDirectory yamlPath
      rel f = yamlDir </> f
      ourShakeOptions = shakeOptions
        { shakeThreads = 0
        , shakeFiles = rel "gen"
        , shakeVersion = projVersion
        , shakeOutput = \verb str -> if verb <= Warn
          then writeMsg (MessageWarning, Message str [])
          else if verb <= Info
            then writeMsg (MessageLog, Message str [])
            else return ()
        }
      audioDependPath name = rel $ "gen/audio" </> T.unpack name <.> "wav"
      buildInfo = BuildInfo
        { biSongYaml = songYaml
        , biYamlDir  = yamlDir
        , biRelative = rel
        , biAudioLib = audioLib
        , biAudioDependPath = audioDependPath
        , biOggWavForPlan = \planName -> rel $ "gen/plan" </> T.unpack planName </> "audio.wav"
        }

  do

    shakeEmbed ourShakeOptions $ do

      phony "yaml"  $ lg $ show songYaml
      phony "audio" $ lg $ show audioDirs

      -- Find and convert all Jammit audio into the work directory
      let jammitAudioParts = map J.Only    [minBound .. maxBound]
                          ++ map J.Without [minBound .. maxBound]
      forM_ (HM.toList $ _jammit songYaml) $ \(jammitName, jammitQuery) ->
        forM_ jammitAudioParts $ \audpart ->
          rel (jammitPath jammitName audpart) %> \out -> do
            inside ("Looking for the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart) $ do
              let title  = fromMaybe (getTitle  $ _metadata songYaml) $ _jammitTitle  jammitQuery
                  artist = fromMaybe (getArtist $ _metadata songYaml) $ _jammitArtist jammitQuery
                  inst   = fromJammitInstrument $ J.audioPartToInstrument audpart
              p <- searchJammit audioLib (title, artist, inst)
              result <- stackIO $ fmap J.getAudioParts $ J.loadLibrary $ toFilePath p
              case [ jcfx | (audpart', jcfx) <- result, audpart == audpart' ] of
                jcfx : _ -> do
                  lg $ "Found the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart
                  stackIO $ J.runAudio [jcfx] [] out
                []       -> fail "Couldn't find a necessary Jammit track"

      -- Cover art
      rel "gen/cover.bmp"      %> \out -> loadRGB8 songYaml >>= stackIO . writeBitmap  out . STBIR.resize STBIR.defaultOptions 256 256
      rel "gen/cover.png"      %> \out -> loadRGB8 songYaml >>= stackIO . writePng     out . STBIR.resize STBIR.defaultOptions 256 256
      rel "gen/cover-full.png" %> \out -> loadRGB8 songYaml >>= stackIO . writePng     out
      let hmxImageTypes =
            [ (".png_xbox", PNGXbox)
            , (".png_ps3" , PNGPS3 )
            , (".png_wii" , PNGWii )
            ]
      forM_ hmxImageTypes $ \(ext, pngType) -> do
        rel ("gen/cover" <> ext) %> \out -> case _fileAlbumArt $ _metadata songYaml of
          Just f | takeExtension f == ext -> do
            shk $ copyFile' f out
            forceRW out
          _      -> loadRGB8 songYaml >>= stackIO . BL.writeFile out . toDXT1File pngType

      rel "gen/notes.mid" %> \out -> shk $ do
        let f = rel $ _fileMidi $ _global songYaml
        doesFileExist f >>= \b -> if b
          then copyFile' f out
          else saveMIDI out RBFile.Song
            { RBFile.s_tempos = U.tempoMapFromBPS RTB.empty
            , RBFile.s_signatures = U.measureMapFromTimeSigs U.Error RTB.empty
            , RBFile.s_tracks = mempty :: RBFile.OnyxFile U.Beats
            }

      forM_ (HM.toList $ _audio songYaml) $ \(name, _) -> do
        audioDependPath name %> \out -> do
          src <- manualLeaf yamlDir audioLib (audioDepend buildInfo) songYaml $ Named name
          buildAudio src out

      forM_ (extraTargets ++ HM.toList (_targets songYaml)) $ \(targetName, target) -> do
        let dir = rel $ "gen/target" </> T.unpack targetName
        case target of
          RB3 rb3 -> rbRules buildInfo dir rb3 Nothing
          RB2 rb2 -> let
            rb3 = TargetRB3
              { rb3_Common = rb2_Common rb2
              , rb3_2xBassPedal = rb2_2xBassPedal rb2
              , rb3_SongID = rb2_SongID rb2
              , rb3_Version = rb2_Version rb2
              , rb3_Guitar = rb2_Guitar rb2
              , rb3_Bass = rb2_Bass rb2
              , rb3_Drums = rb2_Drums rb2
              , rb3_Vocal = rb2_Vocal rb2
              , rb3_Keys = RBFile.FlexExtra "undefined"
              , rb3_Harmonix = False
              , rb3_Magma = rb2_Magma rb2
              , rb3_PS3Encrypt = rb2_PS3Encrypt rb2
              }
            in rbRules buildInfo dir rb3 $ Just rb2
          GH1 gh1 -> gh1Rules buildInfo dir gh1
          GH2 gh2 -> gh2Rules buildInfo dir gh2
          PS ps -> psRules buildInfo dir ps
          Melody tgt -> melodyRules buildInfo dir tgt
          RS rs -> rsRules buildInfo dir rs
          GH5 gh5 -> gh5Rules buildInfo dir gh5
          GH3 gh3 -> gh3Rules buildInfo dir gh3
          DTX dtx -> dtxRules buildInfo dir dtx
          PG pg -> pgRules buildInfo dir pg
          Konga _ -> return () -- TODO

      forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do

        let dir = rel $ "gen/plan" </> T.unpack planName

        -- plan audio, currently only used for REAPER project
        let allPlanParts :: [(RBFile.FlexPartName, PartAudio ())]
            allPlanParts = case plan of
              Plan{..}     -> HM.toList $ getParts $ void <$> _planParts
              MoggPlan{..} -> do
                (fpart, pa) <- HM.toList $ getParts _moggParts
                guard $ not $ null $ concat $ toList pa
                return (fpart, void pa)
            dummyMIDI :: RBFile.Song (RBFile.OnyxFile U.Beats)
            dummyMIDI = RBFile.Song
              { RBFile.s_tempos = U.tempoMapFromBPS RTB.empty
              , RBFile.s_signatures = U.measureMapFromTimeSigs U.Error RTB.empty
              , RBFile.s_tracks = mempty :: RBFile.OnyxFile U.Beats
              }
        dir </> "song.wav" %> \out -> do
          s <- sourceSongCountin buildInfo def dummyMIDI 0 False planName plan [ (fpart, 1) | (fpart, _) <- allPlanParts ]
          runAudio (clampIfSilent s) out
        dir </> "crowd.wav" %> \out -> do
          s <- sourceCrowd buildInfo def dummyMIDI 0 planName plan
          runAudio (clampIfSilent s) out
        forM_ allPlanParts $ \(fpart, pa) -> do
          let name = T.unpack $ RBFile.getPartName fpart
          case pa of
            PartSingle () -> do
              dir </> name <.> "wav" %> \out -> do
                s <- sourceSimplePart buildInfo [fpart] def dummyMIDI 0 False planName plan fpart 1
                runAudio (clampIfSilent s) out
            PartDrumKit mkick msnare () -> do
              forM_ mkick $ \() -> do
                dir </> (name ++ "-kick") <.> "wav" %> \out -> do
                  s <- sourceKick buildInfo [fpart] def dummyMIDI 0 False planName plan fpart 1
                  runAudio (clampIfSilent s) out
              forM_ msnare $ \() -> do
                dir </> (name ++ "-snare") <.> "wav" %> \out -> do
                  s <- sourceSnare buildInfo [fpart] def dummyMIDI 0 False planName plan fpart 1
                  runAudio (clampIfSilent s) out
              dir </> (name ++ "-kit") <.> "wav" %> \out -> do
                s <- sourceKit buildInfo [fpart] def dummyMIDI 0 False planName plan fpart 1
                runAudio (clampIfSilent s) out
        let allPlanAudio :: [FilePath]
            allPlanAudio = map (dir </>) $ concat
              [ [ "song.wav" ]
              , sort $ allPlanParts >>= \(fpart, pa) -> let
                name = T.unpack $ RBFile.getPartName fpart
                in case pa of
                  PartSingle () -> [name <.> "wav"]
                  PartDrumKit mkick msnare () -> concat
                    [ map (\() -> (name ++ "-kick") <.> "wav") $ toList mkick
                    , map (\() -> (name ++ "-snare") <.> "wav") $ toList msnare
                    , [(name ++ "-kit") <.> "wav"]
                    ]
              , [ "crowd.wav"
                | case plan of Plan{..} -> isJust _crowd; MoggPlan{..} -> not $ null _moggCrowd
                ]
              ]

        -- REAPER project
        rel ("notes-" ++ T.unpack planName ++ ".RPP") %> \out -> do
          let tempo = rel $ fromMaybe "gen/notes.mid" $ _fileTempo plan
              tunings = TuningInfo
                { tuningGuitars = do
                  (fpart, part) <- HM.toList $ getParts $ _parts songYaml
                  pg <- toList $ partProGuitar part
                  return (fpart, pgTuning pg)
                , tuningCents = _tuningCents plan
                }
          makeReaperShake tunings (rel "gen/notes.mid") tempo allPlanAudio out

        dir </> "web/song.js" %> \out -> do
          let json = dir </> "display.json"
          s <- shk $ readFile' json
          let s' = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s
              js = "window.onyxSong = " ++ s' ++ ";\n"
          liftIO $ writeFile out js
        phony (dir </> "web") $ do
          stackIO $ Dir.createDirectoryIfMissing True $ dir </> "web"
          stackIO webDisplay >>= (`copyDirRecursive` (dir </> "web"))
          shk $ need
            [ dir </> "web/audio-mp3.js"
            , dir </> "web/audio-ogg.js"
            , dir </> "web/song.js"
            ]
        forM_ ["mp3", "ogg"] $ \ext -> do
          dir </> ("web/audio-" <> ext) <.> "js" %> \out -> do
            let audio = dir </> "preview-audio" <.> ext
            shk $ need [audio]
            bs <- stackIO $ BL.readFile audio
            stackIO $ BL.writeFile out $ "window.audioBin = \"" <> B64.encode bs <> "\";\n"

        dir </> "everything.wav" %> \out -> case plan of
          MoggPlan{..} -> do
            src <- shk $ buildSource $ Input $ dir </> "audio.ogg"
            let vols = zipWith f [0..] _vols
                f i vol = if elem i _moggCrowd then -99 else vol
            runAudio (applyPansVols (map realToFrac _pans) (map realToFrac vols) src) out
          Plan{..} -> do
            let planAudios = concat
                  [ toList _song
                  , toList _planParts >>= toList
                  ]
            srcs <- mapM (buildAudioToSpec yamlDir audioLib (audioDepend buildInfo) songYaml [(-1, 0), (1, 0)] . Just) planAudios
            count <- shk $ buildSource $ Input $ dir </> "countin.wav"
            runAudio (foldr mix count srcs) out
        dir </> "everything.ogg" %> buildAudio (Input $ dir </> "everything.wav")

        -- MIDI files

        let midprocessed = dir </> "processed.mid"
            midraw = dir </> "raw.mid"
            display = dir </> "display.json"
        midraw %> \out -> do
          lg "Loading the MIDI file..."
          input <- shakeMIDI $ rel "gen/notes.mid"
          let _ = input :: RBFile.Song (RBFile.RawFile U.Beats)
          tempos <- fmap RBFile.s_tempos $ case _fileTempo plan of
            Nothing -> return input
            Just m  -> shakeMIDI m
          saveMIDI out input { RBFile.s_tempos = tempos }
        midprocessed %> \out -> do
          -- basically just autogen a BEAT track
          input <- shakeMIDI midraw
          output <- RB3.processTiming input $ getAudioLength buildInfo planName plan
          saveMIDI out output

        display %> \out -> do
          song <- shakeMIDI midprocessed
          liftIO $ BL.writeFile out $ makeDisplay songYaml song

        -- count-in audio
        dir </> "countin.wav" %> \out -> do
          let hits = case plan of MoggPlan{} -> []; Plan{..} -> case _countin of Countin h -> h
          src <- buildAudioToSpec yamlDir audioLib (audioDepend buildInfo) songYaml [(-1, 0), (1, 0)] =<< case NE.nonEmpty hits of
            Nothing    -> return Nothing
            Just hits' -> Just . (\expr -> PlanAudio expr [] []) <$> do
              mid <- shakeMIDI $ dir </> "raw.mid"
              let _ = mid :: RBFile.Song (RBFile.RawFile U.Beats)
              return $ Mix $ flip fmap hits' $ \(posn, aud) -> let
                time = Seconds $ realToFrac $ case posn of
                  Left  mb   -> U.applyTempoMap (RBFile.s_tempos mid) $ U.unapplyMeasureMap (RBFile.s_signatures mid) mb
                  Right secs -> secs
                in Pad Start time aud
          runAudio src out

        -- Getting MOGG/OGG from MoggPlan
        let ogg  = dir </> "audio.ogg"
            wav  = dir </> "audio.wav"
            mogg = dir </> "audio.mogg"
        case plan of
          Plan{} -> return ()
          MoggPlan{..} -> do
            ogg %> \out -> do
              shk $ need [mogg]
              if _decryptSilent
                then errorToWarning (moggToOgg mogg out) >>= \case
                  Just () -> return ()
                  Nothing -> do
                    -- Make a no-samples ogg with the right channel count
                    buildAudio (Silence (length _pans) $ Frames 0) out
                else moggToOgg mogg out
            wav %> buildAudio (Input ogg)
            mogg %> \out -> do
              p <- inside "Searching for MOGG file" $ case (_fileMOGG, _moggMD5) of
                (Nothing, Nothing ) -> fatal "No file path or MD5 hash specified for MOGG file"
                (Just f , _       ) -> return f -- TODO maybe check md5 if it is specified
                (Nothing, Just md5) -> toFilePath <$> searchMOGG audioLib md5
              lg $ "Found the MOGG file: " <> p
              -- TODO: check if it's actually an OGG (starts with OggS)
              shk $ copyFile' p out
              forceRW out
            (dir </> "silent-channels.txt") %> \out -> do
              src <- lift $ lift $ buildSource $ Input ogg
              chans <- stackIO $ runResourceT $ runConduit $ emptyChannels src
              stackIO $ writeFile out $ show chans

        -- Audio files for the online preview app
        dir </> "preview-audio.mp3" %> \out -> do
          src <- lift $ lift $ buildSource $ Input $ dir </> "everything.wav"
          stackIO $ runResourceT $ decentMP3 out src
        dir </> "preview-audio.ogg" %> \out -> do
          src <- lift $ lift $ buildSource $ Input $ dir </> "everything.wav"
          stackIO $ runResourceT $ decentVorbis out src

        -- Warn about notes that might hang off before a pro keys range shift
        phony (dir </> "hanging") $ do
          song <- shakeMIDI midprocessed
          lg $ T.unpack $ closeShiftsFile song

        -- Print out a summary of (non-vocal) overdrive and unison phrases
        phony (dir </> "overdrive") $ printOverdrive midprocessed

        {-
          -- Check for some extra problems that Magma doesn't catch.
          phony (pedalDir </> "problems") $ do
            song <- RBFile.loadMIDI $ pedalDir </> "notes.mid"
            let problems = RB3.findProblems song
            mapM_ putNormal problems
            unless (null problems) $ fail "At least 1 problem was found in the MIDI."
        -}

      lift $ want $ map rel buildables
