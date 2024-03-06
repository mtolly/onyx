{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
module Onyx.Build (shakeBuildFiles, shakeBuild, targetTitle, validFileName, validFileNamePiece, NameRule(..), hashRB3) where

import           Codec.Picture
import qualified Codec.Picture.STBIR              as STBIR
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Bifunctor                   (second)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base64.Lazy      as B64
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isSpace)
import           Data.Conduit                     (runConduit)
import           Data.Conduit.Audio
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Centi, Fixed (..), Milli)
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (intercalate, nubOrd, sort)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Time                        (defaultTimeLocale,
                                                   formatTime)
import           Development.Shake                hiding (phony, (%>))
import           Development.Shake.FilePath
import           Onyx.Audio
import           Onyx.Audio.Render
import           Onyx.Audio.Search
import           Onyx.Build.CloneHero
import           Onyx.Build.Common
import           Onyx.Build.GuitarHero1
import           Onyx.Build.GuitarHero2
import           Onyx.Build.GuitarHero3
import           Onyx.Build.GuitarHero5
import           Onyx.Build.PowerGig
import qualified Onyx.Build.RB3CH                 as RB3
import           Onyx.Build.RockBand
import           Onyx.Build.Rocksmith
import           Onyx.Codec.JSON                  (loadYaml)
import qualified Onyx.DTXMania.DTX                as DTX
import           Onyx.Guitar                      (applyLongStatus, guitarify')
import           Onyx.Harmonix.MOGG
import           Onyx.Image.DXT
import           Onyx.Keys.Ranges
import qualified Onyx.MelodysEscape               as Melody
import           Onyx.MIDI.Common
import qualified Onyx.MIDI.Track.Drums            as D
import qualified Onyx.MIDI.Track.Drums.True       as TD
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.Mode
import           Onyx.Overdrive                   (calculateUnisons,
                                                   getOverdrive, printFlexParts)
import           Onyx.Project                     hiding (Difficulty)
import           Onyx.Reaper.Build                (TuningInfo (..),
                                                   makeReaperShake)
import           Onyx.Resources                   (webDisplay)
import           Onyx.StackTrace
import           Onyx.Util.Files                  (copyDirRecursive)
import           Onyx.Util.Text.Decode            (decodeGeneral)
import           Onyx.WebPlayer                   (makeDisplay)
import           Path                             (parseAbsDir, toFilePath)
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.Jammit.Base                as J
import qualified Sound.Jammit.Export              as J
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.Environment.Executable    (getExecutablePath)
import           Text.Read                        (readMaybe)

forceRW :: (MonadIO m) => FilePath -> StackTraceT m ()
forceRW f = stackIO $ do
  p <- Dir.getPermissions f
  Dir.setPermissions f $ Dir.setOwnerReadable True $ Dir.setOwnerWritable True p

printOverdrive :: FilePath -> StackTraceT (QueueLog Action) ()
printOverdrive mid = do
  song <- F.shakeMIDI mid
  let _ = song :: F.Song (F.OnyxFile U.Beats)
  od <- calculateUnisons <$> getOverdrive (F.s_tracks song)
  forM_ (ATB.toPairList $ RTB.toAbsoluteEventList 0 od) $ \(posn, unison) -> do
    let posn' = showPosition (F.s_signatures song) posn
    if all (== 0) [ t | (t, _, _) <- NE.toList unison ]
      then lg $ posn' <> ": " <> printFlexParts [ inst | (_, inst, _) <- NE.toList unison ]
      else lg $ intercalate "\n" $ (posn' <> ":") : do
        (t, inst, _) <- NE.toList unison
        return $ "  (" <> show (realToFrac t :: Milli) <> ") " <> printFlexParts [inst]

------------------------------------------------------------------------------

dtxRules :: BuildInfo -> FilePath -> TargetDTX FilePath -> QueueLog Rules ()
dtxRules buildInfo dir dtx = do

  let songYaml = biSongYaml buildInfo
      rel = biRelative buildInfo
      gen = biGen buildInfo
      metadata = getTargetMetadata songYaml $ DTX dtx

  (planName, _plan) <- case getPlan dtx.common.plan songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show dtx
    Just pair -> return pair
  let planDir = gen $ "plan" </> T.unpack planName

  -- TODO use Onyx.Mode to fetch drums
  let dtxPartDrums  = case getPart dtx.drums songYaml >>= (.drums) of
        Just pd -> Just (dtx.drums, pd)
        Nothing -> Nothing
      dtxPartGuitar = fmap (, dtx.guitar) $ getPart dtx.guitar songYaml >>= anyFiveFret
      dtxPartBass   = fmap (, dtx.bass  ) $ getPart dtx.bass   songYaml >>= anyFiveFret

  dir </> "dtx/empty.wav" %> \out -> do
    buildAudio (Silence 1 $ Seconds 0) out

  dir </> "dtx/bgm.ogg" %> \out -> do
    let wav = planDir </> "everything.wav"
    buildAudio (Input wav) out

  dir </> "dtx/preview.ogg" %> \out -> do
    let planPreviewDir = case dtx.planPreview of
          Nothing   -> planDir
          Just name -> gen $ "plan" </> T.unpack name
    -- not supporting different tempo maps (so we don't need to use planPreviewDir for midi load)
    mid <- F.shakeMIDI $ planDir </> "processed.mid"
    let (pstart, pend) = previewBounds metadata (mid :: F.Song (F.OnyxFile U.Beats)) 0 False
        fromMS ms = Seconds $ fromIntegral (ms :: Int) / 1000
        previewExpr
          = Fade End (Seconds 5)
          $ Fade Start (Seconds 2)
          $ Take Start (fromMS $ pend - pstart)
          $ Drop Start (fromMS pstart)
          $ Input (planPreviewDir </> "everything.wav")
    buildAudio previewExpr out

  artPath <- case metadata.fileAlbumArt of
    Just img | elem (takeExtension img) [".jpg", ".jpeg"] -> do
      dir </> "dtx/cover.jpg" %> shk . copyFile' img
      return "cover.jpg"
    _ -> return "cover.png"
  dir </> "dtx/cover.png" %> shk . copyFile' (gen "cover-full.png")

  let loadMapping = forM (dtxPartDrums >>= \(_, pd) -> pd.fileDTXKit) $ \f -> do
        shk $ need [f]
        bs <- liftIO $ B.readFile f
        case readMaybe $ T.unpack $ decodeGeneral bs of
          Nothing -> fail $ "Couldn't parse mapping of full drums to DTX template from: " <> f
          Just m  -> return (f, m)
      loadMappingTemplate = do
        mapping <- loadMapping
        template <- forM mapping $ \(f, DTX.DTXMapping templateRel _ _) -> do
          let templateFixed = takeDirectory f </> templateRel
          shk $ need [templateFixed]
          templateDTX <- liftIO $ DTX.readDTXLines DTX.FormatDTX <$> DTX.loadDTXLines templateFixed
          return (templateFixed, templateDTX)
        return (mapping, template)

  (dir </> "dtx/mstr.dtx", dir </> "used-chips.txt") %> \(out, outUsedChips) -> do
    mid <- F.shakeMIDI $ planDir </> "processed.mid"
    (mapping, template) <- loadMappingTemplate
    let bgmChip   = "0X" -- TODO read this from BGMWAV in mapping
        emptyChip = "ZZ"
        makeGuitarBass = \case
          Nothing          -> (RTB.empty, RTB.empty)
          Just (buildFive, partName) -> let
            input = ModeInput
              { tempo  = F.s_tempos mid
              , events = F.onyxEvents $ F.s_tracks mid
              , part   = F.getFlexPart partName $ F.s_tracks mid
              }
            result = buildFive FiveTypeGuitarExt input
            noteGroups = guitarify' $ fromMaybe RTB.empty $ Map.lookup Expert result.notes
            dtxNotes = (\(gems, _len) -> (sort $ catMaybes $ map fst gems, emptyChip)) <$> noteGroups
            dtxLongs = U.trackJoin $ RTB.mapMaybe
              (\(_, mlen) -> (\len -> RTB.fromPairList [(0, ()), (len, ())]) <$> mlen)
              noteGroups
            in (dtxNotes, dtxLongs)
        (gtrNotes , gtrLongs ) = makeGuitarBass dtxPartGuitar
        (bassNotes, bassLongs) = makeGuitarBass dtxPartBass
        drumDifficulty = dtxPartDrums >>= \(_part, pd) -> case pd.difficultyDTX of
          Just (MkFixed cents) -> Just $ let
            (twoDigit, thirdDigit) = quotRem cents 10
            in (fromIntegral twoDigit, fromIntegral thirdDigit)
          Nothing              -> Nothing -- translate RB rank/tier?
        dtxFile = DTX.DTX
          { DTX.dtx_TITLE         = Just $ targetTitle songYaml $ DTX dtx
          , DTX.dtx_ARTIST        = Just $ getArtist metadata
          , DTX.dtx_PREIMAGE      = Just artPath
          , DTX.dtx_COMMENT       = Nothing
          , DTX.dtx_GENRE         = metadata.genre
          , DTX.dtx_PREVIEW       = Just "preview.ogg"
          , DTX.dtx_STAGEFILE     = Nothing
          , DTX.dtx_DLEVEL        = fst <$> drumDifficulty
          , DTX.dtx_GLEVEL        = Nothing
          , DTX.dtx_BLEVEL        = Nothing
          , DTX.dtx_DLVDEC        = snd <$> drumDifficulty
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
          , DTX.dtx_MeasureMap    = F.s_signatures mid
          , DTX.dtx_TempoMap      = F.s_tempos mid
          , DTX.dtx_Drums         = case dtxPartDrums of
            Nothing          -> RTB.empty
            Just (part, _pd) -> let
              track
                = maybe mempty F.onyxPartTrueDrums
                $ Map.lookup part
                $ F.onyxParts
                $ F.s_tracks mid
              -- TODO need to properly handle blip overrides on flams!
              trueNotes
                = applyLongStatus (TD.tdChipOverride track)
                $ TD.splitFlams (F.s_tempos mid)
                $ TD.addExplicitStomps (4 :: U.Beats)
                $ TD.getDifficulty Nothing track
              toDTXNotes = fmap $ \(currentOverrides, tdn) -> let
                lane = case TD.tdn_gem tdn of
                  TD.Kick      -> case TD.tdn_limb tdn of
                    Just D.LH -> DTX.LeftBass
                    _         -> DTX.BassDrum
                  TD.Snare     -> DTX.Snare
                  TD.Hihat     -> case TD.tdn_type tdn of
                    TD.GemHihatOpen -> DTX.HihatOpen
                    _               -> DTX.HihatClose
                  TD.HihatFoot -> DTX.LeftPedal
                  TD.CrashL    -> DTX.LeftCymbal
                  TD.Tom1      -> DTX.HighTom
                  TD.Tom2      -> DTX.LowTom
                  TD.Tom3      -> DTX.FloorTom
                  TD.CrashR    -> DTX.Cymbal
                  TD.Ride      -> DTX.RideCymbal
                chip = fromMaybe emptyChip $ do
                  (_, DTX.DTXMapping _ conds allOverrides) <- mapping
                  let addedConds = do
                        DTX.DTXOverride tag extraConds <- allOverrides
                        guard $ elem tag currentOverrides
                        extraConds
                  DTX.lookupDTXMapping (addedConds <> conds) tdn
                in (lane, chip)
              in toDTXNotes trueNotes
          , DTX.dtx_DrumsDummy    = RTB.empty
          , DTX.dtx_Guitar        = gtrNotes
          , DTX.dtx_GuitarWailing = RTB.empty
          , DTX.dtx_GuitarLong    = gtrLongs
          , DTX.dtx_Bass          = bassNotes
          , DTX.dtx_BassWailing   = RTB.empty
          , DTX.dtx_BassLong      = bassLongs
          , DTX.dtx_BGM           = RTB.singleton 0 bgmChip
          , DTX.dtx_BGMExtra      = HM.empty
          , DTX.dtx_Video         = RTB.empty
          }
        usedChips :: [DTX.Chip]
        usedChips = nubOrd $ concat
          [ map snd $ toList $ DTX.dtx_Drums dtxFile
          , map snd $ toList $ DTX.dtx_DrumsDummy dtxFile
          , map snd $ toList $ DTX.dtx_Guitar dtxFile
          , map snd $ toList $ DTX.dtx_Bass dtxFile
          , toList $ DTX.dtx_BGM dtxFile
          , toList (DTX.dtx_BGMExtra dtxFile) >>= toList
          ]
        isUsedChip k _ = elem k usedChips
    liftIO $ B.writeFile out $ TE.encodeUtf16LE $ T.cons '\xFEFF' $ DTX.makeDTX dtxFile
      { DTX.dtx_WAV    = HM.filterWithKey isUsedChip $ DTX.dtx_WAV    dtxFile
      , DTX.dtx_VOLUME = HM.filterWithKey isUsedChip $ DTX.dtx_VOLUME dtxFile
      , DTX.dtx_PAN    = HM.filterWithKey isUsedChip $ DTX.dtx_PAN    dtxFile
      }
    liftIO $ writeFile outUsedChips $ show usedChips

  phony (dir </> "dtx") $ do
    shk $ need
      [ dir </> "dtx/empty.wav"
      , dir </> "dtx/bgm.ogg"
      , dir </> "dtx/preview.ogg"
      , dir </> "dtx" </> artPath
      , dir </> "dtx/mstr.dtx"
      , dir </> "used-chips.txt"
      ]
    usedChips <- shk (readFile' $ dir </> "used-chips.txt") >>=
      maybe (fatal "Panic! Couldn't parse used-chips file") return . readMaybe
    let _ = usedChips :: [DTX.Chip]
    (_, template) <- loadMappingTemplate
    forM_ template $ \(templatePath, templateDTX) -> do
      let usedPaths
            = nubOrd
            $ map snd
            $ filter ((`elem` usedChips) . fst)
            $ HM.toList
            $ DTX.dtx_WAV templateDTX
      forM_ usedPaths $ \path -> do
        -- again, maybe make sure path is local
        when (path /= "bgm.ogg") $ do
          shk $ copyFile'
            (takeDirectory templatePath </> path)
            (rel $ dir </> "dtx" </> path)

------------------------------------------------------------------------------

-- Not used at the moment (removed Melody's Escape target types)
_melodyRules :: BuildInfo -> FilePath -> TargetPart FilePath -> QueueLog Rules ()
_melodyRules buildInfo dir tgt = do

  let songYaml = biSongYaml buildInfo
      gen = biGen buildInfo

  (planName, _) <- case getPlan tgt.common.plan songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show tgt
    Just pair -> return pair
  let planDir = gen $ "plan" </> T.unpack planName
      midraw = planDir </> "raw.mid"

  -- Melody's Escape customs
  let melodyAudio = dir </> "melody/audio.ogg"
      melodyChart = dir </> "melody/song.track"
  -- TODO support audio speed
  melodyAudio %> shk . copyFile' (planDir </> "everything.ogg")
  melodyChart %> \out -> do
    shk $ need [midraw, melodyAudio]
    mid <- F.shakeMIDI midraw
    melody <- liftIO
      $ Melody.randomNotes
      $ maybe mempty F.onyxMelody
      $ Map.lookup tgt.part
      $ F.onyxParts
      $ F.s_tracks mid
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
          , Melody.writeTransitions (F.s_tempos mid) melody
          , Melody.writeNotes (F.s_tempos mid) melody
          ]
    liftIO $ writeFile out str
  phony (dir </> "melody") $ shk $ need [melodyAudio, melodyChart]

------------------------------------------------------------------------------

shakeBuildFiles :: (MonadIO m, Traversable f) => [FilePath] -> FilePath -> f FilePath -> StackTraceT (QueueLog m) (f FilePath)
shakeBuildFiles audioDirs yamlPath = shakeBuild audioDirs yamlPath []

shakeBuild :: (MonadIO m, Traversable f) => [FilePath] -> FilePath -> [(T.Text, Target FilePath)] -> f FilePath -> StackTraceT (QueueLog m) (f FilePath)
shakeBuild audioDirs yamlPathRel extraTargets buildables = do

  yamlPath <- stackIO $ Dir.canonicalizePath yamlPathRel
  songYaml <- loadYaml yamlPath

  checkDefined songYaml

  exeTime <- stackIO $ getExecutablePath >>= Dir.getModificationTime
  yamlTime <- stackIO $ Dir.getModificationTime yamlPath

  audioLib <- newAudioLibrary
  forM_ audioDirs $ \dir -> do
    p <- parseAbsDir dir
    addAudioDir audioLib p

  writeMsg <- lift $ QueueLog ask
  let yamlDir = takeDirectory yamlPath
      genFolder = concat
        [ "gen-"
        , formatTime defaultTimeLocale "%s" exeTime
        , "-"
        , formatTime defaultTimeLocale "%s" yamlTime
        ]
      rel f = yamlDir </> f
      gen f = rel $ genFolder </> f
      ourShakeOptions = shakeOptions
        { shakeThreads = 0
        , shakeFiles = rel genFolder
        , shakeOutput = \verb str -> if verb <= Warn
          then writeMsg (MessageWarning, Message str [])
          else if verb <= Info
            then writeMsg (MessageLog, Message str [])
            else return ()
        }
      audioDependPath name = rel $ genFolder </> "audio" </> T.unpack name <.> "wav"
      buildInfo = BuildInfo
        { biSongYaml = songYaml
        , biYamlDir  = yamlDir
        , biRelative = rel
        , biAudioLib = audioLib
        , biAudioDependPath = audioDependPath
        , biOggWavForPlan = \planName i -> rel $ genFolder </> "plan" </> T.unpack planName </> ("channel-" <> show i <> ".wav")
        , biGenFolder = genFolder
        }

  let isSlash c = c == '/' || c == '\\'
  buildables' <- forM buildables $ \buildable -> case break isSlash buildable of
    ("gen", rest) -> return $ genFolder </> dropWhile isSlash rest
    _             -> return buildable

  do

    shakeEmbed ourShakeOptions $ do

      phony "yaml"  $ lg $ show songYaml
      phony "audio" $ lg $ show audioDirs

      -- Find and convert all Jammit audio into the work directory
      let jammitAudioParts = map J.Only    [minBound .. maxBound]
                          ++ map J.Without [minBound .. maxBound]
      forM_ (HM.toList songYaml.jammit) $ \(jammitName, jammitQuery) ->
        forM_ jammitAudioParts $ \audpart ->
          rel (jammitPath genFolder jammitName audpart) %> \out -> do
            inside ("Looking for the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart) $ do
              let title  = fromMaybe (getTitle  songYaml.metadata) jammitQuery.title
                  artist = fromMaybe (getArtist songYaml.metadata) jammitQuery.artist
                  inst   = fromJammitInstrument $ J.audioPartToInstrument audpart
              p <- searchJammit audioLib (title, artist, inst)
              result <- stackIO $ fmap J.getAudioParts $ J.loadLibrary $ toFilePath p
              case [ jcfx | (audpart', jcfx) <- result, audpart == audpart' ] of
                jcfx : _ -> do
                  lg $ "Found the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart
                  stackIO $ J.runAudio [jcfx] [] out
                []       -> fail "Couldn't find a necessary Jammit track"

      -- Cover art
      gen "cover.bmp"      %> \out -> loadRGB8 songYaml >>= stackIO . writeBitmap  out . STBIR.resize STBIR.defaultOptions 256 256
      gen "cover.png"      %> \out -> loadRGB8 songYaml >>= stackIO . writePng     out . STBIR.resize STBIR.defaultOptions 256 256
      gen "cover-full.png" %> \out -> loadRGB8 songYaml >>= stackIO . writePng     out
      let hmxImageTypes =
            [ (".png_xbox", PNGXbox)
            , (".png_ps3" , PNGPS3 )
            , (".png_wii" , PNGWii )
            ]
      forM_ hmxImageTypes $ \(ext, pngType) -> do
        gen ("cover" <> ext) %> \out -> case songYaml.metadata.fileAlbumArt of
          Just f | takeExtension f == ext -> do
            shk $ copyFile' f out
            forceRW out
          _      -> loadRGB8 songYaml >>= stackIO . BL.writeFile out . toDXT1File pngType

      gen "notes.mid" %> \out -> shk $ do
        let f = rel songYaml.global.fileMidi
        doesFileExist f >>= \b -> if b
          then copyFile' f out
          else F.saveMIDIUtf8 out F.Song
            { F.s_tempos = U.tempoMapFromBPS RTB.empty
            , F.s_signatures = U.measureMapFromTimeSigs U.Error RTB.empty
            , F.s_tracks = mempty :: F.OnyxFile U.Beats
            }

      forM_ (HM.toList songYaml.audio) $ \(name, _) -> do
        audioDependPath name %> \out -> do
          let getSamples = fail "Sample-based audio can't be used as dependencies outside of a plan"
          src <- manualLeaf yamlDir audioLib (audioDepend buildInfo) getSamples songYaml $ Named name
          buildAudio src out

      forM_ (extraTargets <> HM.toList songYaml.targets) $ \(targetName, target) -> do
        let dir = gen $ "target" </> T.unpack targetName
        case target of
          RB3 rb3 -> rbRules buildInfo dir rb3 Nothing
          RB2 rb2 -> let
            rb3 = TargetRB3
              { common = rb2.common
              , is2xBassPedal = rb2.is2xBassPedal
              , songID = rb2.songID
              , guitar = rb2.guitar
              , bass = rb2.bass
              , drums = rb2.drums
              , vocal = rb2.vocal
              , keys = F.FlexExtra "undefined"
              , harmonix = False
              , magma = rb2.magma
              , ps3Encrypt = rb2.ps3Encrypt
              , legalTempos = rb2.legalTempos
              }
            in rbRules buildInfo dir rb3 $ Just rb2
          GH1 gh1 -> gh1Rules buildInfo dir gh1
          GH2 gh2 -> gh2Rules buildInfo dir gh2
          PS ps -> psRules buildInfo dir ps
          RS rs -> rsRules buildInfo dir rs
          GH5 gh5 -> gh5Rules buildInfo dir gh5
          GH3 gh3 -> gh3Rules buildInfo dir gh3
          DTX dtx -> dtxRules buildInfo dir dtx
          PG pg -> pgRules buildInfo dir pg

      forM_ (HM.toList songYaml.plans) $ \(planName, plan) -> do

        let dir = gen $ "plan" </> T.unpack planName

        -- plan audio, currently only used for REAPER project
        let allPlanParts :: [(F.FlexPartName, PartAudio ())]
            allPlanParts = case plan of
              StandardPlan x -> HM.toList $ (void <$> x.parts).getParts
              MoggPlan     x -> do
                (fpart, pa) <- HM.toList x.parts.getParts
                guard $ not $ null $ concat $ toList pa
                return (fpart, void pa)
            dummyMIDI :: F.Song (F.OnyxFile U.Beats)
            dummyMIDI = F.Song
              { F.s_tempos = U.tempoMapFromBPS RTB.empty
              , F.s_signatures = U.measureMapFromTimeSigs U.Error RTB.empty
              , F.s_tracks = mempty :: F.OnyxFile U.Beats
              }
        dir </> "song.wav" %> \out -> do
          s <- sourceBacking buildInfo def dummyMIDI 0 planName plan [ (fpart, 1) | (fpart, _) <- allPlanParts ]
          runAudio (clampIfSilent s) out
        dir </> "crowd.wav" %> \out -> do
          s <- sourceCrowd buildInfo def dummyMIDI 0 planName plan
          runAudio (clampIfSilent s) out
        forM_ allPlanParts $ \(fpart, pa) -> do
          let name = T.unpack $ F.getPartName fpart
          case pa of
            PartSingle () -> do
              dir </> name <.> "wav" %> \out -> do
                s <- sourceSimplePart buildInfo [fpart] def dummyMIDI 0 SpecNoPannedMono planName plan fpart 1
                runAudio (clampIfSilent s) out
            PartDrumKit mkick msnare mtoms () -> do
              forM_ mkick $ \() -> do
                dir </> (name ++ "-kick") <.> "wav" %> \out -> do
                  s <- sourceKick buildInfo [fpart] def dummyMIDI 0 SpecNoPannedMono planName plan fpart 1
                  runAudio (clampIfSilent s) out
              forM_ msnare $ \() -> do
                dir </> (name ++ "-snare") <.> "wav" %> \out -> do
                  s <- sourceSnare buildInfo [fpart] def dummyMIDI 0 SpecNoPannedMono planName plan fpart 1
                  runAudio (clampIfSilent s) out
              dir </> (name ++ "-kit") <.> "wav" %> \out -> do
                s <- sourceKit buildInfo [fpart] def dummyMIDI 0 SpecNoPannedMono planName plan fpart 1
                runAudio (clampIfSilent s) out
              forM_ mtoms $ \() -> do
                dir </> (name ++ "-toms") <.> "wav" %> \out -> do
                  s <- sourceToms buildInfo [fpart] def dummyMIDI 0 SpecNoPannedMono planName plan fpart 1
                  runAudio (clampIfSilent s) out
                dir </> (name ++ "-cymbals") <.> "wav" %> \out -> do
                  s <- sourceCymbals buildInfo [fpart] def dummyMIDI 0 SpecNoPannedMono planName plan fpart 1
                  runAudio (clampIfSilent s) out
        let allPlanAudio :: [FilePath]
            allPlanAudio = map (dir </>) $ concat
              [ [ "song.wav" ]
              , sort $ allPlanParts >>= \(fpart, pa) -> let
                name = T.unpack $ F.getPartName fpart
                in case pa of
                  PartSingle () -> [name <.> "wav"]
                  PartDrumKit mkick msnare mtoms () -> concat
                    [ map (\() -> (name ++ "-kick") <.> "wav") $ toList mkick
                    , map (\() -> (name ++ "-snare") <.> "wav") $ toList msnare
                    , case mtoms of
                      Nothing -> [(name ++ "-kit") <.> "wav"]
                      Just () ->
                        [ (name ++ "-toms"   ) <.> "wav"
                        , (name ++ "-cymbals") <.> "wav"
                        ]
                    ]
              , [ "crowd.wav"
                | case plan of
                  StandardPlan x -> isJust x.crowd
                  MoggPlan     x -> not $ null x.crowd
                ]
              ]

        -- REAPER project
        rel ("notes-" ++ T.unpack planName ++ ".RPP") %> \out -> do
          let tempo = rel $ fromMaybe (genFolder </> "notes.mid") $ getFileTempo plan
              tunings = TuningInfo
                { guitars = do
                  (fpart, part) <- HM.toList songYaml.parts.getParts
                  pg <- toList part.proGuitar
                  return (fpart, pg.tuning)
                , cents = getTuningCents plan
                }
          makeReaperShake tunings (gen "notes.mid") tempo allPlanAudio out

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
          MoggPlan x -> do
            src <- shk $ buildSource $ Input $ case x.fileMOGG of
              Just path | takeExtension path == ".bik" -> rel path
              _                                        -> dir </> "audio.ogg"
            let volsNoCrowd = zipWith noCrowd [0..] x.vols
                noCrowd i vol = if elem i x.crowd then -99 else vol
            runAudio (applyPansVols (map realToFrac x.pans) (map realToFrac volsNoCrowd) src) out
          StandardPlan x -> do
            let planAudios = concat
                  [ toList x.song
                  , toList x.parts >>= toList
                  ]
            srcs <- mapM (buildAudioToSpec yamlDir audioLib (audioDepend buildInfo) songYaml [(-1, 0), (1, 0)] planName . Just) planAudios
            case srcs of
              s : ss -> runAudio (foldr mix s ss) out
              []     -> buildAudio (Silence 2 $ Frames 0) out
        dir </> "everything.ogg" %> buildAudio (Input $ dir </> "everything.wav")

        -- MIDI files

        let midprocessed = dir </> "processed.mid"
            midevents = dir </> "events.mid"
            midraw = dir </> "raw.mid"
            sampleTimes = dir </> "samples.txt"
            display = dir </> "display.json"
        midraw %> \out -> do
          lg "Loading the MIDI file..."
          input <- F.shakeMIDI $ gen "notes.mid"
          let _ = input :: F.Song (F.RawFile U.Beats)
          tempos <- fmap F.s_tempos $ case getFileTempo plan of
            Nothing -> return input
            Just m  -> F.shakeMIDI m
          F.saveMIDIUtf8 out input { F.s_tempos = tempos }
        (midprocessed, midevents) %> \_ -> do
          -- basically just autogen a BEAT track
          input <- F.shakeMIDI midraw
          output <- RB3.processTiming input $ getAudioLength buildInfo planName plan
          F.saveMIDIUtf8 midprocessed output
          -- save a separate midi with only EVENTS track and samples,
          -- to use for timing and audio processing
          F.saveMIDIUtf8 midevents output
            { F.s_tracks = mempty
              { F.onyxEvents  = F.onyxEvents  $ F.s_tracks output
              , F.onyxSamples = F.onyxSamples $ F.s_tracks output
              }
            }

        sampleTimes %> \out -> do
          input <- F.shakeMIDI midraw
          let _ = input :: F.Song (F.OnyxFile U.Beats)
              output :: [(T.Text, [(Double, T.Text, T.Text)])]
              output = map (second getSampleList) $ Map.toList $ F.onyxSamples $ F.s_tracks input
              getSampleList = map makeSampleTriple . ATB.toPairList . RTB.toAbsoluteEventList 0 . F.sampleTriggers
              makeSampleTriple (bts, trigger) =
                ( realToFrac $ U.applyTempoMap (F.s_tempos input) bts
                , F.sampleGroup trigger
                , F.sampleAudio trigger
                )
          stackIO $ writeFile out $ show output

        display %> \out -> do
          song <- F.shakeMIDI midprocessed
          liftIO $ BL.writeFile out $ makeDisplay songYaml song

        -- Getting MOGG/OGG from MoggPlan
        let ogg  = dir </> "audio.ogg"
            wav  = dir </> "audio.wav"
            channelWAV i = dir </> ("channel-" <> show i <> ".wav")
            mogg = dir </> "audio.mogg"
        case plan of
          StandardPlan _ -> return ()
          MoggPlan     x -> do
            ogg %> \out -> do
              shk $ need [mogg]
              if x.decryptSilent
                then errorToWarning (moggToOggFiles mogg out) >>= \case
                  Just () -> return ()
                  Nothing -> do
                    -- Make a no-samples ogg with the right channel count
                    buildAudio (Silence (length x.pans) $ Frames 0) out
                else moggToOggFiles mogg out
            wav %> case x.fileMOGG of
              Just path | takeExtension path == ".bik" -> buildAudio (Input $ rel path)
              _ -> buildAudio (Input ogg)
            let allChannelWAVs = map channelWAV [0 .. length x.pans - 1]
            allChannelWAVs %> \_ -> do
              src <- lift $ lift $ buildSource $ case x.fileMOGG of
                Just path | takeExtension path == ".bik" -> Input $ rel path
                _                                        -> Input ogg
              stackIO $ audioToChannelWAVs src allChannelWAVs
            mogg %> \out -> do
              p <- inside "Searching for MOGG file" $ case (x.fileMOGG, x.moggMD5) of
                (Nothing, Nothing ) -> fatal "No file path or MD5 hash specified for MOGG file"
                (Just f , _       ) -> return f -- TODO maybe check md5 if it is specified
                (Nothing, Just md5) -> toFilePath <$> searchMOGG audioLib md5
              lg $ "MOGG file location: " <> p
              -- TODO: check if it's actually an OGG (starts with OggS)
              shk $ copyFile' p out
              forceRW out
            dir </> "silent-channels.txt" %> \out -> do
              src <- lift $ lift $ buildSource $ case x.fileMOGG of
                Just path | takeExtension path == ".bik" -> Input $ rel path
                _                                        -> Input ogg
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
          song <- F.shakeMIDI midprocessed
          lg $ T.unpack $ closeShiftsFile song

        -- Print out a summary of (non-vocal) overdrive and unison phrases
        phony (dir </> "overdrive") $ printOverdrive midprocessed

        {-
          -- Check for some extra problems that Magma doesn't catch.
          phony (pedalDir </> "problems") $ do
            song <- F.loadMIDI $ pedalDir </> "notes.mid"
            let problems = RB3.findProblems song
            mapM_ putNormal problems
            unless (null problems) $ fail "At least 1 problem was found in the MIDI."
        -}

      lift $ want $ map rel $ toList buildables'

  return buildables'
