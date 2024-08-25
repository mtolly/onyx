{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Build.GuitarHero5 (gh5Rules) where

import           Control.Monad.Extra               (allM)
import           Control.Monad.IO.Class
import           Control.Monad.Random.Strict       (evalRand, mkStdGen)
import           Control.Monad.Trans.Resource
import           Data.Binary.Put                   (runPut)
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as B8
import qualified Data.ByteString.Lazy              as BL
import           Data.Char                         (toUpper)
import           Data.Conduit.Audio
import qualified Data.Conduit.Audio.LAME.Binding   as L
import           Data.Conduit.Audio.SampleRate
import           Data.Foldable                     (toList)
import           Data.Hashable                     (Hashable, hash)
import qualified Data.HashMap.Strict               as HM
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe)
import qualified Data.Text                         as T
import           Development.Shake                 hiding (phony, (%>))
import           Development.Shake.FilePath
import           Onyx.Audio
import           Onyx.Audio.FSB                    (emitFSB, ghBandMP3sToFSB4)
import           Onyx.Audio.Render                 (computeDrumsPart)
import           Onyx.Background                   (VenueTarget (VenueTargetRB3),
                                                    getVenue)
import           Onyx.Build.Common
import           Onyx.Build.GuitarHero5.Logic      (makeGHWoRNote,
                                                    packageNameHash,
                                                    packageNameHashFormat,
                                                    worFileBarePak,
                                                    worFileManifest,
                                                    worFilePS3EmptyVRAMPak,
                                                    worFilePS3SongVRAMPak,
                                                    worFileTextPak)
import           Onyx.Difficulty
import           Onyx.Genre
import           Onyx.Harmonix.DTA.Serialize.Magma (Gender (..))
import           Onyx.MIDI.Common                  (RB3Instrument (..),
                                                    pattern RNil, pattern Wait)
import           Onyx.MIDI.Track.Events            (EventsTrack (..))
import qualified Onyx.MIDI.Track.File              as F
import           Onyx.MIDI.Track.Venue             (venueCameraRB3)
import           Onyx.Neversoft.CRC                (qbKeyCRC, qsKey)
import           Onyx.Neversoft.Crypt              (ghworEncrypt)
import           Onyx.Neversoft.GH4.Metadata       (makeVocalsCents)
import           Onyx.Neversoft.GH5.Note           (makeWoRNoteFile, putNote)
import           Onyx.Neversoft.GH5.Perf
import           Onyx.Neversoft.Pak                (Node (..), buildPak, makeQS,
                                                    parseQS, worMetadataString)
import           Onyx.Neversoft.QB                 (QBArray (..),
                                                    QBSection (..),
                                                    QBStructItem (..), putQB)
import           Onyx.PlayStation.NPData           (ghworCustomMidEdatConfig,
                                                    npdContentID, packNPData)
import           Onyx.PlayStation.PKG              (makePKG)
import           Onyx.Project                      hiding (Difficulty)
import           Onyx.Resources                    (getResourcesPath,
                                                    ghWoRThumbnail)
import           Onyx.StackTrace
import           Onyx.Util.Handle                  (Folder (..), fileReadable)
import           Onyx.Xbox.STFS                    (CreateOptions (..),
                                                    LicenseEntry (..),
                                                    makeCONReadable)
import qualified Sound.MIDI.Util                   as U
import           Text.Read                         (readMaybe)

hashGH5 :: (Hashable f) => SongYaml f -> TargetGH5 f -> Int
hashGH5 songYaml gh5 = let
  hashed =
    ( gh5
    , songYaml.metadata.title
    , songYaml.metadata.artist
    )
  in 1000000000 + (hash hashed `mod` 1000000000)

gh5Rules :: BuildInfo -> FilePath -> TargetGH5 FilePath-> QueueLog Rules ()
gh5Rules buildInfo dir gh5 = do

  let songYaml = biSongYaml buildInfo

  (planName, plan) <- case getPlan gh5.common.plan songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show gh5
    Just pair -> return pair
  let planDir = biGen buildInfo $ "plan" </> T.unpack planName

  let hashed = hashGH5 songYaml gh5
      songID = fromMaybe hashed gh5.songID
      cdl = "cdl" <> show (fromMaybe hashed gh5.cdl)
      songKey = "dlc" <> show songID
      songKeyQB = qbKeyCRC $ B8.pack songKey
      metadata = getTargetMetadata songYaml $ GH5 gh5
      -- Limiting to one-byte chars because I don't know the right way to hash chars beyond U+00FF
      packageInfo = T.map (\c -> if fromEnum c <= 0xFF then c else '_')
        $ targetTitle songYaml (GH5 gh5) <> " (" <> getArtist metadata <> ")"
      -- We put the cdl in as well, otherwise 2 titles that share the first 42 chars can conflict
      -- (for example, a long-title song converted to 2 different speeds)
      packageTitle = T.pack cdl <> " " <> packageInfo
      packageTitles = [packageTitle, "", packageTitle, packageTitle, packageTitle, packageTitle]
      packageDescs = let s = "Custom song created by Onyx Music Game Toolkit" in [s, "", s, s, s, s]
      -- "Emo Edge Track Pack" becomes "emo_edge_track_pack"
      -- "\"Addicted\"" becomes "_addicted_"
      -- "GH: Warriors of Rock 1 Track Pack" becomes "gh__warriors_of_rock_1_track_pack"
      (titleHashHex, titleHash) = packageNameHash packageTitle

      -- I don't actually think these matter or need to be unique
      manifestQBFilenameKey = songKeyQB + 1
      textQBFilenameKey     = songKeyQB + 2
      textQS1FilenameKey    = songKeyQB + 3
      textQS2FilenameKey    = songKeyQB + 4
      textQS3FilenameKey    = songKeyQB + 5
      textQS4FilenameKey    = songKeyQB + 6
      textQS5FilenameKey    = songKeyQB + 7
      songQBFilenameKey     = songKeyQB + 8
      songQSFilenameKey     = songKeyQB + 9
      songNoteFilenameKey   = songKeyQB + 10
      songQB2FilenameKey    = songKeyQB + 11
      songPerfFilenameKey   = songKeyQB + 12

  dir </> "cmanifest.pak.xen" %> \out -> stackIO $ BL.writeFile out
    $ worFileManifest titleHashHex (T.pack cdl) manifestQBFilenameKey [fromIntegral songID]

  dir </> "cdl.pak.xen" %> \out -> stackIO $ BL.writeFile out worFileBarePak

  let difficulties = difficultyGH5 gh5 songYaml
  dir </> "cdl_text.pak.xen" %> \out -> do
    mid <- F.shakeMIDI $ planDir </> "events.mid"
    let _ = mid :: F.Song (F.OnyxFile U.Beats)
        makeQSPair s = let s' = worMetadataString s in (qsKey s', s')
        -- not sure what the \L does; it works without it but we'll just match official songs
        titleQS  = makeQSPair $ "\\L" <> targetTitle songYaml (GH5 gh5)
        artistQS = makeQSPair $ "\\L" <> getArtist metadata
        albumQS  = makeQSPair $ getAlbum metadata
        qs = makeQS [titleQS, artistQS, albumQS]
        genre = worGenre $ interpretGenre
          metadata.genre
          metadata.subgenre
        qb =
          [ QBSectionArray "gh6_dlc_songlist" textQBFilenameKey $
            QBArrayOfQbKey [songKeyQB]
          , QBSectionStruct "gh6_dlc_songlist_props" textQBFilenameKey
            [ QBStructHeader
            , QBStructItemStruct songKeyQB $
              [ QBStructHeader
              , QBStructItemQbKey "checksum" songKeyQB
              , QBStructItemString "name" $ B8.pack songKey
              , QBStructItemQbKeyStringQs "title" $ fst titleQS
              , QBStructItemQbKeyStringQs "artist" $ fst artistQS
              , QBStructItemQbKeyString "artist_text" $ if metadata.cover
                then "artist_text_as_made_famous_by"
                else "artist_text_by"
              , QBStructItemInteger "original_artist"
                $ if metadata.cover then 0 else 1
              -- TODO can we omit year, or pick a better default than 1960
              , QBStructItemInteger "year" $ fromIntegral $ getYear metadata
              , QBStructItemQbKeyStringQs "album_title" $ fst albumQS
              , QBStructItemQbKey "singer" $ case getPart gh5.vocal songYaml >>= (.vocal) >>= (.gender) of
                Nothing     -> "male" -- can we just omit?
                Just Male   -> "male"
                Just Female -> "female"
              , QBStructItemQbKey "genre" $ qbWoRGenre genre
              , QBStructItemInteger "leaderboard" 0 -- does setting this to 0 work?
              , QBStructItemInteger "duration"
                (fromIntegral $ quot (F.songLengthMS mid + 500) 1000) -- this is just displayed in song list
              , QBStructItemInteger "flags" 0 -- what is this?
              , QBStructItemInteger "double_kick" $
                case getPart gh5.drums songYaml >>= (.drums) of
                  Nothing -> 0
                  Just pd -> case pd.kicks of
                    -- TODO I think we need to set this if we have ghost notes even if no double kicks
                    Kicks1x   -> 0
                    Kicks2x   -> 1
                    KicksBoth -> 1
              -- meaning of these seems clear but not sure what criteria you'd use to set them
              , QBStructItemInteger "thin_fretbar_8note_params_low_bpm" 1
              , QBStructItemInteger "thin_fretbar_8note_params_high_bpm" 150
              , QBStructItemInteger "thin_fretbar_16note_params_low_bpm" 1
              , QBStructItemInteger "thin_fretbar_16note_params_high_bpm" 120
              , QBStructItemInteger "guitar_difficulty_rating" $ fromIntegral $ gh5GuitarTier difficulties
              , QBStructItemInteger "bass_difficulty_rating" $ fromIntegral $ gh5BassTier difficulties
              , QBStructItemInteger "vocals_difficulty_rating" $ fromIntegral $ gh5VocalsTier difficulties
              , QBStructItemInteger "drums_difficulty_rating" $ fromIntegral $ gh5DrumsTier difficulties
              , QBStructItemInteger "band_difficulty_rating" 10 -- doesn't seem to be used anywhere, but we could fill in
              -- maybe we could figure out the options for these and match them to the RB kit options?
              , QBStructItemString "snare" "ModernRock"
              , QBStructItemString "kick" "ModernRock"
              , QBStructItemString "tom1" "ModernRock"
              , QBStructItemString "tom2" "ModernRock"
              , QBStructItemString "hihat" "ModernRock"
              , QBStructItemString "cymbal" "ModernRock"
              , QBStructItemString "drum_kit" "ModernRock"
              , QBStructItemString "countoff" "Sticks_Normal"
              , QBStructItemFloat "overall_song_volume" 0
              ] <> toList (makeVocalsCents $ getTuningCents plan)
            ]
          ]
    stackIO $ BL.writeFile out $ worFileTextPak
      (textQBFilenameKey, putQB qb)
      (textQS1FilenameKey, textQS2FilenameKey, textQS3FilenameKey, textQS4FilenameKey, textQS5FilenameKey, qs)

  dir </> "song.pak.xen" %> \out -> do
    shk $ need [dir </> "ghwor.note", dir </> "ghwor.qs", dir </> "drum-anims.txt"]
    note <- stackIO $ BL.readFile $ dir </> "ghwor.note"
    qsSections <- stackIO $ BL.readFile $ dir </> "ghwor.qs"
    qsIDs <- case parseQS qsSections of
      Just pairs -> return $ map fst pairs
      Nothing    -> fatal "Couldn't reparse practice sections .qs file"

    mid <- F.shakeMIDI $ planDir </> "processed.mid"
    endTime <- case mid.s_tracks.onyxEvents.eventsEnd of
      Wait dt _ _ -> return dt
      RNil        -> fatal "panic! no [end] in processed midi"

    let tmap = F.s_tempos mid
        rbCamera = venueCameraRB3 $ flip evalRand (mkStdGen hashed) $ getVenue
          VenueTargetRB3
          partMap
          tmap
          endTime
          mid
        partMap = Map.fromList $ concat
          -- TODO filter based on charted instruments
          [ [(Guitar, gh5.guitar)]
          , [(Bass  , gh5.bass  )]
          , [(Drums , gh5.drums )]
          , [(Vocal , gh5.vocal )]
          ]

    drumAnims <- stackIO (B.readFile $ dir </> "drum-anims.txt") >>= \bs -> case readMaybe $ B8.unpack bs of
      Nothing -> fatal "panic! couldn't read back drums_notes WoR array"
      Just ns -> return ns

    let songKeyAnd s = qbKeyCRC $ B8.pack songKey <> s
        qb =
          [ QBSectionArray (songKeyAnd "_anim_notes") songQBFilenameKey $ QBArrayOfInteger []
          , QBSectionArray (songKeyAnd "_anim") songQBFilenameKey $ QBArrayOfFloatRaw []
          , QBSectionArray (songKeyAnd "_drums_notes") songQBFilenameKey $ QBArrayOfInteger drumAnims
          , QBSectionArray (songKeyAnd "_crowd") songQBFilenameKey $ QBArrayOfFloatRaw []
          , QBSectionArray (songKeyAnd "_lightshow_notes") songQBFilenameKey $ QBArrayOfInteger []
          , QBSectionArray (songKeyAnd "_lightshow") songQBFilenameKey $ QBArrayOfStruct
            [ [ QBStructHeader
              , QBStructItemInteger "time" 0
              , QBStructItemQbKey "scr" "LightShow_SetTime"
              , QBStructItemStruct "params"
                [ QBStructHeader
                , QBStructItemInteger "time" 3
                ]
              ]
            ]
          , QBSectionArray (songKeyAnd "_facial") songQBFilenameKey $ QBArrayOfStruct []
          , QBSectionArray (songKeyAnd "_localized_strings") songQBFilenameKey $ QBArrayOfQbKeyStringQs qsIDs
          ]
        nodes =
          [ ( Node {nodeFileType = ".qb", nodeOffset = 0, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQBFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , putQB qb
            )
          , ( Node {nodeFileType = ".qs.en", nodeOffset = 1, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQSFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , qsSections
            )
          , ( Node {nodeFileType = ".qs.fr", nodeOffset = 2, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQSFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , qsSections
            )
          , ( Node {nodeFileType = ".qs.it", nodeOffset = 3, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQSFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , qsSections
            )
          , ( Node {nodeFileType = ".qs.de", nodeOffset = 4, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQSFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , qsSections
            )
          , ( Node {nodeFileType = ".qs.es", nodeOffset = 5, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQSFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , qsSections
            )
          , ( Node {nodeFileType = ".note", nodeOffset = 6, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songNoteFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , note
            )
          , ( Node {nodeFileType = ".qb", nodeOffset = 7, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = 4208822249, nodeFilenameCRC = 662273024, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            -- nodeFilenameKey and nodeFilenameCRC here are same across songs
            , putQB [QBSectionInteger "g_song_changelist" 4208822249 5377]
            )
          , ( Node {nodeFileType = ".qb", nodeOffset = 8, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songQB2FilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , putQB [QBSectionArray 1148198227 3748754942 $ QBArrayOfStruct []]
            )
          , ( Node {nodeFileType = ".perf", nodeOffset = 9, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = songPerfFilenameKey, nodeFilenameCRC = songKeyQB, nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            -- , makePerf Perf
            --   { perfDLCKey = songKeyQB
            --   , perfUnknown1 = 0
            --   , perfUnknown2 = 0
            --   , perfUnknown3 = 0
            --   , perfEntries =
            --     [ ("autocutcameras", PerfGH5CameraNote [CameraNote 0 0xFFFF 25]) -- drum cam test
            --     , ("momentcameras" , PerfGH5CameraNote [])
            --     ]
            --   }
            , makePerf $ rbCameraToPerf songKeyQB tmap rbCamera endTime
            )
          , ( Node {nodeFileType = ".last", nodeOffset = 10, nodeSize = 0, nodeFilenamePakKey = songKeyQB, nodeFilenameKey = "chunk.last", nodeFilenameCRC = "chunk", nodeUnknown = 0, nodeFlags = 0, nodeName = Nothing}
            , BL.replicate 4 0xAB
            )
          ]
    stackIO $ BL.writeFile out $ buildPak nodes

  (dir </> "ghwor.note", dir </> "ghwor.qs", dir </> "drum-anims.txt") %> \(outNote, outQS, outDrumAnims) -> do
    mid <- F.shakeMIDI $ planDir </> "processed.mid"
    (note, qs, drumAnims) <- makeGHWoRNote songYaml gh5
      (applyTargetMIDI gh5.common mid)
      $ getAudioLength buildInfo planName plan
    stackIO $ BL.writeFile outNote $ runPut $
      putNote songKeyQB $ makeWoRNoteFile note
    stackIO $ BL.writeFile outQS $ makeQS $ HM.toList qs
    stackIO $ writeFile outDrumAnims $ show drumAnims

  dir </> "preview.wav" %> \out -> do
    mid <- F.shakeMIDI $ planDir </> "events.mid"
    let (pstart, pend) = previewBounds metadata (mid :: F.Song (F.OnyxFile U.Beats)) 0 False
        fromMS ms = Seconds $ fromIntegral (ms :: Int) / 1000
    src <- shk $ buildSource
      $ Gain 0.5 -- just guessing at this. without it previews are too loud
      $ Fade End (Seconds 5)
      $ Fade Start (Seconds 2)
      $ Take Start (fromMS $ pend - pstart)
      $ Drop Start (fromMS pstart)
      $ Input (planDir </> "everything.wav")
    runAudio (applySpeedAudio gh5.common src) out

  {-
  -- No longer used, see bottom where we use ps3 (mp3) audio instead

  -- Not supporting stems yet due to FSB generator issue;
  -- it will fail with memory errors on large WAVs, so we have to keep them small.
  -- However they do have to be the full length of the song!
  -- Otherwise pausing doesn't pause the audio once you pass the end of any of the FSBs.
  dir </> "audio1.wav" %> \out -> do
    len <- getAudioLength buildInfo planName plan
    runAudio (applySpeedAudio (common gh5) $ silent (Seconds $ realToFrac len) 1000 8) out
  dir </> "audio2.wav" %> \out -> do
    len <- getAudioLength buildInfo planName plan
    runAudio (applySpeedAudio (common gh5) $ silent (Seconds $ realToFrac len) 1000 6) out
  dir </> "audio3.wav" %> \out -> do
    mid <- F.shakeMIDI $ planDir </> "processed.mid"
    let _ = mid :: F.Song (F.OnyxFile U.Beats)
    src <- shk $ buildSource $ Merge
      $ Input (planDir </> "everything.wav")
      :| [Silence 2 $ Seconds 0]
    runAudio (applyTargetAudio (common gh5) mid src) out

  forM_ ["audio1", "audio2", "audio3", "preview"] $ \audio -> do
    let wav = dir </> audio <.> "wav"
        fsb = dir </> audio <.> "fsb"
    fsb %> \out -> do
      shk $ need [wav]
      xma <- mapStackTraceT (mapQueueLog $ liftIO . runResourceT) $ makeXMAPieces $ Right wav
      ghBandXMAtoFSB4 xma >>= stackIO . BL.writeFile out . emitFSB
    fsb <.> "xen" %> \out -> do
      shk $ need [fsb]
      bs <- stackIO $ B.readFile fsb
      case ghworEncrypt bs of
        Nothing  -> fatal "Unable to encrypt .fsb to .fsb.xen"
        Just enc -> stackIO $ B.writeFile out enc
  -}

  dir </> "ghworlive" %> \out -> do
    let files =
          [ ("cmanifest_" <> titleHash <> ".pak.xen", dir </> "cmanifest.pak.xen")
          , (cdl <> ".pak.xen", dir </> "cdl.pak.xen")
          , (cdl <> "_text.pak.xen", dir </> "cdl_text.pak.xen")
          , ("b" <> songKey <> "_song.pak.xen", dir </> "song.pak.xen")
          , ("a" <> songKey <> "_preview.fsb.xen", dir </> "preview.fsb.xen")
          , ("a" <> songKey <> "_1.fsb.xen", dir </> "audio1.fsb.xen")
          , ("a" <> songKey <> "_2.fsb.xen", dir </> "audio2.fsb.xen")
          , ("a" <> songKey <> "_3.fsb.xen", dir </> "audio3.fsb.xen")
          ]
        folder = Folder
          { folderSubfolders = []
          , folderFiles = map (\(dest, src) -> (T.pack dest, fileReadable src)) files
          }
    shk $ need $ map snd files
    thumb <- stackIO $ ghWoRThumbnail >>= B.readFile
    stackIO $ makeCONReadable CreateOptions
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
      } folder out

  -- PS3 version

  let songKeyCaps        = map toUpper songKey
      cdlCaps            = map toUpper cdl
      titleHashCaps      = map toUpper titleHash
      -- make CAPS_WITH_UNDERSCORES folder name like official songs use
      folderNameCaps     = packageNameHashFormat True packageTitle

      ps3Audio1          = "A" <> songKeyCaps <> "_1.FSB.PS3.EDAT"
      ps3Audio2          = "A" <> songKeyCaps <> "_2.FSB.PS3.EDAT"
      ps3Audio3          = "A" <> songKeyCaps <> "_3.FSB.PS3.EDAT"
      ps3AudioPreview    = "A" <> songKeyCaps <> "_PREVIEW.FSB.PS3.EDAT"
      ps3SongVRAMPak     = "B" <> songKeyCaps <> "_SONG_VRAM.PAK.PS3.EDAT"
      ps3SongPak         = "B" <> songKeyCaps <> "_SONG.PAK.PS3.EDAT"
      ps3CDLTextVRAMPak  = cdlCaps <> "_TEXT_VRAM.PAK.PS3.EDAT"
      ps3CDLTextPak      = cdlCaps <> "_TEXT.PAK.PS3.EDAT"
      ps3CDLVRAMPak      = cdlCaps <> "_VRAM.PAK.PS3.EDAT"
      ps3CDLPak          = cdlCaps <> ".PAK.PS3.EDAT"
      ps3ManifestVRAMPak = "CMANIFEST_" <> titleHashCaps <> "_VRAM.PAK.PS3.EDAT"
      ps3ManifestPak     = "CMANIFEST_" <> titleHashCaps <> ".PAK.PS3.EDAT"

      ps3SongRoot        = dir </> "ps3"
      ps3SongVRAMPakDec  = dir </> "song_vram.pak"
      ps3EmptyVRAMPakDec = dir </> "vram.pak"
      ps3MP3SilenceSmall = dir </> "silence-small.mp3"
      ps3MP3Preview      = dir </> "preview.mp3"
      ps3Audio1PreEdat   = dir </> "audio1.fsb.ps3"
      ps3Audio2PreEdat   = dir </> "audio2.fsb.ps3"
      ps3Audio3PreEdat   = dir </> "audio3.fsb.ps3"
      ps3PreviewPreEdat  = dir </> "preview.fsb.ps3"

      ps3MP3Kick         = dir </> "kick.mp3"
      ps3MP3Snare        = dir </> "snare.mp3"
      ps3MP3Toms         = dir </> "toms.mp3"
      ps3MP3Cymbals      = dir </> "cymbals.mp3"

      ps3MP3Guitar       = dir </> "guitar.mp3"
      ps3MP3Bass         = dir </> "bass.mp3"
      ps3MP3Vocals       = dir </> "vocals.mp3"

      ps3MP3Backing      = dir </> "backing.mp3"
      ps3MP3Crowd        = dir </> "crowd.mp3"

      ps3PkgLabel        = makePS3Name songID songYaml
      ps3EDATConfig      = ghworCustomMidEdatConfig ps3PkgLabel
      ps3ContentID       = npdContentID ps3EDATConfig

  ps3SongVRAMPakDec  %> \out -> stackIO $ BL.writeFile out $ worFilePS3SongVRAMPak songKeyQB
  ps3EmptyVRAMPakDec %> \out -> stackIO $ BL.writeFile out worFilePS3EmptyVRAMPak

  -- I'm not sure if the game requires 48 kHz,
  -- but apparently 44.1 kHz results in inconsistent frame sizes,
  -- which causes problems with the MP3 interleaving.
  let setup lame = liftIO $ do
        L.check $ L.setBrate lame 128
        L.check $ L.setQuality lame 5
        L.check $ L.setOutSamplerate lame 48000
      setupSmall lame = liftIO $ do
        -- Tried 8-bit 16kHz (MPEG-2 layer 3) but got stuck loading in game
        L.check $ L.setBrate lame 32
        L.check $ L.setQuality lame 5
        L.check $ L.setOutSamplerate lame 48000
  ps3MP3Preview %> \out -> do
    src <- shk $ buildSource $ Input (dir </> "preview.wav")
    stackIO $ runResourceT $ sinkMP3PadWithHandle out setup
      $ resampleTo 48000 SincMediumQuality src

  let midAndAudioLength = do
        mid <- F.shakeMIDI $ planDir </> "events.mid"
        return (mid :: F.Song (F.OnyxFile U.Beats), setAudioLength $ realToFrac (F.songLengthMS mid) / 1000 + 1)
      ghDifficulties =
        [ (gh5.drums , difficulties.gh5DrumsTier )
        , (gh5.bass  , difficulties.gh5BassTier  )
        , (gh5.guitar, difficulties.gh5GuitarTier)
        , (gh5.vocal , difficulties.gh5VocalsTier)
        ]
      ghParts = map fst ghDifficulties
      pad = 0 -- TODO did we forget to pad ghwor output?
  ps3MP3SilenceSmall %> \out -> do
    (_, setLength) <- midAndAudioLength
    stackIO $ runResourceT $ sinkMP3PadWithHandle out setupSmall $ setLength $ silent (Frames 0) 48000 2
  (ps3MP3Kick, ps3MP3Snare, ps3MP3Toms, ps3MP3Cymbals) %> \_ -> do
    (mid, setLength) <- midAndAudioLength
    kick    <- sourceKick    buildInfo ghParts gh5.common mid pad SpecStereo planName plan gh5.drums difficulties.gh5DrumsTier
    snare   <- sourceSnare   buildInfo ghParts gh5.common mid pad SpecStereo planName plan gh5.drums difficulties.gh5DrumsTier
    toms    <- sourceToms    buildInfo ghParts gh5.common mid pad SpecStereo planName plan gh5.drums difficulties.gh5DrumsTier
    cymbals <- sourceCymbals buildInfo ghParts gh5.common mid pad SpecStereo planName plan gh5.drums difficulties.gh5DrumsTier
    allSilent <- stackIO $ runResourceT $ allM isSilentSource [kick, snare, toms, cymbals]
    if allSilent
      then do
        shk $ copyFile' ps3MP3SilenceSmall ps3MP3Kick
        shk $ copyFile' ps3MP3SilenceSmall ps3MP3Snare
        shk $ copyFile' ps3MP3SilenceSmall ps3MP3Toms
        shk $ copyFile' ps3MP3SilenceSmall ps3MP3Cymbals
      else do
        stackIO $ runResourceT $ sinkMP3PadWithHandle ps3MP3Kick    setup $ setLength kick
        stackIO $ runResourceT $ sinkMP3PadWithHandle ps3MP3Snare   setup $ setLength snare
        stackIO $ runResourceT $ sinkMP3PadWithHandle ps3MP3Toms    setup $ setLength toms
        stackIO $ runResourceT $ sinkMP3PadWithHandle ps3MP3Cymbals setup $ setLength cymbals
  (ps3MP3Guitar, ps3MP3Bass, ps3MP3Vocals) %> \_ -> do
    (mid, setLength) <- midAndAudioLength
    guitar <- sourceSimplePart buildInfo ghParts gh5.common mid pad SpecStereo planName plan gh5.guitar difficulties.gh5GuitarTier
    bass   <- sourceSimplePart buildInfo ghParts gh5.common mid pad SpecStereo planName plan gh5.bass   difficulties.gh5BassTier
    vocals <- sourceSimplePart buildInfo ghParts gh5.common mid pad SpecStereo planName plan gh5.vocal  difficulties.gh5VocalsTier
    allSilent <- stackIO $ runResourceT $ allM isSilentSource [guitar, bass, vocals]
    if allSilent
      then do
        shk $ copyFile' ps3MP3SilenceSmall ps3MP3Guitar
        shk $ copyFile' ps3MP3SilenceSmall ps3MP3Bass
        shk $ copyFile' ps3MP3SilenceSmall ps3MP3Vocals
      else do
        stackIO $ runResourceT $ sinkMP3PadWithHandle ps3MP3Guitar setup $ setLength guitar
        stackIO $ runResourceT $ sinkMP3PadWithHandle ps3MP3Bass   setup $ setLength bass
        stackIO $ runResourceT $ sinkMP3PadWithHandle ps3MP3Vocals setup $ setLength vocals
  ps3MP3Backing %> \out -> do
    (mid, setLength) <- midAndAudioLength
    backing <- sourceBacking buildInfo gh5.common mid pad planName plan ghDifficulties
    (_, _, isGHDrums) <- computeDrumsPart gh5.drums plan songYaml
    backingWithDrums <- if isGHDrums
      then return backing
      else do
        kit <- sourceKit buildInfo ghParts gh5.common mid pad SpecStereo planName plan gh5.drums difficulties.gh5DrumsTier
        return $ mix backing kit
    stackIO $ runResourceT $ sinkMP3PadWithHandle out setup $ setLength backingWithDrums
  ps3MP3Crowd %> \out -> do
    (mid, setLength) <- midAndAudioLength
    crowd <- sourceCrowd buildInfo gh5.common mid pad planName plan
    stackIO $ runResourceT $ sinkMP3PadWithHandle out setup $ setLength crowd

  let readMP3s paths = do
        shk $ need paths
        stackIO $ mapM (fmap BL.fromStrict . B.readFile) paths
      writeEncryptedFSB out mp3s = do
        fsb <- ghBandMP3sToFSB4 mp3s
        case ghworEncrypt $ BL.toStrict $ emitFSB fsb of
          Nothing  -> fatal "Unable to encrypt .fsb to .fsb.{xen/ps3}"
          Just enc -> stackIO $ B.writeFile out enc
  ps3Audio1PreEdat %> \out -> do
    mp3s <- readMP3s [ps3MP3Kick, ps3MP3Snare, ps3MP3Toms, ps3MP3Cymbals]
    writeEncryptedFSB out mp3s
  ps3Audio2PreEdat %> \out -> do
    mp3s <- readMP3s [ps3MP3Guitar, ps3MP3Bass, ps3MP3Vocals]
    writeEncryptedFSB out mp3s
  ps3Audio3PreEdat %> \out -> do
    mp3s <- readMP3s [ps3MP3Backing, ps3MP3Crowd]
    writeEncryptedFSB out mp3s
  ps3PreviewPreEdat %> \out -> do
    shk $ need [ps3MP3Preview]
    preview <- stackIO $ BL.fromStrict <$> B.readFile ps3MP3Preview
    writeEncryptedFSB out [preview]

  let packNPData' cfg fin fout name = do
        shk $ need [fin]
        stackIO $ packNPData cfg fin fout name
  ps3SongRoot </> ps3Audio1 %> \out -> do
    packNPData' ps3EDATConfig ps3Audio1PreEdat out $ B8.pack ps3Audio1
  ps3SongRoot </> ps3Audio2 %> \out -> do
    packNPData' ps3EDATConfig ps3Audio2PreEdat out $ B8.pack ps3Audio2
  ps3SongRoot </> ps3Audio3 %> \out -> do
    packNPData' ps3EDATConfig ps3Audio3PreEdat out $ B8.pack ps3Audio3
  ps3SongRoot </> ps3AudioPreview %> \out -> do
    packNPData' ps3EDATConfig ps3PreviewPreEdat out $ B8.pack ps3AudioPreview
  ps3SongRoot </> ps3SongVRAMPak %> \out -> do
    packNPData' ps3EDATConfig ps3SongVRAMPakDec out $ B8.pack ps3SongVRAMPak
  ps3SongRoot </> ps3SongPak %> \out -> do
    packNPData' ps3EDATConfig (dir </> "song.pak.xen") out $ B8.pack ps3SongPak
  ps3SongRoot </> ps3CDLTextVRAMPak %> \out -> do
    packNPData' ps3EDATConfig ps3EmptyVRAMPakDec out $ B8.pack ps3CDLTextVRAMPak
  ps3SongRoot </> ps3CDLTextPak %> \out -> do
    packNPData' ps3EDATConfig (dir </> "cdl_text.pak.xen") out $ B8.pack ps3CDLTextPak
  ps3SongRoot </> ps3CDLVRAMPak %> \out -> do
    packNPData' ps3EDATConfig ps3EmptyVRAMPakDec out $ B8.pack ps3CDLVRAMPak
  ps3SongRoot </> ps3CDLPak %> \out -> do
    packNPData' ps3EDATConfig (dir </> "cdl.pak.xen") out $ B8.pack ps3CDLPak
  ps3SongRoot </> ps3ManifestVRAMPak %> \out -> do
    packNPData' ps3EDATConfig ps3EmptyVRAMPakDec out $ B8.pack ps3ManifestVRAMPak
  ps3SongRoot </> ps3ManifestPak %> \out -> do
    packNPData' ps3EDATConfig (dir </> "cmanifest.pak.xen") out $ B8.pack ps3ManifestPak

  dir </> "ps3.pkg" %> \out -> do
    shk $ need $ map (ps3SongRoot </>)
      [ ps3Audio1, ps3Audio2, ps3Audio3, ps3AudioPreview, ps3SongVRAMPak, ps3SongPak
      , ps3CDLTextVRAMPak, ps3CDLTextPak, ps3CDLVRAMPak, ps3CDLPak, ps3ManifestVRAMPak, ps3ManifestPak
      ]
    let container name inner = Folder { folderSubfolders = [(name, inner)], folderFiles = [] }
    main <- container "USRDIR" . container folderNameCaps <$> crawlFolderBytes ps3SongRoot
    extra <- stackIO (getResourcesPath "pkg-contents/ghwor") >>= crawlFolderBytes
    stackIO $ makePKG ps3ContentID (main <> extra) out

  -- redirect ps3 fsb to xbox
  dir </> "audio1.fsb.xen"  %> shk . copyFile' (ps3SongRoot </> ps3Audio1PreEdat )
  dir </> "audio2.fsb.xen"  %> shk . copyFile' (ps3SongRoot </> ps3Audio2PreEdat )
  dir </> "audio3.fsb.xen"  %> shk . copyFile' (ps3SongRoot </> ps3Audio3PreEdat )
  dir </> "preview.fsb.xen" %> shk . copyFile' (ps3SongRoot </> ps3PreviewPreEdat)
