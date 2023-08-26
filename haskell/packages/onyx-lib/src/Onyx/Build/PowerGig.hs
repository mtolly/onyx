{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Build.PowerGig (pgRules) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.Aeson                       as A
import           Data.Binary.Put                  (runPut)
import           Data.Bits                        ((.|.))
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import qualified Data.Text.Lazy                   as TL
import qualified Data.Vector                      as V
import           Development.Shake                hiding (phony, (%>))
import           Development.Shake.FilePath
import           Onyx.Audio
import           Onyx.Audio.FSB                   (writeXMA2)
import           Onyx.Build.Common
import           Onyx.Guitar                      (guitarify')
import           Onyx.MIDI.Common
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.MIDI.Track.FiveFret
import           Onyx.Mode
import           Onyx.PowerGig.Crypt              (buildHeader, encryptE2,
                                                   makeNewPK, rebuildFullHeader)
import qualified Onyx.PowerGig.GEV                as PG
import qualified Onyx.PowerGig.Songs              as PG
import           Onyx.Project                     hiding (Difficulty)
import           Onyx.Reductions                  (completeFiveResult)
import           Onyx.Resources                   (getResourcesPath,
                                                   powerGigThumbnail,
                                                   powerGigTitleThumbnail)
import           Onyx.StackTrace
import           Onyx.Util.Handle                 (Folder (..), crawlFolder,
                                                   fileReadable)
import           Onyx.Xbox.STFS                   (CreateOptions (..),
                                                   LicenseEntry (..),
                                                   makeCONReadable)
import qualified Sound.MIDI.Util                  as U
import           Text.Mustache                    (compileMustacheFile,
                                                   renderMustache)

pgRules :: BuildInfo -> FilePath -> TargetPG FilePath -> QueueLog Rules ()
pgRules buildInfo dir pg = do

  let songYaml      = biSongYaml buildInfo
      key           = fromMaybe "TodoAutoSongKey" pg.key -- TODO generate automatic
      k             = T.unpack key
      metadata      = getTargetMetadata songYaml $ PG pg

      objSTFS       = dir </> "pglive"
      objAddContent = dir </> "stfs/AddContent.lua"
      objDataHdr    = dir </> "stfs/Data.hdr.e.2"
      objDataPk     = dir </> "stfs/Data.pk0"
      objGEV        = dir </> "pk/Audio/songs" </> k </> (k <> ".gev")
      objCueGEV     = dir </> "pk/Audio/songs" </> k </> (k <> "_cue.gev")
      objXML        = dir </> "pk/Audio/songs" </> k </> (k <> ".xml")
      objAudio      = dir </> "audio.xma"
      objAudioE2    = dir </> "pk/Audio/songs" </> k </> (k <> "_all.xma.e.2")
      objDrumKick   = dir </> "pk/Audio/songs" </> k </> "samples" </> (k <> "_kick_iso.xma")
      objDrumSnare  = dir </> "pk/Audio/songs" </> k </> "samples" </> (k <> "_snare_iso.xma")
      objDrumTomHi  = dir </> "pk/Audio/songs" </> k </> "samples" </> (k <> "_tom_hi_iso.xma")
      objDrumTomLow = dir </> "pk/Audio/songs" </> k </> "samples" </> (k <> "_tom_low_iso.xma")
      objDrumCrash  = dir </> "pk/Audio/songs" </> k </> "samples" </> (k <> "_crash_iso.xma")
      objLua        = dir </> "pk/Scripting/Songs" </> (k <> ".lua")

  (planName, _plan) <- case getPlan pg.common.plan songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show pg
    Just pair -> return pair
  let planDir = biGen buildInfo $ "plan" </> T.unpack planName

  objSTFS %> \out -> do
    let files = [objAddContent, objDataHdr, objDataPk]
    shk $ need files
    let folder = Folder
          { folderSubfolders = []
          , folderFiles = [ (T.pack $ takeFileName f, fileReadable f) | f <- files ]
          }
    thumb <- stackIO $ powerGigThumbnail >>= B.readFile
    titleThumb <- stackIO $ powerGigTitleThumbnail >>= B.readFile
    stackIO $ makeCONReadable CreateOptions
      { createNames = [getTitle metadata]
      , createDescriptions = [""]
      , createTitleID = 0x5A4607D1
      , createTitleName = "Power Gig: Rise of the Six String"
      , createThumb = thumb
      , createTitleThumb = titleThumb
      , createLicenses = [LicenseEntry (-1) 1 1, LicenseEntry (-1) 1 0]
      , createMediaID       = 0
      , createVersion       = 0
      , createBaseVersion   = 0
      , createTransferFlags = 0xC0
      , createLIVE = True
      } folder out

  let renderMustacheUTF8 template = TE.encodeUtf8 . TL.toStrict . renderMustache template
      escapeLuaString :: T.Text -> T.Text
      escapeLuaString s = s -- TODO

  objAddContent %> \out -> do
    template <- stackIO (getResourcesPath "power-gig/AddContent.lua") >>= compileMustacheFile
    stackIO $ B.writeFile out $ renderMustacheUTF8 template $ A.object
      [ "package_name" .= escapeLuaString (getTitle metadata)
      , "song_key" .= escapeLuaString key
      ]

  [objDataHdr, objDataPk] %> \_ -> do
    shk $ need
      [ objGEV, objCueGEV, objXML, objAudioE2, objLua
      , objDrumKick, objDrumSnare, objDrumTomHi, objDrumTomLow, objDrumCrash
      ]
    (folder, pk) <- stackIO $ crawlFolder (dir </> "pk") >>= makeNewPK 0
    hdrE2 <- encryptE2 $ BL.toStrict $ buildHeader $ rebuildFullHeader folder
    stackIO $ BL.writeFile objDataHdr hdrE2
    stackIO $ BL.writeFile objDataPk pk

  objGEV %> \out -> do
    mid <- F.shakeMIDI $ planDir </> "processed.mid"
    let _ = mid :: F.Song (F.OnyxFile U.Beats)
        (strh, strs, mapping) = PG.makeStringBank
          [ "guitar_1_expert"
          , "drums_1_expert"
          , "beat"
          ]
        gev = PG.GEV
          { gevPCMC = PG.PCMC
            { pcmcUnk1 = 1
            , pcmcUnk2 = 0 -- timestamp, probably doesn't matter
            , pcmcUnk3 = 0
            , pcmcMIDI = B.concat ["songs:", TE.encodeUtf8 key, "\\", TE.encodeUtf8 key, ".mid"]
            }
          , gevGELH = PG.GELH $ V.fromList $ catMaybes
            [ flip fmap (getPart pg.guitar songYaml >>= anyFiveFret) $ \builder -> let
              result = completeFiveResult False (F.s_signatures mid) $ builder FiveTypeGuitar ModeInput
                { tempo = F.s_tempos mid
                , events = F.onyxEvents $ F.s_tracks mid
                , part = F.getFlexPart pg.guitar $ F.s_tracks mid
                }
              notes :: RTB.T U.Beats ([(Maybe Color, StrumHOPOTap)], Maybe U.Beats)
              notes = guitarify' $ fromMaybe mempty $ Map.lookup Expert result.notes
              in PG.GELS
                { gelsUnk1      = 2
                , gelsTrackName = fromMaybe (error "panic! gev string not found") $ Map.lookup "guitar_1_expert" mapping
                , gelsUnk3      = fromIntegral $ length notes
                , gelsUnk4      = 0 -- TODO unknown
                , gelsUnk5      = 0 -- TODO unknown
                , gelsUnk6      = 0 -- TODO unknown
                , gelsUnk7      = 0 -- number of power chords
                , gelsGEVT      = V.fromList $ do
                  -- TODO hopos
                  (absBeats, (fretsSHT, mlen)) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 notes
                  let startSecs = realToFrac $ U.applyTempoMap (F.s_tempos mid) absBeats
                      sustSecs = case mlen of
                        Nothing  -> 0
                        Just len -> let
                          endSecs = realToFrac $ U.applyTempoMap (F.s_tempos mid) $ absBeats + len
                          in endSecs - startSecs
                      frets = map fst fretsSHT
                  return PG.GEVT
                    { gevtTime     = startSecs
                    , gevtTime2    = startSecs
                    , gevtSustain  = sustSecs
                    , gevtGameBits = foldr (.|.) 0 $ map PG.guitarDrumBit $ concat
                      [ [PG.Bit_StandardNote]
                      , [PG.Bit_Green  | elem (Just Green ) frets]
                      , [PG.Bit_Red    | elem (Just Red   ) frets]
                      , [PG.Bit_Yellow | elem (Just Yellow) frets]
                      , [PG.Bit_Blue   | elem (Just Blue  ) frets]
                      , [PG.Bit_Orange | elem (Just Orange) frets]
                      ]
                    , gevtUnk5     = 2 -- this matches the gelsUnk1 for this track
                    , gevtType     = 2 -- note
                    , gevtData1    = 0 -- pitch of lowest source midi note, hopefully doesn't matter
                    , gevtData2    = 127 -- velocity of note
                    }
                }
            ]
          , gevSTRH = strh
          , gevSTRS = strs
          , gevTMPO = PG.makeTempos $ F.s_tempos mid
          }
    stackIO $ BL.writeFile out $ runPut $ PG.showGEV gev

  objCueGEV %> \out -> do
    mid <- F.shakeMIDI $ planDir </> "processed.mid"
    let _ = mid :: F.Song (F.OnyxFile U.Beats)
        (strh, strs, _mapping) = PG.makeStringBank []
        gev = PG.GEV
          { gevPCMC = PG.PCMC
            { pcmcUnk1 = 1
            , pcmcUnk2 = 0 -- timestamp, probably doesn't matter
            , pcmcUnk3 = 0
            , pcmcMIDI = B.concat ["songs:", TE.encodeUtf8 key, "\\", TE.encodeUtf8 key, "_cue.mid"]
            }
          , gevGELH = PG.GELH $ V.fromList []
          , gevSTRH = strh
          , gevSTRS = strs
          , gevTMPO = PG.makeTempos $ F.s_tempos mid
          }
    stackIO $ BL.writeFile out $ runPut $ PG.showGEV gev

  objXML %> \out -> do
    mid <- F.shakeMIDI $ planDir </> "processed.mid"
    let _ = mid :: F.Song (F.OnyxFile U.Beats)
        -- Hoping this works to be able to give instruments no audio channels
        emptyAudio = PG.Mode
          { mode_2d = Nothing
          , mode_3d = Nothing
          , mode_speakers = Just PG.ModeSpeakers
            { mode_speakers_speaker      = V.fromList
              [ PG.Speaker
                { speaker_output        = "front_left"
                , speaker_front_left    = Nothing
                , speaker_front_right   = Nothing
                , speaker_front_center  = Nothing
                , speaker_low_frequency = Nothing
                , speaker_back_left     = Nothing
                , speaker_back_right    = Nothing
                , speaker_side_left     = Nothing
                , speaker_side_right    = Nothing
                }
              , PG.Speaker
                { speaker_output        = "front_right"
                , speaker_front_left    = Nothing
                , speaker_front_right   = Nothing
                , speaker_front_center  = Nothing
                , speaker_low_frequency = Nothing
                , speaker_back_left     = Nothing
                , speaker_back_right    = Nothing
                , speaker_side_left     = Nothing
                , speaker_side_right    = Nothing
                }
              ]
            , mode_speakers_mute_levels  = Nothing
            , mode_speakers_kit          = V.empty
            , mode_speakers_unmute_pad   = Nothing
            , mode_speakers_num_speakers = Just 2
            , mode_speakers_num_channels = Just 0
            , mode_speakers_base_channel = Just 1
            }
          }
        song = PG.Song
          { song_info   = PG.Info
            { info_title            = getTitle metadata
            , info_title_short      = Nothing
            , info_title_shorter    = Nothing
            , info_artist           = getArtist metadata
            , info_artist_short     = Nothing
            , info_year             = getYear metadata -- xsd claims this can only go 1900 to 2020?
            , info_album            = getAlbum metadata
            , info_composer         = ""
            , info_lyricist         = ""
            , info_genre            = "Rock and Roll" -- TODO either translate to valid genre, or check if any genre works
            , info_cover_artwork    = PG.CoverArtwork
              { art_file_name = "b2wild.png"
              }
            , info_url              = ""
            , info_length           = let
              len = quot (F.songLengthMS mid) 1000
              (minutes, seconds) = quotRem len 60
              in PG.Length
                { length_minutes = minutes
                , length_seconds = seconds
                }
            , info_singer_gender    = Nothing -- "male" "female" or "both"
            -- if intensities are absent, are those instruments disabled?
            , info_guitar_intensity = Just 1 -- 1 to 3 inclusive
            , info_drums_intensity  = Just 1
            , info_vocals_intensity = Nothing
            }
          , song_audio  = PG.Audio
            { audio_combined_audio = Just PG.CombinedAudio
              { ca_file         = key <> "_all_mp3.fsb.e.2"
              , ca_xbox360_file = Just $ key <> "_all.xma.e.2"
              , ca_ps3_file     = Nothing
              , ca_num_channels = 2
              }
            , audio_count_off      = Nothing
            , audio_preview        = V.singleton $ let
              (pstart, pend) = previewBounds metadata mid 0 False
              in PG.Preview
                { preview_start_position   = Just $ fromIntegral pstart / 1000
                , preview_attack_time      = Just 1
                , preview_release_position = Just $ fromIntegral (pstart + pend) / 1000
                , preview_release_time     = Just 1
                , preview_file             = Nothing
                , preview_xbox360_file     = Nothing
                , preview_ps3_file         = Nothing
                }
            , audio_midi           = key <> ".mid"
            , audio_cue            = key <> "_cue.mid"
            , audio_tempo          = key <> "_tempo.mid"
            , audio_key_signature  = PG.KeySignature
              -- TODO
              { ks_note  = "C"
              , ks_pitch = "natural"
              , ks_key   = "major"
              }
            , audio_bpm            = 120 -- TODO get average I guess
            , audio_chords         = V.empty
            , audio_compressor     = Nothing
            , audio_backing_track  = PG.InstrumentType
              { inst_chord_resolution  = Nothing
              , inst_sustain_threshold = Nothing
              , inst_fly_time          = Nothing
              , inst_tolerance         = Nothing
              , inst_wav_file          = Nothing
              , inst_xbox360_file      = Nothing
              , inst_volume            = 0.7
              , inst_fill_mute_level   = Nothing
              , inst_ramp_times        = Nothing
              , inst_mode              = PG.Mode
                { mode_2d = Nothing
                , mode_3d = Nothing
                , mode_speakers = Just PG.ModeSpeakers
                  { mode_speakers_speaker      = V.fromList
                    [ PG.Speaker
                      { speaker_output        = "front_left"
                      , speaker_front_left    = Just 1
                      , speaker_front_right   = Just 0
                      , speaker_front_center  = Nothing
                      , speaker_low_frequency = Nothing
                      , speaker_back_left     = Nothing
                      , speaker_back_right    = Nothing
                      , speaker_side_left     = Nothing
                      , speaker_side_right    = Nothing
                      }
                    , PG.Speaker
                      { speaker_output        = "front_right"
                      , speaker_front_left    = Just 0
                      , speaker_front_right   = Just 1
                      , speaker_front_center  = Nothing
                      , speaker_low_frequency = Nothing
                      , speaker_back_left     = Nothing
                      , speaker_back_right    = Nothing
                      , speaker_side_left     = Nothing
                      , speaker_side_right    = Nothing
                      }
                    ]
                  , mode_speakers_mute_levels  = Nothing
                  , mode_speakers_kit          = V.empty
                  , mode_speakers_unmute_pad   = Nothing
                  , mode_speakers_num_speakers = Just 2
                  , mode_speakers_num_channels = Just 2
                  , mode_speakers_base_channel = Just 1 -- 1-indexed I assume
                  }
                }
              , inst_stereo_width      = Nothing
              , inst_samples           = Nothing
              , inst_display_range     = Nothing
              }
            , audio_guitar         = PG.InstrumentType
              { inst_chord_resolution  = Just PG.PerDifficulty
                { pd_beginner = Nothing
                , pd_easy     = Nothing
                , pd_medium   = Nothing
                , pd_hard     = Nothing
                , pd_expert   = Just 1
                }
              , inst_sustain_threshold = Just PG.PerDifficulty
                { pd_beginner = Nothing
                , pd_easy     = Nothing
                , pd_medium   = Just 1420
                , pd_hard     = Just 1420
                , pd_expert   = Just 950
                }
              , inst_fly_time          = Nothing
              , inst_tolerance         = Nothing
              , inst_wav_file          = Nothing
              , inst_xbox360_file      = Nothing
              , inst_volume            = 0.7
              , inst_fill_mute_level   = Nothing
              , inst_ramp_times        = Just PG.RampTimes
                { ramp_mute_down      = Just 0.07
                , ramp_mute_up        = Just 0.07
                , ramp_fill_mute_down = Just 0.1
                , ramp_fill_mute_up   = Just 0.5
                }
              , inst_mode              = emptyAudio
              , inst_stereo_width      = Nothing
              , inst_samples           = Nothing
              , inst_display_range     = Nothing
              }
            , audio_drums          = PG.InstrumentType
              { inst_chord_resolution  = Just PG.PerDifficulty
                { pd_beginner = Nothing
                , pd_easy     = Nothing
                , pd_medium   = Nothing
                , pd_hard     = Nothing
                , pd_expert   = Just 1
                }
              , inst_sustain_threshold = Nothing
              , inst_fly_time          = Nothing
              , inst_tolerance         = Nothing
              , inst_wav_file          = Nothing
              , inst_xbox360_file      = Nothing
              , inst_volume            = 0.7
              , inst_fill_mute_level   = Nothing
              , inst_ramp_times        = Just PG.RampTimes
                { ramp_mute_down      = Just 0.07
                , ramp_mute_up        = Just 0.07
                , ramp_fill_mute_down = Just 0.1
                , ramp_fill_mute_up   = Just 0.1
                }
              , inst_mode              = emptyAudio
              , inst_stereo_width      = Nothing
              , inst_samples           = Nothing
              , inst_display_range     = Nothing
              }
            , audio_vocals         = PG.InstrumentType
              { inst_chord_resolution  = Nothing
              , inst_sustain_threshold = Nothing
              , inst_fly_time          = Nothing
              , inst_tolerance         = Nothing
              , inst_wav_file          = Nothing
              , inst_xbox360_file      = Nothing
              , inst_volume            = 0.7
              , inst_fill_mute_level   = Nothing
              , inst_ramp_times        = Nothing
              , inst_mode              = emptyAudio
              , inst_stereo_width      = Nothing
              , inst_samples           = Nothing
              , inst_display_range     = Just PG.DisplayRange
                { dr_height = 10
                }
              }
            }
          , song_source = "dlc"
          }
    stackIO $ B.writeFile out $ PG.showSongXML song

  objAudio %> \out -> do
    let wav = planDir </> "everything.wav"
    shk $ need [wav]
    xma <- mapStackTraceT (mapQueueLog $ liftIO . runResourceT) $ makeXMAPieces $ Left $ fileReadable wav
    stackIO $ writeXMA2 out xma
  objAudioE2 %> \out -> do
    shk $ need [objAudio]
    stackIO (B.readFile objAudio) >>= encryptE2 >>= stackIO . BL.writeFile out

  objDrumKick %> \out -> do
    src <- stackIO $ getResourcesPath "power-gig/samples/TornadoOfSouls_kick_iso.xma"
    shk $ copyFile' src out
  objDrumSnare %> \out -> do
    src <- stackIO $ getResourcesPath "power-gig/samples/TornadoOfSouls_snare_iso.xma"
    shk $ copyFile' src out
  objDrumTomHi %> \out -> do
    src <- stackIO $ getResourcesPath "power-gig/samples/TornadoOfSouls_tom_hi_iso.xma"
    shk $ copyFile' src out
  objDrumTomLow %> \out -> do
    src <- stackIO $ getResourcesPath "power-gig/samples/TornadoOfSouls_tom_low_iso.xma"
    shk $ copyFile' src out
  objDrumCrash %> \out -> do
    src <- stackIO $ getResourcesPath "power-gig/samples/TornadoOfSouls_crash_iso.xma"
    shk $ copyFile' src out

  objLua %> \out -> do
    lua <- stackIO $ getResourcesPath "power-gig/song-script.lua" >>= B.readFile
    -- will need to eval template when we add lipsync
    encryptE2 lua >>= stackIO . BL.writeFile out
