{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Import.PowerGig where

import           Audio                          (Audio (..),
                                                 decibelDifferenceInPanRatios,
                                                 fromStereoPanRatios,
                                                 stereoPanRatios)
import           Config
import           Control.Monad                  (forM, unless)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                as B
import           Data.Default.Class             (def)
import qualified Data.HashMap.Strict            as HM
import           Data.List.NonEmpty             (NonEmpty (..), nonEmpty)
import qualified Data.Map                       as Map
import           Data.Maybe                     (listToMaybe)
import           Data.SimpleHandle              (Folder, Readable,
                                                 byteStringSimpleHandle,
                                                 findFileCI, handleToByteString,
                                                 makeHandle, useHandle)
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           Guitars
import           Import.Base
import           PowerGig.Crypt
import           PowerGig.MIDI
import           PowerGig.Songs
import qualified RockBand.Codec.Drums           as D
import qualified RockBand.Codec.File            as RBFile
import qualified RockBand.Codec.Five            as F
import           RockBand.Common                (Difficulty (..), edgeBlipsRB_)
import           Sound.FSB                      (XMAContents (..),
                                                 extractXMAStream, makeXMAs,
                                                 markXMAPacketStreams, parseXMA,
                                                 splitXMA2Packets,
                                                 writeXMA2Packets)
import qualified Sound.MIDI.Util                as U
import           System.FilePath                (dropExtension)

importPowerGig :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m [Import m]
importPowerGig hdrE2 = do
  let base = dropExtension $ dropExtension $ dropExtension hdrE2 -- drop ".hdr.e.2"
  hdr <- stackIO $ B.readFile "/customs/powergig/Data.hdr.e.2" >>= decryptE2 >>= readHeader
  let folder = connectPKFiles base $ getFolder hdr
  keys <- stackIO $ findSongKeys folder
  forM keys $ \key -> do
    song <- loadSongXML key folder
    return $ importPowerGigSong key song folder

importPowerGigSong :: (SendMessage m, MonadIO m) => T.Text -> Song -> Folder T.Text Readable -> Import m
importPowerGigSong key song folder level = do

  mid <- case level of
    ImportQuick -> return emptyChart
    ImportFull -> case findFileCI ("Audio" :| ["songs", key, audio_midi $ song_audio song]) folder of
      Nothing -> fatal "Couldn't find MIDI file"
      Just r  -> RBFile.loadMIDIReadable r
  let _ = mid :: RBFile.Song (PGFile U.Beats)
  let onyxFile = mempty
        { RBFile.onyxParts = Map.fromList
          [ (RBFile.FlexGuitar, mempty
            { RBFile.onyxPartGuitar = mempty
              { F.fiveDifficulties = Map.fromList
                [ (Easy  , getGuitar pgGuitarEasy  )
                , (Medium, getGuitar pgGuitarMedium)
                , (Hard  , getGuitar pgGuitarHard  )
                , (Expert, getGuitar pgGuitarExpert)
                ]
              }
            })
          , (RBFile.FlexDrums, mempty
            { RBFile.onyxPartDrums = mempty
              { D.drumDifficulties = Map.fromList
                [ (Easy  , getDrums pgDrumsEasy  )
                , (Medium, getDrums pgDrumsMedium)
                , (Hard  , getDrums pgDrumsHard  )
                , (Expert, getDrums pgDrumsExpert)
                ]
              }
            })
          -- TODO vox
          ]
        , RBFile.onyxBeat = mempty -- TODO
        }
      getDrums f = let
        pg = f $ RBFile.s_tracks mid
        in mempty
          { D.drumGems = (, D.VelocityNormal) <$> drumGems pg
          }
      getGuitar f = let
        pg = f $ RBFile.s_tracks mid
        -- not sure of hopo system
        in emit5' $ strumHOPOTap' HOPOsRBGuitar (170/480) $ edgeBlipsRB_ $ guitarGems pg
      onyxMid = mid { RBFile.s_tracks = onyxFile }

  combinedAudio <- case audio_combined_audio $ song_audio song of
    Nothing -> fatal "No combined_audio in XML"
    Just ca -> return ca
  streamCount <- case quotRem (ca_num_channels combinedAudio) 2 of
    (n, 0) -> return n
    _      -> fatal $ "Expected an even num_channels for combined_audio, but found " <> show (ca_num_channels combinedAudio)
  let xboxAudio = ca_xbox360_file combinedAudio >>= T.stripSuffix ".e.2"
  (rate, samples, packetsPerBlock, xmas) <- case level of
    ImportQuick -> return (44100, 0, 32, replicate streamCount []) -- probably doesn't matter but powergig uses 44100 Hz and 32-packet XMA blocks
    ImportFull -> case xboxAudio >>= \x -> findFileCI ("Audio" :| ["songs", key, x]) folder of
      Nothing -> fatal "Couldn't find Xbox 360 (XMA) audio, PS3 not supported yet"
      Just r -> do
        xma <- stackIO (useHandle r handleToByteString) >>= parseXMA
        packets <- fmap markXMAPacketStreams $ splitXMA2Packets $ xmaData xma
        let ppblk = xmaPacketsPerBlock xma
        return (xmaRate xma, xmaSamples xma, xmaPacketsPerBlock xma, map (\i -> extractXMAStream ppblk i packets) [0 .. streamCount - 1])
  xmaBytes <- stackIO $ makeXMAs $ flip map xmas $ \xma -> XMAContents
    { xmaChannels = 2
    , xmaRate     = rate
    , xmaSamples  = samples
    , xmaPacketsPerBlock = packetsPerBlock
    , xmaData     = writeXMA2Packets xma
    }
  let audio = flip map (zip [0..] xmaBytes) $ \(i, bs) -> let
        name = "stream-" <> show (i :: Int)
        filename = name <> ".xma"
        afile = AudioFile AudioInfo
          { _md5 = Nothing
          , _frames = Nothing
          , _filePath = Just $ SoftFile filename $ SoftReadable
            $ makeHandle filename $ byteStringSimpleHandle bs
          , _commands = []
          , _rate = Nothing
          , _channels = 2
          }
        in (T.pack name, afile)

  let getPlanAudio f = do
        let inst = f $ song_audio song
        speakers <- case mode_speakers $ inst_mode inst of
          Nothing -> fatal "No mode_speakers found"
          Just ms -> return ms
        unless (mode_speakers_num_speakers speakers == Just 2) $ fatal "mode_speakers doesn't have 2 speakers"
        frontLeft <- maybe (fatal "No front_left speaker found") return $ listToMaybe
          [ spkr | spkr <- V.toList $ mode_speakers_speaker speakers, speaker_output spkr == "front_left" ]
        frontRight <- maybe (fatal "No front_right speaker found") return $ listToMaybe
          [ spkr | spkr <- V.toList $ mode_speakers_speaker speakers, speaker_output spkr == "front_right" ]
        numChannels <- maybe (fatal "No num_channels") return $ mode_speakers_num_channels speakers
        baseChannel <- maybe (fatal "No base_channel") return $ mode_speakers_base_channel speakers

        let channelIndexes = take numChannels [baseChannel - 1 ..]
            makeChannels ((i, 0) : (j, 1) : rest) | i == j
              = Input (Named $ fst $ audio !! i) : makeChannels rest
            makeChannels ((i, side) : rest)
              = Channels [Just side] (Input $ Named $ fst $ audio !! i) : makeChannels rest
            makeChannels [] = []
            channels = makeChannels $ map (`quotRem` 2) channelIndexes

            ratioPairs = take numChannels $ map (\g -> (maybe 0 realToFrac $ g frontLeft, maybe 0 realToFrac $ g frontRight))
              [ speaker_front_left, speaker_front_right, speaker_front_center, speaker_low_frequency
              , speaker_back_left, speaker_back_right, speaker_side_left, speaker_side_right
              ]

        let pans = map fromStereoPanRatios ratioPairs
            globalVolRatio = realToFrac $ inst_volume inst
            vols = zipWith decibelDifferenceInPanRatios
              (map stereoPanRatios pans)
              (map (\(l, r) -> (l * globalVolRatio, r * globalVolRatio)) ratioPairs)
            -- This fixes negative infinity being serialized to null in song.yml, and deserialized to NaN.
            -- (Negative infinity comes via fromStereoPanRatios for an unused drum channel in Hold On)
            -- After upgrading to aeson >= 2.0.0.0 this won't be necessary (will serialize to "-inf" string)
            fixNegativeInfinity = max (-99)
        channels' <- maybe (fatal "No channels") return $ nonEmpty channels
        return $ PlanAudio
          { _planExpr = Merge channels'
          , _planPans = map realToFrac pans
          , _planVols = map (fixNegativeInfinity . realToFrac) vols
          }

  audioBacking <- getPlanAudio audio_backing_track
  audioGuitar  <- PartSingle <$> getPlanAudio audio_guitar
  audioDrums   <- PartSingle <$> getPlanAudio audio_drums
  audioVocals  <- PartSingle <$> getPlanAudio audio_vocals

  return SongYaml
    { _metadata = Metadata
      { _title = Just $ info_title $ song_info song
      , _titleJP = Nothing
      , _artist = Just $ info_artist $ song_info song
      , _artistJP = Nothing
      , _album = Just $ info_album $ song_info song
      , _genre = Just $ info_genre $ song_info song -- does this need to be edited
      , _subgenre = Nothing
      , _year = Just $ info_year $ song_info song
      , _fileAlbumArt = Nothing -- TODO
      , _trackNumber = Nothing
      , _comments = []
      , _key = Nothing
      , _author = Nothing
      , _rating = Unrated
      , _previewStart = Nothing -- TODO
      , _previewEnd = Nothing -- TODO
      , _languages = []
      , _convert = False
      , _rhythmKeys = False
      , _rhythmBass = False
      , _catEMH = False
      , _expertOnly = False
      , _cover = False
      , _difficulty = Tier 1
      }
    , _jammit = HM.empty
    , _audio = HM.fromList audio
    , _plans = HM.singleton "powergig" Plan
      { _song = Just audioBacking
      , _countin = Countin []
      , _planParts = Parts $ HM.fromList
        [ (RBFile.FlexGuitar, audioGuitar)
        , (RBFile.FlexDrums , audioDrums )
        , (RBFile.FlexVocal , audioVocals)
        ]
      , _crowd = Nothing
      , _planComments = []
      , _tuningCents = 0
      , _fileTempo = Nothing
      }
    , _global = def'
      { _fileMidi = SoftFile "notes.mid" $ SoftChart onyxMid
      , _fileSongAnim = Nothing
      , _backgroundVideo = Nothing
      , _fileBackgroundImage = Nothing
      }
    , _parts = Parts $ HM.fromList
      [ (RBFile.FlexGuitar, def
        { partGRYBO = Just def
          { gryboDifficulty = Tier 1 -- TODO
          }
        })
      , (RBFile.FlexDrums, def
        { partDrums = Just PartDrums
          { drumsMode        = Drums4
          , drumsDifficulty  = Tier 1 -- TODO
          , drumsKicks       = Kicks1x
          , drumsFixFreeform = True
          , drumsKit         = HardRockKit
          , drumsLayout      = StandardLayout
          , drumsFallback    = FallbackGreen
          , drumsFileDTXKit  = Nothing
          , drumsFullLayout  = FDStandard
          }
        })
      ]
    , _targets = HM.empty
    }
