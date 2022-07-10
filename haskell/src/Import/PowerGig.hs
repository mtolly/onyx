{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module Import.PowerGig where

import           Audio                            (Audio (..),
                                                   decibelDifferenceInPanRatios,
                                                   fromStereoPanRatios,
                                                   stereoPanRatios)
import           Config
import           Control.Monad                    (forM, guard, unless)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Lazy             as BL
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List.NonEmpty               (NonEmpty (..), nonEmpty)
import qualified Data.Map                         as Map
import           Data.Maybe                       (listToMaybe)
import           Data.SimpleHandle                (Folder, Readable,
                                                   byteStringSimpleHandle,
                                                   findFile, findFileCI,
                                                   handleToByteString,
                                                   makeHandle, useHandle)
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           Guitars
import           Import.Base
import           PowerGig.Crypt
import           PowerGig.GEV
import           PowerGig.Songs
import qualified RockBand.Codec.Drums             as D
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as F
import           RockBand.Common                  (Difficulty (..),
                                                   StrumHOPOTap (..))
import           Sound.FSB                        (XMAContents (..),
                                                   extractXMAStream, makeXMAs,
                                                   markXMAPacketStreams,
                                                   parseXMA, splitMultitrackFSB,
                                                   splitXMA2Packets,
                                                   writeXMA2Packets)
import qualified Sound.MIDI.Util                  as U
import           STFS.Package                     (runGetM)

importPowerGig :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> StackTraceT m [Import m]
importPowerGig sourceDir = do
  hdr <- case findFile (return "Data.hdr.e.2") sourceDir of
    Nothing -> fatal "Data.hdr.e.2 not found"
    Just r  -> stackIO (useHandle r handleToByteString) >>= decryptE2 . BL.toStrict >>= readHeader
  let folder = connectPKFiles sourceDir "Data" $ getFolder hdr
  discKeys <- stackIO $ loadDiscSongKeys folder
  dlcKeys <- stackIO $ case findFile (return "AddContent.lua") sourceDir of
    Nothing -> return []
    Just r  -> findSongKeys . BL.toStrict <$> useHandle r handleToByteString
  forM (discKeys <> dlcKeys) $ \key -> do
    song <- loadSongXML key folder
    return $ importPowerGigSong key song folder

importPowerGigSong :: (SendMessage m, MonadIO m) => T.Text -> Song -> Folder T.Text Readable -> Import m
importPowerGigSong key song folder level = do

  -- we need to read .gev, since .mid are not present in PS3 version at all
  maybeGEV <- case level of
    ImportQuick -> return Nothing
    ImportFull -> do
      gevPath <- case T.stripSuffix ".mid" (audio_midi $ song_audio song) of
        Just base -> return $ "Audio" :| ["songs", key, base <> ".gev"]
        Nothing   -> fatal "<midi> value doesn't end in .mid"
      fmap Just $ case findFileCI gevPath folder of
        Nothing -> fatal "Couldn't find .gev file"
        Just r  -> stackIO (useHandle r handleToByteString) >>= runGetM readGEV
  let tempo = maybe (U.makeTempoMap RTB.empty) getMIDITempos maybeGEV
      gevTracks = Map.fromList $ do
        gev <- toList maybeGEV
        gels <- V.toList $ gelhGELS $ gevGELH gev
        name <- toList $ getString (gelsTrackName gels) gev
        return (name, gels)
  -- TODO some midis (not all) have markers that could be turned into sections.
  -- * cherub rock: no markers
  -- * been caught stealing: duplicate section name markers, "Fill" markers that should be ignored
  -- * crack the skye: mostly ok, except one dev note "Start here, medium drums, Tuesday" :)
  let onyxFile = mempty
        { RBFile.onyxParts = Map.fromList
          [ (RBFile.FlexGuitar, mempty
            { RBFile.onyxPartGuitar = mempty
              { F.fiveDifficulties = Map.fromList
                [ (Easy  , getGuitarGEV "guitar_1_easy"  )
                , (Medium, getGuitarGEV "guitar_1_medium")
                , (Hard  , getGuitarGEV "guitar_1_hard"  )
                , (Expert, getGuitarGEV "guitar_1_expert")
                ]
              , F.fiveOverdrive = case Map.lookup "guitar_1_expert" gevTracks of
                Nothing  -> RTB.empty
                Just trk -> RTB.merge (getController 81 trk) (getController 82 trk)
              }
            })
          , (RBFile.FlexDrums, mempty
            { RBFile.onyxPartDrums = mempty
              { D.drumDifficulties = Map.fromList
                [ (Easy  , getDrumsGEV "drums_1_easy"  )
                , (Medium, getDrumsGEV "drums_1_medium")
                , (Hard  , getDrumsGEV "drums_1_hard"  )
                , (Expert, getDrumsGEV "drums_1_expert")
                ]
              , D.drumOverdrive = case Map.lookup "drums_1_expert" gevTracks of
                Nothing  -> RTB.empty
                Just trk -> RTB.merge (getController 80 trk) (getController 82 trk)
              }
            })
          , (RBFile.FlexVocal, mempty
            { RBFile.onyxPartVocals = mempty -- TODO
            })
          ]
        , RBFile.onyxBeat = mempty -- TODO
        }
      getDrumsGEV trackName = case Map.lookup trackName gevTracks of
        Nothing  -> mempty
        Just trk -> let
          notes = RTB.fromAbsoluteEventList $ ATB.fromPairList $ do
            evt <- V.toList $ gelsGEVT trk
            guard $ gevtType evt == 2
            bits <- toList $ findBits guitarDrumBit $ gevtGameBits evt
            guard $ elem Bit_StandardNote bits
            let time = secsToBeats $ gevtTime evt
            -- TODO handle sustain
            drum <- concat
              [ [D.Kick            | elem Bit_Orange bits]
              , [D.Red             | elem Bit_Red    bits]
              , [D.Pro D.Yellow () | elem Bit_Yellow bits]
              , [D.Pro D.Blue   () | elem Bit_Blue   bits]
              , [D.Pro D.Green  () | elem Bit_Green  bits]
              ]
            return (time, (drum, D.VelocityNormal))
          in mempty
            { D.drumGems = notes
            }
      secsToBeats :: Float -> U.Beats
      secsToBeats = U.unapplyTempoMap tempo . realToFrac
      getController n trk = RTB.fromAbsoluteEventList $ ATB.fromPairList $ do
        evt <- V.toList $ gelsGEVT trk
        guard $ gevtType evt == 4 && gevtData1 evt == n
        return (secsToBeats $ gevtTime evt, gevtData2 evt /= 0)
      getGuitarGEV trackName = emit5' $ case Map.lookup trackName gevTracks of
        Nothing  -> RTB.empty
        Just trk -> let
          hopos = getController 68 trk
          notes = RTB.fromAbsoluteEventList $ ATB.fromPairList $ do
            evt <- V.toList $ gelsGEVT trk
            guard $ gevtType evt == 2
            bits <- toList $ findBits guitarDrumBit $ gevtGameBits evt
            guard $ elem Bit_StandardNote bits
            let start = secsToBeats $ gevtTime evt
                sustain = case gevtSustain evt of
                  0 -> Nothing
                  s -> Just $ secsToBeats (gevtTime evt + s) - start
            color <- concat
              [ [Nothing | not $ any (`elem` bits) [Bit_Green, Bit_Red, Bit_Yellow, Bit_Blue, Bit_Orange]]
              , [Just F.Green   | elem Bit_Green  bits]
              , [Just F.Red     | elem Bit_Red    bits]
              , [Just F.Yellow  | elem Bit_Yellow bits]
              , [Just F.Blue    | elem Bit_Blue   bits]
              , [Just F.Orange  | elem Bit_Orange bits]
              ]
            return (start, (color, sustain))
          in fmap (\(hopo, (color, sustain)) -> ((color, if hopo then HOPO else Strum), sustain))
            $ applyStatus1 False hopos notes
      {-
      getVocals pg = let
        notes = RTB.mapMaybe (\(freestyle, pitchBool) -> guard (not freestyle) >> Just pitchBool)
          $ applyStatus1 False (vocalFreestyle pg)
          $ RTB.merge (vocalNotes pg) ((minBound,) <$> vocalTalkies pg)
        -- lyrics have a space after them for some reason
        lyricsStart = (\case "*" -> "+"; x -> x) . T.strip <$> vocalLyrics pg
        lyricsHash = fmap (\(isTalky, lyric) -> if isTalky then lyric <> "#" else lyric)
          $ applyStatus1 False (vocalTalkies pg) lyricsStart
        in mempty
          { V.vocalNotes = notes
          , V.vocalLyrics = lyricsHash
          , V.vocalPhrase1 = drawPhrases notes $ vocalPhraseEnd pg
          -- TODO vocalGlue (add dashes, except for +; and do it before talky hash)
          -- TODO put OD on phrases from mojo
          }
      -}
      onyxMid = RBFile.Song
        { RBFile.s_tempos     = tempo
        -- unfortunately .gev does not contain time sig info (_cue.gev has the gevtType = 20 events, but no data!)
        -- maybe we can optionally look for .mid just to load time sigs?
        , RBFile.s_signatures = U.measureMapFromLengths U.Error RTB.empty
        , RBFile.s_tracks     = onyxFile
        }

  combinedAudio <- case audio_combined_audio $ song_audio song of
    Nothing -> fatal "No combined_audio in XML"
    Just ca -> return ca
  streamCount <- case quotRem (ca_num_channels combinedAudio) 2 of
    (n, 0) -> return n
    _      -> fatal $ "Expected an even num_channels for combined_audio, but found " <> show (ca_num_channels combinedAudio)
  let xboxAudio = ca_xbox360_file combinedAudio >>= T.stripSuffix ".e.2"
      ps3Audio  = T.stripSuffix ".e.2" (ca_file combinedAudio)
      useXboxAudio rate samples packetsPerBlock xmas = do
        xmaBytes <- stackIO $ makeXMAs $ flip map xmas $ \xma -> XMAContents
          { xmaChannels = 2
          , xmaRate     = rate
          , xmaSamples  = samples
          , xmaPacketsPerBlock = packetsPerBlock
          , xmaSeekTable = Nothing
          , xmaData     = writeXMA2Packets xma
          }
        return $ flip map (zip [0..] xmaBytes) $ \(i, bs) -> let
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
  audio <- case level of
    ImportQuick -> useXboxAudio 44100 0 32 $ replicate streamCount [] -- probably doesn't matter but powergig uses 44100 Hz and 32-packet XMA blocks
    ImportFull -> case xboxAudio >>= \x -> findFileCI ("Audio" :| ["songs", key, x]) folder of
      Just r -> do
        xma <- stackIO (useHandle r handleToByteString) >>= parseXMA
        packets <- fmap markXMAPacketStreams $ splitXMA2Packets $ xmaData xma
        let ppblk = xmaPacketsPerBlock xma
        useXboxAudio (xmaRate xma) (xmaSamples xma) (xmaPacketsPerBlock xma)
          $ map (\i -> extractXMAStream ppblk i packets) [0 .. streamCount - 1]
      Nothing -> case ps3Audio >>= \x -> findFileCI ("Audio" :| ["songs", key, x]) folder of
        Just r -> do
          mp3s <- stackIO $ useHandle r handleToByteString >>= splitMultitrackFSB
          return $ flip map (zip [0..] mp3s) $ \(i, bs) -> let
            name = "stream-" <> show (i :: Int)
            filename = name <> ".mp3"
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
        Nothing -> fatal "Couldn't find either Xbox 360 or PS3 format audio"

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
      , _key = Nothing -- TODO use "key_signature"
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
      -- do all songs have all instruments?
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
      {-
      , (RBFile.FlexVocal, def
        { partVocal = Just PartVocal
          { vocalDifficulty = Tier 1 -- TODO
          , vocalGender = info_singer_gender (song_info song) >>= \case
            "male"   -> Just Male
            "female" -> Just Female
            _        -> Nothing
          , vocalCount = Vocal1
          , vocalKey = Nothing
          , vocalLipsyncRB3 = Nothing
          }
        })
      -}
      ]
    , _targets = HM.empty
    }

data PhraseEvent
  = PhraseNoteOn
  | PhraseNoteOff
  | PhraseEnd
  deriving (Eq, Ord)

{-
-- TODO this doesn't handle slides at phrase ends right, see m62 of Crack the Skye
drawPhrases :: (NNC.C t) => RTB.T t (Pitch, Bool) -> RTB.T t () -> RTB.T t Bool
drawPhrases notes phraseEnd = let
  events = RTB.merge
    ((\(_, onOff) -> if onOff then PhraseNoteOn else PhraseNoteOff) <$> notes)
    (const PhraseEnd <$> phraseEnd)
  -- not in a phrase
  goNoPhrase RNil                       = RNil
  goNoPhrase (Wait t PhraseNoteOn rest) = Wait t True $ goPhrase rest
  goNoPhrase (Wait t _ rest)            = RTB.delay t $ goNoPhrase rest -- shouldn't happen?
  -- in a phrase, singing a note
  goPhrase RNil                        = RNil -- shouldn't happen?
  goPhrase (Wait t PhraseNoteOn rest)  = RTB.delay t $ goPhrase rest
  goPhrase (Wait t PhraseNoteOff rest) = RTB.delay t $ goPhraseNotNote rest
  goPhrase (Wait t PhraseEnd rest)     = RTB.delay t $ goEndingPhrase rest
  -- in a phrase, not singing a note
  goPhraseNotNote RNil = RNil -- shouldn't happen?
  goPhraseNotNote (Wait t PhraseEnd rest) = Wait t False $ goNoPhrase rest -- end phrase immediately
  goPhraseNotNote (Wait t PhraseNoteOff rest) = RTB.delay t $ goPhraseNotNote rest
  goPhraseNotNote (Wait t PhraseNoteOn rest) = RTB.delay t $ goPhrase rest
  -- in a phrase, singing a note, phrase will end when this note does
  goEndingPhrase (Wait t PhraseNoteOff rest) = Wait t False $ goNoPhrase rest
  goEndingPhrase (Wait t _ rest)             = RTB.delay t $ goEndingPhrase rest -- shouldn't happen?
  goEndingPhrase RNil                        = RTB.singleton NNC.zero False -- shouldn't happen?
  in goNoPhrase events
-}
