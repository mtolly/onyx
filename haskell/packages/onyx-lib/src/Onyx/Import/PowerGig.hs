{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Import.PowerGig where

import           Control.Monad                     (forM, guard, unless)
import           Control.Monad.IO.Class            (MonadIO)
import           Data.Bits                         ((.&.))
import qualified Data.ByteString.Lazy              as BL
import           Data.Default.Class                (def)
import           Data.Either                       (isRight)
import qualified Data.EventList.Absolute.TimeBody  as ATB
import qualified Data.EventList.Relative.TimeBody  as RTB
import           Data.Foldable                     (toList)
import           Data.Function                     ((&))
import           Data.Functor                      ((<&>))
import qualified Data.HashMap.Strict               as HM
import           Data.List.NonEmpty                (NonEmpty (..), nonEmpty)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe,
                                                    isJust, isNothing,
                                                    listToMaybe)
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import qualified Data.Vector                       as V
import           Data.Word                         (Word32)
import           Onyx.Audio                        (Audio (..),
                                                    decibelDifferenceInPanRatios,
                                                    fromStereoPanRatios,
                                                    stereoPanRatios)
import           Onyx.Audio.FSB                    (XMA2Contents (..),
                                                    extractXMAStream,
                                                    getFSBStreamBytes,
                                                    makeXMA2s,
                                                    markXMA2PacketStreams,
                                                    parseFSB, parseXMA2,
                                                    splitFSBStreams,
                                                    splitXMA2Packets,
                                                    writeXMA2Packets)
import           Onyx.Guitar
import           Onyx.Harmonix.DTA.Serialize.Magma (Gender (..))
import           Onyx.Import.Base
import           Onyx.MIDI.Common                  (Difficulty (..), Edge (..),
                                                    StrumHOPOTap (..),
                                                    edgeBlips_, joinEdgesSimple,
                                                    pattern RNil, pattern Wait)
import qualified Onyx.MIDI.Track.Drums             as D
import qualified Onyx.MIDI.Track.File              as F
import qualified Onyx.MIDI.Track.FiveFret          as Five
import           Onyx.MIDI.Track.Vocal             (Lyric (..), LyricNote (..),
                                                    Pitch, TalkyDifficulty (..),
                                                    VocalTrack (..),
                                                    putLyricNotes)
import           Onyx.PowerGig.Crypt
import           Onyx.PowerGig.GEV
import           Onyx.PowerGig.MIDI
import           Onyx.PowerGig.Songs
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Binary                  (runGetM)
import           Onyx.Util.Handle                  (Folder, Readable,
                                                    byteStringSimpleHandle,
                                                    findFile, findFileCI,
                                                    handleToByteString,
                                                    makeHandle, useHandle)
import           Optics                            ((.~))
import qualified Sound.MIDI.Util                   as U

importPowerGig :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> T.Text -> StackTraceT m [Import m]
importPowerGig sourceDir base = do
  let hdrName = base <> ".hdr.e.2"
  hdr <- case findFile (return hdrName) sourceDir of
    Nothing -> fatal $ T.unpack hdrName <> " not found"
    Just r  -> stackIO (useHandle r handleToByteString) >>= decryptE2 . BL.toStrict >>= readHeader
  let folder = decryptPKContents $ connectPKFiles sourceDir (T.unpack base) $ getFolder hdr.fh_Contents
  discKeys <- stackIO $ loadDiscSongKeys folder
  dlcKeys <- stackIO case findFile (return "AddContent.lua") sourceDir of
    Nothing -> return []
    Just r  -> findSongKeys . BL.toStrict <$> useHandle r handleToByteString
  forM (discKeys <> dlcKeys) \key -> do
    song <- loadSongXML key folder
    return $ importPowerGigSong key song folder

importPowerGigSong :: (SendMessage m, MonadIO m) => T.Text -> Song -> Folder T.Text Readable -> Import m
importPowerGigSong key song folder level = do

  -- we default to reading .gev, since .mid are not present in PS3 version at all.
  -- .gev are usually present, except the leaked weezer pack doesn't have them
  maybeGEV <- case level of
    ImportQuick -> return Nothing
    ImportFull -> do
      gevPath <- case T.stripSuffix ".mid" song.song_audio.audio_midi of
        Just base -> return $ "Audio" :| ["songs", key, base <> ".gev"]
        Nothing   -> fatal "<midi> value doesn't end in .mid"
      case findFileCI gevPath folder of
        Nothing -> return Nothing
        Just r  -> fmap Just $ stackIO (useHandle r handleToByteString) >>= runGetM readGEV
  maybeMid <- case level of
    ImportFull | isNothing maybeGEV -> case findFileCI ("Audio" :| ["songs", key, song.song_audio.audio_midi]) folder of
      Nothing -> return Nothing
      Just r  -> fmap Just $ F.loadRawMIDIReadable r >>= F.readMIDIFile' . fixLateTrackNames
    _ -> return Nothing
  case (maybeGEV, maybeMid, level) of
    (Nothing, Nothing, ImportFull) -> warn "No GEV or MIDI file found, charts will be empty!"
    (Nothing, Just _ , _         ) -> lg "Loading charts from MIDI because no GEV file was found."
    _                              -> return ()
  let _ = maybeMid :: Maybe (F.Song (PGFile U.Beats))
      mid = fromMaybe emptyChart maybeMid

  let tempo = maybe mid.tempos getMIDITempos maybeGEV
      gevTracks = Map.fromList do
        gev <- toList maybeGEV
        gels <- V.toList gev.gevGELH.gelhGELS
        name <- toList $ getString gels.gelsTrackName gev
        return (name, gels)
  -- TODO some midis (not all) have markers that could be turned into sections.
  -- * cherub rock: no markers
  -- * been caught stealing: duplicate section name markers, "Fill" markers that should be ignored
  -- * crack the skye: mostly ok, except one dev note "Start here, medium drums, Tuesday" :)

  let secsToBeats :: Float -> U.Beats
      secsToBeats = U.unapplyTempoMap tempo . realToFrac

      getController :: Word32 -> GELS -> RTB.T U.Beats Bool
      getController n trk = RTB.fromAbsoluteEventList $ ATB.fromPairList do
        evt <- V.toList trk.gelsGEVT
        guard $ evt.gevtType == 4 && evt.gevtData1 == n
        return (secsToBeats evt.gevtTime, evt.gevtData2 /= 0)

      -- For each glue phrase, mark all but the last non-slide lyric as connecting
      glueLyrics glue lyrics = go False $ RTB.merge (fmap Left glue) (fmap Right lyrics) where
        go isGlue (Wait dt (Right Nothing) rest) = Wait dt Nothing $ go isGlue rest
        go False (Wait dt (Right (Just s)) rest) = Wait dt (Just $ Lyric s False) $ go False rest
        go _ (Wait dt (Left newGlue) rest) = RTB.delay dt $ go newGlue rest
        go True (Wait dt (Right (Just s)) rest) = let
          continues = any (either (const False) isJust) $ takeWhile isRight $ RTB.getBodies rest
          in Wait dt (Just $ Lyric s continues) $ go True rest
        go _ RNil = RNil

      drawPhrases lyricNotes phraseEnds mojo = let
        tubeStarts = Set.fromList $ ATB.getTimes $ RTB.toAbsoluteEventList 0 $ RTB.filter
          ((\case Pitched{} -> True; Talky{} -> True; SlideTo{} -> False) . fst)
          lyricNotes
        tubeEnds = Set.fromList $ map (\(t, (_, len)) -> t + len)
          $ ATB.toPairList
          $ RTB.toAbsoluteEventList 0 lyricNotes
        phraseEndList = ATB.getTimes $ RTB.toAbsoluteEventList 0 phraseEnds
        mojoEdges = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 mojo
        in RTB.fromAbsoluteEventList $ ATB.fromPairList do
          (lastPGEnd, thisPGEnd) <- zip (0 : phraseEndList) phraseEndList
          phraseStart <- toList $ Set.lookupGT lastPGEnd tubeStarts
          let nextPhraseStart = Set.lookupGT thisPGEnd tubeStarts
          phraseEnd <- toList case nextPhraseStart of
            Nothing -> Set.lookupMax tubeEnds
            Just t  -> Set.lookupLT t tubeEnds
          guard $ phraseEnd > phraseStart -- A.V.H. (Ozzy Osbourne) has a spurious \n in a freestyle section that we want to ignore
          let isMojo = maybe False snd $ Map.lookupLE phraseStart mojoEdges
          return (phraseStart, (phraseEnd - phraseStart, isMojo))

      joinVocalsNotesLyrics notes lyrics = RTB.mapMaybe
        (\xs -> case [note | Left note <- xs] of
          [] -> Nothing
          note : _ -> let
            mlyric = case [lyr | Right lyr <- xs] of
              []      -> Nothing -- some continuation notes don't have * lyric
              lyr : _ -> lyr
            in case (note, mlyric) of
              ((sustain, Just pitch), Just lyric) -> Just (Pitched pitch lyric, sustain)
              ((sustain, Just pitch), Nothing) -> Just (SlideTo pitch, sustain)
              ((sustain, Nothing), Just lyric) -> Just (Talky TalkyNormal lyric, sustain)
              ((_sustain, Nothing), Nothing) -> Nothing -- TODO warn
        ) $ RTB.collectCoincident $ RTB.merge (fmap Left notes) (fmap Right lyrics)

      getVocalsGEV trackName = case Map.lookup trackName gevTracks of
        Nothing -> return mempty
        Just trk -> let
          notes = RTB.fromAbsoluteEventList $ ATB.fromPairList do
            evt <- V.toList trk.gelsGEVT
            guard $ evt.gevtType == 2
            guard $ evt.gevtGameBits .&. 0x2000 == 0 -- ignore freestyle notes
            let start = secsToBeats evt.gevtTime
                sustain = secsToBeats (evt.gevtTime + evt.gevtSustain) - start
                pitch = case evt.gevtData1 of
                  0 -> Nothing
                  n -> Just (toEnum $ fromIntegral n - 36 :: Pitch)
            return (start, (sustain, pitch))
          lyrics = glueLyrics (getController 68 trk) $ RTB.fromAbsoluteEventList $ ATB.fromPairList do
            gev <- toList maybeGEV
            evt <- V.toList trk.gelsGEVT
            guard $ evt.gevtType == 13
            str <- toList $ getString evt.gevtData1 gev
            let maybeLyric = case T.strip $ TE.decodeLatin1 str of -- lyrics have a space after them for some reason
                  "*" -> Nothing
                  s   -> Just s
            return (secsToBeats evt.gevtTime, maybeLyric)
          lyricNotes = joinVocalsNotesLyrics notes lyrics
          phraseEnds = RTB.fromAbsoluteEventList $ ATB.fromPairList do
            gev <- toList maybeGEV
            evt <- V.toList trk.gelsGEVT
            guard $ evt.gevtType == 9
            guard $ getString evt.gevtData1 gev == Just "\\r"
            return (secsToBeats evt.gevtTime, ())
          phrases = drawPhrases lyricNotes phraseEnds
            $ RTB.merge (getController 80 trk) (getController 81 trk)
          phraselessTrack = putLyricNotes lyricNotes
          finalTrack = phraselessTrack
            { phrase1 = U.trackJoin $ phrases <&>
              \(len, _mojo) -> Wait 0 True $ Wait len False RNil
            , overdrive = U.trackJoin $ phrases <&> \case
              (_  , False) -> RNil
              (len, True ) -> Wait 0 True $ Wait len False RNil
            }
          in return finalTrack

      getVocalsMIDI :: (Monad m) => (PGFile U.Beats -> VocalDifficulty U.Beats) -> m (VocalTrack U.Beats)
      getVocalsMIDI f = let
        pg = f mid.tracks
        notesPitched
          = fmap (\((), p, len) -> (len, Just p))
          $ joinEdgesSimple
          $ fmap (\(p, b) -> if b then EdgeOn () p else EdgeOff p) pg.vocalNotes
        notesTalky
          = fmap (\((), (), len) -> (len, Nothing))
          $ joinEdgesSimple
          $ fmap (\b -> if b then EdgeOn () () else EdgeOff ()) pg.vocalTalkies
        notes
          = fmap snd
          $ RTB.filter (not . fst)
          $ applyStatus1 False pg.vocalFreestyle
          $ RTB.merge notesPitched notesTalky
        lyrics = glueLyrics pg.vocalGlue $ pg.vocalLyrics <&> \str ->
          case T.strip str of -- lyrics have a space after them for some reason
            "*" -> Nothing
            s   -> Just s
        lyricNotes = joinVocalsNotesLyrics notes lyrics
        phraseEnds = pg.vocalPhraseEnd
        phrases = drawPhrases lyricNotes phraseEnds
          $ RTB.merge pg.vocalMojoGuitarist pg.vocalMojoDrummer
        phraselessTrack = putLyricNotes lyricNotes
        finalTrack = phraselessTrack
          { phrase1 = U.trackJoin $ phrases <&>
            \(len, _mojo) -> Wait 0 True $ Wait len False RNil
          , overdrive = U.trackJoin $ phrases <&> \case
            (_  , False) -> RNil
            (len, True ) -> Wait 0 True $ Wait len False RNil
          }
        in return finalTrack

  vox <- if isJust maybeGEV then getVocalsGEV "vocals_1_expert" else getVocalsMIDI (.pgVocalsExpert)

  let onyxFile = mempty
        { F.onyxParts = Map.fromList
          [ (F.PartGuitar, mempty
            { F.onyxPartGuitar = mempty
              { Five.difficulties = Map.fromList
                [ (Easy  , if isJust maybeGEV then getGuitarGEV "guitar_1_easy"   else getGuitarMIDI (.pd_easy  ) (.pgGuitarEasy  ))
                , (Medium, if isJust maybeGEV then getGuitarGEV "guitar_1_medium" else getGuitarMIDI (.pd_medium) (.pgGuitarMedium))
                , (Hard  , if isJust maybeGEV then getGuitarGEV "guitar_1_hard"   else getGuitarMIDI (.pd_hard  ) (.pgGuitarHard  ))
                , (Expert, if isJust maybeGEV then getGuitarGEV "guitar_1_expert" else getGuitarMIDI (.pd_expert) (.pgGuitarExpert))
                ]
              , Five.overdrive = case Map.lookup "guitar_1_expert" gevTracks of
                Nothing  -> RTB.merge
                  mid.tracks.pgGuitarExpert.guitarMojoDrummer
                  mid.tracks.pgGuitarExpert.guitarMojoVocalist
                Just trk -> RTB.merge (getController 81 trk) (getController 82 trk)
              }
            })
          , (F.PartDrums, mempty
            { F.onyxPartDrums = mempty
              { D.difficulties = Map.fromList
                [ (Easy  , if isJust maybeGEV then getDrumsGEV "drums_1_easy"   else getDrumsMIDI (.pgDrumsEasy  ))
                , (Medium, if isJust maybeGEV then getDrumsGEV "drums_1_medium" else getDrumsMIDI (.pgDrumsMedium))
                , (Hard  , if isJust maybeGEV then getDrumsGEV "drums_1_hard"   else getDrumsMIDI (.pgDrumsHard  ))
                , (Expert, if isJust maybeGEV then getDrumsGEV "drums_1_expert" else getDrumsMIDI (.pgDrumsExpert))
                ]
              , D.overdrive = case Map.lookup "drums_1_expert" gevTracks of
                Nothing  -> RTB.merge
                  mid.tracks.pgDrumsExpert.drumMojoGuitarist
                  mid.tracks.pgDrumsExpert.drumMojoVocalist
                Just trk -> RTB.merge (getController 80 trk) (getController 82 trk)
              }
            })
          , (F.PartVocal, mempty
            { F.onyxPartVocals = vox
            })
          ]
        , F.onyxBeat = mempty -- TODO
        }
      getDrumsGEV trackName = case Map.lookup trackName gevTracks of
        Nothing  -> mempty
        Just trk -> let
          notes = RTB.fromAbsoluteEventList $ ATB.fromPairList do
            evt <- V.toList trk.gelsGEVT
            guard $ evt.gevtType == 2
            bits <- toList $ findBits guitarDrumBit evt.gevtGameBits
            guard $ elem Bit_StandardNote bits
            let time = secsToBeats evt.gevtTime
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
            { D.gems = notes
            }
      getGuitarGEV trackName = emitGuitar5 case Map.lookup trackName gevTracks of
        Nothing  -> RTB.empty
        Just trk -> let
          hopos = getController 68 trk
          notes = RTB.fromAbsoluteEventList $ ATB.fromPairList do
            evt <- V.toList trk.gelsGEVT
            guard $ evt.gevtType == 2
            bits <- toList $ findBits guitarDrumBit evt.gevtGameBits
            guard $ elem Bit_StandardNote bits
            let start = secsToBeats evt.gevtTime
                sustain = case evt.gevtSustain of
                  0 -> Nothing
                  s -> Just $ secsToBeats (evt.gevtTime + s) - start
            color <- concat
              [ [Nothing | not $ any (`elem` bits) [Bit_Green, Bit_Red, Bit_Yellow, Bit_Blue, Bit_Orange]]
              , [Just Five.Green   | elem Bit_Green  bits]
              , [Just Five.Red     | elem Bit_Red    bits]
              , [Just Five.Yellow  | elem Bit_Yellow bits]
              , [Just Five.Blue    | elem Bit_Blue   bits]
              , [Just Five.Orange  | elem Bit_Orange bits]
              ]
            return (start, (color, sustain))
          in fmap (\(hopo, (color, sustain)) -> ((color, if hopo then HOPO else Strum), sustain))
            $ applyStatus1 False hopos notes

      getDrumsMIDI :: (PGFile U.Beats -> DrumDifficulty U.Beats) -> D.DrumDifficulty U.Beats
      getDrumsMIDI f = let
        pg = f mid.tracks
        in mempty
          -- TODO handle lanes
          { D.gems = flip RTB.mapMaybe pg.drumGems \case
            EdgeOn () gem -> Just (gem, D.VelocityNormal)
            EdgeOff _     -> Nothing
          }
      getGuitarMIDI
        :: (PerDifficulty Int -> Maybe Int)
        -> (PGFile U.Beats -> GuitarDifficulty U.Beats)
        -> Five.FiveDifficulty U.Beats
      getGuitarMIDI getSustDiff getMIDIDiff = let
        pg = getMIDIDiff mid.tracks
        minLen = case song.song_audio.audio_guitar.inst_sustain_threshold >>= getSustDiff of
          Just ticks -> fromIntegral ticks / 960
          Nothing    -> 1 -- TODO figure out what it actually is
        in emitGuitar5 $ fmap (\(hopo, (color, mlen)) -> ((color, if hopo then HOPO else Strum), mlen))
          $ applyStatus1 False pg.guitarHOPO $ edgeBlips_ minLen pg.guitarGems

      onyxMid = F.Song
        { F.tempos     = tempo
        -- unfortunately .gev does not contain time sig info (_cue.gev has the gevtType = 20 events, but no data!)
        -- maybe we can optionally look for .mid just to load time sigs?
        , F.timesigs = U.measureMapFromLengths U.Error RTB.empty
        , F.tracks     = onyxFile
        }

  combinedAudio <- case song.song_audio.audio_combined_audio of
    Nothing -> fatal "No combined_audio in XML"
    Just ca -> return ca
  (streamCount, endsWithMono) <- case quotRem combinedAudio.ca_num_channels 2 of
    (n, 0) -> return (n    , False)
    (n, _) -> return (n + 1, True ) -- only seen so far in The Impression That I Get
  let xboxAudio = combinedAudio.ca_xbox360_file >>= T.stripSuffix ".e.2"
      ps3Audio  = T.stripSuffix ".e.2" combinedAudio.ca_file
      channelCounts = map
        (\i -> if endsWithMono && i >= streamCount - 1 then 1 else 2)
        [0 .. streamCount - 1]
      useXboxAudio rate samples packetsPerBlock xmas = do
        xmaBytes <- stackIO $ makeXMA2s $ zip channelCounts xmas <&> \(channels, xma) -> XMA2Contents
          { xma2Channels        = channels
          , xma2Rate            = rate
          , xma2Samples         = samples
          , xma2PacketsPerBlock = packetsPerBlock
          , xma2SeekTable       = Nothing
          , xma2Data            = writeXMA2Packets xma
          }
        return $ zip3 [0..] channelCounts xmaBytes <&> \(i, channels, bs) -> let
          name = "stream-" <> show (i :: Int)
          filename = name <> ".xma"
          afile = AudioFile AudioInfo
            { md5 = Nothing
            , frames = Nothing
            , filePath = Just $ SoftFile filename $ SoftReadable
              $ makeHandle filename $ byteStringSimpleHandle bs
            , commands = []
            , rate = Nothing
            , channels = channels
            }
          in (T.pack name, afile)
  audio <- case level of
    ImportQuick -> useXboxAudio 44100 0 32 $ replicate streamCount [] -- probably doesn't matter but powergig uses 44100 Hz and 32-packet XMA blocks
    ImportFull -> case xboxAudio >>= \x -> findFileCI ("Audio" :| ["songs", key, x]) folder of
      Just r -> do
        xma <- stackIO (useHandle r handleToByteString) >>= parseXMA2
        packets <- fmap markXMA2PacketStreams $ splitXMA2Packets xma.xma2Data
        let ppblk = xma.xma2PacketsPerBlock
        useXboxAudio xma.xma2Rate xma.xma2Samples xma.xma2PacketsPerBlock
          $ map (\i -> extractXMAStream ppblk i packets) [0 .. streamCount - 1]
      Nothing -> case ps3Audio >>= \x -> findFileCI ("Audio" :| ["songs", key, x]) folder of
        Just r -> do
          -- These should always be MP3 inside, but we'll just use the generic split function
          streams <- stackIO $ useHandle r handleToByteString >>= parseFSB >>= splitFSBStreams
          forM (zip [0..] $ zip channelCounts streams) \(i, (channels, stream)) -> do
            (streamData, ext) <- stackIO $ getFSBStreamBytes stream
            let name = "stream-" <> show (i :: Int)
                filename = name <> "." <> T.unpack ext
                afile = AudioFile AudioInfo
                  { md5 = Nothing
                  , frames = Nothing
                  , filePath = Just $ SoftFile filename $ SoftReadable
                    $ makeHandle filename $ byteStringSimpleHandle streamData
                  , commands = []
                  , rate = Nothing
                  , channels = channels
                  }
            return (T.pack name, afile)
        Nothing -> fatal "Couldn't find either Xbox 360 or PS3 format audio"

  let getPlanAudio
        :: (Monad m)
        => (Onyx.PowerGig.Songs.Audio -> InstrumentType)
        -> StackTraceT m (Maybe (Onyx.Audio.Audio t AudioInput))
      getPlanAudio f = do
        let inst = f song.song_audio
        speakers <- case inst.inst_mode.mode_speakers of
          Nothing -> fatal "No mode_speakers found"
          Just ms -> return ms
        unless (speakers.mode_speakers_num_speakers == Just 2) $ fatal "mode_speakers doesn't have 2 speakers"
        frontLeft <- maybe (fatal "No front_left speaker found") return $ listToMaybe
          [ spkr | spkr <- V.toList speakers.mode_speakers_speaker, spkr.speaker_output == "front_left" ]
        frontRight <- maybe (fatal "No front_right speaker found") return $ listToMaybe
          [ spkr | spkr <- V.toList speakers.mode_speakers_speaker, spkr.speaker_output == "front_right" ]
        numChannels <- maybe (fatal "No num_channels") return speakers.mode_speakers_num_channels
        baseChannel <- maybe (fatal "No base_channel") return speakers.mode_speakers_base_channel

        let channelIndexes = take numChannels [baseChannel - 1 ..]
            makeChannels ((i, 0) : (j, 1) : rest) | i == j
              = Input (Named $ fst $ audio !! i) : makeChannels rest
            makeChannels ((i, side) : rest)
              = Channels [Just side] (Input $ Named $ fst $ audio !! i) : makeChannels rest
            makeChannels [] = []
            channels = makeChannels $ map (`quotRem` 2) channelIndexes

            ratioPairs = take numChannels $ map (\g -> (maybe 0 realToFrac $ g frontLeft, maybe 0 realToFrac $ g frontRight))
              [ (.speaker_front_left), (.speaker_front_right), (.speaker_front_center), (.speaker_low_frequency)
              , (.speaker_back_left), (.speaker_back_right), (.speaker_side_left), (.speaker_side_right)
              ]

        let pans = map fromStereoPanRatios ratioPairs
            globalVolRatio = realToFrac inst.inst_volume
            vols = zipWith decibelDifferenceInPanRatios
              (map stereoPanRatios pans)
              (map (\(l, r) -> (l * globalVolRatio, r * globalVolRatio)) ratioPairs)
            -- This fixes negative infinity being serialized to null in song.yml, and deserialized to NaN.
            -- (Negative infinity comes via fromStereoPanRatios for an unused drum channel in Hold On)
            -- After upgrading to aeson >= 2.0.0.0 this won't be necessary (will serialize to "-inf" string)
            fixNegativeInfinity = max (-99)
        case nonEmpty channels of
          Nothing -> return Nothing
          Just ne -> return $ Just $ PansVols
            (map realToFrac pans)
            (map (fixNegativeInfinity . realToFrac) vols)
            (Merge ne)

  audioBacking <- getPlanAudio (.audio_backing_track)
  audioGuitar  <- fmap PartSingle <$> getPlanAudio (.audio_guitar)
  audioDrums   <- fmap PartSingle <$> getPlanAudio (.audio_drums )
  audioVocals  <- fmap PartSingle <$> getPlanAudio (.audio_vocals)

  return SongYaml
    { metadata = Metadata
      { title = Just song.song_info.info_title
      , titleJP = Nothing
      , artist = Just song.song_info.info_artist
      , artistJP = Nothing
      , album = Just song.song_info.info_album
      , genre = Just song.song_info.info_genre -- does this need to be edited
      , subgenre = Nothing
      , year = Just song.song_info.info_year
      , fileAlbumArt = Nothing -- TODO
      , trackNumber = Nothing
      , comments = []
      , key = Nothing -- TODO use "key_signature"
      , author = Nothing
      , rating = Unrated
      , previewStart = Nothing -- TODO
      , previewEnd = Nothing -- TODO
      , languages = []
      , convert = False
      , rhythmKeys = False
      , rhythmBass = False
      , catEMH = False
      , expertOnly = False
      , cover = False
      , difficulty = Tier 1
      , loadingPhrase = Nothing
      }
    , jammit = HM.empty
    , audio = HM.fromList audio
    , plans = HM.singleton "powergig" $ StandardPlan StandardPlanInfo
      { song = audioBacking
      , parts = Parts $ HM.fromList $ catMaybes
        [ (F.PartGuitar,) <$> audioGuitar
        , (F.PartDrums ,) <$> audioDrums
        , (F.PartVocal ,) <$> audioVocals
        ]
      , crowd = Nothing
      , comments = []
      , tuningCents = 0
      , fileTempo = Nothing
      }
    , global = def'
      { fileMidi = SoftFile "notes.mid" $ SoftChart onyxMid
      , fileMidiOriginal = Nothing
      , fileSongAnim = Nothing
      , backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      }
    , parts = Parts $ HM.fromList
      -- do all songs have all instruments?
      [ (F.PartGuitar, emptyPart & #grybo .~ Just do
        (def :: ModeFive) & #difficulty .~ Tier 1 -- TODO
        )
      , (F.PartDrums, (emptyPart :: Part SoftFile)
        & #drums .~ Just (emptyPartDrums Drums4 Kicks1x) -- TODO difficulty
        )
      , (F.PartVocal, (emptyPart :: Part SoftFile) & #vocal .~ Just ModeVocal
          { difficulty = Tier 1 -- TODO
          , gender = song.song_info.info_singer_gender >>= \case
            "male"   -> Just Male
            "female" -> Just Female
            _        -> Nothing
          , count = Vocal1
          , key = Nothing
          , lipsyncRB3 = Nothing
          }
        )
      ]
    , targets = HM.empty
    }

data PhraseEvent
  = PhraseNoteOn
  | PhraseNoteOff
  | PhraseEnd
  deriving (Eq, Ord)
