{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
module Onyx.Import.Amplitude where

import           Control.Monad.Extra              (forM, guard, mapMaybeM,
                                                   unless, when)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Bifunctor                   (first)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Builder          as BB
import qualified Data.ByteString.Char8            as B8
import           Data.Char                        (toLower)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor                     ((<&>))
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Onyx.Amplitude.PS2.Bnk
import           Onyx.Amplitude.PS2.MIDI          (AmpCatchTrack (..),
                                                   AmpSampleTrack (..),
                                                   AmpTrack (..),
                                                   BankTrack (..),
                                                   parseAmpTrack)
import           Onyx.Amplitude.PS2.TxtBin        (Node (..), NodeContents (..),
                                                   TxtBin (..), getTxtBin)
import           Onyx.Amplitude.Track             (CatchDifficulty (..),
                                                   CatchTrack (..))
import           Onyx.Audio                       (Audio (Input),
                                                   gainRatioToDecibels)
import           Onyx.Audio.VGS                   (VGSHeader (..),
                                                   makeVGSHeader)
import           Onyx.Guitar                      (applyStatus1)
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..),
                                                   blipEdgesRB_,
                                                   joinEdgesSimple,
                                                   splitEdgesSimple)
import           Onyx.MIDI.Track.File             (OnyxFile (..), OnyxPart (..),
                                                   PartName (..), RawFile (..),
                                                   SampleTrigger (..),
                                                   SamplesTrack (..), Song (..),
                                                   loadMIDIReadable)
import           Onyx.MIDI.Track.Mania
import           Onyx.PhaseShift.Dance            (NoteType (NoteNormal))
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Binary                 (runGetM)
import           Onyx.Util.Handle
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  ((<.>), (</>))
import           System.IO                        (SeekMode (..), hClose, hSeek)

sampToVGS :: SAMPEntry -> Readable -> Readable
sampToVGS samp nse = makeHandle "nse -> vgs" do
  unless (samp.sampChannels == 1) $ fail "Non-mono SAMP"
  h <- rOpen nse

  hSeek h AbsoluteSeek $ fromIntegral samp.sampFilePosition
  -- TODO most (but not all) samp areas start with an all-zero row;
  -- should we skip that?
  let go !blocks = do
        row <- B.hGet h 16
        case B.indexMaybe row 1 of
          Just 0 -> go $ blocks + 1
          _      -> return blocks
  blocks <- go 0

  header <- byteStringSimpleHandle $ BB.toLazyByteString $ makeVGSHeader VGSHeader
    { version = 2
    , channels = [(fromIntegral samp.sampRate, blocks)]
    }
  let startPosn = fromIntegral samp.sampFilePosition
      dataLength = fromIntegral $ blocks * 16
  sub <- subHandleInternal startPosn (Just dataLength) h hClose

  appendSimpleHandle header sub

importAmpPS2Ark :: (SendMessage m, MonadIO m) => FilePath -> Folder B.ByteString Readable -> StackTraceT m [Import m]
importAmpPS2Ark src folder = do
  let folderDowncase = first (B8.map toLower) folder
      getFile path = case findFile (B8.map toLower <$> path) folderDowncase of
        Nothing -> fatal $ "File not found: " <> show path
        Just r  -> return r
  config <- do
    r <- getFile $ "gen" NE.:| ["freq2_config.txt.bin"]
    stackIO (useHandle r handleToByteString) >>= runGetM getTxtBin
  let songPairs = do
        N Node { contents = S "db" : db } <- config.node.contents
        N Node { contents = S "songs" : allSongs } <- db
        N Node { contents = S _area : songs } <- allSongs
        -- maybe: guard $ _area /= "Tutorial"
        N Node { contents = S shortName : song } <- songs
        return (shortName, song)
  metadata <- do
    r <- getFile $ "gen" NE.:| ["english_songs.txt.bin"]
    bin <- stackIO (useHandle r handleToByteString) >>= runGetM getTxtBin
    return do
      N Node { contents = [S key, S value] } <- bin.node.contents
      return (key, value)
  return $ songPairs <&> \(shortName, song) lvl -> do

    when (lvl == ImportFull) do
      lg $ "Importing Amplitude (PS2) song [" <> B8.unpack shortName <> "] from: " <> src

    let configMap = HM.fromList do
          N Node { contents = S key : rest } <- song
          return (key, rest)

    mid <- case lvl of
      ImportQuick -> return emptyChart
      ImportFull  -> case HM.lookup "midi_file" configMap of
        Just [S midiFile] -> do
          r <- getFile $ "songs" NE.:| [shortName, midiFile]
          raw <- loadMIDIReadable r
          forM raw \(RawFile trks) -> mapMaybeM parseAmpTrack trks
        _                 -> fatal "Couldn't read midi_file path"

    let bnkPaths = nubOrd do
          Bank trk <- mid.tracks
          toList trk.banks
    bnks <- forM bnkPaths \path -> inside ("Loading: " <> T.unpack path) do
      r <- getFile $ "songs" NE.:| [shortName, TE.encodeUtf8 path]
      bnk <- stackIO (useHandle r handleToByteString) >>= runGetM bnkChunks
      return (path, bnk)
    bnkResults <- forM bnks \(path, bnk) -> do
      base <- case T.stripSuffix ".bnk" path of
        Nothing   -> fatal $ "Bank path not ending in .bnk? " <> show path
        Just base -> return base
      -- load each nse into memory only once so we don't exhaust file handles
      nse <- getFile ("songs" NE.:| [shortName, TE.encodeUtf8 base <> ".nse"])
        >>= stackIO . readableToMemory
      let samp = concat [ xs | SAMP xs <- bnk ]
          sanm = concat [ xs | SANM xs <- bnk ]
          inst = concat [ xs | INST xs <- bnk ]
          sdes = concat [ xs | SDES xs <- bnk ]
      bank <- case [ x | BANK x <- bnk ] of
        [x] -> return x
        _   -> fail "Not exactly 1 BANK chunk in .bnk"
      samples <- forM (zip samp sanm) \(entry, name) -> do
        let audioName = T.replace " " "-" $ base <> "-" <> TE.decodeUtf8 name
            vgsPath = T.unpack base </> B8.unpack name <.> "vgs"
            vgs = sampToVGS entry nse
        return (audioName, AudioFile AudioInfo
          { md5      = Nothing
          , frames   = Nothing
          , filePath = Just $ SoftFile vgsPath $ SoftReadable vgs
          , commands = []
          , rate     = Nothing
          , channels = entry.sampChannels
          })
      return (samples, (bank, inst, sdes))

    let lookupBankProgPitch :: Int -> Int -> Int -> [(SDESEntry, T.Text)]
        lookupBankProgPitch b p pitch = do
          (samples, (bank, insts, sdeses)) <- bnkResults
          guard $ bank.bankNumber == b
          let addCounts :: Int -> [INSTEntry] -> [(INSTEntry, Int)]
              addCounts _ []       = []
              addCounts n (i : is) = (i, n) : addCounts (n + i.instSDESCount) is
          (inst, prevCounts) <- addCounts 0 insts
          guard $ inst.instProgNumber == p
          sdes <- take inst.instSDESCount $ drop prevCounts sdeses
          guard $ sdes.sdesMinPitch <= pitch && pitch <= sdes.sdesMaxPitch
          (sampleName, _) <- take 1 $ drop sdes.sdesSAMPNumber samples
          return (sdes, sampleName)

        importSampleTrack :: AmpSampleTrack U.Beats -> SamplesTrack U.Beats
        importSampleTrack trk = let
          processed
            = applyStatus1 Nothing (Just . snd <$> trk.bank   )
            $ applyStatus1 Nothing (Just . snd <$> trk.program)
            $ joinEdgesSimple trk.notes
          results = RTB.catMaybes $ processed <&> \case
            (Just bank, (Just program, (vel, (_chan, V.fromPitch -> pitch), len))) ->
              case lookupBankProgPitch bank program pitch of
                []      -> Nothing
                matches -> let
                  triggers = matches <&> \(sdes, sample) -> SampleTrigger
                    { group     = ""
                    , audio     = sample
                    , pan       = case compare sdes.sdesPan 0x40 of
                      LT -> (fromIntegral sdes.sdesPan / 0x40) - 1
                      EQ -> 0
                      GT -> fromIntegral (sdes.sdesPan - 0x40) / 0x3F
                    , volume    = realToFrac $ gainRatioToDecibels $ fromIntegral (V.fromVelocity vel) / 0x7F
                    , transpose = fromIntegral $ (pitch - sdes.sdesBasePitch) + sdes.sdesTranspose
                    , link      = Just pitch
                    }
                  in Just (triggers, ((), pitch, len))
            _ -> Nothing
          in SamplesTrack
            { sampleTriggers = RTB.flatten $ fst <$> results
            , sampleNotes = splitEdgesSimple $ snd <$> results
            }

        diffNames = "mellow" NE.:| ["normal", "brutal", "insane"]

        catchTracks = do
          Catch n inst name trk <- mid.tracks
          let newName = PartName $ "T" <> T.pack (show n) <> "-" <> name
              part = emptyPart
                { mania = Just $ ModeMania do
                  diff <- diffNames
                  return ManiaChart
                    { name = diff
                    , keys = 3
                    , style = ManiaAmplitude inst
                    , difficulty = Tier 1
                    }
                }
          return (newName, part, trk)

        sampleAudio = HM.fromList $ bnkResults >>= fst

        sampleTracks = HM.fromList do
          name <- [ name | (PartName name, _, _) <- catchTracks ] <> map fst extra
          return ("samples-" <> name, AudioSamples SamplesInfo
            { groupPolyphony = Nothing
            , groupCrossfade = 0
            })

        extra = do
          (i, (name, trk)) <- zip [0..] $ mid.tracks >>= \case
            BG name trk -> return (name   , trk)
            Intro   trk -> return ("INTRO", trk)
            _           -> []
          return ("backing-" <> T.pack (show (i :: Int)) <> "-" <> T.replace " " "-" name, trk)

        newMidi = Song
          { tempos = mid.tempos
          , timesigs = mid.timesigs
          , tracks = mempty
            { onyxParts = Map.fromList do
              (name, _, trk) <- catchTracks
              return (name, mempty
                { onyxPartMania = Map.fromList do
                  let trackDiffs = trk.catch.catchDifficulties
                  (diffName, rbDiff) <- zip (NE.toList diffNames) [Easy, Medium, Hard, Expert]
                  catchDiff <- toList $ Map.lookup rbDiff trackDiffs
                  return (diffName, ManiaTrack
                    { notes     = blipEdgesRB_ $ (\gem -> ((fromEnum gem, NoteNormal), Nothing)) <$> catchDiff.catchGems
                    , overdrive = RTB.empty
                    })
                })
            , onyxSamples = Map.fromList do
              (name, trk) <- [ (name, trk.samples) | (PartName name, _, trk) <- catchTracks ] <> extra
              return ("samples-" <> name, importSampleTrack trk)
            }
          }

    return SongYaml
      { metadata = def'
        { title         = TE.decodeLatin1 <$> lookup (shortName <> "_TITLE") metadata
        , artist        = TE.decodeLatin1 <$> lookup (shortName <> "_ARTIST") metadata
        , loadingPhrase = TE.decodeLatin1 <$> lookup (shortName <> "_BIO") metadata
        , fileAlbumArt  = Nothing
        }
      , global = def'
        { backgroundVideo = Nothing
        , fileBackgroundImage = Nothing
        , fileSongAnim = Nothing
        , fileMidi = SoftFile "notes.mid" $ SoftChart newMidi
        , fileMidiOriginal = Nothing -- could import this
        }
      , audio = sampleAudio <> sampleTracks
      , jammit = HM.empty
      , plans = HM.singleton "amp" $ StandardPlan StandardPlanInfo
        { song        = Nothing -- TODO
        , parts       = Parts $ HM.fromList do
          name <- [ name | (PartName name, _, _) <- catchTracks] <> map fst extra
          return (PartName name, PartSingle $ Input $ Named $ "samples-" <> name)
        , crowd       = Nothing
        , comments    = []
        , tuningCents = 0
        , fileTempo   = Nothing
        }
      , targets = HM.empty
      , parts = Parts $ HM.fromList do
        (name, part, _) <- catchTracks
        return (name, part)
      }
