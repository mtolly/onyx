{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Import.Amplitude2016 where

import           Control.Monad.Extra                   (forM, guard)
import           Control.Monad.IO.Class                (MonadIO)
import           Data.Bifunctor                        (first)
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.EventList.Relative.TimeBody      as RTB
import qualified Data.HashMap.Strict                   as HM
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Map                              as Map
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as TE
import qualified Onyx.Amplitude.File                   as Amp
import qualified Onyx.Amplitude.Track                  as Amp
import           Onyx.Harmonix.Ark
import qualified Onyx.Harmonix.DTA                     as D
import qualified Onyx.Harmonix.DTA.Serialize           as D
import qualified Onyx.Harmonix.DTA.Serialize.Amplitude as Amp
import           Onyx.Import.Base
import           Onyx.MIDI.Common                      (Difficulty (..),
                                                        blipEdgesRB_)
import           Onyx.MIDI.Track.File                  (FlexPartName (..))
import qualified Onyx.MIDI.Track.File                  as F
import           Onyx.MIDI.Track.Mania                 (ManiaTrack (..))
import           Onyx.PhaseShift.Dance                 (NoteType (NoteNormal))
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Handle
import qualified Sound.MIDI.Util                       as U
import           System.FilePath                       (takeDirectory,
                                                        takeFileName)

amplitudeDiffNames :: Onyx.MIDI.Common.Difficulty -> T.Text
amplitudeDiffNames = \case
  Easy   -> "beginner"
  Medium -> "intermediate"
  Hard   -> "advanced"
  Expert -> "expert" -- or super

importAmplitudeArk :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importAmplitudeArk _src gen = do
  (hdr, arks) <- stackIO $ loadGEN gen
  folder <- loadArkFolder hdr arks
  case first TE.decodeLatin1 <$> findFolder ["ps3", "songs"] folder of
    Nothing -> return []
    Just songsFolder -> fmap concat $ forM (folderSubfolders songsFolder) $ \(name, sub) ->
      if ("tut" `T.isPrefixOf` name) || name == "credits" -- just ignore these manually
        then return []
        else return [importAmplitudeSong sub $ name <> ".moggsong"]

importAmplitude :: (SendMessage m, MonadIO m) => FilePath -> Import m
importAmplitude moggSongPath lvl = do
  dir <- stackIO $ crawlFolder $ takeDirectory moggSongPath
  importAmplitudeSong dir (T.pack $ takeFileName moggSongPath) lvl

importAmplitudeSong :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> T.Text -> Import m
importAmplitudeSong songFolder moggSongName level = do
  song <- stackIO (findByteString (pure moggSongName) songFolder) >>= \case
    Nothing -> fatal ".moggsong not found"
    Just bs -> do
      dta <- D.readDTA_latin1 $ BL.toStrict bs
      D.unserialize D.stackChunks dta
  let _ = song :: Amp.Song
      previewStart = realToFrac song.preview_start_ms / 1000
      previewEnd = previewStart + realToFrac song.preview_length_ms / 1000
      split s = case splitPath s of
        Nothing -> fatal $ "Internal error, couldn't parse path: " <> show s
        Just p  -> return p
  mid <- split song.midi_path >>= \p -> maybe (fatal "MIDI not found") return $ findFileCI p songFolder
  ampMidi <- case level of
    ImportQuick -> return Nothing
    ImportFull  -> Just <$> F.loadMIDIReadable mid
  let _ = ampMidi :: Maybe (F.Song (Amp.AmplitudeFile U.Beats))
  mogg <- split song.mogg_path >>= \p -> maybe (fatal "MOGG not found") return $ findFileCI p songFolder
  let getChannels n = case song.tracks !! (n - 1) of
        (_, (chans, _)) -> map fromIntegral chans
      freestyle = do
        (_, (ns, event)) <- song.tracks
        guard $ "event:/FREESTYLE" `T.isPrefixOf` event
        ns
      parts = do
        (n, Amp.Catch inst name trk) <- maybe [] (\m -> Map.toList m.s_tracks.ampTracks) ampMidi
        return (FlexExtra name, getChannels n, inst, trk)
      midi = case ampMidi of
        Just (F.Song temps sigs _) -> F.Song temps sigs mempty
          { F.onyxParts = Map.fromList $ do
            (name, _, _, trk) <- parts
            return (name, mempty
              { F.onyxPartMania = Map.fromList $ do
                (diff, catchDiff) <- Map.toList trk.catchDifficulties
                return (amplitudeDiffNames diff, ManiaTrack
                  { maniaNotes     = blipEdgesRB_ $ (\gem -> ((fromEnum gem, NoteNormal), Nothing)) <$> catchDiff.catchGems
                  , maniaOverdrive = RTB.empty
                  })
              })
          }
        Nothing -> emptyChart
  return SongYaml
    { metadata = def'
      { title        = Just song.title
      , artist       = Just $ case song.artist_short of
        "Harmonix" -> song.artist -- human love
        artist     -> artist
      , previewStart = Just $ PreviewSeconds previewStart
      , previewEnd   = Just $ PreviewSeconds previewEnd
      , fileAlbumArt = Nothing
      }
    , global = def'
      { backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      , fileSongAnim = Nothing
      , fileMidi = SoftFile "notes.mid" $ SoftChart midi
      }
    , audio = HM.empty
    , jammit = HM.empty
    , plans = HM.singleton "mogg" $ MoggPlan MoggPlanInfo
      { fileMOGG = Just $ SoftFile "audio.mogg" $ SoftReadable mogg
      , moggMD5 = Nothing
      , parts = Parts $ HM.fromList $ do
        (name, chans, _, _) <- parts
        return (name, PartSingle chans)
      , crowd = []
      , pans = map realToFrac song.pans
      -- just remove freestyle audio for now by turning volume down
      , vols = zipWith
        (\i vol -> if elem i freestyle then (-999) else vol)
        [0..]
        (map realToFrac song.vols)
      , comments = []
      , tuningCents = 0
      , fileTempo = Nothing
      , karaoke = False
      , multitrack = True
      , decryptSilent = False
      }
    , targets = HM.empty
    , parts = Parts $ HM.fromList $ do
      (name, _, inst, _) <- parts
      return (name, emptyPart { mania = Just PartMania
        { keys = 3
        , turntable = False
        , difficulty = Tier 1 -- ?
        , instrument = Just inst
        , charts = fmap amplitudeDiffNames $ Easy NE.:| [Medium, Hard, Expert]
        }})
    }
