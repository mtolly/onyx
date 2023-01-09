{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Import.Amplitude2016 where

import           Control.Monad.Extra                   (guard)
import           Control.Monad.IO.Class                (MonadIO)
import qualified Data.HashMap.Strict                   as HM
import qualified Data.Map                              as Map
import qualified Data.Text                             as T
import qualified Onyx.Amplitude.File                   as Amp
import qualified Onyx.Harmonix.DTA                     as D
import qualified Onyx.Harmonix.DTA.Serialize           as D
import qualified Onyx.Harmonix.DTA.Serialize.Amplitude as Amp
import           Onyx.Import.Base
import           Onyx.MIDI.Track.File                  (FlexPartName (..))
import qualified Onyx.MIDI.Track.File                  as F
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Handle
import           System.FilePath

importAmplitude :: (SendMessage m, MonadIO m) => FilePath -> Import m
importAmplitude fin _level = do
  song <- stackIO (D.readFileDTA fin) >>= D.unserialize D.stackChunks
  let _ = song :: Amp.Song
      moggPath = takeDirectory fin </> T.unpack song.mogg_path
      midPath  = takeDirectory fin </> T.unpack song.midi_path
      previewStart = realToFrac song.preview_start_ms / 1000
      previewEnd = previewStart + realToFrac song.preview_length_ms / 1000
  F.Song temps sigs amp <- F.loadMIDI midPath
  let getChannels n = case song.tracks !! (n - 1) of
        (_, (chans, _)) -> map fromIntegral chans
      freestyle = do
        (_, (ns, event)) <- song.tracks
        guard $ "event:/FREESTYLE" `T.isPrefixOf` event
        map fromIntegral ns
      parts = do
        (n, Amp.Catch inst name trk) <- Map.toList $ Amp.ampTracks amp
        return (FlexExtra name, getChannels n, inst, trk)
      midi = F.Song temps sigs mempty
        { F.onyxParts = Map.fromList $ do
          (name, _, _, trk) <- parts
          return (name, mempty { F.onyxCatch = trk })
        }
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
      { fileMOGG = Just $ SoftFile "audio.mogg" $ SoftReadable $ fileReadable moggPath
      , moggMD5 = Nothing
      , parts = Parts $ HM.fromList $ do
        (name, chans, _, _) <- parts
        return (name, PartSingle chans)
      , crowd = freestyle -- so it's hidden from web player
      , pans = map realToFrac song.pans
      , vols = map realToFrac song.vols
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
      return (name, emptyPart { amplitude = Just $ PartAmplitude inst })
    }
