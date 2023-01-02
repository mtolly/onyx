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
import           Data.Default.Class                    (def)
import qualified Data.HashMap.Strict                   as HM
import qualified Data.Map                              as Map
import qualified Data.Text                             as T
import qualified Onyx.Amplitude.File                   as Amp
import qualified Onyx.Harmonix.DTA                     as D
import qualified Onyx.Harmonix.DTA.Serialize           as D
import qualified Onyx.Harmonix.DTA.Serialize.Amplitude as Amp
import           Onyx.Import.Base
import           Onyx.MIDI.Track.File                  (FlexPartName (..))
import qualified Onyx.MIDI.Track.File                  as RBFile
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
  RBFile.Song temps sigs amp <- RBFile.loadMIDI midPath
  let getChannels n = case song.tracks !! (n - 1) of
        (_, (chans, _)) -> map fromIntegral chans
      freestyle = do
        (_, (ns, event)) <- song.tracks
        guard $ "event:/FREESTYLE" `T.isPrefixOf` event
        map fromIntegral ns
      parts = do
        (n, Amp.Catch inst name trk) <- Map.toList $ Amp.ampTracks amp
        return (FlexExtra name, getChannels n, inst, trk)
      midi = RBFile.Song temps sigs mempty
        { RBFile.onyxParts = Map.fromList $ do
          (name, _, _, trk) <- parts
          return (name, mempty { RBFile.onyxCatch = trk })
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
      { _backgroundVideo = Nothing
      , _fileBackgroundImage = Nothing
      , _fileSongAnim = Nothing
      , _fileMidi = SoftFile "notes.mid" $ SoftChart midi
      }
    , audio = HM.empty
    , jammit = HM.empty
    , plans = HM.singleton "mogg" MoggPlan
      { _fileMOGG = Just $ SoftFile "audio.mogg" $ SoftReadable $ fileReadable moggPath
      , _moggMD5 = Nothing
      , _moggParts = Parts $ HM.fromList $ do
        (name, chans, _, _) <- parts
        return (name, PartSingle chans)
      , _moggCrowd = freestyle -- so it's hidden from web player
      , _pans = map realToFrac song.pans
      , _vols = map realToFrac song.vols
      , _planComments = []
      , _tuningCents = 0
      , _fileTempo = Nothing
      , _karaoke = False
      , _multitrack = True
      , _decryptSilent = False
      }
    , targets = HM.empty
    , parts = Parts $ HM.fromList $ do
      (name, _, inst, _) <- parts
      return (name, def { partAmplitude = Just (PartAmplitude inst) })
    }
