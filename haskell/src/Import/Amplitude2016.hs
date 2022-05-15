{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Import.Amplitude2016 where

import qualified Amplitude.File                 as Amp
import           Config
import           Control.Monad.Extra            (guard)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.StackTrace
import           Data.Default.Class             (def)
import qualified Data.DTA                       as D
import qualified Data.DTA.Serialize             as D
import qualified Data.DTA.Serialize.Amplitude   as Amp
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map                       as Map
import           Data.SimpleHandle
import qualified Data.Text                      as T
import           Import.Base
import qualified RockBand.Codec.File            as RBFile
import           RockBand.Codec.File            (FlexPartName (..))
import           System.FilePath

importAmplitude :: (SendMessage m, MonadIO m) => FilePath -> Import m
importAmplitude fin _level = do
  song <- stackIO (D.readFileDTA fin) >>= D.unserialize D.stackChunks
  let moggPath = takeDirectory fin </> T.unpack (Amp.mogg_path song)
      midPath  = takeDirectory fin </> T.unpack (Amp.midi_path song)
      previewStart = realToFrac (Amp.preview_start_ms song) / 1000
      previewEnd = previewStart + realToFrac (Amp.preview_length_ms song) / 1000
  RBFile.Song temps sigs amp <- RBFile.loadMIDI midPath
  let getChannels n = case Amp.tracks song !! (n - 1) of
        (_, (chans, _)) -> map fromIntegral chans
      freestyle = do
        (_, (ns, event)) <- Amp.tracks song
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
    { _metadata = def'
      { _title        = Just $ Amp.title song
      , _artist       = Just $ case Amp.artist_short song of
        "Harmonix" -> Amp.artist song -- human love
        artist     -> artist
      , _previewStart = Just $ PreviewSeconds previewStart
      , _previewEnd   = Just $ PreviewSeconds previewEnd
      , _fileAlbumArt = Nothing
      }
    , _global = def'
      { _backgroundVideo = Nothing
      , _fileBackgroundImage = Nothing
      , _fileSongAnim = Nothing
      , _fileMidi = SoftFile "notes.mid" $ SoftChart midi
      }
    , _audio = HM.empty
    , _jammit = HM.empty
    , _plans = HM.singleton "mogg" MoggPlan
      { _fileMOGG = Just $ SoftFile "audio.mogg" $ SoftReadable $ fileReadable moggPath
      , _moggMD5 = Nothing
      , _moggParts = Parts $ HM.fromList $ do
        (name, chans, _, _) <- parts
        return (name, PartSingle chans)
      , _moggCrowd = freestyle -- so it's hidden from web player
      , _pans = map realToFrac $ Amp.pans song
      , _vols = map realToFrac $ Amp.vols song
      , _planComments = []
      , _tuningCents = 0
      , _fileTempo = Nothing
      , _karaoke = False
      , _multitrack = True
      , _decryptSilent = False
      }
    , _targets = HM.empty
    , _parts = Parts $ HM.fromList $ do
      (name, _, inst, _) <- parts
      return (name, def { partAmplitude = Just (PartAmplitude inst) })
    }
