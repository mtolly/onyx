{-# LANGUAGE OverloadedStrings #-}
module Import.Neversoft where

import           Audio                          (Audio (..))
import           Config
import           Control.Monad                  (forM)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Conduit.Audio             as CA
import           Data.Default.Class             (def)
import qualified Data.HashMap.Strict            as HM
import           Data.Maybe                     (catMaybes, listToMaybe)
import           Data.SimpleHandle
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Import.Base
import           Neversoft.Audio                (aesDecrypt, readFSB)
import           Neversoft.Metadata
import           Neversoft.Note
import qualified RockBand.Codec.File            as RBFile

-- | Imports DLC STFS files for (eventually, Guitar Hero 5) and Guitar Hero: Warriors of Rock.
importGH5WoR :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> StackTraceT m [Import m]
importGH5WoR folder = do
  let texts = [ r | (name, r) <- folderFiles folder, "_text.pak.xen" `T.isSuffixOf` name ]
      findFolded f = listToMaybe [ r | (name, r) <- folderFiles folder, T.toCaseFold name == T.toCaseFold f ]
  qbSections <- fmap concat $ forM texts $ \r -> do
    bs <- stackIO $ useHandle r handleToByteString
    case readTextPakXen bs of
      Left  err      -> warn err >> return []
      Right sections -> return sections
  songInfo <- fmap concat $ forM qbSections $ \sect -> do
    case parseSongInfo sect of
      Left  err   -> warn err >> return []
      Right infos -> return infos
  fmap catMaybes $ forM songInfo $ \info -> do
    case findFolded $ "b" <> TE.decodeUtf8 (songName info) <> "_song.pak.xen" of
      Nothing      -> return Nothing -- song which is listed in the database, but not actually in this package
      Just pakFile -> do
        songPak <- stackIO (useHandle pakFile handleToByteString) >>= loadSongPak
        let midiFixed = ghToMidi songPak
            midiOnyx = midiFixed
              { RBFile.s_tracks = RBFile.fixedToOnyx $ RBFile.s_tracks midiFixed
              }
            getAudio name = do
              bs <- case findFolded name of
                Nothing -> fatal $ "Couldn't find audio file: " <> show name
                Just r  -> stackIO $ useHandle r handleToByteString
              dec <- case aesDecrypt $ BL.toStrict bs of
                Nothing  -> fatal $ "Couldn't decrypt audio file: " <> show name
                Just dec -> return dec
              stackIO $ readFSB $ BL.fromStrict dec
        src1 <- getAudio $ "a" <> TE.decodeUtf8 (songName info) <> "_1.fsb.xen"
        src2 <- getAudio $ "a" <> TE.decodeUtf8 (songName info) <> "_2.fsb.xen"
        src3 <- getAudio $ "a" <> TE.decodeUtf8 (songName info) <> "_3.fsb.xen"
        (kickChans, snareChans, kitChansMix) <- case CA.channels src1 of
          8 -> return ([0, 1], [2, 3], [[4, 5], [6, 7]]) -- last 4 are 2 tom channels, 2 cymbal channels
          n -> fatal $ "Unrecognized number of drum audio channels: " <> show n
        (gtrChans, bassChans, voxChans) <- case CA.channels src2 of
          6 -> return ([0, 1], [2, 3], [4, 5])
          n -> fatal $ "Unrecognized number of guitar/bass/vocal audio channels: " <> show n
        (songChans, crowdChans) <- case CA.channels src3 of
          4 -> return ([0, 1], [2, 3])
          n -> fatal $ "Unrecognized number of backing/crowd audio channels: " <> show n
        let chans name cs = PlanAudio (Channels (map Just cs) $ Input $ Named name) [] []
            chansMix name inputs = PlanAudio (Mix $ map (\cs -> Channels (map Just cs) $ Input $ Named name) inputs) [] []
        return $ Just $ \_level -> do
          return SongYaml
            { _metadata = def'
              { _title = Just $ songTitle info
              , _artist = Just $ songArtist info
              , _year = Just $ songYear info
              , _album = Just $ songAlbumTitle info
              , _fileAlbumArt = Nothing
              }
            , _global = def'
              { _fileMidi = SoftFile "notes.mid" $ SoftChart midiOnyx
              , _fileSongAnim = Nothing
              , _backgroundVideo = Nothing
              , _fileBackgroundImage = Nothing
              }
            , _audio = HM.fromList
              [ ("audio1", AudioFile AudioInfo
                { _md5 = Nothing
                , _frames = Nothing
                , _filePath = Just $ SoftFile "audio1.wav" $ SoftAudio $ CA.mapSamples CA.fractionalSample src1
                , _commands = []
                , _rate = Nothing
                , _channels = CA.channels src1
                })
              , ("audio2", AudioFile AudioInfo
                { _md5 = Nothing
                , _frames = Nothing
                , _filePath = Just $ SoftFile "audio2.wav" $ SoftAudio $ CA.mapSamples CA.fractionalSample src2
                , _commands = []
                , _rate = Nothing
                , _channels = CA.channels src2
                })
              , ("audio3", AudioFile AudioInfo
                { _md5 = Nothing
                , _frames = Nothing
                , _filePath = Just $ SoftFile "audio3.wav" $ SoftAudio $ CA.mapSamples CA.fractionalSample src3
                , _commands = []
                , _rate = Nothing
                , _channels = CA.channels src3
                })
              ]
            , _jammit = HM.empty
            , _plans = HM.singleton "gh" Plan
              { _song = Just $ chans "audio3" songChans
              , _countin = Countin []
              , _planParts = Parts $ HM.fromList
                [ (RBFile.FlexDrums, PartDrumKit
                  (Just $ chans "audio1" kickChans)
                  (Just $ chans "audio1" snareChans)
                  (chansMix "audio1" kitChansMix)
                  )
                , (RBFile.FlexGuitar, PartSingle $ chans "audio2" gtrChans)
                , (RBFile.FlexBass  , PartSingle $ chans "audio2" bassChans)
                , (RBFile.FlexVocal , PartSingle $ chans "audio2" voxChans)
                ]
              , _crowd = Just $ chans "audio3" crowdChans
              , _planComments = []
              , _tuningCents = 0
              , _fileTempo = Nothing
              }
            , _targets = HM.empty
            , _parts = Parts $ HM.fromList
              [ (RBFile.FlexGuitar, def
                { partGRYBO = Just def
                })
              , (RBFile.FlexBass, def
                { partGRYBO = Just def
                })
              ]
            }
