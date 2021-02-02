{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Import (importFoF, importRBA, importSTFSDir, importSTFS, importMagma, importAmplitude, simpleRBAtoCON, Kicks(..)) where

import qualified Amplitude.File                 as Amp
import           Codec.Picture                  (convertRGB8, readImage)
import           Config                         hiding (Difficulty)
import           Control.Exception              (evaluate)
import           Control.Monad.Codec.Onyx.JSON  (toJSON, yamlEncodeFile)
import           Control.Monad.Extra            (guard)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Resource   (MonadResource)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Digest.Pure.MD5           as MD5
import qualified Data.DTA                       as D
import qualified Data.DTA.Serialize             as D
import qualified Data.DTA.Serialize.Amplitude   as Amp
import qualified Data.DTA.Serialize.RB3         as D
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map                       as Map
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Image                          (DXTFormat (PNGXbox),
                                                 toDXT1File)
import           Import.Base                    (def')
import           Import.FretsOnFire             (importFoF)
import           Import.Magma                   (importMagma)
import           Import.RockBand                (importRBA, importSTFS,
                                                 importSTFSDir)
import           Magma                          (getRBAFile)
import           PrettyDTA                      (C3DTAComments (..),
                                                 DTASingle (..), readRB3DTA,
                                                 writeDTASingle)
import           RockBand.Codec.File            (FlexPartName (..))
import qualified RockBand.Codec.File            as RBFile
import qualified Sound.MIDI.File.Save           as Save
import           STFS.Package                   (rb3pkg)
import qualified System.Directory               as Dir
import           System.FilePath

-- | Converts a Magma v2 RBA to CON without going through an import + recompile.
simpleRBAtoCON :: (SendMessage m, MonadResource m) => FilePath -> FilePath -> StackTraceT m ()
simpleRBAtoCON rba con = inside ("converting RBA " ++ show rba ++ " to CON " ++ show con) $ do
  tempDir "onyx_rba2con" $ \temp -> do
    md5 <- stackIO $ BL.readFile rba >>= evaluate . MD5.md5
    let shortName = "onyx" ++ take 10 (show md5)
    stackIO $ Dir.createDirectoryIfMissing True $ temp </> "songs" </> shortName </> "gen"
    getRBAFile 0 rba $ temp </> "temp_songs.dta"
    getRBAFile 1 rba $ temp </> "songs" </> shortName </> shortName <.> "mid"
    getRBAFile 2 rba $ temp </> "songs" </> shortName </> shortName <.> "mogg"
    getRBAFile 3 rba $ temp </> "songs" </> shortName </> "gen" </> shortName <.> "milo_xbox"
    getRBAFile 4 rba $ temp </> "temp_cover.bmp"
    -- 5 is weights.bin (empty in magma v2)
    getRBAFile 6 rba $ temp </> "temp_extra.dta"
    (_, pkg, isUTF8) <- readRB3DTA $ temp </> "temp_songs.dta"
    extra <- (if isUTF8 then D.readFileDTA_utf8' else D.readFileDTA_latin1') $ temp </> "temp_extra.dta"
    stackIO
      $ B8.writeFile (temp </> "songs/songs.dta")
      $ (if isUTF8 then TE.encodeUtf8 else B8.pack . T.unpack)
      $ writeDTASingle DTASingle
      { dtaTopKey = T.pack shortName
      , dtaSongPackage = pkg
        { D.song = (D.song pkg)
          { D.songName = T.pack $ "songs" </> shortName </> shortName
          }
        , D.songId = Just $ Right $ T.pack shortName
        }
      , dtaC3Comments = C3DTAComments
        { c3dtaCreatedUsing = Nothing
        , c3dtaAuthoredBy   = case extra of
          D.DTA _ (D.Tree _ [D.Parens (D.Tree _
            ( D.String "backend"
            : D.Parens (D.Tree _ [D.Sym "author", D.String s])
            : _
            ))])
            -> Just s
          _ -> Nothing
        , c3dtaSong         = Nothing
        , c3dtaLanguages    = Nothing -- TODO
        , c3dtaKaraoke      = Nothing
        , c3dtaMultitrack   = Nothing
        , c3dtaConvert      = Nothing
        , c3dta2xBass       = Nothing
        , c3dtaRhythmKeys   = Nothing
        , c3dtaRhythmBass   = Nothing
        , c3dtaCATemh       = Nothing
        , c3dtaExpertOnly   = Nothing
        }
      }
    stackIO $ readImage (temp </> "temp_cover.bmp") >>= \case
      Left err -> error err -- TODO
      Right dyn -> let
        out = temp </> "songs" </> shortName </> "gen" </> (shortName ++ "_keep.png_xbox")
        in BL.writeFile out $ toDXT1File PNGXbox $ convertRGB8 dyn
    stackIO $ do
      Dir.removeFile $ temp </> "temp_songs.dta"
      Dir.removeFile $ temp </> "temp_cover.bmp"
      Dir.removeFile $ temp </> "temp_extra.dta"
    let label = D.name pkg <> maybe "" (\artist -> " (" <> artist <> ")") (D.artist pkg)
    rb3pkg label label temp con

importAmplitude :: (SendMessage m, MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
importAmplitude fin dout = do
  song <- stackIO (D.readFileDTA fin) >>= D.unserialize D.stackChunks
  let moggPath = takeDirectory fin </> T.unpack (Amp.mogg_path song)
      midPath  = takeDirectory fin </> T.unpack (Amp.midi_path song)
      previewStart = realToFrac (Amp.preview_start_ms song) / 1000
      previewEnd = previewStart + realToFrac (Amp.preview_length_ms song) / 1000
  md5 <- stackIO $ BL.readFile moggPath >>= evaluate . MD5.md5
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
  stackIO $ Dir.createDirectoryIfMissing False dout
  stackIO $ Save.toFile (dout </> "notes.mid") $ RBFile.showMIDIFile'
    $ RBFile.Song temps sigs mempty
      { RBFile.onyxParts = Map.fromList $ do
        (name, _, _, trk) <- parts
        return (name, mempty { RBFile.onyxCatch = trk })
      }
  stackIO $ Dir.copyFile moggPath $ dout </> "audio.mogg"
  stackIO $ yamlEncodeFile (dout </> "song.yml") $ toJSON SongYaml
    { _metadata = def'
      { _title        = Just $ Amp.title song
      , _artist       = Just $ case Amp.artist_short song of
        "Harmonix" -> Amp.artist song -- human love
        artist     -> artist
      , _previewStart = Just $ PreviewSeconds previewStart
      , _previewEnd   = Just $ PreviewSeconds previewEnd
      }
    , _global = def'
    , _audio = HM.empty
    , _jammit = HM.empty
    , _plans = HM.singleton "mogg" MoggPlan
      { _fileMOGG = Just "audio.mogg"
      , _moggMD5 = Just $ T.pack $ show md5
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
      }
    , _targets = HM.empty
    , _parts = Parts $ HM.fromList $ do
      (name, _, inst, _) <- parts
      return (name, def' { partAmplitude = Just (PartAmplitude inst) })
    }
