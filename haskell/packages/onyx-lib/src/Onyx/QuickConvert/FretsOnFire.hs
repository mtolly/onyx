{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.QuickConvert.FretsOnFire where

import           Control.Monad          (forM, forM_)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Char              (toLower)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Onyx.Audio             (audioIO, loadAudioInput)
import           Onyx.CloneHero.SNG
import           Onyx.FeedBack.Load     (chartToBeats, chartToIni, chartToMIDI,
                                         loadChartFile, loadChartReadable)
import           Onyx.FretsOnFire       (loadPSIni, songToIniContents,
                                         writePSIni)
import           Onyx.MIDI.Track.File   (showMIDIFile')
import           Onyx.StackTrace
import           Onyx.Util.Files        (fixFileCase)
import           Onyx.Util.Handle
import qualified Sound.MIDI.File.Save   as Save
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist, removePathForcibly,
                                         renameDirectory, renameFile)
import           System.FilePath        (dropTrailingPathSeparator,
                                         splitExtension, takeDirectory,
                                         takeExtension, takeFileName, (</>))
import           System.IO              (hClose)
import           System.IO.Temp         (withSystemTempFile)

data QuickFoF = QuickFoF
  { metadata :: [(T.Text, T.Text)]
  , files    :: [(T.Text, Readable)]
  , location :: FilePath
  , format   :: FoFFormat
  }

data FoFFormat
  = FoFFolder
  | FoFSNG
  deriving (Eq)

loadQuickFoF :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m (Maybe QuickFoF)
loadQuickFoF fin = inside ("Loading: " <> fin) $ let
  r = fileReadable fin
  in case map toLower $ takeFileName fin of
    "song.ini" -> do
      let dir = takeDirectory fin
      ini <- loadPSIni r
      folder <- stackIO $ crawlFolder dir
      return $ Just QuickFoF
        { metadata = ini
        , files    = filter (\(name, _) -> T.toLower name /= "song.ini") $ folderFiles folder
        , location = dir
        , format   = FoFFolder
        }
    "notes.chart" -> do
      let dir = takeDirectory fin
      iniPath <- fixFileCase $ dir </> "song.ini"
      stackIO (doesFileExist iniPath) >>= \case
        True -> return Nothing -- load from song.ini instead
        False -> do
          ini <- songToIniContents . chartToIni <$> loadChartFile fin
          folder <- stackIO $ crawlFolder dir
          return $ Just QuickFoF
            { metadata = ini
            , files    = folderFiles folder
            , location = dir
            , format   = FoFFolder
            }
    _ -> if map toLower (takeExtension fin) == ".sng"
      then do
        hdr <- stackIO $ readSNGHeader r
        let folder = getSNGFolder False hdr r
        return $ Just QuickFoF
          { metadata = hdr.metadata
          , files    = folderFiles folder
          , location = fin
          , format   = FoFSNG
          }
      else return Nothing

textExt :: T.Text -> (T.Text, T.Text)
textExt x = let
  (name, ext) = splitExtension $ T.unpack x
  in (T.pack name, T.toLower $ T.pack ext)

convertAudio :: [T.Text] -> T.Text -> QuickFoF -> IO QuickFoF
convertAudio convFrom convTo q = do
  newFiles <- forM q.files $ \pair@(f, r) -> case textExt f of
    (name, ext) | elem ext convFrom -> do
      let newName = name <> convTo
          newStr = T.unpack newName
      bs <- withSystemTempFile newStr $ \fout hout -> do
        withSystemTempFile (T.unpack name) $ \fin hin -> do
          hClose hin
          hClose hout
          saveReadable r fin
          src <- loadAudioInput fin
          audioIO Nothing src fout
          BL.fromStrict <$> B.readFile fout
      return (newName, makeHandle newStr $ byteStringSimpleHandle bs)
    _ -> return pair
  return $ let QuickFoF{..} = q in QuickFoF { files = newFiles, .. }

convertOgg, convertOpus :: QuickFoF -> IO QuickFoF
convertOgg  = convertAudio [".mp3", ".wav", ".opus", ".flac"] ".ogg"
convertOpus = convertAudio [".mp3", ".wav", ".ogg" , ".flac"] ".opus"

convertMIDI :: (SendMessage m, MonadIO m) => QuickFoF -> StackTraceT m QuickFoF
convertMIDI q = do
  newFiles <- forM q.files $ \pair@(f, r) -> case textExt f of
    (name, ".chart") -> do
      chart <- chartToBeats <$> loadChartReadable r
      mid <- chartToMIDI chart
      let bs = Save.toByteString $ fmap TE.encodeUtf8 $ showMIDIFile' mid
          newName = name <> ".mid"
      return (newName, makeHandle (T.unpack newName) $ byteStringSimpleHandle bs)
    _ -> return pair
  return $ let QuickFoF{..} = q in QuickFoF { files = newFiles, .. }

saveQuickFoFSNG :: FilePath -> QuickFoF -> IO ()
saveQuickFoFSNG out q = do
  let temp = out <> ".temp"
  rs <- makeSNG q.metadata q.files
  saveReadables rs temp
  renameFile temp out

saveQuickFoFFolder :: FilePath -> QuickFoF -> IO ()
saveQuickFoFFolder out q = do
  let temp = dropTrailingPathSeparator out <> ".temp"
  createDirectoryIfMissing False temp
  forM_ q.files $ \(name, r) -> do
    saveReadable r $ temp </> T.unpack name
  writePSIni (temp </> "song.ini") q.metadata
  removePathForcibly out
  renameDirectory temp out
