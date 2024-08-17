{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.QuickConvert.FretsOnFire where

import           Control.Monad.Extra    (forM, forM_, guard, mapMaybeM)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Char              (isSpace, toLower)
import           Data.Foldable          (toList)
import qualified Data.HashMap.Strict    as HM
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (catMaybes, fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Onyx.Audio             (Audio (..), audioIO,
                                         audioLengthReadable, audioRateReadable,
                                         buildSource', loadAudioInput)
import           Onyx.CloneHero.SNG
import           Onyx.FeedBack.Load     (chartToBeats, chartToIni, chartToMIDI,
                                         loadChartFile, loadChartReadable)
import           Onyx.FretsOnFire       (loadPSIni, songToIniContents,
                                         writePSIni)
import           Onyx.Import.Encore     (EncoreInfo (..), encoreMidiToFoF,
                                         loadEncoreInfo)
import           Onyx.MIDI.Parse        (getMIDI)
import           Onyx.MIDI.Track.File   (showMIDIFile')
import           Onyx.StackTrace
import           Onyx.Util.Binary       (runGetM)
import           Onyx.Util.Files        (fixFileCase)
import           Onyx.Util.Handle
import qualified Sound.MIDI.File.Save   as Save
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist, removePathForcibly,
                                         renameDirectory, renameFile)
import           System.FilePath        (dropTrailingPathSeparator,
                                         splitExtension, takeDirectory,
                                         takeExtension, takeFileName, (<.>),
                                         (</>))
import           System.IO              (hClose)
import           System.IO.Temp         (withSystemTempFile)
import           UnliftIO.Exception     (catchAny)

data QuickFoF = QuickFoF
  { metadata :: [(T.Text, T.Text)]
  , files    :: [(T.Text, Readable)]
  , location :: FilePath
  , format   :: FoFFormatInput
  }

data FoFFormat
  = FoFFolder
  | FoFSNG
  deriving (Eq)

data FoFFormatInput
  = FoFEncore
  | FoFFormat FoFFormat
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
        , format   = FoFFormat FoFFolder
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
            , format   = FoFFormat FoFFolder
            }
    "info.json" -> do
      info <- loadEncoreInfo fin
      stackIO $ Just <$> convertEncoreFoF fin info
      -- TODO some songs have a song.ini and info.json. in this case:
      -- * if the info.json midi is named "notes.mid", only load encore format.
      --   it can't be a well formed fof format song due to the track names
      -- * otherwise, load fof format
    _ -> if map toLower (takeExtension fin) == ".sng"
      then do
        hdr <- stackIO $ readSNGHeader r
        let folder = getSNGFolder False hdr r
        return $ Just QuickFoF
          { metadata = hdr.metadata
          , files    = folderFiles folder
          , location = fin
          , format   = FoFFormat FoFSNG
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

-- CH as of v1.1.0.4261-PTB seems to require song_length in .sng (not folder?), or you get:
-- "ERROR: These folders either have no audio files, the audio files are named incorrectly or the audio files are in the wrong format!"
fillRequiredMetadata :: QuickFoF -> IO QuickFoF
fillRequiredMetadata = let
  audioExts = [".ogg", ".mp3", ".wav", ".opus", ".flac"]
  toLengthMS :: Integer -> Int -> Integer
  toLengthMS frames rate = let
    secs = fromIntegral frames / fromIntegral rate :: Double
    in round $ secs * 1000
  withSongLength q = case lookup "song_length" q.metadata of
    Just s | T.any (not . isSpace) s -> return q
    _ -> do
      lens <- fmap catMaybes $ forM q.files $ \(name, r) -> let
        path = T.unpack $ T.toLower name
        in if elem (takeExtension path) audioExts
          then do
            mframes <- audioLengthReadable path r `catchAny` \_ -> return Nothing
            mrate   <- audioRateReadable   path r `catchAny` \_ -> return Nothing
            return $ liftA2 toLengthMS mframes mrate
          else return Nothing
      let ms = case lens of
            []     -> 1 -- note, 0 does not work
            n : ns -> foldr max n ns
      return $ let QuickFoF{..} = q in QuickFoF
        { metadata
          =  [ p | p@(k, _) <- q.metadata, k /= "song_length" ]
          <> [ ("song_length", T.pack $ show ms) ]
        , ..
        }
  in withSongLength

convertEncoreFoF :: FilePath -> EncoreInfo FilePath -> IO QuickFoF
convertEncoreFoF f info = do
  newMidi <- generateCachedReadable $ do
    midPath <- fixFileCase info.midi
    bs <- useHandle (fileReadable midPath) handleToByteString
    (mid, _warnings) <- runGetM getMIDI bs
    return $ makeHandle "notes.mid" $ byteStringSimpleHandle $ Save.toByteString $ encoreMidiToFoF mid
  let audioMap =
        [ ("song", "backing")
        , ("guitar", "lead")
        , ("rhythm", "bass")
        , ("drums", "drums")
        , ("vocals", "vocals")
        ]
  newStems <- fmap catMaybes $ forM audioMap $ \(nameCH, keyEncore) -> do
    let paths = fromMaybe [] $ HM.lookup keyEncore info.stems
    existingPaths <- flip mapMaybeM paths $ \p -> do
      p' <- fixFileCase p
      e <- doesFileExist p'
      return $ guard e >> Just p'
    case existingPaths of
      [path] -> return $ Just (T.pack $ nameCH <.> takeExtension path, fileReadable path)
      p : ps@(_ : _) -> do
        let ext = "ogg" -- TODO use preference
        r <- generateCachedReadable $ do
          bs <- withSystemTempFile ("onyx-mix" <.> ext) $ \tmp h -> do
            hClose h
            src <- buildSource' $ Mix $ fmap Input $ p NE.:| ps
            -- TODO use preference for ogg quality
            audioIO Nothing src tmp
            useHandle (fileReadable tmp) handleToByteString
          return $ makeHandle ("onyx-mix" <.> ext) $ byteStringSimpleHandle bs
        return $ Just (T.pack $ nameCH <.> ext, r)
      [] -> return Nothing
  artPath <- mapM fixFileCase info.art
  return QuickFoF
    { metadata = concat
      [ [("name", info.title)]
      , [("artist", info.artist)]
      , ("album",) <$> toList info.album
      , ("loading_phrase",) <$> toList info.loading_phrase
      , [("multiplier_note", "116")]
      , flip map (toList info.length_) $ \secs -> ("song_length", T.pack $ show $ secs * 1000)
      , ("preview_start_time",) . T.pack . show <$> toList info.preview_start_time
      , [("charter", T.intercalate ", " info.charters) | not $ null info.charters]
      , ("year",) <$> toList info.release_year
      , [("genre", T.intercalate ", " info.genres) | not $ null info.genres]
      , ("diff_guitar",) . T.pack . show <$> toList (HM.lookup "plastic_guitar" info.diff)
      , ("diff_bass"  ,) . T.pack . show <$> toList (HM.lookup "plastic_bass"   info.diff)
      , ("diff_drums" ,) . T.pack . show <$> toList (HM.lookup "plastic_drums"  info.diff)
      , ("diff_vocals",) . T.pack . show <$> toList (HM.lookup "pitched_vocals" info.diff)
      ]
    , files = concat
      [ [("notes.mid", newMidi)]
      , case artPath of
        Nothing  -> []
        Just art -> [(T.pack $ "album" <.> takeExtension art, fileReadable art)]
      , newStems
      ]
    , location = takeDirectory f
    , format   = FoFEncore
    }

{-

TODO game downconversion steps

PS (free):
- convert taps and opens to sysex format (pull back opens)
- optional: no opens since no hopo opens
- bass.ogg -> rhythm.ogg (steam PS supports this)
- remove <tags> from metadata, loading phrase, lyrics

FoFiX:
- remove taps and opens, no sysex events allowed
- probably no extended sustains
- guitar.ogg is required
- guitar.ogg must be the song length or practice mode breaks
- for no stems, should just move song.ogg to guitar.ogg (otherwise no audio in practice mode)
- bunch of PS stem filenames not supported, combine

FoF:
- no square album art, convert to cassette format
- IIRC, guitar needs to be the first midi track

-}
