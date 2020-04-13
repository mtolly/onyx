{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module AudioSearch
( AudioLibrary
, newAudioLibrary
, addAudioDir
, searchInfo
, searchMOGG
, searchJammit
, fromJammitInstrument
) where

import           Audio
import           Config
import           Control.Concurrent.MVar        (MVar, modifyMVar_, newMVar,
                                                 putMVar, takeMVar)
import           Control.Monad                  (forM_, join, unless)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toLower)
import           Data.Conduit.Audio
import qualified Data.Digest.Pure.MD5           as MD5
import           Data.Hashable                  (Hashable)
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import           Data.Sequence                  (Seq ((:<|)), (<|), (|>))
import qualified Data.Sequence                  as S
import qualified Data.Text                      as T
import           Path
import           Path.IO
import           RockBand.Common                (RB3Instrument (..))
import qualified Sound.Jammit.Base              as J
import           System.Process                 (CreateProcess (cwd), shell)

newtype AudioLibrary = AudioLibrary (MVar AudioState)

data AudioState = AudioState
  { audioFiles        :: !(HM.HashMap T.Text (Path Abs File)) -- ^ keys are MD5 of audio content (i.e. FLAC fingerprint)
  , audioMOGGs        :: !(HM.HashMap T.Text (Path Abs File)) -- ^ keys are MD5 of whole file
  , audioJammit       :: !(HM.HashMap (T.Text, T.Text, RB3Instrument) (Path Abs Dir)) -- ^ keys are (title, artist, instrument)
  , audioQueueDirs    :: !(S.Seq (Path Abs Dir))
  , audioQueueFLACs   :: !(S.Seq (Path Abs File))
  , audioQueueWAVs    :: !(S.Seq (Path Abs File))
  , audioQueueMOGGs   :: !(S.Seq (Path Abs File))
  , audioQueueJammit  :: !(S.Seq (Path Abs File))
  , audioScannedDirs  :: !(HS.HashSet (Path Abs Dir))
  , audioScannedFiles :: !(HS.HashSet (Path Abs File))
  }

newAudioLibrary :: (MonadIO m) => m AudioLibrary
newAudioLibrary = fmap AudioLibrary $ liftIO $ newMVar AudioState
  { audioFiles = HM.empty
  , audioMOGGs = HM.empty
  , audioJammit = HM.empty
  , audioQueueDirs = S.empty
  , audioQueueFLACs = S.empty
  , audioQueueWAVs = S.empty
  , audioQueueMOGGs = S.empty
  , audioQueueJammit = S.empty
  , audioScannedDirs = HS.empty
  , audioScannedFiles = HS.empty
  }

addAudioDir :: (MonadIO m) => AudioLibrary -> Path Abs Dir -> m ()
addAudioDir (AudioLibrary var) dir = liftIO $ modifyMVar_ var $ return . queueDirBack dir

queueDirFront :: Path Abs Dir -> AudioState -> AudioState
queueDirFront dir ast = if HS.member dir $ audioScannedDirs ast
  then ast
  else ast { audioQueueDirs = dir <| audioQueueDirs ast }

queueDirBack :: Path Abs Dir -> AudioState -> AudioState
queueDirBack dir ast = if HS.member dir $ audioScannedDirs ast
  then ast
  else ast { audioQueueDirs = audioQueueDirs ast |> dir }

queueFileBack :: Path Abs File -> AudioState -> AudioState
queueFileBack f ast = if HS.member f $ audioScannedFiles ast
  then ast
  else case map toLower <$> fileExtension f of
    Just ".flac" -> ast { audioQueueFLACs = audioQueueFLACs ast |> f }
    Just ".wav" -> ast { audioQueueWAVs = audioQueueWAVs ast |> f }
    Just ".mogg" -> ast { audioQueueMOGGs = audioQueueMOGGs ast |> f }
    _ -> case toFilePath $ filename f of
      "info.plist" -> ast { audioQueueJammit = audioQueueJammit ast |> f }
      _            -> ast

addDirectoryContents :: (MonadIO m) => AudioState -> Path Abs Dir -> StackTraceT m AudioState
addDirectoryContents ast dir = do
  songyml <- parseRelFile "song.yml"
  isGen <- if toFilePath (dirname dir) == "gen/"
    then doesFileExist $ parent dir </> songyml
    else return False
  if isGen
    then return ast
    else stackIO' (listDir dir) >>= \case
      Nothing -> return ast
      Just (dirs, files) -> return $ foldr ($) ast $ map queueDirFront dirs ++ map queueFileBack files

popFile :: SearchType -> AudioState -> Maybe (Path Abs File, AudioState)
popFile stype ast = case stype of
  SearchFile -> case audioQueueFLACs ast of
    S.Empty -> if S.null $ audioQueueDirs ast
      then case audioQueueWAVs ast of
        S.Empty    -> Nothing
        p :<| rest -> Just (p, ast { audioQueueWAVs = rest })
      else Nothing
    p :<| rest -> Just (p, ast { audioQueueFLACs = rest })
  SearchMOGG -> case audioQueueMOGGs ast of
    S.Empty    -> Nothing
    p :<| rest -> Just (p, ast { audioQueueMOGGs = rest })
  SearchJammit -> case audioQueueJammit ast of
    S.Empty    -> Nothing
    p :<| rest -> Just (p, ast { audioQueueJammit = rest })

popDir :: AudioState -> Maybe (Path Abs Dir, AudioState)
popDir ast = case audioQueueDirs ast of
  S.Empty -> Nothing
  d :<| ds -> let
    ast' = ast
      { audioQueueDirs = ds
      , audioScannedDirs = HS.insert d $ audioScannedDirs ast
      }
    in Just (d, ast')

verifyFile :: (MonadIO m) => AudioInfo f -> Path Abs File -> StackTraceT m ()
verifyFile AudioInfo{..} f = do
  let verify str expect compute = do
        actual <- compute $ toFilePath f
        unless (actual == expect) $ fatal $
          str ++ " verification failed. Should be " ++ show expect ++ " but found " ++ show actual
  case _md5 of
    Nothing  -> return ()
    Just md5 -> verify "Audio MD5" (Just $ T.unpack md5) audioMD5
  case _frames of
    Nothing   -> return ()
    Just frms -> verify "Audio length" (Just frms) audioLength
  case _rate of
    Nothing   -> return ()
    Just rate -> verify "Audio rate" (Just rate) audioRate
  verify "Audio channels" (Just _channels) audioChannels

stackIO' :: (MonadIO m) => IO a -> StackTraceT m (Maybe a)
stackIO' io = let
  handler :: (Monad m) => IOError -> m (Maybe a)
  handler _ = return Nothing
  in stackCatchIO handler $ fmap Just io

data SearchType = SearchFile | SearchMOGG | SearchJammit
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

processOne :: (MonadIO m) => SearchType -> AudioState -> StackTraceT m (AudioState, Bool)
processOne stype ast = case popFile stype ast of
  Nothing -> case popDir ast of
    Nothing        -> return (ast, False)
    Just (d, ast') -> do
      ast'' <- addDirectoryContents ast' d
      return (ast'', True)
  Just (f, ast') -> let
    ret x = return (x, True)
    computeMD5 = fmap join (stackIO' $ audioMD5 $ toFilePath f) >>= \case
      Nothing -> ret ast'
      Just md5 -> ret ast'
        { audioFiles = HM.insert (T.pack md5) f $ audioFiles ast'
        }
    in case map toLower <$> fileExtension f of
      Just ".flac" -> computeMD5
      Just ".wav" -> computeMD5
      Just ".mogg" -> stackIO' (T.pack . show . MD5.md5 <$> BL.readFile (toFilePath f)) >>= \case
        Nothing -> ret ast'
        Just md5 -> ret ast'
          { audioMOGGs = HM.insert md5 f $ audioMOGGs ast'
          }
      _ -> case toFilePath $ filename f of
        "info.plist" -> do
          let dir = parent f
          fmap join (stackIO' $ J.loadInfo $ toFilePath dir) >>= \case
            Just info -> let
              jmt = (T.pack $ J.title info, T.pack $ J.artist info, fromJammitInstrument $ J.instrument info)
              in ret ast'
                { audioJammit = HM.insert jmt dir $ audioJammit ast'
                }
            Nothing -> ret ast'
        _ -> ret ast'

fromJammitInstrument :: J.Instrument -> RB3Instrument
fromJammitInstrument = \case
  J.Guitar   -> Guitar
  J.Bass     -> Bass
  J.Drums    -> Drums
  J.Keyboard -> Keys
  J.Vocal    -> Vocal

searchCommon :: (MonadIO m, Eq s, Hashable s, Show s, Show a) => SearchType -> (AudioState -> HM.HashMap s a) -> AudioLibrary -> s -> StackTraceT m a
searchCommon stype f (AudioLibrary var) s = let
  go = do
    lib <- stackIO $ takeMVar var
    case HM.lookup s $ f lib of
      Just x -> do
        stackIO $ putMVar var lib
        return x
      Nothing -> processOne stype lib >>= \case
        (lib', True) -> do
          stackIO $ putMVar var lib'
          go
        (_, False) -> fatal $ "Couldn't locate audio for query: " ++ show s
  in go

searchFile :: (MonadIO m) => AudioLibrary -> T.Text -> StackTraceT m (Path Abs File)
searchFile = searchCommon SearchFile audioFiles

searchMOGG :: (MonadIO m) => AudioLibrary -> T.Text -> StackTraceT m (Path Abs File)
searchMOGG = searchCommon SearchMOGG audioMOGGs

searchJammit :: (MonadIO m) => AudioLibrary -> (T.Text, T.Text, RB3Instrument) -> StackTraceT m (Path Abs Dir)
searchJammit = searchCommon SearchJammit audioJammit

searchInfo :: (SendMessage m, MonadIO m) =>
  FilePath -> AudioLibrary -> AudioInfo FilePath -> StackTraceT m (Audio Duration FilePath)
searchInfo dir lib ainfo@AudioInfo{..} = let
  finishFile p = do
    verifyFile ainfo p
    return $ case _rate of
      Nothing -> Resample $ Input $ toFilePath p
      Just _  -> Input $ toFilePath p
  in case _filePath of
    Nothing -> case _md5 of
      Nothing  -> fatal "No file-path or md5 specified for audio file"
      Just md5 -> searchFile lib md5 >>= finishFile
    Just f -> do
      p <- getCurrentDir >>= \cwd -> resolveFile cwd f
      doesFileExist p >>= \case
        True -> return ()
        False -> case _commands of
          [] -> fatal $ "File does not exist: " ++ toFilePath p
          _ -> do
            forM_ _commands $ \c -> do
              out <- stackProcess (shell $ T.unpack c) { cwd = Just dir }
              lg $ "# " ++ T.unpack c ++ "\n" ++ out
            doesFileExist p >>= \case
              True -> return ()
              False -> fatal $ "File does not exist after running commands: " ++ toFilePath p
      finishFile p
