{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module RhythmGame.Audio where

import           Audio                          (applyPansVols)
import           Control.Concurrent.Async       (withAsync)
import           Control.Concurrent.MVar
import           Control.Exception              (bracket, throwIO)
import           Control.Monad                  (forever)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace (logStdout)
import           Data.Conduit                   (($$))
import qualified Data.Conduit                   as C
import qualified Data.Conduit.Audio             as CA
import           Data.Conduit.Audio.Sndfile     (sourceSnd)
import           Data.Int                       (Int16)
import qualified Data.Vector.Storable           as V
import qualified Data.Vector.Storable.Mutable   as MV
import           MoggDecrypt                    (moggToOgg)
import qualified SDL
import           System.FilePath                ((</>))
import           System.IO.Temp

readyAudio :: CA.AudioSource (ResourceT IO) a -> (IO (Maybe (V.Vector a)) -> IO b) -> IO b
readyAudio src fn = do
  var <- newEmptyMVar
  let t1 = runResourceT $ CA.source src $$ do
        forever $ C.await >>= liftIO . putMVar var
  withAsync t1 $ \_ -> do
    _ <- readMVar var -- wait until first chunk is ready before continuing
    fn $ takeMVar var

playSimpleStereo :: CA.AudioSource (ResourceT IO) Int16 -> IO a -> IO a
playSimpleStereo src fn = readyAudio (CA.reorganize 1024 src) $ \getAudio -> do
  let ospec = SDL.OpenDeviceSpec
        { SDL.openDeviceFreq = SDL.Mandate $ round $ CA.rate src
        , SDL.openDeviceFormat = SDL.Mandate SDL.Signed16BitNativeAudio
        , SDL.openDeviceChannels = SDL.Mandate SDL.Stereo
        , SDL.openDeviceSamples = 1024 -- on SDL 2.0.4 this was bugged where it had to be 2*frames
        , SDL.openDeviceCallback = \fmt fillme -> let
          s16 iov = getAudio >>= \case
            Nothing -> MV.set iov 0
            Just v  -> if V.length v == MV.length iov
              then V.unsafeCopy iov v
              else MV.set iov 0 -- TODO partial fill
          in case fmt of
            SDL.Signed16BitNativeAudio -> s16 fillme
            SDL.Signed16BitLEAudio -> s16 fillme
            SDL.Signed16BitBEAudio -> s16 fillme
            _ -> error $ "playSimpleStereo: unsupported audio format " ++ show fmt
        , SDL.openDeviceUsage = SDL.ForPlayback
        , SDL.openDeviceName = Nothing
        }
  bracket (SDL.openAudioDevice ospec) (SDL.closeAudioDevice . fst) $ \(dev, _fmt) -> do
    SDL.setAudioDevicePlaybackState dev SDL.Play
    fn

playMOGG :: [Float] -> [Float] -> FilePath -> IO a -> IO a
playMOGG pans vols mogg fn = withSystemTempDirectory "onyxPlayMogg" $ \tmp -> do
  let ogg = tmp </> "audio.ogg"
  logStdout (moggToOgg mogg ogg) >>= either throwIO return
  src
    <-  CA.mapSamples CA.integralSample
    .   applyPansVols pans vols
    <$> sourceSnd ogg
  playSimpleStereo src fn
