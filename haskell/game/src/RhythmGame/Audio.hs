{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module RhythmGame.Audio where

import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.Async       (withAsync)
import           Control.Concurrent.MVar
import           Control.Exception              (throwIO)
import           Control.Monad                  (forM_, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace (logStdout)
import           Data.Conduit                   (runConduit, (.|))
import qualified Data.Conduit                   as C
import qualified Data.Conduit.Audio             as CA
import           Data.Conduit.Audio.Sndfile     (sourceSnd)
import           Data.Int                       (Int16)
import qualified Data.Vector.Storable           as V
import           Foreign                        (Ptr, sizeOf)
import           Foreign.C                      (CFloat (..))
import           MoggDecrypt                    (moggToOgg)
import           Sound.OpenAL                   (($=))
import qualified Sound.OpenAL                   as AL
import           System.FilePath                ((</>))
import           System.IO.Temp

playMOGG :: [Float] -> [Float] -> FilePath -> IO a -> IO a
playMOGG pans vols mogg fn = withSystemTempDirectory "onyxPlayMogg" $ \tmp -> do
  let ogg = tmp </> "audio.ogg"
  logStdout (moggToOgg mogg ogg) >>= either throwIO return
  -- this should be doable with libsndfile's clamping but it doesn't work for some reason?
  ca <- CA.mapSamples CA.integralSample <$> (sourceSnd ogg :: IO (CA.AudioSource (ResourceT IO) Float))
  let chanCount = CA.channels ca
      floatRate = realToFrac $ CA.rate ca
  dev <- AL.openDevice Nothing >>= maybe (error "couldn't open audio") return
  ctx <- AL.createContext dev [] >>= maybe (error "couldn't create context") return
  AL.currentContext $= Just ctx
  srcs <- AL.genObjectNames chanCount
  forM_ (zip srcs pans) $ \(src, pan) -> AL.sourcePosition src $= AL.Vertex3 (CFloat pan) 0 0
  forM_ (zip srcs vols) $ \(src, volDB) -> AL.sourceGain src $= CFloat (10 ** (volDB / 20))
  firstFull <- newEmptyMVar
  let queueSize = 5
      waitTime = 10000 -- 0.01 secs
      t1 = runResourceT $ runConduit $ CA.source ca .| let
        loop reportFull = do
          current <- liftIO $ AL.buffersQueued $ head srcs
          if current < queueSize
            then do
              -- liftIO $ putStrLn $ "Filling because queue has " <> show current
              C.await >>= \case
                Nothing -> return ()
                Just chunk -> do
                  bufs <- AL.genObjectNames chanCount
                  forM_ (zip bufs $ CA.deinterleave chanCount chunk) $ \(buf, chan) -> do
                    liftIO $ V.unsafeWith chan $ \p -> do
                      let _ = p :: Ptr Int16
                      AL.bufferData buf $= AL.BufferData
                        (AL.MemoryRegion p $ fromIntegral $ V.length chan * sizeOf (V.head chan))
                        AL.Mono16
                        floatRate
                  forM_ (zip srcs bufs) $ \(src, buf) -> do
                    AL.queueBuffers src [buf]
              loop reportFull
            else do
              -- liftIO $ putStrLn "Queue is full"
              when reportFull $ liftIO $ putMVar firstFull ()
              forM_ srcs $ \src -> liftIO (AL.buffersProcessed src) >>= \case
                0 -> return ()
                n -> do
                  -- liftIO $ putStrLn $ "Removing " <> show n <> " finished buffers"
                  bufs <- AL.unqueueBuffers src n
                  AL.deleteObjectNames bufs
              liftIO $ threadDelay waitTime
              loop False
        in loop True
  withAsync t1 $ \_ -> do
    () <- takeMVar firstFull
    AL.play srcs
    fn
