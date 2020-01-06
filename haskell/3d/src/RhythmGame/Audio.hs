{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module RhythmGame.Audio where

import           Audio                          (stretchRealtime)
import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.Async       (async)
import           Control.Concurrent.MVar
import           Control.Exception              (bracket, throwIO)
import           Control.Monad                  (forM_)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace (logStdout)
import           Data.Conduit                   (runConduit, (.|))
import qualified Data.Conduit                   as C
import qualified Data.Conduit.Audio             as CA
import           Data.Conduit.Audio.Sndfile     (sourceSndFrom)
import           Data.Int                       (Int16)
import           Data.IORef
import qualified Data.Vector.Storable           as V
import           Foreign                        (Ptr, sizeOf)
import           Foreign.C                      (CFloat (..))
import           MoggDecrypt                    (moggToOgg)
import           Sound.OpenAL                   (($=))
import qualified Sound.OpenAL                   as AL
import           System.FilePath                ((</>))
import           System.IO.Temp

withMOGG :: FilePath -> (FilePath -> IO a) -> IO a
withMOGG mogg fn = withSystemTempDirectory "onyxLoadMogg" $ \tmp -> do
  let ogg = tmp </> "audio.ogg"
  logStdout (moggToOgg mogg ogg) >>= either throwIO return
  fn ogg

sourceOGGFrom :: Double -> Maybe Double -> FilePath -> IO (CA.AudioSource (ResourceT IO) Int16)
sourceOGGFrom pos mspeed ogg = do
  src <- sourceSndFrom (CA.Seconds pos) ogg
  let adjustSpeed = maybe id (\speed -> stretchRealtime (recip speed) 1) mspeed
  return $ CA.mapSamples CA.integralSample $ adjustSpeed src

withAL :: IO a -> IO a
withAL fn = let
  openDevice = AL.openDevice Nothing >>= maybe (error "couldn't open audio") return
  createContext dev = AL.createContext dev [] >>= maybe (error "couldn't create context") return
  destroyContext ctx = do
    AL.currentContext $= Nothing
    AL.destroyContext ctx
  in bracket openDevice AL.closeDevice $ \dev -> do
    bracket (createContext dev) destroyContext $ \ctx -> do
      AL.currentContext $= Just ctx
      fn

data AudioState = Filling | Playing | Stopping

playSource
  :: [Float]
  -> [Float]
  -> CA.AudioSource (ResourceT IO) Int16
  -> IO (IO ())
playSource pans vols ca = do
  let chanCount = CA.channels ca
      floatRate = realToFrac $ CA.rate ca
  srcs <- AL.genObjectNames chanCount
  forM_ (zip srcs pans) $ \(src, pan) -> AL.sourcePosition src $= AL.Vertex3 (CFloat pan) 0 0
  forM_ (zip srcs vols) $ \(src, volDB) -> AL.sourceGain src $= CFloat (10 ** (volDB / 20))
  firstFull <- newEmptyMVar
  stopper <- newIORef False
  let queueSize = 5
      waitTime = 10000 -- 0.01 secs
      t1 = runResourceT $ runConduit $ CA.source ca .| let
        loop audioState = liftIO (readIORef stopper) >>= \case
          True -> do
            case audioState of
              Stopping -> return ()
              _        -> AL.stop srcs
            -- TODO remove all the sources' buffers, waiting if necessary
            return ()
          False -> do
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
                loop audioState
              else do
                -- liftIO $ putStrLn "Queue is full"
                case audioState of
                  Filling -> liftIO $ putMVar firstFull ()
                  _       -> return ()
                forM_ srcs $ \src -> liftIO (AL.buffersProcessed src) >>= \case
                  0 -> return ()
                  n -> do
                    -- liftIO $ putStrLn $ "Removing " <> show n <> " finished buffers"
                    bufs <- AL.unqueueBuffers src n
                    AL.deleteObjectNames bufs
                liftIO $ threadDelay waitTime
                loop Playing
        in loop Filling
  _ <- async t1
  () <- takeMVar firstFull
  AL.play srcs
  return $ writeIORef stopper True
